//! Value semantics: deep copies at the spec §4 copy sites.
//!
//! A value whose type holds arrays is copied when it is read from an existing
//! place and stored into another: `let`, assignment, literal elements, and
//! `return`/tail of a parameter or `for … of` variable. Fresh values and call
//! arguments are never copied; returning an owned local moves it. Strings are
//! shared — they are immutable.

use inkwell::module::Linkage;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue, ValueKind};

use super::CodegenError;
use super::heap::{element_of, element_size};
use super::lower::{Lowerer, POSITIONED};
use crate::ast::{Expr, ExprKind};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// Lowers `expr` for storing into a place, copying it if it was read from one.
    pub(super) fn value_for_store(
        &mut self,
        expr: &Expr,
        ty: &Type,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let value = self.get(expr, ty)?;
        Ok(if ty.contains_array() && aliasing(expr) {
            self.deep_copy(value, ty)
        } else {
            value
        })
    }

    /// Lowers a `return` value or function tail: copied unless it is a place
    /// rooted in an owned local, which simply moves (spec §4).
    pub(super) fn value_for_return(
        &mut self,
        expr: &Expr,
        ty: &Type,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let value = self.get(expr, ty)?;
        let moves = root(expr).is_some_and(|name| !self.scopes.lookup(name).borrowed);
        Ok(if ty.contains_array() && aliasing(expr) && !moves {
            self.deep_copy(value, ty)
        } else {
            value
        })
    }

    /// A deep copy of `value`: arrays of plain elements by one allocation and
    /// `llvm.memcpy`, nested arrays through `lugha_copy_<type>` (spec §9).
    pub(super) fn deep_copy(
        &mut self,
        value: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> BasicValueEnum<'ctx> {
        let Type::Array(element) = ty else {
            return value;
        };
        let source = value.into_pointer_value();
        if !element.contains_array() {
            return self.copy_flat(source, element).into();
        }
        let copy = self.copy_function(ty);
        let call = self
            .builder
            .build_call(copy, &[source.into()], "copy")
            .expect(POSITIONED);
        match call.try_as_basic_value() {
            ValueKind::Basic(result) => result,
            ValueKind::Instruction(_) => unreachable!("copy functions return the copy"),
        }
    }

    /// Header and elements in one `memcpy`.
    fn copy_flat(&self, source: PointerValue<'ctx>, element: &Type) -> PointerValue<'ctx> {
        let len = self.length(source);
        let copy = self.new_array(len, element);
        let i64_type = self.context.i64_type();
        let data = self
            .builder
            .build_int_mul(
                len,
                i64_type.const_int(element_size(element), false),
                "bytes",
            )
            .expect(POSITIONED);
        let bytes = self
            .builder
            .build_int_add(data, i64_type.const_int(8, false), "bytes")
            .expect(POSITIONED);
        self.builder
            .build_memcpy(copy, 8, source, 8, bytes)
            .expect("memcpy of a fresh, aligned allocation");
        copy
    }

    /// `lugha_copy_<mangled>(ptr) -> ptr`, generated once per array type
    /// whose elements hold arrays.
    fn copy_function(&mut self, ty: &Type) -> FunctionValue<'ctx> {
        let name = format!("lugha_copy_{}", mangle(ty));
        if let Some(function) = self.module.get_function(&name) {
            return function;
        }
        let ptr = self.context.ptr_type(Default::default());
        let function = self.module.add_function(
            &name,
            ptr.fn_type(&[ptr.into()], false),
            Some(Linkage::Internal),
        );
        // Build the body elsewhere, then come back to where lowering was.
        let (saved_block, saved_function) = (self.builder.get_insert_block(), self.function);
        self.function = Some(function);
        self.builder
            .position_at_end(self.context.append_basic_block(function, "entry"));
        let source = function
            .get_nth_param(0)
            .expect("one parameter")
            .into_pointer_value();
        let element = element_of(ty);
        let len = self.length(source);
        let copy = self.new_array(len, &element);
        let size = element_size(&element);
        self.count_loop(len, |this, i| {
            let item = this.load_element(this.element_address(source, i, size), &element);
            let item = this.deep_copy(item, &element);
            let address = this.element_address(copy, i, size);
            this.store_element(address, &element, item);
            Ok(())
        })
        .expect("copy loops only lower runtime calls");
        self.builder.build_return(Some(&copy)).expect(POSITIONED);
        self.function = saved_function;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        function
    }
}

/// True if `expr`'s value may come from an existing place, looking through
/// `if` and block tails.
fn aliasing(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Name(_) | ExprKind::Field(..) | ExprKind::Index(..) => true,
        ExprKind::If { then, else_, .. } => {
            then.tail.as_deref().is_some_and(aliasing) || else_.as_deref().is_some_and(aliasing)
        }
        ExprKind::Block(block) => block.tail.as_deref().is_some_and(aliasing),
        _ => false,
    }
}

/// The variable at the root of a plain place.
fn root(expr: &Expr) -> Option<&str> {
    match &expr.kind {
        ExprKind::Name(name) => Some(name),
        ExprKind::Field(base, _) | ExprKind::Index(base, _, _) => root(base),
        _ => None,
    }
}

/// A type's name in a copy function's symbol: `i64[][]` is `i64_arr_arr`.
fn mangle(ty: &Type) -> String {
    match ty {
        Type::Array(element) => format!("{}_arr", mangle(element)),
        Type::String => "str".to_string(),
        other => other.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    fn body<'a>(ir: &'a str, function: &str) -> &'a str {
        let start = &ir[ir.find(&format!("@lugha_fn_{function}(")).unwrap()..];
        &start[..start.find("\n}").unwrap()]
    }

    #[test]
    fn copy_sites_follow_the_spec_table() {
        let ir = ir("fun main() { let xs = [1, 2]; let ys = xs; let g = [[1]]; let h = g; }");
        assert!(
            ir.contains("llvm.memcpy"),
            "plain arrays copy with memcpy: {ir}"
        );
        assert!(
            ir.contains("define internal ptr @lugha_copy_i64_arr_arr(ptr"),
            "{ir}"
        );
    }

    #[test]
    fn returning_a_parameter_copies_but_a_local_moves() {
        let src = "fun p(a: i64[]): i64[] = a;\nfun l(): i64[] { let a = [1]; a }\nfun main() { let x = p([1]); let y = l(); }";
        let ir = ir(src);
        assert!(body(&ir, "p").contains("memcpy"), "{ir}");
        assert!(!body(&ir, "l").contains("memcpy"), "{ir}");
    }

    #[test]
    fn arguments_and_fresh_values_are_not_copied() {
        let ir = ir(
            "fun f(a: i64[]): i64 = a.len;\nfun main() { let n = f([1, 2]); let xs = [3]; let m = f(xs); }",
        );
        assert!(!body(&ir, "main").contains("memcpy"), "{ir}");
    }
}
