//! Value semantics: deep copies at the spec §4 copy sites.
//!
//! A value whose type holds arrays is copied when it is read from an existing
//! place and stored into another: `let`, assignment, literal elements, and
//! `return`/tail of a parameter or `for … of` variable. Fresh values and call
//! arguments are never copied; returning an owned local moves it. Strings are
//! shared — they are immutable.

use inkwell::module::Linkage;
use inkwell::types::BasicType;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue, StructValue, ValueKind};

use super::CodegenError;
use super::heap::element_of;
use super::lower::{Lowerer, POSITIONED};
use super::value::llvm_type;
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
        Ok(if ty.contains_array(&self.structs) && aliasing(expr) {
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
        Ok(
            if ty.contains_array(&self.structs) && aliasing(expr) && !moves {
                self.deep_copy(value, ty)
            } else {
                value
            },
        )
    }

    /// A deep copy of `value`: arrays of plain elements by one allocation and
    /// `llvm.memcpy`; nested arrays and structs holding arrays through
    /// `lugha_copy_<type>` (spec §9). Anything else is copied by storing it.
    pub(super) fn deep_copy(
        &mut self,
        value: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> BasicValueEnum<'ctx> {
        if !ty.contains_array(&self.structs) {
            return value;
        }
        if let Type::Array(element) = ty
            && !element.contains_array(&self.structs)
        {
            return self.copy_flat(value.into_pointer_value(), element).into();
        }
        let copy = self.copy_function(ty);
        let call = self
            .builder
            .build_call(copy, &[value.into()], "copy")
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
                i64_type.const_int(self.element_size(element), false),
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

    /// `lugha_copy_<mangled>(T) -> T`, generated once per array type whose
    /// elements hold arrays and per struct type holding arrays. Declared
    /// before its body, so `Node` ↔ `Node[]` helpers can call each other.
    fn copy_function(&mut self, ty: &Type) -> FunctionValue<'ctx> {
        let name = format!("lugha_copy_{}", mangle(ty));
        if let Some(function) = self.module.get_function(&name) {
            return function;
        }
        let llvm = llvm_type(self.context, ty);
        let function = self.module.add_function(
            &name,
            llvm.fn_type(&[llvm.into()], false),
            Some(Linkage::Internal),
        );
        // Build the body elsewhere, then come back to where lowering was.
        let (saved_block, saved_function) = (self.builder.get_insert_block(), self.function);
        self.function = Some(function);
        self.builder
            .position_at_end(self.context.append_basic_block(function, "entry"));
        let source = function.get_nth_param(0).expect("one parameter");
        let copy = match ty {
            Type::Struct(name) => self.copy_fields(source.into_struct_value(), name),
            _ => self.copy_elements(source.into_pointer_value(), ty).into(),
        };
        self.builder.build_return(Some(&copy)).expect(POSITIONED);
        self.function = saved_function;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        function
    }

    /// A struct with each array-holding field deep-copied.
    fn copy_fields(&mut self, source: StructValue<'ctx>, name: &str) -> BasicValueEnum<'ctx> {
        let mut copy = source;
        for (index, (field, ty)) in self.structs[name].clone().iter().enumerate() {
            if !ty.contains_array(&self.structs) {
                continue;
            }
            let index = u32::try_from(index).expect("field count fits in u32");
            let value = self
                .builder
                .build_extract_value(copy, index, field)
                .expect("field exists");
            let value = self.deep_copy(value, ty);
            copy = self
                .builder
                .build_insert_value(copy, value, index, field)
                .expect("field exists")
                .into_struct_value();
        }
        copy.into()
    }

    /// A new array with every element deep-copied.
    fn copy_elements(&mut self, source: PointerValue<'ctx>, ty: &Type) -> PointerValue<'ctx> {
        let element = element_of(ty);
        let len = self.length(source);
        let copy = self.new_array(len, &element);
        let size = self.element_size(&element);
        self.count_loop(len, |this, i| {
            let item = this.load_element(this.element_address(source, i, size), &element);
            let item = this.deep_copy(item, &element);
            let address = this.element_address(copy, i, size);
            this.store_element(address, &element, item);
            Ok(())
        })
        .expect("copy loops only lower runtime calls");
        copy
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

/// A type's name in a copy function's symbol: `i64[][]` is `i64_arr_arr`,
/// `Point[]` is `5Point_arr`. Length-prefixing struct names keeps them apart
/// from the `_arr` suffix and the primitive names (PRP-015).
fn mangle(ty: &Type) -> String {
    match ty {
        Type::Array(element) => format!("{}_arr", mangle(element)),
        Type::Struct(name) => format!("{}{name}", name.len()),
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

    #[test]
    fn structs_holding_arrays_copy_through_helpers() {
        let src = "struct Wrap { data: i64[] }\nstruct P { x: i64 }\n\
                   fun main() { let w = Wrap { data: [1] }; let w2 = w; let p = P { x: 1 }; let q = p; }";
        let ir = ir(src);
        assert!(
            ir.contains("define internal %Wrap @lugha_copy_4Wrap(%Wrap"),
            "{ir}"
        );
        assert!(
            body(&ir, "main").contains("call %Wrap @lugha_copy_4Wrap("),
            "{ir}"
        );
        assert!(
            !ir.contains("lugha_copy_1P"),
            "plain structs copy by store: {ir}"
        );
    }

    #[test]
    fn recursive_copy_helpers_are_generated_once() {
        let src = "struct Node { value: i64, kids: Node[] }\n\
                   fun main() { let n = Node { value: 1, kids: [] }; let m = n; let o = n; }";
        let ir = ir(src);
        assert_eq!(
            ir.matches("define internal %Node @lugha_copy_4Node(")
                .count(),
            1,
            "{ir}"
        );
        assert_eq!(
            ir.matches("define internal ptr @lugha_copy_4Node_arr(")
                .count(),
            1,
            "{ir}"
        );
    }
}
