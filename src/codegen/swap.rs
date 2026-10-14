//! `swap(a, b)` (v1.1 §5): both places are evaluated left to right, then
//! their values are loaded and stored crossed. Nothing is allocated or
//! copied: swapping two arrays exchanges two pointers.

use inkwell::values::{BasicValueEnum, PointerValue};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::{Value, llvm_type};
use crate::ast::Expr;
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `swap(a, b)`; the checker guarantees two mutable places of one type.
    pub(super) fn swap(&mut self, args: &[Expr]) -> Result<Value<'ctx>, CodegenError> {
        let [a, b] = args else {
            unreachable!("checked: E0405 enforces swap's two arguments")
        };
        // Both places' indexes first, left to right, bounds-checked before
        // either is written (spec §5); only then are their arrays made
        // unique (v15), so an index of `b` that copies `a`'s array can't
        // leave `a` pointing into a shared one.
        let (a_plan, b_plan) = (self.place_plan(a)?, self.place_plan(b)?);
        let (a, ty, a_element) = self.place_commit(a, &a_plan);
        let (b, _, b_element) = self.place_commit(b, &b_plan);
        let first = self.read(a, &ty, a_element);
        let second = self.read(b, &ty, b_element);
        self.write(a, &ty, a_element, second);
        self.write(b, &ty, b_element, first);
        Ok(Value::Void)
    }

    /// A place's value: a local's slot holds it as is, fields and elements
    /// in element layout (`bool` as `i8`).
    fn read(&self, address: PointerValue<'ctx>, ty: &Type, element: bool) -> BasicValueEnum<'ctx> {
        if element {
            return self.load_element(address, ty);
        }
        self.builder
            .build_load(llvm_type(self.context, ty), address, "swap")
            .expect(POSITIONED)
    }

    fn write(
        &self,
        address: PointerValue<'ctx>,
        ty: &Type,
        element: bool,
        value: BasicValueEnum<'ctx>,
    ) {
        if element {
            self.store_element(address, ty, value);
        } else {
            self.builder.build_store(address, value).expect(POSITIONED);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    fn main_body(ir: &str) -> &str {
        &ir[ir
            .find("define void @lugha_fn_main")
            .expect("main is defined")..]
    }

    #[test]
    fn swapping_arrays_exchanges_pointers() {
        let ir = ir("fun main() { let mut a = [1]; let mut b = [2, 3]; swap(a, b); }");
        let body = main_body(&ir);
        let swap = &body[body.find("%swap = load ptr").expect("a is loaded")..];
        let swap = &swap[..swap.find("ret void").expect("main returns")];
        assert_eq!(swap.matches("load ptr").count(), 2, "{ir}");
        assert_eq!(swap.matches("store ptr").count(), 2, "{ir}");
        assert!(
            !swap.contains("memcpy") && !swap.contains("lugha_rt_alloc") && !swap.contains("call"),
            "{ir}"
        );
    }

    #[test]
    fn element_swaps_check_both_bounds_first() {
        let ir = ir("fun main() { let mut xs = [true, false]; let i = 1; swap(xs[0], xs[i]); }");
        let body = main_body(&ir);
        let first_store = body.find("store i8 %byte").expect("an element is written");
        assert_eq!(
            body[..first_store].matches("%in_bounds").count(),
            4,
            "two checks, each defined and used: {ir}"
        );
        assert!(
            body.contains("trunc i8") && body.contains("zext i1"),
            "bool in element layout: {ir}"
        );
    }
}
