//! Tuples in codegen (v14 draft §9, milestone 47): a tuple is the named
//! struct `%"(A, B)"`, so most of it is struct code. Here: literals, tuple
//! patterns (each field's address tested, no tag), and `<` through one
//! `lugha_cmp_<type>(a, b) -> i32` helper per tuple type.

use inkwell::IntPredicate;
use inkwell::basic_block::BasicBlock;
use inkwell::module::Linkage;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, ValueKind};

use super::CodegenError;
use super::copy::mangle;
use super::lower::{Lowerer, POSITIONED};
use super::patterns::Subject;
use super::value::llvm_type;
use crate::ast::{BinOp, Expr, Pattern};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `(a, b, …)`: each element stored in its field, as a struct literal's.
    pub(super) fn tuple_literal(
        &mut self,
        expr: &Expr,
        given: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let ty = self.ty(expr);
        let fields = self.structs[&ty.to_string()].clone();
        let mut aggregate = llvm_type(self.context, &ty).into_struct_type().get_undef();
        for (index, (value, (_, field_ty))) in given.iter().zip(&fields).enumerate() {
            let value = self.value_for_store(value, field_ty)?;
            let stored = self.stored_form(value, field_ty);
            let index = u32::try_from(index).expect("element count fits in u32");
            aggregate = self
                .builder
                .build_insert_value(aggregate, stored, index, "tuple")
                .expect(POSITIONED)
                .into_struct_value();
        }
        Ok(aggregate.into())
    }

    /// `(p, q, …)` against a tuple `subject`: each element's pattern against
    /// its field, jumping to `next` on the first mismatch.
    pub(super) fn test_tuple(
        &mut self,
        given: &[Pattern],
        subject: Subject<'ctx>,
        ty: &Type,
        next: BasicBlock<'ctx>,
    ) {
        let slot = match subject {
            Subject::At(slot) => slot,
            Subject::Value(value) => {
                let slot = self.entry_alloca(ty, "tuple");
                self.builder.build_store(slot, value).expect(POSITIONED);
                slot
            }
        };
        let struct_type = llvm_type(self.context, ty).into_struct_type();
        let fields = self.structs[&ty.to_string()].clone();
        for (index, (pattern, (_, field_ty))) in given.iter().zip(&fields).enumerate() {
            let index = u32::try_from(index).expect("element count fits in u32");
            let address = self
                .builder
                .build_struct_gep(struct_type, slot, index, "element")
                .expect("the element exists");
            self.test(pattern, Subject::At(address), field_ty, next, false);
        }
    }

    /// `(a, b)`: each element's text inside a built-in form, joined.
    pub(super) fn show_tuple(
        &mut self,
        value: BasicValueEnum<'ctx>,
        fields: &[(String, Type)],
    ) -> BasicValueEnum<'ctx> {
        let record = value.into_struct_value();
        let mut text: BasicValueEnum = self.string_literal("(").into();
        for (index, (field, field_ty)) in fields.iter().enumerate() {
            let at = u32::try_from(index).expect("element count fits in u32");
            let raw = self
                .builder
                .build_extract_value(record, at, field)
                .expect("element exists");
            let element = self.loaded_form(raw, field_ty);
            if index > 0 {
                text = self.join(text, ", ");
            }
            let part = self.part(element, field_ty);
            text = self.concat(text, part);
        }
        self.join(text, ")")
    }

    /// `a op b` for an ordering `op` on tuples of type `ty`.
    pub(super) fn tuple_order(
        &mut self,
        op: BinOp,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> IntValue<'ctx> {
        let sign = self.order_of(a, b, ty);
        let predicate = match op {
            BinOp::Lt => IntPredicate::SLT,
            BinOp::Le => IntPredicate::SLE,
            BinOp::Gt => IntPredicate::SGT,
            _ => IntPredicate::SGE,
        };
        let zero = self.context.i32_type().const_zero();
        self.builder
            .build_int_compare(predicate, sign, zero, "tuple.cmp")
            .expect(POSITIONED)
    }

    /// −1, 0 or 1 as `a` is less than, equal to or greater than `b`.
    fn order_of(
        &mut self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> IntValue<'ctx> {
        let i32_type = self.context.i32_type();
        if *ty == Type::String {
            let params = [Type::String, Type::String];
            let cmp = self.runtime("lugha_rt_str_cmp", &params, Some(Type::I32));
            let call = self
                .builder
                .build_call(cmp, &[a.into(), b.into()], "cmp")
                .expect(POSITIONED);
            let ValueKind::Basic(sign) = call.try_as_basic_value() else {
                unreachable!("str_cmp returns an i32")
            };
            return sign.into_int_value();
        }
        // A struct or enum with `cmp` (v15, milestone 59).
        if super::ordering::user_ordered(ty) {
            return self.user_order(a, b, ty);
        }
        if crate::check::tuple_elements(ty).is_some() {
            let helper = self.cmp_function(ty);
            let call = self
                .builder
                .build_call(helper, &[a.into(), b.into()], "cmp")
                .expect(POSITIONED);
            let ValueKind::Basic(sign) = call.try_as_basic_value() else {
                unreachable!("cmp helpers return an i32")
            };
            return sign.into_int_value();
        }
        let less = self.compare(BinOp::Lt, ty.clone(), a, b);
        let more = self.compare(BinOp::Gt, ty.clone(), a, b);
        let (minus, one, zero) = (
            i32_type.const_all_ones(),
            i32_type.const_int(1, false),
            i32_type.const_zero(),
        );
        let above = self
            .builder
            .build_select(more, one, zero, "above")
            .expect(POSITIONED);
        self.builder
            .build_select(less, minus.into(), above, "order")
            .expect(POSITIONED)
            .into_int_value()
    }

    /// `lugha_cmp_<mangled>(T, T) -> i32`: the first unequal element decides.
    fn cmp_function(&mut self, ty: &Type) -> FunctionValue<'ctx> {
        let name = format!("lugha_cmp_{}", mangle(ty));
        if let Some(function) = self.module.get_function(&name) {
            return function;
        }
        let llvm = llvm_type(self.context, ty);
        let i32_type = self.context.i32_type();
        let function = self.module.add_function(
            &name,
            i32_type.fn_type(&[llvm.into(), llvm.into()], false),
            Some(Linkage::Internal),
        );
        let (saved_block, saved_function) = (self.builder.get_insert_block(), self.function);
        self.function = Some(function);
        self.builder
            .position_at_end(self.context.append_basic_block(function, "entry"));
        let param = |i| {
            function
                .get_nth_param(i)
                .expect("two parameters")
                .into_struct_value()
        };
        let (a, b) = (param(0), param(1));
        let fields = self.structs[&ty.to_string()].clone();
        for (index, (field, field_ty)) in fields.iter().enumerate() {
            let index = u32::try_from(index).expect("element count fits in u32");
            let read = |this: &Self, v| {
                let raw = this
                    .builder
                    .build_extract_value(v, index, field)
                    .expect("element exists");
                this.loaded_form(raw, field_ty)
            };
            let (x, y) = (read(self, a), read(self, b));
            let sign = self.order_of(x, y, field_ty);
            let differ = self
                .builder
                .build_int_compare(IntPredicate::NE, sign, i32_type.const_zero(), "differ")
                .expect(POSITIONED);
            let (done, more) = (self.append("decided"), self.append("next"));
            self.builder
                .build_conditional_branch(differ, done, more)
                .expect(POSITIONED);
            self.builder.position_at_end(done);
            self.builder.build_return(Some(&sign)).expect(POSITIONED);
            self.builder.position_at_end(more);
        }
        self.builder
            .build_return(Some(&i32_type.const_zero()))
            .expect(POSITIONED);
        self.function = saved_function;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        function
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn tuples_are_named_structs_and_compare_by_helper() {
        let ir = ir("fun main() { let t = (1, \"a\"); let b = t < (2, \"b\"); }");
        assert!(ir.contains("%\"(i64, string)\" = type"), "{ir}");
        assert!(ir.contains("@\"lugha_cmp_"), "{ir}");
    }
}
