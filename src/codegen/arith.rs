//! Binary arithmetic and comparisons at each type's width (spec §4, §5).
//!
//! Integers wrap (no `nsw`) and integer `/ %` trap on a bad divisor until
//! milestone 4 adds panics. `f64` follows IEEE 754: no traps, and `!=` is
//! true when either side is NaN.

use inkwell::intrinsics::Intrinsic;
use inkwell::values::{BasicValueEnum, IntValue};
use inkwell::{FloatPredicate, IntPredicate};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::is_signed;
use crate::ast::{BinOp, Expr};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `lhs op rhs` for arithmetic and comparison operators.
    pub(super) fn binary(
        &mut self,
        op: BinOp,
        lhs: &Expr,
        rhs: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Both operands share one type (spec §4); a diverging side has no type of its own.
        let ty = [self.ty(lhs), self.ty(rhs)]
            .into_iter()
            .find(|t| !matches!(t, Type::Never | Type::Error))
            .unwrap_or(Type::I64);
        let a = self.get(lhs, ty)?;
        let b = self.get(rhs, ty)?;
        Ok(match op {
            BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge | BinOp::Eq | BinOp::Ne => {
                self.compare(op, ty, a, b).into()
            }
            _ => self.arithmetic(op, ty, a, b),
        })
    }

    /// `+ - * / %` on values of type `ty`.
    pub(super) fn arithmetic(
        &mut self,
        op: BinOp,
        ty: Type,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let builder = &self.builder;
        if ty == Type::F64 {
            let (a, b) = (a.into_float_value(), b.into_float_value());
            return match op {
                BinOp::Add => builder.build_float_add(a, b, "fadd"),
                BinOp::Sub => builder.build_float_sub(a, b, "fsub"),
                BinOp::Mul => builder.build_float_mul(a, b, "fmul"),
                BinOp::Div => builder.build_float_div(a, b, "fdiv"),
                BinOp::Rem => builder.build_float_rem(a, b, "frem"),
                _ => unreachable!("only arithmetic operators reach here"),
            }
            .expect(POSITIONED)
            .into();
        }
        let (a, b) = (a.into_int_value(), b.into_int_value());
        let signed = is_signed(ty);
        match op {
            BinOp::Add => builder.build_int_add(a, b, "add").expect(POSITIONED).into(),
            BinOp::Sub => builder.build_int_sub(a, b, "sub").expect(POSITIONED).into(),
            BinOp::Mul => builder.build_int_mul(a, b, "mul").expect(POSITIONED).into(),
            BinOp::Div | BinOp::Rem => {
                self.trap_on_bad_divisor(a, b, signed);
                let builder = &self.builder;
                let result = match (op, signed) {
                    (BinOp::Div, true) => builder.build_int_signed_div(a, b, "div"),
                    (BinOp::Div, false) => builder.build_int_unsigned_div(a, b, "div"),
                    (_, true) => builder.build_int_signed_rem(a, b, "rem"),
                    (_, false) => builder.build_int_unsigned_rem(a, b, "rem"),
                };
                result.expect(POSITIONED).into()
            }
            _ => unreachable!("only arithmetic operators reach here"),
        }
    }

    /// Comparisons: signed for `i32`/`i64`, unsigned for `u8`, ordered for
    /// `f64` except `!=` (unordered, so NaN != NaN).
    fn compare(
        &self,
        op: BinOp,
        ty: Type,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> IntValue<'ctx> {
        if ty == Type::F64 {
            let predicate = match op {
                BinOp::Lt => FloatPredicate::OLT,
                BinOp::Le => FloatPredicate::OLE,
                BinOp::Gt => FloatPredicate::OGT,
                BinOp::Ge => FloatPredicate::OGE,
                BinOp::Eq => FloatPredicate::OEQ,
                _ => FloatPredicate::UNE,
            };
            let (a, b) = (a.into_float_value(), b.into_float_value());
            return self
                .builder
                .build_float_compare(predicate, a, b, "fcmp")
                .expect(POSITIONED);
        }
        let signed = is_signed(ty);
        let predicate = match op {
            BinOp::Lt if signed => IntPredicate::SLT,
            BinOp::Le if signed => IntPredicate::SLE,
            BinOp::Gt if signed => IntPredicate::SGT,
            BinOp::Ge if signed => IntPredicate::SGE,
            BinOp::Lt => IntPredicate::ULT,
            BinOp::Le => IntPredicate::ULE,
            BinOp::Gt => IntPredicate::UGT,
            BinOp::Ge => IntPredicate::UGE,
            BinOp::Eq => IntPredicate::EQ,
            _ => IntPredicate::NE,
        };
        let (a, b) = (a.into_int_value(), b.into_int_value());
        self.builder
            .build_int_compare(predicate, a, b, "icmp")
            .expect(POSITIONED)
    }

    /// Branches to `llvm.trap` on a zero divisor, or (signed) `MIN / -1` at
    /// this width, leaving the builder where dividing is safe.
    fn trap_on_bad_divisor(&mut self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>, signed: bool) {
        let int = lhs.get_type();
        let b = &self.builder;
        let eq = |x, y, name| {
            b.build_int_compare(IntPredicate::EQ, x, y, name)
                .expect(POSITIONED)
        };
        let mut bad = eq(rhs, int.const_zero(), "div.zero");
        if signed {
            let min = int.const_int(1 << (int.get_bit_width() - 1), false);
            let overflow = b
                .build_and(
                    eq(lhs, min, "div.min"),
                    eq(rhs, int.const_all_ones(), "div.minus_one"),
                    "div.overflow",
                )
                .expect(POSITIONED);
            bad = b.build_or(bad, overflow, "div.bad").expect(POSITIONED);
        }
        let trap_block = self.append("div.trap");
        let ok_block = self.append("div.ok");
        self.builder
            .build_conditional_branch(bad, trap_block, ok_block)
            .expect(POSITIONED);

        self.builder.position_at_end(trap_block);
        let trap = Intrinsic::find("llvm.trap")
            .and_then(|t| t.get_declaration(&self.module, &[]))
            .expect("LLVM 21 provides the non-overloaded llvm.trap intrinsic");
        self.builder.build_call(trap, &[], "").expect(POSITIONED);
        self.builder.build_unreachable().expect(POSITIONED);
        self.builder.position_at_end(ok_block);
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn integers_use_their_width_and_wrap() {
        let ir = ir("fun main(): i32 { let x: i32 = 1; x + 2 }");
        assert!(ir.contains("add i32"), "{ir}");
        assert!(!ir.contains("nsw") && !ir.contains("nuw"), "{ir}");
    }

    #[test]
    fn u8_divides_unsigned_with_only_a_zero_guard() {
        let ir = ir("fun main() { let a: u8 = 200; let b = a / 3; let c = a < b; }");
        assert!(ir.contains("udiv i8"), "{ir}");
        assert!(ir.contains("icmp ult i8"), "{ir}");
        assert!(ir.contains("@llvm.trap()"), "{ir}");
        assert!(!ir.contains("div.min"), "no MIN guard for unsigned: {ir}");
    }

    #[test]
    fn signed_division_guards_min_at_its_width() {
        let ir = ir("fun main() { let a: i32 = 7; let b = a / 2; }");
        assert!(ir.contains("sdiv i32"), "{ir}");
        assert!(
            ir.contains("icmp eq i32") && ir.contains(", -2147483648"),
            "MIN guard at 32 bits: {ir}"
        );
    }

    #[test]
    fn f64_is_ieee() {
        let ir = ir(
            "fun main() { let a = 1.5; let b = a / 0.0; let c = a < b; let d = a != b; let e = a % 2.0; }",
        );
        assert!(ir.contains("fdiv double"), "{ir}");
        assert!(ir.contains("fcmp olt double"), "{ir}");
        assert!(ir.contains("fcmp une double"), "{ir}");
        assert!(ir.contains("frem double"), "{ir}");
        assert!(!ir.contains("llvm.trap"), "{ir}");
    }
}
