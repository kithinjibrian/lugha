//! Binary arithmetic and comparisons at each type's width (spec §4, §5, §9).
//!
//! Integer `+ - *` are checked with `llvm.{s,u}{add,sub,mul}.with.overflow`
//! and `/ %` check for zero and (signed) `MIN / -1`; a failure panics with
//! `integer overflow` or `division by zero` at the operator. `f64` follows
//! IEEE 754: no panics, and `!=` is true when either side is NaN.

use inkwell::intrinsics::Intrinsic;
use inkwell::values::{BasicValueEnum, IntValue, ValueKind};
use inkwell::{FloatPredicate, IntPredicate};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::is_signed;
use crate::ast::{BinOp, Expr};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `lhs op rhs` for arithmetic and comparison operators; `at` is the
    /// operator's source offset, where a panic points.
    pub(super) fn binary(
        &mut self,
        op: BinOp,
        at: usize,
        lhs: &Expr,
        rhs: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Both operands share one type (spec §4); a diverging side has no type of its own.
        let ty = [self.ty(lhs), self.ty(rhs)]
            .into_iter()
            .find(|t| !matches!(t, Type::Never | Type::Error))
            .unwrap_or(Type::I64);
        let a = self.get(lhs, &ty)?;
        let b = self.get(rhs, &ty)?;
        if ty == Type::String {
            return Ok(self.string_binary(op, a, b));
        }
        Ok(match op {
            BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge | BinOp::Eq | BinOp::Ne => {
                self.compare(op, ty, a, b).into()
            }
            _ => self.arithmetic(op, ty, a, b, at),
        })
    }

    /// `+ - * / %` on values of type `ty`, panicking at `at` on integer
    /// overflow or a bad divisor.
    pub(super) fn arithmetic(
        &mut self,
        op: BinOp,
        ty: Type,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
        at: usize,
    ) -> BasicValueEnum<'ctx> {
        if ty == Type::F64 {
            let builder = &self.builder;
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
        let signed = is_signed(&ty);
        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul => self.checked(op, a, b, signed, at).into(),
            BinOp::Div | BinOp::Rem => {
                self.check_divisor(a, b, signed, at);
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

    /// `a op b` through `llvm.{s,u}{add,sub,mul}.with.overflow.iN`.
    fn checked(
        &mut self,
        op: BinOp,
        a: IntValue<'ctx>,
        b: IntValue<'ctx>,
        signed: bool,
        at: usize,
    ) -> IntValue<'ctx> {
        let operation = match op {
            BinOp::Add => "add",
            BinOp::Sub => "sub",
            _ => "mul",
        };
        let name = format!(
            "llvm.{}{operation}.with.overflow",
            if signed { "s" } else { "u" }
        );
        let intrinsic = Intrinsic::find(&name)
            .and_then(|i| i.get_declaration(&self.module, &[a.get_type().into()]))
            .expect("LLVM 21 provides the overflow intrinsics");
        let call = self
            .builder
            .build_call(intrinsic, &[a.into(), b.into()], "checked")
            .expect(POSITIONED);
        let ValueKind::Basic(pair) = call.try_as_basic_value() else {
            unreachable!("overflow intrinsics return {{ iN, i1 }}")
        };
        let pair = pair.into_struct_value();
        let value = self
            .builder
            .build_extract_value(pair, 0, "value")
            .expect(POSITIONED)
            .into_int_value();
        let overflow = self
            .builder
            .build_extract_value(pair, 1, "overflow")
            .expect(POSITIONED)
            .into_int_value();
        self.panic_if(overflow, "integer overflow", at);
        value
    }

    /// Panics on a zero divisor, and on signed `MIN / -1`, which overflows (spec §5).
    fn check_divisor(&mut self, a: IntValue<'ctx>, b: IntValue<'ctx>, signed: bool, at: usize) {
        let int = a.get_type();
        let eq = |this: &Self, x, y, name| {
            this.builder
                .build_int_compare(IntPredicate::EQ, x, y, name)
                .expect(POSITIONED)
        };
        let zero = eq(self, b, int.const_zero(), "div.zero");
        self.panic_if(zero, "division by zero", at);
        if signed {
            let min = int.const_int(1 << (int.get_bit_width() - 1), false);
            let (is_min, is_minus_one) = (
                eq(self, a, min, "div.min"),
                eq(self, b, int.const_all_ones(), "div.minus_one"),
            );
            let overflow = self
                .builder
                .build_and(is_min, is_minus_one, "div.overflow")
                .expect(POSITIONED);
            self.panic_if(overflow, "integer overflow", at);
        }
    }

    /// Branches to a panic with `message` at `at` when `condition` holds,
    /// leaving the builder on the path where it doesn't.
    fn panic_if(&mut self, condition: IntValue<'ctx>, message: &str, at: usize) {
        let panic_block = self.append("panic");
        let ok_block = self.append("ok");
        self.builder
            .build_conditional_branch(condition, panic_block, ok_block)
            .expect(POSITIONED);
        self.builder.position_at_end(panic_block);
        self.emit_panic(message, at);
        self.builder.position_at_end(ok_block);
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
        let signed = is_signed(&ty);
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
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn integer_arithmetic_is_checked_at_its_width() {
        let ir = ir("fun main(): i32 { let x: i32 = 1; let b: u8 = 2; let c = b * b; x + 2 }");
        assert!(ir.contains("@llvm.sadd.with.overflow.i32"), "{ir}");
        assert!(ir.contains("@llvm.umul.with.overflow.i8"), "{ir}");
        assert!(ir.contains("@lugha_rt_panic"), "{ir}");
        assert!(!ir.contains("llvm.trap"), "{ir}");
    }

    #[test]
    fn u8_divides_unsigned_with_only_a_zero_check() {
        let ir = ir("fun main() { let a: u8 = 200; let b = a / 3; let c = a < b; }");
        assert!(ir.contains("udiv i8"), "{ir}");
        assert!(ir.contains("icmp ult i8"), "{ir}");
        assert!(ir.contains("division by zero"), "{ir}");
        assert!(!ir.contains("div.min"), "no MIN check for unsigned: {ir}");
    }

    #[test]
    fn signed_division_checks_min_at_its_width() {
        let ir = ir("fun main() { let a: i32 = 7; let b = a / 2; }");
        assert!(ir.contains("sdiv i32"), "{ir}");
        assert!(
            ir.contains("icmp eq i32") && ir.contains(", -2147483648"),
            "MIN check at 32 bits: {ir}"
        );
    }

    #[test]
    fn panics_point_at_the_operator() {
        let ir = ir("fun main() {\n    let x: i64 = 1;\n    let y = x  *  x;\n}");
        assert!(ir.contains("i64 3, i64 16)"), "line 3, column of `*`: {ir}");
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
        assert!(!ir.contains("lugha_rt_panic"), "{ir}");
    }
}
