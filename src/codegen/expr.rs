//! Integer expressions: literals, unary `-`, and `+ - * / %` on `i64`.
//!
//! Until milestone 4 adds panics, `+ - *` wrap (no `nsw` flags, so it is
//! defined behaviour) and `/ %` trap on a zero divisor or `MIN / -1`, both
//! of which are undefined behaviour in LLVM.

use inkwell::IntPredicate;
use inkwell::intrinsics::Intrinsic;
use inkwell::values::IntValue;

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED, unsupported};
use crate::ast::{BinOp, Expr, ExprKind, UnOp};

impl<'ctx> Lowerer<'ctx> {
    /// Lowers an integer expression to an `i64` value.
    pub(super) fn expr(&mut self, expr: &Expr) -> Result<IntValue<'ctx>, CodegenError> {
        let i64_type = self.context.i64_type();
        match &expr.kind {
            // Literals above i64::MAX keep their bits; range checks arrive with the checker.
            ExprKind::Int(value) => Ok(i64_type.const_int(*value, false)),
            ExprKind::Unary(UnOp::Neg, operand) => {
                let operand = self.expr(operand)?;
                Ok(self
                    .builder
                    .build_int_sub(i64_type.const_zero(), operand, "neg")
                    .expect(POSITIONED))
            }
            ExprKind::Binary(
                op @ (BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem),
                lhs,
                rhs,
            ) => {
                let lhs = self.expr(lhs)?;
                let rhs = self.expr(rhs)?;
                Ok(self.arithmetic(*op, lhs, rhs))
            }
            _ => Err(expr_unsupported(expr)),
        }
    }

    fn arithmetic(
        &mut self,
        op: BinOp,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
    ) -> IntValue<'ctx> {
        let b = &self.builder;
        match op {
            BinOp::Add => b.build_int_add(lhs, rhs, "add").expect(POSITIONED),
            BinOp::Sub => b.build_int_sub(lhs, rhs, "sub").expect(POSITIONED),
            BinOp::Mul => b.build_int_mul(lhs, rhs, "mul").expect(POSITIONED),
            BinOp::Div | BinOp::Rem => {
                self.trap_on_bad_divisor(lhs, rhs);
                let b = &self.builder;
                if op == BinOp::Div {
                    b.build_int_signed_div(lhs, rhs, "div").expect(POSITIONED)
                } else {
                    b.build_int_signed_rem(lhs, rhs, "rem").expect(POSITIONED)
                }
            }
            _ => unreachable!("only arithmetic operators reach here"),
        }
    }

    /// Branches to `llvm.trap` if `rhs == 0` or `lhs == MIN && rhs == -1`,
    /// leaving the builder in the block where dividing is safe.
    fn trap_on_bad_divisor(&mut self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) {
        let i64_type = self.context.i64_type();
        let b = &self.builder;
        let eq = |a, c, name| {
            b.build_int_compare(IntPredicate::EQ, a, c, name)
                .expect(POSITIONED)
        };
        let zero = eq(rhs, i64_type.const_zero(), "div.zero");
        let min = eq(lhs, i64_type.const_int(i64::MIN as u64, false), "div.min");
        let minus_one = eq(rhs, i64_type.const_all_ones(), "div.minus_one");
        let overflow = b
            .build_and(min, minus_one, "div.overflow")
            .expect(POSITIONED);
        let bad = b.build_or(zero, overflow, "div.bad").expect(POSITIONED);

        let function = self
            .function
            .expect("expressions are lowered inside a function");
        let trap_block = self.context.append_basic_block(function, "div.trap");
        let ok_block = self.context.append_basic_block(function, "div.ok");
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

/// The milestone that adds each expression form beyond integer arithmetic.
fn expr_unsupported(expr: &Expr) -> CodegenError {
    let (what, milestone) = match &expr.kind {
        ExprKind::Float(_) => ("floats", 3),
        ExprKind::Str(_) => ("strings", 4),
        ExprKind::Bool(_) => ("`true` and `false`", 2),
        ExprKind::Name(_) => ("variables", 2),
        ExprKind::Unary(UnOp::Not, _) => ("`!`", 2),
        ExprKind::Binary(BinOp::And | BinOp::Or, ..) => ("`&&` and `||`", 2),
        ExprKind::Binary(..) => ("comparisons", 2),
        ExprKind::Cast(..) => ("`as` casts", 3),
        ExprKind::Call(..) => ("function calls", 2),
        ExprKind::If { .. } => ("`if` expressions", 2),
        ExprKind::Block(_) => ("block expressions", 2),
        ExprKind::Index(..) => ("indexing", 5),
        ExprKind::Field(..) => ("field access", 5),
        ExprKind::StructLit(..) => ("structs", 5),
        ExprKind::Array(_) | ExprKind::Repeat(..) => ("arrays", 5),
        ExprKind::Int(_) | ExprKind::Unary(UnOp::Neg, _) => {
            unreachable!("supported in milestone 1")
        }
    };
    unsupported(what, milestone, expr.span)
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::{ir, unsupported};

    #[test]
    fn arithmetic_wraps_without_nsw_flags() {
        let ir = ir("fun main(): i32 { 0 - 9223372036854775807 * 3 + 1 }");
        assert!(!ir.contains("nsw") && !ir.contains("nuw"), "{ir}");
    }

    #[test]
    fn division_and_remainder_are_guarded_by_a_trap() {
        for op in ["/", "%"] {
            let ir = ir(&format!("fun main(): i32 {{ 10 {op} 3 }}"));
            assert!(ir.contains("@llvm.trap()"), "{op}: {ir}");
            assert!(ir.contains("unreachable"), "{op}: {ir}");
        }
    }

    #[test]
    fn expressions_beyond_milestone_1_are_unsupported() {
        let cases = [
            ("fun main() { \"hi\" }", ("strings", 4, "\"hi\"")),
            ("fun main() { 1.5 }", ("floats", 3, "1.5")),
            (
                "fun main(): i32 { if 1 { 2 } else { 3 } }",
                ("`if` expressions", 2, "if 1 { 2 } else { 3 }"),
            ),
            ("fun main(): i32 { f(1) }", ("function calls", 2, "f(1)")),
            ("fun main(): i32 { 1 + x }", ("variables", 2, "x")),
            ("fun main(): i32 { 1 < 2 }", ("comparisons", 2, "1 < 2")),
            (
                "fun main(): i32 { 1 as i32 }",
                ("`as` casts", 3, "1 as i32"),
            ),
            // The outermost unsupported construct is reported first.
            ("fun main(): i32 { [1][0] }", ("indexing", 5, "[1][0]")),
            ("fun main() { [1, 2] }", ("arrays", 5, "[1, 2]")),
        ];
        for (src, want) in cases {
            assert_eq!(unsupported(src), want, "{src}");
        }
    }
}
