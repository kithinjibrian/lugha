//! Expressions: literals, names, arithmetic, comparisons, `!`, short-circuit
//! `&&`/`||`, and dispatch to blocks and `if` (spec §5).
//!
//! Until milestone 4 adds panics, `+ - *` wrap (no `nsw` flags, so it is
//! defined behaviour) and `/ %` trap on a zero divisor or `MIN / -1`, both
//! of which are undefined behaviour in LLVM.

use inkwell::IntPredicate;
use inkwell::intrinsics::Intrinsic;
use inkwell::values::IntValue;

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED, unsupported};
use super::value::{Kind, Value, type_error};
use crate::ast::{BinOp, Expr, ExprKind, UnOp};

impl<'ctx> Lowerer<'ctx> {
    /// Lowers an expression to a value.
    pub(super) fn expr(&mut self, expr: &Expr) -> Result<Value<'ctx>, CodegenError> {
        Ok(match &expr.kind {
            // Literals above i64::MAX keep their bits; range checks arrive with the checker.
            ExprKind::Int(value) => Value::Int(self.context.i64_type().const_int(*value, false)),
            ExprKind::Bool(value) => {
                Value::Bool(self.context.bool_type().const_int(u64::from(*value), false))
            }
            ExprKind::Name(name) => {
                let Some(local) = self.scopes.lookup(name) else {
                    // v0 has no function values (spec §6).
                    let what = if self.functions.contains_key(name) {
                        "type checking"
                    } else {
                        "checking undefined names"
                    };
                    return Err(unsupported(what, 3, expr.span));
                };
                Value::of(local.kind, self.load(local, name))
            }
            ExprKind::Unary(UnOp::Neg, operand) => {
                let operand = self.expr(operand)?.int(operand.span)?;
                let zero = self.context.i64_type().const_zero();
                Value::Int(
                    self.builder
                        .build_int_sub(zero, operand, "neg")
                        .expect(POSITIONED),
                )
            }
            ExprKind::Unary(UnOp::Not, operand) => {
                let operand = self.expr(operand)?.bool(operand.span)?;
                Value::Bool(self.builder.build_not(operand, "not").expect(POSITIONED))
            }
            ExprKind::Binary(op @ (BinOp::And | BinOp::Or), lhs, rhs) => {
                self.short_circuit(*op, lhs, rhs)?
            }
            ExprKind::Binary(op, lhs, rhs) => self.binary(*op, lhs, rhs)?,
            ExprKind::If { cond, then, else_ } => {
                self.if_expr(expr.span, cond, then, else_.as_deref())?
            }
            ExprKind::Block(block) => self.block(block)?,
            ExprKind::Call(callee, args) => self.call(expr, callee, args)?,
            _ => return Err(expr_unsupported(expr)),
        })
    }

    fn binary(&mut self, op: BinOp, lhs: &Expr, rhs: &Expr) -> Result<Value<'ctx>, CodegenError> {
        let left = self.expr(lhs)?;
        let right = self.expr(rhs)?;
        let compare = |predicate| (predicate, "cmp");
        let (predicate, name) = match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
                let (a, b) = (left.int(lhs.span)?, right.int(rhs.span)?);
                return Ok(Value::Int(self.arithmetic(op, a, b)));
            }
            BinOp::Eq | BinOp::Ne => {
                let (a, b) = match (left, right) {
                    (Value::Int(a), Value::Int(b)) | (Value::Bool(a), Value::Bool(b)) => (a, b),
                    _ => return Err(type_error(rhs.span)),
                };
                let predicate = if op == BinOp::Eq {
                    IntPredicate::EQ
                } else {
                    IntPredicate::NE
                };
                let result = self
                    .builder
                    .build_int_compare(predicate, a, b, "eq")
                    .expect(POSITIONED);
                return Ok(Value::Bool(result));
            }
            BinOp::Lt => compare(IntPredicate::SLT),
            BinOp::Le => compare(IntPredicate::SLE),
            BinOp::Gt => compare(IntPredicate::SGT),
            BinOp::Ge => compare(IntPredicate::SGE),
            BinOp::And | BinOp::Or => {
                unreachable!("short-circuit operators are lowered separately")
            }
        };
        let (a, b) = (left.int(lhs.span)?, right.int(rhs.span)?);
        Ok(Value::Bool(
            self.builder
                .build_int_compare(predicate, a, b, name)
                .expect(POSITIONED),
        ))
    }

    /// `&&` / `||`: the right operand runs in its own block only when needed,
    /// and a `phi` joins the result (spec §5, §9).
    fn short_circuit(
        &mut self,
        op: BinOp,
        lhs: &Expr,
        rhs: &Expr,
    ) -> Result<Value<'ctx>, CodegenError> {
        let left = self.expr(lhs)?.bool(lhs.span)?;
        let left_end = self.current_block();
        let rhs_block = self.append("logic.rhs");
        let merge = self.append("logic.end");
        let (on_true, on_false) = if op == BinOp::And {
            (rhs_block, merge)
        } else {
            (merge, rhs_block)
        };
        self.builder
            .build_conditional_branch(left, on_true, on_false)
            .expect(POSITIONED);

        self.builder.position_at_end(rhs_block);
        let right = self.expr(rhs)?.bool(rhs.span)?;
        let right_end = self.current_block();
        self.builder
            .build_unconditional_branch(merge)
            .expect(POSITIONED);

        self.builder.position_at_end(merge);
        // Skipping the right operand means `false` for `&&` and `true` for `||`.
        let skipped = self
            .context
            .bool_type()
            .const_int(u64::from(op == BinOp::Or), false);
        let phi = self
            .builder
            .build_phi(Kind::Bool.llvm(self.context), "logic")
            .expect(POSITIONED);
        phi.add_incoming(&[(&skipped, left_end), (&right, right_end)]);
        Ok(Value::Bool(phi.as_basic_value().into_int_value()))
    }

    /// `+ - * / %` on `i64`: wrapping, with `/ %` guarded by a trap.
    pub(super) fn arithmetic(
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

/// The milestone that adds each expression form codegen can't lower yet.
fn expr_unsupported(expr: &Expr) -> CodegenError {
    let (what, milestone) = match &expr.kind {
        ExprKind::Float(_) => ("floats", 3),
        ExprKind::Str(_) => ("strings", 4),
        ExprKind::Cast(..) => ("`as` casts", 3),
        ExprKind::Index(..) => ("indexing", 5),
        ExprKind::Field(..) => ("field access", 5),
        ExprKind::StructLit(..) => ("structs", 5),
        ExprKind::Array(_) | ExprKind::Repeat(..) => ("arrays", 5),
        _ => unreachable!("lowered by Lowerer::expr"),
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
    fn short_circuit_joins_with_a_phi() {
        let ir = ir("fun main() { let b = true && false; let c = b || true; }");
        assert_eq!(ir.matches("phi i1").count(), 2, "{ir}");
    }

    #[test]
    fn kinds_must_match() {
        assert_eq!(
            unsupported("fun main(): i32 { 1 + true }"),
            ("type checking", 3, "true")
        );
        assert_eq!(
            unsupported("fun main() { let b = 1 == true; }"),
            ("type checking", 3, "true")
        );
        assert_eq!(
            unsupported("fun main() { let b = !1; }"),
            ("type checking", 3, "1")
        );
        assert_eq!(
            unsupported("fun main(): i32 { 1 < 2 }"),
            ("type checking", 3, "1 < 2")
        );
        assert_eq!(
            unsupported("fun main(): i32 { y + 1 }"),
            ("checking undefined names", 3, "y")
        );
    }

    #[test]
    fn expressions_beyond_milestone_2_are_unsupported() {
        let cases = [
            ("fun main() { \"hi\" }", ("strings", 4, "\"hi\"")),
            ("fun main() { 1.5 }", ("floats", 3, "1.5")),
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
