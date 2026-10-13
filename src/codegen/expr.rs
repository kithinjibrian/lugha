//! Expressions: literals, names, unary operators, short-circuit `&&`/`||`,
//! and dispatch to arithmetic, casts, calls, blocks and `if` (spec §5).

use inkwell::values::BasicValueEnum;

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::{Value, int_type};
use crate::ast::{BinOp, Expr, ExprKind, UnOp};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// Lowers an expression.
    pub(super) fn expr(&mut self, expr: &Expr) -> Result<Value<'ctx>, CodegenError> {
        let value: BasicValueEnum<'ctx> = match &expr.kind {
            ExprKind::Int(_) | ExprKind::Float(_) => self.literal(expr, false),
            // `-` directly on a literal is part of it (spec §4 rule 6): `-2147483648` is one i32.
            ExprKind::Unary(UnOp::Neg, inner)
                if matches!(inner.kind, ExprKind::Int(_) | ExprKind::Float(_)) =>
            {
                self.literal(inner, true)
            }
            ExprKind::Bool(value) => self
                .context
                .bool_type()
                .const_int(u64::from(*value), false)
                .into(),
            ExprKind::Name(name) if name == "None" && self.scopes.find(name).is_none() => {
                let ty = self.ty(expr);
                self.construct(&ty, "None", &[])?
            }
            ExprKind::Name(name) => {
                let local = self.scopes.lookup(name);
                self.load(&local, name)
            }
            ExprKind::Unary(UnOp::Neg, operand) => {
                let ty = self.ty(expr);
                let value = self.get(operand, &ty)?;
                if ty == Type::F64 {
                    self.builder
                        .build_float_neg(value.into_float_value(), "neg")
                        .expect(POSITIONED)
                        .into()
                } else {
                    // Checked `0 - x`: negating the minimum overflows (spec §5).
                    let zero = int_type(self.context, &ty).const_zero().into();
                    self.arithmetic(BinOp::Sub, ty, zero, value, expr.span.start)
                }
            }
            ExprKind::Unary(UnOp::Not, operand) => {
                let value = self.get(operand, &Type::Bool)?.into_int_value();
                self.builder
                    .build_not(value, "not")
                    .expect(POSITIONED)
                    .into()
            }
            ExprKind::Binary(op @ (BinOp::And | BinOp::Or), _, lhs, rhs) => {
                self.short_circuit(*op, lhs, rhs)?
            }
            ExprKind::Binary(op, op_span, lhs, rhs) => self.binary(*op, op_span.start, lhs, rhs)?,
            ExprKind::Cast(inner, _) => self.cast(expr, inner)?,
            ExprKind::Call(callee, args) => {
                // `f(…)` and `m.f(…)`: the checker named the function (PRP-034).
                if self
                    .callees
                    .get(&expr.id)
                    .is_some_and(|key| self.functions.contains_key(key))
                {
                    return self.call(expr, args);
                }
                if let Some(value) = self.method_call(expr, callee, args)? {
                    return Ok(value);
                }
                match self.constructed_variant(callee) {
                    Some(variant) => {
                        let ty = self.ty(expr);
                        self.construct(&ty, &variant, args)?
                    }
                    None => return self.call(expr, args),
                }
            }
            ExprKind::If { cond, then, else_ } => {
                return self.if_expr(expr, cond, then, else_.as_deref());
            }
            ExprKind::Block(block) => return self.block(block),
            ExprKind::Match(scrutinee, arms) => return self.match_expr(expr, scrutinee, arms),
            ExprKind::Str(text) => self.string_literal(text).into(),
            ExprKind::Interp(parts) => self.interpolation(parts)?,
            ExprKind::Index(base, open, index) => self.index(base, open.start, index)?,
            // The checker gives a type name `void`: `Shape.Empty` is a variant.
            ExprKind::Field(base, field) if self.ty(base) == Type::Void => {
                let ty = self.ty(expr);
                self.construct(&ty, &field.name, &[])?
            }
            ExprKind::Field(base, field) => self.field(base, field)?,
            ExprKind::StructLit(lit) => self.struct_literal(expr, &lit.fields)?,
            ExprKind::Array(elements) => self.array_literal(expr, elements)?,
            ExprKind::Repeat(value, count) => self.repeat(expr, value, count)?,
        };
        Ok(Value::Val(value))
    }

    /// A numeric literal at its checked type; `negate` folds a leading `-`.
    fn literal(&self, literal: &Expr, negate: bool) -> BasicValueEnum<'ctx> {
        match literal.kind {
            ExprKind::Float(value) => {
                let value = if negate { -value } else { value };
                self.context.f64_type().const_float(value).into()
            }
            ExprKind::Int(value) => {
                // Two's complement at the target width: the bits of -v, truncated by LLVM.
                let bits = if negate { value.wrapping_neg() } else { value };
                int_type(self.context, &self.ty(literal))
                    .const_int(bits, false)
                    .into()
            }
            _ => unreachable!("only numeric literals reach here"),
        }
    }

    /// `&&` / `||`: the right operand runs in its own block only when needed,
    /// and a `phi` joins the result (spec §5, §9).
    fn short_circuit(
        &mut self,
        op: BinOp,
        lhs: &Expr,
        rhs: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let left = self.get(lhs, &Type::Bool)?.into_int_value();
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
        let right = self.get(rhs, &Type::Bool)?.into_int_value();
        let right_end = self.current_block();
        self.builder
            .build_unconditional_branch(merge)
            .expect(POSITIONED);

        self.builder.position_at_end(merge);
        // Skipping the right operand means `false` for `&&` and `true` for `||`.
        let bool_type = self.context.bool_type();
        let skipped = bool_type.const_int(u64::from(op == BinOp::Or), false);
        let phi = self
            .builder
            .build_phi(bool_type, "logic")
            .expect(POSITIONED);
        phi.add_incoming(&[(&skipped, left_end), (&right, right_end)]);
        Ok(phi.as_basic_value())
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn negated_literals_are_single_constants() {
        let ir = ir("fun main(): i32 { let m: i32 = -2147483648; m }");
        assert!(ir.contains("i32 -2147483648"), "{ir}");
        assert!(!ir.contains("sub i32 0"), "{ir}");
    }

    #[test]
    fn short_circuit_joins_with_a_phi() {
        let ir = ir("fun main() { let b = true && false; let c = b || true; }");
        assert_eq!(ir.matches("phi i1").count(), 2, "{ir}");
    }

    #[test]
    fn f64_negation_is_fneg() {
        let ir = ir("fun main() { let x = 2.5; let y = -x; }");
        assert!(ir.contains("fneg double"), "{ir}");
    }
}
