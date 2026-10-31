//! Expressions: literals, names, unary operators, short-circuit `&&`/`||`,
//! and dispatch to arithmetic, casts, calls, blocks and `if` (spec §5).

use inkwell::values::BasicValueEnum;

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::{Value, int_type};
use crate::ast::{BinOp, Expr, ExprKind, UnOp};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// Lowers an expression, converting it to an interface value where the
    /// checker recorded a conversion (v5).
    pub(super) fn expr(&mut self, expr: &Expr) -> Result<Value<'ctx>, CodegenError> {
        // A spawned call's argument, evaluated by the spawning thread (v17).
        if let Some(value) = self.preset(expr) {
            return Ok(Value::Val(value));
        }
        match self.conversions.get(&expr.id).cloned() {
            Some(to) => self.converted(expr, &to),
            None => self.unconverted(expr),
        }
    }

    /// Lowers an expression at its own type.
    pub(super) fn unconverted(&mut self, expr: &Expr) -> Result<Value<'ctx>, CodegenError> {
        let value: BasicValueEnum<'ctx> = match &expr.kind {
            ExprKind::Pass(..) => {
                unreachable!("checked: `out`/`ptr`/`mut` only as an extern's argument")
            }
            ExprKind::Hashed(lit) => self.hashed(expr, lit)?,
            ExprKind::Int(_) | ExprKind::Float(_) => self.literal(expr, false),
            ExprKind::Char(code) => self
                .context
                .i32_type()
                .const_int(u64::from(*code), false)
                .into(),
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
            // A function used as a value (v3): the checker recorded which.
            // A variant used as a function value (v17).
            ExprKind::Name(_) | ExprKind::Field(..)
                if self.variant_values.contains_key(&expr.id) =>
            {
                self.variant_function_value(expr)
            }
            ExprKind::Name(_) | ExprKind::Field(..) if self.callees.contains_key(&expr.id) => {
                self.function_value(expr)
            }
            // A constant (v17, milestone 109).
            ExprKind::Name(_) | ExprKind::Field(..) if self.const_uses.contains_key(&expr.id) => {
                self.const_value(expr)
            }
            ExprKind::Name(name) => {
                let local = self.scopes.lookup(name);
                self.load(&local, name)
            }
            // `-v` on a struct, enum or interface value calls `neg` (v15).
            ExprKind::Unary(UnOp::Neg, operand)
                if let Some(value) = self.operator(expr, "neg", operand, None)? =>
            {
                value
            }
            ExprKind::Unary(UnOp::Neg, operand) => {
                let ty = self.ty(expr);
                let value = self.get(operand, &ty)?;
                if ty.is_float() {
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
            ExprKind::Unary(UnOp::BitNot, operand) => {
                let value = self.get(operand, &self.ty(expr))?.into_int_value();
                self.builder
                    .build_not(value, "bitnot")
                    .expect(POSITIONED)
                    .into()
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
            // `a + b` on a struct, enum or interface value calls `add` (v15).
            ExprKind::Binary(op, _, lhs, rhs)
                if let Some(name) = super::arith::method_of(*op)
                    && let Some(value) = self.operator(expr, name, lhs, Some(rhs))? =>
            {
                value
            }
            ExprKind::Binary(op, op_span, lhs, rhs) => self.binary(*op, op_span.start, lhs, rhs)?,
            ExprKind::Cast(inner, _) => self.cast(expr, inner)?,
            // `$float.sqrt(x)` in `std.math` (v15).
            ExprKind::Call(callee, args) if let Some(op) = crate::check::float_op(callee) => {
                self.float_call(expr, op, args)?
            }
            // `f64.from_bits(b)` (v17, milestone 108): the same bits as a float.
            ExprKind::Call(callee, args)
                if let Some((float, _)) = crate::check::from_bits(callee) =>
            {
                let bits = self.get(&args[0], &crate::check::bits_of(&float))?;
                self.bitcast(bits, &float)
            }
            // `$os.sleep(ms)` and `$os.yield()` in `std.task` (v17).
            ExprKind::Call(callee, args)
                if let Some(op @ ("sleep" | "yield")) = crate::check::os_op(callee) =>
            {
                return self.task_os_call(op, args);
            }
            // `$os.seed()` in `std.random`, `$os.env(name)` in `std.env`, … (v15).
            ExprKind::Call(callee, args) if crate::check::hash_op(callee).is_some() => {
                self.hash_call(args)?
            }
            // `derive.fields(x, ctx, f)` and the rest in derive blocks (v17, milestones 141–142).
            ExprKind::Call(_, args) if let Some(op) = self.derive_calls.get(&expr.id).cloned() => {
                self.derive_intrinsic(expr, &op, args)?
            }
            ExprKind::Call(callee, args) if let Some(op) = crate::check::os_op(callee) => {
                self.os_call(expr, op, args)?
            }
            // `$string.from_bytes(bytes)` in the prelude (v15).
            ExprKind::Call(callee, args) if crate::check::string_op(callee).is_some() => {
                self.string_from_bytes(args)?
            }
            ExprKind::Call(callee, args) if matches!(self.ty(callee), Type::Fun(..)) => {
                return self.call_value(callee, args);
            }
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
            ExprKind::IfLet(parts) => return self.if_let(expr, parts),
            ExprKind::Block(block) => return self.block(block),
            ExprKind::Loop { label, body } => return self.loop_expr(expr, label.as_ref(), body),
            ExprKind::Match(scrutinee, arms) => return self.match_expr(expr, scrutinee, arms),
            ExprKind::Str(text) => self.string_literal(text).into(),
            ExprKind::Interp(parts) => self.interpolation(parts)?,
            // `format` (v14): its template's parts, lowered as interpolation.
            ExprKind::Format(call) => {
                let pieces = crate::template::pieces(&call.template)
                    .expect("the checker accepted the template");
                let exact = crate::check::template_exact(call);
                let parts = crate::template::parts(&pieces, &call.args, call.template_span, exact);
                self.interpolation(&parts)?
            }
            ExprKind::Closure(c) => self.closure_value(expr, c)?,
            ExprKind::Try(inner) => return self.try_value(inner),
            // `m[k]` on a map (v17): the `at` the checker recorded here.
            ExprKind::Index(base, open, index) if self.is_map(base) => {
                self.map_entry(expr, base, index, open.start)?
            }
            ExprKind::Index(base, open, index) => self.index(base, open.start, index)?,
            // `i16.MAX` (v10): the checker typed the keyword `void` too.
            ExprKind::Field(base, field) if Type::int_limit(base, &field.name).is_some() => {
                let (ty, max) = Type::int_limit(base, &field.name).expect("just matched");
                self.int_limit(&ty, max).into()
            }
            // `CPtr.null` (v17): a null pointer.
            ExprKind::Field(base, field)
                if self.ty(base) == Type::Unit
                    && field.name == "null"
                    && matches!(self.ty(expr), Type::Handle(ref k) if k == "CPtr") =>
            {
                self.context
                    .ptr_type(Default::default())
                    .const_null()
                    .into()
            }
            // The checker gives a type name `void`: `Shape.Empty` is a variant.
            ExprKind::Field(base, field) if self.ty(base) == Type::Unit => {
                let ty = self.ty(expr);
                self.construct(&ty, &field.name, &[])?
            }
            ExprKind::Field(base, field) => self.field(base, field)?,
            ExprKind::StructLit(lit) => {
                self.struct_literal(expr, &lit.fields, lit.base.as_deref())?
            }
            ExprKind::Array(elements) => self.array_literal(expr, elements)?,
            ExprKind::Tuple(elements) => self.tuple_literal(expr, elements)?,
            ExprKind::Repeat(value, count) => self.repeat(expr, value, count)?,
            ExprKind::Spawn(closure) => self.spawn(expr, closure)?,
            ExprKind::Select(arms) => return self.select(expr, arms),
        };
        Ok(Value::Val(value))
    }

    /// A numeric literal at its checked type; `negate` folds a leading `-`.
    fn literal(&self, literal: &Expr, negate: bool) -> BasicValueEnum<'ctx> {
        match literal.kind {
            ExprKind::Float(value) => {
                let value = if negate { -value } else { value };
                // An `f32` literal is the `f32` nearest its value, as `x as f32` (v7).
                if self.ty(literal) == Type::F32 {
                    #[allow(clippy::cast_possible_truncation)] // rounding to f32 is the point
                    let single = f64::from(value as f32);
                    return self.context.f32_type().const_float(single).into();
                }
                self.context.f64_type().const_float(value).into()
            }
            // `0` or `1` as a `T: Num` that is a float here (v15).
            ExprKind::Int(value) if self.ty(literal).is_float() => {
                #[allow(clippy::cast_precision_loss)] // only `0` and `1` reach here
                let value = if negate {
                    -(value as f64)
                } else {
                    value as f64
                };
                let float = if self.ty(literal) == Type::F32 {
                    self.context.f32_type()
                } else {
                    self.context.f64_type()
                };
                float.const_float(value).into()
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
