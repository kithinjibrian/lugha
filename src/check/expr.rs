//! Expressions: literals, names, calls, `if`, blocks, unary operators.
//!
//! Checking is bidirectional: the context may pass an `Expect` (a type and
//! the reason for it), which numeric literals adopt (spec §4 rules 3–6).

use super::{Checker, Checking, Type, errors, stop};
use crate::ast::{Block, Expr, ExprKind, UnOp};
use crate::diagnostic::Label;
use crate::span::Span;

/// What the context wants an expression to be, and why — the "why" becomes
/// a secondary label, e.g. "this operand is i32".
#[derive(Debug, Clone)]
pub(super) struct Expect {
    pub ty: Type,
    pub reason: Option<Label>,
}

impl Expect {
    pub(super) fn of(ty: Type) -> Self {
        Expect { ty, reason: None }
    }

    pub(super) fn because(ty: Type, span: Span, message: String) -> Self {
        Expect {
            ty,
            reason: Some(Label { span, message }),
        }
    }
}

/// A numeric literal, possibly with a `-` folded in (spec §4 rule 6).
pub(super) fn is_literal(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Int(_) | ExprKind::Float(_) => true,
        ExprKind::Unary(UnOp::Neg, inner) => {
            matches!(inner.kind, ExprKind::Int(_) | ExprKind::Float(_))
        }
        _ => false,
    }
}

impl Checker {
    /// Checks `expr`, records its type and returns it.
    pub(super) fn expr(&mut self, expr: &Expr, expect: Option<&Expect>) -> Checking<Type> {
        let ty = self.expr_kind(expr, expect)?;
        self.record(expr, ty.clone());
        Ok(ty)
    }

    /// Checks an expression used as a value: `void` is E0407 and becomes `Error`.
    pub(super) fn value(&mut self, expr: &Expr, expect: Option<&Expect>) -> Checking<Type> {
        let ty = self.expr(expr, expect)?;
        if ty == Type::Void {
            let mut d = errors::no_value(expr.span);
            if let ExprKind::Block(block) = &expr.kind
                && let Some(semicolon) = self.stray_semicolon(block)
            {
                d = errors::with_stray_semicolon(d, semicolon);
            }
            self.report(d);
            return Ok(Type::Error);
        }
        Ok(ty)
    }

    /// Checks `expr` where a value of `expect.ty` is required (E0403 otherwise).
    pub(super) fn expect_type(&mut self, expr: &Expr, expect: &Expect) -> Checking<Type> {
        let found = self.value(expr, Some(expect))?;
        if !found.fits(&expect.ty) {
            self.report(errors::mismatch(
                expect.ty.clone(),
                found,
                expr.span,
                expect.reason.as_ref(),
            ));
            return Ok(Type::Error);
        }
        Ok(found)
    }

    fn expr_kind(&mut self, expr: &Expr, expect: Option<&Expect>) -> Checking<Type> {
        match &expr.kind {
            _ if is_literal(expr) => {
                let ty = self.literal(expr, expect);
                if let ExprKind::Unary(_, inner) = &expr.kind {
                    self.record(inner, ty.clone());
                }
                Ok(ty)
            }
            ExprKind::Bool(_) => Ok(Type::Bool),
            ExprKind::Name(name) => self.name(name, expr.span),
            ExprKind::Unary(op, operand) => self.unary(*op, operand, expr, expect),
            ExprKind::Binary(op, _, lhs, rhs) => self.binary(expr, *op, lhs, rhs, expect),
            ExprKind::Call(callee, args) => self.call(expr, callee, args),
            ExprKind::If { cond, then, else_ } => {
                self.if_expr(expr, cond, then, else_.as_deref(), expect)
            }
            ExprKind::Block(block) => self.block(block, expect),
            ExprKind::Str(_) => Ok(Type::String),
            ExprKind::Cast(inner, target) => self.cast(expr, inner, target),
            ExprKind::Index(base, _, index) => self.index(base, index),
            ExprKind::Field(base, field) => self.field(base, field),
            ExprKind::StructLit(..) => Err(stop("structs", 5, expr.span)),
            ExprKind::Array(elements) => self.array_literal(expr, elements, expect),
            ExprKind::Repeat(value, count) => self.repeat(value, count, expect),
            ExprKind::Int(_) | ExprKind::Float(_) => unreachable!("literals are matched first"),
        }
    }

    fn unary(
        &mut self,
        op: UnOp,
        operand: &Expr,
        expr: &Expr,
        expect: Option<&Expect>,
    ) -> Checking<Type> {
        let (ty, ok, symbol) = match op {
            UnOp::Neg => {
                let ty = self.value(operand, expect)?;
                let ok = matches!(ty, Type::I32 | Type::I64 | Type::F64);
                (ty, ok, "-")
            }
            UnOp::Not => {
                let ty = self.value(operand, Some(&Expect::of(Type::Bool)))?;
                let ok = ty == Type::Bool;
                (ty, ok, "!")
            }
        };
        if ok || matches!(ty, Type::Error | Type::Never) {
            return Ok(ty);
        }
        self.report(errors::bad_operands(symbol, &[ty], expr.span));
        Ok(Type::Error)
    }

    /// `if`: a `bool` condition; with `else`, both branches agree unless one
    /// never finishes (spec §5, §6). Without `else` the type is `void`.
    fn if_expr(
        &mut self,
        expr: &Expr,
        cond: &Expr,
        then: &Block,
        else_: Option<&Expr>,
        expect: Option<&Expect>,
    ) -> Checking<Type> {
        self.expect_type(cond, &Expect::of(Type::Bool))?;
        let Some(else_expr) = else_ else {
            self.block(then, None)?;
            return Ok(Type::Void);
        };
        let then_ty = self.block(then, expect)?;
        let else_ty = self.expr(else_expr, expect)?;
        Ok(match (then_ty, else_ty) {
            (Type::Never, ty) | (ty, Type::Never) => ty,
            (Type::Error, _) | (_, Type::Error) => Type::Error,
            (a, b) if a == b => a,
            (a, b) => {
                let then_span = then.tail.as_ref().map_or(then.span, |tail| tail.span);
                self.report(errors::branch_mismatch(
                    expr.span,
                    (then_span, a),
                    (else_expr.span, b),
                ));
                Type::Error
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, stopped};

    #[test]
    fn if_branches_must_agree_unless_one_diverges() {
        let src = "fun main() { let v = if true { 1 } else { false }; }";
        assert_eq!(errors(src), [("E0403", "if true { 1 } else { false }")]);
        let abs = "fun abs(x: i64): i64 { if x < 0 { return 0; } else { x } }\nfun main() { let a = abs(1); }";
        assert_eq!(let_type(abs, "a"), Type::I64);
        // The expected type flows into both branches.
        assert_eq!(
            let_type(
                "fun main() { let c = true; let v: u8 = if c { 1 } else { 2 }; }",
                "v"
            ),
            Type::U8
        );
    }

    #[test]
    fn later_milestone_expressions_stop_the_checker() {
        // Strings (PRP-013) and arrays (PRP-014) are checked; struct literals arrive in PRP-015.
        let src = "fun main() { let p = P { x: 1 }; }";
        assert_eq!(stopped(src), ("structs", 5, "P { x: 1 }"));
    }
}
