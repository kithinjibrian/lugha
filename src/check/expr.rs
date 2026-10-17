//! Expressions: literals, names, calls, `if`, blocks, unary operators.
//!
//! Checking is bidirectional: the context may pass an `Expect` (a type and
//! the reason for it), which numeric literals adopt (spec §4 rules 3–6).

use super::{Binding, Checker, Type, errors, liveness};
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
    pub(super) fn expr(&mut self, expr: &Expr, expect: Option<&Expect>) -> Type {
        let ty = self.expr_kind(expr, expect);
        // An actor's state is made only inside the actor or for `spawn` (v17).
        self.state_outside(expr, &ty);
        self.record(expr, ty.clone());
        // Where an interface is expected, the value becomes one (v5).
        self.convert(expr, ty, expect)
    }

    /// Checks an expression used as a value. Every expression has one: a
    /// statement-like one is `()` (v17, milestone 107; E0407 is retired).
    pub(super) fn value(&mut self, expr: &Expr, expect: Option<&Expect>) -> Type {
        self.expr(expr, expect)
    }

    /// Checks `expr` where a value of `expect.ty` is required (E0403 otherwise).
    pub(super) fn expect_type(&mut self, expr: &Expr, expect: &Expect) -> Type {
        let found = self.value(expr, Some(expect));
        if !found.fits(&expect.ty) {
            let unit = found == Type::Unit;
            let mut d =
                errors::mismatch(expect.ty.clone(), found, expr.span, expect.reason.as_ref());
            // `{ x; }` where a value is wanted: the `;` made it `()`.
            if unit
                && let ExprKind::Block(block) = &expr.kind
                && let Some(semicolon) = self.stray_semicolon(block)
            {
                d = errors::with_stray_semicolon(d, semicolon);
            }
            self.report(d);
            return Type::Error;
        }
        found
    }

    fn expr_kind(&mut self, expr: &Expr, expect: Option<&Expect>) -> Type {
        match &expr.kind {
            _ if is_literal(expr) => {
                let ty = self.literal(expr, expect);
                if let ExprKind::Unary(_, inner) = &expr.kind {
                    self.record(inner, ty.clone());
                }
                ty
            }
            ExprKind::Bool(_) => Type::Bool,
            ExprKind::Char(_) => Type::Char,
            ExprKind::Name(name) if name == "None" && self.local(name).is_none() => {
                self.none(expr.span, expect)
            }
            ExprKind::Name(name) => {
                if let Some(local) = self.local(name) {
                    self.hover.define(expr.span, local.span);
                    // A capture is never moved from: the closure may run again (v3).
                    let captured = self.note_capture(name);
                    let mention = liveness::Mention {
                        decl: local.span,
                        binding: if captured {
                            Binding::Param
                        } else {
                            local.binding
                        },
                        at: expr.span.start,
                    };
                    self.mentions.insert(expr.id, mention);
                }
                self.name(expr, name, expect)
            }
            ExprKind::Unary(op, operand) => self.unary(*op, operand, expr, expect),
            ExprKind::Binary(op, _, lhs, rhs) => self.binary(expr, *op, lhs, rhs, expect),
            // `$float.sqrt(x)` in a bundled source (v15).
            ExprKind::Call(callee, args) if let Some(op) = super::float_ops::float_op(callee) => {
                self.float_call(expr, (callee, op), args, expect)
            }
            // `f64.from_bits(b)` (v17, milestone 108).
            ExprKind::Call(callee, args)
                if let Some(found) = super::float_bits::from_bits(callee) =>
            {
                self.float_from_bits(expr, callee, found, args)
            }
            ExprKind::Hashed(lit) => self.hashed(expr, lit, expect),
            // An `out` argument where no extern's `out` parameter takes it (v17).
            ExprKind::Pass(_, place) => {
                self.report(super::out_params::misplaced(expr.span));
                self.expr(place, None);
                Type::Error
            }
            ExprKind::Call(callee, args) if let Some(op) = super::hashing::hash_op(callee) => {
                self.hash_call(expr, (callee, op), args)
            }
            ExprKind::Call(callee, args) if let Some(op) = super::float_ops::os_op(callee) => {
                self.os_call(expr, (callee, op), args)
            }
            ExprKind::Call(callee, args) if let Some(op) = super::float_ops::string_op(callee) => {
                self.string_call(expr, (callee, op), args)
            }
            ExprKind::Call(callee, args) => {
                if let Some(ty) = self.module_call(expr, callee, args, expect) {
                    ty
                } else if let Some(ty) = self.method_call(expr, callee, args, expect) {
                    ty
                } else if let Some(ty) = self.construction(expr, callee, args, expect) {
                    ty
                } else {
                    self.call(expr, callee, args, expect)
                }
            }
            ExprKind::If { cond, then, else_ } => {
                self.if_expr(expr, cond, then, else_.as_deref(), expect)
            }
            ExprKind::IfLet(parts) => self.if_let(expr, parts, expect),
            ExprKind::Block(block) => self.block(block, expect),
            ExprKind::Match(scrutinee, arms) => self.match_expr(expr, scrutinee, arms, expect),
            ExprKind::Str(_) => Type::String,
            ExprKind::Interp(parts) => self.interpolation(parts),
            ExprKind::Format(call) => self.format_call(call),
            ExprKind::Closure(c) => self.closure(expr, c, expect),
            ExprKind::Try(inner) => self.try_expr(expr, inner),
            ExprKind::Cast(inner, target) => self.cast(expr, inner, target),
            ExprKind::Index(base, _, index) => self.index(expr, base, index),
            // `CPtr.null` (v17, milestone 105): C's NULL as a `CPtr`.
            ExprKind::Field(base, field)
                if field.name == "null"
                    && matches!(&base.kind, ExprKind::Name(n) if n == super::input::CPTR
                        && self.local(n).is_none()
                        && self.named_type(&self.key(n)).is_none()) =>
            {
                self.record(base, Type::Unit);
                Type::Handle(super::input::CPTR.to_string())
            }
            ExprKind::Field(base, field) => match self.variant_value(expr, (base, field), expect) {
                Some(ty) => ty,
                None => match self.function_field(expr, (base, field), expect) {
                    Some(ty) => ty,
                    None => self.field(base, field),
                },
            },
            ExprKind::StructLit(lit) => self.struct_literal(expr, lit, expect),
            ExprKind::Array(elements) => self.array_literal(expr, elements, expect),
            ExprKind::Tuple(elements) => self.tuple_literal(elements, expect),
            ExprKind::Repeat(value, count) => self.repeat(value, count, expect),
            ExprKind::Spawn(closure) => self.spawn(closure, expect),
            ExprKind::Select(arms) => self.select(expr, arms, expect),
            ExprKind::Int(_) | ExprKind::Float(_) => unreachable!("literals are matched first"),
        }
    }

    fn unary(&mut self, op: UnOp, operand: &Expr, expr: &Expr, expect: Option<&Expect>) -> Type {
        let (ty, ok, symbol) = match op {
            UnOp::Neg => {
                let mark = self.changes_mark();
                let ty = self.value(operand, expect);
                // `-v` on a struct, enum or `T` calls `v.neg()` (v15).
                if super::operators::overloaded(&ty) {
                    let at = crate::span::Span::new(expr.span.start, operand.span.start);
                    return self.operator_call(expr, ("-", "neg", at), (operand, &ty), None, mark);
                }
                let ok = matches!(ty, Type::I32 | Type::I64) || ty.is_float();
                (ty, ok, "-")
            }
            UnOp::Not => {
                let ty = self.value(operand, Some(&Expect::of(Type::Bool)));
                let ok = ty == Type::Bool;
                (ty, ok, "!")
            }
            UnOp::BitNot => {
                let ty = self.value(operand, expect);
                let ok = ty.is_integer();
                (ty, ok, "~")
            }
        };
        if ok || matches!(ty, Type::Error | Type::Never) {
            return ty;
        }
        self.report(errors::bad_operands(symbol, &[ty], expr.span));
        Type::Error
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
    ) -> Type {
        self.expect_type(cond, &Expect::of(Type::Bool));
        let Some(else_expr) = else_ else {
            self.block(then, None);
            return Type::Unit;
        };
        // With nothing expected, a branch that can't be typed alone (`[]`,
        // `None`, `text("a")`) takes the other branch's type (kivuli).
        if expect.is_none() {
            let then_waits = then.tail.as_ref().is_some_and(|t| self.branch_waits(t));
            let else_waits = self.branch_waits(else_expr);
            if then_waits && !else_waits {
                let else_ty = self.expr(else_expr, None);
                let then_ty = self.block(then, sibling(&else_ty).as_ref());
                return self.join_branches(expr, then, then_ty, else_expr, else_ty);
            }
            if else_waits && !then_waits {
                let then_ty = self.block(then, None);
                let else_ty = self.expr(else_expr, sibling(&then_ty).as_ref());
                return self.join_branches(expr, then, then_ty, else_expr, else_ty);
            }
        }
        let then_ty = self.block(then, expect);
        let else_ty = self.expr(else_expr, expect);
        self.join_branches(expr, then, then_ty, else_expr, else_ty)
    }

    /// True if branch `e` (through a block's tail) can't be typed alone
    /// and waits for a sibling's type: `[]`, `None`, `[None]`, `Some([])`,
    /// an untyped closure, a waiting generic call. A literal can (it
    /// defaults: `[1]` is `i64[]`).
    pub(super) fn branch_waits(&self, e: &Expr) -> bool {
        match &e.kind {
            ExprKind::Block(b) => b.tail.as_ref().is_some_and(|t| self.branch_waits(t)),
            _ if is_literal(e) => false,
            ExprKind::Array(xs) => xs.iter().all(|x| self.branch_waits(x)),
            ExprKind::Repeat(value, _) => self.branch_waits(value),
            ExprKind::Tuple(xs) => xs.iter().any(|x| self.branch_waits(x)),
            ExprKind::Call(callee, args)
                if matches!(&callee.kind, ExprKind::Name(n) if n == "Some") && args.len() == 1 =>
            {
                self.branch_waits(&args[0])
            }
            _ => self.waits(e),
        }
    }

    /// The type of an `if` (or `if let`, v9) whose branches have these
    /// types: they agree unless one diverges (E0403 otherwise).
    pub(super) fn join_branches(
        &mut self,
        expr: &Expr,
        then: &Block,
        then_ty: Type,
        else_expr: &Expr,
        else_ty: Type,
    ) -> Type {
        match (then_ty, else_ty) {
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
        }
    }
}

/// The expectation a sibling branch of type `ty` gives a waiting one, if
/// `ty` is a value's type.
pub(super) fn sibling(ty: &Type) -> Option<Expect> {
    (!matches!(ty, Type::Never | Type::Error | Type::Unit)).then(|| Expect::of(ty.clone()))
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type};

    #[test]
    fn if_branches_must_agree_unless_one_diverges() {
        let src = "fun main() { let v = if true { 1 } else { false }; }";
        assert_eq!(errors(src), [("E0403", "if true { 1 } else { false }")]);
        // An empty `[]` takes the other branch's type, on either side (kivuli).
        for v in [
            "if c { [] } else { [1] }",
            "if c { [1] } else { [] }",
            "if c { { [] } } else { [1] }",
        ] {
            let src = format!("fun main() {{ let c = true; let v = {v}; }}");
            assert_eq!(let_type(&src, "v"), Type::Array(Box::new(Type::I64)), "{v}");
        }
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
}
