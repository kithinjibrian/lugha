//! Statements: `let`, assignment, expression statements, loops and jumps.

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED, unsupported};
use super::scope::Local;
use super::value::{Value, annotation_type};
use crate::ast::{
    AssignOp, BinOp, Expr, ExprKind, ForIter, Ident, Stmt, StmtKind, Type as Annotation,
};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// Lowers a statement and reports whether it diverges (spec §6).
    pub(super) fn stmt(&mut self, stmt: &Stmt) -> Result<bool, CodegenError> {
        match &stmt.kind {
            StmtKind::Let { name, ty, init, .. } => self.let_stmt(name, ty.as_ref(), init)?,
            StmtKind::Assign {
                op,
                op_span,
                place,
                value,
            } => self.assign(*op, op_span.start, place, value)?,
            StmtKind::Expr { expr, .. } => return Ok(matches!(self.expr(expr)?, Value::Never)),
            // Loops never definitely return (spec §6), whatever their body does.
            StmtKind::While { cond, body } => self.while_loop(cond, body)?,
            StmtKind::For {
                var,
                iter: ForIter::Range(start, end),
                body,
            } => self.for_range(var, start, end, body)?,
            StmtKind::For {
                iter: ForIter::Array(_),
                ..
            } => return Err(unsupported("arrays", 5, stmt.span)),
            StmtKind::Return(value) => {
                self.return_stmt(value.as_ref())?;
                return Ok(true);
            }
            StmtKind::Break | StmtKind::Continue => {
                self.jump(matches!(stmt.kind, StmtKind::Break));
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// `let name [: ty] = init;` — the slot has the initialiser's checked
    /// type; `init` is evaluated before `name` is in scope (spec §5).
    fn let_stmt(
        &mut self,
        name: &Ident,
        annotation: Option<&Annotation>,
        init: &Expr,
    ) -> Result<(), CodegenError> {
        let ty = match (self.ty(init), annotation) {
            (Type::Never | Type::Error, Some(annotation)) => annotation_type(annotation),
            // Dead code after a diverging initialiser: any slot type keeps the IR valid.
            (Type::Never | Type::Error, None) => Type::I64,
            (ty, _) => ty,
        };
        let value = self.get(init, ty)?;
        let ptr = self.entry_alloca(ty, &name.name);
        self.builder.build_store(ptr, value).expect(POSITIONED);
        self.scopes.declare(&name.name, Local { ptr, ty });
        Ok(())
    }

    /// `name op value;` — the checker guarantees a mutable local of a fitting type.
    /// `at` is the compound operator, where an overflow panic points.
    fn assign(
        &mut self,
        op: AssignOp,
        at: usize,
        place: &Expr,
        value: &Expr,
    ) -> Result<(), CodegenError> {
        let ExprKind::Name(name) = &place.kind else {
            return Err(unsupported(
                "assigning to fields and elements",
                5,
                place.span,
            ));
        };
        let local = self.scopes.lookup(name);
        let new = match compound_op(op) {
            None => self.get(value, local.ty)?,
            Some(op) => {
                // `x op= e` reads `x` once, then evaluates `e` (spec §5).
                let current = self.load(local, name);
                let rhs = self.get(value, local.ty)?;
                self.arithmetic(op, local.ty, current, rhs, at)
            }
        };
        self.builder.build_store(local.ptr, new).expect(POSITIONED);
        Ok(())
    }
}

fn compound_op(op: AssignOp) -> Option<BinOp> {
    match op {
        AssignOp::Assign => None,
        AssignOp::Add => Some(BinOp::Add),
        AssignOp::Sub => Some(BinOp::Sub),
        AssignOp::Mul => Some(BinOp::Mul),
        AssignOp::Div => Some(BinOp::Div),
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn locals_live_in_the_entry_block_at_their_type() {
        let ir = ir("fun main(): i32 { let mut x: i32 = 1; while x < 5 { let y = x; x += y; } x }");
        let body = &ir[ir.find("define i32 @lugha_fn_main").unwrap()..];
        let first_branch = body.find("br ").unwrap();
        assert_eq!(
            body.matches("alloca").count(),
            body[..first_branch].matches("alloca").count(),
            "{ir}"
        );
        assert!(body.contains("alloca i32"), "{ir}");
    }

    #[test]
    fn compound_assignment_uses_the_local_type() {
        let ir =
            ir("fun main() { let mut b: u8 = 250; b += 10; b /= 2; let mut f = 1.5; f *= 2.0; }");
        assert!(ir.contains("@llvm.uadd.with.overflow.i8"), "{ir}");
        assert!(ir.contains("udiv i8"), "{ir}");
        assert!(ir.contains("fmul double"), "{ir}");
    }
}
