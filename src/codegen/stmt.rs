//! Statements: `let`, assignment, expression statements, loops and jumps.

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED, unsupported};
use super::scope::Local;
use super::value::{Kind, Value, annotation_kind, type_error};
use crate::ast::{AssignOp, BinOp, Expr, ExprKind, ForIter, Ident, Stmt, StmtKind, Type};

impl<'ctx> Lowerer<'ctx> {
    /// Lowers a statement and reports whether it diverges — whether control
    /// can't reach the next statement (spec §6).
    pub(super) fn stmt(&mut self, stmt: &Stmt) -> Result<bool, CodegenError> {
        match &stmt.kind {
            StmtKind::Let { name, ty, init, .. } => self.let_stmt(name, ty.as_ref(), init)?,
            StmtKind::Assign { op, place, value } => self.assign(*op, place, value)?,
            StmtKind::Expr { expr, .. } => {
                let value = self.expr(expr)?;
                return Ok(matches!(value, Value::Never { .. }));
            }
            // Loops never definitely return (spec §6), whatever their body does.
            StmtKind::While { cond, body } => self.while_loop(cond, body)?,
            StmtKind::For {
                var,
                iter: ForIter::Range(start, end),
                body,
            } => {
                self.for_range(var, start, end, body)?;
            }
            StmtKind::For {
                iter: ForIter::Array(_),
                ..
            } => {
                return Err(unsupported("arrays", 5, stmt.span));
            }
            StmtKind::Return(value) => {
                self.return_stmt(value.as_ref(), stmt.span)?;
                return Ok(true);
            }
            StmtKind::Break | StmtKind::Continue => {
                self.jump(matches!(stmt.kind, StmtKind::Break), stmt.span)?;
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// `let [mut] name [: ty] = init;` — `init` is evaluated before `name` is
    /// in scope, so `let x = x + 1;` reads the outer `x` (spec §5).
    fn let_stmt(
        &mut self,
        name: &Ident,
        ty: Option<&Type>,
        init: &Expr,
    ) -> Result<(), CodegenError> {
        let annotated = ty.map(annotation_kind).transpose()?;
        let (kind, value) = self.expr(init)?.typed(init.span)?;
        if annotated.is_some_and(|expected| expected != kind) {
            return Err(type_error(init.span));
        }
        let ptr = self.entry_alloca(kind, &name.name);
        self.builder.build_store(ptr, value).expect(POSITIONED);
        self.scopes.declare(&name.name, Local { ptr, kind });
        Ok(())
    }

    /// `place op value;` where `place` must be a local name until milestone 5.
    /// Mutability isn't checked until milestone 3 (CLAUDE.md KNOWN ISSUES).
    fn assign(&mut self, op: AssignOp, place: &Expr, value: &Expr) -> Result<(), CodegenError> {
        let name = match &place.kind {
            ExprKind::Name(name) => name,
            ExprKind::Field(..) | ExprKind::Index(..) => {
                return Err(unsupported(
                    "assigning to fields and elements",
                    5,
                    place.span,
                ));
            }
            _ => return Err(unsupported("checking assignment targets", 3, place.span)),
        };
        let local = self
            .scopes
            .lookup(name)
            .ok_or_else(|| unsupported("checking undefined names", 3, place.span))?;
        let new = match compound_op(op) {
            None => {
                let (kind, new) = self.expr(value)?.typed(value.span)?;
                if kind != local.kind {
                    return Err(type_error(value.span));
                }
                new
            }
            Some(op) => {
                if local.kind != Kind::Int {
                    return Err(type_error(place.span));
                }
                // `x op= e` reads `x` once, then evaluates `e` (spec §5).
                let current = self.load(local, name);
                let rhs = self.expr(value)?.int(value.span)?;
                self.arithmetic(op, current, rhs)
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
    use crate::codegen::test_util::{ir, unsupported};

    #[test]
    fn let_checks_kinds_and_annotations() {
        assert_eq!(
            unsupported("fun main() { let b: bool = 1; }"),
            ("type checking", 3, "1")
        );
        assert_eq!(
            unsupported("fun main() { let x: f64 = 1; }"),
            ("floats", 3, "f64")
        );
        assert_eq!(
            unsupported("fun main() { let s = \"a\"; }"),
            ("strings", 4, "\"a\"")
        );
        assert_eq!(
            unsupported("fun main() { let v = if true { }; }"),
            ("type checking", 3, "if true { }")
        );
    }

    #[test]
    fn assignment_targets() {
        let field = "fun main() { let mut p = 1; p.x = 1; }";
        assert_eq!(
            unsupported(field),
            ("assigning to fields and elements", 5, "p.x")
        );
        assert_eq!(
            unsupported("fun main() { q = 1; }"),
            ("checking undefined names", 3, "q")
        );
        let bool_add = "fun main() { let mut b = true; b += 1; }";
        assert_eq!(unsupported(bool_add), ("type checking", 3, "b"));
    }

    #[test]
    fn later_milestones_are_unsupported() {
        assert_eq!(
            unsupported("fun main() { for x of xs { } }"),
            ("arrays", 5, "for x of xs { }")
        );
    }

    #[test]
    fn locals_live_in_the_entry_block() {
        let ir = ir("fun main(): i32 { let mut x = 1; while x < 5 { let y = x; x += y; } x }");
        let body = &ir[ir.find("define i64 @lugha_fn_main").unwrap()..];
        let first_branch = body.find("br ").unwrap();
        assert_eq!(
            body.matches("alloca").count(),
            body[..first_branch].matches("alloca").count(),
            "{ir}"
        );
    }
}
