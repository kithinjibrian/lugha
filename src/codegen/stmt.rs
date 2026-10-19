//! Statements: `let`, assignment, expression statements, loops and jumps.

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::scope::Local;
use super::value::Value;
use crate::ast::{AssignOp, BinOp, Expr, ForIter, Ident, Stmt, StmtKind, Type as Annotation};
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
            StmtKind::WhileLet {
                pattern,
                scrutinee,
                body,
            } => self.while_let(pattern, scrutinee, body)?,
            StmtKind::LetElse {
                pattern,
                init,
                else_,
            } => self.let_else(pattern, init, else_.as_deref())?,
            StmtKind::For {
                var,
                iter: ForIter::Range(start, end),
                body,
            } => self.for_range(var, start, end, body)?,
            StmtKind::For {
                var,
                iter: ForIter::Array(iter),
                body,
            } => self.for_of(var, iter, body, false)?,
            StmtKind::For {
                var,
                iter: ForIter::MutOf(iter),
                body,
            } => self.for_of(var, iter, body, true)?,
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
            (Type::Never | Type::Error, Some(annotation)) => self.annotation_type(annotation),
            // Dead code after a diverging initialiser: any slot type keeps the IR valid.
            (Type::Never | Type::Error, None) => Type::I64,
            (ty, _) => ty,
        };
        let value = self.value_for_store(init, &ty)?;
        let ptr = self.entry_alloca(&ty, &name.name);
        self.builder.build_store(ptr, value).expect(POSITIONED);
        self.scopes.declare(
            &name.name,
            Local {
                ptr,
                ty,
                borrowed: false,
            },
        );
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
        // `m[k] = v` on a map (v17): the `insert` the checker recorded at the place.
        if let crate::ast::ExprKind::Index(base, _, key) = &place.kind
            && self.is_map(base)
        {
            return self
                .map_method(place, base, "insert", &[(**key).clone(), value.clone()])
                .map(|_| ());
        }
        // The place's indexes are evaluated once, before the value (spec §5);
        // the arrays on its path are made unique after the value (v15), so a
        // value that shares one of them is never written through.
        let plan = self.place_plan(place)?;
        let ty = self.ty(place);
        let new = match compound_op(op) {
            None => self.value_for_store(value, &ty)?,
            // `a += b` on a struct or enum: `a = a.add(b)` (v15).
            Some(op) if matches!(ty, Type::Struct(..) | Type::Enum(..)) => {
                let current = self.place_read(place, &plan);
                let name = super::arith::method_of(op).expect("compound operators are arithmetic");
                self.operator_on(place, (current, &ty), name, std::slice::from_ref(value))?
            }
            // `s += t` and `xs += ys` join (v17, milestone 122).
            Some(BinOp::Add) if matches!(ty, Type::String | Type::Array(_)) => {
                let current = self.place_read(place, &plan);
                let rhs = self.get(value, &ty)?;
                match ty {
                    Type::String => self.string_binary(BinOp::Add, current, rhs),
                    _ => self.join_arrays(current, rhs, &ty),
                }
            }
            Some(op) => {
                // `x op= e` reads `x` once, then evaluates `e` (spec §5).
                // Compound operators need a numeric type, which has no `bool` widening.
                let current = self.place_read(place, &plan);
                let rhs = self.get(value, &ty)?;
                self.arithmetic(op, ty.clone(), current, rhs, at)
            }
        };
        // A local's slot holds its value as is; fields and elements are
        // stored in element layout (`bool` as `i8`).
        let (address, ty, element) = self.place_commit(place, &plan);
        if element {
            self.store_element(address, &ty, new);
        } else {
            self.builder.build_store(address, new).expect(POSITIONED);
        }
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
        AssignOp::Rem => Some(BinOp::Rem),
        AssignOp::BitAnd => Some(BinOp::BitAnd),
        AssignOp::BitOr => Some(BinOp::BitOr),
        AssignOp::BitXor => Some(BinOp::BitXor),
        AssignOp::Shl => Some(BinOp::Shl),
        AssignOp::Shr => Some(BinOp::Shr),
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
