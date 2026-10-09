//! Statements, blocks and function bodies.

use super::expr::Expect;
use super::{Checker, Checking, Type, errors, stop};
use crate::ast::{AssignOp, Block, Expr, ExprKind, ForIter, FunDecl, Ident, Stmt, StmtKind};
use crate::span::Span;

impl Checker {
    /// Checks one function body against its signature.
    pub(super) fn function(&mut self, f: &FunDecl) -> Checking<()> {
        let signature = &self.functions[&f.name.name];
        let (params, ret) = (signature.params.clone(), signature.ret);
        self.ret = ret;
        self.scopes.clear();
        self.push();
        for (param, ty) in f.params.iter().zip(params) {
            self.declare(&param.name.name, ty);
        }
        let expect = (ret != Type::Void).then(|| Expect::of(ret));
        let body = self.block(&f.body, expect.as_ref())?;
        self.pop();
        // A `void` body in a non-void function is a missing return: PRP-009.
        if ret != Type::Void && body != Type::Void && !body.fits(ret) {
            let span = f.body.tail.as_ref().map_or(f.body.span, |tail| tail.span);
            self.report(errors::mismatch(ret, body, span, None));
        }
        Ok(())
    }

    /// A block in its own scope: the tail's type, `void`, or `Never` once a
    /// statement diverges (spec §5, §6).
    pub(super) fn block(&mut self, block: &Block, expect: Option<&Expect>) -> Checking<Type> {
        self.push();
        let mut diverged = false;
        for stmt in &block.stmts {
            diverged |= self.stmt(stmt)?;
        }
        let ty = match &block.tail {
            Some(tail) => self.expr(tail, expect)?,
            None => Type::Void,
        };
        self.pop();
        Ok(if diverged { Type::Never } else { ty })
    }

    /// Checks a statement and reports whether it diverges.
    fn stmt(&mut self, stmt: &Stmt) -> Checking<bool> {
        match &stmt.kind {
            StmtKind::Let { name, ty, init, .. } => self.let_stmt(name, ty.as_ref(), init)?,
            StmtKind::Assign { op, place, value } => self.assign(*op, place, value)?,
            StmtKind::Expr { expr, .. } => return Ok(self.expr(expr, None)? == Type::Never),
            StmtKind::While { cond, body } => {
                self.expect_type(cond, &Expect::of(Type::Bool))?;
                self.block(body, None)?;
            }
            StmtKind::For {
                var,
                iter: ForIter::Range(start, end),
                body,
            } => self.for_range(var, start, end, body)?,
            StmtKind::For {
                iter: ForIter::Array(_),
                ..
            } => return Err(stop("arrays", 5, stmt.span)),
            StmtKind::Return(value) => {
                self.return_stmt(value.as_ref(), stmt.span)?;
                return Ok(true);
            }
            StmtKind::Break | StmtKind::Continue => return Ok(true),
        }
        Ok(false)
    }

    /// `let`: the annotation, if any, is expected; the binding enters scope afterwards (spec §5).
    fn let_stmt(
        &mut self,
        name: &Ident,
        annotation: Option<&crate::ast::Type>,
        init: &Expr,
    ) -> Checking<()> {
        let ty = match annotation {
            Some(annotation) => {
                let ty = self.resolve(annotation)?;
                let why = "expected because of this annotation".to_string();
                self.expect_type(init, &Expect::because(ty, annotation.span, why))?;
                ty
            }
            None => match self.value(init, None)? {
                // Nothing useful to bind; `Error` keeps later uses quiet.
                Type::Never => Type::Error,
                ty => ty,
            },
        };
        self.declare(&name.name, ty);
        Ok(())
    }

    /// Assignment to a local. Places other than names, and mutability, are PRP-009.
    fn assign(&mut self, op: AssignOp, place: &Expr, value: &Expr) -> Checking<()> {
        let ExprKind::Name(name) = &place.kind else {
            if matches!(place.kind, ExprKind::Field(..) | ExprKind::Index(..)) {
                return Err(stop("assigning to fields and elements", 5, place.span));
            }
            self.expr(place, None)?;
            self.expr(value, None)?;
            return Ok(());
        };
        let target = match self.local(name) {
            Some(local) => local.ty,
            None => {
                self.expr(place, None)?;
                self.expr(value, None)?;
                return Ok(());
            }
        };
        self.record(place, target);
        if op != AssignOp::Assign && !target.is_numeric() && target != Type::Error {
            let span = Span::new(place.span.start, value.span.end);
            self.report(errors::bad_operands(op.symbol(), &[target], span));
            self.expr(value, None)?;
            return Ok(());
        }
        self.expect_type(
            value,
            &Expect::because(target, place.span, format!("`{name}` is {target}")),
        )?;
        Ok(())
    }

    /// `for var in start..end`: both bounds share one integer type (spec §5).
    fn for_range(&mut self, var: &Ident, start: &Expr, end: &Expr, body: &Block) -> Checking<()> {
        let ty = match self.operands(start, end, None)? {
            (Type::Error, _) | (_, Type::Error) => Type::Error,
            (a, b) if a != b => {
                self.report(errors::mismatch(a, b, end.span, None));
                Type::Error
            }
            (ty, _) if !ty.is_integer() => {
                self.report(errors::mismatch(Type::I64, ty, start.span, None));
                Type::Error
            }
            (ty, _) => ty,
        };
        self.push();
        self.declare(&var.name, ty);
        self.block(body, None)?;
        self.pop();
        Ok(())
    }

    /// `return [value];` must match the function's return type (spec §6).
    fn return_stmt(&mut self, value: Option<&Expr>, span: Span) -> Checking<()> {
        match (self.ret, value) {
            (Type::Void, None) => {}
            (Type::Void, Some(value)) => {
                let ty = self.expr(value, None)?;
                if !matches!(ty, Type::Void | Type::Never | Type::Error) {
                    self.report(errors::mismatch(Type::Void, ty, span, None));
                }
            }
            (ret, None) => self.report(errors::mismatch(ret, Type::Void, span, None)),
            (ret, Some(value)) => {
                self.expect_type(value, &Expect::of(ret))?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok, stopped};

    #[test]
    fn let_and_assignment_check_types() {
        assert_eq!(
            errors("fun main() { let a: bool = 1 < 2; let b: i32 = a; }"),
            [("E0403", "a")]
        );
        assert_eq!(
            errors("fun main() { let mut x: u8 = 1; x = true; }"),
            [("E0403", "true")]
        );
        assert_eq!(
            errors("fun main() { let mut b = true; b += 1; }"),
            [("E0404", "b += 1")]
        );
        assert_eq!(errors("fun main() { q = 1; }"), [("E0301", "q")]);
        assert_eq!(
            let_type(
                "fun main() { let mut x: u8 = 1; x += 200; let y = x; }",
                "y"
            ),
            Type::U8
        );
    }

    #[test]
    fn conditions_expect_bool() {
        assert_eq!(
            errors("fun main() { let n = 1; while n { } }"),
            [("E0403", "n")]
        );
    }

    #[test]
    fn range_bounds_share_an_integer_type() {
        assert_eq!(
            let_type(
                "fun main() { let n: i32 = 3; for i in 0..n { let k = i; } }",
                "k"
            ),
            Type::I32
        );
        assert_eq!(
            let_type("fun main() { for i in 0..3 { let k = i; } }", "k"),
            Type::I64
        );
        assert_eq!(
            errors("fun main() { let n: i32 = 3; let m: i64 = 4; for i in n..m { } }"),
            [("E0403", "m")]
        );
        assert_eq!(
            errors("fun main() { for i in 0..2.5 { } }"),
            [("E0401", "2.5")]
        );
        assert_eq!(
            stopped("fun main() { let xs = 1; for x of xs { } }"),
            ("arrays", 5, "for x of xs { }")
        );
    }

    #[test]
    fn returns_match_the_function() {
        assert_eq!(
            errors("fun f(): i32 { return true; }\nfun main() {}"),
            [("E0403", "true")]
        );
        assert_eq!(
            errors("fun f(): u8 { return 300; }\nfun main() {}"),
            [("E0402", "300")]
        );
        assert_eq!(
            errors("fun f(): i32 { return; }\nfun main() {}"),
            [("E0403", "return;")]
        );
        assert_eq!(
            errors("fun f() { return 1; }\nfun main() {}"),
            [("E0403", "return 1;")]
        );
        assert_eq!(
            errors("fun f(): u8 { 300 }\nfun main() {}"),
            [("E0402", "300")]
        );
        assert_eq!(
            errors("fun f(): bool { 1 }\nfun main() {}"),
            [("E0401", "1")]
        );
    }

    #[test]
    fn every_expression_of_a_valid_program_gets_a_type() {
        // `0..3` alone would make `i` an i64 (spec §4); the i32 bound types the range.
        let src = "fun fib(n: i32): i32 = if n < 2 { n } else { fib(n - 1) + fib(n - 2) };\n\
                   fun main(): i32 { let n: i32 = 3; let mut t: i32 = 0; for i in 0..n { t += i; } fib(10) + t - n }";
        let checked = ok(src);
        assert!(!checked.types.contains(&Type::Error), "{:?}", checked.types);
    }
}
