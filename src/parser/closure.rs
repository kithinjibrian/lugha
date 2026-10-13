use super::{PResult, Parser};
use crate::ast::{Block, Closure, Expr, ExprKind, Ident, Type};
use crate::diagnostic::Diagnostic;
use crate::lexer::TokenKind;

impl Parser<'_> {
    /// After `fun` in an expression: the parameters, an optional return
    /// type, and a block or `= expr` (the longest expression).
    pub(super) fn closure(&mut self, start: usize) -> PResult<Expr> {
        let open = self.expect(&TokenKind::LParen)?;
        let params = self.comma_list(open, TokenKind::RParen, |p| p.closure_param())?;
        let ret = self.return_type()?;
        let body = if self.eat(&TokenKind::Eq) {
            let tail = self.expr()?;
            Block {
                stmts: Vec::new(),
                span: tail.span,
                tail: Some(Box::new(tail)),
            }
        } else {
            self.block()?
        };
        let closure = Closure { params, ret, body };
        Ok(self.mk(start, ExprKind::Closure(Box::new(closure))))
    }

    /// `x` or `x: T`; anything else is E0209 (`self`, `mut x`, a pattern).
    fn closure_param(&mut self) -> PResult<(Ident, Option<Type>)> {
        if matches!(self.peek(), TokenKind::SelfKw | TokenKind::Mut) {
            let span = self.span();
            let d = Diagnostic::error("E0209", "a closure parameter is a plain name", span)
                .with_help(
                    "write `x` or `x: T`; closures have no `self`, and parameters are never `mut`",
                );
            return Err(self.error(d));
        }
        let name = self.ident("parameter name")?;
        let ty = if self.eat(&TokenKind::Colon) {
            Some(self.ty()?)
        } else {
            None
        };
        Ok((name, ty))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{errors, expr};

    #[test]
    fn closures_are_expressions() {
        assert_eq!(
            expr("fun(x) = x + 1"),
            "(closure ((x _)) _ (block (+ x 1)))"
        );
        assert_eq!(
            expr("fun(x: i64, y): i64 { x }"),
            "(closure ((x i64) (y _)) i64 (block x))"
        );
        assert_eq!(expr("fun() {}"), "(closure () _ (block))");
        assert_eq!(
            expr("fun(f: fun(i64): i64) = f(1)"),
            "(closure ((f (fun-type (i64) i64))) _ (block (call f 1)))"
        );
    }

    #[test]
    fn an_expression_body_ends_where_the_expression_does() {
        assert_eq!(
            expr("sort(xs, fun(a, b) = a > b)"),
            "(call sort xs (closure ((a _) (b _)) _ (block (> a b))))"
        );
    }

    #[test]
    fn parameters_are_plain_names() {
        assert_eq!(
            errors("fun main() { let f = fun(self) = 1; }")[0].0,
            "E0209"
        );
        assert_eq!(
            errors("fun main() { let f = fun(mut x) = x; }")[0].0,
            "E0209"
        );
    }
}
