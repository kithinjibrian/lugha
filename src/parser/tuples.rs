//! Tuples in the parser (v14 draft §3, milestone 47): `(A, B)` types,
//! `(a, b)` values, `t.0` members, `(p, q)` patterns, and `for (i, w) of …`,
//! which becomes a hidden loop variable destructured at the top of the body.

use super::{PResult, Parser};
use crate::ast::{
    Block, Expr, ExprKind, Ident, Pattern, PatternKind, Stmt, StmtKind, Type, TypeKind,
};
use crate::diagnostic::Diagnostic;
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    /// The rest of `(A, B, …)` after its first type, at a `,`.
    pub(super) fn tuple_type(&mut self, open: Span, first: Type) -> PResult<TypeKind> {
        let mut elements = vec![first];
        while self.eat(&TokenKind::Comma) && !self.at(&TokenKind::RParen) {
            elements.push(self.nested(|p| p.ty())?);
        }
        self.close(open, &TokenKind::RParen)?;
        self.two_or_more(elements.len(), open)?;
        Ok(TypeKind::Tuple(elements))
    }

    /// The rest of `(a, b, …)` after its first value, at a `,`.
    pub(super) fn tuple_literal(&mut self, start: usize, open: Span, first: Expr) -> PResult<Expr> {
        let mut elements = vec![first];
        while self.eat(&TokenKind::Comma) && !self.at(&TokenKind::RParen) {
            elements.push(self.expr()?);
        }
        self.close(open, &TokenKind::RParen)?;
        self.two_or_more(elements.len(), open)?;
        Ok(self.mk(start, ExprKind::Tuple(elements)))
    }

    /// `()` (v17, milestone 107): the unit value, an empty tuple. Kept out
    /// of `primary`, whose frame is on every nesting level's stack.
    #[inline(never)]
    pub(super) fn unit_literal(&mut self, start: usize) -> PResult<Expr> {
        self.bump();
        self.bump();
        Ok(self.mk(start, ExprKind::Tuple(Vec::new())))
    }

    /// The rest of `(p, q, …)` after its first pattern, at a `,`.
    pub(super) fn tuple_pattern(
        &mut self,
        start: usize,
        open: Span,
        first: Pattern,
    ) -> PResult<Pattern> {
        let mut elements = vec![first];
        while self.eat(&TokenKind::Comma) && !self.at(&TokenKind::RParen) {
            elements.push(self.pattern()?);
        }
        self.close(open, &TokenKind::RParen)?;
        self.two_or_more(elements.len(), open)?;
        Ok(Pattern {
            kind: PatternKind::Tuple(elements),
            span: Span::new(start, self.prev_end()),
        })
    }

    /// E0201 for `(a,)`: a tuple has two or more elements.
    fn two_or_more(&mut self, count: usize, open: Span) -> PResult<()> {
        if count >= 2 {
            return Ok(());
        }
        let at = Span::new(open.start, self.prev_end());
        let d = Diagnostic::error("E0201", "a tuple needs two or more elements", at)
            .with_help("drop the comma to group one value: `(x)`");
        self.error(d);
        Ok(())
    }

    /// A member after `.`: a field name, or a tuple index such as `0`.
    pub(super) fn member_name(&mut self) -> PResult<Ident> {
        if let TokenKind::Int(n) = self.peek().clone() {
            let span = self.bump();
            return Ok(Ident {
                name: n.to_string(),
                span,
            });
        }
        self.ident("field name")
    }

    /// True if a `(` after `for` opens the optional parentheses around the
    /// header (`for (x of xs)`) rather than a tuple pattern (`for (i, w) of`).
    pub(super) fn header_paren(&self) -> bool {
        self.at(&TokenKind::LParen)
            && matches!(self.peek_at(1), TokenKind::Ident(_))
            && matches!(self.peek_at(2), TokenKind::In | TokenKind::Of)
    }

    /// The loop variable: a name, or a tuple pattern under a hidden name.
    pub(super) fn loop_var(&mut self) -> PResult<(Ident, Option<Pattern>)> {
        if !self.at(&TokenKind::LParen) {
            return Ok((self.ident("loop variable")?, None));
        }
        let pattern = self.pattern()?;
        // `$` keeps the hidden name out of every program's reach.
        let hidden = Ident {
            name: format!("$item{}", pattern.span.start),
            span: pattern.span,
        };
        Ok((hidden, Some(pattern)))
    }

    /// `body` with `let pattern = $item;` first, for a destructuring loop.
    pub(super) fn destructured(&mut self, pattern: Option<Pattern>, mut body: Block) -> Block {
        let Some(pattern) = pattern else {
            return body;
        };
        let span = pattern.span;
        let name = format!("$item{}", span.start);
        let init = self.mk(span.start, ExprKind::Name(name));
        let init = Expr { span, ..init };
        let stmt = Stmt {
            kind: StmtKind::LetElse {
                pattern: Box::new(pattern),
                init,
                else_: None,
            },
            span,
        };
        body.stmts.insert(0, stmt);
        body
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{body, errors, expr, program};

    #[test]
    fn tuples_parse_everywhere() {
        assert_eq!(expr("(a, b)"), "(tuple a b)");
        assert_eq!(expr("t.0.1"), "(. (. t 0) 1)");
        assert_eq!(
            program("fun f(p: (i64, string)) {}"),
            "(fun f ((p (tuple i64 string))) void (block))"
        );
        assert_eq!(body("let (a, _) = t;"), "(block (let-pat (tuple a _) t))");
        assert!(
            body("for (i, w) of xs { }").contains("(let-pat (tuple i w) $item"),
            "{}",
            body("for (i, w) of xs { }")
        );
        assert_eq!(body("for (x of xs) { }"), "(block (for x (of xs) (block)))");
        assert_eq!(errors("fun f() { let t = (a,); }")[0].0, "E0201");
    }

    #[test]
    fn unit_is_the_empty_tuple_v17() {
        assert_eq!(expr("()"), "(tuple)");
        assert_eq!(expr("(a)"), "a");
        assert_eq!(expr("f(())"), "(call f (tuple))");
        assert_eq!(
            program("fun f(u: ()): Result<(), string> {}"),
            "(fun f ((u (tuple))) (applied Result (tuple) string) (block))"
        );
        assert_eq!(body("let x: () = ();"), "(block (let x (tuple) (tuple)))");
        assert_eq!(body("let () = f();"), "(block (let-pat (tuple) (call f)))");
        assert!(body("match o { Some(()) => 1, _ => 2 }").contains("(Some (tuple))"));
    }
}
