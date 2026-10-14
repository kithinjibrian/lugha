//! `format` calls (v14 draft §5, milestone 48): `format("…", a, b)` with a
//! literal template becomes `ExprKind::Format`; any other `format(…)` stays
//! an ordinary call, which the checker rejects (E0432).

use super::{PResult, Parser};
use crate::ast::{Expr, ExprKind, FormatCall};
use crate::lexer::TokenKind;

impl Parser<'_> {
    /// Whether `format` just read starts a call with a literal template:
    /// `(` then a string followed by `,` or `)`.
    pub(super) fn at_template(&self) -> bool {
        self.peek() == &TokenKind::LParen
            && matches!(self.peek_at(1), TokenKind::Str(_))
            && matches!(self.peek_at(2), TokenKind::Comma | TokenKind::RParen)
    }

    /// `format("…", args…)` after `format`, at its `(`.
    pub(super) fn format_call(&mut self, start: usize) -> PResult<Expr> {
        let open = self.bump();
        let TokenKind::Str(template) = self.peek().clone() else {
            unreachable!("at_template saw the string")
        };
        let template_span = self.bump();
        let args = if self.eat(&TokenKind::Comma) {
            self.comma_list(open, TokenKind::RParen, |p| p.expr())?
        } else {
            self.close(open, &TokenKind::RParen)?;
            Vec::new()
        };
        let call = FormatCall {
            template,
            template_span,
            args,
        };
        Ok(self.mk(start, ExprKind::Format(Box::new(call))))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::expr;

    #[test]
    fn format_with_a_literal_template_is_its_own_form() {
        assert_eq!(
            expr(r#"format("{} has {:>4}", name, n + 1,)"#),
            r#"(format "{} has {:>4}" name (+ n 1))"#
        );
        assert_eq!(expr(r#"format("{{}}").len"#), r#"(. (format "{{}}") len)"#);
        // Not a literal template: an ordinary call.
        assert_eq!(expr("format(s, 1)"), "(call format s 1)");
        assert_eq!(expr(r#"format("a" + b)"#), r#"(call format (+ "a" b))"#);
        assert_eq!(
            expr(r#"p.format("{x}")"#),
            r#"(call (. p format) (interp "" x ""))"#
        );
    }
}
