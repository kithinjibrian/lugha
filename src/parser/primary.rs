//! Primary expressions: literals, names, struct literals, arrays, and the
//! block-like `if` and `{ }` expressions (spec §3).

use super::{PResult, Parser};
use crate::ast::{Expr, ExprKind, Ident};
use crate::diagnostic::Diagnostic;
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    pub(super) fn primary(&mut self) -> PResult<Expr> {
        let start = self.span().start;
        let kind = match self.peek().clone() {
            TokenKind::Int(value) => ExprKind::Int(value),
            TokenKind::Float(value) => ExprKind::Float(value),
            TokenKind::Str(value) => ExprKind::Str(value),
            TokenKind::True => ExprKind::Bool(true),
            TokenKind::False => ExprKind::Bool(false),
            TokenKind::Ident(name) => {
                let name = Ident {
                    name,
                    span: self.bump(),
                };
                return self.name_or_struct(start, name);
            }
            TokenKind::LParen => {
                let open = self.bump();
                let inner = self.expr()?;
                self.close(open, &TokenKind::RParen)?;
                return Ok(inner);
            }
            TokenKind::LBracket => {
                let open = self.bump();
                return self.with_struct_literals(true, |p| p.array(start, open));
            }
            TokenKind::If | TokenKind::LBrace => return self.block_like(),
            _ => return Err(self.expected("expression")),
        };
        self.bump();
        Ok(self.mk(start, kind))
    }

    /// After an identifier: a struct literal if `{` follows and literals are
    /// allowed; E0205 for an unparenthesised literal in a condition.
    fn name_or_struct(&mut self, start: usize, name: Ident) -> PResult<Expr> {
        if !self.at(&TokenKind::LBrace) {
            return Ok(self.mk(start, ExprKind::Name(name.name)));
        }
        if !self.no_struct {
            return self.struct_lit(start, name);
        }
        // `Name { field:` can't begin a block, so it must be a misplaced literal.
        let looks_like_literal =
            matches!(self.peek_at(1), TokenKind::Ident(_)) && self.peek_at(2) == &TokenKind::Colon;
        if !looks_like_literal {
            return Ok(self.mk(start, ExprKind::Name(name.name)));
        }
        let help = format!("wrap it in parentheses: `({} {{ … }})`", name.name);
        let literal = self.with_struct_literals(true, |p| p.struct_lit(start, name))?;
        let message = "struct literal in a condition must be in parentheses";
        self.error(Diagnostic::error("E0205", message, literal.span).with_help(help));
        Ok(literal)
    }

    fn struct_lit(&mut self, start: usize, name: Ident) -> PResult<Expr> {
        let open = self.bump();
        let fields = self.comma_list(open, TokenKind::RBrace, |p| {
            let field = p.ident("field name")?;
            p.expect(&TokenKind::Colon)?;
            Ok((field, p.expr()?))
        })?;
        Ok(self.mk(start, ExprKind::StructLit(name, fields)))
    }

    /// `[]`, `[a, b, …]` or `[value; count]`, after the `[` at `open`.
    fn array(&mut self, start: usize, open: Span) -> PResult<Expr> {
        if self.eat(&TokenKind::RBracket) {
            return Ok(self.mk(start, ExprKind::Array(Vec::new())));
        }
        let first = self.expr()?;
        if self.eat(&TokenKind::Semi) {
            let count = self.expr()?;
            self.close(open, &TokenKind::RBracket)?;
            return Ok(self.mk(start, ExprKind::Repeat(Box::new(first), Box::new(count))));
        }
        let mut elements = vec![first];
        if self.eat(&TokenKind::Comma) {
            elements.extend(self.comma_list(open, TokenKind::RBracket, |p| p.expr())?);
        } else {
            self.close(open, &TokenKind::RBracket)?;
        }
        Ok(self.mk(start, ExprKind::Array(elements)))
    }

    /// An `if` expression or a block expression.
    pub(super) fn block_like(&mut self) -> PResult<Expr> {
        if self.at(&TokenKind::If) {
            return self.if_expr();
        }
        let start = self.span().start;
        let block = self.block()?;
        Ok(self.mk(start, ExprKind::Block(block)))
    }

    fn if_expr(&mut self) -> PResult<Expr> {
        self.nested(|p| {
            let start = p.bump().start;
            let cond = p.cond()?;
            let then = p.block()?;
            let else_ = if p.eat(&TokenKind::Else) {
                match p.peek() {
                    TokenKind::If | TokenKind::LBrace => Some(Box::new(p.block_like()?)),
                    _ => return Err(p.expected("`{` or `if`")),
                }
            } else {
                None
            };
            Ok(p.mk(
                start,
                ExprKind::If {
                    cond: Box::new(cond),
                    then,
                    else_,
                },
            ))
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{body, errors, expr};

    #[test]
    fn literal_forms() {
        assert_eq!(
            expr("Point { x: 1.0, y: 2.0, }"),
            "(struct-lit Point (x 1.0) (y 2.0))"
        );
        assert_eq!(expr("Empty {}"), "(struct-lit Empty)");
        assert_eq!(expr("[]"), "(array)");
        assert_eq!(expr("[1, 2,]"), "(array 1 2)");
        assert_eq!(expr("[false; n + 1]"), "(repeat false (+ n 1))");
        assert_eq!(expr("(1 + 2) * 3"), "(* (+ 1 2) 3)");
    }

    #[test]
    fn struct_literal_in_condition_is_e0205_and_parsing_continues() {
        let src = "fun t() { if p == Point { x: 0.0, y: 0.0 } { } let z = ; }";
        assert_eq!(
            errors(src),
            [("E0205", "Point { x: 0.0, y: 0.0 }"), ("E0201", ";")]
        );
    }

    #[test]
    fn struct_literal_restriction_lifts_inside_brackets_and_blocks() {
        assert_eq!(
            body("if p == (Point { x: 0.0 }) { }"),
            "(block (if (== p (struct-lit Point (x 0.0))) (block)))"
        );
        assert_eq!(
            body("if ok { Point { x: 1.0 } } else { o }"),
            "(block (if ok (block (struct-lit Point (x 1.0))) (block o)))"
        );
        assert_eq!(
            body("while f(P { a: 1 }) { }"),
            "(block (while (call f (struct-lit P (a 1))) (block)))"
        );
    }

    #[test]
    fn if_else_chains_are_expressions() {
        assert_eq!(
            body("let v = if c { 1 } else if d { 2 } else { 3 };"),
            "(block (let v (if c (block 1) (if d (block 2) (block 3)))))"
        );
    }
}
