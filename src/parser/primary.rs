//! Primary expressions: literals, names, struct literals, arrays, and the
//! block-like `if` and `{ }` expressions (spec §3).

use super::{PResult, Parser};
use crate::ast::{Expr, ExprKind, FormatText, Ident, Part, StructLiteral};
use crate::diagnostic::Diagnostic;
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    pub(super) fn primary(&mut self) -> PResult<Expr> {
        let start = self.span().start;
        let kind = match self.peek().clone() {
            TokenKind::Int(value) => ExprKind::Int(value),
            TokenKind::Float(value) => ExprKind::Float(value),
            TokenKind::Char(code) => ExprKind::Char(code),
            TokenKind::Str(value) => ExprKind::Str(value),
            TokenKind::Fun => {
                self.bump();
                return self.closure(start);
            }
            TokenKind::StrStart(text) => {
                self.bump();
                return self.interpolated(start, text);
            }
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
                if self.at(&TokenKind::Comma) {
                    return self.tuple_literal(start, open, inner);
                }
                self.close(open, &TokenKind::RParen)?;
                return Ok(inner);
            }
            TokenKind::LBracket => {
                let open = self.bump();
                return self.with_struct_literals(true, |p| p.array(start, open));
            }
            TokenKind::If | TokenKind::LBrace | TokenKind::Match => return self.block_like(),
            // `i16.MAX`: an integer type keyword before `.` is a name (v10).
            TokenKind::TyI8
            | TokenKind::TyI16
            | TokenKind::TyI32
            | TokenKind::TyI64
            | TokenKind::TyU8
            | TokenKind::TyU16
            | TokenKind::TyU32
            | TokenKind::TyU64
                if self.peek_at(1) == &TokenKind::Dot =>
            {
                ExprKind::Name(super::describe::symbol(self.peek()).to_string())
            }
            // `self` is a name the checker binds like a parameter (v1, milestone 7).
            TokenKind::SelfKw => ExprKind::Name("self".to_string()),
            _ => return Err(self.expected("expression")),
        };
        self.bump();
        Ok(self.mk(start, kind))
    }

    /// After an identifier: a struct literal if `{` follows and literals are
    /// allowed; E0205 for an unparenthesised literal in a condition.
    fn name_or_struct(&mut self, start: usize, name: Ident) -> PResult<Expr> {
        let name = self.self_name(name);
        match self.struct_lit_after(start, None, name.clone())? {
            Some(literal) => Ok(literal),
            None => Ok(self.mk(start, ExprKind::Name(name.name))),
        }
    }

    /// After `Name` or `module.Name`: the struct literal that a `{` starts,
    /// or `None` when there is no `{`, or when in a condition it begins the
    /// block instead (spec §3; v2 draft §3 for `m.Name { … }`).
    // Not inlined: `primary` and `postfix` recurse once per nesting level,
    // and their stack frames must stay small for the E0206 depth limit.
    #[inline(never)]
    pub(super) fn struct_lit_after(
        &mut self,
        start: usize,
        module: Option<Ident>,
        name: Ident,
    ) -> PResult<Option<Expr>> {
        if !self.at(&TokenKind::LBrace) {
            return Ok(None);
        }
        if !self.no_struct {
            return self.struct_lit(start, module, name).map(Some);
        }
        // `Name { field:` can't begin a block, so it must be a misplaced literal.
        let looks_like_literal = !self.scrutinee
            && matches!(self.peek_at(1), TokenKind::Ident(_))
            && self.peek_at(2) == &TokenKind::Colon;
        if !looks_like_literal {
            return Ok(None);
        }
        let written = match &module {
            Some(m) => format!("{}.{}", m.name, name.name),
            None => name.name.clone(),
        };
        let help = format!("wrap it in parentheses: `({written} {{ … }})`");
        let literal = self.with_struct_literals(true, |p| p.struct_lit(start, module, name))?;
        let message = "struct literal in a condition must be in parentheses";
        self.error(Diagnostic::error("E0205", message, literal.span).with_help(help));
        Ok(Some(literal))
    }

    fn struct_lit(&mut self, start: usize, module: Option<Ident>, name: Ident) -> PResult<Expr> {
        let open = self.bump();
        let fields = self.comma_list(open, TokenKind::RBrace, |p| {
            let field = p.ident("field name")?;
            p.expect(&TokenKind::Colon)?;
            Ok((field, p.expr()?))
        })?;
        let literal = StructLiteral {
            module,
            name,
            fields,
        };
        Ok(self.mk(start, ExprKind::StructLit(Box::new(literal))))
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

    /// An `if`, `match` or block expression.
    pub(super) fn block_like(&mut self) -> PResult<Expr> {
        if self.at(&TokenKind::If) {
            return self.if_expr();
        }
        if self.at(&TokenKind::Match) {
            return self.match_expr();
        }
        let start = self.span().start;
        let block = self.block()?;
        Ok(self.mk(start, ExprKind::Block(block)))
    }

    fn if_expr(&mut self) -> PResult<Expr> {
        self.nested(|p| {
            let start = p.bump().start;
            if p.at(&TokenKind::Let) {
                return p.if_let(start);
            }
            let cond = p.cond()?;
            let then = p.block()?;
            let else_ = p.else_part()?;
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

impl Parser<'_> {
    /// `"a {x} b"` after its `StrStart`: an expression, then `StrMid` and
    /// another expression, until `StrEnd` (v3 draft §3).
    fn interpolated(&mut self, start: usize, first: String) -> PResult<Expr> {
        let mut parts = vec![Part::Text(first)];
        loop {
            // Inside `{…}` a struct literal is fine: the braces delimit it.
            let e = self.with_struct_literals(true, |p| p.expr())?;
            // `{x:>5}` (v13): the lexer gives the specifier as one token.
            let format = match self.peek().clone() {
                TokenKind::FmtSpec(text) => Some(FormatText {
                    text,
                    span: self.bump(),
                }),
                _ => None,
            };
            parts.push(Part::Expr(e, format));
            match self.peek().clone() {
                TokenKind::StrMid(text) => {
                    self.bump();
                    parts.push(Part::Text(text));
                }
                TokenKind::StrEnd(text) => {
                    self.bump();
                    parts.push(Part::Text(text));
                    return Ok(self.mk(start, ExprKind::Interp(parts)));
                }
                _ => return Err(self.expected("`}`")),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{body, errors, expr};

    #[test]
    fn interpolated_strings_hold_expressions() {
        assert_eq!(expr(r#""a {x + 1} b""#), r#"(interp "a " (+ x 1) " b")"#);
        assert_eq!(
            expr(r#""{p.name}: {f(2)}!""#),
            r#"(interp "" (. p name) ": " (call f 2) "!")"#
        );
        assert_eq!(expr(r#""plain""#), r#""plain""#);
        // String literals and interpolations nest inside `{…}` (v4.1).
        assert_eq!(
            expr(r#""<{xs.join("-")}>""#),
            r#"(interp "<" (call (. xs join) "-") ">")"#
        );
    }

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

    #[test]
    fn the_v10_integer_types_and_their_limits() {
        assert_eq!(
            body("let a: i8 = 1; let b = a as i16; let c: u16 = 2;"),
            "(block (let a i8 1) (let b (as a i16)) (let c u16 2))"
        );
        assert_eq!(expr("i16.MAX"), "(. i16 MAX)");
        assert_eq!(
            expr("u8.MIN.count_ones()"),
            "(call (. (. u8 MIN) count_ones))"
        );
    }
}
