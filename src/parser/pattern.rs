//! `match` expressions, their patterns and guards (v1 §3, v2 draft §3).

use super::{PResult, Parser};
use crate::ast::{Arm, Expr, ExprKind, Pattern, PatternKind};
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    /// `match cond { pattern => expr, … }`. A `,` is optional after an arm
    /// whose body is a block.
    pub(super) fn match_expr(&mut self) -> PResult<Expr> {
        self.nested(|p| {
            let start = p.bump().start;
            let scrutinee = p.cond()?;
            let open = p.expect(&TokenKind::LBrace)?;
            let mut arms = Vec::new();
            loop {
                if p.eat(&TokenKind::RBrace) {
                    break;
                }
                if p.at(&TokenKind::Eof) {
                    p.close(open, &TokenKind::RBrace)?;
                }
                let arm = match p.arm() {
                    Ok(arm) => arm,
                    Err(_) => {
                        // Skip to the next arm so one bad arm reports one error.
                        p.sync_arm();
                        continue;
                    }
                };
                let block_body = matches!(arm.body.kind, ExprKind::Block(_));
                arms.push(arm);
                if !p.eat(&TokenKind::Comma) && !block_body {
                    p.close(open, &TokenKind::RBrace)?;
                    break;
                }
            }
            Ok(p.mk(start, ExprKind::Match(Box::new(scrutinee), arms)))
        })
    }

    /// Skips past the next `,` at this nesting level, or up to the `}` that
    /// closes the `match`.
    fn sync_arm(&mut self) {
        let mut depth = 0usize;
        loop {
            match self.peek() {
                TokenKind::Eof => return,
                TokenKind::LBrace | TokenKind::LParen | TokenKind::LBracket => depth += 1,
                TokenKind::RBrace if depth == 0 => return,
                TokenKind::RBrace | TokenKind::RParen | TokenKind::RBracket => {
                    depth = depth.saturating_sub(1);
                }
                TokenKind::Comma if depth == 0 => {
                    self.bump();
                    return;
                }
                _ => {}
            }
            self.bump();
        }
    }

    fn arm(&mut self) -> PResult<Arm> {
        let pattern = self.pattern()?;
        // A guard is a condition: `if x == P { … } =>` would be ambiguous.
        let guard = if self.eat(&TokenKind::If) {
            Some(self.cond()?)
        } else {
            None
        };
        self.expect(&TokenKind::FatArrow)?;
        let body = self.expr()?;
        let span = Span::new(pattern.span.start, body.span.end);
        Ok(Arm {
            pattern,
            guard,
            body,
            span,
        })
    }

    /// One pattern; payload slots are patterns too, each a nesting level,
    /// so deep nesting is E0206 rather than a stack overflow.
    fn pattern(&mut self) -> PResult<Pattern> {
        self.nested(|p| p.pattern_here())
    }

    fn pattern_here(&mut self) -> PResult<Pattern> {
        let start = self.span().start;
        let kind = match self.peek().clone() {
            TokenKind::Ident(name) if name == "_" => {
                self.bump();
                PatternKind::Wildcard
            }
            TokenKind::Ident(_) => {
                let name = self.ident("pattern")?;
                let slots = if self.at(&TokenKind::LParen) {
                    let open = self.bump();
                    Some(self.comma_list(open, TokenKind::RParen, |p| p.pattern())?)
                } else {
                    None
                };
                PatternKind::Variant(name, slots)
            }
            TokenKind::Minus if matches!(self.peek_at(1), TokenKind::Int(_)) => {
                self.bump();
                let TokenKind::Int(value) = *self.peek() else {
                    unreachable!("checked just above")
                };
                self.bump();
                PatternKind::Int {
                    value,
                    negative: true,
                }
            }
            TokenKind::Int(value) => {
                self.bump();
                PatternKind::Int {
                    value,
                    negative: false,
                }
            }
            TokenKind::Str(text) => {
                self.bump();
                PatternKind::Str(text)
            }
            TokenKind::True | TokenKind::False => {
                let value = self.at(&TokenKind::True);
                self.bump();
                PatternKind::Bool(value)
            }
            _ => return Err(self.expected("a pattern")),
        };
        Ok(Pattern {
            kind,
            span: Span::new(start, self.prev_end()),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{body, errors, expr};

    #[test]
    fn match_is_an_expression_with_flat_patterns() {
        assert_eq!(
            expr("match s { Circle(r) => r * r, Rect(w, _) => w, Empty => 0.0, _ => 1.0 }"),
            "(match s (=> (Circle r) (* r r)) (=> (Rect w _) w) (=> Empty 0.0) (=> _ 1.0))"
        );
        assert_eq!(
            expr("match n { 0 => \"zero\", -1 => \"minus\", _ => \"other\", }"),
            "(match n (=> 0 \"zero\") (=> -1 \"minus\") (=> _ \"other\"))"
        );
        assert_eq!(
            expr("match b { true => 1, false => 0 }"),
            "(match b (=> true 1) (=> false 0))"
        );
    }

    #[test]
    fn block_arms_need_no_comma_and_match_is_block_like() {
        assert_eq!(
            body("match x { Some(v) => { f(v); } None => {} } let y = 1;"),
            "(block (stmt (match x (=> (Some v) (block (; (call f v)))) (=> None (block)))) (let y 1))"
        );
        // The scrutinee is a condition: `Name {` ends it.
        assert_eq!(expr("match e { A => 1 }"), "(match e (=> A 1))");
    }

    #[test]
    fn patterns_nest_and_arms_take_guards() {
        assert_eq!(
            expr(
                "match s { Some(Circle(r)) if r > 1.0 => r, Rect(_, Some(1)) => 0.0, Some(\"x\") => 1.0, Some(-2) => 2.0, _ => 3.0 }"
            ),
            "(match s (=> (Some (Circle r)) (guard (> r 1.0)) r) (=> (Rect _ (Some 1)) 0.0) (=> (Some \"x\") 1.0) (=> (Some -2) 2.0) (=> _ 3.0))"
        );
        let deep = format!(
            "fun t() {{ match x {{ {}y{} => 1 }} }}",
            "S(".repeat(300),
            ")".repeat(300)
        );
        assert!(
            errors(&deep).iter().any(|(code, _)| *code == "E0206"),
            "{:?}",
            errors(&deep)
        );
    }

    #[test]
    fn malformed_arms_are_e0201_and_parsing_continues() {
        assert_eq!(
            errors("fun t() { match x { A 1, B => 2 } let y = ; }"),
            [("E0201", "1"), ("E0201", ";")]
        );
        assert_eq!(
            errors("fun t() { match x { 1.5 => 0 } }"),
            [("E0201", "1.5")]
        );
    }
}
