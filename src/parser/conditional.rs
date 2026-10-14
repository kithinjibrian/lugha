//! Conditional patterns (v9 draft §3, milestone 33): `if let`, `while let`
//! and `let … else`, each a pattern tested against one value.

use super::{PResult, Parser};
use crate::ast::{Expr, ExprKind, IfLet, Pattern, PatternKind, StmtKind};
use crate::lexer::TokenKind;

impl Parser<'_> {
    /// `if let pattern = cond { … } [else …]`, after `if` (at `start`).
    pub(super) fn if_let(&mut self, start: usize) -> PResult<Expr> {
        self.bump();
        let pattern = self.pattern()?;
        self.expect(&TokenKind::Eq)?;
        let scrutinee = self.cond()?;
        let then = self.block()?;
        let else_ = self.else_part()?;
        let parts = IfLet {
            pattern,
            scrutinee,
            then,
            else_,
        };
        Ok(self.mk(start, ExprKind::IfLet(Box::new(parts))))
    }

    /// `else { … }` or `else if …`, if there is one.
    pub(super) fn else_part(&mut self) -> PResult<Option<Box<Expr>>> {
        if !self.eat(&TokenKind::Else) {
            return Ok(None);
        }
        match self.peek() {
            TokenKind::If | TokenKind::LBrace => Ok(Some(Box::new(self.block_like()?))),
            _ => Err(self.expected("`{` or `if`")),
        }
    }

    /// `while let pattern = cond { … }`, after `while`.
    pub(super) fn while_let(&mut self) -> PResult<StmtKind> {
        self.bump();
        let pattern = self.pattern()?;
        self.expect(&TokenKind::Eq)?;
        let scrutinee = self.cond()?;
        let body = self.block()?;
        Ok(StmtKind::WhileLet {
            pattern: Box::new(pattern),
            scrutinee,
            body: Box::new(body),
        })
    }

    /// True if the `let` statement here (after `let`) starts with a pattern
    /// rather than `[mut] name [: type] =`.
    pub(super) fn let_pattern_ahead(&self) -> bool {
        match self.peek() {
            TokenKind::Mut => false,
            TokenKind::Ident(_) => !matches!(self.peek_at(1), TokenKind::Colon | TokenKind::Eq),
            _ => true,
        }
    }

    /// `let pattern = init else { … };`, after `let`.
    pub(super) fn let_else(&mut self) -> PResult<StmtKind> {
        let pattern = self.pattern()?;
        self.expect(&TokenKind::Eq)?;
        let init = self.expr()?;
        self.let_else_rest(pattern, init)
    }

    /// `else { … };` after a `let`'s initialiser.
    pub(super) fn let_else_rest(&mut self, pattern: Pattern, init: Expr) -> PResult<StmtKind> {
        // Without `else` (v14) the pattern must always match (E0433).
        let else_ = match self.eat(&TokenKind::Else) {
            true => Some(Box::new(self.block()?)),
            false => None,
        };
        self.expect(&TokenKind::Semi)?;
        Ok(StmtKind::LetElse {
            pattern: Box::new(pattern),
            init,
            else_,
        })
    }

    /// `let name = init else { … };`: the name as a pattern.
    pub(super) fn named_let_else(
        &mut self,
        name: crate::ast::Ident,
        init: Expr,
    ) -> PResult<StmtKind> {
        let pattern = Pattern {
            span: name.span,
            kind: PatternKind::Variant(name, None),
        };
        self.let_else_rest(pattern, init)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{body, errors};

    #[test]
    fn conditional_patterns_parse() {
        assert_eq!(
            body("if let Some(x) = o { f(x); } else if let None = o { g(); } else { h(); }"),
            "(block (if-let (Some x) o (block (; (call f x))) (if-let None o (block (; (call g))) (block (; (call h))))))"
        );
        assert_eq!(
            body("while let Some(x) = it.next() { f(x); }"),
            "(block (while-let (Some x) (call (. it next)) (block (; (call f x)))))"
        );
        assert_eq!(
            body("let Some(n) = o else { return; }; let x = e else { return; };"),
            "(block (let-else (Some n) o (block (return))) (let-else x e (block (return))))"
        );
        assert_eq!(
            body("let Some(mut n) | Other(mut n) = o else { return; };"),
            "(block (let-else (| (Some (mut n)) (Other (mut n))) o (block (return))))"
        );
        assert!(!errors("fun f() { let Some(n) = o else { return; } }").is_empty());
    }

    #[test]
    fn for_of_mut_parses_v13() {
        assert_eq!(
            crate::parser::test_util::body("for x of mut xs { }"),
            "(block (for x (of-mut xs) (block)))"
        );
    }

    #[test]
    fn interpolations_take_format_specifiers_v13() {
        assert_eq!(
            crate::parser::test_util::expr("\"{x:>5}!\""),
            "(interp \"\" (fmt x \">5\") \"!\")"
        );
    }
}
