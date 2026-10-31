//! `loop`, labels and the jumps that name them (v17, milestone 152; spec
//! §3, §5): `[label:] loop { … }`, a label before `while` and `for`,
//! `break [label] [value];` and `continue [label];`.

use super::{PResult, Parser};
use crate::ast::{Expr, ExprKind, Ident, StmtKind};
use crate::lexer::TokenKind;

impl Parser<'_> {
    /// True at `name:` followed by a loop keyword: a label. Off while a map
    /// literal's key is read, where `a: loop { … }` is an entry.
    pub(super) fn label_ahead(&self) -> bool {
        !self.no_label
            && matches!(self.peek(), TokenKind::Ident(_))
            && self.peek_at(1) == &TokenKind::Colon
            && matches!(
                self.peek_at(2),
                TokenKind::Loop | TokenKind::While | TokenKind::For
            )
    }

    /// `name:` before a loop.
    pub(super) fn label(&mut self) -> PResult<Ident> {
        let label = self.ident("label")?;
        self.expect(&TokenKind::Colon)?;
        Ok(label)
    }

    /// Runs `f` with `label` among the enclosing labels, so `break label`
    /// inside reads `label` as the label rather than a value.
    pub(super) fn labelled<T>(
        &mut self,
        label: Option<&Ident>,
        f: impl FnOnce(&mut Self) -> PResult<T>,
    ) -> PResult<T> {
        let Some(label) = label else {
            return f(self);
        };
        self.labels.push(label.name.clone());
        let result = f(self);
        self.labels.pop();
        result
    }

    /// `name: loop { … }` as a value, at the label. Kept out of `primary`,
    /// whose frame is on every nesting level's stack (E0206's limit).
    #[inline(never)]
    pub(super) fn labelled_loop(&mut self, start: usize) -> PResult<Expr> {
        let label = self.label()?;
        self.loop_expr(start, Some(label))
    }

    /// `[label:] loop { body }`, at `loop`.
    #[inline(never)]
    pub(super) fn loop_expr(&mut self, start: usize, label: Option<Ident>) -> PResult<Expr> {
        self.expect(&TokenKind::Loop)?;
        let body = Box::new(self.labelled(label.as_ref(), Self::block)?);
        Ok(self.mk(start, ExprKind::Loop { label, body }))
    }

    /// A loop statement after its label: `while`, `while let` or `for`
    /// (a labelled `loop` is an expression, read by the caller).
    pub(super) fn labelled_stmt(&mut self, label: Ident) -> PResult<StmtKind> {
        let mut kind = self.labelled(Some(&label), |p| match p.peek() {
            TokenKind::While if p.peek_at(1) == &TokenKind::Let => {
                p.bump();
                p.while_let()
            }
            TokenKind::While => {
                p.bump();
                let cond = p.cond()?;
                Ok(StmtKind::While {
                    label: None,
                    cond,
                    body: p.block()?,
                })
            }
            _ => p.for_stmt(),
        })?;
        if let StmtKind::While { label: l, .. }
        | StmtKind::WhileLet { label: l, .. }
        | StmtKind::For { label: l, .. } = &mut kind
        {
            *l = Some(label);
        }
        Ok(kind)
    }

    /// `break [label] [value];` or `continue [label];`, at the keyword.
    /// After `break`, a name is the label only when an enclosing loop has
    /// that label, so `break x;` with a variable `x` breaks with its value.
    pub(super) fn jump_stmt(&mut self) -> PResult<StmtKind> {
        let is_break = self.at(&TokenKind::Break);
        self.bump();
        let label = match self.peek() {
            TokenKind::Ident(name) if !is_break || self.labels.contains(name) => {
                Some(self.ident("label")?)
            }
            _ => None,
        };
        let value = if is_break && !self.at(&TokenKind::Semi) {
            Some(self.expr()?)
        } else {
            None
        };
        self.expect(&TokenKind::Semi)?;
        Ok(match is_break {
            true => StmtKind::Break { label, value },
            false => StmtKind::Continue { label },
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{body, errors};

    #[test]
    fn loops_and_their_values() {
        assert_eq!(body("loop { }"), "(block (loop (block)))");
        assert_eq!(body("loop { } x"), "(block (stmt (loop (block))) x)");
        assert_eq!(
            body("let x = loop { break 5; };"),
            "(block (let x (loop (block (break 5)))))"
        );
        assert_eq!(body("loop { break; }"), "(block (loop (block (break))))");
    }

    #[test]
    fn labels_on_every_loop() {
        assert_eq!(
            body("outer: for i in 0..n { continue outer; }"),
            "(block (for outer: i (range 0 n) (block (continue outer:))))"
        );
        assert_eq!(
            body("w: while c { break w; }"),
            "(block (while w: c (block (break w:))))"
        );
        assert_eq!(
            body("w: while let Some(x) = o { }"),
            "(block (while-let w: (Some x) o (block)))"
        );
        assert_eq!(
            body("let v = find: loop { break find 3; };"),
            "(block (let v (loop find: (block (break find: 3)))))"
        );
    }

    #[test]
    fn a_name_after_break_is_a_label_only_when_one_encloses() {
        assert_eq!(
            body("loop { break x; }"),
            "(block (loop (block (break x))))"
        );
        assert_eq!(
            body("x: loop { break x; }"),
            "(block (loop x: (block (break x:))))"
        );
        assert_eq!(
            body("x: loop { break x x; }"),
            "(block (loop x: (block (break x: x))))"
        );
    }

    #[test]
    fn a_map_key_is_never_a_label() {
        assert_eq!(
            body("let m = #{a: loop { break 1; }};"),
            "(block (let m (map (array a) (array (loop (block (break 1)))))))"
        );
    }

    #[test]
    fn loop_is_a_keyword() {
        assert_eq!(errors("fun t() { let loop = 1; }"), [("E0201", "loop")]);
    }
}
