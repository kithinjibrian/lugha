//! Pointer parameters (v17): `out name: T` (milestone 102), `ptr name: T`
//! and `mut name: T` (milestone 103) in an extern's parameters, and `out
//! place`, `ptr e`, `mut place` as call arguments. `out` and `ptr` are
//! contextual: keywords only there.

use super::{PResult, Parser};
use crate::ast::{Expr, ExprKind, Param, PassMode};
use crate::diagnostic::Diagnostic;
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    /// `out`, `ptr` or `mut` before a parameter or argument, if it is
    /// there: the mode and its span, the word consumed. `out`/`mut` need a
    /// name after them; a `ptr` argument may take any expression that
    /// can't continue the word (`ptr(x)` stays a call of `ptr`).
    fn mode_word(&mut self, argument: bool) -> Option<(PassMode, Span)> {
        let named = matches!(self.peek_at(1), TokenKind::Ident(_) | TokenKind::SelfKw);
        let mode = match self.peek() {
            TokenKind::Ident(w) if w == "out" && named => PassMode::Out,
            TokenKind::Mut if named => PassMode::Mut,
            TokenKind::Ident(w) if w == "ptr" => {
                let value = matches!(
                    self.peek_at(1),
                    TokenKind::Int(_)
                        | TokenKind::Float(_)
                        | TokenKind::Str(_)
                        | TokenKind::Char(_)
                        | TokenKind::True
                        | TokenKind::False
                        | TokenKind::LBracket
                        | TokenKind::HashBrace
                );
                if !(named || (argument && value)) {
                    return None;
                }
                PassMode::Ptr
            }
            _ => return None,
        };
        Some((mode, self.bump()))
    }

    /// An extern's parameter: `[out | ptr | mut] name: T`.
    pub(super) fn extern_param(&mut self) -> PResult<Param> {
        let mode = self.mode_word(false);
        let mut param = self.param("parameter name")?;
        param.mode = mode;
        Ok(param)
    }

    /// `out`, `ptr` or `mut` before a parameter that isn't an extern's:
    /// E0201, and the parameter is read as if it weren't there.
    pub(super) fn misplaced_out(&mut self) {
        if let Some((mode, at)) = self.mode_word(false) {
            let message = format!("only an `extern fun`'s parameters can be `{}`", mode.word());
            let d = Diagnostic::error("E0201", message, at).with_help(
                "a Lugha function returns its results; these are for C's pointer parameters",
            );
            self.error(d);
        }
    }

    /// A call argument: `expr`, or `out place`, `ptr e`, `mut place` (v17).
    /// Never inlined: calls are on every nesting level's stack (E0206
    /// assumes small frames).
    #[inline(never)]
    pub(super) fn argument(&mut self) -> PResult<Expr> {
        let start = self.span().start;
        if let Some((mode, _)) = self.mode_word(true) {
            let place = self.expr()?;
            return Ok(self.mk(start, ExprKind::Pass(mode, Box::new(place))));
        }
        self.expr()
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{errors, program};

    #[test]
    fn out_in_extern_parameters_and_arguments_v17() {
        assert_eq!(
            program(
                "extern fun get(c: i64, out n: u64): bool;\nfun f() { get(1, out x); get(1, out s.n[0]); }"
            ),
            "(extern get ((c i64) (out n u64)) bool)\n(fun f () void (block (; (call get 1 (out x))) (; (call get 1 (out (index (. s n) 0))))))"
        );
        // `out` alone is still a name.
        assert_eq!(
            program("fun f(out: i64) { g(out); }"),
            "(fun f ((out i64)) void (block (; (call g out))))"
        );
    }

    #[test]
    fn ptr_and_mut_parameters_and_arguments_v17() {
        assert_eq!(
            program(
                "extern fun get(ptr i: P, mut n: u64);\nfun f() { get(ptr p, mut x); get(ptr 3, mut s.n); }"
            ),
            "(extern get ((ptr i P) (mut n u64)) void)\n(fun f () void (block (; (call get (ptr p) (mut x))) (; (call get (ptr 3) (mut (. s n))))))"
        );
        // `ptr(x)` is a call of a function named `ptr`.
        assert_eq!(
            program("fun f() { g(ptr(x)); }"),
            "(fun f () void (block (; (call g (call ptr x)))))"
        );
    }

    #[test]
    fn out_on_a_lugha_parameter_is_e0201_v17() {
        assert_eq!(errors("fun f(out n: u64) {}"), [("E0201", "out")]);
    }
}
