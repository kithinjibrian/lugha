//! `out` parameters (v17, milestone 102, PRP-134): `out name: T` in an
//! extern's parameters, and `out place` as a call argument. `out` is
//! contextual: a keyword only there, before a name.

use super::{PResult, Parser};
use crate::ast::{Expr, ExprKind, Param};
use crate::diagnostic::Diagnostic;
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    /// `out` before a name, if it is there: its span, the word consumed.
    fn out_word(&mut self) -> Option<Span> {
        let is_out = matches!(self.peek(), TokenKind::Ident(w) if w == "out");
        let named = matches!(self.peek_at(1), TokenKind::Ident(_) | TokenKind::SelfKw);
        (is_out && named).then(|| self.bump())
    }

    /// An extern's parameter: `[out] name: T`.
    pub(super) fn extern_param(&mut self) -> PResult<Param> {
        let out = self.out_word();
        let mut param = self.param("parameter name")?;
        param.out = out;
        Ok(param)
    }

    /// `out` before a parameter that isn't an extern's: E0201, and the
    /// parameter is read as if it weren't there.
    pub(super) fn misplaced_out(&mut self) {
        if let Some(at) = self.out_word() {
            let d = Diagnostic::error(
                "E0201",
                "only an `extern fun`'s parameters can be `out`",
                at,
            )
            .with_help("a Lugha function returns its results; `out` is for C's `T *` parameters");
            self.error(d);
        }
    }

    /// A call argument: `expr`, or `out place` (v17). Never inlined: calls
    /// are on every nesting level's stack (E0206 assumes small frames).
    #[inline(never)]
    pub(super) fn argument(&mut self) -> PResult<Expr> {
        let start = self.span().start;
        if self.out_word().is_some() {
            let place = self.expr()?;
            return Ok(self.mk(start, ExprKind::Out(Box::new(place))));
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
    fn out_on_a_lugha_parameter_is_e0201_v17() {
        assert_eq!(errors("fun f(out n: u64) {}"), [("E0201", "out")]);
    }
}
