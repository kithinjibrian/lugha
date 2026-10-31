//! `defer` (v17, milestone 151; spec §6): the deferred statement has type
//! `()` (E0456) and can't leave its own body (E0455), since it runs while
//! its block is already being left.

use super::{Checker, Type, errors};
use crate::ast::{Stmt, StmtKind};
use crate::span::Span;

/// The `defer` being checked: where it is, and how many loops enclosed it,
/// so a `break` of a loop inside it is told from one leaving it.
#[derive(Clone, Copy)]
pub(super) struct Deferring {
    span: Span,
    loops: usize,
}

impl Checker {
    /// `defer e;`: `e` is checked where it is written (it runs later, but
    /// sees the same names) and must have type `()`; an assignment is
    /// checked as the statement it is (PRP-183).
    pub(super) fn defer_stmt(&mut self, inner: &Stmt, span: Span) {
        // The label points at the keyword, not the whole statement.
        let keyword = Span::new(span.start, span.start + "defer".len());
        let saved = self.deferring.replace(Deferring {
            span: keyword,
            loops: self.loops.len(),
        });
        if let StmtKind::Expr { expr, .. } = &inner.kind {
            let ty = self.expr(expr, None);
            if !matches!(ty, Type::Unit | Type::Never | Type::Error) {
                self.report(errors::deferred_value(ty, expr.span));
            }
        } else {
            self.stmt(inner);
        }
        self.deferring = saved;
    }

    /// Reports E0455 when `keyword` (`return`, `?`, `break`, `continue`)
    /// would leave the deferred statement being checked. A `break` or
    /// `continue` whose `target` loop (an index in `loops`) is inside it
    /// stays inside; a closure's body is its own (`closures.rs` clears
    /// `deferring`). A jump with no target was already reported.
    pub(super) fn leaves_defer(&mut self, keyword: &str, span: Span, target: Option<usize>) {
        let Some(deferring) = self.deferring else {
            return;
        };
        let jump = matches!(keyword, "break" | "continue");
        if jump && target.is_none() {
            return;
        }
        if !jump || target.is_some_and(|t| t < deferring.loops) {
            self.report(errors::leaves_defer(keyword, span, deferring.span));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    fn main(body: &str) -> String {
        format!("fun f(): Result<i64, string> = Ok(1);\nfun main() {{ {body} }}")
    }

    #[test]
    fn a_unit_statement_or_block_is_accepted() {
        ok(&main("let x = 1; defer println(x); defer { println(2); }"));
        ok(&main("defer { let r = f(); }"));
        ok(&main("defer { f(); }"));
        ok(&main("defer if true { println(1); }"));
        ok(&main("defer panic(\"no\");"));
    }

    #[test]
    fn a_value_is_e0456() {
        assert_eq!(errors(&main("defer f();")), [("E0456", "f()")]);
        assert_eq!(errors(&main("defer 1 + 2;")), [("E0456", "1 + 2")]);
        assert_eq!(errors(&main("defer { 3 }")), [("E0456", "{ 3 }")]);
    }

    #[test]
    fn leaving_the_deferred_statement_is_e0455() {
        assert_eq!(errors(&main("defer { return; }")), [("E0455", "return;")]);
        assert_eq!(
            errors(&main("while true { defer { break; } }")),
            [("E0455", "break;")]
        );
        assert_eq!(
            errors(&main("for i in 0..3 { defer { continue; } }")),
            [("E0455", "continue;")]
        );
        let src = "fun g(): Result<i64, string> { defer { let n = f()?; }\nOk(1) }\nfun f(): Result<i64, string> = Ok(1);\nfun main() {}";
        assert_eq!(errors(src), [("E0455", "f()?")]);
    }

    #[test]
    fn jumps_that_stay_inside_are_fine() {
        ok(&main("defer { while true { break; } }"));
        ok(&main("defer { for i in 0..3 { continue; } }"));
        ok(&main(
            "defer { let g = fun(): i64 { return 1; }; println(g()); }",
        ));
        ok(&main("defer { defer println(1); }"));
    }

    #[test]
    fn an_assignment_is_checked_as_the_statement_it_is() {
        ok(&main(
            "let mut n = 1; defer n += 1; defer n = 0; let mut xs = [1]; defer xs[0] = 2;",
        ));
        assert_eq!(errors(&main("let n = 1; defer n += 1;")), [("E0501", "n")]);
        assert_eq!(
            errors(&main("let mut n = 1; defer n = \"a\";")),
            [("E0403", "\"a\"")]
        );
    }
}
