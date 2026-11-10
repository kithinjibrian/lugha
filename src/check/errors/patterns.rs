//! Diagnostics for patterns: or-patterns and `mut` (E0428, E0429), and refutable patterns where every
//! value must match (E0433).

use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0428: an or-pattern's alternatives bind differently, or `mut` is on
/// something that isn't a binding; `why` says which.
pub(in crate::check) fn or_bindings(why: String, span: Span) -> Diagnostic {
    Diagnostic::error("E0428", why, span)
        .with_help("every alternative of `p | q` must bind the same names, each at one type")
}

/// E0429: a `let … else` block that can finish normally.
pub(in crate::check) fn let_else_finishes(span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0429",
        "the `else` of a `let … else` must not finish",
        span,
    )
    .with_help("end it with `return`, `break`, `continue`, `panic(…)` or `exit(…)`")
}

/// E0433: a pattern in `let` without `else`, or in a `for` destructuring,
/// that some value wouldn't match.
pub(in crate::check) fn refutable(span: Span) -> Diagnostic {
    Diagnostic::error("E0433", "this pattern doesn't match every value", span)
        .with_help("use `let … else { … };`, or `match`, for a pattern that can fail")
}
