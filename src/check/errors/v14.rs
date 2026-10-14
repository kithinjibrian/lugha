//! Codes new in v14 (milestones 45–50): E0433 for refutable patterns.

use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0433: a pattern in `let` without `else`, or in a `for` destructuring,
/// that some value wouldn't match.
pub(in crate::check) fn refutable(span: Span) -> Diagnostic {
    Diagnostic::error("E0433", "this pattern doesn't match every value", span)
        .with_help("use `let … else { … };`, or `match`, for a pattern that can fail")
}
