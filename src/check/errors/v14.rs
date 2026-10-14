//! Codes new in v14 (milestones 45–50): E0432 for `format` templates,
//! E0433 for refutable patterns.

use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0432: a `format` template that isn't a literal, is malformed, or has
/// a different number of holes than values.
pub(in crate::check) fn bad_template(why: String, span: Span) -> Diagnostic {
    Diagnostic::error("E0432", why, span)
        .with_help("write `format(\"{} and {:>5}\", a, b)`: one `{}` or `{:spec}` per value")
}

/// E0313: `name` reached through `module`, a re-exported module (`m.v`),
/// isn't `public`.
pub(in crate::check) fn not_reexported(name: &str, module: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0313",
        format!("`{name}` is not `public`, so `{module}.{name}` can't reach it"),
        span,
    )
    .with_help("a module re-exported whole offers only its `public` items")
}

/// E0433: a pattern in `let` without `else`, or in a `for` destructuring,
/// that some value wouldn't match.
pub(in crate::check) fn refutable(span: Span) -> Diagnostic {
    Diagnostic::error("E0433", "this pattern doesn't match every value", span)
        .with_help("use `let … else { … };`, or `match`, for a pattern that can fail")
}
