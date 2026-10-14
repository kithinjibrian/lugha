//! Codes new in v9 (milestones 31–33): E0428 for or-patterns and `mut`.

use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0428: an or-pattern's alternatives bind differently, or `mut` is on
/// something that isn't a binding; `why` says which.
pub(in crate::check) fn or_bindings(why: String, span: Span) -> Diagnostic {
    Diagnostic::error("E0428", why, span)
        .with_help("every alternative of `p | q` must bind the same names, each at one type")
}
