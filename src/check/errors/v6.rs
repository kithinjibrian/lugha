//! Diagnostics new in v6 (milestones 23–26), under existing codes until a
//! milestone adds its own.

use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0418 for a generic interface written as a type: generic interface
/// values come with milestone 23's second half (PRP-051).
pub(in crate::check) fn generic_interface_value(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0418",
        format!("generic interface `{name}` can't be a value type yet"),
        span,
    )
    .with_help(format!("use it as a bound: `<S: {name}<…>>`"))
}
