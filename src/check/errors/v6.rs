//! Codes new in v6 (milestones 23–26): E0427 for interfaces that use `Self`.

use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0427: an interface whose method uses `Self` written as a type.
pub(in crate::check) fn interface_uses_self(name: &str, item: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0427",
        format!("`{name}` can't be a type: `{item}` uses `Self`"),
        span,
    )
    .with_help(format!(
        "two `{name}` values could hold different types; use it as a bound: `<T: {name}>`"
    ))
}
