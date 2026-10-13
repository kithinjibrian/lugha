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

/// E0422 for a value of one interface type that doesn't declare every item
/// of the interface it should convert to (v6).
pub(in crate::check) fn undeclared(
    from: &crate::check::Type,
    to: &str,
    problems: &[String],
    span: Span,
) -> Diagnostic {
    Diagnostic::error(
        "E0422",
        format!("`{from}` doesn't declare everything `{to}` needs"),
        span,
    )
    .with_help(problems.join("; "))
}
