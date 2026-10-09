//! E03xx: names and scopes (spec §9).

use crate::diagnostic::Diagnostic;
use crate::span::Span;

pub(in crate::check) fn undefined(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0301", format!("cannot find `{name}` in this scope"), span)
}

pub(in crate::check) fn duplicate(name: &str, span: Span, first: Span) -> Diagnostic {
    Diagnostic::error("E0302", format!("`{name}` is defined more than once"), span)
        .with_primary_label("defined again here")
        .with_label(first, "first defined here")
}

pub(in crate::check) fn reserved(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0302",
        format!("`{name}` is the name of a built-in function"),
        span,
    )
}

pub(in crate::check) fn reserved_extern(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0306",
        format!("`{name}`: names starting with `lugha_` are reserved for the compiler and runtime"),
        span,
    )
}

pub(in crate::check) fn missing_main() -> Diagnostic {
    Diagnostic::error("E0303", "no `main` function", Span::new(0, 0))
        .with_help("add `fun main() { }`")
}

pub(in crate::check) fn bad_main(span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0304",
        "`main` must be `fun main()` or `fun main(): i32`",
        span,
    )
}

pub(in crate::check) fn unknown_type(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0305", format!("cannot find type `{name}`"), span)
}
