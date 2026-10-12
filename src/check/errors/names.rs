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
    let d = Diagnostic::error("E0305", format!("cannot find type `{name}`"), span);
    // Rust habit: recursion needs no box in Lugha (v1 §4).
    if name == "Box" {
        d.with_help("recursive enums need no `Box`; write the type itself")
    } else {
        d
    }
}

pub(in crate::check) fn recursive_struct(name: &str, path: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0307", format!("struct `{name}` contains itself"), span)
        .with_primary_label(path)
        .with_help(format!(
            "a struct can't hold itself inline; an array can, e.g. a field of type `{name}[]`"
        ))
}

pub(in crate::check) fn duplicate_field(
    field: &str,
    name: &str,
    span: Span,
    first: Span,
) -> Diagnostic {
    Diagnostic::error(
        "E0308",
        format!("field `{field}` is declared twice in `{name}`"),
        span,
    )
    .with_label(first, "first declared here")
}

pub(in crate::check) fn builtin_name(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0302", format!("`{name}` is a built-in name"), span)
}

pub(in crate::check) fn duplicate_variant(
    variant: &str,
    name: &str,
    span: Span,
    first: Span,
) -> Diagnostic {
    Diagnostic::error(
        "E0309",
        format!("variant `{variant}` is declared twice in `{name}`"),
        span,
    )
    .with_label(first, "first declared here")
}

pub(in crate::check) fn local_named_like_type(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0311", format!("`{name}` is the name of a type"), span)
        .with_help("rename the variable")
}
