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

pub(in crate::check) fn local_named_like_type(name: &str, span: Span, module: bool) -> Diagnostic {
    let what = if module {
        "an imported module"
    } else {
        "a type"
    };
    Diagnostic::error("E0311", format!("`{name}` is the name of {what}"), span)
        .with_help("rename the variable")
}

pub(in crate::check) fn duplicate_method(
    name: &str,
    ty: &str,
    span: Span,
    first: Span,
) -> Diagnostic {
    Diagnostic::error(
        "E0310",
        format!("method `{name}` is declared twice for `{ty}`"),
        span,
    )
    .with_label(first, "first declared here")
}

pub(in crate::check) fn method_named_like(
    name: &str,
    what: &str,
    ty: &str,
    span: Span,
) -> Diagnostic {
    Diagnostic::error(
        "E0310",
        format!("`{ty}` already has a {what} called `{name}`"),
        span,
    )
    .with_help("give the method a different name")
}

/// E0313: another module's `private` item, method or field (v2 draft §6).
pub(in crate::check) fn private_item(
    name: &str,
    module: &[String],
    span: Span,
    help: Option<String>,
) -> Diagnostic {
    let d = Diagnostic::error(
        "E0313",
        format!("`{name}` is private to module `{}`", module.join(".")),
        span,
    );
    match help {
        Some(help) => d.with_help(help),
        None => d.with_help("only its own module can use it; remove `private` to share it"),
    }
}

/// E0317: `public` is for packages, which arrive in v3.
pub(in crate::check) fn public_reserved(span: Span) -> Diagnostic {
    Diagnostic::error("E0317", "`public` needs packages (v3)", span)
        .with_help("remove it: everything is already visible to the whole program")
}

/// E0318: a signature other modules can use mentions a `private` type.
pub(in crate::check) fn leaked_private(ty: &str, span: Span, declared: Option<Span>) -> Diagnostic {
    let d = Diagnostic::error(
        "E0318",
        format!("`{ty}` is private, but this signature is visible to other modules"),
        span,
    )
    .with_help("make this declaration `private` too, or remove `private` from the type");
    match declared {
        Some(at) => d.with_label(at, "declared `private` here"),
        None => d,
    }
}

/// E0304 for `main` outside the main file (v2 draft §6).
pub(in crate::check) fn main_in_module(span: Span) -> Diagnostic {
    Diagnostic::error("E0304", "`main` belongs in the main file", span)
        .with_help("rename it, or move it to the file you pass to `lughac`")
}

/// E0306: the same C function declared with two signatures (v2 draft §8).
pub(in crate::check) fn extern_mismatch(name: &str, span: Span, first: Span) -> Diagnostic {
    Diagnostic::error(
        "E0306",
        format!("extern `{name}` is declared with a different signature elsewhere"),
        span,
    )
    .with_label(first, "first declared here")
    .with_help("every declaration of a C function must match")
}
