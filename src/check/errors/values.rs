//! Diagnostics for building values: struct update (E0440, W0104), variant values (E0412) and
//! bindings of the unit type (W0106).

use crate::check::Type;
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// `..base` of another type than the literal's struct.
pub(in crate::check) fn base_type(literal: &Type, found: &Type, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0440",
        format!("`..base` must be a `{literal}`, but this is a `{found}`"),
        span,
    )
    .with_help("the fields not written are copied from `base`, so it must be the same struct")
}

/// `..base` when every field is written: nothing is copied from it.
pub(in crate::check) fn base_unused(span: Span) -> Diagnostic {
    Diagnostic::warning(
        "W0104",
        "every field is written, so `..base` copies nothing",
        span,
    )
    .with_help("remove `..base`, or leave out the fields it should supply")
}

/// A generic variant used as a value with nothing fixing its type arguments.
pub(in crate::check) fn variant_value_infer(variant: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0412",
        format!("cannot infer the type arguments of `{variant}` used as a function"),
        span,
    )
    .with_help("say what it should be: `let f: fun(i64): Option<i64> = Some;`, or pass it where the type is known")
}

/// `let x = e` where `e` is `()` but not the literal.
pub(in crate::check) fn unit_binding(name: &str, span: Span, init: Span) -> Diagnostic {
    Diagnostic::warning("W0106", format!("`{name}` holds `()`"), span)
        .with_label(init, "this has no other value than `()`")
        .with_help(format!(
            "drop the `let`, or write `let {name}: () = …` if that's what you meant"
        ))
}
