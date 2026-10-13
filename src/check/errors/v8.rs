//! Codes changed in v8 (milestone 30): E0313 for another package's item
//! that isn't `public`, and E0318 for a declaration exposing a less visible
//! type.

use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0313: `name` has no visibility keyword, so only its own package may use it.
pub(in crate::check) fn package_item(
    name: &str,
    package: Option<&str>,
    span: Span,
    help: Option<String>,
) -> Diagnostic {
    let home = package.map_or_else(|| "the program".to_string(), |p| format!("package `{p}`"));
    Diagnostic::error(
        "E0313",
        format!("`{name}` is not `public`: it is only visible inside {home}"),
        span,
    )
    .with_help(help.unwrap_or_else(|| "mark it `public` to use it from other packages".into()))
}

/// E0318: a declaration visible beyond `ty`'s reach mentions it. `public`
/// says which rule: a `public` declaration needs `public` types (v8), any
/// other one only types that aren't `private` (v2).
pub(in crate::check) fn leaked_type(
    ty: &str,
    public: bool,
    span: Span,
    declared: Option<Span>,
) -> Diagnostic {
    let (message, help, label) = if public {
        (
            format!("`{ty}` is not `public`, but this declaration is"),
            "make the type `public` too, or drop `public` from this declaration",
            "declared without `public` here",
        )
    } else {
        (
            format!("`{ty}` is private, but this signature is visible to other modules"),
            "make this declaration `private` too, or remove `private` from the type",
            "declared `private` here",
        )
    };
    let d = Diagnostic::error("E0318", message, span).with_help(help);
    match declared {
        Some(at) => d.with_label(at, label),
        None => d,
    }
}
