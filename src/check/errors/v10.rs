//! Codes new in v10 (milestones 34–36): E0430 for default methods.

use crate::check::Type;
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0430: two visible interfaces, `names`, supply default `name` for `ty`.
pub(in crate::check) fn two_defaults(
    name: &str,
    ty: &Type,
    names: &[String; 2],
    span: Span,
) -> Diagnostic {
    let [a, b] = names;
    Diagnostic::error(
        "E0430",
        format!("`{name}` on `{ty}` is a default method of both `{a}` and `{b}`"),
        span,
    )
    .with_help(format!(
        "call it through a value or bound of the one you mean: `let x: {a} = …;` or `T: {a}`"
    ))
}
