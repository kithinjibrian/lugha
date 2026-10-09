//! One constructor per checker error code, so the wording lives in one place
//! (spec §9: codes are stable, messages may improve).

use super::types::Type;
use crate::diagnostic::{Diagnostic, Label};
use crate::span::Span;

pub(super) fn undefined(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0301", format!("cannot find `{name}` in this scope"), span)
}

pub(super) fn duplicate(name: &str, span: Span, first: Span) -> Diagnostic {
    Diagnostic::error("E0302", format!("`{name}` is defined more than once"), span)
        .with_primary_label("defined again here")
        .with_label(first, "first defined here")
}

pub(super) fn reserved(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0302",
        format!("`{name}` is the name of a built-in function"),
        span,
    )
}

pub(super) fn missing_main() -> Diagnostic {
    Diagnostic::error("E0303", "no `main` function", Span::new(0, 0))
        .with_help("add `fun main() { }`")
}

pub(super) fn bad_main(span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0304",
        "`main` must be `fun main()` or `fun main(): i32`",
        span,
    )
}

pub(super) fn unknown_type(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0305", format!("cannot find type `{name}`"), span)
}

pub(super) fn wrong_literal(
    is_float: bool,
    expected: Type,
    span: Span,
    reason: Option<&Label>,
) -> Diagnostic {
    let kind = if is_float { "float" } else { "integer" };
    let d = Diagnostic::error(
        "E0401",
        format!("{kind} literal where {expected} expected"),
        span,
    )
    .with_primary_label(format!("expected {expected}"));
    with_reason(d, reason)
}

pub(super) fn out_of_range(ty: Type, span: Span) -> Diagnostic {
    let (max, neg) = ty.literal_range();
    let min = if neg == 0 {
        "0".to_string()
    } else {
        format!("-{neg}")
    };
    Diagnostic::error(
        "E0402",
        format!("integer literal out of range for {ty}"),
        span,
    )
    .with_help(format!("{ty} holds {min} to {max}"))
}

pub(super) fn mismatch(
    expected: Type,
    found: Type,
    span: Span,
    reason: Option<&Label>,
) -> Diagnostic {
    let d = Diagnostic::error("E0403", format!("expected {expected}, found {found}"), span)
        .with_primary_label(format!("expected {expected}"));
    with_reason(d, reason)
}

pub(super) fn branch_mismatch(span: Span, then: (Span, Type), else_: (Span, Type)) -> Diagnostic {
    Diagnostic::error("E0403", "`if` and `else` have different types", span)
        .with_label(then.0, format!("this is {}", then.1))
        .with_label(else_.0, format!("this is {}", else_.1))
}

pub(super) fn bad_operands(op: &str, types: &[Type], span: Span) -> Diagnostic {
    let list = types
        .iter()
        .map(Type::to_string)
        .collect::<Vec<_>>()
        .join(" and ");
    Diagnostic::error("E0404", format!("cannot apply `{op}` to {list}"), span)
}

pub(super) fn arity(name: &str, expected: usize, given: usize, span: Span) -> Diagnostic {
    let s = if expected == 1 { "" } else { "s" };
    let were = if given == 1 { "was" } else { "were" };
    let message = format!("`{name}` takes {expected} argument{s} but {given} {were} given");
    Diagnostic::error("E0405", message, span)
}

pub(super) fn not_function(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0406", format!("`{name}` is not a function"), span)
}

pub(super) fn not_callable(span: Span) -> Diagnostic {
    Diagnostic::error("E0406", "this expression is not a function", span)
}

pub(super) fn not_value(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0406",
        format!("`{name}` is a function, not a value"),
        span,
    )
}

pub(super) fn no_value(span: Span) -> Diagnostic {
    Diagnostic::error("E0407", "this expression has no value", span)
        .with_primary_label("its type is void")
}

fn with_reason(d: Diagnostic, reason: Option<&Label>) -> Diagnostic {
    match reason {
        Some(label) => d.with_label(label.span, label.message.clone()),
        None => d,
    }
}
