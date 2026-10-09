//! E04xx: types (spec §9).

use super::with_reason;
use crate::check::Type;
use crate::diagnostic::{Diagnostic, Label};
use crate::span::Span;

pub(in crate::check) fn extern_string_return(span: Span) -> Diagnostic {
    Diagnostic::error("E0409", "an extern function can't return `string`", span)
        .with_help("C strings have no length header; return a number and use it from Lugha")
}

pub(in crate::check) fn wrong_literal(
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

pub(in crate::check) fn out_of_range(ty: Type, span: Span) -> Diagnostic {
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

pub(in crate::check) fn mismatch(
    expected: Type,
    found: Type,
    span: Span,
    reason: Option<&Label>,
) -> Diagnostic {
    let d = Diagnostic::error("E0403", format!("expected {expected}, found {found}"), span)
        .with_primary_label(format!("expected {expected}"));
    with_reason(d, reason)
}

pub(in crate::check) fn branch_mismatch(
    span: Span,
    then: (Span, Type),
    else_: (Span, Type),
) -> Diagnostic {
    Diagnostic::error("E0403", "`if` and `else` have different types", span)
        .with_label(then.0, format!("this is {}", then.1))
        .with_label(else_.0, format!("this is {}", else_.1))
}

pub(in crate::check) fn bad_operands(op: &str, types: &[Type], span: Span) -> Diagnostic {
    let list = types
        .iter()
        .map(Type::to_string)
        .collect::<Vec<_>>()
        .join(" and ");
    Diagnostic::error("E0404", format!("cannot apply `{op}` to {list}"), span)
}

pub(in crate::check) fn arity(name: &str, expected: usize, given: usize, span: Span) -> Diagnostic {
    let s = if expected == 1 { "" } else { "s" };
    let were = if given == 1 { "was" } else { "were" };
    let message = format!("`{name}` takes {expected} argument{s} but {given} {were} given");
    Diagnostic::error("E0405", message, span)
}

pub(in crate::check) fn not_function(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0406", format!("`{name}` is not a function"), span)
}

pub(in crate::check) fn intrinsic_argument(name: &str, found: Type, span: Span) -> Diagnostic {
    let expected = match name {
        "print" | "println" => "a number, bool or string",
        "to_string" => "a number or bool",
        _ => "a string",
    };
    Diagnostic::error(
        "E0403",
        format!("`{name}` expects {expected}, found {found}"),
        span,
    )
}

pub(in crate::check) fn no_field(name: &str, ty: Type, span: Span) -> Diagnostic {
    Diagnostic::error("E0410", format!("no field `{name}` on `{ty}`"), span)
}

pub(in crate::check) fn not_indexable(ty: Type, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0411",
        format!("cannot index into a value of type `{ty}`"),
        span,
    )
}

pub(in crate::check) fn empty_array(span: Span) -> Diagnostic {
    Diagnostic::error("E0412", "cannot infer the element type of `[]`", span)
        .with_help("annotate it: `let xs: i64[] = [];`")
}

pub(in crate::check) fn not_iterable(ty: &Type, span: Span) -> Diagnostic {
    Diagnostic::error("E0411", format!("cannot iterate over `{ty}`"), span).with_help(
        "`for … of` walks arrays; use `for i in 0..s.len` with `s[i]` for a string's bytes",
    )
}

pub(in crate::check) fn extern_array(span: Span) -> Diagnostic {
    Diagnostic::error("E0409", "arrays can't cross the C boundary", span)
        .with_help("pass a number or a string instead")
}

pub(in crate::check) fn not_callable(span: Span) -> Diagnostic {
    Diagnostic::error("E0406", "this expression is not a function", span)
}

pub(in crate::check) fn not_value(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0406",
        format!("`{name}` is a function, not a value"),
        span,
    )
}

pub(in crate::check) fn no_value(span: Span) -> Diagnostic {
    Diagnostic::error("E0407", "this expression has no value", span)
        .with_primary_label("its type is void")
}

pub(in crate::check) fn bad_cast(from: Type, to: Type, span: Span) -> Diagnostic {
    let d = Diagnostic::error("E0408", format!("cannot cast `{from}` to `{to}`"), span);
    match (from, to) {
        (Type::Bool, _) => d.with_help("write `if b { 1 } else { 0 }`"),
        (_, Type::Bool) => d.with_help("compare instead, e.g. `x != 0`"),
        _ => d,
    }
}

pub(in crate::check) fn extern_struct(span: Span) -> Diagnostic {
    Diagnostic::error("E0409", "structs can't cross the C boundary", span)
        .with_help("pass the fields one by one instead")
}

pub(in crate::check) fn missing_fields(missing: &[&str], name: &str, span: Span) -> Diagnostic {
    let s = if missing.len() == 1 { "" } else { "s" };
    let list = missing
        .iter()
        .map(|f| format!("`{f}`"))
        .collect::<Vec<_>>()
        .join(", ");
    Diagnostic::error(
        "E0413",
        format!("missing field{s} {list} in `{name}`"),
        span,
    )
}

pub(in crate::check) fn field_given_twice(field: &str, span: Span, first: Span) -> Diagnostic {
    Diagnostic::error("E0414", format!("field `{field}` is given twice"), span)
        .with_label(first, "first given here")
}
