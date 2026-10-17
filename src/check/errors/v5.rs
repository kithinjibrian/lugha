//! Codes new in v5 (milestones 20–22): E0425 for interface values, and the
//! E0403 help for converting between interfaces.

use crate::check::Type;
use crate::diagnostic::{Diagnostic, Label};
use crate::span::Span;

/// E0425: an interface with an associated function written as a type.
pub(in crate::check) fn interface_not_value(name: &str, item: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0425",
        format!("`{name}` can't be a type: it declares the associated function `{item}`"),
        span,
    )
    .with_help(format!(
        "`{item}` has no `self`, so a `{name}` value couldn't call it; use it as a bound: `<T: {name}>`"
    ))
}

/// E0403: an interface value where another interface is expected.
pub(in crate::check) fn interface_to_interface(
    expected: Type,
    found: Type,
    span: Span,
    reason: Option<&Label>,
) -> Diagnostic {
    super::mismatch(expected, found, span, reason)
        .with_help("a value of one interface type doesn't convert to another interface")
}

/// E0410, with help when the value is an interface value: its fields are
/// reached through a type pattern (v5 draft §4).
pub(in crate::check) fn no_field_on(name: &str, ty: Type, span: Span) -> Diagnostic {
    let interface = matches!(ty, Type::Interface(..));
    let d = super::no_field(name, ty, span);
    match interface {
        true => d.with_help(
            "an interface value has only its methods; use a type pattern to reach the value inside",
        ),
        false => d,
    }
}

/// E0404 for an operator, with help when an operand is an interface value,
/// which has no `==` (v5 draft §4).
pub(in crate::check) fn bad_operands_on(op: &str, types: &[Type], span: Span) -> Diagnostic {
    let d = super::bad_operands(op, types, span);
    match types.iter().any(|t| matches!(t, Type::Interface(..))) {
        true => d.with_help("interface values have no `==`; match on the concrete types first"),
        false => d,
    }
}

/// E0409 for a function type or an interface type in an extern signature:
/// both hold Lugha code that C can't call.
pub(in crate::check) fn extern_value(ty: &Type, span: Span) -> Diagnostic {
    match ty {
        Type::Interface(..) => {
            Diagnostic::error("E0409", "interface values can't cross the C boundary", span)
                .with_help("C can't call a Lugha method; pass the data it needs instead")
        }
        Type::Unit => Diagnostic::error("E0409", "`()` can't cross the C boundary", span)
            .with_help(
                "leave the parameter out; an extern without a result type returns C's `void`",
            ),
        _ => super::extern_fun(span),
    }
}

/// E0426: a type pattern that can't apply (v5 milestone 21).
pub(in crate::check) fn bad_type_pattern(
    message: String,
    span: Span,
    help: Option<String>,
) -> Diagnostic {
    let d = Diagnostic::error("E0426", message, span);
    match help {
        Some(help) => d.with_help(help),
        None => d,
    }
}
