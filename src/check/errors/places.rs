//! E05xx and W01xx: places, mutability and control flow (spec §9).

use crate::check::Binding;
use crate::check::Type;
use crate::diagnostic::Diagnostic;
use crate::span::Span;

pub(in crate::check) fn string_immutable(span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0506",
        "cannot assign into a string: strings are immutable",
        span,
    )
    .with_help("build a new string instead, e.g. with `+`")
}

pub(in crate::check) fn assign_while_iterating(
    name: &str,
    span: Span,
    for_span: Span,
) -> Diagnostic {
    Diagnostic::error(
        "E0507",
        format!("cannot assign to `{name}` while iterating over it"),
        span,
    )
    .with_label(for_span, "iterated here")
}

pub(in crate::check) fn not_mutable(
    name: &str,
    span: Span,
    binding: Binding,
    declared: Span,
) -> Diagnostic {
    let help = match binding {
        Binding::Param => {
            format!("parameters are immutable; shadow it: `let mut {name} = {name};`")
        }
        Binding::LoopVar => "loop variables are immutable; copy it into a `let mut`".to_string(),
        Binding::Let { .. } => format!("make it mutable: `let mut {name}`"),
    };
    Diagnostic::error(
        "E0501",
        format!("cannot assign to `{name}`: it is not mutable"),
        span,
    )
    .with_label(declared, "declared here")
    .with_help(help)
}

pub(in crate::check) fn not_place(span: Span) -> Diagnostic {
    Diagnostic::error("E0502", "cannot assign to this expression", span)
        .with_help("only variables, fields and elements can be assigned")
}

pub(in crate::check) fn missing_return(
    name: &str,
    ty: Type,
    span: Span,
    has_loop: bool,
) -> Diagnostic {
    let help = if has_loop {
        "loops never count as returning (spec §6); add `panic(\"unreachable\");` after the loop"
    } else {
        "end the body with a value, or `return` on every path"
    };
    Diagnostic::error(
        "E0503",
        format!("`{name}` may end without returning a value of type {ty}"),
        span,
    )
    .with_help(help)
}

pub(in crate::check) fn with_stray_semicolon(d: Diagnostic, semicolon: Span) -> Diagnostic {
    d.with_label(semicolon, "remove this semicolon")
        .with_help("remove this semicolon to make it the result")
}

pub(in crate::check) fn outside_loop(keyword: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0504", format!("`{keyword}` outside of a loop"), span)
}

pub(in crate::check) fn discarded(what: &str, ty: Type, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0505",
        format!("this {what} has a value of type {ty} that is discarded"),
        span,
    )
    .with_help("add `;` to discard it, or make it the block's last expression")
}

pub(in crate::check) fn unreachable(span: Span, cause: Span) -> Diagnostic {
    Diagnostic::warning("W0101", "unreachable code", span)
        .with_label(cause, "any code after this never runs")
}
