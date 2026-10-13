//! Generics (v2 draft §4, PRP-035): bounds and parameter names (E0315,
//! E0316), type arguments (E0418, E0419), inference (E0412) and the
//! E0404 hint for a missing bound.

use crate::check::Type;
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0315: a bound other than `Eq` or `Ord` (v2 draft §4).
pub(in crate::check) fn unknown_bound(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0315", format!("unknown bound `{name}`"), span)
        .with_help("the bounds are `Eq` and `Ord`")
}

/// E0316: a type parameter named twice, or like a type or module.
pub(in crate::check) fn bad_type_param(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0316",
        format!("`{name}` can't name a type parameter here"),
        span,
    )
    .with_help(
        "type parameters need names of their own, not those of types, modules or other parameters",
    )
}

/// E0404 on a parameter without the bound the operator needs: say which.
pub(in crate::check) fn with_bound_help(
    d: Diagnostic,
    param: Option<String>,
    ord: bool,
) -> Diagnostic {
    match param {
        Some(p) => d.with_help(format!("declare `{p}: {}`", if ord { "Ord" } else { "Eq" })),
        None => d,
    }
}

/// E0418: a generic type with the wrong number of type arguments.
pub(in crate::check) fn type_args(
    name: &str,
    expected: usize,
    given: usize,
    span: Span,
) -> Diagnostic {
    Diagnostic::error(
        "E0418",
        format!(
            "`{name}` takes {expected} type argument{} but {given} {} given",
            if expected == 1 { "" } else { "s" },
            if given == 1 { "was" } else { "were" }
        ),
        span,
    )
}

/// E0419: a type argument that doesn't satisfy its parameter's bound.
pub(in crate::check) fn unmet_bound(ty: &Type, bound: &str, span: Span) -> Diagnostic {
    let help = if bound == "Ord" {
        "only numbers, strings and parameters with `Ord` are ordered"
    } else {
        "a type parameter needs `Eq` before `==` works on it"
    };
    Diagnostic::error("E0419", format!("`{ty}` doesn't satisfy `{bound}`"), span).with_help(help)
}

/// E0412 for a type parameter no argument or expected type decides.
pub(in crate::check) fn cannot_infer(param: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0412",
        format!("cannot infer the type argument `{param}`"),
        span,
    )
    .with_help("annotate the variable, e.g. `let s: Stack<i64> = …;`")
}

/// E0420: generic instances nested too deep.
pub(in crate::check) fn too_deep(chain: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0420", "generic instantiation too deep", span)
        .with_help(format!("{chain}: each call needs a deeper instance"))
}

/// E0420: a generic type whose instances keep growing.
pub(in crate::check) fn type_too_deep(ty: &Type, span: Span) -> Diagnostic {
    Diagnostic::error("E0420", format!("generic type `{ty}` nests too deep"), span)
        .with_help("a generic type may contain itself only with its own parameters unchanged")
}
