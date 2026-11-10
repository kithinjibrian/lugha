//! Diagnostics for constants (E0445, E0446, and E0501 for assigning to one) and for
//! `include_bytes` and `include_str` (E0447, E0448).

use crate::check::Type;
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// A constant whose value reaches outside the program:
/// `path` says how, "`load` calls `read_file`, which reads files".
pub(in crate::check) fn impure(path: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0445", "a constant can only use pure code", span).with_help(format!(
        "{path}; a constant is computed before `main` and may not print, read or write files, use the operating system, call C, or start tasks, channels or actors: compute the value in `main` instead"
    ))
}

/// A type a constant can't have.
pub(in crate::check) fn const_type(ty: &Type, span: Span) -> Diagnostic {
    Diagnostic::error("E0445", format!("a constant can't be of type `{ty}`"), span).with_help(
        "constants hold plain data: numbers, `bool`, `char`, strings, `()`, and arrays, structs, tuples and enums of those",
    )
}

/// Constants that depend on themselves.
pub(in crate::check) fn const_cycle(path: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0446", "this constant depends on itself", span)
        .with_help(format!("the values go round: {path}"))
}

/// Assigning to a constant.
pub(in crate::check) fn assign_const(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0501",
        format!("cannot assign to `{name}`: it is a constant"),
        span,
    )
    .with_help(format!("copy it first: `let mut x = {name};`"))
}

/// A path `include_bytes`/`include_str` can't include, and why.
pub(in crate::check) fn cant_include(path: &str, why: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0447", format!("can't include `{path}`: {why}"), span).with_help(
        "an included file is named by a string literal relative to this file, with `/`, and stays inside the package's folder",
    )
}

/// `include_bytes`/`include_str` without one string literal.
pub(in crate::check) fn include_literal(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0447",
        format!("`{name}` expects one string literal path"),
        span,
    )
    .with_help(format!(
        "write the path itself: `{name}(\"data/file.bin\")`"
    ))
}

/// `include_str` of bytes that aren't UTF-8.
pub(in crate::check) fn include_not_utf8(
    path: &str,
    at: usize,
    byte: u8,
    span: Span,
) -> Diagnostic {
    Diagnostic::error(
        "E0448",
        format!("`{path}` is not UTF-8: byte {at} is 0x{byte:02X}"),
        span,
    )
    .with_help("`include_bytes` takes any bytes; `string_from_utf8` can check them at run time")
}
