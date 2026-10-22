//! v17 codes: green threads (E0438, milestone 91); actors (E0439, milestone 92); struct update (E0440, W0104, milestone 93); variant values (E0412, milestone 94); the unit type (W0106, milestone 107); constants (E0445, E0446, milestone 109).

use crate::check::Type;
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// `spawn xs.push(1)`, `spawn p.bump()`: the new thread changes its own copy.
pub(in crate::check) fn spawn_changes_copy(span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0438",
        "`spawn` of a call that changes its receiver: the new thread would change its own copy",
        span,
    )
    .with_help(
        "nobody would see the change; send the new value back through a channel, or keep it in a `Ref`",
    )
}

/// A malformed `select`: the arms it may not have, or lack.
pub(in crate::check) fn bad_select(message: impl Into<String>, span: Span) -> Diagnostic {
    Diagnostic::error("E0438", message, span).with_help(
        "a `select` waits on `name = c.recv()` and `c.send(v)` arms, with at most one \
         `after(ms)` or one `default`",
    )
}

/// `Chan.new()` with nothing saying what it carries.
pub(in crate::check) fn chan_unknown(span: Span) -> Diagnostic {
    Diagnostic::error("E0412", "cannot infer what this channel carries", span)
        .with_help("annotate it: `let c: Chan<i64> = Chan.new();`")
}

/// An actor used against its rules (milestone 92).
pub(in crate::check) fn actor_misuse(message: impl Into<String>, span: Span) -> Diagnostic {
    Diagnostic::error("E0439", message, span).with_help(
        "an actor is started with `spawn` and reached only through its methods; \
         what crosses its boundary is copied and may hold no `Ref`",
    )
}

/// `..base` of another type than the literal's struct (milestone 93).
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

/// A generic variant used as a value with nothing fixing its type arguments (milestone 94).
pub(in crate::check) fn variant_value_infer(variant: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0412",
        format!("cannot infer the type arguments of `{variant}` used as a function"),
        span,
    )
    .with_help("say what it should be: `let f: fun(i64): Option<i64> = Some;`, or pass it where the type is known")
}

/// `let x = e` where `e` is `()` but not the literal (v17, milestone 107).
pub(in crate::check) fn unit_binding(name: &str, span: Span, init: Span) -> Diagnostic {
    Diagnostic::warning("W0106", format!("`{name}` holds `()`"), span)
        .with_label(init, "this has no other value than `()`")
        .with_help(format!(
            "drop the `let`, or write `let {name}: () = …` if that's what you meant"
        ))
}

/// A part of a constant's value that isn't a constant expression (milestone 109).
pub(in crate::check) fn not_constant(span: Span) -> Diagnostic {
    Diagnostic::error("E0445", "not a constant expression", span).with_help(
        "a constant's value is built from literals, other constants, operators, `as`, and array, struct, tuple and variant literals; no calls, closures, `if`, blocks or interpolation",
    )
}

/// A type a constant can't have (milestone 109).
pub(in crate::check) fn const_type(ty: &Type, span: Span) -> Diagnostic {
    Diagnostic::error("E0445", format!("a constant can't be of type `{ty}`"), span).with_help(
        "constants hold plain data: numbers, `bool`, `char`, strings, `()`, and arrays, structs, tuples and enums of those",
    )
}

/// Constants that depend on themselves (milestone 109).
pub(in crate::check) fn const_cycle(path: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0446", "this constant depends on itself", span)
        .with_help(format!("the values go round: {path}"))
}

/// Assigning to a constant (milestone 109).
pub(in crate::check) fn assign_const(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0501",
        format!("cannot assign to `{name}`: it is a constant"),
        span,
    )
    .with_help(format!("copy it first: `let mut x = {name};`"))
}

/// A path `include_bytes`/`include_str` can't include, and why (milestone 128).
pub(in crate::check) fn cant_include(path: &str, why: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0447", format!("can't include `{path}`: {why}"), span).with_help(
        "an included file is named by a string literal relative to this file, with `/`, and stays inside the package's folder",
    )
}

/// `include_bytes`/`include_str` without one string literal (milestone 128).
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

/// `include_str` of bytes that aren't UTF-8 (milestone 128).
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
