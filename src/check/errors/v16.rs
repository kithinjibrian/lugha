//! v16 codes: tests (E0434, milestone 84); callbacks (E0435, E0436, milestone 88).

use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// A `@test` function with parameters, type parameters or a return type.
pub(in crate::check) fn bad_test(span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0434",
        "a test takes no parameters and returns nothing",
        span,
    )
    .with_help("write `@test fun name() { … }`, and check results with `assert` or `assert_eq`")
}

/// A test called or used as a value.
pub(in crate::check) fn test_used(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0434",
        format!("`{name}` is a test: only `lughac test` runs it"),
        span,
    )
    .with_help("move the shared code into an ordinary function and call that")
}

/// A malformed callback declaration in an extern (v16, milestone 88).
pub(in crate::check) fn callback_decl(message: impl Into<String>, span: Span) -> Diagnostic {
    Diagnostic::error("E0435", message, span).with_help(
        "write the callback as `cb: fun(T, data)` and its user data as a parameter `user: data`; \
         `release` is the function C calls when it drops the callback",
    )
}

/// `data` or `release` written where no extern signature gives them a meaning.
pub(in crate::check) fn marker_outside(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0435",
        format!("`{name}` only means something in an extern's signature"),
        span,
    )
    .with_help("`data` and `release` mark the parameters C uses for a callback's user data")
}

/// A function value with captures where C keeps no user data.
pub(in crate::check) fn callback_captures(span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0436",
        "C can't keep this function's captures: this callback has no `data`",
        span,
    )
    .with_help(
        "pass a named function, or a closure that captures nothing; a callback declared with `data` takes any function",
    )
}

/// A type that can't go into or out of a callback.
pub(in crate::check) fn callback_type(span: Span) -> Diagnostic {
    Diagnostic::error("E0409", "this type can't cross into a callback", span).with_help(
        "a callback's parameters and result are numbers, `bool`, handles (`extern type`) or `Option` of a handle; \
         its parameters may also be `string` or `Option<string>`, copied from C, but it can't return one: \
         C would keep a pointer into Lugha's memory",
    )
}

/// `extern "symbol"` with a symbol that isn't a usable C name.
pub(in crate::check) fn bad_symbol(symbol: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0306",
        format!("`{symbol}` is not a C function name lughac can call"),
        span,
    )
    .with_help("use letters, digits and `_`, not starting with a digit; names starting with `lugha_` are reserved")
}
