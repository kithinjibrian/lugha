//! v16 codes: tests (E0434, milestone 84).

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
