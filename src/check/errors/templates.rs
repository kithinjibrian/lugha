//! Diagnostics for format specifiers (E0431) and `format` templates (E0432).

use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0431: a format specifier that is malformed, or doesn't apply to its
/// value; `why` says which.
pub(in crate::check) fn bad_format(why: String, span: Span) -> Diagnostic {
    Diagnostic::error("E0431", why, span)
        .with_help("a specifier is `[[fill]align][#][0][width][.precision][kind]`, e.g. `{x:>8.2}`")
}

/// E0432: a `format` template that isn't a literal, is malformed, or has
/// a different number of holes than values.
pub(in crate::check) fn bad_template(why: String, span: Span) -> Diagnostic {
    Diagnostic::error("E0432", why, span)
        .with_help("write `format(\"{} and {:>5}\", a, b)`: one `{}` or `{:spec}` per value")
}
