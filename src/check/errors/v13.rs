//! Codes new in v13 (milestones 41–44): E0431 for format specifiers.

use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0431: a format specifier that is malformed, or doesn't apply to its
/// value; `why` says which.
pub(in crate::check) fn bad_format(why: String, span: Span) -> Diagnostic {
    Diagnostic::error("E0431", why, span)
        .with_help("a specifier is `[[fill]align][#][0][width][.precision][kind]`, e.g. `{x:>8.2}`")
}
