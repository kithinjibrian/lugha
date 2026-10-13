//! Codes new in v4 (milestones 17–19): E0424 for `?`.

use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0424: `?` where it can't return its failure.
pub(in crate::check) fn cant_try(span: Span, help: &str) -> Diagnostic {
    Diagnostic::error("E0424", "`?` can't return here", span).with_help(help)
}
