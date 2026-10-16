//! v17 codes: green threads (E0438, milestone 91).

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
