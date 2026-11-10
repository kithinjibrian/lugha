//! Diagnostics for leaving code early: `?` (E0424), `defer` (E0455, E0456) and loop labels
//! (E0512, E0513, E0514).

use crate::check::Type;
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0424: `?` where it can't return its failure.
pub(in crate::check) fn cant_try(span: Span, help: &str) -> Diagnostic {
    Diagnostic::error("E0424", "`?` can't return here", span).with_help(help)
}

/// `return`, `?`, `break` or `continue` leaving a deferred statement: it runs while its block is
/// already being left.
pub(in crate::check) fn leaves_defer(keyword: &str, span: Span, defer: Span) -> Diagnostic {
    Diagnostic::error(
        "E0455",
        format!("`{keyword}` can't leave a deferred statement"),
        span,
    )
    .with_label(defer, "deferred here")
    .with_help("it runs while its block is already being left; handle the case inside it instead")
}

/// A deferred statement with a value: nothing would see it.
pub(in crate::check) fn deferred_value(ty: Type, span: Span) -> Diagnostic {
    let help = if matches!(&ty, Type::Enum(k, _) if k == crate::check::RESULT) {
        "handle the error inside it, or discard it on purpose: `defer { fs.remove_file(tmp); }`"
    } else {
        "nothing would see its value: add `;` inside braces to discard it, `defer { e; }`"
    };
    Diagnostic::error(
        "E0456",
        format!("a deferred statement must have type (), found {ty}"),
        span,
    )
    .with_help(help)
}

/// `break label` or `continue label` with no enclosing loop of that name. `in_closure`: a loop
/// outside the closure has it.
pub(in crate::check) fn unknown_label(label: &crate::ast::Ident, in_closure: bool) -> Diagnostic {
    let d = Diagnostic::error(
        "E0512",
        format!("unknown label `{}`", label.name),
        label.span,
    );
    if in_closure {
        d.with_help("labels don't reach into closures: a closure's body is its own function")
    } else {
        d.with_help("a label names an enclosing loop: `outer: for … { break outer; }`")
    }
}

/// A loop labelled like a loop around it.
pub(in crate::check) fn label_in_use(label: &crate::ast::Ident, outer: Span) -> Diagnostic {
    Diagnostic::error(
        "E0513",
        format!(
            "label `{}` is already used by an enclosing loop",
            label.name
        ),
        label.span,
    )
    .with_label(outer, "the enclosing loop's label")
    .with_help("give the inner loop another name")
}

/// `break value` out of a `while` or `for` loop.
pub(in crate::check) fn break_value_outside_loop(span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0514",
        "`break` with a value out of a `while` or `for` loop",
        span,
    )
    .with_help("only `loop` gives a value; a `while` or `for` can end without a `break`")
}
