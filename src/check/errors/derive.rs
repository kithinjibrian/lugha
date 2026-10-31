//! Derive blocks' diagnostics (v17, milestone 142, PRP-173): E0451 for a
//! type that doesn't meet a derivable interface, E0452 for a misplaced or
//! overlapping block, E0453 for a misused field intrinsic. A block's item
//! that doesn't match its interface is E0422, as for any type.

use crate::check::Type;
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0451: `ty` doesn't meet derivable interface `interface`; `path` leads
/// to the part at fault (empty for `ty` itself), `what` says why, and
/// `covered` lists the types the interface has blocks for.
pub(in crate::check) fn not_derived(
    ty: &Type,
    interface: &str,
    (path, what): (&str, &str),
    covered: &[String],
    span: Span,
) -> Diagnostic {
    let place = if path.is_empty() {
        "it is".to_string()
    } else {
        format!("`{}` is", path.trim_start_matches('.'))
    };
    Diagnostic::error(
        "E0451",
        format!("`{ty}` doesn't meet `{interface}`: {place} {what}"),
        span,
    )
    .with_help(format!("`{interface}` has blocks for {}", list(covered)))
}

/// `a`, `a and b`, `a, b and c`.
fn list(items: &[String]) -> String {
    match items {
        [] => "nothing".to_string(),
        [one] => one.clone(),
        [rest @ .., last] => format!("{} and {last}", rest.join(", ")),
    }
}

/// E0452: a block for an interface this module doesn't declare.
pub(in crate::check) fn derive_elsewhere(name: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0452",
        format!("a `derive` block for `{name}` must be in the module that declares `{name}`"),
        span,
    )
    .with_help("only an interface's own module says how types meet it; a type's own methods can replace an item instead")
}

/// E0452: two blocks of `interface` that both cover `ty`.
pub(in crate::check) fn derive_overlap(interface: &str, ty: &str, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0452",
        format!("two `derive {interface}` blocks cover `{ty}`"),
        span,
    )
    .with_help("each type gets its items from one block: remove or narrow one of them")
}

/// E0422: a block whose items don't match its interface's; `problems` as
/// for any type that doesn't satisfy one.
pub(in crate::check) fn block_unsatisfied(
    block: &str,
    interface: &str,
    problems: &[String],
    span: Span,
) -> Diagnostic {
    Diagnostic::error(
        "E0422",
        format!("`{block}` doesn't match `{interface}`"),
        span,
    )
    .with_help(problems.join("; "))
}

/// E0453: a field intrinsic used wrongly; `why` says how.
pub(in crate::check) fn bad_intrinsic(op: &str, why: &str, span: Span) -> Diagnostic {
    Diagnostic::error("E0453", format!("`derive.{op}` {why}"), span).with_help(
        "`derive.fields` and `derive.build` work in `derive … for struct` and `for tuple` blocks; `derive.variant`, `variants` and `build_variant` in `for enum` blocks; `f` names a function of this module generic over one `F` with the block's interface",
    )
}

/// E0453: a local named `derive` inside a derive block, where `derive.` means the intrinsics.
pub(in crate::check) fn derive_local(span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0453",
        "a local can't be named `derive` inside a derive block",
        span,
    )
    .with_help("inside a block `derive.` means the field intrinsics: rename the local")
}
