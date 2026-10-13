//! Where an import points, and the import errors: E0312, E0314, E0319.

use std::path::Path;

use super::{import_path_span, names};
use crate::ast::{Import, Item, Program};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0319 for importing the main file, or E0314 for a clash with an earlier
/// import or an item of the same file; `None` if `import` is fine.
pub(super) fn check(
    import: &Import,
    program: &Program,
    main: &Path,
    target: &Path,
) -> Option<Diagnostic> {
    let at = import_path_span(import);
    if same_file(target, main) {
        return Some(
            Diagnostic::error("E0319", "the main file can't be imported", at)
                .with_help("move what both files need into a module of its own"),
        );
    }
    let last = &import.path.last().expect("an import has a path").name;
    let earlier = program
        .imports
        .iter()
        .take_while(|other| !std::ptr::eq(*other, import));
    for other in earlier {
        if names(other) == names(import) {
            return Some(clash(
                at,
                format!("`{}` is imported twice", names(import).join(".")),
                other,
            ));
        }
        if other.path.last().is_some_and(|s| &s.name == last) {
            return Some(clash(
                at,
                format!("two imports are both named `{last}`"),
                other,
            ));
        }
    }
    let item = program.items.iter().find_map(|item| match item {
        Item::Fun(f) if &f.name.name == last => Some(f.name.span),
        Item::Extern(e) if &e.name.name == last => Some(e.name.span),
        Item::Struct(s) if &s.name.name == last => Some(s.name.span),
        Item::Enum(e) if &e.name.name == last => Some(e.name.span),
        _ => None,
    })?;
    Some(
        Diagnostic::error(
            "E0314",
            format!("the import `{last}` clashes with an item"),
            at,
        )
        .with_label(item, format!("`{last}` is also declared here")),
    )
}

/// E0312 at the import that asked for a missing module.
pub(super) fn not_found(path: &[String], file: &Path, at: Span) -> Diagnostic {
    Diagnostic::error(
        "E0312",
        format!("module `{}` not found", path.join(".")),
        at,
    )
    .with_help(format!("there is no file `{}`", file.display()))
}

fn clash(at: Span, message: String, earlier: &Import) -> Diagnostic {
    Diagnostic::error("E0314", message, at)
        .with_label(import_path_span(earlier), "the earlier import")
}

/// Two paths name the same file, compared as written: imports are built
/// from the main file's own directory, so equal files have equal paths.
fn same_file(a: &Path, b: &Path) -> bool {
    a == b
}
