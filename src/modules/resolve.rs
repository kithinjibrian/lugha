//! Where an import points, and the import errors: E0312, E0314, E0319.

use std::path::Path;

use super::{import_path_span, names};
use crate::ast::{Import, Item, Program};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0319 for importing the main file (`main`; `None` when it is loaded as
/// a package's module, v17), or E0314 for a clash with an earlier
/// import or an item of the same file; `None` if `import` is fine.
pub(super) fn check(
    import: &Import,
    program: &Program,
    main: Option<&Path>,
    target: &Path,
) -> Option<Diagnostic> {
    let at = import_path_span(import);
    if main.is_some_and(|main| same_file(target, main)) {
        return Some(
            Diagnostic::error("E0319", "the main file can't be imported", at)
                .with_help("move what both files need into a module of its own"),
        );
    }
    // The name the file binds: the alias, or the last segment (v14).
    let last = &import.bound().name;
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
        if &other.bound().name == last {
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
pub(super) fn not_found(
    path: &[String],
    file: &Path,
    at: Span,
    beside: Option<&[String]>,
) -> Diagnostic {
    let mut help = format!("there is no file `{}`", file.display());
    // `import test.support;` in `test/uses.la` means `test/test/support.la`;
    // the neighbour is `import support;` (kivuli's report, session 237).
    if let Some(rest) = beside {
        help.push_str(&format!(
            "; `{}.la` is beside this file: `import {};`",
            rest.join("/"),
            rest.join(".")
        ));
    }
    Diagnostic::error(
        "E0312",
        format!("module `{}` not found", path.join(".")),
        at,
    )
    .with_help(help)
}

/// The file module path `rest` names under `root`: `a/b.la` for `a.b`.
pub(super) fn file_of(root: &Path, rest: &[String]) -> std::path::PathBuf {
    let mut file = root.join(rest.join("/"));
    file.set_extension("la");
    file
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
