//! Package paths (v8, PRP-058): whether a path exists through the loader's
//! `read`, lexical `.`/`..` folding, and E0320. Moved verbatim from
//! `modules/packages.rs` (PRP-080).

use std::io;
use std::path::{Component, Path, PathBuf};

use super::Read;
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0320 at `at`: `name` is a package and a local folder or file.
pub(super) fn clash(name: &str, at: Span) -> Diagnostic {
    Diagnostic::error(
        "E0320",
        format!("`{name}` is both a package and a local module"),
        at,
    )
    .with_help(format!(
        "rename the folder `{name}/` (or `{name}.la`) or the package in `lugha.toml`"
    ))
}

/// True if `path` exists. Reading a folder fails with an error other than
/// `NotFound`, as `std::fs::read` does, so one callback serves both.
pub(super) fn exists(read: &mut Read<'_>, path: &Path) -> bool {
    match read(path) {
        Ok(_) => true,
        Err(e) => e.kind() != io::ErrorKind::NotFound,
    }
}

/// `path` with `.` and `..` resolved lexically, as written; `.` when
/// nothing is left (v15: `example/..` was an empty path, a bug).
pub(crate) fn normalized(path: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    for part in path.components() {
        match part {
            Component::CurDir => {}
            Component::ParentDir
                if matches!(out.components().next_back(), Some(Component::Normal(_))) =>
            {
                out.pop();
            }
            other => out.push(other),
        }
    }
    if out.as_os_str().is_empty() {
        out.push(".");
    }
    out
}
