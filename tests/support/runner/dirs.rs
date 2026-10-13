//! Multi-file cases (PRP-033): a directory `d/` with expectations beside it
//! (`d.stdout`, `d.exit`, `d.stderr`) is one case, built from `d/main.la`;
//! the other `.la` files in it are its modules, not cases.

use std::path::{Path, PathBuf};

use super::Malformed;

/// The directories under `root` that are cases, from the walked `files`.
pub fn case_dirs(root: &Path, files: &[PathBuf]) -> Vec<PathBuf> {
    files
        .iter()
        .filter(|rel| {
            let ext = rel.extension().and_then(|e| e.to_str());
            matches!(ext, Some("stdout" | "stderr" | "exit"))
        })
        .map(|rel| rel.with_extension(""))
        .filter(|stem| root.join(stem).is_dir())
        .collect()
}

/// The case directory `dir`'s `main.la`, relative to `root`.
///
/// # Errors
///
/// A [`Malformed`] entry if the directory has no `main.la`.
pub fn main_of(root: &Path, dir: &Path) -> Result<PathBuf, Malformed> {
    let main = dir.join("main.la");
    if root.join(&main).is_file() {
        return Ok(main);
    }
    Err(Malformed {
        name: format!("{}/", dir.to_string_lossy()),
        reason: "a case directory needs a main.la".into(),
    })
}
