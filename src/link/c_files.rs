//! Compiling a build's `[link]` C files (v16, milestone 87, PRP-117): each
//! on its own with `cc -c`, its package's flags only, into the build's
//! private temp folder. Never in the runtime's compile, so a package's
//! `-D` can't change `lugha_rt.c`.

use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;

use super::LinkError;
use super::inputs::CFile;

/// Compiles each of `files` to an object in `temp`, in order.
///
/// # Errors
///
/// [`LinkError::CcNotFound`] or [`LinkError::Io`] if `cc` can't be
/// started; [`LinkError::CFile`] with `cc`'s message for the first file
/// that doesn't compile.
pub fn compile(files: &[CFile], temp: &Path) -> Result<Vec<PathBuf>, LinkError> {
    let mut objects = Vec::new();
    for (n, c) in files.iter().enumerate() {
        // Numbered, not named after the source: two packages may both have `shim.c`.
        let object = temp.join(format!("c{n}.o"));
        let result = Command::new("cc")
            .arg("-c")
            .arg(&c.file)
            .args(&c.flags)
            .arg("-o")
            .arg(&object)
            .output();
        let out = match result {
            Ok(out) => out,
            Err(e) if e.kind() == io::ErrorKind::NotFound => return Err(LinkError::CcNotFound),
            Err(e) => return Err(LinkError::Io(e)),
        };
        if !out.status.success() {
            return Err(LinkError::CFile {
                span: c.span,
                file: c.file.clone(),
                stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
            });
        }
        objects.push(object);
    }
    Ok(objects)
}
