//! Compiling a build's `[link]` C files (v16, milestone 87, PRP-117): each
//! on its own with `cc -c`, its package's flags only, into the build's
//! private temp folder. Never in the runtime's compile, so a package's
//! `-D` can't change `lugha_rt.c`. Objects come from the object cache when
//! it has a fresh one (v17, milestone 127, `object_cache.rs`).

use std::io;
use std::path::{Path, PathBuf};

use super::LinkError;
use super::compiler::Compiler;
use super::inputs::CFile;
use super::object_cache::Cache;

/// Each of `files` as an object, in order: the cache's, or compiled into `temp`.
///
/// # Errors
///
/// [`LinkError::CcNotFound`] or [`LinkError::Io`] if `cc` can't be
/// started; [`LinkError::CFile`] with `cc`'s message for the first file
/// that doesn't compile.
pub(super) fn compile(
    files: &[CFile],
    temp: &Path,
    cache: &Cache,
) -> Result<Vec<PathBuf>, LinkError> {
    let mut objects = Vec::new();
    for (n, c) in files.iter().enumerate() {
        // Numbered, not named after the source: two packages may both have `shim.c`.
        let object = temp.join(format!("c{n}.o"));
        // An unreadable file isn't cached: `cc` then says what is wrong with it.
        let found = match std::fs::read(&c.file) {
            Ok(source) => cache.object("c", &source, &c.flags, &object, |extra| {
                one(cache.cc(), c, extra, &object)
            }),
            Err(_) => one(cache.cc(), c, &[], &object).map(|()| object.clone()),
        }?;
        objects.push(found);
    }
    Ok(objects)
}

/// `cc -c` of `c` into `object`, with `extra` arguments (the cache's
/// dependency file), `cc` being the build's compiler.
fn one(
    cc: &Compiler,
    c: &CFile,
    extra: &[std::ffi::OsString],
    object: &Path,
) -> Result<(), LinkError> {
    let result = cc
        .command()
        .arg("-c")
        .arg(&c.file)
        .args(&c.flags)
        .args(extra)
        .arg("-o")
        .arg(object)
        .output();
    let out = match result {
        Ok(out) => out,
        Err(e) if e.kind() == io::ErrorKind::NotFound => {
            return Err(LinkError::CcNotFound(cc.name()));
        }
        Err(e) => return Err(LinkError::Io(e)),
    };
    if !out.status.success() {
        return Err(LinkError::CFile {
            span: c.span,
            file: c.file.clone(),
            stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
        });
    }
    Ok(())
}
