//! `lughac fmt [paths…] [--check]` (v16 draft decisions 60–63, milestone
//! 80): formats files in place, or with `--check` lists the ones that
//! would change.
//!
//! A folder means every `.la` file under it, in name order; hidden folders
//! and `target/` are skipped and symbolic links inside a folder are never
//! followed. A file is written only when its text changes, through a
//! temporary file in the same folder and a rename, so a crash can't leave
//! half a file.

use std::io;
use std::path::{Path, PathBuf};

use thiserror::Error;

use super::Output;
use super::render::Report;
use super::source::Source;
use crate::formatter::{self, FormatError};

/// Why a path couldn't be formatted, beyond its own syntax errors.
#[derive(Debug, Error)]
enum FmtError {
    #[error("cannot read `{path}`: {source}")]
    Read { path: String, source: io::Error },
    #[error("cannot write `{path}`: {source}")]
    Write { path: String, source: io::Error },
}

/// Runs `lughac fmt`; returns the exit code: 0, 1 when `--check` found a
/// file to change or a file has syntax errors, 2 when a file couldn't be
/// read or written or the formatter failed its own verification.
pub(super) fn fmt(paths: &[PathBuf], check: bool, out: &mut Output) -> u8 {
    let here = [PathBuf::from(".")];
    let paths = if paths.is_empty() { &here[..] } else { paths };
    let mut code = 0;
    for path in paths {
        let mut files = Vec::new();
        let listed = match path.is_dir() {
            true => la_files(path, &mut files).map_err(|source| FmtError::Read {
                path: path.display().to_string(),
                source,
            }),
            false => {
                files.push(path.clone());
                Ok(())
            }
        };
        if let Err(error) = listed {
            out.error(&error.to_string());
            code = 2;
        }
        for file in files {
            code = code.max(one(&file, check, out));
        }
    }
    code
}

/// Formats one file; its exit code.
fn one(file: &Path, check: bool, out: &mut Output) -> u8 {
    let name = display(file);
    let src = match std::fs::read_to_string(file) {
        Ok(src) => src,
        Err(source) => {
            out.error(&FmtError::Read { path: name, source }.to_string());
            return 2;
        }
    };
    let formatted = match formatter::format(&src) {
        Ok(text) => text,
        Err(FormatError::Syntax(diagnostics)) => {
            out.diagnostics(&Source::single(name, src), &diagnostics);
            return 1;
        }
        Err(FormatError::Unsafe(reason)) => {
            let message = format!(
                "internal error: formatting `{name}` failed verification ({reason}); the file was not changed"
            );
            out.report(&Source::single(name, src), &Report::internal(message, None));
            return 2;
        }
    };
    if formatted == src {
        return 0;
    }
    if check {
        out.print(&format!("would reformat {name}\n"));
        return 1;
    }
    match replace(file, &formatted) {
        Ok(()) => 0,
        Err(source) => {
            out.error(&FmtError::Write { path: name, source }.to_string());
            2
        }
    }
}

/// Writes `text` to a temporary file beside `file`, with its permissions,
/// then renames it over `file`.
fn replace(file: &Path, text: &str) -> io::Result<()> {
    let folder = file.parent().unwrap_or(Path::new(""));
    let base = file
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    let temp = folder.join(format!(".{base}.lughac-fmt.tmp"));
    let permissions = std::fs::metadata(file)?.permissions();
    let written = std::fs::write(&temp, text)
        .and_then(|()| std::fs::set_permissions(&temp, permissions))
        .and_then(|()| std::fs::rename(&temp, file));
    if written.is_err() {
        // Best effort: the original is untouched; a leftover temp file is the only harm.
        let _ = std::fs::remove_file(&temp);
    }
    written
}

/// The `.la` files under `dir`, sorted, recursing into folders that are
/// neither hidden nor `target`, never through a symbolic link.
pub(super) fn la_files(dir: &Path, out: &mut Vec<PathBuf>) -> io::Result<()> {
    let mut entries = std::fs::read_dir(dir)?
        .map(|e| e.map(|e| e.path()))
        .collect::<io::Result<Vec<_>>>()?;
    entries.sort();
    for path in entries {
        let kind = std::fs::symlink_metadata(&path)?.file_type();
        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_default();
        if kind.is_dir() && !name.starts_with('.') && name != "target" {
            la_files(&path, out)?;
        } else if kind.is_file() && path.extension().is_some_and(|e| e == "la") {
            out.push(path);
        }
    }
    Ok(())
}

/// A path as the user would write it: without a leading `./`.
pub(super) fn display(path: &Path) -> String {
    path.strip_prefix(".").unwrap_or(path).display().to_string()
}
