//! Git packages at the edge (v14 draft §6, milestone 50): the fetcher each
//! command loads with, writing `lugha.lock`, and `lughac update`.

use std::io;
use std::path::{Path, PathBuf};

use super::pipeline::{self, Failure};
use super::render::Report;
use super::source::Source;
use crate::diagnostic::Severity;
use crate::modules::fetch::{Fetcher, Mode};
use crate::modules::git::SystemGit;
use crate::modules::layout::LoadEnv;
use crate::modules::{cache, lock::LOCK};

/// What a command loads with: the system `git`, the user's cache, and
/// the home and current folders that bound the manifest lookup (v15).
pub(super) fn env(mode: Mode) -> LoadEnv {
    let root = cache::root(|name| std::env::var_os(name));
    // Without a current folder the lookup can't go up: `/` stops it at once.
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("/"));
    let home = std::env::var_os("HOME")
        .map(PathBuf::from)
        .filter(|h| h.is_absolute());
    LoadEnv {
        fetcher: Fetcher::new(Box::new(SystemGit), root, mode),
        home,
        cwd,
    }
}

/// Writes `text` as the lock next to `main`: a temporary file, then a
/// rename, so a reader never sees half a lock.
pub(super) fn write_lock(main: &Path, text: &str, source: &Source) -> Result<(), Failure> {
    let folder = main.parent().unwrap_or(Path::new(""));
    let (file, temp) = (folder.join(LOCK), folder.join(format!("{LOCK}.tmp")));
    let written: io::Result<()> =
        std::fs::write(&temp, text).and_then(|()| std::fs::rename(&temp, &file));
    written.map_err(|error| {
        let message = format!("cannot write `{}`: {error}", file.display());
        Failure::Internal(source.clone(), Box::new(Report::internal(message, None)))
    })
}

/// `lughac update`: every git package re-resolved to its newest matching
/// tag, and the lock rewritten.
pub(super) fn update(file: &Path) -> Result<(Source, Vec<crate::diagnostic::Diagnostic>), Failure> {
    let mode = Mode {
        offline: false,
        update: true,
    };
    let (source, loaded) = pipeline::loaded(file, mode)?;
    if loaded
        .diagnostics
        .iter()
        .any(|d| d.severity == Severity::Error)
    {
        return Err(Failure::Program(source, loaded.diagnostics));
    }
    Ok((source, loaded.diagnostics))
}
