//! The compile pipeline behind each command: read → lex → parse → codegen → link.
//!
//! Every stage stops the pipeline on error (spec §9). Failures come back as
//! `Failure::Program` (exit 1) or `Failure::Internal` (exit 2), each with the
//! source needed to render them.

use std::io;
use std::os::unix::process::ExitStatusExt;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

use super::gate;
use super::render::Report;
use super::source::Source;
use crate::ast::Program;
use crate::check::{self, Checked};
use crate::codegen::{self, CodegenError, OptLevel};
use crate::diagnostic::{Diagnostic, Severity};
use crate::link;
use crate::modules::{self, Loaded, Module};

/// Why a command stopped.
pub(super) enum Failure {
    /// Errors in the user's program — exit 1.
    Program(Source, Vec<Diagnostic>),
    /// An unreadable file or a compiler limitation or bug — exit 2.
    /// The report is boxed: it is large and this path is rare.
    Internal(Source, Box<Report>),
}

/// A program whose modules all loaded and parsed, with any warnings.
pub(super) struct Front {
    pub source: Source,
    /// Every module, the main file first (PRP-033).
    pub modules: Vec<Module>,
    /// The main module. While imports are gated it is the whole program.
    pub program: Program,
    pub warnings: Vec<Diagnostic>,
    /// The checker's output; `None` for the parse-only front end (`--emit=ast`).
    pub checked: Option<Checked>,
}

/// Loads `path` and every module it imports. A file that can't be read is
/// internal; mistakes in the program come back as diagnostics.
pub(super) fn loaded(path: &Path) -> Result<(Source, Loaded), Failure> {
    let name = path.display().to_string();
    let mut read = |file: &Path| std::fs::read(file);
    match modules::load(path, &mut read) {
        Ok(loaded) => {
            let root = path.parent().unwrap_or(Path::new(""));
            Ok((Source::of(&name, root, &loaded.modules), loaded))
        }
        Err(error) => {
            let report = Report::internal(error.to_string(), None);
            Err(Failure::Internal(
                Source::single(name, ""),
                Box::new(report),
            ))
        }
    }
}

/// Loads, lexes and parses every module — everything `--emit=ast` needs.
pub(super) fn parsed(path: &Path) -> Result<Front, Failure> {
    let (source, loaded) = loaded(path)?;
    if loaded
        .diagnostics
        .iter()
        .any(|d| d.severity == Severity::Error)
    {
        return Err(Failure::Program(source, loaded.diagnostics));
    }
    let program = loaded.modules[0]
        .program
        .clone()
        .expect("a module without errors parsed");
    Ok(Front {
        source,
        modules: loaded.modules,
        program,
        warnings: loaded.diagnostics,
        checked: None,
    })
}

/// Reads, lexes, parses and type-checks `path` (spec §9 stages 1–3), stopping
/// at v2 constructs whose milestone hasn't landed (exit 2).
pub(super) fn front(path: &Path) -> Result<Front, Failure> {
    let mut front = parsed(path)?;
    // v2 syntax parses, but the checker only knows what has landed (PRP-030).
    let gated = front
        .modules
        .iter()
        .filter_map(|m| m.program.as_ref().and_then(gate::first))
        .next();
    if let Some(gated) = gated {
        let message = format!(
            "not implemented yet: {} (milestone {})",
            gated.what, gated.milestone
        );
        let report = Report::internal(message, Some(gated.span));
        return Err(Failure::Internal(front.source, Box::new(report)));
    }
    match check::check(&front.program) {
        Ok((checked, warnings)) => {
            front.warnings.extend(warnings);
            front.checked = Some(checked);
            Ok(front)
        }
        Err(errors) => Err(Failure::Program(front.source, errors)),
    }
}

/// The program's LLVM IR, for `--emit=ir`.
pub(super) fn ir(front: &Front) -> Result<String, Failure> {
    codegen::emit_ir(&front.program, checked(front), &source_info(front))
        .map_err(|e| codegen_failure(&front.source, e))
}

/// Compiles and links `front` into the executable `exe`.
pub(super) fn build(front: &Front, opt: OptLevel, exe: &Path) -> Result<(), Failure> {
    let temp = TempDir::new().map_err(|e| internal(&front.source, temp_error(e)))?;
    let object = temp.path.join("prog.o");
    codegen::emit_object(
        &front.program,
        checked(front),
        &source_info(front),
        opt,
        &object,
    )
    .map_err(|e| codegen_failure(&front.source, e))?;
    link::link(&[&object], exe).map_err(|e| internal(&front.source, e.to_string()))
}

/// Builds `front` into a temp dir and runs it with inherited standard streams.
///
/// Returns the program's exit code, or 128 + N if signal N killed it, as a shell reports it.
pub(super) fn run(front: &Front, opt: OptLevel) -> Result<u8, Failure> {
    let temp = TempDir::new().map_err(|e| internal(&front.source, temp_error(e)))?;
    let exe = temp.path.join("prog");
    build(front, opt, &exe)?;
    let status = Command::new(&exe).status().map_err(|e| {
        internal(
            &front.source,
            format!("cannot run the compiled program: {e}"),
        )
    })?;
    Ok(match (status.code(), status.signal()) {
        // Exit codes are already 0–255 on Unix; truncation is a no-op.
        (Some(code), _) => code as u8,
        (None, Some(signal)) => 128u8.wrapping_add(signal as u8),
        (None, None) => 1,
    })
}

/// Where `build` writes when there is no `-o`: the file name without its extension.
///
/// A file with no extension gets `.out`, so the source is never overwritten.
pub(super) fn default_output(file: &Path) -> PathBuf {
    let stem = PathBuf::from(file.file_stem().unwrap_or(file.as_os_str()));
    if file.extension().is_none() {
        stem.with_extension("out")
    } else {
        stem
    }
}

/// The source name and text codegen needs for panic locations.
fn source_info(front: &Front) -> codegen::SourceInfo<'_> {
    codegen::SourceInfo {
        name: &front.source.main().name,
        text: &front.source.main().text,
    }
}

/// The type table of a front end that ran the checker.
fn checked(front: &Front) -> &Checked {
    front
        .checked
        .as_ref()
        .expect("codegen runs only after `front()` type-checked")
}

fn codegen_failure(source: &Source, error: CodegenError) -> Failure {
    // Codegen errors are compiler or LLVM failures, with no source location.
    Failure::Internal(
        source.clone(),
        Box::new(Report::internal(error.to_string(), None)),
    )
}

fn internal(source: &Source, message: String) -> Failure {
    Failure::Internal(source.clone(), Box::new(Report::internal(message, None)))
}

fn temp_error(error: io::Error) -> String {
    format!("cannot create a temporary directory: {error}")
}

/// A private temporary directory, removed on drop.
struct TempDir {
    path: PathBuf,
}

impl TempDir {
    fn new() -> io::Result<Self> {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let path = std::env::temp_dir().join(format!("lughac-{}-{n}", std::process::id()));
        // `create_dir`, not `create_dir_all`: fail rather than reuse a directory someone planted.
        std::fs::create_dir(&path)?;
        Ok(TempDir { path })
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir is harmless and must not hide the real result.
        let _ = std::fs::remove_dir_all(&self.path);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_output_never_overwrites_the_source() {
        assert_eq!(
            default_output(Path::new("arith.la")),
            PathBuf::from("arith")
        );
        assert_eq!(
            default_output(Path::new("dir/prog.la")),
            PathBuf::from("prog")
        );
        assert_eq!(default_output(Path::new("prog")), PathBuf::from("prog.out"));
    }
}
