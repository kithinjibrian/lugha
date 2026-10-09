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

use super::render::Report;
use super::source::{self, LoadError, Source};
use crate::ast::Program;
use crate::check::{self, CheckError, Checked};
use crate::codegen::{self, CodegenError, OptLevel};
use crate::diagnostic::Diagnostic;
use crate::lexer::{self, Token};
use crate::{link, parser};

/// Why a command stopped.
pub(super) enum Failure {
    /// Errors in the user's program — exit 1.
    Program(Source, Vec<Diagnostic>),
    /// An unreadable file or a compiler limitation or bug — exit 2.
    /// The report is boxed: it is large and this path is rare.
    Internal(Source, Box<Report>),
}

/// A program that lexed and parsed, with any warnings found on the way.
pub(super) struct Front {
    pub source: Source,
    pub program: Program,
    pub warnings: Vec<Diagnostic>,
    /// The checker's output; `None` for the parse-only front end (`--emit=ast`).
    pub checked: Option<Checked>,
}

/// Reads `path`; an unreadable file is internal, invalid UTF-8 is E0110.
pub(super) fn load(path: &Path) -> Result<Source, Failure> {
    source::load(path).map_err(|error| match error {
        LoadError::Io(e) => {
            let name = path.display().to_string();
            let report = Report::internal(format!("cannot read `{name}`: {e}"), None);
            Failure::Internal(
                Source {
                    name,
                    text: String::new(),
                },
                Box::new(report),
            )
        }
        LoadError::Utf8(source, diagnostic) => Failure::Program(source, vec![*diagnostic]),
    })
}

/// Lexes `source`, returning tokens and warnings.
pub(super) fn tokens(source: &Source) -> Result<(Vec<Token>, Vec<Diagnostic>), Failure> {
    lexer::lex(&source.text).map_err(|errors| Failure::Program(source.clone(), errors))
}

/// Reads, lexes and parses `path` — everything `--emit=ast` needs.
pub(super) fn parsed(path: &Path) -> Result<Front, Failure> {
    let source = load(path)?;
    let (tokens, mut warnings) = tokens(&source)?;
    match parser::parse(&tokens) {
        Ok((program, parse_warnings)) => {
            warnings.extend(parse_warnings);
            Ok(Front {
                source,
                program,
                warnings,
                checked: None,
            })
        }
        Err(errors) => Err(Failure::Program(source, errors)),
    }
}

/// Reads, lexes, parses and type-checks `path` (spec §9 stages 1–3).
pub(super) fn front(path: &Path) -> Result<Front, Failure> {
    let mut front = parsed(path)?;
    match check::check(&front.program) {
        Ok((checked, warnings)) => {
            front.warnings.extend(warnings);
            front.checked = Some(checked);
            Ok(front)
        }
        Err(CheckError::Program(errors)) => Err(Failure::Program(front.source, errors)),
        Err(CheckError::Unsupported {
            what,
            milestone,
            span,
        }) => {
            let message = format!("not implemented yet: {what} (milestone {milestone})");
            Err(Failure::Internal(
                front.source,
                Box::new(Report::internal(message, Some(span))),
            ))
        }
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
        name: &front.source.name,
        text: &front.source.text,
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
    let span = match &error {
        CodegenError::Unsupported { span, .. } => Some(*span),
        CodegenError::Verify(_) | CodegenError::Emit(_) => None,
    };
    Failure::Internal(
        source.clone(),
        Box::new(Report::internal(error.to_string(), span)),
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
