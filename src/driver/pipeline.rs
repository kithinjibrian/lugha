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
use super::source::Source;
use super::update;
use crate::ast;
use crate::check::{self, Checked};
use crate::codegen::{self, CodegenError, OptLevel};
use crate::diagnostic::{Diagnostic, Severity};
use crate::link;
use crate::modules::fetch::Mode;
use crate::modules::link_table::PackageLink;
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
    pub warnings: Vec<Diagnostic>,
    /// The checker's output; `None` for the parse-only front end (`--emit=ast`).
    pub checked: Option<Checked>,
    /// The `[link]` tables of the program's packages (v16, milestone 87).
    pub links: Vec<PackageLink>,
}

/// Loads `path` and every module it imports, fetching git packages as
/// `mode` allows and writing a changed `lugha.lock` (v14). A file that
/// can't be read is internal; mistakes in the program come back as diagnostics.
pub(super) fn loaded(path: &Path, mode: Mode) -> Result<(Source, Loaded), Failure> {
    let mut read = |file: &Path| std::fs::read(file);
    loaded_with(path, mode, &mut read, (true, false, false))
}

/// `loaded` reading files through `read`, writing `lugha.lock` only if
/// `write_lock`, and keeping modules' recovered programs if `partial`: the
/// language server reads open documents' unsaved text, never writes (v16
/// draft decision 13) and checks text with syntax errors (decision 38).
/// With `as_module`, a file in its package's `source` folder is loaded as
/// that package's module (v17 draft decision 43).
pub(super) fn loaded_with(
    path: &Path,
    mode: Mode,
    read: &mut modules::Read<'_>,
    (write_lock, partial, as_module): (bool, bool, bool),
) -> Result<(Source, Loaded), Failure> {
    let name = path.display().to_string();
    let env = modules::layout::LoadEnv {
        partial,
        as_module,
        ..update::env(mode)
    };
    match modules::load(path, read, env) {
        Ok(loaded) => {
            let root = path.parent().unwrap_or(Path::new(""));
            let source = Source::of(&name, root, &loaded);
            if let (Some(text), true) = (&loaded.lock, write_lock) {
                update::write_lock(&loaded.lock_folder, text, &source)?;
            }
            Ok((source, loaded))
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
pub(super) fn parsed(path: &Path, mode: Mode) -> Result<Front, Failure> {
    parse_loaded(loaded(path, mode)?)
}

fn parse_loaded((source, loaded): (Source, Loaded)) -> Result<Front, Failure> {
    if loaded
        .diagnostics
        .iter()
        .any(|d| d.severity == Severity::Error)
    {
        return Err(Failure::Program(source, loaded.diagnostics));
    }
    Ok(Front {
        source,
        modules: loaded.modules,
        warnings: loaded.diagnostics,
        checked: None,
        links: loaded.links,
    })
}

/// Reads, lexes, parses and type-checks `path` (spec §9 stages 1–3).
pub(super) fn front(path: &Path, mode: Mode) -> Result<Front, Failure> {
    check_parsed(parsed(path, mode)?, false)
}

/// [`parsed`] for `lughac test` and `lughac check`: a file in its package's
/// `source` folder is that package's module (v17, milestone 97).
fn parsed_module(path: &Path, mode: Mode) -> Result<Front, Failure> {
    let mut read = |file: &Path| std::fs::read(file);
    parse_loaded(loaded_with(path, mode, &mut read, (true, false, true))?)
}

/// [`front`] for `lughac test` (milestone 84): no `main` needed, and the
/// checker lists the file's tests.
pub(super) fn front_tests(path: &Path, mode: Mode) -> Result<Front, Failure> {
    check_parsed(parsed_module(path, mode)?, true)
}

/// [`front`] for `lughac check` (v17, milestone 97): a file that declares
/// no `fun main` is checked as a library, the way `lughac test` checks it;
/// only `build` and `run` need `main`.
pub(super) fn front_check(path: &Path, mode: Mode) -> Result<Front, Failure> {
    let front = parsed_module(path, mode)?;
    let library = front
        .modules
        .first()
        .and_then(|m| m.program.as_ref())
        .is_some_and(|p| {
            !p.items
                .iter()
                .any(|i| matches!(i, ast::Item::Fun(f) if f.name.name == "main"))
        });
    check_parsed(front, library)
}

/// A program checked for the language server: what the checker found,
/// errors or not, with the modules and files it came from.
pub(super) struct PartialFront {
    pub source: Source,
    /// Every module, the main file first and the prelude last.
    pub modules: Vec<Module>,
    /// The loader's diagnostics: its warnings, and the syntax errors of a
    /// program checked partially (milestone 75); the checker's are in `partial`.
    pub warnings: Vec<Diagnostic>,
    pub partial: check::Partial,
}

/// Reads through `read` (never writing `lugha.lock`), lexes, parses and
/// checks `path` for the language server, keeping the checker's findings
/// even when the program has errors (v16 draft decision 20). `as_module`
/// loads it as a module, so other modules may import it.
pub(super) fn front_partial(
    path: &Path,
    mode: Mode,
    read: &mut modules::Read<'_>,
    as_module: bool,
) -> Result<PartialFront, Failure> {
    let (source, loaded) = loaded_with(path, mode, read, (false, true, as_module))?;
    // A module that didn't lex has no program: nothing to check.
    if loaded.modules.iter().any(|m| m.program.is_none()) {
        return Err(Failure::Program(source, loaded.diagnostics));
    }
    // Past every file loaded, manifests too: the loader's diagnostics stay
    // in the files they name.
    let end = loaded
        .manifests
        .iter()
        .map(|m| (m.base, &m.text))
        .chain(loaded.modules.iter().map(|m| (m.base, &m.text)))
        .map(|(base, text)| base + text.len() + 1)
        .max()
        .unwrap_or(0);
    let mut front = Front {
        source,
        modules: loaded.modules,
        warnings: loaded.diagnostics,
        checked: None,
        links: loaded.links,
    };
    let mut prelude_errors = Vec::new();
    modules::with_prelude_at(&mut front.modules, &mut prelude_errors, end);
    let prelude = front.modules.last().expect("the prelude was just added");
    front.source.add("<prelude>", prelude.base, &prelude.text);
    let start = prelude.base;
    if !prelude_errors.is_empty() {
        return Err(prelude_failure(&front.source, &prelude_errors));
    }
    let partial = check::check_partial(&units(&front.modules));
    // A mistake inside the prelude is the compiler's, not the program's.
    let in_prelude = partial
        .diagnostics
        .iter()
        .any(|d| d.severity == Severity::Error && d.span.start >= start);
    if in_prelude {
        return Err(prelude_failure(&front.source, &partial.diagnostics));
    }
    Ok(PartialFront {
        source: front.source,
        modules: front.modules,
        warnings: front.warnings,
        partial,
    })
}

fn check_parsed(mut front: Front, tests: bool) -> Result<Front, Failure> {
    // The prelude's array helpers (v3) are checked and compiled with every program.
    let mut prelude_errors = Vec::new();
    // Past every file loaded, manifests too: a link diagnostic (v16) points
    // into a `lugha.toml` after the checker ran, and must not land in the prelude.
    let end = front
        .source
        .files
        .iter()
        .map(|f| f.base + f.text.len() + 1)
        .max()
        .unwrap_or(0);
    modules::with_prelude_at(&mut front.modules, &mut prelude_errors, end);
    let prelude = front.modules.last().expect("the prelude was just added");
    front.source.add("<prelude>", prelude.base, &prelude.text);
    let start = prelude.base;
    let units = units(&front.modules);
    let result = if tests {
        check::check_tests(&units)
    } else {
        check::check_modules(&units)
    };
    match result {
        Ok((checked, warnings)) if prelude_errors.is_empty() => {
            front.warnings.extend(warnings);
            front.checked = Some(checked);
            Ok(front)
        }
        Ok(_) => Err(prelude_failure(&front.source, &prelude_errors)),
        // A mistake inside the prelude is the compiler's, not the program's.
        Err(errors) => match errors.iter().find(|d| d.span.start >= start) {
            Some(_) => Err(prelude_failure(&front.source, &errors)),
            None => Err(Failure::Program(front.source, errors)),
        },
    }
}

/// An internal error for a diagnostic inside the prelude: a compiler bug.
fn prelude_failure(source: &Source, errors: &[Diagnostic]) -> Failure {
    let first = errors.first().map_or("an error", |d| d.message.as_str());
    internal(source, format!("the prelude doesn't check: {first}"))
}

/// The program's LLVM IR, for `--emit=ir`.
pub(super) fn ir(front: &Front) -> Result<String, Failure> {
    let units = units(&front.modules);
    codegen::emit_ir_modules(&units, checked(front), &sources(front))
        .map_err(|e| codegen_failure(&front.source, e))
}

/// Compiles and links `front` into the executable `exe`, with what its
/// `[link]` tables add (v16): `pkg-config` runs first, so its failures
/// come before codegen's work.
pub(super) fn build(front: &Front, opt: OptLevel, exe: &Path) -> Result<(), Failure> {
    let mut plan = link::Plan::new(&front.links, &mut link::pkg_config::run)
        .map_err(|e| Failure::Program(front.source.clone(), vec![e.diagnostic()]))?;
    plan.tls = std_import(&front.modules, "tls", &["tls", "http"]);
    plan.crypto = std_import(&front.modules, "crypto", &["crypto"]);
    let temp = TempDir::new().map_err(|e| internal(&front.source, temp_error(e)))?;
    let object = temp.path.join("prog.o");
    codegen::emit_object_modules(
        &units(&front.modules),
        checked(front),
        &sources(front),
        opt,
        &object,
    )
    .map_err(|e| codegen_failure(&front.source, e))?;
    link::link_with(&[&object], &plan, &temp.path, exe).map_err(|e| match e.diagnostic() {
        Some(d) => Failure::Program(front.source.clone(), vec![d]),
        None => internal(&front.source, e.to_string()),
    })
}

/// The program's import that brings in OpenSSL's `std.<module>` (v17):
/// `None` unless the program loads it; else the first import of the
/// program's own modules that leads there (one of `via`: `std.http` leads
/// to `std.tls`). Only such programs get that runtime part and OpenSSL.
fn std_import(modules: &[Module], module: &str, via: &[&str]) -> Option<crate::span::Span> {
    if !modules.iter().any(|m| m.path == ["std", module]) {
        return None;
    }
    modules
        .iter()
        .filter(|m| m.package.as_deref() != Some("std"))
        .filter_map(|m| m.program.as_ref())
        .flat_map(|p| &p.imports)
        .find(|i| {
            let names: Vec<&str> = i.path.iter().map(|s| s.name.as_str()).collect();
            names.len() >= 2 && names[0] == "std" && via.contains(&names[1])
        })
        .map(|i| i.span)
}

/// Builds `front` into a temp dir and runs it with inherited standard
/// streams, passing `args` to the program.
///
/// Returns the program's exit code, or 128 + N if signal N killed it, as a shell reports it.
pub(super) fn run(front: &Front, opt: OptLevel, args: &[String]) -> Result<u8, Failure> {
    let temp = TempDir::new().map_err(|e| internal(&front.source, temp_error(e)))?;
    let exe = temp.path.join("prog");
    build(front, opt, &exe)?;
    let status = Command::new(&exe).args(args).status().map_err(|e| {
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
/// Every module's file, for panic locations in any of them.
fn sources(front: &Front) -> Vec<codegen::ModuleSource<'_>> {
    front
        .source
        .files
        .iter()
        .map(|f| codegen::ModuleSource {
            name: &f.name,
            base: f.base,
            text: &f.text,
        })
        .collect()
}

/// The modules as the checker and codegen take them, the main file first.
fn units(modules: &[Module]) -> Vec<check::Unit<'_>> {
    modules
        .iter()
        .map(|m| check::Unit {
            path: &m.path,
            program: m.program.as_ref().expect("every module parsed"),
            imports: &m.imports,
            package: m.package.as_deref(),
        })
        .collect()
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
pub(super) struct TempDir {
    pub(super) path: PathBuf,
}

impl TempDir {
    pub(super) fn new() -> io::Result<Self> {
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
