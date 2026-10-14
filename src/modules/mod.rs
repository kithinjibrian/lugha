//! Modules — loading the main file and every module it imports (v2 draft
//! §6, PRP-033). Runs after the parser and before the checker.
//!
//! A file is a module named by its path from the main file's directory:
//! `import shapes.circle;` reads `<root>/shapes/circle.la`. Each file is read
//! once, so import cycles need nothing special. All files share one offset
//! space: each gets a base after the previous one, and its spans include it,
//! so a `Span` still needs no file id; the driver maps offsets back.
//!
//! Depends on: ast, lexer, parser, diagnostic, span.

pub mod cache;
pub mod fetch;
pub mod git;
mod inline;
pub mod layout;
pub mod lock;
pub mod manifest;
pub mod package_table;
mod packages;
mod parse;
pub(crate) mod paths;
mod resolve;
#[cfg(test)]
mod std_tests;
#[cfg(test)]
mod tests;
pub mod version;
#[cfg(test)]
mod version_tests;

use std::collections::VecDeque;
use std::io;
use std::path::{Path, PathBuf};

use self::layout::LoadEnv;
use crate::ast::{Import, Program};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// One loaded source file.
#[derive(Debug, Clone)]
pub struct Module {
    /// The module's path segments: `["shapes", "circle"]`; empty for the main file.
    pub path: Vec<String>,
    /// The file it was read from.
    pub file: PathBuf,
    /// Where the file starts in the program's shared offset space.
    pub base: usize,
    /// The file's text (for invalid UTF-8, only the valid prefix).
    pub text: String,
    /// The parsed module; `None` if it didn't lex or parse (see the diagnostics).
    pub program: Option<Program>,
    /// Its package; `None` for the root package (v8).
    pub package: Option<String>,
    /// Each import's module path, in order, as resolved through the
    /// manifests (v8): a package's own `import util.x;` is `geo.util.x`.
    pub imports: Vec<Vec<String>>,
}

/// A `lugha.toml` that was read, so its diagnostics map back to it (v8).
#[derive(Debug, Clone)]
pub struct ManifestFile {
    /// The file it was read from.
    pub file: PathBuf,
    /// Where it starts in the program's shared offset space.
    pub base: usize,
    /// Its text.
    pub text: String,
}

/// How the loader reads a file. Reading a folder must fail with an error
/// other than `NotFound`, as `std::fs::read` does: that is how a
/// manifest's folders are found to exist.
pub type Read<'a> = dyn FnMut(&Path) -> io::Result<Vec<u8>> + 'a;

/// Every module of a program, and every diagnostic found loading them.
#[derive(Debug, Clone)]
pub struct Loaded {
    /// The main file first, then the others in the order they were reached.
    pub modules: Vec<Module>,
    /// Errors and warnings from all modules, in the shared offset space.
    pub diagnostics: Vec<Diagnostic>,
    /// Every manifest read (v8), in the same offset space; `lugha.lock` too (v14).
    pub manifests: Vec<ManifestFile>,
    /// New text for `lugha.lock`, when the build changed it (v14).
    pub lock: Option<String>,
}

/// Why a program couldn't be loaded at all — an internal error, not a
/// mistake in the program.
#[derive(Debug, thiserror::Error)]
pub enum LoadError {
    /// A file exists (or is the main file) but can't be read.
    #[error("cannot read `{}`: {source}", path.display())]
    Io {
        path: PathBuf,
        #[source]
        source: io::Error,
    },
    /// The files together are larger than the offset space.
    #[error("the program's files are too large to load together")]
    TooLarge,
    /// A git package needs the cache, and no cache folder is known (v14).
    #[error("no cache for git packages: set `LUGHA_CACHE` or `HOME` to an absolute path")]
    NoCache,
    /// The package cache can't be written (v14).
    #[error("cannot write the package cache: {0}")]
    Cache(#[source] io::Error),
}

/// Loads `main` and every module it imports, reading files with `read`
/// and fetching git packages with `fetcher` (v14).
///
/// # Errors
///
/// [`LoadError::Io`] if the main file can't be read, or another file exists
/// but can't be read; [`LoadError::TooLarge`] if the offsets overflow. A
/// missing module, a clash and every lex or parse error are diagnostics in
/// [`Loaded::diagnostics`] instead. [`LoadError::NoCache`] or
/// [`LoadError::Cache`] if a git package's cache can't be found or written.
///
/// # Examples
///
/// ```
/// use std::path::Path;
/// let mut read = |_: &Path| Ok(b"fun main() {}".to_vec());
/// let env = lugha::modules::layout::LoadEnv::plain();
/// let loaded = lugha::modules::load(Path::new("main.la"), &mut read, env).unwrap();
/// assert_eq!(loaded.modules.len(), 1);
/// ```
pub fn load(main: &Path, read: &mut Read<'_>, env: LoadEnv) -> Result<Loaded, LoadError> {
    let root = main.parent().unwrap_or(Path::new("")).to_path_buf();
    let manifest_folder = layout::manifest_folder(&root, read, &env);
    let mut loaded = Loaded {
        modules: Vec::new(),
        diagnostics: Vec::new(),
        manifests: Vec::new(),
        lock: None,
    };
    let (mut base, mut next_id) = (0usize, 0u32);
    let mut packages = packages::Packages::new(root, manifest_folder, env.fetcher);
    // Every file ever queued, with its module path, so each is read once
    // whoever imports it, and only under one name (E0328, v15).
    let mut seen: Vec<(PathBuf, Vec<String>)> = vec![(paths::normalized(main), Vec::new())];
    // A module to read, its package, and the import that asked for it (`None` for main).
    type Pending = (Vec<String>, PathBuf, Option<String>, Option<Span>);
    let mut queue: VecDeque<Pending> = VecDeque::new();
    queue.push_back((Vec::new(), main.to_path_buf(), None, None));
    while let Some((path, file, package, asked_by)) = queue.pop_front() {
        // A `std` module is embedded in `lughac`, never read from disk (v15).
        let embedded = (package.as_deref() == Some(packages::STD))
            .then(|| crate::STD.iter().find(|(n, _)| *n == path[1..].join(".")))
            .flatten();
        let bytes =
            match embedded.map_or_else(|| read(&file), |(_, text)| Ok(text.as_bytes().to_vec())) {
                Ok(bytes) => bytes,
                Err(e) if e.kind() == io::ErrorKind::NotFound && asked_by.is_some() => {
                    let at = asked_by.expect("checked just above");
                    loaded
                        .diagnostics
                        .push(resolve::not_found(&path, &file, at));
                    continue;
                }
                Err(source) => return Err(LoadError::Io { path: file, source }),
            };
        let mut module =
            parse::parse_module(path, file, (base, next_id), bytes, &mut loaded.diagnostics);
        module.package = package.clone();
        // One byte of gap, so a span at a file's very end isn't the next file's start.
        base = base
            .checked_add(module.text.len())
            .and_then(|b| b.checked_add(1))
            .ok_or(LoadError::TooLarge)?;
        // After its first module, so the main file keeps offset 0 (read once).
        packages.read_manifest(&package, read, &mut base, &mut loaded)?;
        if let Some(program) = &module.program {
            next_id = program.expr_count;
            for import in &program.imports {
                // E0325/E0326 already stop the build; keep one path per import.
                if packages.unfetched(import) {
                    module.imports.push(names(import));
                    continue;
                }
                let (target, error) =
                    packages.resolve(import, &package, read, &mut base, &mut loaded)?;
                module.imports.push(target.path.clone());
                let error = error.or_else(|| resolve::check(import, program, main, &target.file));
                if let Some(d) = error {
                    loaded.diagnostics.push(d);
                    continue;
                }
                let file = paths::normalized(&target.file);
                match seen.iter().find(|(f, _)| *f == file) {
                    Some((_, first)) if *first != target.path => {
                        let at = import_path_span(import);
                        let d = layout::two_names(&target.file, first, &target.path, at);
                        loaded.diagnostics.push(d);
                    }
                    Some(_) => {}
                    None => {
                        seen.push((file, target.path.clone()));
                        let at = Some(import_path_span(import));
                        queue.push_back((target.path, target.file, target.package, at));
                    }
                }
            }
        }
        loaded.modules.push(module);
    }
    packages.check_versions(read, &mut base, &mut loaded)?;
    loaded.lock = packages.fetcher.lock_text();
    Ok(loaded)
}

/// The prelude's module path. `$` can't start a module name, so no file clashes.
pub const PRELUDE_PATH: &str = "$prelude";

/// Appends the prelude (`lugha::PRELUDE`) to `modules`, after the last one
/// in the shared offset space, numbering its expressions after theirs.
/// Its diagnostics, which only a compiler bug can cause, go to `diagnostics`.
pub fn with_prelude(modules: &mut Vec<Module>, diagnostics: &mut Vec<Diagnostic>) {
    let base = modules.last().map_or(0, |m| m.base + m.text.len() + 1);
    let first_id = modules
        .iter()
        .filter_map(|m| m.program.as_ref())
        .map(|p| p.expr_count)
        .max()
        .unwrap_or(0);
    let path = vec![PRELUDE_PATH.to_string()];
    let bytes = crate::PRELUDE.as_bytes().to_vec();
    let file = PathBuf::from("<prelude>");
    let module = parse::parse_module(path, file, (base, first_id), bytes, diagnostics);
    modules.push(module);
}

/// An import's segments as strings.
fn names(import: &Import) -> Vec<String> {
    import.path.iter().map(|s| s.name.clone()).collect()
}

/// From the first to the last segment of an import's path.
fn import_path_span(import: &Import) -> Span {
    let first = import.path.first().expect("an import has a path");
    let last = import.path.last().expect("an import has a path");
    Span::new(first.span.start, last.span.end)
}
