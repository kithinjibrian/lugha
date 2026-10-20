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
#[cfg(test)]
mod entry_tests;
pub mod fetch;
pub mod git;
mod inline;
pub mod layout;
pub mod link_table;
#[cfg(test)]
mod link_tests;
pub mod lock;
pub mod manifest;
pub mod package_table;
mod packages;
mod parse;
pub(crate) mod paths;
mod resolve;
#[cfg(test)]
mod roots_tests;
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
    /// Where `lugha.lock` belongs: beside the manifest governing the
    /// build, or the main file's folder when there is none (v17).
    pub lock_folder: PathBuf,
    /// The `[link]` tables of the packages with a module in the program,
    /// the program's first, in the order their manifests were read; only
    /// tables that list something (v16, milestone 87).
    pub links: Vec<link_table::PackageLink>,
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
    load_all(&[main.to_path_buf()], read, env)
}

/// [`load`] for several roots in one folder, loaded as one program (v17,
/// milestone 124): `lughac test` builds a package folder's test files
/// together. With more than one, each root is a module from the start —
/// its package's module inside the `source` folder, else the one named by
/// its stem (`diff.la` → `diff`, or `$test<n>` when the stem is no name) —
/// so a root another imports keeps one name. The roots come first in
/// [`Loaded::modules`], in order.
///
/// # Errors
///
/// As [`load`].
pub fn load_all(roots: &[PathBuf], read: &mut Read<'_>, env: LoadEnv) -> Result<Loaded, LoadError> {
    let main = roots.first().expect("a load has a root").as_path();
    let root = main.parent().unwrap_or(Path::new("")).to_path_buf();
    let as_modules: Vec<_> = roots
        .iter()
        .map(|r| {
            env.as_module
                .then(|| layout::module_path(r, read, &env))
                .flatten()
        })
        .collect();
    let several = roots.len() > 1;
    let manifest_folder = layout::manifest_folder(&root, read, &env);
    let mut loaded = Loaded {
        modules: Vec::new(),
        diagnostics: Vec::new(),
        manifests: Vec::new(),
        lock: None,
        links: Vec::new(),
        lock_folder: manifest_folder.clone(),
    };
    let (mut base, mut next_id) = (0usize, 0u32);
    let mut packages = packages::Packages::new(root.clone(), manifest_folder, env.fetcher);
    // Every file ever queued, with its module path, so each is read once
    // whoever imports it, and only under one name (E0328, v15).
    // Several roots are named before any import can reach one of them.
    let mut seen: Vec<(PathBuf, Vec<String>)> = roots
        .iter()
        .zip(&as_modules)
        .enumerate()
        .map(|(i, (r, own))| {
            let path = match own {
                _ if !several => Vec::new(),
                Some((_, path)) => path.clone(),
                None => vec![stem_name(r).unwrap_or_else(|| format!("$test{i}"))],
            };
            (paths::normalized(r), path)
        })
        .collect();
    // A module to read, its package, and the import that asked for it (`None` for a root).
    type Pending = (Vec<String>, PathBuf, Option<String>, Option<Span>);
    let mut queue: VecDeque<Pending> = roots
        .iter()
        .zip(&seen)
        .map(|(r, (_, path))| (path.clone(), r.to_path_buf(), None, None))
        .collect();
    // The main file was loaded as a module (`check`/`test`): no E0319.
    let mut entry_is_module = several;
    while let Some((path, file, mut package, asked_by)) = queue.pop_front() {
        // A `std` module is embedded in `lughac`, never read from disk (v15).
        let embedded = (package.as_deref() == Some(packages::STD))
            .then(|| crate::STD.iter().find(|(n, _)| *n == path[1..].join(".")))
            .flatten();
        let bytes =
            match embedded.map_or_else(|| read(&file), |(_, text)| Ok(text.as_bytes().to_vec())) {
                Ok(bytes) => bytes,
                Err(e) if e.kind() == io::ErrorKind::NotFound && asked_by.is_some() => {
                    let at = asked_by.expect("checked just above");
                    // From the main file: a module beside it, by a shorter name.
                    let from_main = loaded
                        .modules
                        .first()
                        .is_some_and(|m| at.end <= m.text.len());
                    let beside = (1..path.len()).map(|k| &path[k..]).find(|rest| {
                        from_main && paths::exists(read, &resolve::file_of(&root, rest))
                    });
                    loaded
                        .diagnostics
                        .push(resolve::not_found(&path, &file, at, beside));
                    continue;
                }
                Err(source) => return Err(LoadError::Io { path: file, source }),
            };
        let mut module = parse::parse_module(
            path,
            file,
            (base, next_id),
            bytes,
            &mut loaded.diagnostics,
            env.partial,
        );
        module.package = package.clone();
        // One byte of gap, so a span at a file's very end isn't the next file's start.
        base = base
            .checked_add(module.text.len())
            .and_then(|b| b.checked_add(1))
            .ok_or(LoadError::TooLarge)?;
        // After its first module, so the main file keeps offset 0 (read once).
        packages.read_manifest(&package, read, &mut base, &mut loaded)?;
        // A package's file under `test`/`check` is its module (v17 draft
        // decision 43): imported by a hidden entry, so cycles through it hold.
        let at_root = asked_by
            .is_none()
            .then(|| roots.iter().position(|r| *r == module.file))
            .flatten();
        if let Some(i) = at_root
            && let Some((name, path)) = &as_modules[i]
            && packages.own.as_ref() == Some(name)
        {
            module.path = path.clone();
            module.package = Some(name.clone());
            package = Some(name.clone());
            seen[i].1 = path.clone();
            // Its imports resolve as the package's: the names its manifest lists.
            packages.read_manifest(&package, read, &mut base, &mut loaded)?;
            entry_is_module = true;
        } else if !several
            && env.as_module
            && asked_by.is_none()
            && let Some(name) = library_name(&module, main)
        {
            // Outside a package, a library file (no `fun main`) under
            // `check`/`test` is the module named after it, so modules it
            // imports may import it back (session 239).
            module.path = vec![name.clone()];
            seen[0].1 = vec![name];
            entry_is_module = true;
        }
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
                let entry = (!entry_is_module).then_some(main);
                let error = error.or_else(|| resolve::check(import, program, entry, &target.file));
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
    // A program under its package's manifest (in `test/`, `examples/`, …)
    // sees the package's package-level items (v17 draft decision 45). Set
    // after every import resolved: resolution still treats it as the root.
    if let Some(own) = &packages.own {
        for module in loaded.modules.iter_mut().filter(|m| m.package.is_none()) {
            module.package = Some(own.clone());
        }
    }
    loaded.lock = packages.fetcher.lock_text();
    Ok(loaded)
}

/// The module name of `main`, a library file (no `fun main`), when its
/// file name can be one: `diff.la` → `diff`.
fn library_name(module: &Module, main: &Path) -> Option<String> {
    let program = module.program.as_ref()?;
    let has_main = program
        .items
        .iter()
        .any(|i| matches!(i, crate::ast::Item::Fun(f) if f.name.name == "main"));
    (!has_main).then(|| stem_name(main)).flatten()
}

/// `file`'s stem when it can name a module: `diff.la` → `diff`.
fn stem_name(file: &Path) -> Option<String> {
    let stem = file.file_stem()?.to_str()?;
    let mut chars = stem.chars();
    let first = chars.next()?;
    let ident = (first.is_ascii_alphabetic() || first == '_')
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '_');
    ident.then(|| stem.to_string())
}

/// The prelude's module path. `$` can't start a module name, so no file clashes.
pub const PRELUDE_PATH: &str = "$prelude";

/// Appends the prelude (`lugha::PRELUDE`) to `modules`, after the last one
/// in the shared offset space, numbering its expressions after theirs.
/// Its diagnostics, which only a compiler bug can cause, go to `diagnostics`.
pub fn with_prelude(modules: &mut Vec<Module>, diagnostics: &mut Vec<Diagnostic>) {
    let base = modules.last().map_or(0, |m| m.base + m.text.len() + 1);
    with_prelude_at(modules, diagnostics, base);
}

/// [`with_prelude`] with the prelude at offset `base`, for a program whose
/// manifests sit after its last module: the language server places it past
/// every file it loaded, so a manifest's diagnostic is never taken for the
/// prelude's (milestone 75).
pub fn with_prelude_at(modules: &mut Vec<Module>, diagnostics: &mut Vec<Diagnostic>, base: usize) {
    let first_id = modules
        .iter()
        .filter_map(|m| m.program.as_ref())
        .map(|p| p.expr_count)
        .max()
        .unwrap_or(0);
    let path = vec![PRELUDE_PATH.to_string()];
    let bytes = crate::PRELUDE.as_bytes().to_vec();
    let file = PathBuf::from("<prelude>");
    let module = parse::parse_module(path, file, (base, first_id), bytes, diagnostics, false);
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
