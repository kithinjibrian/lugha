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

mod resolve;
#[cfg(test)]
mod tests;

use std::collections::VecDeque;
use std::io;
use std::path::{Path, PathBuf};

use crate::ast::{Import, Program};
use crate::diagnostic::Diagnostic;
use crate::span::Span;
use crate::{lexer, parser};

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
}

/// Every module of a program, and every diagnostic found loading them.
#[derive(Debug, Clone)]
pub struct Loaded {
    /// The main file first, then the others in the order they were reached.
    pub modules: Vec<Module>,
    /// Errors and warnings from all modules, in the shared offset space.
    pub diagnostics: Vec<Diagnostic>,
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
}

/// Loads `main` and every module it imports, reading files with `read`.
///
/// # Errors
///
/// [`LoadError::Io`] if the main file can't be read, or another file exists
/// but can't be read; [`LoadError::TooLarge`] if the offsets overflow. A
/// missing module, a clash and every lex or parse error are diagnostics in
/// [`Loaded::diagnostics`] instead.
///
/// # Examples
///
/// ```
/// use std::path::Path;
/// let mut read = |_: &Path| Ok(b"fun main() {}".to_vec());
/// let loaded = lugha::modules::load(Path::new("main.la"), &mut read).unwrap();
/// assert_eq!(loaded.modules.len(), 1);
/// ```
pub fn load(
    main: &Path,
    read: &mut dyn FnMut(&Path) -> io::Result<Vec<u8>>,
) -> Result<Loaded, LoadError> {
    let root = main.parent().unwrap_or(Path::new("")).to_path_buf();
    let mut loaded = Loaded {
        modules: Vec::new(),
        diagnostics: Vec::new(),
    };
    // Every file ever queued, so each is read once, whoever imports it.
    let mut seen: Vec<PathBuf> = vec![main.to_path_buf()];
    // A module to read, and the import that asked for it (`None` for main).
    let mut queue: VecDeque<(Vec<String>, PathBuf, Option<Span>)> = VecDeque::new();
    queue.push_back((Vec::new(), main.to_path_buf(), None));
    let (mut base, mut next_id) = (0usize, 0u32);
    while let Some((path, file, asked_by)) = queue.pop_front() {
        let bytes = match read(&file) {
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
        let module = parse_module(path, file, (base, next_id), bytes, &mut loaded.diagnostics);
        if let Some(program) = &module.program {
            next_id = program.expr_count;
        }
        if let Some(program) = &module.program {
            for import in &program.imports {
                let target = resolve::target(&root, import);
                if let Some(d) = resolve::check(import, program, main, &target) {
                    loaded.diagnostics.push(d);
                    continue;
                }
                if !seen.contains(&target) {
                    seen.push(target.clone());
                    queue.push_back((names(import), target, Some(import_path_span(import))));
                }
            }
        }
        // One byte of gap, so a span at a file's very end isn't the next file's start.
        base = base
            .checked_add(module.text.len())
            .and_then(|b| b.checked_add(1))
            .ok_or(LoadError::TooLarge)?;
        loaded.modules.push(module);
    }
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
    let module = parse_module(path, file, (base, first_id), bytes, diagnostics);
    modules.push(module);
}

/// Decodes, lexes and parses one file at `base`, numbering its expressions
/// from `first_id`, and moves its diagnostics into the shared offset space.
fn parse_module(
    path: Vec<String>,
    file: PathBuf,
    (base, first_id): (usize, u32),
    bytes: Vec<u8>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Module {
    let text = match String::from_utf8(bytes) {
        Ok(text) => text,
        Err(error) => {
            let valid = error.utf8_error().valid_up_to();
            let mut bytes = error.into_bytes();
            bytes.truncate(valid);
            let at = Span::new(base + valid, base + valid);
            diagnostics.push(
                Diagnostic::error("E0110", "source is not valid UTF-8", at)
                    .with_help("save the file as UTF-8"),
            );
            let text = String::from_utf8(bytes).expect("bytes before valid_up_to are UTF-8");
            return Module {
                path,
                file,
                base,
                text,
                program: None,
            };
        }
    };
    let program = match lexer::lex(&text) {
        Err(errors) => {
            diagnostics.extend(errors.into_iter().map(|d| shift(d, base)));
            None
        }
        Ok((mut tokens, warnings)) => {
            diagnostics.extend(warnings.into_iter().map(|d| shift(d, base)));
            for token in &mut tokens {
                token.span = Span::new(token.span.start + base, token.span.end + base);
            }
            // The parser's spans all come from these tokens, so they are already shifted.
            match parser::parse_from(&tokens, first_id) {
                Ok((program, warnings)) => {
                    diagnostics.extend(warnings);
                    Some(program)
                }
                Err(errors) => {
                    diagnostics.extend(errors);
                    None
                }
            }
        }
    };
    Module {
        path,
        file,
        base,
        text,
        program,
    }
}

/// `d` moved by `base` bytes.
fn shift(mut d: Diagnostic, base: usize) -> Diagnostic {
    d.span = Span::new(d.span.start + base, d.span.end + base);
    for label in &mut d.labels {
        label.span = Span::new(label.span.start + base, label.span.end + base);
    }
    d
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
