//! The formatter (v16, milestone 80): prints a `.la` file in the one
//! canonical style (spec §9, draft decisions 60–65).
//!
//! It parses the file, prints the tree through a small document algebra
//! (`doc`) that breaks lists only when they don't fit in 100 columns, puts
//! every comment back next to the item, statement, field, variant or arm
//! it belongs to (`comments`), and finally re-parses its own output and
//! compares it with the original (`verify`): a printer bug can only make
//! it refuse, never change what a program means. Literals and names are
//! copied from the source, so `0xFF` stays `0xFF`.
//!
//! Depends on: lexer, parser, ast, span, diagnostic.

mod comments;
mod doc;
mod exprs;
mod items;
mod leaves;
mod stmts;
mod verify;

#[cfg(test)]
mod tests;

use crate::diagnostic::Diagnostic;
use crate::lexer::{self, Comment};
use crate::span::Span;

/// Why a source couldn't be formatted.
#[derive(Debug, thiserror::Error)]
pub enum FormatError {
    /// The source doesn't lex or parse; nothing is formatted.
    #[error("the source has syntax errors")]
    Syntax(Vec<Diagnostic>),
    /// The formatted text would differ from the source in meaning or
    /// comments — a formatter bug, so nothing may be written.
    #[error("the formatted text doesn't match the original: {0}")]
    Unsafe(String),
}

/// `src` in the canonical style, verified to parse to the same tree with
/// the same comments.
///
/// # Errors
///
/// [`FormatError::Syntax`] if `src` has syntax errors;
/// [`FormatError::Unsafe`] if the output failed verification.
///
/// # Examples
///
/// ```
/// let out = lugha::formatter::format("fun  two():i64=2;").unwrap();
/// assert_eq!(out, "fun two(): i64 = 2;\n");
/// ```
pub fn format(src: &str) -> Result<String, FormatError> {
    format_with(src, false)
}

/// [`format`] for a source bundled with `lughac` (the prelude, a `std`
/// module), where names may start with `$`.
///
/// # Errors
///
/// As [`format`].
pub fn format_bundled(src: &str) -> Result<String, FormatError> {
    format_with(src, true)
}

fn format_with(src: &str, bundled: bool) -> Result<String, FormatError> {
    let program = verify::parse(src, bundled)?;
    let mut printer = Printer::new(src);
    let doc = printer.program(&program);
    let mut out = doc::print(&doc, 0);
    if !out.is_empty() {
        out.push('\n');
    }
    verify::verify(src, &out, bundled)?;
    Ok(out)
}

/// The printer's state: the source and which of its comments have been
/// printed.
struct Printer<'a> {
    src: &'a str,
    comments: Vec<Comment>,
    /// `claimed[i]`: `comments[i]` is placed in the output.
    claimed: Vec<bool>,
    /// Printing a condition, where a struct literal needs parentheses
    /// (spec §3, E0205), as the parser's own flag.
    no_struct: bool,
    /// Inside a broken `if` chain's `else`: its `else if` breaks too (v17).
    in_broken_chain: bool,
}

impl<'a> Printer<'a> {
    fn new(src: &'a str) -> Printer<'a> {
        let comments = lexer::comments(src);
        let claimed = vec![false; comments.len()];
        Printer {
            src,
            comments,
            claimed,
            no_struct: false,
            in_broken_chain: false,
        }
    }

    /// The source text at `span`.
    fn text(&self, span: Span) -> &'a str {
        &self.src[span.start..span.end]
    }

    /// The source text at `span` without whitespace: a name, possibly
    /// qualified (`m.v`), as written.
    fn name(&self, span: Span) -> String {
        self.text(span).split_whitespace().collect()
    }
}
