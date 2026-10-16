//! Verification (decision 63): the output must parse to the same tree as
//! the source, positions ignored, and keep the same comments in order.

use super::FormatError;
use crate::ast::Program;
use crate::lexer;
use crate::parser::{self, sexp};

/// `src` lexed and parsed, or its syntax errors.
pub(super) fn parse(src: &str, bundled: bool) -> Result<Program, FormatError> {
    let lexed = if bundled {
        lexer::lex_bundled(src)
    } else {
        lexer::lex(src)
    };
    let (tokens, _warnings) = lexed.map_err(FormatError::Syntax)?;
    let (program, _) = parser::parse(&tokens).map_err(FormatError::Syntax)?;
    Ok(program)
}

/// Checks that `out` means what `original` means and keeps its comments.
pub(super) fn verify(original: &str, out: &str, bundled: bool) -> Result<(), FormatError> {
    let before = parse(original, bundled)?;
    let after = parse(out, bundled)
        .map_err(|_| FormatError::Unsafe("the output doesn't parse".to_string()))?;
    if tree(&before) != tree(&after) {
        return Err(FormatError::Unsafe(
            "the output's syntax tree differs".to_string(),
        ));
    }
    if comments(original) != comments(out) {
        return Err(FormatError::Unsafe(
            "the output's comments differ".to_string(),
        ));
    }
    Ok(())
}

/// The tree as text, without the one thing that depends on positions: a
/// destructuring loop's hidden variable, `$item<offset>`.
fn tree(program: &Program) -> String {
    let text = sexp::program(program);
    let mut out = String::with_capacity(text.len());
    let mut rest = text.as_str();
    while let Some(at) = rest.find("$item") {
        out.push_str(&rest[..at + 5]);
        rest = rest[at + 5..].trim_start_matches(|c: char| c.is_ascii_digit());
    }
    out.push_str(rest);
    out
}

/// Each comment's lines, trimmed: re-indenting a block comment moves its
/// later lines, which is not a change of its text.
fn comments(src: &str) -> Vec<Vec<String>> {
    lexer::comments(src)
        .iter()
        .map(|c| {
            src[c.span.start..c.span.end]
                .lines()
                .map(|l| l.trim().to_string())
                .collect()
        })
        .collect()
}
