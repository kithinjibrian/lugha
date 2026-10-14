//! Reading one module (PRP-033): decoding, lexing and parsing a file into
//! the shared offset space. Moved verbatim from `modules/mod.rs` (PRP-080).

use std::path::PathBuf;

use super::Module;
use crate::diagnostic::Diagnostic;
use crate::span::Span;
use crate::{lexer, parser};

/// Decodes, lexes and parses one file at `base`, numbering its expressions
/// from `first_id`, and moves its diagnostics into the shared offset space.
pub(super) fn parse_module(
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
                package: None,
                imports: Vec::new(),
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
        package: None,
        imports: Vec::new(),
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
