//! `format` templates (v14 draft §5, milestone 48): the literal text of
//! `format("{} has {:>4} items", …)`, split into text and holes once here
//! for the checker (E0432) and codegen alike.

use crate::ast::{Expr, FormatText, Part};
use crate::span::Span;

/// One piece of a template.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Piece {
    /// Literal text, `{{` and `}}` already made single braces.
    Text(String),
    /// `{}` or `{:spec}`: the specifier's text, and the byte offset of the
    /// hole's `{` in the template.
    Hole(Option<String>, usize),
}

/// Splits `text`, a template as the lexer kept it (escapes decoded, braces
/// as written), into pieces; the error says what is wrong with it.
pub fn pieces(text: &str) -> Result<Vec<Piece>, String> {
    let mut pieces = Vec::new();
    let mut literal = String::new();
    let mut rest = text.char_indices().peekable();
    while let Some((at, c)) = rest.next() {
        match c {
            '{' | '}' if rest.peek().map(|&(_, n)| n) == Some(c) => {
                rest.next();
                literal.push(c);
            }
            '}' => return Err("a lone `}` in the template: write `}}` for a brace".to_string()),
            '{' => {
                let Some(close) = text[at..].find('}') else {
                    return Err("a `{` in the template is never closed".to_string());
                };
                let inside = &text[at + 1..at + close];
                let spec = match inside.strip_prefix(':') {
                    Some(spec) => Some(spec.to_string()),
                    None if inside.is_empty() => None,
                    None => {
                        return Err(format!(
                            "`{{{inside}}}` in a template: holes are `{{}}` or `{{:spec}}`, \
                             filled from the arguments in order"
                        ));
                    }
                };
                // Skip to just past the `}`.
                while rest.next_if(|&(i, _)| i <= at + close).is_some() {}
                if !literal.is_empty() {
                    pieces.push(Piece::Text(std::mem::take(&mut literal)));
                }
                pieces.push(Piece::Hole(spec, at));
            }
            _ => literal.push(c),
        }
    }
    if !literal.is_empty() {
        pieces.push(Piece::Text(literal));
    }
    Ok(pieces)
}

/// The number of holes in `pieces`.
pub fn holes(pieces: &[Piece]) -> usize {
    pieces
        .iter()
        .filter(|p| matches!(p, Piece::Hole(..)))
        .count()
}

/// `pieces` with each hole filled by the next of `args`, as the parts of an
/// interpolated string. A specifier's span is inside the template when
/// `exact` (its text has no escapes, so offsets match the source), otherwise
/// the whole template `span`.
pub fn parts(pieces: &[Piece], args: &[Expr], span: Span, exact: bool) -> Vec<Part> {
    let mut args = args.iter();
    pieces
        .iter()
        .map(|piece| match piece {
            Piece::Text(text) => Part::Text(text.clone()),
            Piece::Hole(spec, at) => {
                let arg = args.next().expect("the checker matched holes to arguments");
                let format = spec.as_ref().map(|text| {
                    let span = if exact {
                        // `"` then the hole's `{`: the spec starts after its `:`.
                        let start = span.start + 1 + at + 1;
                        Span::new(start, start + 1 + text.len())
                    } else {
                        span
                    };
                    FormatText {
                        text: text.clone(),
                        span,
                    }
                });
                Part::Expr(arg.clone(), format)
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::{Piece, holes, pieces};

    fn text(s: &str) -> Piece {
        Piece::Text(s.to_string())
    }

    #[test]
    fn templates_split_into_text_and_holes() {
        assert_eq!(
            pieces("{} has {:>4} items").unwrap(),
            [
                Piece::Hole(None, 0),
                text(" has "),
                Piece::Hole(Some(">4".into()), 7),
                text(" items")
            ]
        );
        assert_eq!(
            pieces("{{}} é {}").unwrap(),
            [text("{} é "), Piece::Hole(None, 8)]
        );
        assert_eq!(pieces("").unwrap(), []);
        assert_eq!(holes(&pieces("{}{}{:x}").unwrap()), 3);
    }

    #[test]
    fn mistakes_are_reported() {
        for bad in ["{x}", "{0}", "a { b", "}", "{}}"] {
            assert!(pieces(bad).is_err(), "{bad}");
        }
    }
}
