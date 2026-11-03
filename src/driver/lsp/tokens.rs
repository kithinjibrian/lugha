//! Semantic tokens (PRP-187, milestone 154): every token of a file,
//! classified, so editors without Lugha's TextMate grammar can colour it.
//! The lexer gives the spans; the check only names what identifiers are.

use lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens, SemanticTokensLegend,
};

use super::token_names::Names;
use crate::check;
use crate::lexer::{self, Token, TokenKind};

/// A token type, in the legend's order (its index is what is sent).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Kind {
    Namespace,
    Type,
    Struct,
    Enum,
    Interface,
    TypeParameter,
    Parameter,
    Variable,
    Property,
    EnumMember,
    Function,
    Method,
    Decorator,
    Keyword,
    Comment,
    String,
    Number,
    Operator,
}

/// The modifiers' bits, in the legend's order.
pub(super) const DECLARATION: u32 = 1;
pub(super) const MUTABLE: u32 = 1 << 1;
pub(super) const DEFAULT_LIBRARY: u32 = 1 << 2;
pub(super) const DOCUMENTATION: u32 = 1 << 3;

/// The legend the server declares: only LSP's predefined names, so every
/// client and theme already colours them.
pub(super) fn legend() -> SemanticTokensLegend {
    use SemanticTokenType as T;
    SemanticTokensLegend {
        token_types: vec![
            T::NAMESPACE,
            T::TYPE,
            T::STRUCT,
            T::ENUM,
            T::INTERFACE,
            T::TYPE_PARAMETER,
            T::PARAMETER,
            T::VARIABLE,
            T::PROPERTY,
            T::ENUM_MEMBER,
            T::FUNCTION,
            T::METHOD,
            T::DECORATOR,
            T::KEYWORD,
            T::COMMENT,
            T::STRING,
            T::NUMBER,
            T::OPERATOR,
        ],
        token_modifiers: vec![
            SemanticTokenModifier::DECLARATION,
            SemanticTokenModifier::new("mutable"),
            SemanticTokenModifier::DEFAULT_LIBRARY,
            SemanticTokenModifier::DOCUMENTATION,
        ],
    }
}

/// A classified token: bytes of the file's text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Raw {
    start: usize,
    end: usize,
    kind: Kind,
    mods: u32,
}

/// The tokens of `text`, names classified by `names` (a check of this
/// file) where the check's text and `text` still agree.
pub(super) fn answer(text: &str, names: Option<(&Names, &str)>) -> SemanticTokens {
    // A text that doesn't lex (an unclosed string being typed) keeps its
    // colour up to the error rather than losing all of it.
    let (tokens, limit) = match lexer::lex(text) {
        Ok((tokens, _)) => (tokens, text.len()),
        Err(errors) => {
            let mut cut = errors.iter().map(|d| d.span.start).min().unwrap_or(0);
            cut = cut.min(text.len());
            while !text.is_char_boundary(cut) {
                cut -= 1;
            }
            match lexer::lex(&text[..cut]) {
                Ok((tokens, _)) => (tokens, cut),
                Err(_) => (Vec::new(), 0),
            }
        }
    };
    // The check's positions hold only in the text both share.
    let shared = names.map_or(0, |(_, checked)| {
        text.bytes()
            .zip(checked.bytes())
            .take_while(|(a, b)| a == b)
            .count()
    });
    let mut raw = Vec::new();
    let docs = lexer::doc_comments(text);
    for c in lexer::comments(text) {
        if c.span.end <= limit {
            let doc = docs.iter().any(|d| d.start == c.span.start);
            let mods = if doc { DOCUMENTATION } else { 0 };
            raw.push(Raw {
                start: c.span.start,
                end: c.span.end,
                kind: Kind::Comment,
                mods,
            });
        }
    }
    let mut after_at = false;
    for (i, token) in tokens.iter().enumerate() {
        let classified = match &token.kind {
            // The name in `@test`.
            TokenKind::Ident(_) if after_at => Some((Kind::Decorator, 0)),
            TokenKind::Ident(word) => {
                let (start, end) = (token.span.start, token.span.end);
                let resolved = names
                    .filter(|_| end <= shared)
                    .and_then(|(n, _)| n.classify(start, end));
                resolved.or_else(|| intrinsic(word, tokens.get(i + 1)))
            }
            kind => lexical(kind),
        };
        after_at = token.kind == TokenKind::At;
        if let Some((kind, mods)) = classified {
            raw.push(Raw {
                start: token.span.start,
                end: token.span.end,
                kind,
                mods,
            });
        }
    }
    raw.sort_by_key(|r| r.start);
    SemanticTokens {
        result_id: None,
        data: encode(text, &raw),
    }
}

/// An unresolved call of an intrinsic (`println(…)`) or a built-in
/// variant (`Some`, `None`, `Ok`, `Err`): neither has a declaration, so the
/// check names nothing there. A program's own name of the same spelling
/// resolves, and wins before this is asked.
fn intrinsic(word: &str, next: Option<&Token>) -> Option<(Kind, u32)> {
    if ["Some", "None", "Ok", "Err"].contains(&word) {
        return Some((Kind::EnumMember, DEFAULT_LIBRARY));
    }
    let called = next.is_some_and(|t| t.kind == TokenKind::LParen);
    (called && check::intrinsics().any(|i| i == word)).then_some((Kind::Function, DEFAULT_LIBRARY))
}

/// The kind of a token that isn't a name; `None` for punctuation.
fn lexical(kind: &TokenKind) -> Option<(Kind, u32)> {
    use TokenKind::*;
    Some(match kind {
        Int(_) | Float(_) => (Kind::Number, 0),
        Str(_) | Char(_) | StrStart(_) | StrMid(_) | StrEnd(_) | FmtSpec(_) => (Kind::String, 0),
        TyI32 | TyI64 | TyU8 | TyF64 | TyBool | TyString | TyU32 | TyU64 | TyF32 | TyI8 | TyI16
        | TyU16 | TyChar => (Kind::Type, DEFAULT_LIBRARY),
        Plus | Minus | Star | Slash | Percent | PercentEq | Question | Amp | AmpEq | Pipe
        | PipeEq | Caret | CaretEq | Tilde | Shl | ShlEq | EqEq | BangEq | Lt | LtEq | Gt
        | GtEq | AndAnd | OrOr | Bang | Eq | PlusEq | MinusEq | StarEq | SlashEq | DotDot
        | FatArrow => (Kind::Operator, 0),
        At => (Kind::Decorator, 0),
        LParen | RParen | LBrace | RBrace | LBracket | RBracket | Comma | Semi | Colon | Dot
        | HashBrace | ColonColon | Eof | Ident(_) => return None,
        // Every other token is a keyword (spec §2).
        _ => (Kind::Keyword, 0),
    })
}

/// LSP's relative form: per token the line delta, the start (relative on
/// the same line), the length, the type and the modifiers; columns and
/// lengths in UTF-16 units. A token over several lines becomes one per
/// line, which every client accepts.
fn encode(text: &str, raw: &[Raw]) -> Vec<SemanticToken> {
    let mut out = Vec::new();
    let (mut prev_line, mut prev_col) = (0u32, 0u32);
    let mut lines = Lines::new(text);
    for r in raw {
        let mut start = r.start;
        while start < r.end {
            let stop = text[start..r.end].find('\n').map_or(r.end, |i| start + i);
            let piece = text[start..stop].trim_end_matches('\r');
            if !piece.is_empty() {
                let (line, col) = lines.at(start);
                debug_assert!(
                    line > prev_line || (line == prev_line && col >= prev_col) || out.is_empty(),
                    "tokens overlap at byte {start}"
                );
                out.push(SemanticToken {
                    delta_line: line - prev_line,
                    delta_start: if line == prev_line {
                        col - prev_col
                    } else {
                        col
                    },
                    length: piece.encode_utf16().count() as u32,
                    token_type: r.kind as u32,
                    token_modifiers_bitset: r.mods,
                });
                (prev_line, prev_col) = (line, col);
            }
            start = stop + 1;
        }
    }
    out
}

/// Line and UTF-16 column of byte offsets, asked in increasing order.
struct Lines<'a> {
    text: &'a str,
    line: u32,
    line_start: usize,
}

impl<'a> Lines<'a> {
    fn new(text: &'a str) -> Lines<'a> {
        Lines {
            text,
            line: 0,
            line_start: 0,
        }
    }

    fn at(&mut self, offset: usize) -> (u32, u32) {
        let skipped = &self.text[self.line_start..offset];
        if let Some(last) = skipped.rfind('\n') {
            self.line += skipped.matches('\n').count() as u32;
            self.line_start += last + 1;
        }
        let col = self.text[self.line_start..offset].encode_utf16().count() as u32;
        (self.line, col)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Absolute `(line, column, length, kind, modifiers)` of each token.
    fn decoded(text: &str) -> Vec<(u32, u32, u32, u32, u32)> {
        let (mut line, mut col) = (0, 0);
        answer(text, None)
            .data
            .iter()
            .map(|t| {
                if t.delta_line > 0 {
                    col = 0;
                }
                line += t.delta_line;
                col += t.delta_start;
                (line, col, t.length, t.token_type, t.token_modifiers_bitset)
            })
            .collect()
    }

    const KW: u32 = Kind::Keyword as u32;
    const STR: u32 = Kind::String as u32;
    const COMMENT: u32 = Kind::Comment as u32;

    #[test]
    fn positions_are_relative_and_count_utf16_units() {
        // `é` is two bytes and one UTF-16 unit; `𝄞` four bytes and two units.
        let tokens = decoded("let a = \"é𝄞\";\nreturn a;\n");
        // Without a check, `a` is unresolved and has no token.
        assert_eq!(tokens[0], (0, 0, 3, KW, 0));
        assert_eq!(tokens[1], (0, 6, 1, Kind::Operator as u32, 0));
        assert_eq!(tokens[2], (0, 8, 5, STR, 0));
        assert_eq!(tokens[3], (1, 0, 6, KW, 0));
        let raw = answer("let a = 1;\nlet b = 2;\n", None).data;
        // The next line starts again from column 0; the same line is relative.
        assert_eq!((raw[1].delta_line, raw[1].delta_start), (0, 6));
        assert_eq!((raw[3].delta_line, raw[3].delta_start), (1, 0));
    }

    #[test]
    fn a_comment_over_several_lines_is_one_token_per_line() {
        let tokens = decoded("/* one\n   two\n   three */\nlet x = 1;\n");
        assert_eq!(
            &tokens[..3],
            &[
                (0, 0, 6, COMMENT, 0),
                (1, 0, 6, COMMENT, 0),
                (2, 0, 11, COMMENT, 0)
            ]
        );
        assert_eq!(tokens[3].3, KW);
    }

    #[test]
    fn a_doc_comment_is_documentation() {
        let tokens = decoded("/** Doubles. */\nfun f() {}\n// plain\n");
        assert_eq!(tokens[0], (0, 0, 15, COMMENT, DOCUMENTATION));
        assert_eq!(tokens.last(), Some(&(2, 0, 8, COMMENT, 0)));
    }

    #[test]
    fn interpolation_pieces_and_format_specs_are_strings() {
        let tokens = decoded("println(\"a {x:>4} b {y}\");\n");
        let strings: Vec<_> = tokens.iter().filter(|t| t.3 == STR).collect();
        // `"a {`, `>4`, `} b {`, `}"`.
        assert_eq!(strings.len(), 4, "{tokens:?}");
        assert_eq!(strings[0].1, 8);
    }

    #[test]
    fn attributes_and_builtin_types_and_intrinsics() {
        let tokens = decoded("@test\nfun f(): i64 = 1;\nprintln(2);\nSome(None);\n");
        let deco = Kind::Decorator as u32;
        assert_eq!(&tokens[..2], &[(0, 0, 1, deco, 0), (0, 1, 4, deco, 0)]);
        assert!(tokens.contains(&(1, 9, 3, Kind::Type as u32, DEFAULT_LIBRARY)));
        assert!(tokens.contains(&(2, 0, 7, Kind::Function as u32, DEFAULT_LIBRARY)));
        let variant = Kind::EnumMember as u32;
        assert!(tokens.contains(&(3, 0, 4, variant, DEFAULT_LIBRARY)));
        assert!(tokens.contains(&(3, 5, 4, variant, DEFAULT_LIBRARY)));
    }

    #[test]
    fn a_text_that_does_not_lex_keeps_its_tokens_before_the_error() {
        let tokens = decoded("let a = 1;\nlet b = \"open\n");
        // `let`, `=`, `1`, `let`, `=`: the string from its `"` on is gone.
        assert_eq!(tokens.len(), 5, "{tokens:?}");
        assert_eq!(tokens[4], (1, 6, 1, Kind::Operator as u32, 0));
    }
}
