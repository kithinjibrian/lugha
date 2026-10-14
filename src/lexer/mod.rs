//! Lexer — turns `.la` source text into tokens (spec §2).
//!
//! Produces a flat token list ending in `Eof`, each token with its byte span.
//! Reports every lexical error it can find (E0101–E0109) instead of stopping
//! at the first. Does not check literal ranges per type or fold `-` into
//! literals — that is the checker's job (spec §4).
//!
//! Depends on: span, diagnostic.

mod interp;
mod number;
mod punct;
mod string;
pub mod token;

pub use token::{Token, TokenKind};

use crate::diagnostic::{Diagnostic, Severity};
use crate::span::Span;

/// Splits `source` into tokens.
///
/// # Errors
///
/// Returns every lexical error found (codes E0101–E0109) if there is at least one.
///
/// # Examples
///
/// ```
/// use lugha::lexer::{lex, TokenKind};
///
/// let (tokens, _warnings) = lex("x + 1").unwrap();
/// let kinds: Vec<_> = tokens.into_iter().map(|t| t.kind).collect();
/// assert_eq!(kinds, [TokenKind::Ident("x".into()), TokenKind::Plus, TokenKind::Int(1), TokenKind::Eof]);
/// ```
pub fn lex(source: &str) -> Result<(Vec<Token>, Vec<Diagnostic>), Vec<Diagnostic>> {
    let mut lexer = Lexer {
        src: source,
        pos: 0,
        tokens: Vec::new(),
        diagnostics: Vec::new(),
        interp: Vec::new(),
    };
    lexer.run();
    if lexer
        .diagnostics
        .iter()
        .any(|d| d.severity == Severity::Error)
    {
        Err(lexer.diagnostics)
    } else {
        Ok((lexer.tokens, lexer.diagnostics))
    }
}

/// Lexing state shared with the `number` and `string` submodules.
struct Lexer<'a> {
    src: &'a str,
    /// Byte offset of the next unread character; always on a char boundary.
    pos: usize,
    tokens: Vec<Token>,
    diagnostics: Vec<Diagnostic>,
    /// The interpolations being lexed, innermost last (v3, nested in v4.1).
    interp: Vec<interp::Interp>,
}

impl Lexer<'_> {
    fn run(&mut self) {
        while let Some(c) = self.peek_char() {
            let start = self.pos;
            if self.in_interpolation(c) {
                continue;
            }
            match c {
                ' ' | '\t' | '\r' | '\n' => self.pos += 1,
                '/' if self.byte_at(1) == Some(b'/') => self.skip_comment(),
                'a'..='z' | 'A'..='Z' | '_' => self.identifier(start),
                '0'..='9' => self.number(start),
                '"' => self.string(start),
                _ => self.punctuation(start, c),
            }
        }
        self.finish_interpolation();
        let end = self.src.len();
        self.tokens.push(Token {
            kind: TokenKind::Eof,
            span: Span::new(end, end),
        });
    }

    fn peek_char(&self) -> Option<char> {
        self.src[self.pos..].chars().next()
    }

    /// The byte `offset` bytes ahead of `pos`. Only compared against ASCII, so
    /// it never needs to respect char boundaries.
    fn byte_at(&self, offset: usize) -> Option<u8> {
        self.src.as_bytes().get(self.pos + offset).copied()
    }

    /// Advances past every byte matching `pred` and returns how many were skipped.
    fn skip_while(&mut self, pred: impl Fn(u8) -> bool) -> usize {
        let start = self.pos;
        while self.byte_at(0).is_some_and(&pred) {
            self.pos += 1;
        }
        self.pos - start
    }

    fn push(&mut self, kind: TokenKind, start: usize) {
        self.tokens.push(Token {
            kind,
            span: Span::new(start, self.pos),
        });
    }

    fn error(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    fn skip_comment(&mut self) {
        // `\n` is ASCII, so the byte search can't land inside a multi-byte char.
        self.pos = self.src[self.pos..]
            .find('\n')
            .map_or(self.src.len(), |i| self.pos + i);
    }

    fn identifier(&mut self, start: usize) {
        self.skip_while(is_ident_byte);
        let word = &self.src[start..self.pos];
        let kind = token::keyword(word).unwrap_or_else(|| TokenKind::Ident(word.to_string()));
        self.push(kind, start);
    }
}

fn is_ident_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

#[cfg(test)]
pub(crate) mod test_util {
    use super::{TokenKind, lex};

    /// Token kinds of `src` without the trailing `Eof`. Panics if lexing fails.
    pub fn kinds(src: &str) -> Vec<TokenKind> {
        let (tokens, _) = lex(src).unwrap_or_else(|d| panic!("{src:?} failed: {d:?}"));
        let mut kinds: Vec<_> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(kinds.pop(), Some(TokenKind::Eof));
        kinds
    }

    /// `(code, spanned source text)` for every diagnostic. Panics if lexing succeeds.
    pub fn errors(src: &str) -> Vec<(&'static str, &str)> {
        let diags = lex(src).expect_err("expected lexical errors");
        diags
            .iter()
            .map(|d| (d.code, &src[d.span.start..d.span.end]))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::test_util::{errors, kinds};
    use super::*;
    use TokenKind::*;

    #[test]
    fn keywords_are_recognised_exactly() {
        let src = "fun extern struct enum impl match self let mut if else while for in of return \
                   break continue true false as i32 i64 u8 f64 bool string u32 u64 f32 i8 i16 u16";
        let expected = [
            Fun, Extern, Struct, Enum, Impl, Match, SelfKw, Let, Mut, If, Else, While, For, In, Of,
            Return, Break, Continue, True, False, As, TyI32, TyI64, TyU8, TyF64, TyBool, TyString,
            TyU32, TyU64, TyF32, TyI8, TyI16, TyU16,
        ];
        assert_eq!(kinds(src), expected);
        // v2 draft §2; longer words stay identifiers.
        assert_eq!(kinds("import private public"), [Import, Private, Public]);
        assert_eq!(
            kinds("interface interfaces"),
            [Interface, Ident("interfaces".into())]
        );
        assert_eq!(
            kinds("important pub privately"),
            [
                Ident("important".into()),
                Ident("pub".into()),
                Ident("privately".into())
            ]
        );
        // `Option`, `Some` and `None` are built-in names, not keywords (v1 §2).
        assert_eq!(
            kinds("Option Some None"),
            [
                Ident("Option".into()),
                Ident("Some".into()),
                Ident("None".into())
            ]
        );
        assert_eq!(
            kinds("fun_x Fun _ x9"),
            [
                Ident("fun_x".into()),
                Ident("Fun".into()),
                Ident("_".into()),
                Ident("x9".into())
            ]
        );
    }

    #[test]
    fn comments_and_whitespace_are_skipped() {
        assert_eq!(
            kinds("a // b c\n\td\r\n// é 👋"),
            [Ident("a".into()), Ident("d".into())]
        );
        assert_eq!(kinds("//"), []);
        assert_eq!(kinds("a/b"), [Ident("a".into()), Slash, Ident("b".into())]);
    }

    #[test]
    fn unexpected_characters_are_e0101() {
        // `&` and `|` are operators since v7.
        for src in ["@", "#", "é", "$", "\u{0}"] {
            assert_eq!(errors(src), [("E0101", src)], "{src:?}");
        }
        assert_eq!(errors("a @ b"), [("E0101", "@")]);
    }

    #[test]
    fn every_error_is_reported_in_source_order() {
        let src = "let a = @;\nlet s = \"\\q\";\nlet n = 1__0;";
        let codes: Vec<_> = errors(src).into_iter().map(|(code, _)| code).collect();
        assert_eq!(codes, ["E0101", "E0103", "E0105"]);
    }

    #[test]
    fn spans_slice_back_to_source_text() {
        let src = "fun main(): i32 {\n    let s = \"h\\n\"; // c\n    s.len + 0xFF\n}";
        let (tokens, _) = lex(src).unwrap();
        let texts: Vec<_> = tokens
            .iter()
            .map(|t| &src[t.span.start..t.span.end])
            .collect();
        assert_eq!(
            texts,
            [
                "fun", "main", "(", ")", ":", "i32", "{", "let", "s", "=", "\"h\\n\"", ";", "s",
                ".", "len", "+", "0xFF", "}", ""
            ]
        );
        let eof = tokens.last().unwrap();
        assert_eq!(
            (eof.kind.clone(), eof.span.start, eof.span.end),
            (Eof, src.len(), src.len())
        );
    }
}
