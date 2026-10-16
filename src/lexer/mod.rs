//! Lexer — turns `.la` source text into tokens (spec §2).
//!
//! Produces a flat token list ending in `Eof`, each token with its byte span.
//! Reports every lexical error it can find (E0101–E0109) instead of stopping
//! at the first. Does not check literal ranges per type or fold `-` into
//! literals — that is the checker's job (spec §4).
//!
//! Depends on: span, diagnostic.

mod char_lit;
mod format_spec;
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
    lex_with(source, false)
}

/// [`lex`] for a source bundled with `lughac` (the prelude, a `std`
/// module): identifiers may start with `$`, naming compiler built-ins
/// such as `$float.sqrt` that a program can never write (v15).
///
/// # Errors
///
/// As [`lex`].
pub fn lex_bundled(source: &str) -> Result<(Vec<Token>, Vec<Diagnostic>), Vec<Diagnostic>> {
    lex_with(source, true)
}

/// The spans of `source`'s doc comments: block comments starting `/**`,
/// other than `/**/` (v16, milestone 77). Found by the lexer's own scan, so
/// strings and line comments are respected; errors elsewhere don't matter.
pub fn doc_comments(source: &str) -> Vec<Span> {
    let mut lexer = Lexer::new(source, true);
    lexer.run();
    lexer.docs
}

/// Whether a comment is `// …` or `/* … */`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentKind {
    Line,
    Block,
}

/// A comment in the source: where it is, from `//` or `/*` to the end of
/// its line (the line break excluded) or its `*/`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Comment {
    pub span: Span,
    pub kind: CommentKind,
}

/// Every comment of `source` in order, outermost only (v16, milestone 80,
/// for the formatter). Found by the lexer's own scan, as [`doc_comments`].
pub fn comments(source: &str) -> Vec<Comment> {
    let mut lexer = Lexer::new(source, true);
    lexer.run();
    lexer.comments
}

fn lex_with(source: &str, bundled: bool) -> Result<(Vec<Token>, Vec<Diagnostic>), Vec<Diagnostic>> {
    let mut lexer = Lexer::new(source, bundled);
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
    /// A bundled source, where `$` may start an identifier (v15).
    bundled: bool,
    /// Doc comments seen, outermost only (v16).
    docs: Vec<Span>,
    /// Every comment seen, outermost only (v16).
    comments: Vec<Comment>,
}

impl<'a> Lexer<'a> {
    fn new(src: &'a str, bundled: bool) -> Lexer<'a> {
        Lexer {
            src,
            pos: 0,
            tokens: Vec::new(),
            diagnostics: Vec::new(),
            interp: Vec::new(),
            bundled,
            docs: Vec::new(),
            comments: Vec::new(),
        }
    }
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
                '/' if self.byte_at(1) == Some(b'*') => self.skip_block_comment(start),
                'a'..='z' | 'A'..='Z' | '_' => self.identifier(start),
                // `$float`: a compiler built-in, in bundled sources only (v15).
                '$' if self.bundled => {
                    self.pos += 1;
                    self.identifier(start);
                }
                '0'..='9' => self.number(start),
                '"' => self.string(start),
                '\'' => self.char_literal(start),
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
        let start = self.pos;
        // `\n` is ASCII, so the byte search can't land inside a multi-byte char.
        self.pos = self.src[self.pos..]
            .find('\n')
            .map_or(self.src.len(), |i| self.pos + i);
        // A `\r\n` line's `\r` isn't part of the comment.
        let end = if self.src[..self.pos].ends_with('\r') {
            self.pos - 1
        } else {
            self.pos
        };
        self.comments.push(Comment {
            span: Span::new(start, end.max(start)),
            kind: CommentKind::Line,
        });
    }

    /// Skips a `/* … */` comment starting at `start`, counting nested ones
    /// (v16, milestone 76). Nothing inside is special but `/*` and `*/`.
    /// Never closed: E0113 at `start`, the outermost, and the rest of the
    /// input is the comment. Inside an interpolation the comment ends with
    /// its line, as the string must: a line break there ends the string (E0102).
    fn skip_block_comment(&mut self, start: usize) {
        let bytes = self.src.as_bytes();
        let stop = if self.interp.is_empty() {
            bytes.len()
        } else {
            self.src[start..]
                .find('\n')
                .map_or(bytes.len(), |i| start + i)
        };
        // A counter, not recursion: any depth is fine.
        let mut depth = 0usize;
        let mut i = start;
        while i < stop {
            match (bytes[i], bytes.get(i + 1)) {
                (b'/', Some(b'*')) => {
                    depth += 1;
                    i += 2;
                }
                (b'*', Some(b'/')) => {
                    depth -= 1;
                    i += 2;
                    if depth == 0 {
                        self.pos = i;
                        self.comments.push(Comment {
                            span: Span::new(start, i),
                            kind: CommentKind::Block,
                        });
                        // `/**…*/` but not `/**/` documents what follows (v16).
                        if bytes.get(start + 2) == Some(&b'*') && i - start > 4 {
                            self.docs.push(Span::new(start, i));
                        }
                        return;
                    }
                }
                _ => i += 1,
            }
        }
        self.pos = stop;
        if stop < bytes.len() {
            // The interpolation's line ends first: the main loop reports it.
            return;
        }
        self.diagnostics.push(
            Diagnostic::error(
                "E0113",
                "unterminated block comment",
                Span::new(start, start + 2),
            )
            .with_primary_label("this comment is never closed")
            .with_help("close it with `*/`"),
        );
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
                   break continue true false as i32 i64 u8 f64 bool string u32 u64 f32 i8 i16 u16 char";
        let expected = [
            Fun, Extern, Struct, Enum, Impl, Match, SelfKw, Let, Mut, If, Else, While, For, In, Of,
            Return, Break, Continue, True, False, As, TyI32, TyI64, TyU8, TyF64, TyBool, TyString,
            TyU32, TyU64, TyF32, TyI8, TyI16, TyU16, TyChar,
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
        for src in ["`", "#", "é", "$", "\u{0}"] {
            assert_eq!(errors(src), [("E0101", src)], "{src:?}");
        }
        assert_eq!(errors("a ` b"), [("E0101", "`")]);
        // `@` starts an attribute (v16, milestone 84).
        assert_eq!(kinds("@test"), [At, Ident("test".into())]);
    }

    #[test]
    fn every_error_is_reported_in_source_order() {
        let src = "let a = `;\nlet s = \"\\q\";\nlet n = 1__0;";
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

    #[test]
    fn dollar_names_lex_only_in_bundled_sources_v15() {
        let (tokens, _) = super::lex_bundled("$float.sqrt").unwrap();
        assert_eq!(tokens[0].kind, Ident("$float".into()));
        assert_eq!(errors("$float"), [("E0101", "$")]);
    }
    #[test]
    fn block_comments_are_skipped_v16() {
        assert_eq!(kinds("/* x */"), []);
        assert_eq!(kinds("a/*x*/b"), [Ident("a".into()), Ident("b".into())]);
        assert_eq!(
            kinds("a /* one\ntwo\n */ b"),
            [Ident("a".into()), Ident("b".into())]
        );
        assert_eq!(kinds("/**/ a /***/ /** doc */"), [Ident("a".into())]);
        // `//`, quotes and apostrophes mean nothing inside one.
        assert_eq!(kinds("/* // \" ' */ a"), [Ident("a".into())]);
        // Inside a string or a line comment, `/*` is text.
        assert_eq!(kinds("\"/* x\""), [Str("/* x".into())]);
        assert_eq!(kinds("// /* x\na"), [Ident("a".into())]);
    }

    #[test]
    fn block_comments_nest_v16() {
        assert_eq!(kinds("/* a /* b */ c */ d"), [Ident("d".into())]);
        assert_eq!(kinds("/*/**/*/ e"), [Ident("e".into())]);
        // Deep nesting is counted, not recursed into.
        let deep = format!("{}{} z", "/*".repeat(10_000), "*/".repeat(10_000));
        assert_eq!(kinds(&deep), [Ident("z".into())]);
    }

    #[test]
    fn spans_after_a_block_comment_are_exact_v16() {
        let src = "/* é 👋 */ let x";
        let (tokens, _) = lex(src).unwrap();
        assert_eq!(&src[tokens[0].span.start..tokens[0].span.end], "let");
        assert_eq!(&src[tokens[1].span.start..tokens[1].span.end], "x");
    }

    #[test]
    fn an_unclosed_block_comment_is_e0113_at_the_outermost_v16() {
        assert_eq!(errors("a /* never"), [("E0113", "/*")]);
        let src = "/* outer /* inner */ still open";
        let diags = lex(src).expect_err("unclosed");
        assert_eq!(diags.len(), 1);
        assert_eq!((diags[0].code, diags[0].span.start), ("E0113", 0));
        assert!(diags[0].help.as_deref().is_some_and(|h| h.contains("*/")));
    }

    #[test]
    fn a_stray_close_is_two_operators_v16() {
        assert_eq!(kinds("*/"), [Star, Slash]);
    }

    #[test]
    fn inside_an_interpolation_a_block_comment_stays_on_its_line_v16() {
        assert_eq!(
            kinds("\"{a /* c */ + 1}\""),
            [
                StrStart(String::new()),
                Ident("a".into()),
                Plus,
                Int(1),
                StrEnd(String::new()),
            ]
        );
        let codes: Vec<&str> = lex("\"{a /* c\n */}\"")
            .expect_err("broken")
            .iter()
            .map(|d| d.code)
            .collect();
        // The line break ends the string, as any line break there does.
        assert!(codes.contains(&"E0102"), "{codes:?}");
    }
    #[test]
    fn doc_comments_are_the_block_comments_starting_with_two_stars_v16() {
        let texts = |src: &str| -> Vec<String> {
            super::doc_comments(src)
                .iter()
                .map(|s| src[s.start..s.end].to_string())
                .collect()
        };
        assert_eq!(texts("/** a */ fun f() {}"), ["/** a */"]);
        assert_eq!(texts("/** a /* b */ c */ x"), ["/** a /* b */ c */"]);
        assert!(
            texts("/**/ /* x */ /*** stars */")
                .iter()
                .all(|t| t.starts_with("/***"))
        );
        assert_eq!(
            texts("let s = \"/** no */\"; // /** nor */\n"),
            Vec::<String>::new()
        );
        // Found even when the file has errors elsewhere.
        assert_eq!(texts("/** kept */ fun @"), ["/** kept */"]);
    }
}
