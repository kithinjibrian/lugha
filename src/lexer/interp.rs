use super::{Lexer, TokenKind};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// An interpolation being lexed: where its string and its `{` start, and
/// how many braces of its own expression are open.
#[derive(Debug, Clone, Copy)]
pub(super) struct Interp {
    string_start: usize,
    brace: usize,
    depth: usize,
}

impl Lexer<'_> {
    /// The text of a string from `pos` to its closing `"` or next `{`.
    /// `first` is false when it continues after an interpolation's `}`,
    /// which `start` holds.
    pub(super) fn string_text(&mut self, start: usize, string_start: usize, first: bool) {
        let mut value = String::new();
        loop {
            let Some(c) = self.peek_char() else {
                return self.unterminated(string_start);
            };
            match c {
                '\n' => return self.unterminated(string_start),
                '\r' if self.byte_at(1) == Some(b'\n') => return self.unterminated(string_start),
                '"' => {
                    self.pos += 1;
                    let kind = if first {
                        TokenKind::Str(value)
                    } else {
                        TokenKind::StrEnd(value)
                    };
                    return self.push(kind, start);
                }
                '{' | '}' if self.byte_at(1) == Some(c as u8) => {
                    value.push(c);
                    self.pos += 2;
                }
                '}' => {
                    self.interp_error(
                        self.pos,
                        self.pos + 1,
                        "unmatched `}` in a string",
                        "write `}}` for a literal brace",
                    );
                    value.push('}');
                    self.pos += 1;
                }
                '{' => {
                    let brace = self.pos;
                    self.pos += 1;
                    self.skip_while(|b| b == b' ' || b == b'\t');
                    if self.byte_at(0) == Some(b'}') {
                        self.pos += 1;
                        let help = "put an expression inside, or write `{{}}` for literal braces";
                        self.interp_error(brace, self.pos, "empty `{}` in a string", help);
                        continue;
                    }
                    let kind = if first {
                        TokenKind::StrStart(value)
                    } else {
                        TokenKind::StrMid(value)
                    };
                    self.tokens.push(super::Token {
                        kind,
                        span: Span::new(start, brace + 1),
                    });
                    self.interp = Some(Interp {
                        string_start,
                        brace,
                        depth: 0,
                    });
                    return;
                }
                '\\' => self.escape(&mut value),
                _ => {
                    value.push(c);
                    self.pos += c.len_utf8();
                }
            }
        }
    }

    /// Handles `c` if it means something special inside an interpolation:
    /// its braces, the `}` that ends it, a `"`, or a line break. False to
    /// lex `c` as an ordinary token.
    pub(super) fn in_interpolation(&mut self, c: char) -> bool {
        let Some(mut interp) = self.interp else {
            return false;
        };
        match c {
            '{' => interp.depth += 1,
            '}' if interp.depth > 0 => interp.depth -= 1,
            '}' => {
                let start = self.pos;
                self.pos += 1;
                self.interp = None;
                self.string_text(start, interp.string_start, false);
                return true;
            }
            '\n' => {
                self.interp = None;
                self.unterminated(interp.string_start);
                return true;
            }
            '"' => {
                self.interp = None;
                self.quote_in_interpolation(interp);
                return true;
            }
            _ => return false,
        }
        self.interp = Some(interp);
        false
    }

    /// A `"` inside `{…}`: a string literal there if the line has another
    /// `"`, else the end of a string whose `{` was never closed. Both E0111.
    fn quote_in_interpolation(&mut self, interp: Interp) {
        let at = self.pos;
        let line = self.src[at + 1..].split('\n').next().unwrap_or("");
        match line.find('"') {
            Some(i) => {
                self.pos = at + 1 + i + 1;
                let help = "build the string in a variable first";
                self.interp_error(
                    at,
                    self.pos,
                    "a string literal can't appear inside `{…}`",
                    help,
                );
                self.interp = Some(interp);
            }
            None => {
                self.pos += 1;
                let help = "close it with `}`, or write `{{` for a literal brace";
                self.interp_error(interp.brace, at, "`{` in a string is never closed", help);
                self.push(TokenKind::StrEnd(String::new()), at);
            }
        }
    }

    /// At the end of input: a string whose interpolation never closed.
    pub(super) fn finish_interpolation(&mut self) {
        if let Some(interp) = self.interp.take() {
            self.unterminated(interp.string_start);
        }
    }

    fn interp_error(&mut self, start: usize, end: usize, message: &str, help: &str) {
        let d =
            Diagnostic::error("E0111", message.to_string(), Span::new(start, end)).with_help(help);
        self.error(d);
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::TokenKind::*;
    use crate::lexer::lex;
    use crate::lexer::test_util::kinds;

    fn id(s: &str) -> crate::lexer::TokenKind {
        Ident(s.into())
    }

    #[test]
    fn interpolations_split_a_string_into_parts_and_tokens() {
        assert_eq!(
            kinds(r#""a {x} b {y + 1} c""#),
            [
                StrStart("a ".into()),
                id("x"),
                StrMid(" b ".into()),
                id("y"),
                Plus,
                Int(1),
                StrEnd(" c".into())
            ]
        );
        assert_eq!(
            kinds(r#""{x}""#),
            [StrStart(String::new()), id("x"), StrEnd(String::new())]
        );
    }

    #[test]
    fn doubled_braces_are_literal() {
        assert_eq!(kinds(r#""{{x}}""#), [Str("{x}".into())]);
        assert_eq!(kinds(r#""set: {{1, 2}}""#), [Str("set: {1, 2}".into())]);
    }

    #[test]
    fn braces_inside_an_interpolation_nest() {
        assert_eq!(
            kinds(r#""{if c { 1 } else { 2 }}!""#),
            [
                StrStart(String::new()),
                If,
                id("c"),
                LBrace,
                Int(1),
                RBrace,
                Else,
                LBrace,
                Int(2),
                RBrace,
                StrEnd("!".into())
            ]
        );
    }

    #[test]
    fn escapes_apply_inside_the_parts() {
        assert_eq!(
            kinds(r#""a\t{x}\n""#),
            [StrStart("a\t".into()), id("x"), StrEnd("\n".into())]
        );
    }

    #[test]
    fn mistakes_are_e0111() {
        for src in [
            r#""a } b""#,
            r#""{x""#,
            r#""{}""#,
            r#""{ }""#,
            r#""{f("s")}""#,
        ] {
            let diags = lex(src).expect_err(src);
            assert!(diags.iter().all(|d| d.code == "E0111"), "{src}: {diags:?}");
            assert!(!diags.is_empty());
        }
        let d = &lex(r#""a } b""#).unwrap_err()[0];
        assert!(d.help.as_deref().unwrap().contains("}}"), "{:?}", d.help);
    }
}
