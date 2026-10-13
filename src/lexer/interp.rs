use super::{Lexer, TokenKind};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// An interpolation being lexed: where its string and its `{` start, and
/// how many braces of its own expression are open. Interpolations nest
/// (v4.1, PRP-045): a `"` inside `{…}` starts a string that may interpolate
/// too, so the lexer keeps a stack of them.
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
                return self.string_unclosed(string_start);
            };
            match c {
                '\n' => return self.string_unclosed(string_start),
                '\r' if self.byte_at(1) == Some(b'\n') => {
                    return self.string_unclosed(string_start);
                }
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
                    self.interp.push(Interp {
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

    /// Handles `c` if it means something special inside the innermost
    /// interpolation: its braces, the `}` that ends it, a `"` starting a
    /// nested string, or a line break. False to lex `c` as an ordinary token.
    pub(super) fn in_interpolation(&mut self, c: char) -> bool {
        let Some(interp) = self.interp.last_mut() else {
            return false;
        };
        match c {
            '{' => interp.depth += 1,
            '}' if interp.depth > 0 => interp.depth -= 1,
            '}' => {
                let string_start = interp.string_start;
                self.interp.pop();
                let start = self.pos;
                self.pos += 1;
                self.string_text(start, string_start, false);
                return true;
            }
            '"' => {
                self.string(self.pos);
                return true;
            }
            '\n' => {
                self.interp_unclosed();
                return true;
            }
            _ => {}
        }
        false
    }

    /// A line or the input ends inside the innermost interpolation: the
    /// string it belongs to never ends.
    fn interp_unclosed(&mut self) {
        if let Some(interp) = self.interp.pop() {
            self.string_unclosed(interp.string_start);
        }
    }

    /// A string that runs past its line. If it began inside an
    /// interpolation, the missing `}` is the likely mistake: E0111 at that
    /// `{`. Otherwise E0102. Either way every open interpolation ends.
    fn string_unclosed(&mut self, string_start: usize) {
        match self.interp.last().copied() {
            Some(outer) => {
                self.interp.clear();
                let help = "close it with `}`, or write `{{` for a literal brace";
                let at = outer.brace;
                self.interp_error(at, at + 1, "`{` in a string is never closed", help);
            }
            None => self.unterminated(string_start),
        }
    }

    /// At the end of input: a string whose interpolation never closed.
    pub(super) fn finish_interpolation(&mut self) {
        self.interp_unclosed();
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
    fn string_literals_and_interpolations_nest() {
        assert_eq!(
            kinds(r#""a{f("s")}b""#),
            [
                StrStart("a".into()),
                id("f"),
                LParen,
                Str("s".into()),
                RParen,
                StrEnd("b".into())
            ]
        );
        assert_eq!(
            kinds(r#""{"{x}"}""#),
            [
                StrStart(String::new()),
                StrStart(String::new()),
                id("x"),
                StrEnd(String::new()),
                StrEnd(String::new())
            ]
        );
        assert_eq!(
            kinds(r#""{if c { "y" } else { "n" }}""#),
            [
                StrStart(String::new()),
                If,
                id("c"),
                LBrace,
                Str("y".into()),
                RBrace,
                Else,
                LBrace,
                Str("n".into()),
                RBrace,
                StrEnd(String::new())
            ]
        );
    }

    #[test]
    fn mistakes_are_e0111() {
        let at = |src: &str| -> Vec<(&'static str, String)> {
            let diags = lex(src).expect_err(src);
            diags
                .iter()
                .map(|d| (d.code, src[d.span.start..d.span.end].to_string()))
                .collect()
        };
        assert_eq!(at(r#""a } b""#), [("E0111", "}".to_string())]);
        assert_eq!(at(r#""{x""#), [("E0111", "{".to_string())]);
        assert_eq!(at(r#""{}""#), [("E0111", "{}".to_string())]);
        assert_eq!(at(r#""{ }""#), [("E0111", "{ }".to_string())]);
        assert_eq!(at(r#""{ "a" ""#), [("E0111", "{".to_string())]);
        // A line break ends a string that began inside `{…}`: the outer `{` is unclosed.
        assert_eq!(at("\"{\"a\nb"), [("E0111", "{".to_string())]);
        let d = &lex(r#""a } b""#).unwrap_err()[0];
        assert!(d.help.as_deref().unwrap().contains("}}"), "{:?}", d.help);
    }
}
