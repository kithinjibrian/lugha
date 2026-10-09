//! String literals (spec §2).
//!
//! A string may not contain a raw line break: an unclosed string ends at the
//! end of its line, so one missing `"` can't swallow the rest of the file.

use super::{Lexer, TokenKind};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

impl Lexer<'_> {
    /// Lexes a string literal starting at `start`, which holds `"`.
    pub(super) fn string(&mut self, start: usize) {
        self.pos += 1;
        let mut value = String::new();
        loop {
            let Some(c) = self.peek_char() else {
                return self.unterminated(start);
            };
            match c {
                '\n' => return self.unterminated(start),
                '\r' if self.byte_at(1) == Some(b'\n') => return self.unterminated(start),
                '"' => {
                    self.pos += 1;
                    return self.push(TokenKind::Str(value), start);
                }
                '\\' => self.escape(&mut value),
                _ => {
                    value.push(c);
                    self.pos += c.len_utf8();
                }
            }
        }
    }

    fn escape(&mut self, value: &mut String) {
        let at = self.pos;
        let decoded = match self.src[at + 1..].chars().next() {
            Some('n') => '\n',
            Some('t') => '\t',
            Some('r') => '\r',
            Some('\\') => '\\',
            Some('"') => '"',
            Some('0') => '\0',
            // `\` at end of input or line: leave the break for `string` to report.
            None | Some('\n' | '\r') => {
                self.pos += 1;
                return;
            }
            Some(other) => {
                self.pos += 1 + other.len_utf8();
                let span = Span::new(at, self.pos);
                let message = format!("unknown escape sequence `\\{other}`");
                let help = r#"valid escapes are \n \t \r \\ \" \0"#;
                return self.error(Diagnostic::error("E0103", message, span).with_help(help));
            }
        };
        value.push(decoded);
        self.pos += 2;
    }

    fn unterminated(&mut self, start: usize) {
        let span = Span::new(start, self.pos);
        let help = "strings end on the line they start; write `\\n` for a line break";
        self.error(Diagnostic::error("E0102", "unterminated string", span).with_help(help));
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::TokenKind::*;
    use crate::lexer::lex;
    use crate::lexer::test_util::{errors, kinds};

    #[test]
    fn strings_decode_escapes() {
        assert_eq!(kinds(r#""""#), [Str(String::new())]);
        assert_eq!(kinds(r#""hello\n""#), [Str("hello\n".into())]);
        assert_eq!(kinds(r#""\n\t\r\\\"\0""#), [Str("\n\t\r\\\"\0".into())]);
        assert_eq!(kinds("\"héllo 👋\""), [Str("héllo 👋".into())]);
    }

    #[test]
    fn unterminated_at_end_of_input_is_e0102() {
        assert_eq!(errors("\"abc"), [("E0102", "\"abc")]);
        assert_eq!(errors("\"ab\\"), [("E0102", "\"ab\\")]);
    }

    #[test]
    fn unterminated_at_end_of_line_resumes_on_next_line() {
        let src = "\"abc\nlet x @";
        // The span stops at the line break, and `@` on line 2 is still found.
        assert_eq!(errors(src), [("E0102", "\"abc"), ("E0101", "@")]);
    }

    #[test]
    fn unknown_escape_is_e0103_and_lexing_continues() {
        assert_eq!(errors(r#""a\qb\x""#), [("E0103", r"\q"), ("E0103", r"\x")]);
        assert_eq!(errors("\"\\é\""), [("E0103", "\\é")]);
        let diags = lex(r#""\q""#).unwrap_err();
        assert!(diags[0].help.is_some());
    }
}
