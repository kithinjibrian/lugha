//! `char` literals (v14 draft §2, milestone 46): `'a'`, `'é'`, `'\n'`,
//! `'\u{1F600}'`. One Unicode scalar value between single quotes; anything
//! else is E0112.

use super::{Lexer, TokenKind};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

impl Lexer<'_> {
    /// A `char` literal starting at the `'` at `start`.
    pub(super) fn char_literal(&mut self, start: usize) {
        self.pos += 1;
        let value = match self.peek_char() {
            Some('\\') => self.char_escape(),
            Some('\'') => Err("an empty character literal".to_string()),
            Some('\n') | None => Err("a character literal that never closes".to_string()),
            Some(c) => {
                self.pos += c.len_utf8();
                Ok(u32::from(c))
            }
        };
        // Anything up to the closing `'` (on this line) is one more error.
        let closed = self.peek_char() == Some('\'');
        if closed {
            self.pos += 1;
        } else {
            while let Some(c) = self.peek_char() {
                if c == '\'' || c == '\n' {
                    break;
                }
                self.pos += c.len_utf8();
            }
            if self.peek_char() == Some('\'') {
                self.pos += 1;
            }
        }
        let value = match value {
            Ok(_) if !closed => Err("a character literal holds one character".to_string()),
            other => other,
        };
        match value {
            Ok(code) => self.push(TokenKind::Char(code), start),
            Err(why) => {
                let span = Span::new(start, self.pos.max(start + 1));
                let help = r"write one character, or an escape: \n \t \r \0 \\ \' \u{1F600}";
                self.error(Diagnostic::error("E0112", why, span).with_help(help));
                // Keep a token so the parser sees a value here.
                self.push(TokenKind::Char(0xFFFD), start);
            }
        }
    }

    /// The escape at `\`, as a code point.
    fn char_escape(&mut self) -> Result<u32, String> {
        self.pos += 1;
        let Some(c) = self.peek_char() else {
            return Err("an escape that never ends".to_string());
        };
        self.pos += c.len_utf8();
        let simple = match c {
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            '0' => '\0',
            '\\' => '\\',
            '\'' => '\'',
            '"' => '"',
            'u' => return self.unicode_escape(),
            other => return Err(format!("unknown escape `\\{other}`")),
        };
        Ok(u32::from(simple))
    }

    /// `\u{H…}` after its `u`: one to six hex digits naming a scalar value.
    fn unicode_escape(&mut self) -> Result<u32, String> {
        if self.peek_char() != Some('{') {
            return Err("`\\u` needs braces: `\\u{1F600}`".to_string());
        }
        self.pos += 1;
        let digits = self.skip_while(|b| b.is_ascii_hexdigit());
        let text = &self.src[self.pos - digits..self.pos];
        if self.peek_char() != Some('}') {
            return Err("`\\u{…}` needs a closing `}`".to_string());
        }
        self.pos += 1;
        if digits == 0 || digits > 6 {
            return Err("`\\u{…}` takes one to six hex digits".to_string());
        }
        let code = u32::from_str_radix(text, 16).map_err(|_| "a bad hex number".to_string())?;
        char::from_u32(code)
            .map(u32::from)
            .ok_or_else(|| format!("U+{code:X} is not a Unicode scalar value"))
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::TokenKind::Char;
    use crate::lexer::lex;
    use crate::lexer::test_util::kinds;

    #[test]
    fn character_literals_and_escapes() {
        assert_eq!(kinds("'a'"), [Char(97)]);
        assert_eq!(kinds("'é'"), [Char(0xE9)]);
        assert_eq!(kinds("'😀'"), [Char(0x1F600)]);
        assert_eq!(
            kinds(r"'\n' '\t' '\r' '\0' '\\' '\'' '\u{41}' '\u{10FFFF}'"),
            [
                Char(10),
                Char(9),
                Char(13),
                Char(0),
                Char(92),
                Char(39),
                Char(65),
                Char(0x10FFFF)
            ]
        );
    }

    #[test]
    fn bad_literals_are_e0112() {
        for bad in [
            "''",
            "'ab'",
            r"'\q'",
            r"'\u{D800}'",
            r"'\u{110000}'",
            r"'\u{}'",
            "'a",
        ] {
            let errors = lex(bad).expect_err(bad);
            assert_eq!(errors[0].code, "E0112", "{bad}");
        }
    }
}
