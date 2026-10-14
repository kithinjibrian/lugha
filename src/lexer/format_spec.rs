//! Format specifiers in the lexer (v13 draft §3, milestone 44): inside an
//! interpolation, a `:` outside every `(`, `[` and `{` of its expression
//! starts a specifier, kept as one raw token for the checker to parse.

use super::{Lexer, TokenKind};

impl Lexer<'_> {
    /// The format specifier after a top-level `:`, as one raw token up to
    /// (not including) the `}` that closes the interpolation (v13).
    pub(super) fn format_spec(&mut self) {
        let start = self.pos;
        self.pos += 1;
        let text_start = self.pos;
        while let Some(c) = self.peek_char() {
            if c == '}' || c == '\n' || c == '"' {
                break;
            }
            self.pos += c.len_utf8();
        }
        let text = self.src[text_start..self.pos].to_string();
        self.push(TokenKind::FmtSpec(text), start);
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::TokenKind::*;
    use crate::lexer::test_util::kinds;

    fn id(s: &str) -> crate::lexer::TokenKind {
        Ident(s.into())
    }

    #[test]
    fn a_top_level_colon_starts_a_format_specifier_v13() {
        assert_eq!(
            kinds(r#""{x:>5}!""#),
            [
                StrStart(String::new()),
                id("x"),
                FmtSpec(">5".into()),
                StrEnd("!".into())
            ]
        );
        assert_eq!(
            kinds(r#""{f(fun(a: i64): i64 = a)}""#)
                .iter()
                .filter(|k| matches!(k, FmtSpec(_)))
                .count(),
            0
        );
    }
}
