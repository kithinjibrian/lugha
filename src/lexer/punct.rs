//! Operators and punctuation (spec §2), longest match first.

use super::Lexer;
use super::token::TokenKind;
use crate::diagnostic::Diagnostic;
use crate::span::Span;

impl Lexer<'_> {
    pub(super) fn punctuation(&mut self, start: usize, c: char) {
        use TokenKind::*;
        let next = self.byte_at(1);
        let (kind, len) = match (c, next) {
            ('<', Some(b'=')) => (LtEq, 2),
            ('>', Some(b'=')) => (GtEq, 2),
            ('=', Some(b'=')) => (EqEq, 2),
            ('!', Some(b'=')) => (BangEq, 2),
            ('&', Some(b'&')) => (AndAnd, 2),
            ('|', Some(b'|')) => (OrOr, 2),
            ('+', Some(b'=')) => (PlusEq, 2),
            ('-', Some(b'=')) => (MinusEq, 2),
            ('*', Some(b'=')) => (StarEq, 2),
            ('/', Some(b'=')) => (SlashEq, 2),
            ('.', Some(b'.')) => (DotDot, 2),
            ('=', Some(b'>')) => (FatArrow, 2),
            (':', Some(b':')) => (ColonColon, 2),
            ('<', _) => (Lt, 1),
            ('>', _) => (Gt, 1),
            ('=', _) => (Eq, 1),
            ('!', _) => (Bang, 1),
            ('+', _) => (Plus, 1),
            ('-', _) => (Minus, 1),
            ('*', _) => (Star, 1),
            ('/', _) => (Slash, 1),
            ('%', _) => (Percent, 1),
            ('.', _) => (Dot, 1),
            ('(', _) => (LParen, 1),
            (')', _) => (RParen, 1),
            ('{', _) => (LBrace, 1),
            ('}', _) => (RBrace, 1),
            ('[', _) => (LBracket, 1),
            (']', _) => (RBracket, 1),
            (',', _) => (Comma, 1),
            (';', _) => (Semi, 1),
            (':', _) => (Colon, 1),
            _ => return self.unexpected(start, c),
        };
        self.pos += len;
        self.push(kind, start);
    }

    fn unexpected(&mut self, start: usize, c: char) {
        self.pos += c.len_utf8();
        let span = Span::new(start, self.pos);
        let mut diagnostic =
            Diagnostic::error("E0101", format!("unexpected character {c:?}"), span);
        if let Some(fix) = match c {
            '&' => Some("`&&`"),
            '|' => Some("`||`"),
            _ => None,
        } {
            diagnostic = diagnostic.with_help(format!("did you mean {fix}?"));
        }
        self.error(diagnostic);
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_util::kinds;
    use super::TokenKind::*;

    #[test]
    fn operators_use_longest_match() {
        let src = "<= >= == != && || += -= *= /= .. < > = ! + - * / % . ( ) { } [ ] , ; :";
        let expected = [
            LtEq, GtEq, EqEq, BangEq, AndAnd, OrOr, PlusEq, MinusEq, StarEq, SlashEq, DotDot, Lt,
            Gt, Eq, Bang, Plus, Minus, Star, Slash, Percent, Dot, LParen, RParen, LBrace, RBrace,
            LBracket, RBracket, Comma, Semi, Colon,
        ];
        assert_eq!(kinds(src), expected);
        assert_eq!(
            kinds("a..b"),
            [Ident("a".into()), DotDot, Ident("b".into())]
        );
        assert_eq!(
            kinds("x<=-y"),
            [Ident("x".into()), LtEq, Minus, Ident("y".into())]
        );
    }

    #[test]
    fn v1_arrows_and_paths_are_single_tokens() {
        assert_eq!(
            kinds("=> :: = > : :"),
            [FatArrow, ColonColon, Eq, Gt, Colon, Colon]
        );
        assert_eq!(
            kinds("a=>b"),
            [Ident("a".into()), FatArrow, Ident("b".into())]
        );
        assert_eq!(kinds("==>"), [EqEq, Gt]);
    }
}
