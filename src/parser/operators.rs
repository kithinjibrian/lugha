//! The binary and assignment operator tables (spec §3), and `>>`: the lexer
//! leaves it as two `>` tokens, since `Option<Option<i64>>` closes two
//! lists, and an expression reads two `>` with nothing between as one (v7).

use super::Parser;
use crate::ast::{AssignOp, BinOp};
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    /// The binary operator starting here, and how many tokens it takes.
    pub(super) fn binary_operator(&self) -> Option<(BinOp, usize)> {
        if self.at(&TokenKind::Gt) && self.touching(&TokenKind::Gt) {
            return Some((BinOp::Shr, 2));
        }
        // `>>=` ends the expression: the statement reads it as an assignment.
        if self.at(&TokenKind::Gt) && self.touching(&TokenKind::GtEq) {
            return None;
        }
        binop(self.peek()).map(|op| (op, 1))
    }

    /// The assignment operator starting here, and how many tokens it takes.
    pub(super) fn assignment_operator(&self) -> Option<(AssignOp, usize)> {
        if self.at(&TokenKind::Gt) && self.touching(&TokenKind::GtEq) {
            return Some((AssignOp::Shr, 2));
        }
        assign_op(self.peek()).map(|op| (op, 1))
    }

    /// True if the next token is `kind` and starts where the current one ends.
    fn touching(&self, kind: &TokenKind) -> bool {
        let (Some(here), Some(next)) = (self.tokens.get(self.pos), self.tokens.get(self.pos + 1))
        else {
            return false;
        };
        next.kind == *kind && next.span.start == here.span.end
    }

    /// Consumes `n` tokens and returns the span they cover.
    pub(super) fn bump_n(&mut self, n: usize) -> Span {
        let first = self.bump();
        let mut last = first;
        for _ in 1..n {
            last = self.bump();
        }
        Span::new(first.start, last.end)
    }
}

fn binop(kind: &TokenKind) -> Option<BinOp> {
    Some(match kind {
        TokenKind::OrOr => BinOp::Or,
        TokenKind::AndAnd => BinOp::And,
        TokenKind::EqEq => BinOp::Eq,
        TokenKind::BangEq => BinOp::Ne,
        TokenKind::Lt => BinOp::Lt,
        TokenKind::LtEq => BinOp::Le,
        TokenKind::Gt => BinOp::Gt,
        TokenKind::GtEq => BinOp::Ge,
        TokenKind::Plus => BinOp::Add,
        TokenKind::Minus => BinOp::Sub,
        TokenKind::Star => BinOp::Mul,
        TokenKind::Slash => BinOp::Div,
        TokenKind::Percent => BinOp::Rem,
        TokenKind::Pipe => BinOp::BitOr,
        TokenKind::Caret => BinOp::BitXor,
        TokenKind::Amp => BinOp::BitAnd,
        TokenKind::Shl => BinOp::Shl,
        _ => return None,
    })
}

fn assign_op(kind: &TokenKind) -> Option<AssignOp> {
    Some(match kind {
        TokenKind::Eq => AssignOp::Assign,
        TokenKind::PlusEq => AssignOp::Add,
        TokenKind::MinusEq => AssignOp::Sub,
        TokenKind::StarEq => AssignOp::Mul,
        TokenKind::SlashEq => AssignOp::Div,
        TokenKind::AmpEq => AssignOp::BitAnd,
        TokenKind::PipeEq => AssignOp::BitOr,
        TokenKind::CaretEq => AssignOp::BitXor,
        TokenKind::ShlEq => AssignOp::Shl,
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{body, errors, expr, program};

    #[test]
    fn bit_operators_follow_rusts_precedence() {
        assert_eq!(expr("a & b == c"), "(== (& a b) c)");
        assert_eq!(expr("a + b << 2"), "(<< (+ a b) 2)");
        assert_eq!(expr("a | b ^ c & d"), "(| a (^ b (& c d)))");
        assert_eq!(expr("~a & b"), "(& (bitnot a) b)");
        assert_eq!(expr("a >> 1 >> 2"), "(>> (>> a 1) 2)");
        assert_eq!(expr("x as u32 << 1"), "(<< (as x u32) 1)");
    }

    #[test]
    fn shift_right_is_two_touching_angles() {
        assert_eq!(expr("x >> 1"), "(>> x 1)");
        assert!(!errors("fun main() { let y = a > > b; }").is_empty());
        program("fun f(o: Option<Option<i64>>) {}");
    }

    #[test]
    fn compound_bit_assignments_parse() {
        let parsed = body("x &= 1; x |= 2; x ^= 3; x <<= 4; x >>= 5;");
        for op in ["&=", "|=", "^=", "<<=", ">>="] {
            assert!(parsed.contains(op), "{op}: {parsed}");
        }
    }
}
