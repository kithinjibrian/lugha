//! Expressions: Pratt loop, prefix, postfix and primary forms (spec §3).
//!
//! Binding is by the spec §3 levels: `||` 1 … `* / %` 6, `as` 7, prefix
//! operators tighter, postfix tightest. Equality and comparison are
//! non-associative.

use super::{PResult, Parser, Reported};
use crate::ast::{BinOp, Expr, ExprKind, UnOp};
use crate::diagnostic::Diagnostic;
use crate::lexer::TokenKind;

/// Precedence level of `as`, above every binary operator.
const CAST_LEVEL: u8 = 7;

impl Parser<'_> {
    /// Parses an expression; struct literals are allowed.
    pub(super) fn expr(&mut self) -> PResult<Expr> {
        self.with_struct_literals(true, |p| p.nested(|p| p.binary(0)))
    }

    /// Parses an `if`/`while` condition or `for` head, where `Name {` ends the expression.
    pub(super) fn cond(&mut self) -> PResult<Expr> {
        self.with_struct_literals(false, |p| p.nested(|p| p.binary(0)))
    }

    /// The Pratt loop: operators at `min_level` or above.
    fn binary(&mut self, min_level: u8) -> PResult<Expr> {
        let start = self.span().start;
        let mut lhs = self.prefix()?;
        let mut previous: Option<BinOp> = None;
        loop {
            if self.at(&TokenKind::As) {
                if CAST_LEVEL < min_level {
                    break;
                }
                self.bump();
                let ty = self.ty()?;
                lhs = self.mk(start, ExprKind::Cast(Box::new(lhs), ty));
                continue;
            }
            let Some(op) = binop(self.peek()) else { break };
            if op.level() < min_level {
                break;
            }
            if let Some(prev) =
                previous.filter(|p| p.level() == op.level() && matches!(op.level(), 3 | 4))
            {
                return Err(self.chained(prev, op));
            }
            self.bump();
            let rhs = self.binary(op.level() + 1)?;
            lhs = self.mk(start, ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)));
            previous = Some(op);
        }
        Ok(lhs)
    }

    fn chained(&mut self, prev: BinOp, op: BinOp) -> Reported {
        let (p, o) = (prev.symbol(), op.symbol());
        let message = format!("`{p}` and `{o}` can't be chained");
        let help = if op.level() == 4 {
            format!("combine comparisons with `&&`: `(a {p} b) && (b {o} c)`")
        } else {
            format!("add parentheses: `(a {p} b) {o} c`")
        };
        self.error(Diagnostic::error("E0203", message, self.span()).with_help(help))
    }

    fn prefix(&mut self) -> PResult<Expr> {
        let op = match self.peek() {
            TokenKind::Minus => UnOp::Neg,
            TokenKind::Bang => UnOp::Not,
            _ => return self.postfix(),
        };
        let start = self.bump().start;
        let operand = self.nested(|p| p.prefix())?;
        Ok(self.mk(start, ExprKind::Unary(op, Box::new(operand))))
    }

    fn postfix(&mut self) -> PResult<Expr> {
        let start = self.span().start;
        let mut expr = self.primary()?;
        loop {
            let kind = match self.peek() {
                TokenKind::LParen => {
                    let open = self.bump();
                    let args = self.comma_list(open, TokenKind::RParen, |p| p.expr())?;
                    ExprKind::Call(Box::new(expr), args)
                }
                TokenKind::LBracket => {
                    let open = self.bump();
                    let index = self.expr()?;
                    self.close(open, &TokenKind::RBracket)?;
                    ExprKind::Index(Box::new(expr), Box::new(index))
                }
                TokenKind::Dot => {
                    self.bump();
                    ExprKind::Field(Box::new(expr), self.ident("field name")?)
                }
                _ => return Ok(expr),
            };
            expr = self.mk(start, kind);
        }
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
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use crate::ast::{ExprId, ExprKind, Item};
    use crate::parser::test_util::{errors, expr, parse_src};

    #[test]
    fn precedence_follows_the_spec_table() {
        let cases = [
            ("2 + 3 * 4", "(+ 2 (* 3 4))"),
            ("a || b && c", "(|| a (&& b c))"),
            ("a + b == c * d", "(== (+ a b) (* c d))"),
            ("-x as f64", "(as (neg x) f64)"),
            ("!a && b", "(&& (not a) b)"),
            ("a * b as f64", "(* a (as b f64))"),
            ("-a * b", "(* (neg a) b)"),
            ("a < b == c >= d", "(== (< a b) (>= c d))"),
            ("xs as i64[]", "(as xs i64[])"),
        ];
        for (src, want) in cases {
            assert_eq!(expr(src), want, "{src}");
        }
    }

    #[test]
    fn binary_operators_are_left_associative() {
        assert_eq!(expr("a - b - c"), "(- (- a b) c)");
        assert_eq!(expr("a / b / c % d"), "(% (/ (/ a b) c) d)");
        assert_eq!(expr("a || b || c"), "(|| (|| a b) c)");
    }

    #[test]
    fn postfix_chains() {
        assert_eq!(expr("a.b[i](c)"), "(call (index (. a b) i) c)");
        assert_eq!(expr("f(1, 2,)"), "(call f 1 2)");
        assert_eq!(expr("f()"), "(call f)");
        assert_eq!(expr("pts[i].x"), "(. (index pts i) x)");
        assert_eq!(expr("-xs.len"), "(neg (. xs len))");
    }

    #[test]
    fn chained_comparison_is_e0203() {
        assert_eq!(errors("fun t() { a < b < c; }"), [("E0203", "<")]);
        assert_eq!(errors("fun t() { a == b != c; }"), [("E0203", "!=")]);
        assert_eq!(expr("(a == b) == c"), "(== (== a b) c)");
        let diags = parse_src("fun t() { a < b < c; }").unwrap_err();
        assert!(
            diags[0].help.as_deref().is_some_and(|h| h.contains("&&")),
            "{diags:?}"
        );
    }

    #[test]
    fn assignment_inside_an_expression_is_e0204() {
        assert_eq!(errors("fun t() { a = b = c; }"), [("E0204", "=")]);
        assert_eq!(errors("fun t() { if (x = 1) {} }"), [("E0204", "=")]);
        assert_eq!(errors("fun t() { if x = 1 {} }"), [("E0204", "=")]);
        assert_eq!(errors("fun t() { f(x += 1); }"), [("E0204", "+=")]);
        let diags = parse_src("fun t() { if (x = 1) {} }").unwrap_err();
        assert!(
            diags[0].help.as_deref().is_some_and(|h| h.contains("==")),
            "{diags:?}"
        );
    }

    #[test]
    fn deep_nesting_is_e0206_not_a_stack_overflow() {
        let src = format!("fun t() {{ {}1{} }}", "(".repeat(300), ")".repeat(300));
        let codes: Vec<_> = errors(&src).into_iter().map(|(code, _)| code).collect();
        assert_eq!(codes, ["E0206"]);
        let src = format!("fun t() {{ {}x }}", "-".repeat(5_000));
        assert_eq!(errors(&src).len(), 1);
    }

    #[test]
    fn ids_are_dense_in_creation_order_and_spans_cover_operands() {
        let src = "fun t() { 1 + 2 }";
        let p = parse_src(src).unwrap();
        assert_eq!(p.expr_count, 3);
        let Item::Fun(f) = &p.items[0] else {
            unreachable!()
        };
        let tail = f.body.tail.as_ref().unwrap();
        assert_eq!(tail.id, ExprId(2));
        let ExprKind::Binary(_, l, r) = &tail.kind else {
            panic!("{tail:?}")
        };
        assert_eq!((l.id, r.id), (ExprId(0), ExprId(1)));
        assert_eq!(&src[tail.span.start..tail.span.end], "1 + 2");
    }
}
