//! Blocks and statements (spec §3, §5).
//!
//! A statement starting with `if` or `{` is block-like: it needs no `;`, an
//! optional `;` after it is recorded, and if it is followed by `}` it is the
//! block's tail value.

use super::{PResult, Parser, Reported};
use crate::ast::{AssignOp, Block, Expr, ForIter, Stmt, StmtKind};
use crate::diagnostic::Diagnostic;
use crate::lexer::TokenKind;
use crate::span::Span;

/// One element of a block.
enum Element {
    Stmt(Stmt),
    /// The final expression, directly followed by `}`.
    Tail(Expr),
    /// Nothing to add (a stray `;`, already reported).
    Nothing,
}

impl Parser<'_> {
    /// Parses `{ stmts tail }`. Struct literals are allowed inside, even within a condition.
    pub(super) fn block(&mut self) -> PResult<Block> {
        self.nested(|p| p.with_struct_literals(true, Self::block_body))
    }

    fn block_body(&mut self) -> PResult<Block> {
        let open = self.expect(&TokenKind::LBrace)?;
        let mut stmts = Vec::new();
        let mut tail = None;
        loop {
            if self.eat(&TokenKind::RBrace) {
                break;
            }
            if self.at(&TokenKind::Eof) {
                self.close(open, &TokenKind::RBrace)?;
            }
            let before = self.pos;
            match self.element() {
                Ok(Element::Stmt(stmt)) => stmts.push(stmt),
                Ok(Element::Tail(expr)) => tail = Some(Box::new(expr)),
                Ok(Element::Nothing) => {}
                Err(Reported) => {
                    self.sync_stmt();
                    // Guarantee progress so a failure that consumed nothing can't loop.
                    if self.pos == before {
                        self.bump();
                    }
                }
            }
        }
        Ok(Block {
            stmts,
            tail,
            span: Span::new(open.start, self.prev_end()),
        })
    }

    fn element(&mut self) -> PResult<Element> {
        let start = self.span().start;
        let kind = match self.peek() {
            TokenKind::Let => self.let_stmt()?,
            TokenKind::While => {
                self.bump();
                let cond = self.cond()?;
                StmtKind::While {
                    cond,
                    body: self.block()?,
                }
            }
            TokenKind::For => self.for_stmt()?,
            TokenKind::Return => {
                self.bump();
                let value = if self.at(&TokenKind::Semi) {
                    None
                } else {
                    Some(self.expr()?)
                };
                self.expect(&TokenKind::Semi)?;
                StmtKind::Return(value)
            }
            TokenKind::Break | TokenKind::Continue => {
                let kind = if self.at(&TokenKind::Break) {
                    StmtKind::Break
                } else {
                    StmtKind::Continue
                };
                self.bump();
                self.expect(&TokenKind::Semi)?;
                kind
            }
            TokenKind::Semi => {
                let span = self.bump();
                let diagnostic = Diagnostic::error("E0201", "expected statement, found `;`", span);
                self.error(diagnostic.with_help("remove this `;`"));
                return Ok(Element::Nothing);
            }
            TokenKind::If | TokenKind::LBrace => {
                let expr = self.block_like()?;
                if self.at(&TokenKind::RBrace) {
                    return Ok(Element::Tail(expr));
                }
                let semicolon = self.eat(&TokenKind::Semi);
                StmtKind::Expr { expr, semicolon }
            }
            _ => {
                let expr = self.expr()?;
                if let Some(op) = assign_op(self.peek()) {
                    let op_span = self.bump();
                    let value = self.expr()?;
                    self.expect(&TokenKind::Semi)?;
                    StmtKind::Assign {
                        op,
                        op_span,
                        place: expr,
                        value,
                    }
                } else if self.at(&TokenKind::RBrace) {
                    return Ok(Element::Tail(expr));
                } else {
                    self.expect(&TokenKind::Semi)?;
                    StmtKind::Expr {
                        expr,
                        semicolon: true,
                    }
                }
            }
        };
        Ok(Element::Stmt(Stmt {
            kind,
            span: Span::new(start, self.prev_end()),
        }))
    }

    fn let_stmt(&mut self) -> PResult<StmtKind> {
        self.bump();
        let mutable = self.eat(&TokenKind::Mut);
        let name = self.ident("variable name")?;
        let ty = if self.eat(&TokenKind::Colon) {
            Some(self.ty()?)
        } else {
            None
        };
        self.expect(&TokenKind::Eq)?;
        let init = self.expr()?;
        self.expect(&TokenKind::Semi)?;
        Ok(StmtKind::Let {
            mutable,
            name,
            ty,
            init,
        })
    }

    fn for_stmt(&mut self) -> PResult<StmtKind> {
        self.bump();
        let open = if self.at(&TokenKind::LParen) {
            Some(self.bump())
        } else {
            None
        };
        // Inside the optional parentheses struct literals are allowed again.
        let head = |p: &mut Self| if open.is_some() { p.expr() } else { p.cond() };
        let var = self.ident("loop variable")?;
        let iter = if self.eat(&TokenKind::In) {
            let first = head(self)?;
            self.expect(&TokenKind::DotDot)?;
            ForIter::Range(first, head(self)?)
        } else if self.eat(&TokenKind::Of) {
            ForIter::Array(head(self)?)
        } else {
            return Err(self.expected("`in` or `of`"));
        };
        if let Some(open) = open {
            self.close(open, &TokenKind::RParen)?;
        }
        Ok(StmtKind::For {
            var,
            iter,
            body: self.block()?,
        })
    }
}

fn assign_op(kind: &TokenKind) -> Option<AssignOp> {
    Some(match kind {
        TokenKind::Eq => AssignOp::Assign,
        TokenKind::PlusEq => AssignOp::Add,
        TokenKind::MinusEq => AssignOp::Sub,
        TokenKind::StarEq => AssignOp::Mul,
        TokenKind::SlashEq => AssignOp::Div,
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{body, errors};

    #[test]
    fn tail_versus_statement() {
        assert_eq!(body("x * x"), "(block (* x x))");
        assert_eq!(body("x * x;"), "(block (; (* x x)))");
        assert_eq!(
            body("if c { a } - 1"),
            "(block (stmt (if c (block a))) (neg 1))"
        );
        assert_eq!(
            body("if c { f(); };"),
            "(block (; (if c (block (; (call f))))))"
        );
        assert_eq!(body("{ 1 }"), "(block (block 1))");
        assert_eq!(body("{ } x"), "(block (stmt (block)) x)");
    }

    #[test]
    fn let_and_assignment() {
        assert_eq!(body("let mut x: i32 = 5;"), "(block (let mut x i32 5))");
        assert_eq!(body("let p = q;"), "(block (let p q))");
        assert_eq!(
            body("x = 1; x += 1; x -= 1; x *= 1; x /= 1; pts[i].x = 2.0;"),
            "(block (= x 1) (+= x 1) (-= x 1) (*= x 1) (/= x 1) (= (. (index pts i) x) 2.0))"
        );
    }

    #[test]
    fn loops_and_jumps() {
        assert_eq!(
            body("while i < n { i += 1; }"),
            "(block (while (< i n) (block (+= i 1))))"
        );
        assert_eq!(
            body("for i in 0..n { }"),
            "(block (for i (range 0 n) (block)))"
        );
        assert_eq!(
            body("for (i in 0..n + 1) { }"),
            "(block (for i (range 0 (+ n 1)) (block)))"
        );
        assert_eq!(body("for x of xs { }"), "(block (for x (of xs) (block)))");
        assert_eq!(
            body("return; return x; break; continue;"),
            "(block (return) (return x) (break) (continue))"
        );
    }

    #[test]
    fn optional_condition_parentheses() {
        assert_eq!(body("if (x > 0) { }"), body("if x > 0 { }"));
        assert_eq!(body("while (x) { }"), body("while x { }"));
    }

    #[test]
    fn unexpected_tokens_are_e0201() {
        assert_eq!(
            errors("fun t() { let x = 1 let y = 2; }"),
            [("E0201", "let")]
        );
        assert_eq!(errors("fun t() { let x = ); }"), [("E0201", ")")]);
        assert_eq!(errors("fun t() { let x = 1;; }"), [("E0201", ";")]);
    }

    #[test]
    fn unclosed_delimiters_at_end_of_file_are_e0202() {
        assert_eq!(
            errors("fun main() { (1 + 2"),
            [("E0202", "("), ("E0202", "{")]
        );
    }

    #[test]
    fn errors_recover_at_statement_boundaries() {
        let src =
            "fun a() {\n    let x = 1 +;\n    let y = (2;\n    foo(;\n}\nfun b() { let ok = 1; }";
        let codes: Vec<_> = errors(src).into_iter().map(|(code, _)| code).collect();
        assert_eq!(codes, ["E0201", "E0201", "E0201"]);
    }

    #[test]
    fn compound_assignment_records_its_operator_span() {
        let src = "fun t() { x += 1; }";
        let p = crate::parser::test_util::parse_src(src).unwrap();
        let crate::ast::Item::Fun(f) = &p.items[0] else {
            unreachable!()
        };
        let crate::ast::StmtKind::Assign { op_span, .. } = &f.body.stmts[0].kind else {
            panic!()
        };
        assert_eq!(&src[op_span.start..op_span.end], "+=");
    }
}
