//! Green-thread syntax (v17, milestone 91): `spawn call`, and `select { … }`
//! with its four kinds of arm. A `spawn`'s call is wrapped in a closure with
//! no parameters here, so the checker and codegen treat what the new thread
//! runs as any closure body.

use super::{PResult, Parser, Reported};
use crate::ast::{Block, Closure, Expr, ExprKind, Ident, SelectArm, SelectKind};
use crate::diagnostic::Diagnostic;
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    /// `spawn call`, at `spawn`: the call in a closure with no parameters.
    pub(super) fn spawn_expr(&mut self) -> PResult<Expr> {
        let start = self.bump().start;
        let call = self.nested(|p| p.postfix())?;
        if !matches!(call.kind, ExprKind::Call(..)) {
            let message = "`spawn` needs a call";
            let help = "write the call the new thread runs, `spawn work(x)`, or call a \
                        closure on the spot: `spawn fun() { … }()`";
            let d = Diagnostic::error("E0211", message, call.span).with_help(help);
            return Err(self.error(d));
        }
        let span = call.span;
        let body = Block {
            stmts: Vec::new(),
            span,
            tail: Some(Box::new(call)),
        };
        let closure = Closure {
            params: Vec::new(),
            ret: None,
            body,
        };
        // The closure covers exactly the call, so it never claims more
        // source than what was written.
        let closure = self.mk(span.start, ExprKind::Closure(Box::new(closure)));
        Ok(self.mk(start, ExprKind::Spawn(Box::new(closure))))
    }

    /// `select { arm, … }`, at `select`: arms end with `,` unless their
    /// value is a block, as in `match`.
    pub(super) fn select_expr(&mut self) -> PResult<Expr> {
        self.nested(|p| {
            let start = p.bump().start;
            let open = p.expect(&TokenKind::LBrace)?;
            let mut arms = Vec::new();
            loop {
                if p.eat(&TokenKind::RBrace) {
                    break;
                }
                if p.at(&TokenKind::Eof) {
                    p.close(open, &TokenKind::RBrace)?;
                }
                let arm = match p.select_arm() {
                    Ok(arm) => arm,
                    Err(_) => {
                        // One bad arm, one error: skip to the next.
                        p.sync_arm();
                        p.eat(&TokenKind::Comma);
                        continue;
                    }
                };
                let block_body = matches!(arm.body.kind, ExprKind::Block(_));
                arms.push(arm);
                if !p.eat(&TokenKind::Comma) && !block_body {
                    p.close(open, &TokenKind::RBrace)?;
                    break;
                }
            }
            Ok(p.mk(start, ExprKind::Select(arms)))
        })
    }

    fn select_arm(&mut self) -> PResult<SelectArm> {
        let start = self.span().start;
        let next = self.peek_at(1).clone();
        let kind = match self.peek().clone() {
            TokenKind::Ident(word) if word == "default" && next == TokenKind::FatArrow => {
                SelectKind::Default(self.bump())
            }
            TokenKind::Ident(word) if word == "after" && next == TokenKind::LParen => {
                self.bump();
                let open = self.bump();
                let ms = self.expr()?;
                self.close(open, &TokenKind::RParen)?;
                SelectKind::After(ms)
            }
            TokenKind::Ident(name) if next == TokenKind::Eq => {
                let binding = Ident {
                    name,
                    span: self.bump(),
                };
                self.bump();
                let call = self.expr()?;
                if !is_method_call(&call, "recv", 0) {
                    return Err(self.bad_arm(&call));
                }
                SelectKind::Recv { binding, call }
            }
            _ => {
                let call = self.expr()?;
                if !is_method_call(&call, "send", 1) {
                    return Err(self.bad_arm(&call));
                }
                SelectKind::Send(call)
            }
        };
        self.expect(&TokenKind::FatArrow)?;
        let body = self.expr()?;
        let span = Span::new(start, body.span.end);
        Ok(SelectArm { kind, body, span })
    }

    fn bad_arm(&mut self, at: &Expr) -> Reported {
        let message = "a `select` arm waits for `name = c.recv()`, `c.send(value)`, \
                       `after(ms)` or `default`";
        self.error(Diagnostic::error("E0211", message, at.span))
    }
}

/// `base.name(args)` with `count` arguments.
fn is_method_call(call: &Expr, name: &str, count: usize) -> bool {
    match &call.kind {
        ExprKind::Call(callee, args) => {
            args.len() == count
                && matches!(&callee.kind, ExprKind::Field(_, method) if method.name == name)
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{body, errors, expr};

    #[test]
    fn spawn_takes_a_call() {
        assert_eq!(expr("spawn f(1, x)"), "(spawn (call f 1 x))");
        assert_eq!(expr("spawn m.f(x)"), "(spawn (call (. m f) x))");
        assert_eq!(expr("spawn p.area()"), "(spawn (call (. p area)))");
        assert_eq!(
            expr("spawn fun(): i64 { 1 }()"),
            "(spawn (call (closure () i64 (block 1))))"
        );
        // The operand is the whole postfix chain: this spawns `f().g()`.
        assert_eq!(expr("spawn f().g()"), "(spawn (call (. (call f) g)))");
        assert_eq!(
            expr("(spawn f()).join()"),
            "(call (. (spawn (call f)) join))"
        );
        assert_eq!(
            body("let t = spawn work(3);"),
            "(block (let t (spawn (call work 3))))"
        );
    }

    #[test]
    fn spawn_without_a_call_is_e0211() {
        assert_eq!(errors("fun t() { spawn x; }"), [("E0211", "x")]);
        assert_eq!(errors("fun t() { spawn 1 + f(); }"), [("E0211", "1")]);
        assert_eq!(errors("fun t() { spawn f()?; }"), [("E0211", "f()?")]);
        assert_eq!(errors("fun t() { spawn f; }"), [("E0211", "f")]);
    }

    #[test]
    fn select_has_four_kinds_of_arm() {
        assert_eq!(
            expr(
                "select {
                    m = inbox.recv() => m,
                    _ = stop.recv() => { 0 }
                    out.send(42) => 1,
                    after(500) => 2,
                }"
            ),
            "(select (recv m inbox m) (recv _ stop (block 0)) (send out 42 1) (after 500 2))"
        );
        assert_eq!(
            expr("select { x = c.recv() => x, default => None }"),
            "(select (recv x c x) (default None))"
        );
        assert_eq!(
            expr("select { x = chans[0].recv() => x }"),
            "(select (recv x (index chans 0) x))"
        );
    }

    #[test]
    fn select_is_block_like() {
        assert_eq!(
            body("select { default => println(1) } let x = 2;"),
            "(block (stmt (select (default (call println 1)))) (let x 2))"
        );
    }

    #[test]
    fn after_and_default_stay_names_outside_select() {
        assert_eq!(
            body("let after = 1; let default = after(2);"),
            "(block (let after 1) (let default (call after 2)))"
        );
    }

    #[test]
    fn malformed_arms_are_e0211() {
        for (src, at) in [
            ("select { x => 1 }", "x"),
            ("select { m = a.get() => 1 }", "a.get()"),
            ("select { m = a.recv(1) => 1 }", "a.recv(1)"),
            ("select { a.send() => 1 }", "a.send()"),
            ("select { a.send(1, 2) => 1 }", "a.send(1, 2)"),
            ("select { f() => 1 }", "f()"),
            ("select { after => 1 }", "after"),
        ] {
            let src = format!("fun t() {{ {src} }}");
            assert_eq!(errors(&src), [("E0211", at)], "{src}");
        }
    }

    #[test]
    fn one_bad_arm_reports_once_and_the_rest_parse() {
        assert_eq!(
            errors("fun t() { select { x => 1, y => 2 } }"),
            [("E0211", "x"), ("E0211", "y")]
        );
    }
}
