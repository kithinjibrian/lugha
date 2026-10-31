//! Blocks and statements: one statement per line, `{` on the line of what
//! opens it, blank lines kept (at most one) but none at a block's edges.

use super::Printer;
use super::doc::{Doc, concat, text};
use crate::ast::{Block, Expr, ExprKind, ForIter, Stmt, StmtKind};
use crate::span::Span;

/// A statement or a block's final expression.
enum Part<'b> {
    Stmt(&'b Stmt),
    Tail(&'b Expr),
}

impl Printer<'_> {
    /// `{ … }`, without its first `skip` statements (a destructuring
    /// loop's hidden one).
    pub(super) fn block(&mut self, b: &Block, skip: usize) -> Doc {
        let mut parts: Vec<Part> = b.stmts.iter().skip(skip).map(Part::Stmt).collect();
        parts.extend(b.tail.as_deref().map(Part::Tail));
        let inner = Span::new(
            b.span.start + 1,
            b.span.end.saturating_sub(1).max(b.span.start + 1),
        );
        let body = self.allowing(|p| {
            p.sequence(
                inner,
                &parts,
                |_, part| match part {
                    Part::Stmt(s) => s.span,
                    Part::Tail(e) => e.span,
                },
                |_| "",
                |p, part| match part {
                    Part::Stmt(s) => p.stmt(s),
                    Part::Tail(e) => p.expr(e),
                },
            )
        });
        self.braced(body)
    }

    fn stmt(&mut self, s: &Stmt) -> Doc {
        match &s.kind {
            StmtKind::Let {
                mutable,
                name,
                ty,
                init,
            } => {
                let mut head = format!(
                    "let {}{}",
                    if *mutable { "mut " } else { "" },
                    self.ident(name)
                );
                if let Some(t) = ty {
                    head.push_str(&format!(": {}", self.ty(t)));
                }
                head.push_str(" = ");
                concat(vec![text(head), self.expr(init), text(";")])
            }
            StmtKind::Assign {
                op, place, value, ..
            } => concat(vec![
                self.expr(place),
                text(format!(" {} ", op.symbol())),
                self.expr(value),
                text(";"),
            ]),
            StmtKind::Expr { expr, semicolon } => {
                let doc = self.expr(expr);
                match semicolon {
                    true => concat(vec![doc, text(";")]),
                    false => doc,
                }
            }
            StmtKind::Defer(expr) => {
                // A block-like body takes no `;`, as a block statement doesn't.
                let block_like = matches!(
                    expr.kind,
                    ExprKind::Block(_)
                        | ExprKind::If { .. }
                        | ExprKind::IfLet(_)
                        | ExprKind::Match(..)
                        | ExprKind::Select(_)
                );
                let mut parts = vec![text("defer "), self.expr(expr)];
                if !block_like {
                    parts.push(text(";"));
                }
                concat(parts)
            }
            StmtKind::While { cond, body } => concat(vec![
                text("while "),
                self.cond(cond),
                text(" "),
                self.block(body, 0),
            ]),
            StmtKind::WhileLet {
                pattern,
                scrutinee,
                body,
            } => concat(vec![
                text(format!("while let {} = ", self.pattern(pattern))),
                self.cond(scrutinee),
                text(" "),
                self.block(body, 0),
            ]),
            StmtKind::LetElse {
                pattern,
                init,
                else_,
            } => {
                let mut parts = vec![
                    text(format!("let {} = ", self.pattern(pattern))),
                    self.expr(init),
                ];
                if let Some(otherwise) = else_ {
                    parts.push(text(" else "));
                    parts.push(self.block(otherwise, 0));
                }
                parts.push(text(";"));
                concat(parts)
            }
            StmtKind::For { var, iter, body } => {
                let (head, skip) = match destructured(var.name.as_str(), body) {
                    Some(pattern) => (self.pattern(pattern), 1),
                    None => (self.ident(var), 0),
                };
                let mut parts = vec![text(format!("for {head} "))];
                match iter {
                    ForIter::Range(from, to) => {
                        parts.push(text("in "));
                        parts.push(self.cond(from));
                        parts.push(text(".."));
                        parts.push(self.cond(to));
                    }
                    ForIter::Array(xs) => {
                        parts.push(text("of "));
                        parts.push(self.cond(xs));
                    }
                    ForIter::MutOf(xs) => {
                        parts.push(text("of mut "));
                        parts.push(self.cond(xs));
                    }
                }
                parts.push(text(" "));
                parts.push(self.block(body, skip));
                concat(parts)
            }
            StmtKind::Return(None) => text("return;"),
            StmtKind::Return(Some(value)) => {
                concat(vec![text("return "), self.expr(value), text(";")])
            }
            StmtKind::Break => text("break;"),
            StmtKind::Continue => text("continue;"),
        }
    }
}

/// The pattern of `for (i, w) of …`: the parser binds a hidden `$item…`
/// and destructures it in a first statement (`parser/tuples.rs`).
fn destructured<'b>(var: &str, body: &'b Block) -> Option<&'b crate::ast::Pattern> {
    if !var.starts_with("$item") {
        return None;
    }
    match body.stmts.first().map(|s| &s.kind) {
        Some(StmtKind::LetElse {
            pattern,
            init,
            else_: None,
        }) if matches!(&init.kind, ExprKind::Name(n) if n == var) => Some(pattern),
        _ => None,
    }
}
