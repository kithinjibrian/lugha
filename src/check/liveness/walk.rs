//! The walk behind move on last use: every store site of a body, in
//! evaluation order, with the loops around it (`mod.rs` decides each move).

use std::collections::{HashMap, HashSet};

use super::Mention;
use crate::ast::{Block, Expr, ExprId, ExprKind, ForIter, Part, Stmt, StmtKind};
use crate::span::Span;

pub(super) struct Walk<'a> {
    pub(super) mentions: &'a HashMap<ExprId, Mention>,
    /// The latest mention of each binding.
    pub(super) last: HashMap<Span, usize>,
    /// The loops enclosing the current point.
    pub(super) loops: Vec<Span>,
    pub(super) moves: HashSet<ExprId>,
}

impl Walk<'_> {
    pub(super) fn block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.stmt(stmt);
        }
        if let Some(tail) = &block.tail {
            self.expr(tail);
        }
    }

    fn stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Let { init, .. } => self.store(init),
            StmtKind::Assign { place, value, .. } => {
                self.expr(place);
                self.store(value);
            }
            StmtKind::Expr { expr, .. } => self.expr(expr),
            StmtKind::Return(value) => value.iter().for_each(|v| self.expr(v)),
            StmtKind::While { cond, body } => {
                self.loops.push(stmt.span);
                self.expr(cond);
                self.block(body);
                self.loops.pop();
            }
            StmtKind::For { iter, body, .. } => {
                // The bounds and the iterated array are evaluated once, but
                // counting them as inside the loop only errs towards copying.
                self.loops.push(stmt.span);
                match iter {
                    ForIter::Range(start, end) => {
                        self.expr(start);
                        self.expr(end);
                    }
                    ForIter::Array(xs) => self.expr(xs),
                }
                self.block(body);
                self.loops.pop();
            }
            StmtKind::Break | StmtKind::Continue => {}
        }
    }

    /// A value stored into a new place: a move if it can be (spec §4 copy sites).
    fn store(&mut self, value: &Expr) {
        if self.dead_after(value) {
            self.moves.insert(value.id);
        }
        self.expr(value);
    }

    fn expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Array(elements) => elements.iter().for_each(|e| self.store(e)),
            ExprKind::StructLit(lit) => lit.fields.iter().for_each(|(_, e)| self.store(e)),
            ExprKind::Unary(_, e) | ExprKind::Cast(e, _) | ExprKind::Field(e, _) => self.expr(e),
            ExprKind::Binary(_, _, a, b) | ExprKind::Index(a, _, b) | ExprKind::Repeat(a, b) => {
                self.expr(a);
                self.expr(b);
            }
            ExprKind::Call(callee, args) => {
                self.expr(callee);
                // `xs.push(v)` stores `v` (v1 §4); for a user `push` the mark is unused.
                let push = matches!(&callee.kind, ExprKind::Field(_, m) if m.name == "push");
                for a in args {
                    if push { self.store(a) } else { self.expr(a) }
                }
            }
            ExprKind::If { cond, then, else_ } => {
                self.expr(cond);
                self.block(then);
                else_.iter().for_each(|e| self.expr(e));
            }
            ExprKind::Block(block) => self.block(block),
            // A closure's own lets can move; captures never do (`Binding::Param`).
            ExprKind::Closure(c) => self.block(&c.body),
            ExprKind::Try(e) => self.expr(e),
            ExprKind::Match(scrutinee, arms) => {
                self.expr(scrutinee);
                arms.iter().for_each(|arm| self.expr(&arm.body));
            }
            // Interpolated values are read, like any other mention (v3).
            ExprKind::Interp(parts) => parts
                .iter()
                .filter_map(Part::expr)
                .for_each(|e| self.expr(e)),
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Str(_)
            | ExprKind::Bool(_)
            | ExprKind::Name(_) => {}
        }
    }
}
