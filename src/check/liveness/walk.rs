//! The walk behind move on last use: every store site of a body, in
//! evaluation order, with the loops around it (`mod.rs` decides each move).

use std::collections::HashMap;

use super::{Dead, Frame, Liveness, Mention};
use crate::ast::{Block, Expr, ExprId, ExprKind, ForIter, Part, SelectKind, Stmt, StmtKind};
use crate::span::Span;

pub(super) struct Walk<'a> {
    pub(super) mentions: &'a HashMap<ExprId, Mention>,
    /// Where each binding is mentioned, in order.
    pub(super) positions: HashMap<Span, Vec<usize>>,
    /// The function each call calls, for keeping through hand-ons.
    pub(super) callees: &'a HashMap<ExprId, String>,
    /// Whether a value may hold arrays: only those make a parameter kept.
    pub(super) deep: &'a dyn Fn(ExprId) -> bool,
    /// The loops enclosing the current point.
    pub(super) loops: Vec<Span>,
    /// How many closures enclose the current point.
    pub(super) closures: usize,
    /// Enclosing `b = …` statements: `b`'s declaration, where the value
    /// ends, and the closure depth of the statement.
    pub(super) overwrites: Vec<(Span, usize, usize)>,
    /// The function being walked.
    pub(super) frame: Option<Frame>,
    pub(super) out: Liveness,
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
            StmtKind::Assign {
                place, value, op, ..
            } => {
                self.expr(place);
                // `b = …` overwrites `b` whole (milestone 149).
                let whole = match (&place.kind, op) {
                    (ExprKind::Name(_), crate::ast::AssignOp::Assign) => {
                        self.mentions.get(&place.id)
                    }
                    _ => None,
                };
                if let Some(m) = whole {
                    self.overwrites
                        .push((m.decl, value.span.end, self.closures));
                }
                self.store(value);
                if whole.is_some() {
                    self.overwrites.pop();
                }
            }
            StmtKind::Expr { expr, .. } => self.expr(expr),
            StmtKind::Return(value) => value.iter().for_each(|v| self.result(v)),
            StmtKind::While { cond, body } => {
                self.loops.push(stmt.span);
                self.expr(cond);
                self.block(body);
                self.loops.pop();
            }
            // v9: the scrutinee is read, as a `match`'s is.
            StmtKind::WhileLet {
                scrutinee, body, ..
            } => {
                self.loops.push(stmt.span);
                self.expr(scrutinee);
                self.block(body);
                self.loops.pop();
            }
            StmtKind::LetElse { init, else_, .. } => {
                self.expr(init);
                if let Some(e) = else_ {
                    self.block(e);
                }
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
                    ForIter::Array(xs) | ForIter::MutOf(xs) => self.expr(xs),
                }
                self.block(body);
                self.loops.pop();
            }
            StmtKind::Break | StmtKind::Continue => {}
        }
    }

    /// A value stored into a new place: a move if it can be (spec §4 copy sites).
    fn store(&mut self, value: &Expr) {
        match self.dead_after(value) {
            Dead::Local => {
                self.out.moves.insert(value.id);
            }
            Dead::Param(slot) => {
                self.out.param_moves.insert(value.id);
                self.frame.as_mut().expect("parameters are a frame's").kept[slot] = true;
            }
            Dead::No => {}
        }
        self.expr(value);
    }

    /// A function's result: stored by its caller. Only a parameter can
    /// move here (an owned local's result already moves, spec §4).
    pub(super) fn result(&mut self, value: &Expr) {
        if self.closures == 0
            && let Dead::Param(slot) = self.dead_after(value)
        {
            self.out.param_moves.insert(value.id);
            self.frame.as_mut().expect("parameters are a frame's").kept[slot] = true;
        }
        self.expr(value);
    }

    /// An argument (or receiver, `slot` 0 of a method) of `call`: handed
    /// over on its root's last use (milestone 149).
    fn argument(&mut self, call: &Expr, slot: usize, value: &Expr) {
        match self.dead_after(value) {
            Dead::Local => {
                self.out.hands.insert(value.id);
            }
            Dead::Param(mine) => {
                self.out.param_hands.insert(value.id);
                let callee = self.callees.get(&call.id).cloned().unwrap_or_default();
                let frame = self.frame.as_mut().expect("parameters are a frame's");
                frame.passed.push((mine, callee, slot, value.id));
            }
            Dead::No => {}
        }
        self.expr(value);
    }

    fn expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Array(elements) | ExprKind::Tuple(elements) => {
                elements.iter().for_each(|e| self.store(e))
            }
            // `out place` (v17): the place C fills, written after the call.
            ExprKind::Pass(_, place) => self.expr(place),
            // `#{ … }` (v17): its keys' and values' arrays, stored like elements.
            ExprKind::Hashed(lit) => {
                self.expr(&lit.keys);
                self.expr(&lit.values);
            }
            ExprKind::StructLit(lit) => {
                lit.fields.iter().for_each(|(_, e)| self.store(e));
                // Its unwritten fields are stored too: copied from a place, moved on its last use (v17).
                lit.base.iter().for_each(|e| self.store(e));
            }
            ExprKind::Unary(_, e) | ExprKind::Cast(e, _) | ExprKind::Field(e, _) => self.expr(e),
            ExprKind::Binary(_, _, a, b) | ExprKind::Index(a, _, b) | ExprKind::Repeat(a, b) => {
                self.expr(a);
                self.expr(b);
            }
            ExprKind::Call(callee, args) => {
                // `xs.push(v)` stores `v` (v1 §4); for a user `push` the mark is unused.
                let push = matches!(&callee.kind, ExprKind::Field(_, m) if m.name == "push");
                // A receiver is a method's slot 0; a module or type name has no mention.
                let receiver = match &callee.kind {
                    ExprKind::Field(base, _) if !push => Some(base),
                    _ => None,
                };
                match receiver {
                    Some(base) => self.argument(expr, 0, base),
                    None => self.expr(callee),
                }
                // A method's arguments come after its receiver; `m.f(…)` and
                // `T.f(…)` name a module or type, never a local. A wrong slot
                // could only cost marks: codegen decides from the callee.
                let first = usize::from(receiver.is_some_and(|b| {
                    !matches!(b.kind, ExprKind::Name(_)) || self.mentions.contains_key(&b.id)
                }));
                for (i, a) in args.iter().enumerate() {
                    if push {
                        self.store(a)
                    } else {
                        self.argument(expr, first + i, a)
                    }
                }
            }
            ExprKind::If { cond, then, else_ } => {
                self.expr(cond);
                self.block(then);
                else_.iter().for_each(|e| self.expr(e));
            }
            ExprKind::IfLet(parts) => {
                self.expr(&parts.scrutinee);
                self.block(&parts.then);
                parts.else_.iter().for_each(|e| self.expr(e));
            }
            ExprKind::Block(block) => self.block(block),
            // A closure's own lets can move; captures never do (`Binding::Param`).
            ExprKind::Closure(c) => {
                self.closures += 1;
                self.block(&c.body);
                self.closures -= 1;
            }
            ExprKind::Try(e) => self.expr(e),
            ExprKind::Match(scrutinee, arms) => {
                self.expr(scrutinee);
                arms.iter().for_each(|arm| self.expr(&arm.body));
            }
            // What a green thread runs is a closure (v17).
            ExprKind::Spawn(closure) => self.expr(closure),
            ExprKind::Select(arms) => {
                for arm in arms {
                    match &arm.kind {
                        SelectKind::Recv { call, .. } => self.expr(call),
                        // A sent value is stored in the channel.
                        SelectKind::Send(call) => {
                            if let ExprKind::Call(callee, args) = &call.kind {
                                self.expr(callee);
                                args.iter().for_each(|a| self.store(a));
                            }
                        }
                        SelectKind::After(ms) => self.expr(ms),
                        SelectKind::Default(_) => {}
                    }
                    self.expr(&arm.body);
                }
            }
            // Interpolated values are read, like any other mention (v3).
            ExprKind::Interp(parts) => parts
                .iter()
                .filter_map(Part::expr)
                .for_each(|e| self.expr(e)),
            ExprKind::Format(call) => call.args.iter().for_each(|e| self.expr(e)),
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Char(_)
            | ExprKind::Str(_)
            | ExprKind::Bool(_)
            | ExprKind::Name(_) => {}
        }
    }
}
