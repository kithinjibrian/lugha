//! Expressions: one space around binary operators, parentheses only where
//! precedence needs them (the parser's levels, `parser/expr.rs`), lists
//! that break when too long, literals copied from the source.

use super::Printer;
use super::doc::{Doc, concat, list, spaced, text};
use crate::ast::{Arm, BinOp, Block, Expr, ExprKind, SelectArm, SelectKind, UnOp, spawned_call};

/// Binding strength: a closure's `= body` takes everything after it.
const CLOSURE: u8 = 0;
/// `as`, above every binary operator (1–10).
const CAST: u8 = 11;
const PREFIX: u8 = 12;
const POSTFIX: u8 = 13;
const ATOM: u8 = 14;

fn strength(e: &Expr) -> u8 {
    match &e.kind {
        ExprKind::Closure(_) => CLOSURE,
        ExprKind::Binary(op, ..) => op.level(),
        ExprKind::Cast(..) => CAST,
        // `spawn` binds like a prefix operator (v17).
        ExprKind::Unary(..) | ExprKind::Spawn(_) => PREFIX,
        ExprKind::Call(..) | ExprKind::Index(..) | ExprKind::Field(..) | ExprKind::Try(_) => {
            POSTFIX
        }
        _ => ATOM,
    }
}

impl Printer<'_> {
    /// An expression where a whole one stands: a value, an argument.
    pub(super) fn expr(&mut self, e: &Expr) -> Doc {
        match &e.kind {
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Char(_)
            | ExprKind::Str(_)
            | ExprKind::Bool(_)
            | ExprKind::Name(_)
            | ExprKind::Interp(_) => text(self.text(e.span)),
            ExprKind::Format(call) => {
                let mut items = vec![text(self.text(call.template_span))];
                items.extend(self.exprs(&call.args));
                concat(vec![text("format"), list("(", items, ")")])
            }
            ExprKind::Tuple(elements) => {
                let items = self.exprs(elements);
                list("(", items, ")")
            }
            ExprKind::Array(elements) => {
                let items = self.exprs(elements);
                list("[", items, "]")
            }
            ExprKind::Repeat(value, count) => {
                let (v, n) = self.allowing(|p| (p.expr(value), p.expr(count)));
                concat(vec![text("["), v, text("; "), n, text("]")])
            }
            ExprKind::Try(inner) => concat(vec![self.operand(inner, POSTFIX), text("?")]),
            ExprKind::Unary(op, inner) => {
                let symbol = match op {
                    UnOp::Neg => "-",
                    UnOp::Not => "!",
                    UnOp::BitNot => "~",
                };
                // `- -x` must not print as `--x`.
                let doubled =
                    *op == UnOp::Neg && matches!(inner.kind, ExprKind::Unary(UnOp::Neg, _));
                let operand = if doubled {
                    self.parenthesized(inner)
                } else {
                    self.operand(inner, PREFIX)
                };
                concat(vec![text(symbol), operand])
            }
            ExprKind::Binary(op, _, lhs, rhs) => self.binary(*op, lhs, rhs),
            ExprKind::Cast(inner, ty) => {
                let ty = self.ty(ty);
                concat(vec![self.operand(inner, CAST), text(" as "), text(ty)])
            }
            ExprKind::Call(callee, args) => {
                let callee = self.operand(callee, POSTFIX);
                let args = self.exprs(args);
                concat(vec![callee, list("(", args, ")")])
            }
            ExprKind::Index(base, _, index) => {
                let base = self.operand(base, POSTFIX);
                let index = self.allowing(|p| p.expr(index));
                concat(vec![base, text("["), index, text("]")])
            }
            ExprKind::Field(base, member) => {
                let base = self.operand(base, POSTFIX);
                concat(vec![base, text("."), text(self.ident(member))])
            }
            ExprKind::StructLit(lit) => {
                let mut name = match &lit.module {
                    Some(m) => format!("{}.", self.ident(m)),
                    None => String::new(),
                };
                name.push_str(&self.ident(&lit.name));
                let fields: Vec<Doc> = self.allowing(|p| {
                    let mut docs: Vec<Doc> = lit
                        .fields
                        .iter()
                        .map(|(f, v)| concat(vec![text(format!("{}: ", p.ident(f))), p.expr(v)]))
                        .collect();
                    // `..base` last (v17).
                    if let Some(b) = &lit.base {
                        docs.push(concat(vec![text(".."), p.expr(b)]));
                    }
                    docs
                });
                let doc = match fields.is_empty() {
                    true => text(format!("{name} {{}}")),
                    false => concat(vec![text(format!("{name} ")), spaced("{", fields, "}")]),
                };
                // In a condition a literal needs parentheses (E0205).
                match self.no_struct {
                    true => concat(vec![text("("), doc, text(")")]),
                    false => doc,
                }
            }
            ExprKind::Closure(c) => {
                let params: Vec<Doc> = c
                    .params
                    .iter()
                    .map(|(name, ty)| match ty {
                        Some(t) => text(format!("{}: {}", self.ident(name), self.ty(t))),
                        None => text(self.ident(name)),
                    })
                    .collect();
                let head = concat(vec![
                    text("fun"),
                    list("(", params, ")"),
                    text(self.ret(&c.ret)),
                ]);
                concat(vec![head, self.body(&c.body)])
            }
            ExprKind::If { cond, then, else_ } => {
                let head = concat(vec![text("if "), self.cond(cond), text(" ")]);
                self.if_chain(head, then, else_.as_deref())
            }
            ExprKind::IfLet(parts) => {
                let head = concat(vec![
                    text(format!("if let {} = ", self.pattern(&parts.pattern))),
                    self.cond(&parts.scrutinee),
                    text(" "),
                ]);
                self.if_chain(head, &parts.then, parts.else_.as_deref())
            }
            ExprKind::Block(b) => self.block(b, 0),
            ExprKind::Match(scrutinee, arms) => self.matching(e, scrutinee, arms),
            ExprKind::Spawn(closure) => {
                concat(vec![text("spawn "), self.expr(spawned_call(closure))])
            }
            ExprKind::Select(arms) => self.selecting(e, arms),
        }
    }

    /// Each of `es` as a whole expression, where struct literals are allowed.
    fn exprs(&mut self, es: &[Expr]) -> Vec<Doc> {
        self.allowing(|p| es.iter().map(|e| p.expr(e)).collect())
    }

    /// Runs `f` where struct literals need no parentheses: inside
    /// brackets, parentheses and blocks.
    pub(super) fn allowing<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let saved = std::mem::replace(&mut self.no_struct, false);
        let out = f(self);
        self.no_struct = saved;
        out
    }

    /// A condition, `for` head or `match` scrutinee: a struct literal
    /// there is printed in parentheses.
    pub(super) fn cond(&mut self, e: &Expr) -> Doc {
        let saved = std::mem::replace(&mut self.no_struct, true);
        let out = self.expr(e);
        self.no_struct = saved;
        out
    }

    /// `e` where an operand binding at least `min` is needed.
    fn operand(&mut self, e: &Expr, min: u8) -> Doc {
        // A closure with a block body ends at its `}`, so it is called
        // without parentheses: `spawn fun() { … }()` (v17).
        let ends_itself = match &e.kind {
            ExprKind::Closure(c) => !self.src[..c.body.span.start].trim_end().ends_with('='),
            _ => false,
        };
        if (strength(e) < min && !ends_itself) || self.keeps_parentheses(e) {
            self.parenthesized(e)
        } else {
            self.expr(e)
        }
    }

    fn parenthesized(&mut self, e: &Expr) -> Doc {
        let inner = self.allowing(|p| p.expr(e));
        concat(vec![text("("), inner, text(")")])
    }

    /// A block-like expression or struct literal written in parentheses
    /// as an operand keeps them: without them a statement could start
    /// with it and read differently.
    fn keeps_parentheses(&self, e: &Expr) -> bool {
        let block_like = matches!(
            e.kind,
            ExprKind::If { .. }
                | ExprKind::IfLet(_)
                | ExprKind::Block(_)
                | ExprKind::Match(..)
                | ExprKind::Select(_)
                | ExprKind::StructLit(_)
        );
        let before = self.src[..e.span.start].trim_end();
        let after = self.src[e.span.end..].trim_start();
        block_like && before.ends_with('(') && after.starts_with(')')
    }

    fn binary(&mut self, op: BinOp, lhs: &Expr, rhs: &Expr) -> Doc {
        let level = op.level();
        // Equality and comparison don't chain (E0203): `(a == b) == c`.
        let left_min = if matches!(level, 3 | 4) {
            level + 1
        } else {
            level
        };
        // `(x as T) < y`: after `as`, `T<` would read as type arguments.
        let cast_lt = matches!(lhs.kind, ExprKind::Cast(..)) && op.symbol().starts_with('<');
        let left = if cast_lt {
            self.parenthesized(lhs)
        } else {
            self.operand(lhs, left_min)
        };
        let right = self.operand(rhs, level + 1);
        concat(vec![left, text(format!(" {} ", op.symbol())), right])
    }

    /// `head { then } else …`, `else if` chains on the closing line.
    fn if_chain(&mut self, head: Doc, then: &Block, else_: Option<&Expr>) -> Doc {
        let mut parts = vec![head, self.block(then, 0)];
        if let Some(otherwise) = else_ {
            parts.push(text(" else "));
            parts.push(self.allowing(|p| p.expr(otherwise)));
        }
        concat(parts)
    }

    /// A function or closure body: ` = e` as written that way, else the block.
    pub(super) fn body(&mut self, body: &Block) -> Doc {
        let before = self.src[..body.span.start].trim_end();
        match (&body.tail, before.ends_with('=')) {
            (Some(tail), true) if body.stmts.is_empty() => {
                concat(vec![text(" = "), self.allowing(|p| p.expr(tail))])
            }
            _ => concat(vec![text(" "), self.block(body, 0)]),
        }
    }

    /// `match s { arms }`, one arm per line; a comma after each arm but
    /// one whose body is a block — unless the next arm starts with `(` or
    /// `-`, which would continue the block as a call or a subtraction
    /// (`{ 1 } (a, _)`), so the output wouldn't parse back.
    fn matching(&mut self, whole: &Expr, scrutinee: &Expr, arms: &[Arm]) -> Doc {
        let head = concat(vec![text("match "), self.cond(scrutinee), text(" ")]);
        let open = self.after_brace(scrutinee.span.end, whole.span.end);
        let inner = crate::span::Span::new(open, whole.span.end - 1);
        let seps: Vec<&'static str> = arms
            .iter()
            .enumerate()
            .map(|(i, a)| {
                let continues = arms
                    .get(i + 1)
                    .map(|next| self.pattern(&next.pattern))
                    .is_some_and(|p| p.starts_with(['(', '-']));
                match a.body.kind {
                    ExprKind::Block(_) if !continues => "",
                    _ => ",",
                }
            })
            .collect();
        let arms = self.allowing(|p| {
            p.sequence(
                inner,
                arms,
                |_, a| a.span,
                |a| {
                    let i = arms
                        .iter()
                        .position(|x| std::ptr::eq(x, a))
                        .expect("an arm of this match");
                    seps[i]
                },
                |p, a| p.arm(a),
            )
        });
        concat(vec![head, self.braced(arms)])
    }

    fn arm(&mut self, a: &Arm) -> Doc {
        let mut parts = vec![text(self.pattern(&a.pattern))];
        if let Some(guard) = &a.guard {
            parts.push(text(" if "));
            parts.push(self.cond(guard));
        }
        parts.push(text(" => "));
        parts.push(self.expr(&a.body));
        concat(parts)
    }

    /// `select { arms }` (v17), one arm per line, commas as in `match`.
    fn selecting(&mut self, whole: &Expr, arms: &[SelectArm]) -> Doc {
        let open = self.after_brace(whole.span.start, whole.span.end);
        let inner = crate::span::Span::new(open, whole.span.end - 1);
        let arms = self.allowing(|p| {
            p.sequence(
                inner,
                arms,
                |_, a| a.span,
                |a| match a.body.kind {
                    ExprKind::Block(_) => "",
                    _ => ",",
                },
                |p, a| p.select_arm(a),
            )
        });
        concat(vec![text("select "), self.braced(arms)])
    }

    fn select_arm(&mut self, a: &SelectArm) -> Doc {
        let head = match &a.kind {
            SelectKind::Recv { binding, call } => concat(vec![
                text(format!("{} = ", self.ident(binding))),
                self.expr(call),
            ]),
            SelectKind::Send(call) => self.expr(call),
            SelectKind::After(ms) => concat(vec![text("after("), self.expr(ms), text(")")]),
            SelectKind::Default(_) => text("default"),
        };
        concat(vec![head, text(" => "), self.expr(&a.body)])
    }
}
