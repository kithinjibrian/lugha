//! Move on last use (PRP-018): store sites whose value is a place rooted in
//! a `let` local that is never mentioned again move instead of copying.
//!
//! The rule is deliberately simple and errs towards copying. A value `E` moves
//! when it is a place chain rooted in a `let` local `b` and:
//! - no mention of `b` starts after `E` ends (source order is evaluation
//!   order, spec §5);
//! - every loop enclosing `E` also encloses `b`'s declaration, so no later
//!   iteration can reach `b` again.
//!
//! Or (v17, milestone 149) when `E` is inside the value of `b = …`, with no
//! mention of `b` after it in that value: the assignment overwrites `b`
//! before anything reads it again, loops or not.
//!
//! Handing over arguments (milestone 149, PRP-180): a call argument on its
//! last use is *handed over*; a function *keeps* a parameter when it stores
//! it (or hands it to a keeping parameter) on its last use. Codegen compiles
//! a keeping function a second time, owning its kept parameters, and only
//! direct calls reach that instance. Closures' and `for … of` variables
//! are never moved from.

use std::collections::{HashMap, HashSet};

mod walk;

use walk::Walk;

use super::Binding;
use super::modules::{Unit, qualify};
use crate::ast::{Expr, ExprId, ExprKind, FunDecl, Item};
use crate::span::Span;

/// A `Name` expression that resolved to a local.
#[derive(Debug, Clone, Copy)]
pub(super) struct Mention {
    /// The binding's declaration, unique per binding.
    pub decl: Span,
    pub binding: Binding,
    /// Where the mention starts.
    pub at: usize,
}

/// What liveness found, for `Checked`.
#[derive(Debug, Default)]
pub(super) struct Liveness {
    /// Store sites that move a dead `let` local's value.
    pub moves: HashSet<ExprId>,
    /// Store sites that move a kept parameter: only in the owning instance.
    pub param_moves: HashSet<ExprId>,
    /// Call arguments (and receivers) rooted in a dead `let` local.
    pub hands: HashSet<ExprId>,
    /// Call arguments rooted in a kept parameter on its last use: handed
    /// over only in the owning instance.
    pub param_hands: HashSet<ExprId>,
    /// Per function or method key, which parameters it keeps (receiver
    /// first for a method); only keys with at least one.
    pub keeps: HashMap<String, Vec<bool>>,
}

/// One function's parameters while it is walked.
pub(super) struct Frame {
    /// Each slot's declaration (receiver first); `None` for `mut self`.
    pub params: Vec<Option<Span>>,
    pub kept: Vec<bool>,
    /// Parameter slots handed on: (slot, callee key, callee slot, argument).
    pub passed: Vec<(usize, String, usize, ExprId)>,
}

/// How a value read from a place ends its root's life.
pub(super) enum Dead {
    No,
    /// A `let` local nothing reads again.
    Local,
    /// The current function's parameter in this slot, on its last use.
    Param(usize),
}

/// True if a value of type `ty` may hold arrays: deep (spec §4), or
/// mentioning a type parameter, which some instance may make deep.
pub(super) fn maybe_deep(ty: &crate::check::Type, structs: &crate::check::Structs) -> bool {
    use crate::check::Type;
    match ty {
        Type::Param(_) => true,
        Type::Struct(_, args) | Type::Enum(_, args) | Type::Interface(_, args)
            if args.iter().any(|a| maybe_deep(a, structs)) =>
        {
            true
        }
        _ => ty.is_deep(structs),
    }
}

/// Liveness of every function, method and derive item in `units`.
pub(super) fn analyse(
    units: &[Unit],
    mentions: &HashMap<ExprId, Mention>,
    callees: &HashMap<ExprId, String>,
    deep: &dyn Fn(ExprId) -> bool,
) -> Liveness {
    let mut positions: HashMap<Span, Vec<usize>> = HashMap::new();
    for mention in mentions.values() {
        positions.entry(mention.decl).or_default().push(mention.at);
    }
    for list in positions.values_mut() {
        list.sort_unstable();
    }
    let mut walk = Walk {
        mentions,
        positions,
        callees,
        deep,
        loops: Vec::new(),
        closures: 0,
        overwrites: Vec::new(),
        frame: None,
        block_ends: Vec::new(),
        out: Liveness::default(),
    };
    let mut frames: Vec<(String, Frame)> = Vec::new();
    for unit in units {
        for item in &unit.program.items {
            match item {
                Item::Fun(f) => {
                    frames.push((qualify(unit.path, &f.name.name), walk.function(f)));
                }
                Item::Impl(i) => {
                    let owner = qualify(unit.path, &i.ty.name);
                    for f in &i.methods {
                        frames.push((format!("{owner}.{}", f.name.name), walk.function(f)));
                    }
                }
                // Derive items are reached through bounds and blocks: they
                // move their own locals, but never keep a parameter.
                Item::Derive(d) => d.methods.iter().for_each(|f| {
                    walk.function(f);
                }),
                _ => {}
            }
        }
    }
    let mut out = walk.out;
    // Keeping spreads backwards through handed-on parameters, to a fixed point.
    let mut keeps: HashMap<String, Vec<bool>> = frames
        .iter()
        .map(|(k, f)| (k.clone(), f.kept.clone()))
        .collect();
    loop {
        let mut changed = false;
        for (key, frame) in &frames {
            for (slot, callee, at, _) in &frame.passed {
                let kept = keeps
                    .get(callee)
                    .is_some_and(|k| k.get(*at).copied().unwrap_or(false));
                let mine = keeps.get_mut(key).expect("every frame has an entry");
                if kept && !mine[*slot] {
                    mine[*slot] = true;
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }
    // A parameter handed on counts only if its function keeps it (and so
    // owns it in its owning instance).
    for (key, frame) in &frames {
        for (slot, _, _, arg) in &frame.passed {
            if !keeps[key][*slot] {
                out.param_hands.remove(arg);
            }
        }
    }
    keeps.retain(|_, k| k.iter().any(|&b| b));
    out.keeps = keeps;
    out
}

impl Walk<'_> {
    /// Walks one function's body with its parameters in scope.
    fn function(&mut self, f: &FunDecl) -> Frame {
        let mut params: Vec<Option<Span>> = f
            .receiver
            .iter()
            .map(|r| (!r.mutable).then_some(r.span))
            .collect();
        params.extend(f.params.iter().map(|p| Some(p.name.span)));
        let kept = vec![false; params.len()];
        self.frame = Some(Frame {
            params,
            kept,
            passed: Vec::new(),
        });
        self.block(&f.body);
        // The body's result is stored by the caller, as `return` values are.
        if let Some(tail) = &f.body.tail {
            self.result(tail);
        }
        self.frame.take().expect("set above")
    }

    /// How reading `value` ends its root's life, if it does.
    pub(super) fn dead_after(&self, value: &Expr) -> Dead {
        let Some(root) = root(value) else {
            return Dead::No;
        };
        let Some(mention) = self.mentions.get(&root.id) else {
            return Dead::No;
        };
        let later = |end: usize| {
            self.positions
                .get(&mention.decl)
                .is_some_and(|ps| ps.last().is_some_and(|&last| last >= end))
        };
        match mention.binding {
            Binding::Let { .. } => {
                // `b = f(b)`: overwritten before anything reads it again.
                if let Some(&(decl, end, depth)) = self.overwrites.last()
                    && decl == mention.decl
                    && depth == self.closures
                    && !self.positions[&decl]
                        .iter()
                        .any(|&p| p >= value.span.end && p < end)
                {
                    return Dead::Local;
                }
                let in_every_loop = self
                    .loops
                    .iter()
                    .all(|l| l.start <= mention.decl.start && mention.decl.end <= l.end);
                if in_every_loop && !later(value.span.end) {
                    Dead::Local
                } else {
                    Dead::No
                }
            }
            // Inside a closure a parameter is a capture, never moved.
            Binding::Param
                if self.closures == 0 && self.loops.is_empty() && (self.deep)(value.id) =>
            {
                let slot = self
                    .frame
                    .as_ref()
                    .and_then(|f| f.params.iter().position(|p| *p == Some(mention.decl)));
                match slot {
                    Some(slot) if !later(value.span.end) => Dead::Param(slot),
                    _ => Dead::No,
                }
            }
            _ => Dead::No,
        }
    }
}

/// The `Name` at the root of a place chain (`p`, `w.data`, `g[i][j]`).
fn root(expr: &Expr) -> Option<&Expr> {
    match &expr.kind {
        ExprKind::Name(_) => Some(expr),
        ExprKind::Field(base, _) | ExprKind::Index(base, _, _) => root(base),
        _ => None,
    }
}

#[cfg(test)]
mod tests;
