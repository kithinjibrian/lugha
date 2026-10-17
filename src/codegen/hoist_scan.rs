//! The scan behind owning arrays once per loop (`hoist.rs`, v15): what a
//! loop's code does with each variable, and the arrays it writes through.

use std::collections::{HashMap, HashSet};

use super::hoist::Head;
use super::lower::Lowerer;
use crate::ast::{Block, Expr, ExprKind, ForIter, Pattern, PatternKind, SelectKind, StmtKind};
use crate::check::Type;

/// What a loop's code does with each name.
#[derive(Default)]
pub(super) struct Scan {
    /// Roots the loop may share again: never hoisted.
    pub sharing: HashSet<String>,
    /// Names the loop assigns, binds or may change: not fixed as indexes.
    pub changed: HashSet<String>,
    /// Names the loop binds: never roots of a hoisted path.
    pub bound: HashSet<String>,
    /// `for … of mut` variables in the loop, and the root they alias.
    pub aliases: HashMap<String, String>,
    /// The arrays the loop writes through.
    pub paths: Vec<Expr>,
}

/// The variable a place chain (`a.b[i].c`) starts from.
pub(super) fn chain_root(expr: &Expr) -> Option<&str> {
    match &expr.kind {
        ExprKind::Name(name) => Some(name),
        ExprKind::Field(base, _) | ExprKind::Index(base, _, _) => chain_root(base),
        _ => None,
    }
}

/// The arrays a write to `place` goes through: each indexed base.
fn arrays_on(place: &Expr, out: &mut Vec<Expr>) {
    match &place.kind {
        ExprKind::Index(base, _, _) => {
            out.push((**base).clone());
            arrays_on(base, out);
        }
        ExprKind::Field(base, _) => arrays_on(base, out),
        _ => {}
    }
}

/// Every name a pattern may bind (a bare payload name may be a variant;
/// counting it errs towards not hoisting).
fn pattern_names(pattern: &Pattern, out: &mut HashSet<String>) {
    match &pattern.kind {
        PatternKind::Variant(name, subs) => {
            out.insert(name.name.clone());
            subs.iter().flatten().for_each(|p| pattern_names(p, out));
        }
        PatternKind::Type(Some(name), _) => {
            out.insert(name.name.clone());
        }
        PatternKind::Tuple(subs) | PatternKind::Or(subs) => {
            subs.iter().for_each(|p| pattern_names(p, out))
        }
        PatternKind::Mut(inner) => pattern_names(inner, out),
        _ => {}
    }
}

impl<'ctx> Lowerer<'ctx> {
    /// Scans a loop: its heads, then its body.
    pub(super) fn scan_loop(&self, heads: &[Head<'_>], body: &Block) -> Scan {
        let mut scan = Scan::default();
        for head in heads {
            match head {
                Head::Read(e) => self.scan_read(&mut scan, e),
                Head::Binds(name) => scan.bind(name),
                Head::Pattern(p) => scan.bind_pattern(p),
                Head::Alias(var, place) => self.scan_alias(&mut scan, var, place),
                Head::Steps(place) => self.scan_receiver(&mut scan, place),
            }
        }
        self.block_scan(&mut scan, body);
        scan
    }

    /// The variable whose arrays `name` reaches: itself, or for a
    /// `for … of mut` variable, the root of the place it aliases.
    pub(super) fn root_of(&self, scan: &Scan, name: &str) -> String {
        if let Some(root) = scan.aliases.get(name) {
            return self.root_of(scan, root);
        }
        let outer = self
            .scopes
            .find(name)
            .and_then(|l| self.aliases.get(&l.ptr));
        match outer.and_then(|alias| chain_root(&alias.place)) {
            Some(root) if root != name => self.root_of(scan, root),
            _ => name.to_owned(),
        }
    }

    /// Scans a block's statements and tail.
    fn block_scan(&self, scan: &mut Scan, block: &Block) {
        for stmt in &block.stmts {
            match &stmt.kind {
                StmtKind::Let { name, init, .. } => {
                    self.scan_read(scan, init);
                    scan.bind(&name.name);
                }
                // `m[k] = v` on a map (v17) is `m.insert(k, v)`: the map changes.
                StmtKind::Assign { place, value, .. } if matches!(&place.kind, ExprKind::Index(base, _, _) if self.is_map(base)) =>
                {
                    let ExprKind::Index(base, _, key) = &place.kind else {
                        unreachable!("matched just above")
                    };
                    let deep = self.ty(base).is_deep(&self.structs);
                    self.scan_write(scan, base, deep);
                    self.scan_read(scan, key);
                    self.scan_read(scan, value);
                }
                StmtKind::Assign { place, value, .. } => {
                    let deep = self.ty(place).is_deep(&self.structs);
                    self.scan_write(scan, place, deep);
                    self.scan_read(scan, value);
                }
                StmtKind::Expr { expr, .. } => self.scan_read(scan, expr),
                StmtKind::While { cond, body } => {
                    self.scan_read(scan, cond);
                    self.block_scan(scan, body);
                }
                StmtKind::WhileLet {
                    pattern,
                    scrutinee,
                    body,
                } => {
                    scan.bind_pattern(pattern);
                    self.scan_read(scan, scrutinee);
                    self.block_scan(scan, body);
                }
                StmtKind::LetElse {
                    pattern,
                    init,
                    else_,
                } => {
                    scan.bind_pattern(pattern);
                    self.scan_read(scan, init);
                    if let Some(block) = else_ {
                        self.block_scan(scan, block);
                    }
                }
                StmtKind::For { var, iter, body } => {
                    match iter {
                        ForIter::Range(start, end) => {
                            self.scan_read(scan, start);
                            self.scan_read(scan, end);
                        }
                        ForIter::Array(xs) => self.scan_read(scan, xs),
                        ForIter::MutOf(place) => match self.ty(place) {
                            Type::Array(_) => self.scan_alias(scan, &var.name, place),
                            _ => self.scan_receiver(scan, place),
                        },
                    }
                    scan.bind(&var.name);
                    self.block_scan(scan, body);
                }
                StmtKind::Return(value) => {
                    if let Some(value) = value {
                        self.scan_read(scan, value);
                    }
                }
                StmtKind::Break | StmtKind::Continue => {}
            }
        }
        if let Some(tail) = &block.tail {
            self.scan_read(scan, tail);
        }
    }

    /// `expr` evaluated for its value. A place whose value holds an array
    /// may be copied (stored, passed, returned, matched): its root may share.
    fn scan_read(&self, scan: &mut Scan, expr: &Expr) {
        if let Some(root) = chain_root(expr) {
            if self.ty(expr).is_deep(&self.structs) {
                scan.sharing.insert(self.root_of(scan, root));
            }
            self.scan_indexes(scan, expr);
            return;
        }
        match &expr.kind {
            ExprKind::Call(callee, args) => {
                self.call_scan(scan, callee, args);
                args.iter().for_each(|a| self.scan_read(scan, a));
            }
            ExprKind::Closure(closure) => {
                // A capture is a copy of the captured variable.
                for name in self.captures.get(&expr.id).into_iter().flatten() {
                    scan.sharing.insert(self.root_of(scan, name));
                }
                closure.params.iter().for_each(|(p, _)| scan.bind(&p.name));
                self.block_scan(scan, &closure.body);
            }
            ExprKind::Match(scrutinee, arms) => {
                self.scan_read(scan, scrutinee);
                for arm in arms {
                    scan.bind_pattern(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.scan_read(scan, guard);
                    }
                    self.scan_read(scan, &arm.body);
                }
            }
            ExprKind::Spawn(closure) => self.scan_read(scan, closure),
            ExprKind::Select(arms) => {
                for arm in arms {
                    match &arm.kind {
                        SelectKind::Recv { binding, call } => {
                            scan.bind(&binding.name);
                            self.scan_read(scan, call);
                        }
                        SelectKind::Send(call) | SelectKind::After(call) => {
                            self.scan_read(scan, call)
                        }
                        SelectKind::Default(_) => {}
                    }
                    self.scan_read(scan, &arm.body);
                }
            }
            ExprKind::IfLet(parts) => {
                scan.bind_pattern(&parts.pattern);
                self.scan_read(scan, &parts.scrutinee);
                self.block_scan(scan, &parts.then);
                parts.else_.iter().for_each(|e| self.scan_read(scan, e));
            }
            ExprKind::If { cond, then, else_ } => {
                self.scan_read(scan, cond);
                self.block_scan(scan, then);
                else_.iter().for_each(|e| self.scan_read(scan, e));
            }
            ExprKind::Block(block) => self.block_scan(scan, block),
            ExprKind::Array(items) | ExprKind::Tuple(items) => {
                items.iter().for_each(|e| self.scan_read(scan, e))
            }
            // `out place` (v17): the call writes the place.
            ExprKind::Out(place) => {
                let deep = self.ty(place).is_deep(&self.structs);
                self.scan_write(scan, place, deep);
            }
            ExprKind::Hashed(lit) => {
                self.scan_read(scan, &lit.keys);
                self.scan_read(scan, &lit.values);
            }
            ExprKind::StructLit(lit) => {
                lit.fields.iter().for_each(|(_, e)| self.scan_read(scan, e));
                lit.base.iter().for_each(|e| self.scan_read(scan, e));
            }
            ExprKind::Unary(_, e) | ExprKind::Cast(e, _) | ExprKind::Try(e) => {
                self.scan_read(scan, e)
            }
            ExprKind::Field(base, _) => self.scan_read(scan, base),
            ExprKind::Binary(_, _, a, b) | ExprKind::Index(a, _, b) | ExprKind::Repeat(a, b) => {
                self.scan_read(scan, a);
                self.scan_read(scan, b);
            }
            ExprKind::Interp(parts) => parts
                .iter()
                .filter_map(|p| p.expr())
                .for_each(|e| self.scan_read(scan, e)),
            ExprKind::Format(call) => call.args.iter().for_each(|e| self.scan_read(scan, e)),
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Char(_)
            | ExprKind::Str(_)
            | ExprKind::Bool(_)
            | ExprKind::Name(_) => {}
        }
    }

    /// A call's callee: `xs.push(v)` and the like write `xs`; any other
    /// method on a value holding arrays may copy it; `swap` exchanges whole
    /// places.
    fn call_scan(&self, scan: &mut Scan, callee: &Expr, args: &[Expr]) {
        match &callee.kind {
            ExprKind::Field(base, method) if chain_root(base).is_some() => {
                let builtin = matches!(method.name.as_str(), "push" | "pop" | "clear");
                if let (Type::Array(element), true) = (self.ty(base), builtin) {
                    // `push` of a value holding arrays brings in an array
                    // that may be shared.
                    let deep = method.name == "push" && element.is_deep(&self.structs);
                    self.scan_write(scan, base, deep);
                    scan.paths.push((**base).clone());
                } else {
                    self.scan_receiver(scan, base);
                }
            }
            ExprKind::Name(name) if name == "swap" => {
                for arg in args {
                    if let Some(root) = chain_root(arg) {
                        scan.changed.insert(root.to_owned());
                        scan.sharing.insert(self.root_of(scan, root));
                    }
                }
            }
            _ => self.scan_read(scan, callee),
        }
    }

    /// A method's receiver: a value without arrays is read or written in
    /// place; one holding arrays may be copied by the method.
    fn scan_receiver(&self, scan: &mut Scan, place: &Expr) {
        let deep = self.ty(place).is_deep(&self.structs);
        if let Some(root) = chain_root(place) {
            scan.changed.insert(root.to_owned());
            if deep {
                scan.sharing.insert(self.root_of(scan, root));
            }
            arrays_on(place, &mut scan.paths);
            self.scan_indexes(scan, place);
        } else {
            self.scan_read(scan, place);
        }
    }

    /// A write into `place`; `deep` when the value written holds arrays.
    fn scan_write(&self, scan: &mut Scan, place: &Expr, deep: bool) {
        let Some(root) = chain_root(place) else {
            return self.scan_read(scan, place);
        };
        scan.changed.insert(root.to_owned());
        if deep {
            scan.sharing.insert(self.root_of(scan, root));
        }
        arrays_on(place, &mut scan.paths);
        self.scan_indexes(scan, place);
    }

    /// `for var of mut place` over an array: writes through `var` are
    /// writes into `place`.
    fn scan_alias(&self, scan: &mut Scan, var: &str, place: &Expr) {
        // Registering the alias writes nothing; what the body writes
        // through `var` is scanned as a write rooted at `place`'s root.
        self.scan_write(scan, place, false);
        scan.paths.push(place.clone());
        if let Some(root) = chain_root(place) {
            scan.aliases.insert(var.to_owned(), root.to_owned());
        }
    }

    /// The index expressions of a place chain, which are read.
    fn scan_indexes(&self, scan: &mut Scan, place: &Expr) {
        match &place.kind {
            ExprKind::Index(base, _, index) => {
                self.scan_read(scan, index);
                self.scan_indexes(scan, base);
            }
            ExprKind::Field(base, _) => self.scan_indexes(scan, base),
            _ => {}
        }
    }
}

impl Scan {
    fn bind(&mut self, name: &str) {
        self.bound.insert(name.to_owned());
        self.changed.insert(name.to_owned());
    }

    fn bind_pattern(&mut self, pattern: &Pattern) {
        let mut names = HashSet::new();
        pattern_names(pattern, &mut names);
        names.iter().for_each(|n| self.bind(n));
    }
}
