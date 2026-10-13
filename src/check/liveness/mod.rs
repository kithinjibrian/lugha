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
//! Parameters and `for … of` variables are never moved from.

use std::collections::{HashMap, HashSet};

mod walk;

use walk::Walk;

use super::Binding;
use crate::ast::{Expr, ExprId, ExprKind, Item, Program};
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

/// Every store-site value of `program` that can move instead of copying.
pub(super) fn moves(program: &Program, mentions: &HashMap<ExprId, Mention>) -> HashSet<ExprId> {
    let mut last: HashMap<Span, usize> = HashMap::new();
    for mention in mentions.values() {
        let latest = last.entry(mention.decl).or_insert(mention.at);
        *latest = (*latest).max(mention.at);
    }
    let mut walk = Walk {
        mentions,
        last,
        loops: Vec::new(),
        moves: HashSet::new(),
    };
    for item in &program.items {
        if let Item::Fun(f) = item {
            walk.block(&f.body);
        }
    }
    walk.moves
}

impl Walk<'_> {
    /// True if `value` is a place rooted in a `let` local that nothing can
    /// reach once `value` has been read.
    pub(super) fn dead_after(&self, value: &Expr) -> bool {
        let Some(root) = root(value) else {
            return false;
        };
        let Some(mention) = self.mentions.get(&root.id) else {
            return false;
        };
        if !matches!(mention.binding, Binding::Let { .. }) {
            return false;
        }
        let in_every_loop = self
            .loops
            .iter()
            .all(|l| l.start <= mention.decl.start && mention.decl.end <= l.end);
        in_every_loop && self.last[&mention.decl] < value.span.end
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
mod tests {
    use crate::check::test_util::ok;

    /// The source text of every moved expression in `src`, in source order.
    fn moved(src: &str) -> Vec<&str> {
        let program = {
            let (tokens, _) = crate::lexer::lex(src).expect("lexes");
            crate::parser::parse(&tokens).expect("parses").0
        };
        let checked = ok(src);
        let mut spans = Vec::new();
        visit_program(&program, &mut |e| {
            if checked.moves.contains(&e.id) {
                spans.push(e.span);
            }
        });
        spans.sort_by_key(|s| s.start);
        spans.iter().map(|s| &src[s.start..s.end]).collect()
    }

    fn visit_program(program: &crate::ast::Program, f: &mut dyn FnMut(&crate::ast::Expr)) {
        use crate::ast::{Item, StmtKind};
        fn block(b: &crate::ast::Block, f: &mut dyn FnMut(&crate::ast::Expr)) {
            for s in &b.stmts {
                match &s.kind {
                    StmtKind::Let { init, .. } => expr(init, f),
                    StmtKind::Assign { place, value, .. } => {
                        expr(place, f);
                        expr(value, f);
                    }
                    StmtKind::Expr { expr: e, .. } => expr(e, f),
                    StmtKind::While { cond, body } => {
                        expr(cond, f);
                        block(body, f);
                    }
                    StmtKind::For { body, .. } => block(body, f),
                    _ => {}
                }
            }
            if let Some(t) = &b.tail {
                expr(t, f);
            }
        }
        fn expr(e: &crate::ast::Expr, f: &mut dyn FnMut(&crate::ast::Expr)) {
            use crate::ast::ExprKind::*;
            f(e);
            match &e.kind {
                Array(es) => es.iter().for_each(|x| expr(x, f)),
                StructLit(lit) => lit.fields.iter().for_each(|(_, x)| expr(x, f)),
                Index(a, _, b) => {
                    expr(a, f);
                    expr(b, f);
                }
                Field(a, _) => expr(a, f),
                Call(callee, args) => {
                    expr(callee, f);
                    args.iter().for_each(|x| expr(x, f));
                }
                _ => {}
            }
        }
        for item in &program.items {
            if let Item::Fun(fun) = item {
                block(&fun.body, f);
            }
        }
    }

    #[test]
    fn a_dead_local_moves_and_a_live_one_copies() {
        assert_eq!(moved("fun main() { let xs = [1]; let ys = xs; }"), ["xs"]);
        assert!(moved("fun main() { let xs = [1]; let ys = xs; println(xs[0]); }").is_empty());
        assert!(moved("fun main() { let mut xs = [1]; let ys = xs; xs[0] = 2; }").is_empty());
    }

    #[test]
    fn loops_only_move_locals_declared_inside_them() {
        let life = "fun main() { let mut g = [[true]]; for s in 0..3 { let mut next = g; next[0][0] = false; g = next; } }";
        assert_eq!(moved(life), ["next"]);
        assert!(
            moved("fun main() { let a = [1]; for i in 0..3 { let mut c = a; c[0] = i; } }")
                .is_empty()
        );
    }

    #[test]
    fn shadowed_names_are_different_bindings() {
        assert_eq!(
            moved("fun main() { let a = [1]; let b = a; let a = [2]; println(a[0]); }"),
            ["a"]
        );
    }

    #[test]
    fn records_parameters_and_loop_variables() {
        let src = "struct P { v: i64[] }\nfun main() { let mut ps = [P { v: [1] }]; let mut p = ps[0]; p.v[0] = 2; ps[0] = p; }";
        assert_eq!(moved(src), ["p"]);
        assert!(moved("fun f(a: i64[]) { let b = a; }\nfun main() {}").is_empty());
        assert!(moved("fun main() { let g = [[1]]; for r of g { let c = r; } }").is_empty());
        let field =
            "struct W { data: i64[] }\nfun main() { let w = W { data: [1] }; let d = w.data; }";
        assert_eq!(moved(field), ["w.data"]);
    }

    #[test]
    fn literal_elements_and_fields_move_on_their_last_mention() {
        // Only the second `a` moves: the first is followed by another mention.
        let src = "fun main() { let a = [1]; let g = [a, a]; }";
        let moved_at: Vec<usize> = moved(src)
            .iter()
            .map(|m| m.as_ptr() as usize - src.as_ptr() as usize)
            .collect();
        assert_eq!(moved_at, [src.rfind('a').unwrap()]);
        let src = "struct W { data: i64[] }\nfun main() { let xs = [1]; let w = W { data: xs }; }";
        assert_eq!(moved(src), ["xs"]);
    }

    #[test]
    fn push_moves_a_dead_local_into_the_array() {
        let src = "fun main() { let mut g = [[1]]; let r = [2]; g.push(r); }";
        assert_eq!(moved(src), ["r"]);
        assert!(
            moved("fun main() { let mut g = [[1]]; let r = [2]; g.push(r); g[0] = r; }").len() == 1
        );
    }
}
