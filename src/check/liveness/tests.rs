//! Move on last use and handing over arguments: which values move.

use crate::check::test_util::ok;
use std::collections::HashSet;

/// The source text of every moved expression in `src`, in source order.
fn moved(src: &str) -> Vec<&str> {
    let checked = ok(src);
    marked(src, &checked.moves)
}

/// The source text of every expression in `set`, in source order.
fn marked<'a>(src: &'a str, set: &HashSet<crate::ast::ExprId>) -> Vec<&'a str> {
    let program = {
        let (tokens, _) = crate::lexer::lex(src).expect("lexes");
        crate::parser::parse(&tokens).expect("parses").0
    };
    let mut spans = Vec::new();
    visit_program(&program, &mut |e| {
        if set.contains(&e.id) {
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
                StmtKind::While { cond, body, .. } => {
                    expr(cond, f);
                    block(body, f);
                }
                StmtKind::For { body, .. } => block(body, f),
                StmtKind::Defer(inner) => {
                    if let StmtKind::Expr { expr: e, .. } = &inner.kind {
                        expr(e, f)
                    }
                }
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
            StructLit(lit) => {
                lit.fields.iter().for_each(|(_, x)| expr(x, f));
                lit.base.iter().for_each(|x| expr(x, f));
            }
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
        match item {
            Item::Fun(fun) => block(&fun.body, f),
            Item::Impl(i) => i.methods.iter().for_each(|m| block(&m.body, f)),
            _ => {}
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
        moved("fun main() { let a = [1]; for i in 0..3 { let mut c = a; c[0] = i; } }").is_empty()
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
    let field = "struct W { data: i64[] }\nfun main() { let w = W { data: [1] }; let d = w.data; }";
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

/// The source text of every handed-over argument in `src`.
fn handed(src: &str) -> Vec<&str> {
    let checked = ok(src);
    marked(src, &checked.hands)
}

const BAG: &str = "struct Bag { items: i64[] }\n\
    fun insert(b: Bag, k: i64): Bag { let mut out = b; out.items.push(k); out }\n\
    fun total(b: Bag): i64 = b.items.len;\n";

#[test]
fn an_overwritten_local_is_handed_over_v17() {
    let src = format!(
        "{BAG}fun main() {{ let mut bag = Bag {{ items: [] }}; for k in 0..3 {{ bag = insert(bag, k); }} println(total(bag)); }}"
    );
    assert_eq!(handed(&src), ["bag", "bag"]);
    // Read again in the same value, or later: kept.
    let src = format!(
        "{BAG}fun main() {{ let mut bag = Bag {{ items: [] }}; bag = insert(bag, total(bag)); let b2 = insert(bag, 1); println(total(bag)); }}"
    );
    // Only the last mention inside the value, and the very last.
    assert_eq!(handed(&src), ["bag", "bag"]);
    let at: Vec<usize> = handed(&src)
        .iter()
        .map(|m| m.as_ptr() as usize - src.as_ptr() as usize)
        .collect();
    assert_eq!(at[0], src.find("total(bag)").unwrap() + 6);
}

#[test]
fn field_targets_and_closures_hand_nothing_over_v17() {
    let src = format!(
        "{BAG}struct Box2 {{ bag: Bag }}\n\
         fun main() {{ let mut w = Box2 {{ bag: Bag {{ items: [] }} }}; w.bag = insert(w.bag, 1); let mut b = w.bag; let f = fun(): Bag = insert(b, 2); b = insert(b, 3); println(total(b)); }}"
    );
    // Not `w.bag` (a field target), nor `b` inside the closure (a capture).
    assert_eq!(handed(&src), ["b", "b"]);
}

#[test]
fn a_stored_parameter_is_kept_and_a_read_one_is_not_v17() {
    let checked = ok(&format!("{BAG}fun main() {{}}"));
    // `k` is pushed, but holds no array: not kept.
    assert_eq!(checked.keeps.get("insert"), Some(&vec![true, false]));
    assert_eq!(checked.keeps.get("total"), None);
    // Stored but read afterwards: not its last use, not kept.
    let checked = ok(&format!(
        "{BAG}fun twice(b: Bag): Bag {{ let mut c = b; c.items.push(total(b)); c }}\nfun main() {{}}"
    ));
    assert_eq!(checked.keeps.get("twice"), None);
    // Returned as is: kept.
    let checked = ok(&format!("{BAG}fun same(b: Bag): Bag = b;\nfun main() {{}}"));
    assert_eq!(checked.keeps.get("same"), Some(&vec![true]));
}

#[test]
fn keeping_spreads_through_parameters_handed_on_v17() {
    let checked = ok(&format!(
        "{BAG}fun via(b: Bag): Bag = insert(b, 1);\n\
         fun even(b: Bag, n: i64): Bag = if n == 0 {{ b }} else {{ odd(b, n - 1) }};\n\
         fun odd(b: Bag, n: i64): Bag = even(insert(b, n), n - 1);\n\
         fun main() {{}}"
    ));
    assert_eq!(checked.keeps.get("via"), Some(&vec![true]));
    assert_eq!(checked.keeps.get("odd"), Some(&vec![true, false]));
    assert!(checked.param_hands.len() >= 2);
}

#[test]
fn receivers_are_slot_zero_and_methods_are_walked_v17() {
    let src = "struct Bag { items: i64[] }\n\
        impl Bag {\n\
            fun with(self, k: i64): Bag { let mut b = self; b.items.push(k); b }\n\
            fun grow(mut self, k: i64) { let xs = [k]; let ys = xs; self.items.push(ys[0]); }\n\
        }\n\
        fun main() { let mut bag = Bag { items: [] }; bag = bag.with(1); bag.grow(2); }";
    let checked = ok(src);
    assert_eq!(checked.keeps.get("Bag.with"), Some(&vec![true, false]));
    assert_eq!(checked.keeps.get("Bag.grow"), None);
    assert_eq!(moved(src), ["xs", "ys[0]"]);
    assert_eq!(handed(src), ["bag", "bag"]); // the second, a `mut self`, is ignored by codegen
}

#[test]
fn a_deferred_mention_counts_at_the_end_of_its_block() {
    // The `defer` runs after `let ys = xs;`, so `xs` is still read then.
    let src = "fun main() { let xs = [1]; defer println(xs[0]); let ys = xs; }";
    assert!(moved(src).is_empty());
    // Inside the deferred statement it would be the last use, but the rule
    // errs towards copying: the statement's mentions count at the end too.
    let src = "fun main() { let xs = [1]; defer { let ys = xs; } println(xs[0]); }";
    assert!(moved(src).is_empty());
    // Below its block, the deferred statement has already run.
    let src = "fun main() { let xs = [1]; { defer println(xs[0]); } let ys = xs; }";
    assert_eq!(moved(src), ["xs"]);
}
