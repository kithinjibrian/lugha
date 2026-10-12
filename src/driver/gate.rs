//! The v1 feature gate (PRP-019): v1 syntax parses, but each construct is
//! stopped here, before checking, until the milestone that implements it.
//! Later PRPs delete entries as they land; the gate goes when v1 is complete.
//!
//! Still gated: methods (milestone 7).

use crate::ast::{Block, Expr, ExprKind, ForIter, FunDecl, Item, Program, Stmt, StmtKind};
use crate::span::Span;

/// A construct the checker can't handle yet.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct Gated {
    pub what: &'static str,
    pub milestone: u8,
    pub span: Span,
}

const METHODS: (&str, u8) = ("methods", 7);

/// The first not-yet-implemented v1 construct in `program`, in source order.
pub(super) fn first(program: &Program) -> Option<Gated> {
    let mut found = Vec::new();
    for item in &program.items {
        match item {
            Item::Impl(i) => hit(&mut found, METHODS, i.span),
            Item::Fun(f) => fun(&mut found, f),
            Item::Enum(_) | Item::Struct(_) | Item::Extern(_) => {}
        }
    }
    found.into_iter().min_by_key(|g| g.span.start)
}

fn hit(found: &mut Vec<Gated>, (what, milestone): (&'static str, u8), span: Span) {
    found.push(Gated {
        what,
        milestone,
        span,
    });
}

fn fun(found: &mut Vec<Gated>, f: &FunDecl) {
    if let Some(receiver) = f.receiver {
        hit(found, METHODS, receiver.span);
    }
    block(found, &f.body);
}

fn block(found: &mut Vec<Gated>, b: &Block) {
    b.stmts.iter().for_each(|s| stmt(found, s));
    b.tail.iter().for_each(|e| expr(found, e));
}

fn stmt(found: &mut Vec<Gated>, s: &Stmt) {
    match &s.kind {
        StmtKind::Let { init, .. } => expr(found, init),
        StmtKind::Assign { place, value, .. } => {
            expr(found, place);
            expr(found, value);
        }
        StmtKind::Expr { expr: e, .. } => expr(found, e),
        StmtKind::While { cond, body } => {
            expr(found, cond);
            block(found, body);
        }
        StmtKind::For { iter, body, .. } => {
            match iter {
                ForIter::Range(a, b) => {
                    expr(found, a);
                    expr(found, b);
                }
                ForIter::Array(xs) => expr(found, xs),
            }
            block(found, body);
        }
        StmtKind::Return(value) => value.iter().for_each(|e| expr(found, e)),
        StmtKind::Break | StmtKind::Continue => {}
    }
}

fn expr(found: &mut Vec<Gated>, e: &Expr) {
    match &e.kind {
        ExprKind::Match(scrutinee, arms) => {
            expr(found, scrutinee);
            arms.iter().for_each(|a| expr(found, &a.body));
        }
        ExprKind::Name(name) if name == "self" => hit(found, METHODS, e.span),
        ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Str(_)
        | ExprKind::Bool(_)
        | ExprKind::Name(_) => {}
        ExprKind::Unary(_, a) | ExprKind::Field(a, _) => expr(found, a),
        ExprKind::Cast(a, _) => expr(found, a),
        ExprKind::Binary(_, _, a, b) | ExprKind::Index(a, _, b) | ExprKind::Repeat(a, b) => {
            expr(found, a);
            expr(found, b);
        }
        ExprKind::Call(callee, args) => {
            expr(found, callee);
            args.iter().for_each(|a| expr(found, a));
        }
        ExprKind::StructLit(_, fields) => fields.iter().for_each(|(_, v)| expr(found, v)),
        ExprKind::Array(elements) => elements.iter().for_each(|x| expr(found, x)),
        ExprKind::If { cond, then, else_ } => {
            expr(found, cond);
            block(found, then);
            else_.iter().for_each(|x| expr(found, x));
        }
        ExprKind::Block(b) => block(found, b),
    }
}

#[cfg(test)]
mod tests {
    use super::first;

    fn gated(src: &str) -> Option<(&'static str, u8, &str)> {
        let (tokens, _) = crate::lexer::lex(src).expect("lexes");
        let (program, _) = crate::parser::parse(&tokens).expect("parses");
        first(&program).map(|g| (g.what, g.milestone, &src[g.span.start..g.span.end]))
    }

    #[test]
    fn methods_are_still_gated() {
        assert_eq!(
            gated("impl P {}\nfun main() {}"),
            Some(("methods", 7, "impl P {}"))
        );
        let receiver = "struct P { x: i64 }\nfun main() { let s = self; }";
        assert_eq!(gated(receiver), Some(("methods", 7, "self")));
    }

    #[test]
    fn plain_enums_match_and_options_pass() {
        assert_eq!(
            gated(
                "enum S { A(f64), B }\nfun main() { let o: Option<i64> = None; let x = match o { _ => 1 }; }"
            ),
            None
        );
        assert_eq!(
            gated("enum T { A(T[]) }\nfun main() {}"),
            None,
            "arrays break the cycle"
        );
    }

    #[test]
    fn v0_programs_pass_untouched() {
        assert_eq!(
            gated("struct P { x: i64 }\nfun main() { let p = P { x: 1 }; println(p.x); }"),
            None
        );
    }
}
