//! The v1 feature gate (PRP-019): v1 syntax parses, but each construct is
//! stopped here, before checking, until the milestone that implements it.
//! Later PRPs delete entries as they land; the gate goes when v1 is complete.

use crate::ast::{
    Block, Expr, ExprKind, ForIter, FunDecl, Item, Program, Stmt, StmtKind, Type, TypeKind,
};
use crate::span::Span;

/// A construct the checker can't handle yet.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct Gated {
    pub what: &'static str,
    pub milestone: u8,
    pub span: Span,
}

const ENUMS: (&str, u8) = ("enums", 6);
const METHODS: (&str, u8) = ("methods", 7);

/// The first not-yet-implemented v1 construct in `program`, in source order.
pub(super) fn first(program: &Program) -> Option<Gated> {
    let mut found = Vec::new();
    for item in &program.items {
        match item {
            Item::Enum(e) => hit(&mut found, ENUMS, e.span),
            Item::Impl(i) => hit(&mut found, METHODS, i.span),
            Item::Fun(f) => fun(&mut found, f),
            Item::Extern(e) => {
                e.params.iter().for_each(|p| ty(&mut found, &p.ty));
                e.ret.iter().for_each(|t| ty(&mut found, t));
            }
            Item::Struct(s) => s.fields.iter().for_each(|f| ty(&mut found, &f.ty)),
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
    f.params.iter().for_each(|p| ty(found, &p.ty));
    f.ret.iter().for_each(|t| ty(found, t));
    block(found, &f.body);
}

fn ty(found: &mut Vec<Gated>, t: &Type) {
    match &t.kind {
        TypeKind::Option(_) => hit(found, ENUMS, t.span),
        TypeKind::Array(element) => ty(found, element),
        _ => {}
    }
}

fn block(found: &mut Vec<Gated>, b: &Block) {
    b.stmts.iter().for_each(|s| stmt(found, s));
    b.tail.iter().for_each(|e| expr(found, e));
}

fn stmt(found: &mut Vec<Gated>, s: &Stmt) {
    match &s.kind {
        StmtKind::Let {
            ty: annotation,
            init,
            ..
        } => {
            annotation.iter().for_each(|t| ty(found, t));
            expr(found, init);
        }
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
        ExprKind::Match(..) => hit(found, ENUMS, e.span),
        ExprKind::Name(name) if name == "self" => hit(found, METHODS, e.span),
        ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Str(_)
        | ExprKind::Bool(_)
        | ExprKind::Name(_) => {}
        ExprKind::Unary(_, a) | ExprKind::Field(a, _) => expr(found, a),
        ExprKind::Cast(a, t) => {
            expr(found, a);
            ty(found, t);
        }
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
    fn each_v1_construct_names_its_milestone() {
        assert_eq!(
            gated("enum E { A }\nfun main() {}"),
            Some(("enums", 6, "enum E { A }"))
        );
        assert_eq!(
            gated("fun main() { let x = match 1 { _ => 2 }; }"),
            Some(("enums", 6, "match 1 { _ => 2 }"))
        );
        assert_eq!(
            gated("fun f(): Option<i64> = 1;\nfun main() {}"),
            Some(("enums", 6, "Option<i64>"))
        );
        assert_eq!(
            gated("impl P {}\nfun main() {}"),
            Some(("methods", 7, "impl P {}"))
        );
    }

    #[test]
    fn the_first_construct_in_source_order_wins() {
        let src = "impl P { fun f(self) {} }\nenum E { A }\nfun main() {}";
        assert_eq!(
            gated(src),
            Some(("methods", 7, "impl P { fun f(self) {} }"))
        );
        assert_eq!(
            gated("fun main() { let v: Option<i64>[] = []; }"),
            Some(("enums", 6, "Option<i64>"))
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
