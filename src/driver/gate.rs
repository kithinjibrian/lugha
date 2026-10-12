//! The v1 feature gate (PRP-019): v1 syntax parses, but each construct is
//! stopped here, before checking, until the milestone that implements it.
//! Later PRPs delete entries as they land; the gate goes when v1 is complete.
//!
//! Still gated: recursive enums (PRP-021) and methods (milestone 7).

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

const RECURSIVE: (&str, u8) = ("recursive enums", 6);
const METHODS: (&str, u8) = ("methods", 7);

/// The first not-yet-implemented v1 construct in `program`, in source order.
pub(super) fn first(program: &Program) -> Option<Gated> {
    let mut found = Vec::new();
    for item in &program.items {
        match item {
            Item::Enum(e) if recursive(program, &e.name.name) => hit(&mut found, RECURSIVE, e.span),
            Item::Enum(_) => {}
            // A struct cycle through an enum or `Option` needs indirect slots
            // too; a plain struct cycle is the checker's E0307.
            Item::Struct(s) if recursive(program, &s.name.name) => {
                hit(&mut found, RECURSIVE, s.span)
            }
            Item::Impl(i) => hit(&mut found, METHODS, i.span),
            Item::Fun(f) => fun(&mut found, f),
            Item::Extern(e) => {
                e.params.iter().for_each(|p| ty(&mut found, &p.ty));
                e.ret.iter().for_each(|t| ty(&mut found, t));
            }
            Item::Struct(_) => {}
        }
    }
    found.into_iter().min_by_key(|g| g.span.start)
}

/// True if the struct or enum `name` reaches itself through a path of
/// inline payloads and fields (not arrays, which are pointers) that passes
/// through at least one enum or `Option`. Works on names alone.
fn recursive(program: &Program, name: &str) -> bool {
    let mut stack: Vec<(String, bool)> = refs(program, name);
    let mut seen: Vec<(String, bool)> = Vec::new();
    while let Some((next, via_enum)) = stack.pop() {
        if next == name && via_enum {
            return true;
        }
        if !seen.contains(&(next.clone(), via_enum)) {
            let onward = refs(program, &next)
                .into_iter()
                .map(|(n, e)| (n, e || via_enum));
            stack.extend(onward);
            seen.push((next, via_enum));
        }
    }
    false
}

/// The type names held inline by the struct or enum called `name`, each
/// marked true if the link goes through an enum payload or an `Option`.
fn refs(program: &Program, name: &str) -> Vec<(String, bool)> {
    let mut out = Vec::new();
    for item in &program.items {
        let (types, from_enum): (Vec<&Type>, bool) = match item {
            Item::Enum(e) if e.name.name == name => {
                (e.variants.iter().flat_map(|v| &v.payloads).collect(), true)
            }
            Item::Struct(s) if s.name.name == name => {
                (s.fields.iter().map(|f| &f.ty).collect(), false)
            }
            _ => continue,
        };
        for t in types {
            inline_names(t, from_enum, &mut out);
        }
    }
    out
}

fn inline_names(t: &Type, via_enum: bool, out: &mut Vec<(String, bool)>) {
    match &t.kind {
        TypeKind::Named(n) => out.push((n.clone(), via_enum)),
        TypeKind::Option(inner) => inline_names(inner, true, out),
        _ => {}
    }
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

fn ty(_found: &mut [Gated], _t: &Type) {}

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
    fn recursive_enums_and_methods_are_still_gated() {
        assert_eq!(
            gated("enum L { N, C(i64, L) }\nfun main() {}"),
            Some(("recursive enums", 6, "enum L { N, C(i64, L) }"))
        );
        let via_struct = "enum T { Leaf, Node(P) }\nstruct P { t: T }\nfun main() {}";
        assert_eq!(gated(via_struct).map(|g| g.0), Some("recursive enums"));
        let via_option =
            "struct N { next: Option<N> }\nenum E { A(N) }\nenum F { B(Option<F>) }\nfun main() {}";
        assert_eq!(
            gated(via_option).map(|g| g.2),
            Some("struct N { next: Option<N> }")
        );
        // A plain struct cycle is left for the checker (E0307).
        assert_eq!(
            gated("struct A { b: B }\nstruct B { a: A }\nfun main() {}"),
            None
        );
        assert_eq!(
            gated("impl P {}\nfun main() {}"),
            Some(("methods", 7, "impl P {}"))
        );
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
