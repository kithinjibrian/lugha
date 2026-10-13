//! The v2 feature gate (PRP-030; milestone 9's entries went with PRP-032): all of the v2 draft's syntax parses, but
//! each construct is stopped here, before checking, until the milestone that
//! implements it. Later PRPs delete entries as they land; the gate goes when
//! v2 is complete (milestone 11). v1's gate worked the same way (MEMORY 24).

mod walk;

use walk::Walk;

use crate::ast::{Item, Program};
use crate::span::Span;

/// A construct the checker can't handle yet.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct Gated {
    pub what: &'static str,
    pub milestone: u8,
    pub span: Span,
}

const MODULES: (&str, u8) = ("modules", 10);
const VISIBILITY: (&str, u8) = ("visibility", 10);
const GENERICS: (&str, u8) = ("generics", 11);

/// The first not-yet-implemented v2 construct in `program`, in source order.
pub(super) fn first(program: &Program) -> Option<Gated> {
    // Type arguments are gated only on the program's own structs and enums:
    // an unknown name like `Box<List>` is an error anyway, and the checker
    // explains it (E0305).
    let declared = program
        .items
        .iter()
        .filter_map(|item| match item {
            Item::Struct(s) => Some(s.name.name.as_str()),
            Item::Enum(e) => Some(e.name.name.as_str()),
            _ => None,
        })
        .collect();
    let mut walk = Walk {
        declared,
        found: Vec::new(),
    };
    for import in &program.imports {
        walk.hit(MODULES, import.span);
    }
    for item in &program.items {
        walk.item(item);
    }
    walk.found.into_iter().min_by_key(|g| g.span.start)
}

#[cfg(test)]
mod tests {
    use super::first;
    use crate::{lexer, parser};

    /// `(what, milestone, gated source text)` of the first gated construct.
    fn gated(src: &str) -> Option<(&'static str, u8, &str)> {
        let (tokens, _) = lexer::lex(src).expect("lexes");
        let (program, _) = parser::parse(&tokens).expect("parses");
        first(&program).map(|g| (g.what, g.milestone, &src[g.span.start..g.span.end]))
    }

    #[test]
    fn v1_programs_pass_through() {
        let src = "enum L { N, C(i64, L) }\nstruct S { xs: Option<L>[] }\nimpl S { fun f(mut self) {} }\n\
                   fun main() { let s = S { xs: [] }; match s.xs[0] { Some(l) => 1, _ => 0 }; }";
        assert_eq!(gated(src), None);
        // An unknown generic name is left to the checker's E0305 help.
        assert_eq!(gated("enum L { N, C(Box<L>) }\nfun main() {}"), None);
    }

    #[test]
    fn milestone_10_constructs() {
        assert_eq!(
            gated("import geometry;\nfun main() {}"),
            Some(("modules", 10, "import geometry;"))
        );
        assert_eq!(
            gated("private fun f() {}"),
            Some(("visibility", 10, "private"))
        );
        assert_eq!(
            gated("impl S { public fun f(self) {} }"),
            Some(("visibility", 10, "public"))
        );
        assert_eq!(
            gated("fun f(v: geometry.Vec2) {}"),
            Some(("modules", 10, "geometry.Vec2"))
        );
        assert_eq!(
            gated("fun f() { let v = geometry.Vec2 { x: 1.0 }; }"),
            Some(("modules", 10, "geometry.Vec2 { x: 1.0 }"))
        );
        assert_eq!(
            gated("impl geometry.Vec2 {}"),
            Some(("modules", 10, "geometry"))
        );
    }

    #[test]
    fn milestone_11_constructs() {
        assert_eq!(
            gated("fun id<T>(x: T): T = x;"),
            Some(("generics", 11, "T"))
        );
        assert_eq!(gated("struct S<T> { x: T }"), Some(("generics", 11, "T")));
        assert_eq!(gated("enum E<T> { A(T) }"), Some(("generics", 11, "T")));
        assert_eq!(
            gated("struct S {}\nimpl S<T> {}"),
            Some(("generics", 11, "T"))
        );
        assert_eq!(
            gated("struct S { x: i64 }\nfun f(s: S<i64>) {}"),
            Some(("generics", 11, "S<i64>"))
        );
        // Types are found everywhere: lets, casts, fields, payloads, returns.
        assert_eq!(
            gated("struct S {}\nfun f() { let s: Option<S<i64>[]> = None; }"),
            Some(("generics", 11, "S<i64>"))
        );
    }

    #[test]
    fn field_visibility_is_gated() {
        assert_eq!(
            gated("struct R { private lo: i64 }"),
            Some(("visibility", 10, "private"))
        );
    }

    #[test]
    fn the_first_in_source_order_wins() {
        assert_eq!(
            gated("fun f<T>() {}\nprivate fun g() {}"),
            Some(("generics", 11, "T"))
        );
        assert_eq!(
            gated("struct S { x: geometry.Vec2 }\nprivate fun f() {}"),
            Some(("modules", 10, "geometry.Vec2"))
        );
    }
}
