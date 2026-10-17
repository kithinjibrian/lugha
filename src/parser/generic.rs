//! v2 generic and module syntax in declarations and types: type parameters
//! after a name, `impl` headers, and `m.T` / `T<args>` types (v2 draft §3).

use super::{PResult, Parser};
use crate::ast::{AppliedType, Ident, TypeKind, TypeParam};
use crate::lexer::TokenKind;

/// `m.v` as one module name, spanning both (v14).
pub(super) fn joined(m: &Ident, v: &Ident) -> Ident {
    Ident {
        name: format!("{}.{}", m.name, v.name),
        span: crate::span::Span::new(m.span.start, v.span.end),
    }
}

impl Parser<'_> {
    /// `<T, U: Eq + Ord>` after a declaration's name; empty when there is none.
    pub(super) fn type_params(&mut self) -> PResult<Vec<TypeParam>> {
        if !self.at(&TokenKind::Lt) {
            return Ok(Vec::new());
        }
        let open = self.bump();
        if self.at(&TokenKind::Gt) {
            return Err(self.expected("type parameter name"));
        }
        self.comma_list(open, TokenKind::Gt, |p| {
            let name = p.ident("type parameter name")?;
            let mut bounds = Vec::new();
            if p.eat(&TokenKind::Colon) {
                loop {
                    bounds.push(p.bound()?);
                    if !p.eat(&TokenKind::Plus) {
                        break;
                    }
                }
            }
            Ok(TypeParam { name, bounds })
        })
    }

    /// `fun(A, B): R` or `(T)` (v3 draft §3). A return type takes any `[]`
    /// after it, so `(fun(i64): i64)[]` is how an array of functions is written.
    pub(super) fn compound_type(&mut self) -> PResult<TypeKind> {
        if self.at(&TokenKind::LParen) {
            let open = self.bump();
            // `()` (v17, milestone 107): the unit type, an empty tuple.
            if self.eat(&TokenKind::RParen) {
                return Ok(TypeKind::Tuple(Vec::new()));
            }
            let inner = self.nested(|p| p.ty())?;
            // `(A, B)` (v14): a tuple type.
            if self.at(&TokenKind::Comma) {
                return self.tuple_type(open, inner);
            }
            self.close(open, &TokenKind::RParen)?;
            return Ok(inner.kind);
        }
        self.bump();
        let paren = self.expect(&TokenKind::LParen)?;
        let params = self.nested(|p| p.comma_list(paren, TokenKind::RParen, |p| p.ty()))?;
        let ret = if self.eat(&TokenKind::Colon) {
            Some(Box::new(self.nested(|p| p.ty())?))
        } else {
            None
        };
        Ok(TypeKind::Fun(params, ret))
    }

    /// The type's parameters in an `impl` header, `<T, U>`, without bounds:
    /// those come from the type's declaration (v2 draft §3).
    pub(super) fn impl_params(&mut self) -> PResult<Vec<TypeParam>> {
        // The same syntax as a function's, bounds included (v15).
        self.type_params()
    }

    /// `Name`, `m.Name`, or `m.v.Name` through a re-exported module (v14),
    /// after the first identifier: the module, its segments joined with
    /// `.`, and the name.
    pub(super) fn qualified_name(
        &mut self,
        first: Ident,
        what: &str,
    ) -> PResult<(Option<Ident>, Ident)> {
        if !self.eat(&TokenKind::Dot) {
            return Ok((None, first));
        }
        let second = self.ident(what)?;
        if !self.eat(&TokenKind::Dot) {
            return Ok((Some(first), second));
        }
        let name = self.ident(what)?;
        Ok((Some(joined(&first, &second)), name))
    }

    /// The rest of a named type after its first identifier: `Name`, `m.Name`,
    /// `Name<args>` or `m.Name<args>`. Each argument list is one nesting
    /// level, so `A<A<A<…>>>` hits E0206 instead of overflowing the stack.
    pub(super) fn named_type(&mut self, first: Ident) -> PResult<TypeKind> {
        let (module, name) = self.qualified_name(first, "type name")?;
        let args = if self.at(&TokenKind::Lt) {
            let open = self.bump();
            if self.at(&TokenKind::Gt) {
                return Err(self.expected("type"));
            }
            self.nested(|p| p.comma_list(open, TokenKind::Gt, |p| p.ty()))?
        } else {
            Vec::new()
        };
        if module.is_none()
            && args.is_empty()
            && let Some(kind) = self.self_type(&name)
        {
            return Ok(kind);
        }
        Ok(if module.is_none() && args.is_empty() {
            TypeKind::Named(name.name)
        } else {
            TypeKind::Applied(Box::new(AppliedType { module, name, args }))
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{body, errors, expr, program};

    #[test]
    fn function_types_and_parenthesized_types() {
        let sig = |ty: &str| program(&format!("fun f(x: {ty}) {{}}"));
        assert_eq!(
            sig("fun(i64, i64): bool"),
            "(fun f ((x (fun-type (i64 i64) bool))) void (block))"
        );
        assert_eq!(
            sig("fun(i64)"),
            "(fun f ((x (fun-type (i64) void))) void (block))"
        );
        assert_eq!(
            sig("fun(): fun(i64): i64"),
            "(fun f ((x (fun-type () (fun-type (i64) i64)))) void (block))"
        );
        // `[]` belongs to the return type; parentheses make an array of functions.
        assert_eq!(
            sig("fun(i64): i64[]"),
            "(fun f ((x (fun-type (i64) i64[]))) void (block))"
        );
        assert_eq!(
            sig("(fun(i64): i64)[]"),
            "(fun f ((x (fun-type (i64) i64)[])) void (block))"
        );
        assert_eq!(sig("(i64)"), "(fun f ((x i64)) void (block))");
    }

    #[test]
    fn declarations_take_type_parameters_and_bounds() {
        assert_eq!(
            program("fun max<T: Ord>(a: T, b: T): T = a;"),
            "(fun max (type-params (T Ord)) ((a T) (b T)) T (block a))"
        );
        assert_eq!(
            program("struct Pair<A, B> { a: A, b: B }"),
            "(struct Pair (type-params A B) (a A) (b B))"
        );
        assert_eq!(
            program("enum Tree<T: Eq + Ord,> { Leaf, Node(Tree<T>, T) }"),
            "(enum Tree (type-params (T Eq Ord)) Leaf (Node (applied Tree T) T))"
        );
        assert_eq!(errors("fun f<>() {}"), [("E0201", ">")]);
        assert_eq!(errors("fun f<T:>() {}"), [("E0201", ">")]);
    }

    #[test]
    fn impl_headers_name_the_type_and_its_parameters() {
        assert_eq!(
            program("impl Stack<T> { fun new(): Stack<T> = Stack { items: [] }; }"),
            "(impl Stack (type-params T) (fun new () (applied Stack T) (block (struct-lit Stack (items (array))))))"
        );
        assert_eq!(program("impl geometry.Vec2 {}"), "(impl geometry.Vec2)");
    }

    #[test]
    fn types_can_be_qualified_and_applied() {
        assert_eq!(
            program(
                "fun f(s: Stack<i64>, t: Stack<Option<i64>>[], v: geometry.Vec2, p: m.Pair<i64, string>) {}"
            ),
            "(fun f ((s (applied Stack i64)) (t (applied Stack (Option i64))[]) (v (applied geometry.Vec2)) (p (applied m.Pair i64 string))) void (block))"
        );
        assert_eq!(
            body("let s: Stack<i64> = x; let y = x as m.T;"),
            "(block (let s (applied Stack i64) x) (let y (as x (applied m.T))))"
        );
        // `Box<L>` is kept for the checker's E0305 help.
        assert_eq!(
            program("enum L { N, C(Box<L>) }"),
            "(enum L N (C (applied Box L)))"
        );
        let deep = format!("fun f(x: {}i64{}) {{}}", "A<".repeat(300), ">".repeat(300));
        assert!(
            errors(&deep).iter().any(|(code, _)| *code == "E0206"),
            "{:?}",
            errors(&deep)
        );
    }

    #[test]
    fn qualified_struct_literals_follow_the_condition_rule() {
        assert_eq!(
            expr("geometry.Vec2 { x: 1.0 }"),
            "(struct-lit geometry.Vec2 (x 1.0))"
        );
        assert_eq!(
            expr("geometry.Vec2 { x: 1.0 }.x"),
            "(. (struct-lit geometry.Vec2 (x 1.0)) x)"
        );
        assert_eq!(
            errors("fun t() { if p == geometry.Vec2 { x: 1.0 } { } }"),
            [("E0205", "geometry.Vec2 { x: 1.0 }")]
        );
        // Not a literal: a block follows a field read in a condition.
        assert_eq!(body("if a.b { }"), "(block (if (. a b) (block)))");
    }
}
