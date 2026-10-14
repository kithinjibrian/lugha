//! `Self` in `impl` blocks (v10 draft §3–4, milestone 34): while the
//! parser reads `impl T<P…> { … }`, a `Self` in a type or an expression
//! stands for the type as written, so later stages see `T` itself.

use super::Parser;
use crate::ast::{AppliedType, Ident, Type, TypeKind};

/// The name `Self` stands in for.
const SELF: &str = "Self";

impl Parser<'_> {
    /// Runs `f` with `Self` meaning `ty<params…>`, then restores what it
    /// meant before, whether `f` succeeded or not.
    pub(super) fn in_impl<T>(
        &mut self,
        ty: &Ident,
        params: &[Ident],
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let outer = self.impl_self.replace((ty.clone(), params.to_vec()));
        let result = f(self);
        self.impl_self = outer;
        result
    }

    /// The type a bare `Self` written at `name` stands for in an `impl`.
    // Not inlined: `named_type` recurses once per nesting level.
    #[inline(never)]
    pub(super) fn self_type(&self, name: &Ident) -> Option<TypeKind> {
        let (ty, params) = self.impl_self.as_ref().filter(|_| name.name == SELF)?;
        let at = |n: &Ident| Ident {
            name: n.name.clone(),
            span: name.span,
        };
        if params.is_empty() {
            return Some(TypeKind::Named(ty.name.clone()));
        }
        let args = params
            .iter()
            .map(|p| Type {
                kind: TypeKind::Named(p.name.clone()),
                span: name.span,
            })
            .collect();
        Some(TypeKind::Applied(Box::new(AppliedType {
            module: None,
            name: at(ty),
            args,
        })))
    }

    /// `name`, or the `impl`'s type name at the same span when `name` is
    /// `Self`: `Self { … }`, `Self.f()` and `Self.Red` read as `T`'s.
    pub(super) fn self_name(&self, name: Ident) -> Ident {
        match &self.impl_self {
            Some((ty, _)) if name.name == SELF => Ident {
                name: ty.name.clone(),
                span: name.span,
            },
            _ => name,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::program;

    #[test]
    fn self_is_the_impls_type_in_types_and_expressions() {
        let src = "impl Point {\n\
                   fun o(): Self = Self { x: 0 };\n\
                   fun p(self, q: Self): Self[] = [Self.o(), q];\n\
                   fun r(): Light = Self.Red;\n\
                   }";
        assert_eq!(
            program(src),
            "(impl Point \
             (fun o () Point (block (struct-lit Point (x 0)))) \
             (fun p (self (q Point)) Point[] (block (array (call (. Point o)) q))) \
             (fun r () Light (block (. Point Red))))"
        );
    }

    #[test]
    fn a_generic_impls_self_carries_its_parameters() {
        let src = "impl Stack<T> { fun f(self, o: Self): Option<Self> = None; }";
        assert_eq!(
            program(src),
            "(impl Stack (type-params T) \
             (fun f (self (o (applied Stack T))) (Option (applied Stack T)) (block None)))"
        );
    }

    #[test]
    fn outside_an_impl_self_stays_self() {
        assert_eq!(
            program("fun f(): Self = Self { x: 0 };"),
            "(fun f () Self (block (struct-lit Self (x 0))))"
        );
        assert_eq!(
            program("impl P { fun f() {} }\nfun g(): Self = Self.x;"),
            "(impl P (fun f () void (block)))\n(fun g () Self (block (. Self x)))"
        );
    }

    #[test]
    fn self_is_the_declared_type_in_declarations_v13() {
        assert_eq!(
            program("struct N { kids: Self[] }\nenum L { C(i64, Self), E }"),
            "(struct N (kids N[]))\n(enum L (C i64 L) E)"
        );
        assert_eq!(
            program("struct T<A> { k: Option<Self> }"),
            "(struct T (type-params A) (k (Option (applied T A))))"
        );
    }
}
