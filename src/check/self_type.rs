//! `Self` in interfaces (v6 draft §4, milestone 25): inside an interface's
//! signatures and default bodies, `Self` is the type that satisfies it.
//! Items are compared and called with `Self` replaced by the type in
//! question; an interface that mentions `Self` beyond its receivers is a
//! bound only (E0427).

use std::collections::HashMap;

use super::applied::Applied;
use super::generics::Generic;
use super::interfaces::Requirement;
use super::{Checker, SELF, Type, errors};
use crate::ast::{self, InterfaceDecl, TypeKind, TypeParam};

/// `Self` as a type parameter: in scope while an interface's items resolve,
/// standing for the type that satisfies it.
pub(super) fn self_param() -> Generic {
    Generic {
        name: SELF.to_string(),
        bounds: Default::default(),
    }
}

/// The first method of `decl` whose written parameters or result mention
/// `Self`: such an interface has no values (E0427). Read from the syntax, so
/// it is known in pass 1, before any type that names the interface resolves.
pub(super) fn written_self_use(decl: &InterfaceDecl) -> Option<String> {
    let names_self = |t: &ast::Type| written(t);
    decl.items
        .iter()
        .filter(|it| it.receiver.is_some())
        .find(|it| {
            it.params.iter().any(|p| names_self(&p.ty)) || it.ret.as_ref().is_some_and(names_self)
        })
        .map(|it| it.name.name.clone())
}

/// True if the written type `ty` names `Self` anywhere.
fn written(ty: &ast::Type) -> bool {
    match &ty.kind {
        TypeKind::Named(n) => n == SELF,
        TypeKind::Array(inner) | TypeKind::Option(inner) => written(inner),
        TypeKind::Applied(a) => {
            a.module.is_none() && a.name.name == SELF || a.args.iter().any(written)
        }
        TypeKind::Fun(params, ret) => {
            params.iter().any(written) || ret.as_deref().is_some_and(written)
        }
        _ => false,
    }
}

impl Checker {
    /// E0316 for an interface parameter named `Self`: inside an interface
    /// `Self` is the implementing type.
    pub(super) fn no_self_param(&mut self, params: &[TypeParam]) {
        for p in params.iter().filter(|p| p.name.name == SELF) {
            self.report(errors::bad_type_param(&p.name.name, p.name.span));
        }
    }

    /// The items of `applied` for the type `self_ty`: its parameters at its
    /// arguments, and `Self` replaced by `self_ty`.
    pub(super) fn items_for(&self, applied: &Applied, self_ty: &Type) -> Vec<Requirement> {
        let map: HashMap<String, Type> = [(SELF.to_string(), self_ty.clone())].into();
        self.items(applied)
            .into_iter()
            .map(|r| Requirement {
                params: r.params.iter().map(|t| t.substitute(&map)).collect(),
                ret: r.ret.substitute(&map),
                ..r
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{diagnostics, errors, let_type, ok};

    const ORD: &str = "interface Ordered {\n\
            fun less(self, other: Self): bool;\n\
            fun max(self, other: Self): Self = if self.less(other) { other } else { self };\n\
        }\n\
        interface Number { fun zero(): Self; fun plus(self, other: Self): Self; }\n\
        struct Money { cents: i64 }\n\
        impl Money {\n\
            fun less(self, other: Money): bool = self.cents < other.cents;\n\
            fun zero(): Money = Money { cents: 0 };\n\
            fun plus(self, other: Money): Money = Money { cents: self.cents + other.cents };\n\
        }\n\
        struct Stack<T> { items: T[] }\n\
        impl Stack<T> { fun less(self, other: Stack<T>): bool = self.items.len < other.items.len; }\n\
        struct Bad { n: i64 }\n\
        impl Bad { fun less(self, other: i64): bool = false; }\n";

    fn codes(body: &str) -> Vec<&'static str> {
        let src = format!("{ORD}{body}\nfun main() {{}}");
        errors(&src).into_iter().map(|(c, _)| c).collect()
    }

    #[test]
    fn self_resolves_only_inside_interfaces() {
        ok(&format!(
            "{ORD}interface Many {{\n\
                fun all(self, xs: Self[]): Option<Self>;\n\
                fun one(self): Self[] {{ let me: Self = self; [me] }}\n\
            }}\nfun main() {{}}"
        ));
        assert_eq!(codes("fun f(x: Self) {}"), ["E0305"]);
        // v13: `Self` in a declaration is the declared type, so this recurses.
        assert_eq!(codes("struct S { me: Self }"), ["E0307"]);
        // v10: inside an `impl`, `Self` is the type itself.
        ok(&format!(
            "{ORD}impl Money {{ fun me(self): Self = self; }}\nfun main() {{}}"
        ));
    }

    #[test]
    fn satisfaction_replaces_self_with_the_type() {
        ok(&format!(
            "{ORD}fun big<T: Ordered>(a: T, b: T): T = a.max(b);\n\
             fun main() {{ let m = big(Money {{ cents: 1 }}, Money {{ cents: 2 }}); let s = big(Stack {{ items: [1] }}, Stack {{ items: [2] }}); }}"
        ));
        let src = format!(
            "{ORD}fun big<T: Ordered>(a: T, b: T): T = a.max(b);\nfun main() {{ let m = big(Bad {{ n: 1 }}, Bad {{ n: 2 }}); }}"
        );
        let d = &diagnostics(&src)[0];
        assert_eq!(d.code, "E0422");
        let help = d.help.as_deref().expect("a help");
        assert!(
            help.contains("`less` should be `fun less(self, Bad): bool`"),
            "{help}"
        );
    }

    #[test]
    fn through_a_bound_self_is_the_parameter() {
        let at = |body: &str, name: &str| {
            let_type(
                &format!(
                    "{ORD}fun f<T: Ordered + Number>(a: T, b: T) {{ {body} }}\nfun main() {{}}"
                ),
                name,
            )
        };
        assert_eq!(at("let x = a.less(b);", "x"), Type::Bool);
        assert_eq!(at("let x = a.max(b);", "x"), Type::Param("T".into()));
        assert_eq!(at("let x = T.zero();", "x"), Type::Param("T".into()));
        assert_eq!(
            codes("fun f<T: Ordered>(a: T) { let x = a.less(1); }"),
            ["E0401"]
        );
    }

    #[test]
    fn interfaces_using_self_are_bounds_only() {
        for ty in ["Ordered", "Ordered[]", "Option<Ordered>"] {
            let src = format!("{ORD}fun f(x: {ty}) {{}}\nfun main() {{}}");
            assert_eq!(
                errors(&src).iter().map(|(c, _)| *c).collect::<Vec<_>>(),
                ["E0427"],
                "{ty}"
            );
            let d = &diagnostics(&src)[0];
            assert!(d.message.contains("`less` uses `Self`"), "{}", d.message);
            assert!(
                d.help.as_deref().unwrap_or("").contains("<T: Ordered>"),
                "{:?}",
                d.help
            );
        }
        ok(&format!(
            "{ORD}interface Grow {{ fun grow(mut self, by: i64); }}\nfun f(x: Grow) {{}}\nfun main() {{}}"
        ));
        assert_eq!(codes("fun f(x: Number) {}"), ["E0425"]);
        // Fields resolve before interface items: still E0427.
        assert_eq!(
            codes("struct S { o: Ordered }\nenum E { A(Ordered) }"),
            ["E0427", "E0427"]
        );
    }

    #[test]
    fn inference_reads_through_self() {
        let src = "interface Pick<T> { fun pick(self, other: Self): T; }\n\
            struct P { n: i64 }\n\
            impl P { fun pick(self, other: P): i64 = other.n; }\n\
            fun g<T, O: Pick<T>>(o: O): T = o.pick(o);\n\
            fun main() { let x = g(P { n: 1 }); }";
        assert_eq!(let_type(src, "x"), Type::I64);
    }
}
