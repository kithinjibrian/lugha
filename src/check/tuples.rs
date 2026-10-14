//! Tuples (v14 draft §4–5, milestone 47): `(A, B, …)` is the generic
//! struct `$tuple<n>` with fields `0`, `1`, …, registered on first use, so
//! layout, copies, `==`, `t.0` and instances reuse the struct paths. Here:
//! the type, literals, patterns, `<`, and irrefutable `let`/`for` (E0433).

use super::expr::Expect;
use super::generics::{Bounds, Generic};
use super::patterns::{Bindings, Ctor, Pat};
use super::structs::StructInfo;
use super::{Checker, Type, errors};
use crate::ast::{Expr, Pattern};
use crate::span::Span;

/// The start of every tuple struct's key: `$tuple2`, `$tuple3`, ….
pub const TUPLE: &str = "$tuple";

/// The element types of a tuple type, `None` for any other type.
pub fn elements(ty: &Type) -> Option<&[Type]> {
    match ty {
        Type::Struct(key, args) if key.starts_with(TUPLE) => Some(args),
        _ => None,
    }
}

impl Checker {
    /// The tuple type of `elements`, registering its struct on first use.
    pub(super) fn tuple_type(&mut self, elements: Vec<Type>) -> Type {
        let n = elements.len();
        let key = format!("{TUPLE}{n}");
        if !self.structs.contains_key(&key) {
            let generics = (0..n)
                .map(|i| Generic {
                    name: format!("T{i}"),
                    bounds: Bounds::default(),
                })
                .collect();
            let fields = (0..n)
                .map(|i| (i.to_string(), Type::Param(format!("T{i}"))))
                .collect();
            let info = StructInfo {
                fields,
                span: Span::new(0, 0),
                generics,
            };
            self.structs.insert(key.clone(), info);
        }
        Type::Struct(key, elements)
    }

    /// `(a, b, …)`: each element against the expected tuple's, if any.
    pub(super) fn tuple_literal(&mut self, given: &[Expr], expect: Option<&Expect>) -> Type {
        let wanted = expect
            .and_then(|e| elements(&e.ty).map(<[Type]>::to_vec))
            .filter(|ts| ts.len() == given.len());
        let types = given
            .iter()
            .enumerate()
            .map(|(i, e)| match &wanted {
                Some(ts) => self.expect_type(e, &Expect::of(ts[i].clone())),
                None => self.value(e, None),
            })
            .collect();
        self.tuple_type(types)
    }

    /// `(p, q, …)` against `ty`: each element's ways to match, combined.
    pub(super) fn tuple_pattern(
        &mut self,
        pattern: &Pattern,
        given: &[Pattern],
        ty: &Type,
        bindings: &mut Bindings,
    ) -> Vec<Pat> {
        let Some(types) = elements(ty)
            .filter(|ts| ts.len() == given.len())
            .map(<[Type]>::to_vec)
        else {
            if !matches!(ty, Type::Error | Type::Never) {
                self.report(errors::pattern_mismatch(ty, pattern.span));
            }
            return vec![Pat::Wild];
        };
        let mut rows: Vec<Vec<Pat>> = vec![Vec::new()];
        for (p, t) in given.iter().zip(&types) {
            let ways = self.pattern_at(p, t, bindings, false);
            rows = rows
                .iter()
                .flat_map(|row| {
                    ways.iter()
                        .map(move |w| [row.clone(), vec![w.clone()]].concat())
                })
                .collect();
        }
        let ctor = Ctor::Variant(0, "()".to_string());
        rows.into_iter()
            .map(|subs| Pat::Ctor(ctor.clone(), subs))
            .collect()
    }

    /// E0433 unless the patterns `pats` match every value of `ty`.
    pub(super) fn always_matches(&mut self, pats: &[Pat], ty: &Type, span: Span) {
        if matches!(ty, Type::Error | Type::Never) {
            return;
        }
        let rows: Vec<Vec<Pat>> = pats.iter().map(|p| vec![p.clone()]).collect();
        if self
            .space()
            .useful(&rows, &[Pat::Wild], std::slice::from_ref(ty))
        {
            self.report(errors::refutable(span));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, let_type, ok};

    fn ty(body: &str) -> String {
        let_type(&format!("fun main() {{ {body} }}"), "r").to_string()
    }

    #[test]
    fn tuples_have_types() {
        assert_eq!(ty("let r = (1, \"a\");"), "(i64, string)");
        assert_eq!(ty("let t = (1, (true, 2.5)); let r = t.1.0;"), "bool");
        assert_eq!(ty("let r: (u8, i64) = (1, 2);"), "(u8, i64)");
        ok("fun divmod(a: i64, b: i64): (i64, i64) = (a / b, a % b);\n\
            fun max<T: Ord>(a: T, b: T): T = if a < b { b } else { a };\n\
            fun main() { let (q, r) = divmod(7, 2); let m = max((1, 2), (1, 3)); \
            let mut t = (1, 2); t.0 = 5; for (i, w) of [(1, \"a\")] { println(w); } }");
    }

    #[test]
    fn misuse_is_reported() {
        let codes = |body: &str| {
            errors(&format!("fun main() {{ {body} }}"))
                .into_iter()
                .map(|(c, _)| c)
                .collect::<Vec<_>>()
        };
        assert_eq!(codes("let t = (1, 2); let (1, x) = t;"), ["E0433"]);
        assert_eq!(
            codes("let r = match (true, 1) { (true, _) => 1 };"),
            ["E0417"]
        );
        assert_eq!(codes("let b = ([1], 2) < ([1], 3);"), ["E0404"]);
        assert_eq!(codes("let t = (1, 2); let z = t.5;"), ["E0410"]);
    }
}
