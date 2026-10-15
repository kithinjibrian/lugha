//! User-defined order (v15 draft decisions 37–39, milestone 59): a struct
//! or enum with `fun cmp(self, other: Self): Ordering`, the prelude's
//! `Ordering`, is `Ord`. `<`, `<=`, `>` and `>=` on it call `cmp`.

use super::generics::{Generic, subst};
use super::instances::Instance;
use super::tuples::TUPLE;
use super::{Checker, Type};
use crate::ast::Expr;

/// The prelude's `Ordering` enum.
pub const ORDERING: &str = "$prelude.Ordering";

impl Checker {
    /// Whether struct or enum `ty` has `fun cmp(self, other: Self): Ordering`
    /// (the prelude's `Ordering`), from an `impl` whose bounds hold at `ty`.
    pub(super) fn has_cmp(&self, ty: &Type) -> bool {
        let (Type::Struct(_, args) | Type::Enum(_, args)) = ty else {
            return false;
        };
        let Some(m) = self.method(ty, "cmp") else {
            return false;
        };
        let outer = &m.generics[..m.outer];
        let shape = m.receiver == Some(false)
            && m.generics.len() == m.outer
            && m.params.len() == 1
            && subst(&m.params[0], outer, args) == *ty
            && matches!(&m.ret, Type::Enum(k, a) if k == ORDERING && a.is_empty());
        shape && outer.iter().zip(args).all(|(g, a)| self.bounds_hold(g, a))
    }

    /// Whether `arg` meets `g`'s bounds, judged without reporting: `Eq`,
    /// `Ord` and the built-in operator bounds; any other interface bound
    /// counts as unmet (an `impl` bounded that way gives no order).
    fn bounds_hold(&self, g: &Generic, arg: &Type) -> bool {
        (!g.bounds.eq || self.eq_ok(arg))
            && (!g.bounds.ord || self.ord_ok(arg))
            && g.bounds.interfaces.iter().all(|i| {
                self.builtin_operator(arg, &i.key)
                    .is_some_and(|unmet| unmet.is_empty())
            })
    }

    /// Records the `cmp` instances an ordering comparison at `ty` calls: a
    /// generic type's method, each element's for a tuple, and through a
    /// type parameter, whatever each instance's type needs.
    pub(super) fn order_uses(&mut self, expr: &Expr, ty: &Type) {
        match ty {
            Type::Param(p) => self.bound_used(expr, p, "cmp"),
            Type::Struct(key, args) if key.starts_with(TUPLE) => {
                for a in args.clone() {
                    self.order_uses(expr, &a);
                }
            }
            Type::Struct(_, args) | Type::Enum(_, args) => {
                if let Some(m) = self.method(ty, "cmp") {
                    self.used(expr, &m.key, &args.clone());
                }
            }
            _ => {}
        }
    }
}

impl Checker {
    /// The `cmp` instances ordering a value of concrete type `ty` needs: a
    /// generic type's own, and each element's for a tuple. A non-generic
    /// type's `cmp` is compiled anyway.
    pub(super) fn cmp_targets(&self, ty: &Type) -> Vec<Instance> {
        match ty {
            Type::Struct(key, args) if key.starts_with(TUPLE) => {
                args.iter().flat_map(|a| self.cmp_targets(a)).collect()
            }
            Type::Struct(_, args) | Type::Enum(_, args) if !args.is_empty() => self
                .method(ty, "cmp")
                .map(|m| Instance {
                    key: m.key,
                    args: args.clone(),
                })
                .into_iter()
                .collect(),
            _ => Vec::new(),
        }
    }
}

/// Help for a struct or enum that isn't `Ord`.
pub(super) fn cmp_help(ty: &Type) -> Option<String> {
    match ty {
        Type::Struct(key, _) if key.starts_with(TUPLE) => None,
        Type::Struct(..) | Type::Enum(..) => Some(format!(
            "give `{ty}` a `fun cmp(self, other: {ty}): Ordering` to order it"
        )),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    const SCORE: &str = "struct Score { loss: f64 }
        impl Score {
            fun cmp(self, o: Score): Ordering =
                if self.loss < o.loss { Ordering.Less } else if self.loss > o.loss { Ordering.Greater } else { Ordering.Equal };
        }
        fun largest<T: Ord>(a: T, b: T): T = if a < b { b } else { a };
        ";

    #[test]
    fn a_type_with_cmp_is_ord() {
        ok(&format!(
            "{SCORE}fun main() {{ let a = Score {{ loss: 1.0 }}; let b = Score {{ loss: 2.0 }};
                let x = a < b; let y = a >= b; let z = largest(a, b); let t = (a, 1) < (b, 0);
                let s = [b, a].sort(); }}"
        ));
        ok("enum Level { Low, High }
            impl Level { fun cmp(self, o: Level): Ordering = match (self, o) {
                (Low, High) => Ordering.Less, (High, Low) => Ordering.Greater, _ => Ordering.Equal }; }
            fun main() { let up = Level.Low < Level.High; }");
        ok("struct Box<T> { v: T }
            impl Box<T: Ord> { fun cmp(self, o: Box<T>): Ordering =
                if self.v < o.v { Ordering.Less } else if o.v < self.v { Ordering.Greater } else { Ordering.Equal }; }
            fun main() { let b = Box { v: 1 } < Box { v: 2 }; }");
    }

    #[test]
    fn other_shapes_are_not_ord() {
        for (decl, why) in [
            ("fun cmp(self, o: P): i64 = 0;", "returns i64"),
            (
                "fun cmp(mut self, o: P): Ordering = Ordering.Equal;",
                "mut self",
            ),
            (
                "fun cmp(self, o: i64): Ordering = Ordering.Equal;",
                "other type",
            ),
            ("fun size(self): i64 = 0;", "no cmp"),
        ] {
            let src = format!(
                "struct P {{ x: i64 }}\nimpl P {{ {decl} }}\nfun main() {{ let a = P {{ x: 1 }}; let b = a < a; }}"
            );
            assert_eq!(errors(&src), [("E0404", "a < a")], "{why}");
        }
        assert_eq!(
            errors(&format!(
                "struct P {{ x: i64 }}\n{}fun main() {{ let a = P {{ x: 1 }}; let c = largest(a, a); }}",
                &SCORE[SCORE.find("fun largest").unwrap()..]
            )),
            [("E0419", "largest(a, a)")]
        );
        assert_eq!(
            errors(&format!(
                "{SCORE}fun main() {{ let a = Score {{ loss: 1.0 }}; let b = [a] < [a]; }}"
            )),
            [("E0404", "[a] < [a]")]
        );
        assert_eq!(
            errors("struct P { x: i64 }\nfun main() { let s = [P { x: 1 }].sort(); }"),
            [("E0419", "[P { x: 1 }].sort()")]
        );
        assert_eq!(
            errors(
                "struct Box<T> { v: T }
                impl Box<T: Ord> { fun cmp(self, o: Box<T>): Ordering = Ordering.Equal; }
                struct Q { x: i64 }
                fun main() { let b = Box { v: Q { x: 1 } } < Box { v: Q { x: 2 } }; }"
            ),
            [("E0404", "Box { v: Q { x: 1 } } < Box { v: Q { x: 2 } }")]
        );
    }

    #[test]
    fn a_module_s_own_ordering_means_its_own() {
        let src = "enum Ordering { Less, More }
            struct P { x: i64 }
            impl P { fun cmp(self, o: P): Ordering = Ordering.More; }
            fun main() { let a = P { x: 1 }; let m = a.cmp(a);";
        ok(&format!("{src} }}"));
        assert_eq!(
            errors(&format!("{src} let b = a < a; }}")),
            [("E0404", "a < a")]
        );
    }
}
