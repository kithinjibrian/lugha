//! Iterators (v6 draft §4, milestone 24): the prelude's `Iter<T>`, and
//! `for x of e` over any `e` whose type has `fun next(mut self): Option<T>`.

use super::applied::Applied;
use super::{Binding, Checker, Type, errors};

use crate::ast::{Block, Expr, Ident};

/// The prelude's iterator interface.
pub(super) const ITER: &str = "$prelude.Iter";

/// What codegen needs for one iterator loop (`Checked.iterations`).
#[derive(Debug, Clone, PartialEq)]
pub struct Iteration {
    /// The type `next` yields inside its `Some`.
    pub element: Type,
    /// The bound interface whose default `next` a type parameter may use:
    /// its key and arguments, in the loop's own parameters.
    pub default: Option<(String, Vec<Type>)>,
}

/// The prelude's operator bounds (v15).
pub(super) fn is_operator_bound(name: &str) -> bool {
    matches!(
        name,
        "Add" | "Sub" | "Mul" | "Div" | "Rem" | "Neg" | "Num" | "Float"
    )
}

impl Checker {
    /// `key`, the interface a bare `name` would mean here, or the prelude's
    /// `Iter` when the module declares nothing by that name (v6).
    pub(super) fn or_prelude_interface(
        &self,
        key: String,
        module: Option<&Ident>,
        name: &str,
    ) -> String {
        let declared = self.interfaces.contains_key(&key) || self.named_type(&key).is_some();
        // `Iter` (v6), `Display` (v12) and the operator bounds (v15) are the
        // prelude's interfaces.
        let prelude = format!("$prelude.{name}");
        let known = (matches!(name, "Iter" | "Display") || is_operator_bound(name))
            && self.interfaces.contains_key(&prelude);
        if module.is_none() && known && !declared {
            return prelude;
        }
        key
    }

    /// `for var of iter { body }` where `iter`, of type `ty`, isn't an
    /// array: `var` is an immutable element, and the body may change `iter`,
    /// since the loop works on a copy.
    pub(super) fn iterator_loop(&mut self, var: &Ident, iter: &Expr, ty: &Type, body: &Block) {
        let element = self.iterator_element(iter, ty);
        self.push();
        self.declare(var, element, Binding::LoopVar);
        self.loop_body(body);
        self.pop();
    }

    /// The element type of a loop over `iter`, of type `ty`, which isn't an
    /// array: E0411 unless `ty` offers `fun next(mut self): Option<T>`.
    fn iterator_element(&mut self, iter: &Expr, ty: &Type) -> Type {
        let Some(iteration) = self.next_of(ty) else {
            self.report(errors::not_iterable(ty, iter.span));
            return Type::Error;
        };
        match ty {
            Type::Param(p) => self.bound_used(iter, p, "next"),
            // A non-generic type's `next` is listed only for the prelude's (v10).
            Type::Struct(owner, args) | Type::Enum(owner, args) => {
                let key = self.methods[owner]["next"].key.clone();
                self.used(iter, &key, args);
            }
            _ => {}
        }
        let element = iteration.element.clone();
        self.iterations.insert(iter.id, iteration);
        element
    }

    /// The `T` of `ty`'s `next(mut self): Option<T>`, and for a type
    /// parameter the bound interface whose default it may be.
    pub(super) fn next_of(&self, ty: &Type) -> Option<Iteration> {
        if matches!(ty, Type::Error | Type::Never) {
            return None;
        }
        let offered = self.offered(ty, "next")?;
        let Type::Option(element) = offered.ret else {
            return None;
        };
        if offered.receiver != Some(true) || !offered.params.is_empty() {
            return None;
        }
        let default = match ty {
            Type::Param(p) => self
                .bound_entry(p, "next")
                .filter(|(req, _)| req.default.is_some())
                .map(|(_, a): (_, Applied)| (a.key, a.args)),
            _ => None,
        };
        Some(Iteration {
            element: *element,
            default,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{diagnostics, errors, let_type, ok};

    const LIB: &str = "struct Count { n: i64 }\n\
        impl Count { fun next(mut self): Option<i64> = None; }\n\
        struct Repeat<T> { x: T }\n\
        impl Repeat<T> { fun next(mut self): Option<T> = Some(self.x); }\n\
        interface Source<T> { fun next(mut self): Option<T>; }\n\
        struct Stuck { n: i64 }\n\
        impl Stuck { fun next(self): Option<i64> = None; }\n\
        struct Takes { n: i64 }\n\
        impl Takes { fun next(mut self, k: i64): Option<i64> = None; }\n\
        struct Plain { n: i64 }\n\
        impl Plain { fun next(mut self): i64 = 0; }\n";

    fn element(setup: &str, iter: &str) -> Type {
        let src = format!("{LIB}fun main() {{ {setup} for x of ({iter}) {{ let y = x; }} }}");
        let_type(&src, "y")
    }

    fn codes(body: &str) -> Vec<&'static str> {
        let src = format!("{LIB}fun main() {{ {body} }}");
        errors(&src).into_iter().map(|(c, _)| c).collect()
    }

    #[test]
    fn whatever_has_next_iterates() {
        assert_eq!(element("", "Count { n: 0 }"), Type::I64);
        assert_eq!(element("", "Repeat { x: \"s\" }"), Type::String);
        assert_eq!(
            element("let it: Iter<i64> = Count { n: 0 };", "it"),
            Type::I64
        );
        assert_eq!(
            element("let s: Source<string> = Repeat { x: \"s\" };", "s"),
            Type::String
        );
        ok(&format!(
            "{LIB}fun f<T, I: Iter<T>>(it: I): T[] {{ let mut out: T[] = []; for x of it {{ out.push(x); }} out }}\n\
             fun main() {{ let xs = f(Count {{ n: 0 }}); }}"
        ));
    }

    #[test]
    fn anything_else_is_e0411_with_help() {
        for iter in ["5", "Stuck { n: 0 }", "Takes { n: 0 }", "Plain { n: 0 }"] {
            let body = format!("for x of ({iter}) {{}}");
            assert_eq!(codes(&body), ["E0411"], "{iter}");
            let src = format!("{LIB}fun main() {{ {body} }}");
            let help = diagnostics(&src)[0].help.clone().expect("a help");
            assert!(
                help.contains("fun next(mut self): Option<T>"),
                "{iter}: {help}"
            );
        }
    }

    #[test]
    fn the_loop_works_on_a_copy() {
        assert_eq!(codes("for x of (Count { n: 0 }) { x = 1; }"), ["E0501"]);
        ok(&format!(
            "{LIB}fun main() {{ let mut c = Count {{ n: 0 }}; for x of c {{ c.n = 5; c = Count {{ n: 1 }}; }} }}"
        ));
    }

    #[test]
    fn a_modules_own_iter_hides_the_preludes() {
        ok(&format!(
            "{LIB}interface Iter {{ fun size(self): i64; }}\n\
             fun f<T: Iter>(x: T): i64 = x.size();\n\
             fun main() {{ for x of (Count {{ n: 0 }}) {{}} }}"
        ));
        ok(&format!(
            "{LIB}fun f(it: Iter<i64>) {{}}\nfun g<I: Iter<i64>>(it: I) {{}}\nfun main() {{}}"
        ));
    }
}
