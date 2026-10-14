//! Iterator helpers (v10 draft §4, milestone 36): `it.map(f)`, `it.sum()`
//! and the rest on anything `for … of` iterates, as calls of the prelude's
//! `iter_<name>` with the receiver converted to `Iter<T>`.

use super::expr::Expect;
use super::generics::subst;
use super::infer::bind;
use super::iterators::ITER;
use super::{Checker, Type, errors, qualify};
use crate::ast::{Expr, Ident};
use crate::modules::PRELUDE_PATH;

/// The helpers every iterator has, unless its type declares the name.
const HELPERS: [&str; 20] = [
    "map",
    "filter",
    "take",
    "skip",
    "enumerate",
    "collect",
    "count",
    "fold",
    "any",
    "all",
    "find",
    "sum",
    // v13
    "zip",
    "chain",
    "take_while",
    "skip_while",
    "flat_map",
    "min",
    "max",
    "position",
];

/// The prelude structs a program may name (v10), unless it declares the name.
pub(super) const ITER_TYPES: [&str; 16] = [
    "ArrayIter",
    "Bytes",
    "Range",
    "Map",
    "Filter",
    "Take",
    "Skip",
    "Enumerate",
    "Indexed",
    // v13
    "Pair",
    "Zip",
    "Chain",
    "TakeWhile",
    "SkipWhile",
    "FlatMap",
    // v14
    "Chars",
];

impl Checker {
    /// `base.name(args)` on `receiver`, an iterator without its own `name`:
    /// a call of the prelude's helper with `base`, converted to `Iter<T>`,
    /// first. `None` if `name` isn't a helper for it (E0415 follows).
    pub(super) fn iter_helper_call(
        &mut self,
        call: &Expr,
        (base, receiver): (&Expr, &Type),
        name: &Ident,
        args: &[Expr],
        expect: Option<&Expect>,
    ) -> Option<Type> {
        if !HELPERS.contains(&name.name.as_str()) || self.declares(receiver, &name.name) {
            return None;
        }
        let element = self.next_of(receiver)?.element;
        let function = match name.name.as_str() {
            "sum" if element.is_numeric() => format!("iter_sum_{element}"),
            "sum" => return None,
            other => format!("iter_{other}"),
        };
        let key = qualify(&[PRELUDE_PATH.to_string()], &function);
        let sig = self.functions.get(&key)?;
        let (generics, params, ret) = (sig.generics.clone(), sig.params.clone(), sig.ret.clone());
        if args.len() + 1 != params.len() {
            let d = errors::arity(&name.name, params.len() - 1, args.len(), call.span);
            self.report(d);
            return Some(self.unchecked_args(args));
        }
        let iter = Type::Interface(ITER.to_string(), vec![element]);
        self.convert(base, receiver.clone(), Some(&Expect::of(iter.clone())));
        let bound = bind(&params[0], &iter, &generics);
        let inputs: Vec<_> = args
            .iter()
            .zip(&params[1..])
            .map(|(a, t)| (a, Expect::of(t.clone())))
            .collect();
        self.callees.insert(call.id, key.clone());
        Some(
            match self.infer(call, &generics, bound, Some((&ret, expect)), &inputs) {
                Some(targs) => {
                    self.used(call, &key, &targs);
                    subst(&ret, &generics, &targs)
                }
                None => Type::Error,
            },
        )
    }

    /// Whether `ty` has its own method, interface item or bound item `name`.
    fn declares(&self, ty: &Type, name: &str) -> bool {
        match ty {
            Type::Struct(..) | Type::Enum(..) => self.method(ty, name).is_some(),
            _ => self.offered(ty, name).is_some(),
        }
    }

    /// A prelude iterator type's key for a bare `name` (v10).
    pub(super) fn prelude_iter_type(&self, name: &str) -> Option<String> {
        let key = qualify(&[PRELUDE_PATH.to_string()], name);
        (ITER_TYPES.contains(&name) && self.structs.contains_key(&key)).then_some(key)
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    const COUNT: &str = "struct Count { n: i64 }\n\
        impl Count { fun next(mut self): Option<i64> { self.n += 1; Some(self.n) } }\n";

    fn ty(body: &str) -> String {
        let src = format!("{COUNT}fun main() {{ let xs = [3, 1]; {body} }}");
        let_type(&src, "r").to_string()
    }

    #[test]
    fn helpers_and_sources_have_their_types() {
        assert_eq!(ty("let r = xs.iter();"), "$prelude.ArrayIter<i64>");
        assert_eq!(ty("let r = \"ab\".bytes();"), "$prelude.Bytes");
        assert_eq!(ty("let r = range(0, 3);"), "$prelude.Range");
        assert_eq!(
            ty("let r = xs.iter().map(fun(n) = \"{n}\");"),
            "$prelude.Map<i64, string>"
        );
        assert_eq!(
            ty("let r = xs.iter().filter(fun(n) = n > 1);"),
            "$prelude.Filter<i64>"
        );
        assert_eq!(ty("let r = xs.iter().collect();"), "i64[]");
        assert_eq!(ty("let r = xs.iter().count();"), "i64");
        assert_eq!(ty("let r = xs.iter().find(fun(n) = n > 1);"), "Option<i64>");
        assert_eq!(ty("let r = \"ab\".bytes().sum();"), "u8");
        assert_eq!(
            ty("let r = xs.iter().enumerate().find(fun(p) = p.i > 0);"),
            "Option<$prelude.Indexed<i64>>"
        );
    }

    #[test]
    fn any_iterator_takes_them_and_own_methods_win() {
        assert_eq!(ty("let r = Count { n: 0 }.take(2).sum();"), "i64");
        assert_eq!(
            ty("let it: Iter<i64> = xs.iter(); let r = it.count();"),
            "i64"
        );
        ok(&format!(
            "{COUNT}fun two<I: Iter<i64>>(it: I): i64[] = it.take(2).collect();\n\
             fun main() {{ let r = two(Count {{ n: 0 }}); }}"
        ));
        let own = "struct Own { n: i64 }\n\
            impl Own { fun next(mut self): Option<i64> = None; fun count(self): string = \"mine\"; }\n\
            fun main() { let r: string = Own { n: 1 }.count(); }";
        ok(own);
        assert_eq!(
            let_type(
                "fun range(a: i64, b: i64): i64 = a + b;\nfun main() { let r = range(1, 2); }",
                "r"
            ),
            Type::I64
        );
    }

    #[test]
    fn strings_loop_over_bytes() {
        assert_eq!(
            let_type(
                "fun main() { let mut r: u8 = 0; for b of \"ab\" { r = b; } }",
                "r"
            ),
            Type::U8
        );
    }

    #[test]
    fn misuse_is_reported() {
        let e = |body: &str| {
            errors(&format!("fun main() {{ {body} }}"))
                .into_iter()
                .map(|(c, t)| (c, t.to_string()))
                .collect::<Vec<_>>()
        };
        assert_eq!(
            e("let r = [\"a\"].iter().sum();"),
            [("E0415", "sum".to_string())]
        );
        assert_eq!(
            e("let r = (5).map(fun(x: i64): i64 = x);"),
            [("E0415", "map".to_string())]
        );
        assert_eq!(
            e("let r = [1].iter().take();"),
            [("E0405", "[1].iter().take()".to_string())]
        );
        assert_eq!(
            e("let r = [1].iter().take(true);"),
            [("E0403", "true".to_string())]
        );
    }

    #[test]
    fn the_v13_helpers_have_their_types() {
        assert_eq!(
            ty("let r = xs.iter().zip([\"a\"].iter()).find(fun(p) = true);"),
            "Option<$prelude.Pair<i64, string>>"
        );
        assert_eq!(
            ty(
                "let r = xs.iter().chain(xs.iter()).take_while(fun(n) = n > 0).skip_while(fun(n) = n > 2).collect();"
            ),
            "i64[]"
        );
        assert_eq!(
            ty("let r = xs.iter().flat_map(fun(n) = [\"s\"].iter()).collect();"),
            "string[]"
        );
        assert_eq!(ty("let r = xs.iter().min();"), "Option<i64>");
        assert_eq!(
            ty("let r = xs.iter().position(fun(n) = n > 1);"),
            "Option<i64>"
        );
        let e = |body: &str| {
            errors(&format!(
                "{COUNT}struct S {{ n: i64 }}\nfun main() {{ {body} }}"
            ))
            .into_iter()
            .map(|(c, _)| c)
            .collect::<Vec<_>>()
        };
        assert!(e("let r = [S { n: 1 }].iter().min();").contains(&"E0419"));
        assert!(e("let r = [1].iter().chain([\"a\"].iter());").contains(&"E0422"));
    }
}
