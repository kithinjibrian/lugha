//! Functions' and methods' resolved signatures by their declaration's name
//! (v16 draft decision 34): the language server fills a generic call's
//! instance into them for signature help.

use super::Type;
use super::generics::{Generic, subst};
use crate::ast::Param;

/// A function's or method's signature with its parameters' names.
#[derive(Debug, Clone, PartialEq)]
pub struct Sig {
    /// Its type parameters' names: an `impl`'s first, then its own.
    pub generics: Vec<String>,
    /// How many of `generics` are the `impl`'s, bound by the receiver.
    pub outer: usize,
    /// `Some(mutable)` for a method's `self` / `mut self`.
    pub receiver: Option<bool>,
    /// Each parameter's name and type, after any receiver.
    pub params: Vec<(String, Type)>,
    pub ret: Type,
}

impl Sig {
    /// The signature of a declaration with `declared` parameters of types
    /// `params`, returning `ret`.
    pub(super) fn of(
        generics: &[Generic],
        outer: usize,
        receiver: Option<bool>,
        (declared, params): (&[Param], &[Type]),
        ret: &Type,
    ) -> Sig {
        Sig {
            generics: generics.iter().map(|g| g.name.clone()).collect(),
            outer,
            receiver,
            params: declared
                .iter()
                .map(|p| p.name.name.clone())
                .zip(params.iter().cloned())
                .collect(),
            ret: ret.clone(),
        }
    }

    /// The signature with `args` for its own type parameters (after the
    /// `impl`'s), as `fun name(a: f64, b: f64): f64`; `None` unless there is
    /// one argument per own parameter. `owner` prefixes a method's name.
    pub fn filled(&self, name: &str, owner: Option<&str>, args: &[Type]) -> Option<String> {
        let own = &self.generics[self.outer.min(self.generics.len())..];
        if own.is_empty() || own.len() != args.len() {
            return None;
        }
        let generics: Vec<Generic> = own
            .iter()
            .map(|n| Generic {
                name: n.clone(),
                bounds: Default::default(),
            })
            .collect();
        let mut parts: Vec<String> = match self.receiver {
            Some(true) => vec!["mut self".into()],
            Some(false) => vec!["self".into()],
            None => Vec::new(),
        };
        parts.extend(
            self.params
                .iter()
                .map(|(n, t)| format!("{n}: {}", subst(t, &generics, args))),
        );
        let name = owner.map_or_else(|| name.to_string(), |o| format!("{o}.{name}"));
        let ret = subst(&self.ret, &generics, args);
        Some(match ret {
            Type::Unit => format!("fun {name}({})", parts.join(", ")),
            ret => format!("fun {name}({}): {ret}", parts.join(", ")),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::test_util;
    use crate::span::Span;

    const SRC: &str = "fun twice(n: i64): i64 = n * 2;
fun max<T: Ord>(a: T, b: T): T { if a > b { a } else { b } }
struct Box<T> { v: T }
impl Box<T> { fun map<U>(self, f: fun(T): U): Box<U> = Box { v: f(self.v) }; fun put(mut self, v: T) { self.v = v; } }
fun main() { }
";

    fn sig(word: &str) -> Sig {
        let partial = test_util::partial(SRC);
        let start = SRC.find(word).expect("declared");
        partial.signatures[&Span::new(start, start + word.len() - 1)].clone()
    }

    #[test]
    fn functions_keep_their_parameters_names_and_types() {
        let twice = sig("twice(");
        assert_eq!(twice.params, [("n".to_string(), Type::I64)]);
        assert_eq!(
            (twice.ret, twice.receiver, twice.generics.len()),
            (Type::I64, None, 0)
        );
        let max = sig("max<");
        assert_eq!(max.generics, ["T"]);
        assert_eq!(max.params[1], ("b".to_string(), Type::Param("T".into())));
    }

    #[test]
    fn methods_keep_their_receiver_and_both_kinds_of_generics() {
        let map = sig("map<");
        assert_eq!(
            (map.generics.clone(), map.outer, map.receiver),
            (vec!["T".into(), "U".into()], 1, Some(false))
        );
        let put = sig("put(");
        assert_eq!(put.receiver, Some(true));
    }

    #[test]
    fn a_generic_signature_fills_in_its_own_parameters() {
        let max = sig("max<");
        assert_eq!(
            max.filled("max", None, &[Type::F64]).as_deref(),
            Some("fun max(a: f64, b: f64): f64")
        );
        assert_eq!(max.filled("max", None, &[]), None);
        assert_eq!(sig("twice(").filled("twice", None, &[]), None);
        let map = sig("map<");
        assert_eq!(
            map.filled("map", Some("Box"), &[Type::String]).as_deref(),
            Some("fun Box.map(self, f: fun(T): string): Box<string>")
        );
    }
}
