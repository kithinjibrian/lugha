//! Generic interfaces (v6 draft §4, milestone 23): an interface with its
//! type arguments, its items at those arguments, and bounds that pass
//! arguments (`S: Source<T>`, E0418, E0419).

use std::fmt;

use super::generics::{Generic, subst};
use super::interfaces::Requirement;
use super::{Checker, Type, errors};
use crate::ast::{Bound, TypeParam};

/// An interface with its type arguments: `Source<i64>`, or `Shape` with none.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Applied {
    /// The interface's key.
    pub key: String,
    /// Its type arguments, one per parameter; empty for a plain interface.
    pub args: Vec<Type>,
}

impl Applied {
    /// A plain interface, without arguments.
    pub(super) fn plain(key: &str) -> Applied {
        Applied {
            key: key.to_string(),
            args: Vec::new(),
        }
    }

    /// The same interface with `generics` replaced by `args` in its arguments.
    pub(super) fn subst(&self, generics: &[Generic], args: &[Type]) -> Applied {
        Applied {
            key: self.key.clone(),
            args: self.args.iter().map(|a| subst(a, generics, args)).collect(),
        }
    }
}

impl fmt::Display for Applied {
    /// As written: `Source<i64>`, `Shape`.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = Type::Interface(self.key.clone());
        if self.args.is_empty() {
            return write!(f, "{name}");
        }
        let args: Vec<String> = self.args.iter().map(Type::to_string).collect();
        write!(f, "{name}<{}>", args.join(", "))
    }
}

impl Checker {
    /// The items of `applied`, with the interface's parameters replaced by
    /// its arguments.
    pub(super) fn items(&self, applied: &Applied) -> Vec<Requirement> {
        let Some(info) = self.interfaces.get(&applied.key) else {
            return Vec::new();
        };
        info.items
            .iter()
            .map(|r| Requirement {
                params: r
                    .params
                    .iter()
                    .map(|t| subst(t, &info.generics, &applied.args))
                    .collect(),
                ret: subst(&r.ret, &info.generics, &applied.args),
                ..r.clone()
            })
            .collect()
    }

    /// The parameters of interface `key`; none for a plain one.
    pub(super) fn interface_generics(&self, key: &str) -> Vec<Generic> {
        self.interfaces
            .get(key)
            .map(|i| i.generics.clone())
            .unwrap_or_default()
    }

    /// `b`'s interface with its arguments, resolved without reporting: the
    /// declaration's checks report (`bound_args`). `None` for the wrong
    /// number of arguments.
    pub(super) fn applied_bound(&mut self, key: String, b: &Bound) -> Option<Applied> {
        let arity = self.interface_generics(&key).len();
        if b.args.len() != arity {
            return None;
        }
        let args = self.quietly(|c| b.args.iter().map(|a| c.resolve(a)).collect());
        Some(Applied { key, args })
    }

    /// E0418 for a bound with the wrong number of arguments and E0419 for
    /// an argument that misses the interface parameter's bound, reporting
    /// what resolving the arguments finds (E0305, E0313).
    pub(super) fn bound_args(&mut self, key: &str, b: &Bound) {
        let generics = self.interface_generics(key);
        let args: Vec<Type> = b.args.iter().map(|a| self.resolve(a)).collect();
        if args.len() != generics.len() {
            let d = errors::type_args(&b.name.name, generics.len(), args.len(), b.span);
            self.report(d);
            return;
        }
        if !args.contains(&Type::Error) {
            self.meets_bounds(&generics, &args, b.span);
        }
    }

    /// Runs `f`, dropping what it reports: for resolving declarations
    /// early, before the pass that checks them.
    pub(super) fn quietly<T>(&mut self, f: impl FnOnce(&mut Checker) -> T) -> T {
        let reported = self.diagnostics.len();
        let result = f(self);
        self.diagnostics.truncate(reported);
        result
    }

    /// The parameters `params` as written, with `outer` in scope for their
    /// bounds' arguments: names first, then each bound resolved quietly.
    pub(super) fn generics_with(
        &mut self,
        params: &[TypeParam],
        outer: &[Generic],
    ) -> Vec<Generic> {
        let names: Vec<Generic> = params
            .iter()
            .map(|p| Generic {
                name: p.name.name.clone(),
                bounds: Default::default(),
            })
            .collect();
        let saved = std::mem::replace(&mut self.type_params, [outer, &names].concat());
        let generics = params
            .iter()
            .map(|p| Generic {
                name: p.name.name.clone(),
                bounds: self.bounds_of(p),
            })
            .collect();
        self.type_params = saved;
        generics
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{diagnostics, errors, ok};

    const SOURCE: &str = "interface Source<T> { fun next(mut self): Option<T>; }\n\
        struct Count { n: i64, end: i64 }\n\
        impl Count {\n\
            fun next(mut self): Option<i64> {\n\
                if self.n == self.end { return None; }\n\
                self.n += 1;\n\
                Some(self.n)\n\
            }\n\
        }\n\
        struct Repeat<T> { x: T, left: i64 }\n\
        impl Repeat<T> {\n\
            fun next(mut self): Option<T> {\n\
                if self.left == 0 { return None; }\n\
                self.left -= 1;\n\
                Some(self.x)\n\
            }\n\
        }\n";

    fn e(body: &str) -> Vec<(&'static str, String)> {
        errors(&format!("{SOURCE}{body}\nfun main() {{}}"))
            .into_iter()
            .map(|(c, t)| (c, t.to_string()))
            .collect()
    }

    fn codes(body: &str) -> Vec<&'static str> {
        e(body).into_iter().map(|(c, _)| c).collect()
    }

    #[test]
    fn declarations_use_their_parameters() {
        ok(&format!("{SOURCE}fun main() {{}}"));
        assert_eq!(codes("interface I<T, T> { fun f(self): T; }"), ["E0316"]);
        assert_eq!(codes("interface I<Self> { fun f(self); }"), ["E0316"]);
        assert_eq!(codes("interface I<T> { fun f(self): Nope<T>; }"), ["E0305"]);
    }

    #[test]
    fn bounds_pass_arguments() {
        ok(&format!(
            "{SOURCE}fun a<S: Source<i64>>(s: S) {{}}\n\
             fun b<T, S: Source<T>>(s: S) {{}}\n\
             fun c<T, S: Source<T[]> + Eq>(s: S) {{}}\n\
             fun main() {{}}"
        ));
        assert_eq!(e("fun f<S: Source>(s: S) {}"), [("E0418", "Source".into())]);
        assert_eq!(
            e("fun f<S: Source<i64, i64>>(s: S) {}"),
            [("E0418", "Source<i64, i64>".into())]
        );
        assert_eq!(codes("fun f<S: Source<Nope>>(s: S) {}"), ["E0305"]);
        let sorted = "interface Sorted<T: Ord> { fun get(self): T; }\nstruct P {}\n";
        assert_eq!(
            codes(&format!("{sorted}fun f<S: Sorted<P>>(s: S) {{}}")),
            ["E0419"]
        );
        assert_eq!(
            codes("fun f<S: Source<i64> + Source<string>>(s: S) {}"),
            ["E0423"]
        );
    }

    #[test]
    fn calls_through_bounds_use_the_arguments() {
        ok(&format!(
            "{SOURCE}fun a<S: Source<i64>>(s: S): i64 {{ let mut t = s; t.next().unwrap_or(0) }}\n\
             fun b<T, S: Source<T>>(s: S, d: T): T {{ let mut t = s; t.next().unwrap_or(d) }}\n\
             fun main() {{}}"
        ));
        assert_eq!(
            codes("fun f<S: Source<i64>>(s: S): string { let mut t = s; t.next().unwrap_or(0) }"),
            ["E0403"]
        );
    }

    #[test]
    fn satisfaction_substitutes_the_arguments() {
        ok(&format!(
            "{SOURCE}fun a<S: Source<i64>>(s: S) {{}}\n\
             fun b<S: Source<string>>(s: S) {{}}\n\
             fun c<U: Source<i64>>(u: U) {{ a(u); }}\n\
             fun main() {{ a(Count {{ n: 0, end: 1 }}); b(Repeat {{ x: \"x\", left: 1 }}); }}"
        ));
        let src = format!(
            "{SOURCE}fun b<S: Source<string>>(s: S) {{}}\n\
             fun main() {{ b(Count {{ n: 0, end: 1 }}); }}"
        );
        assert_eq!(errors(&src).len(), 1);
        let d = &diagnostics(&src)[0];
        assert_eq!(d.code, "E0422");
        assert_eq!(d.message, "`Count` doesn't satisfy `Source<string>`");
        let help = d.help.as_deref().expect("a help");
        assert!(
            help.contains("`next` should be `fun next(mut self): Option<string>`"),
            "{help}"
        );
    }

    #[test]
    fn generic_interfaces_are_not_value_types_yet() {
        let src = format!("{SOURCE}fun f(s: Source<i64>) {{}}\nfun main() {{}}");
        assert_eq!(errors(&src), [("E0418", "Source<i64>")]);
        let d = &diagnostics(&src)[0];
        assert!(
            d.message.contains("can't be a value type yet"),
            "{}",
            d.message
        );
        assert_eq!(
            codes("fun f() { let s: Source<i64> = Count { n: 0, end: 1 }; }"),
            ["E0418"]
        );
        assert_eq!(e("fun f(s: Source) {}"), [("E0418", "Source".into())]);
    }
}
