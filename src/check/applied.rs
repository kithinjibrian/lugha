//! Generic interfaces (v6 draft §4, milestone 23): an interface with its
//! type arguments and its items at those arguments. Bounds that pass
//! arguments are in `bound_args.rs`.

use std::fmt;

use super::generics::{Generic, subst};
use super::interfaces::Requirement;
use super::{Checker, Type};
use crate::ast::TypeParam;

/// An interface with its type arguments: `Source<i64>`, or `Shape` with none.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Applied {
    /// The interface's key.
    pub key: String,
    /// Its type arguments, one per parameter; empty for a plain interface.
    pub args: Vec<Type>,
}

impl Applied {
    /// The interface of an interface value's type, `None` for any other type.
    pub(super) fn of(ty: &Type) -> Option<Applied> {
        let Type::Interface(key, args) = ty else {
            return None;
        };
        Some(Applied {
            key: key.clone(),
            args: args.clone(),
        })
    }

    /// The type of this interface's values (v6).
    pub(super) fn ty(&self) -> Type {
        Type::Interface(self.key.clone(), self.args.clone())
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
        write!(f, "{}", self.ty())
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
pub(super) mod tests {
    use crate::check::test_util::{diagnostics, errors, ok};

    pub(in crate::check) const SOURCE: &str = "interface Source<T> { fun next(mut self): Option<T>; }\n\
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

    pub(in crate::check) fn e(body: &str) -> Vec<(&'static str, String)> {
        errors(&format!("{SOURCE}{body}\nfun main() {{}}"))
            .into_iter()
            .map(|(c, t)| (c, t.to_string()))
            .collect()
    }

    pub(in crate::check) fn codes(body: &str) -> Vec<&'static str> {
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
    fn generic_interfaces_are_value_types() {
        ok(&format!(
            "{SOURCE}struct Pipe {{ from: Source<i64> }}\n\
             fun f(s: Source<i64>): Option<i64> {{ let mut t = s; t.next() }}\n\
             fun main() {{\n\
                 let s: Source<i64> = Count {{ n: 0, end: 1 }};\n\
                 let all: Source<i64>[] = [Count {{ n: 0, end: 1 }}, Repeat {{ x: 7, left: 1 }}];\n\
                 let p = Pipe {{ from: Count {{ n: 0, end: 1 }} }};\n\
                 let o: Option<Source<string>> = Some(Repeat {{ x: \"r\", left: 1 }});\n\
                 let n = f(s);\n\
             }}"
        ));
        assert_eq!(e("fun f(s: Source) {}"), [("E0418", "Source".into())]);
        assert_eq!(
            e("fun f(s: Source<i64, i64>) {}"),
            [("E0418", "Source<i64, i64>".into())]
        );
        let sorted = "interface Sorted<T: Ord> { fun get(self): T; }\nstruct P {}\n";
        assert_eq!(
            codes(&format!("{sorted}fun f(s: Sorted<P>) {{}}")),
            ["E0419"]
        );
    }

    #[test]
    fn conversion_and_calls_use_the_arguments() {
        let src =
            format!("{SOURCE}fun main() {{ let s: Source<string> = Count {{ n: 0, end: 1 }}; }}");
        let d = &diagnostics(&src)[0];
        assert_eq!(d.code, "E0422");
        let help = d.help.as_deref().expect("a help");
        assert!(help.contains("Option<string>"), "{help}");
        assert_eq!(
            codes("fun w(s: Source<string>) {}\nfun f(s: Source<i64>) { w(s); }"),
            ["E0403"]
        );
        assert_eq!(
            codes("fun f(s: Source<i64>): string { let mut t = s; t.next().unwrap_or(0) }"),
            ["E0403"]
        );
        assert_eq!(codes("fun f(s: Source<i64>) { s.next(); }"), ["E0501"]);
    }

    #[test]
    fn values_satisfy_bounds_and_match_type_patterns() {
        ok(&format!(
            "{SOURCE}fun a<S: Source<i64>>(s: S) {{}}\n\
             fun d(s: Source<i64>): i64 = match s {{ c: Count => c.end, _: Repeat<i64> => 1, _ => 0 }};\n\
             fun main() {{ let s: Source<i64> = Count {{ n: 0, end: 1 }}; a(s); let n = d(s); }}"
        ));
        assert_eq!(
            codes("fun d(s: Source<string>): i64 = match s { c: Count => 1, _ => 0 };"),
            ["E0426"]
        );
    }
}
