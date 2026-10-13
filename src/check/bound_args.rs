//! Bounds that pass arguments to a generic interface (v6 draft §4,
//! milestone 23): `S: Source<T>`, resolved quietly for `bounds_of` and
//! checked once at the declaration (E0418, E0419).

use super::applied::Applied;
use super::{Checker, Type, errors};
use crate::ast::Bound;

impl Checker {
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
}

#[cfg(test)]
mod tests {
    use crate::check::applied::tests::{SOURCE, codes, e};
    use crate::check::test_util::ok;

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
}
