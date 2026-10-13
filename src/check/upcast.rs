//! Converting between interfaces (v6 draft §4, milestone 26): a value of
//! interface type `J` becomes an `I` when `J` declares every item of `I`,
//! defaulted ones included, the same way.

use super::applied::Applied;
use super::{Checker, Type};

impl Checker {
    /// What `from` lacks to convert to `to`, both interfaces at their
    /// arguments, one entry per item: empty when it declares them all.
    pub(super) fn undeclared(&self, from: &Applied, to: &Applied) -> Vec<String> {
        let declared = self.items(from);
        let mut problems = Vec::new();
        for req in self.items(to) {
            match declared.iter().find(|d| d.name == req.name) {
                Some(d) if d.same(&req) => {}
                Some(_) => problems.push(format!("`{}` is declared differently", req.name)),
                None => problems.push(format!("no `{}`", req.name)),
            }
        }
        problems
    }

    /// True if a value of type `from`, another interface than `to`, converts
    /// to `to`; E0422 naming what it lacks otherwise.
    pub(super) fn upcasts(&mut self, from: &Type, to: &Applied, span: crate::span::Span) -> bool {
        let source = Applied::of(from).expect("an interface type");
        let problems = self.undeclared(&source, to);
        if problems.is_empty() {
            return true;
        }
        self.report(super::errors::undeclared(
            from,
            &to.to_string(),
            &problems,
            span,
        ));
        false
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{diagnostics, errors, ok};

    const PETS: &str = "interface Named { fun name(self): string; }\n\
        interface Pet { fun name(self): string; fun sound(self): string; }\n\
        interface Loud { fun name(self): string; fun sound(self): string = \"!\"; }\n\
        interface Odd { fun name(self): i64; }\n\
        interface Source<T> { fun next(mut self): Option<T>; fun size(self): i64; }\n\
        interface Sized { fun size(self): i64; }\n\
        struct Dog { n: string }\n\
        impl Dog { fun name(self): string = self.n; fun sound(self): string = \"woof\"; }\n\
        struct Wrap { n: Named }\n";

    fn codes(body: &str) -> Vec<&'static str> {
        let src = format!("{PETS}{body}\nfun main() {{}}");
        errors(&src).into_iter().map(|(c, _)| c).collect()
    }

    #[test]
    fn a_value_converts_to_an_interface_it_declares() {
        ok(&format!(
            "{PETS}fun take(n: Named) {{}}\n\
             fun back(p: Pet): Named = p;\n\
             fun all(p: Pet): Named[] = [p, Dog {{ n: \"d\" }}];\n\
             fun some(p: Pet): Option<Named> = Some(p);\n\
             fun field(p: Pet): Wrap = Wrap {{ n: p }};\n\
             fun sized(s: Source<i64>): Sized = s;\n\
             fun main() {{ let p: Pet = Dog {{ n: \"d\" }}; let n: Named = p; take(p); }}"
        ));
    }

    #[test]
    fn a_missing_or_different_item_is_e0422() {
        assert_eq!(codes("fun f(n: Named): Pet = n;"), ["E0422"]);
        assert_eq!(codes("fun f(o: Odd): Named = o;"), ["E0422"]);
        // Defaults count: `Named` lacks `Loud`'s defaulted `sound`.
        assert_eq!(codes("fun f(n: Named): Loud = n;"), ["E0422"]);
        let src = format!("{PETS}fun f(n: Named): Pet = n;\nfun main() {{}}");
        let d = &diagnostics(&src)[0];
        assert_eq!(d.message, "`Named` doesn't declare everything `Pet` needs");
        assert_eq!(d.help.as_deref(), Some("no `sound`"));
    }

    #[test]
    fn other_arguments_and_containers_stay_e0403() {
        assert_eq!(
            codes("fun f(s: Source<i64>): Source<string> = s;"),
            ["E0403"]
        );
        assert_eq!(codes("fun f(ps: Pet[]): Named[] = ps;"), ["E0403"]);
    }

    #[test]
    fn question_mark_converts_interface_errors() {
        let base = "interface Problem { fun text(self): string; }\n\
            interface Parse { fun text(self): string; fun at(self): i64; }\n\
            interface Other { fun at(self): i64; }\n\
            fun p(): Result<i64, Parse> = Ok(1);\n\
            fun q(): Result<i64, Other> = Ok(1);\n";
        ok(&format!(
            "{base}fun r(): Result<i64, Problem> {{ let n = p()?; Ok(n) }}\nfun main() {{}}"
        ));
        let src = format!(
            "{base}fun r(): Result<i64, Problem> {{ let n = q()?; Ok(n) }}\nfun main() {{}}"
        );
        assert_eq!(
            errors(&src).iter().map(|(c, _)| *c).collect::<Vec<_>>(),
            ["E0424"]
        );
    }
}
