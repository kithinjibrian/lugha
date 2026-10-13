use super::expr::Expect;
use super::generics::subst;
use super::infer::bind;
use super::{Checker, Type, errors, qualify};
use crate::ast::{Expr, Ident};
use crate::modules::PRELUDE_PATH;

/// The helpers, by method name; each is a generic function of the prelude.
const HELPERS: [&str; 6] = ["map", "filter", "fold", "sort_by", "any", "all"];

impl Checker {
    /// `xs.m(args)` where `xs` is an array and `m` a helper: a call of the
    /// prelude's `m` with `xs` first. Its element type is bound from the
    /// receiver, the rest inferred as for any generic call. `None` if `m`
    /// isn't a helper (or the prelude isn't loaded, as in `check()`).
    pub(super) fn helper_call(
        &mut self,
        call: &Expr,
        (receiver, name): (&Type, &Ident),
        args: &[Expr],
        expect: Option<&Expect>,
    ) -> Option<Type> {
        if !matches!(receiver, Type::Array(_)) || !HELPERS.contains(&name.name.as_str()) {
            return None;
        }
        let key = qualify(&[PRELUDE_PATH.to_string()], &name.name);
        let sig = self.functions.get(&key)?;
        let (generics, params, ret) = (sig.generics.clone(), sig.params.clone(), sig.ret.clone());
        if args.len() + 1 != params.len() {
            self.report(errors::arity(
                &name.name,
                params.len() - 1,
                args.len(),
                call.span,
            ));
            return Some(self.unchecked_args(args));
        }
        // The receiver was checked already: it only binds the element type.
        let bound = bind(&params[0], receiver, &generics);
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
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    fn ty(body: &str, name: &str) -> Type {
        let src = format!(
            "fun double(x: i64): i64 = x * 2;\nfun main() {{ let xs = [3, 1, 2]; {body} }}"
        );
        let_type(&src, name)
    }

    fn arr(t: Type) -> Type {
        Type::Array(Box::new(t))
    }

    #[test]
    fn helpers_have_their_types() {
        assert_eq!(ty("let a = xs.map(fun(x) = x * 2);", "a"), arr(Type::I64));
        assert_eq!(
            ty("let a = xs.map(fun(x) = \"{x}\");", "a"),
            arr(Type::String)
        );
        assert_eq!(
            ty("let a = xs.filter(fun(x) = x > 1);", "a"),
            arr(Type::I64)
        );
        assert_eq!(
            ty("let a = xs.fold(0, fun(acc, x) = acc + x);", "a"),
            Type::I64
        );
        assert_eq!(
            ty("let a = xs.fold(\"\", fun(acc, x) = acc + \"{x}\");", "a"),
            Type::String
        );
        assert_eq!(
            ty("let a = xs.sort_by(fun(a, b) = a < b);", "a"),
            arr(Type::I64)
        );
        assert_eq!(ty("let a = xs.any(fun(x) = x > 2);", "a"), Type::Bool);
        assert_eq!(ty("let a = xs.all(fun(x) = x > 0);", "a"), Type::Bool);
        assert_eq!(ty("let a = xs.map(double);", "a"), arr(Type::I64));
    }

    #[test]
    fn helpers_chain() {
        ok("struct P { name: string, age: i64 }\n\
            fun main() { let ps = [P { name: \"a\", age: 30 }]; \
            let names = ps.filter(fun(p) = p.age >= 18).map(fun(p) = p.name); let n = names.len; }");
    }

    #[test]
    fn wrong_arguments_are_reported_at_the_call() {
        let main = |body: &str| format!("fun main() {{ let xs = [1]; {body} }}");
        assert_eq!(errors(&main("let a = xs.map();"))[0].0, "E0405");
        assert_eq!(
            errors(&main("let a = xs.filter(fun(x) = x);"))[0].0,
            "E0403"
        );
        assert_eq!(
            errors(&main("let a = xs.map(fun(x: string) = x);"))[0].0,
            "E0403"
        );
    }

    #[test]
    fn prelude_names_stay_out_of_programs() {
        ok("fun map(x: i64): i64 = x;\nfun filter() {}\nfun main() { let n = map(1); filter(); }");
        let src = "fun main() { let xs = [1]; let a = map(xs, fun(x: i64): i64 = x); }";
        assert_eq!(errors(src)[0].0, "E0301");
    }
}
