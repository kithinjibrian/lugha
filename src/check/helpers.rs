use super::expr::Expect;
use super::generics::subst;
use super::infer::bind;
use super::results::RESULT;
use super::{Checker, Type, errors, qualify};
use crate::ast::{Expr, Ident};
use crate::modules::PRELUDE_PATH;

/// The prelude function behind method `method` on `receiver`, if it is a
/// helper: the array helpers keep their names, the others are prefixed by
/// the receiver's kind (`str_trim`, `option_map`, `result_map_err`).
fn helper_function(receiver: &Type, method: &str) -> Option<String> {
    const ARRAY: [&str; 6] = ["map", "filter", "fold", "sort_by", "any", "all"];
    const STRING: [&str; 6] = [
        "lines",
        "split",
        "trim",
        "contains",
        "starts_with",
        "ends_with",
    ];
    const CHAR: [&str; 7] = [
        "is_alphabetic",
        "is_digit",
        "is_whitespace",
        "is_upper",
        "is_lower",
        "to_upper",
        "to_lower",
    ];
    const OPTION: [&str; 5] = ["unwrap_or", "ok_or", "map", "is_some", "is_none"];
    const RESULTS: [&str; 6] = ["unwrap_or", "ok", "map", "map_err", "is_ok", "is_err"];
    let name = match receiver {
        Type::Array(e) if method == "join" && **e == Type::String => "str_join".to_string(),
        // The iterator sources (v10).
        Type::Array(_) if method == "iter" => "array_iter".to_string(),
        Type::String if method == "bytes" => "str_bytes".to_string(),
        // Characters (v14).
        Type::String if matches!(method, "chars" | "char_count") => format!("str_{method}"),
        Type::Char if CHAR.contains(&method) => format!("char_{method}"),
        Type::Array(_) if ARRAY.contains(&method) => method.to_string(),
        Type::String if STRING.contains(&method) => format!("str_{method}"),
        Type::Option(_) if OPTION.contains(&method) => format!("option_{method}"),
        Type::Enum(k, _) if k == RESULT && RESULTS.contains(&method) => format!("result_{method}"),
        _ => return None,
    };
    Some(name)
}

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
        let function = helper_function(receiver, &name.name)?;
        let key = qualify(&[PRELUDE_PATH.to_string()], &function);
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
    fn string_helpers_have_their_types() {
        let s = |body: &str, name: &str| {
            let_type(&format!("fun main() {{ let s = \" a,b \"; {body} }}"), name)
        };
        assert_eq!(s("let v = s.lines();", "v"), arr(Type::String));
        assert_eq!(s("let v = s.split(\",\");", "v"), arr(Type::String));
        assert_eq!(s("let v = s.trim();", "v"), Type::String);
        assert_eq!(s("let v = s.contains(\"a\");", "v"), Type::Bool);
        assert_eq!(s("let v = s.starts_with(\" \");", "v"), Type::Bool);
        assert_eq!(s("let v = s.ends_with(\" \");", "v"), Type::Bool);
        assert_eq!(s("let v = s.slice(0, 2);", "v"), Type::String);
        assert_eq!(s("let v = [\"a\", \"b\"].join(\"-\");", "v"), Type::String);
        let main = |body: &str| format!("fun main() {{ let s = \"x\"; {body} }}");
        assert_eq!(errors(&main("let v = [1].join(\"-\");"))[0].0, "E0415");
        assert_eq!(errors(&main("let v = s.trim(1);"))[0].0, "E0405");
        assert_eq!(errors(&main("let v = s.split(true);"))[0].0, "E0403");
        assert_eq!(errors(&main("let v = s.upper();"))[0].0, "E0415");
    }

    #[test]
    fn option_and_result_helpers_have_their_types() {
        let src = |body: &str| {
            format!(
                "fun main() {{ let o: Option<i64> = Some(1); let r: Result<i64, string> = Ok(2); {body} }}"
            )
        };
        let t = |body: &str, name: &str| let_type(&src(body), name);
        let result = |a: Type, b: Type| Type::Enum("$prelude.Result".into(), vec![a, b]);
        assert_eq!(t("let v = o.unwrap_or(0);", "v"), Type::I64);
        assert_eq!(
            t("let v = o.ok_or(\"none\");", "v"),
            result(Type::I64, Type::String)
        );
        assert_eq!(
            t("let v = o.map(fun(x) = \"{x}\");", "v"),
            Type::Option(Box::new(Type::String))
        );
        assert_eq!(t("let v = o.is_some();", "v"), Type::Bool);
        assert_eq!(t("let v = o.is_none();", "v"), Type::Bool);
        assert_eq!(t("let v = r.unwrap_or(0);", "v"), Type::I64);
        assert_eq!(t("let v = r.ok();", "v"), Type::Option(Box::new(Type::I64)));
        assert_eq!(
            t("let v = r.map(fun(x) = x + 1);", "v"),
            result(Type::I64, Type::String)
        );
        assert_eq!(
            t("let v = r.map_err(fun(e) = e.len);", "v"),
            result(Type::I64, Type::I64)
        );
        assert_eq!(t("let v = r.is_ok();", "v"), Type::Bool);
        assert_eq!(t("let v = r.is_err();", "v"), Type::Bool);
        ok(
            "fun num(s: string): Result<i64, string> { let n = parse_i64(s).ok_or(\"bad\")?; Ok(n) }\nfun main() {}",
        );
    }

    #[test]
    fn prelude_names_stay_out_of_programs() {
        ok("fun map(x: i64): i64 = x;\nfun filter() {}\nfun main() { let n = map(1); filter(); }");
        let src = "fun main() { let xs = [1]; let a = map(xs, fun(x: i64): i64 = x); }";
        assert_eq!(errors(src)[0].0, "E0301");
    }
}
