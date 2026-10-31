use super::expr::Expect;
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind, Ident};

/// The prelude's `Result`, by key.
pub const RESULT: &str = "$prelude.Result";

impl Checker {
    /// `Ok(x)` or `Err(e)` without a prefix: the prelude's `Result`, built
    /// like `E.V(x)`. `None` if `callee` isn't one (or a local shadows it).
    pub(super) fn result_construction(
        &mut self,
        call: &Expr,
        callee: &Expr,
        args: &[Expr],
        expect: Option<&super::expr::Expect>,
    ) -> Option<Type> {
        let ExprKind::Name(name) = &callee.kind else {
            return None;
        };
        if !matches!(name.as_str(), "Ok" | "Err")
            || self.local(name).is_some()
            || !self.enums.contains_key(RESULT)
        {
            return None;
        }
        self.record(callee, Type::Unit);
        let variant = Ident {
            name: name.clone(),
            span: callee.span,
        };
        Some(self.construct((call, call.span), RESULT, &variant, args, expect))
    }

    /// True while checking the prelude, which may declare the built-in names.
    pub(super) fn in_prelude(&self) -> bool {
        self.module.len() == 1 && self.module[0] == crate::modules::PRELUDE_PATH
    }

    /// The prelude's key for a type name no module declares: `Result`,
    /// `Ordering` (v15), `Map` and `Set` (v17), or an iterator type (v10).
    pub(super) fn prelude_type(&self, name: &str) -> Option<String> {
        if name == "Result" && self.enums.contains_key(RESULT) {
            return Some(RESULT.to_string());
        }
        if name == "Ordering" && self.enums.contains_key(super::ordering::ORDERING) {
            return Some(super::ordering::ORDERING.to_string());
        }
        // Maps and sets (v17, milestone 101).
        for key in [super::maps::MAP, super::maps::SET] {
            if key.strip_prefix("$prelude.") == Some(name) && self.structs.contains_key(key) {
                return Some(key.to_string());
            }
        }
        self.prelude_iter_type(name)
    }

    /// `e?`: on `Result<T, E>` in a function returning `Result<U, E>`, or on
    /// `Option<T>` in one returning `Option<U>`, the `T`; E0424 otherwise.
    pub(super) fn try_expr(&mut self, expr: &Expr, inner: &Expr, expect: Option<&Expect>) -> Type {
        // The value wanted from `e?` is what `e` holds (v17, milestone 140):
        // `let p: Person = json.decode(v)?;` infers `decode`'s `T`.
        let hint = expect.and_then(|e| self.try_hint(&e.ty));
        self.leaves_defer("?", expr.span);
        let ty = self.value(inner, hint.as_ref());
        if matches!(ty, Type::Error | Type::Never) || self.closure_return_unknown(expr.span) {
            return Type::Error;
        }
        let ret = self.ret.clone();
        let is_result = |t: &Type| matches!(t, Type::Enum(k, _) if k == RESULT);
        let help = match (&ty, &ret) {
            (Type::Enum(_, a), Type::Enum(_, b)) if is_result(&ty) && is_result(&ret) => {
                if a[1] == b[1] || b[1] == Type::Error || self.try_converts(inner, &a[1], &b[1]) {
                    return a[0].clone();
                }
                format!("the error types must match: `{}` and `{}`", a[1], b[1])
            }
            (Type::Option(t), Type::Option(_)) => return (**t).clone(),
            _ if !is_result(&ty) && !matches!(ty, Type::Option(_)) => {
                "`?` works on `Result` and `Option`".to_string()
            }
            // `main` has nowhere to send a failure (its own body, not a closure's).
            _ if self.owner == "main" && self.closures.is_empty() => {
                "`main` can't return an error: handle it with `match`".to_string()
            }
            (Type::Option(_), _) if is_result(&ret) => {
                "a missing value isn't an error by itself: handle the `Option` with `match`"
                    .to_string()
            }
            _ => format!("the function returns {ret}, so `?` has nowhere to send a failure"),
        };
        self.report(errors::cant_try(expr.span, &help));
        Type::Error
    }

    /// What `e` in `e?` should be when `e?` should be `want`: a `Result`
    /// of it (its error left open, as conversions may change it) in a
    /// function returning a `Result`, an `Option` of it in one returning an
    /// `Option`.
    fn try_hint(&self, want: &Type) -> Option<Expect> {
        if matches!(want, Type::Error | Type::Never) {
            return None;
        }
        match &self.ret {
            Type::Enum(k, _) if k == RESULT => Some(Expect::of(Type::Enum(
                RESULT.to_string(),
                vec![want.clone(), Type::Error],
            ))),
            Type::Option(_) => Some(Expect::of(Type::Option(Box::new(want.clone())))),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{diagnostics, errors, let_type, ok};

    const HALF: &str = "fun half(n: i64): Result<i64, string> = if n % 2 == 0 { Ok(n / 2) } else { Err(\"odd\") };\n";

    fn codes(src: &str) -> Vec<&'static str> {
        errors(src).into_iter().map(|(c, _)| c).collect()
    }

    fn result(t: Type, e: Type) -> Type {
        Type::Enum("$prelude.Result".into(), vec![t, e])
    }

    #[test]
    fn question_mark_passes_the_wanted_type_on_v17() {
        ok("fun none<T>(): Result<T, string> = Err(\"x\");\n\
            fun nothing<T>(): Option<T> = None;\n\
            fun f(): Result<i64, string> { let a: i64 = none()?; Ok(a) }\n\
            fun g(): Option<string> { let s: string = nothing()?; Some(s) }\n\
            fun main() {}");
        // A concrete operand still has to match: one error, not two.
        assert_eq!(
            codes(&format!(
                "{HALF}fun f(): Result<i64, string> {{ let s: string = half(2)?; Ok(1) }}\nfun main() {{}}"
            )),
            ["E0403"]
        );
    }

    #[test]
    fn ok_and_err_build_results() {
        let ty = let_type("fun main() { let r: Result<i64, string> = Ok(1); }", "r");
        assert_eq!(ty, result(Type::I64, Type::String));
        ok("fun main() { let r: Result<i64, string> = Err(\"no\"); let s = r == Ok(2); }");
        assert_eq!(codes("fun main() { let r = Ok(1); }"), ["E0412"]);
        assert_eq!(
            codes("fun main() { let r: Result<i64, string> = Err(); }"),
            ["E0416"]
        );
        let d = diagnostics("fun main() { let r: Result<i64, string> = true; }");
        assert_eq!(d[0].message, "expected Result<i64, string>, found bool");
    }

    #[test]
    fn results_match_like_any_enum() {
        ok(&format!(
            "{HALF}fun main() {{ let n = match half(4) {{ Ok(n) => n, Err(_) => 0 }}; }}"
        ));
        assert_eq!(
            codes(&format!(
                "{HALF}fun main() {{ let n = match half(4) {{ Ok(n) => n }}; }}"
            )),
            ["E0417"]
        );
    }

    #[test]
    fn the_names_are_reserved() {
        assert_eq!(codes("struct Result {}\nfun main() {}"), ["E0302"]);
        assert_eq!(codes("fun Ok() {}\nfun main() {}"), ["E0302"]);
        assert_eq!(codes("fun main() { let Result = 1; }"), ["E0311"]);
        assert_eq!(codes("fun f(r: Result<i64>) {}\nfun main() {}"), ["E0418"]);
    }

    #[test]
    fn try_gives_the_value_or_returns() {
        let q = format!(
            "{HALF}fun quarter(n: i64): Result<i64, string> {{ let h = half(n)?; half(h) }}\nfun main() {{}}"
        );
        ok(&q);
        ok(
            "fun first(xs: i64[]): Option<i64> { let mut ys = xs; let x = ys.pop()?; Some(x) }\nfun main() {}",
        );
        assert_eq!(
            let_type(
                &format!(
                    "{HALF}fun g(): Result<bool, string> {{ let h = half(2)?; Ok(true) }}\nfun main() {{ let x = 1; }}"
                ),
                "x"
            ),
            Type::I64
        );
    }

    #[test]
    fn try_where_it_cant_return_is_e0424() {
        let cases = [
            "fun f(): Result<i64, string> { let n = 1?; Ok(n) }",
            "fun f(xs: i64[]): Result<i64, string> { let mut ys = xs; let x = ys.pop()?; Ok(x) }",
            "fun f(): Result<i64, i64> { let h = half(2)?; Ok(h) }",
            "fun f() { let h = half(2)?; }",
        ];
        for body in cases {
            let src = format!("{HALF}{body}\nfun main() {{}}");
            assert_eq!(codes(&src), ["E0424"], "{body}");
        }
        let src = format!("{HALF}fun main() {{ let h = half(2)?; }}");
        assert_eq!(codes(&src), ["E0424"]);
        assert!(
            diagnostics(&src)[0]
                .help
                .as_deref()
                .unwrap()
                .contains("match")
        );
    }

    #[test]
    fn try_in_a_closure_returns_from_the_closure() {
        ok(&format!(
            "{HALF}fun main() {{ let f = fun(n: i64): Result<i64, string> {{ let h = half(n)?; Ok(h + 1) }}; }}"
        ));
        assert_eq!(
            codes(&format!(
                "{HALF}fun main() {{ let f = fun(n: i64) {{ let h = half(n)?; }}; }}"
            )),
            ["E0412"]
        );
    }

    #[test]
    fn question_mark_converts_into_an_interface_error() {
        let base = "interface Error { fun message(self): string; }\n\
            struct ParseError { text: string }\n\
            impl ParseError { fun message(self): string = self.text; }\n\
            interface Other { fun message(self): string; }\n\
            fun parse(s: string): Result<i64, ParseError> = Err(ParseError { text: s });\n";
        ok(&format!(
            "{base}fun read(s: string): Result<i64, Error> {{ let n = parse(s)?; Ok(n) }}\nfun main() {{}}"
        ));
        let plain = format!(
            "{base}fun half(n: i64): Result<i64, string> = Ok(n);\nfun read(): Result<i64, Error> {{ let n = half(1)?; Ok(n) }}\nfun main() {{}}"
        );
        assert_eq!(codes(&plain), ["E0424"]);
        // v6: an interface error converts when it declares every item.
        let other = format!(
            "{base}fun get(o: Other): Result<i64, Other> = Err(o);\nfun read(o: Other): Result<i64, Error> {{ let n = get(o)?; Ok(n) }}\nfun main() {{}}"
        );
        ok(&other);
    }
}
