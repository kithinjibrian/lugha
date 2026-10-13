//! Bounds (v2 draft §4, PRP-035): what `Eq` and `Ord` allow, E0419 for a
//! type argument that misses its bound, and the parameter a comparison needs one on.

use super::generics::{Bounds, Generic};
use super::{Checker, Type, errors};
use crate::span::Span;

impl Checker {
    /// E0419 for the first argument that doesn't meet its parameter's bounds.
    pub(super) fn meets_bounds(&mut self, generics: &[Generic], args: &[Type], span: Span) -> bool {
        for (g, arg) in generics.iter().zip(args) {
            let unmet = if g.bounds.eq && !self.eq_ok(arg) {
                Some("Eq")
            } else if g.bounds.ord && !self.ord_ok(arg) {
                Some("Ord")
            } else {
                None
            };
            if let Some(bound) = unmet {
                self.report(errors::unmet_bound(arg, bound, span));
                return false;
            }
        }
        true
    }

    /// The bounds of the parameter `name` in scope.
    fn bounds(&self, name: &str) -> Bounds {
        self.type_params
            .iter()
            .find(|g| g.name == name)
            .map_or_else(Bounds::default, |g| g.bounds)
    }

    /// True if `==` works on `ty`: always, except through a parameter
    /// without `Eq` (v2 draft §4).
    pub(super) fn eq_ok(&self, ty: &Type) -> bool {
        match ty {
            Type::Param(n) => self.bounds(n).eq,
            Type::Array(inner) | Type::Option(inner) => self.eq_ok(inner),
            Type::Struct(_, args) | Type::Enum(_, args) => args.iter().all(|a| self.eq_ok(a)),
            _ => true,
        }
    }

    /// True if `<` works on `ty`: numbers, strings, and parameters with `Ord`.
    pub(super) fn ord_ok(&self, ty: &Type) -> bool {
        match ty {
            Type::Param(n) => self.bounds(n).ord,
            _ => ty.is_numeric() || *ty == Type::String,
        }
    }

    /// The parameter a comparison needs a bound on: `T` in `T`, `T[]` or
    /// `Pair<T, i64>` when `T` lacks `Eq` (or `Ord`, for `ord`).
    pub(super) fn missing_bound(&self, ty: &Type, ord: bool) -> Option<String> {
        match ty {
            Type::Param(n) if (ord && !self.bounds(n).ord) || (!ord && !self.bounds(n).eq) => {
                Some(n.clone())
            }
            Type::Array(inner) | Type::Option(inner) => self.missing_bound(inner, ord),
            Type::Struct(_, args) | Type::Enum(_, args) => {
                args.iter().find_map(|a| self.missing_bound(a, ord))
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{diagnostics, errors, let_type, ok};

    #[test]
    fn type_arguments_must_meet_their_bounds() {
        let set = "struct Set<T: Ord> { items: T[] }\n";
        assert_eq!(
            errors(&format!("{set}fun f(s: Set<i64[]>) {{}}\nfun main() {{}}")),
            [("E0419", "Set<i64[]>")]
        );
        ok(&format!(
            "{set}fun f(s: Set<string>) {{}}\nfun g<U: Ord>(s: Set<U>) {{}}\nfun main() {{}}"
        ));
        assert_eq!(
            errors(&format!("{set}fun g<U>(s: Set<U>) {{}}\nfun main() {{}}")),
            [("E0419", "Set<U>")]
        );
        let d = diagnostics(&format!("{set}fun f(s: Set<i64[]>) {{}}\nfun main() {{}}"));
        assert_eq!(d[0].message, "`i64[]` doesn't satisfy `Ord`");
    }

    #[test]
    fn a_bare_parameter_allows_only_what_its_bounds_give() {
        let d = diagnostics("fun same<T>(a: T, b: T): bool = a == b;\nfun main() {}");
        assert_eq!(d[0].code, "E0404");
        assert!(
            d[0].help.as_deref().unwrap().contains("T: Eq"),
            "{:?}",
            d[0].help
        );
        assert_eq!(
            errors("fun less<T: Eq>(a: T, b: T): bool = a < b;\nfun main() {}"),
            [("E0404", "a < b")]
        );
        ok(
            "fun same<T: Eq>(a: T, b: T): bool = a == b;\nfun less<T: Ord>(a: T, b: T): bool = a < b;\nfun main() {}",
        );
        for (body, code, at) in [
            ("let c = a + a;", "E0404", "a + a"),
            ("let c = a as i64;", "E0408", "a as i64"),
            ("println(a);", "E0403", "a"),
            ("let n = a.len;", "E0410", "len"),
            ("let c = a[0];", "E0411", "a"),
            ("let x: T = 0;", "E0401", "0"),
        ] {
            let src = format!("fun f<T>(a: T) {{ {body} }}\nfun main() {{}}");
            assert_eq!(errors(&src), [(code, at)], "{body}");
        }
        assert_eq!(
            let_type(
                "fun id<T>(a: T): T { let b = a; b }\nfun main() { let x = 1; }",
                "x"
            ),
            Type::I64
        );
    }
}
