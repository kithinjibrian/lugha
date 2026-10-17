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
                let d = errors::unmet_bound(arg, bound, span);
                // A struct or enum becomes `Ord` through `cmp` (v15).
                let help = super::ordering::cmp_help(arg).filter(|_| bound == "Ord");
                self.report(match help {
                    Some(help) => d.with_help(help),
                    None => d,
                });
                return false;
            }
            for interface in &g.bounds.interfaces {
                // Only the numbers are `Num`, and only `f32`/`f64` are
                // `Float`: E0419, like `Ord` (v15).
                let builtin = interface.key.strip_prefix("$prelude.");
                if let Some(bound @ ("Num" | "Float")) = builtin
                    && self
                        .builtin_operator(arg, &interface.key)
                        .is_some_and(|p| !p.is_empty())
                {
                    self.report(errors::unmet_bound(arg, bound, span));
                    return false;
                }
                // A bound's arguments may name the other parameters (v6).
                let interface = interface.subst(generics, args);
                if !self.satisfies(arg, &interface, span) {
                    return false;
                }
            }
        }
        true
    }

    /// The bounds of the parameter `name` in scope.
    pub(super) fn bounds(&self, name: &str) -> Bounds {
        self.type_params
            .iter()
            .find(|g| g.name == name)
            .map_or_else(Bounds::default, |g| g.bounds.clone())
    }

    /// True if `==` works on `ty`: always, except through a parameter
    /// without `Eq` (v2 draft §4).
    pub(super) fn eq_ok(&self, ty: &Type) -> bool {
        match ty {
            // `Num` gives `==` and `<` too (v15).
            Type::Param(n) => self.bounds(n).eq || self.has_num(n),
            Type::Array(inner) | Type::Option(inner) => self.eq_ok(inner),
            // Only C knows which field of a union is live (v17, milestone 104).
            Type::Struct(key, _) if self.holds_union(key) => false,
            // A function anywhere inside has no `==` (v3 draft §4).
            Type::Struct(_, args) | Type::Enum(_, args) => {
                !self.holds_fun(ty) && args.iter().all(|a| self.eq_ok(a))
            }
            // Neither do interface values (v5 draft §4), nor boxes (v16).
            // Nor tasks and channels (v17).
            Type::Fun(..)
            | Type::Interface(..)
            | Type::Ref(_)
            | Type::Task(_)
            | Type::Chan(_)
            | Type::Actor(_) => false,
            _ => true,
        }
    }

    /// True if `<` works on `ty`: numbers, strings, and parameters with `Ord`.
    pub(super) fn ord_ok(&self, ty: &Type) -> bool {
        match ty {
            Type::Param(n) => self.bounds(n).ord || self.has_num(n),
            // A tuple orders lexicographically when every element does (v14).
            Type::Struct(key, args) if key.starts_with(super::tuples::TUPLE) => {
                args.iter().all(|a| self.ord_ok(a))
            }
            // A struct or enum with `cmp` (v15, milestone 59).
            Type::Struct(..) | Type::Enum(..) => self.has_cmp(ty),
            _ => ty.is_numeric() || matches!(ty, Type::String | Type::Char),
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

    /// True if a value of `ty` holds a function or an interface value (v5)
    /// somewhere: in a field, a payload or an element, through any depth of
    /// structs and enums. Neither has `==`.
    pub(super) fn holds_fun(&self, ty: &Type) -> bool {
        self.holds_fun_in(ty, &mut Vec::new())
    }

    fn holds_fun_in(&self, ty: &Type, seen: &mut Vec<Type>) -> bool {
        let inner: Vec<Type> = match ty {
            // A box makes its holder lose `==` too (v16, milestone 67).
            Type::Fun(..)
            | Type::Interface(..)
            | Type::Ref(_)
            | Type::Task(_)
            | Type::Chan(_)
            | Type::Actor(_) => return true,
            Type::Array(inner) | Type::Option(inner) => return self.holds_fun_in(inner, seen),
            // Recursive types meet themselves again: one look is enough.
            Type::Struct(..) | Type::Enum(..) if seen.contains(ty) => return false,
            Type::Struct(key, args) => {
                let fields = self
                    .structs
                    .get(key)
                    .map(|s| s.fields.clone())
                    .unwrap_or_default();
                fields
                    .iter()
                    .filter_map(|(f, _)| self.field_type(key, args, f))
                    .collect()
            }
            Type::Enum(key, args) => {
                let variants = self.enum_variants(key, args).unwrap_or_default();
                variants.into_iter().flat_map(|(_, ps)| ps).collect()
            }
            _ => return false,
        };
        seen.push(ty.clone());
        inner.iter().any(|t| self.holds_fun_in(t, seen))
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
        // v12: a bare parameter prints.
        ok("fun f<T>(a: T) { println(a); }\nfun main() {}");
        ok(
            "fun same<T: Eq>(a: T, b: T): bool = a == b;\nfun less<T: Ord>(a: T, b: T): bool = a < b;\nfun main() {}",
        );
        for (body, code, at) in [
            ("let c = a + a;", "E0404", "a + a"),
            ("let c = a as i64;", "E0408", "a as i64"),
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
