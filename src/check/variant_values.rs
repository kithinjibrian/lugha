//! Variants as function values (v17, milestone 94, PRP-125): a variant
//! with payloads, written without a call, is a function of its payloads —
//! `Msg.Named` is `fun(string): Msg`, and so are `Some`, `Ok` and `Err`. A
//! generic enum's variant takes its type arguments from the expected
//! function type, as a generic function does (E0412 without one).

use std::collections::HashMap;

use super::expr::Expect;
use super::generics::{Generic, params, subst};
use super::results::RESULT;
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind};

/// Which enum a variant value builds.
enum Of<'a> {
    /// `Some`: the built-in `Option<T>`.
    Option,
    /// A declared enum (or the prelude's `Result`), by key.
    Enum(&'a str),
}

impl Checker {
    /// `Some`, `Ok` or `Err` named without a call (no local or function has
    /// that name): a function value. `None` when `name` is none of them.
    pub(super) fn named_variant_value(
        &mut self,
        expr: &Expr,
        name: &str,
        expect: Option<&Expect>,
    ) -> Option<Type> {
        match name {
            "Some" => Some(self.variant_function(expr, Of::Option, name, expect)),
            "Ok" | "Err" if self.enums.contains_key(RESULT) => {
                Some(self.variant_function(expr, Of::Enum(RESULT), name, expect))
            }
            _ => None,
        }
    }

    /// True if `e` may be a variant used as a function value: `Some`, `Ok`,
    /// `Err` (not a local), or `E.V` / `m.E.V`, so inference hands it what
    /// it knows of the expected function type, as it does a closure.
    pub(super) fn may_be_variant_value(&self, e: &Expr) -> bool {
        match &e.kind {
            ExprKind::Name(n) => {
                matches!(n.as_str(), "Some" | "Ok" | "Err") && self.local(n).is_none()
            }
            ExprKind::Field(base, _) => match &base.kind {
                ExprKind::Name(n) => self.local(n).is_none(),
                ExprKind::Field(..) => true,
                _ => false,
            },
            _ => false,
        }
    }

    /// `E.V` without a call for a variant with payloads: `fun(payloads): E`.
    /// `None` when the variant has no payloads (it is a value of `E`).
    pub(super) fn enum_variant_function(
        &mut self,
        expr: &Expr,
        key: &str,
        variant: &str,
        expect: Option<&Expect>,
    ) -> Option<Type> {
        let payloads = self.enums[key]
            .variants
            .iter()
            .find(|(v, _)| v == variant)
            .map(|(_, p)| p.len())?;
        (payloads > 0).then(|| self.variant_function(expr, Of::Enum(key), variant, expect))
    }

    /// The function type of variant `variant` of `of`, its type arguments
    /// fixed by what `expect` says of its result or its parameters.
    fn variant_function(
        &mut self,
        expr: &Expr,
        of: Of<'_>,
        variant: &str,
        expect: Option<&Expect>,
    ) -> Type {
        let t = Generic {
            name: "T".to_string(),
            bounds: Default::default(),
        };
        let (generics, payloads, declared) = match of {
            Of::Option => {
                let param = Type::Param("T".into());
                (vec![t], vec![param.clone()], Type::Option(Box::new(param)))
            }
            Of::Enum(key) => {
                let info = &self.enums[key];
                let payloads = info
                    .variants
                    .iter()
                    .find(|(v, _)| v == variant)
                    .map(|(_, p)| p.clone())
                    .expect("callers found the variant");
                let generics = info.generics.clone();
                let declared = Type::Enum(key.to_string(), params(&generics));
                (generics, payloads, declared)
            }
        };
        let names: Vec<String> = generics.iter().map(|g| g.name.clone()).collect();
        let mut bound = HashMap::new();
        if let Some(Type::Fun(params, ret)) = expect.map(|e| &e.ty) {
            bind(&declared, ret, &names, &mut bound);
            for (p, a) in payloads.iter().zip(params) {
                bind(p, a, &names, &mut bound);
            }
        }
        let mut args = Vec::new();
        for g in &generics {
            match bound.get(&g.name) {
                Some(ty) => args.push(ty.clone()),
                None => {
                    self.report(errors::variant_value_infer(variant, expr.span));
                    return Type::Error;
                }
            }
        }
        let enum_ty = subst(&declared, &generics, &args);
        let payloads = payloads
            .iter()
            .map(|p| subst(p, &generics, &args))
            .collect();
        self.variant_values
            .insert(expr.id, (enum_ty.clone(), variant.to_string()));
        Type::Fun(payloads, Box::new(enum_ty))
    }
}

/// Binds the parameters `names` in `declared` to what `actual` has there.
/// Inference hands over what it doesn't know yet as `Error`, which binds
/// nothing; a type parameter left in `actual` is one in scope (`wrap<T>`'s
/// own `T`), a real type here.
fn bind(declared: &Type, actual: &Type, names: &[String], out: &mut HashMap<String, Type>) {
    match (declared, actual) {
        (Type::Param(n), a) if names.contains(n) => {
            if !mentions_unknown(a) {
                out.entry(n.clone()).or_insert_with(|| a.clone());
            }
        }
        (Type::Array(d), Type::Array(a))
        | (Type::Option(d), Type::Option(a))
        | (Type::Ref(d), Type::Ref(a)) => bind(d, a, names, out),
        (Type::Struct(k, ds), Type::Struct(l, as_)) | (Type::Enum(k, ds), Type::Enum(l, as_))
            if k == l =>
        {
            ds.iter().zip(as_).for_each(|(d, a)| bind(d, a, names, out));
        }
        (Type::Fun(dp, dr), Type::Fun(ap, ar)) => {
            dp.iter().zip(ap).for_each(|(d, a)| bind(d, a, names, out));
            bind(dr, ar, names, out);
        }
        _ => {}
    }
}

/// True if `ty` holds a part inference doesn't know yet (`Error`) or `Never`.
fn mentions_unknown(ty: &Type) -> bool {
    match ty {
        Type::Error | Type::Never => true,
        Type::Array(inner) | Type::Option(inner) | Type::Ref(inner) => mentions_unknown(inner),
        Type::Struct(_, args) | Type::Enum(_, args) | Type::Interface(_, args) => {
            args.iter().any(mentions_unknown)
        }
        Type::Fun(params, ret) => params.iter().any(mentions_unknown) || mentions_unknown(ret),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    const MSG: &str = "enum Msg { Add, Named(string), Moved(f64, f64) }\n";

    #[test]
    fn a_variant_with_payloads_is_a_function() {
        ok(&format!(
            "{MSG}fun apply(f: fun(string): Msg): Msg = f(\"x\");\n\
             fun main() {{\n\
                 let f: fun(string): Msg = Msg.Named;\n\
                 let g = Msg.Moved;\n\
                 let m: Msg = g(1.0, 2.0);\n\
                 let n = apply(Msg.Named);\n\
                 let a = Msg.Add;\n\
                 let fs = [Msg.Named, Msg.Named];\n\
             }}"
        ));
    }

    #[test]
    fn some_ok_and_err_take_their_types_from_what_is_expected() {
        ok("fun main() {\n\
                let s: fun(i64): Option<i64> = Some;\n\
                let xs: Option<i64>[] = [1, 2].iter().map(Some).collect();\n\
                let ok: fun(i64): Result<i64, string> = Ok;\n\
                let err: fun(string): Result<i64, string> = Err;\n\
            }");
        ok("enum Tree<T> { Leaf, Node(Tree<T>, T, Tree<T>) }\n\
            fun main() { let n: fun(Tree<i64>, i64, Tree<i64>): Tree<i64> = Tree.Node; }");
    }

    #[test]
    fn a_generic_variant_with_nothing_expected_is_e0412() {
        assert_eq!(errors("fun main() { let s = Some; }"), [("E0412", "Some")]);
        assert_eq!(errors("fun main() { let e = Err; }"), [("E0412", "Err")]);
        assert_eq!(
            errors("enum Box<T> { Full(T), Empty }\nfun main() { let f = Box.Full; }"),
            [("E0412", "Box.Full")]
        );
    }

    #[test]
    fn a_variant_value_of_the_wrong_type_is_e0403() {
        assert_eq!(
            errors(&format!(
                "{MSG}fun main() {{ let f: fun(i64): Msg = Msg.Named; }}"
            )),
            [("E0403", "Msg.Named")]
        );
    }

    #[test]
    fn locals_and_functions_named_like_variants_win() {
        ok("fun main() { let Some_ = 1; let f = fun(x: i64): i64 = x; println(f(Some_)); }");
        ok(&format!(
            "{MSG}fun main() {{ let named = Msg.Named(\"a\"); let add = Msg.Add; }}"
        ));
    }

    #[test]
    fn through_a_module() {
        use crate::check::modules::test_util::ok as ok_files;
        ok_files(&[
            (
                "main.la",
                "import m;\nfun main() { let f: fun(i64): m.E = m.E.Wrap; let v = f(1); }\n",
            ),
            ("m.la", "public enum E { Wrap(i64), Nothing }\n"),
        ]);
    }
}
