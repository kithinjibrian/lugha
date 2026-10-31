//! Functions as values (v3 draft §4, milestone 14): `fun(A): R` types,
//! named functions used without a call, and calls of function values.

use super::expr::Expect;
use super::generics::{Generic, subst};
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind, Ident, Type as Annotation};

impl Checker {
    /// The type `fun(params): ret` names.
    pub(super) fn resolve_fun(&mut self, params: &[Annotation], ret: Option<&Annotation>) -> Type {
        let params = params.iter().map(|p| self.resolve(p)).collect();
        let ret = ret.map_or(Type::Unit, |r| self.resolve(r));
        Type::Fun(params, Box::new(ret))
    }

    /// Function `key` used as a value: its function type, with a generic
    /// function's type arguments taken from `expect` (E0412 otherwise).
    /// Codegen finds it by `Checked::callees` and `Checked::instances`.
    pub(super) fn function_value(&mut self, at: &Expr, key: &str, expect: Option<&Expect>) -> Type {
        let sig = &self.functions[key];
        let name = key.rsplit('.').next().unwrap_or(key).to_string();
        if sig.is_extern {
            let help = "an extern can't be a value: call it inside a Lugha function, and use that";
            self.report(errors::not_value(&name, at.span, help));
            return Type::Error;
        }
        let (generics, params, ret) = (sig.generics.clone(), sig.params.clone(), sig.ret.clone());
        self.not_a_test(key, at.span);
        self.callees.insert(at.id, key.to_string());
        let decl = self.functions[key].span;
        self.hover.function(at.span, decl, None);
        self.generic_value(at, key, &generics, Type::Fun(params, Box::new(ret)), expect)
    }

    /// `ty` at the type arguments `expect` decides for `generics`, recorded
    /// as a use of `key`; `ty` itself when there are none.
    fn generic_value(
        &mut self,
        at: &Expr,
        key: &str,
        generics: &[Generic],
        ty: Type,
        expect: Option<&Expect>,
    ) -> Type {
        if generics.is_empty() {
            // A std function used as a value is compiled only if listed (v15).
            self.used(at, key, &[]);
            return ty;
        }
        match self.infer(at, generics, Default::default(), Some((&ty, expect)), &[]) {
            Some(args) => {
                self.used(at, key, &args);
                subst(&ty, generics, &args)
            }
            None => Type::Error,
        }
    }

    /// `m.f`, `T.f` or `m.T.f` without a call: a function or associated
    /// function as a value. `None` if `base.name` isn't one, so field
    /// access handles it. A method with `self` is E0406.
    pub(super) fn function_field(
        &mut self,
        expr: &Expr,
        (base, name): (&Expr, &Ident),
        expect: Option<&Expect>,
    ) -> Option<Type> {
        if let Some(m) = Self::module_name(base)
            && self.module_path(&m).is_some()
        {
            let key = self.member_through(&m, name)?;
            // `m.NAME`: another module's constant (v17, milestone 109).
            if self.consts.contains_key(&key) {
                self.record(base, Type::Unit);
                self.item_visible(&key, name.span);
                return Some(self.const_read(expr, name.span, &key));
            }
            if !self.functions.contains_key(&key) {
                return None;
            }
            self.record(base, Type::Unit);
            self.item_visible(&key, name.span);
            return Some(self.function_value(expr, &key, expect));
        }
        let owner = self.type_named_by(base)?;
        // `E.V` is the variant even beside a `self` method `V` (v17, milestone 123).
        if self.has_variant(&owner, &name.name) {
            return None;
        }
        let method = self.method(&owner, &name.name)?;
        self.record(base, Type::Unit);
        if method.receiver.is_some() {
            let help = format!(
                "a method isn't a value; call it on a value: `x.{}()`",
                name.name
            );
            self.report(errors::not_value(
                &format!("{owner}.{}", name.name),
                expr.span,
                &help,
            ));
            return Some(Type::Error);
        }
        if let Type::Struct(k, _) | Type::Enum(k, _) = &owner {
            self.method_visible(k, &name.name, name.span);
        }
        self.callees.insert(expr.id, method.key.clone());
        self.hover.function(name.span, method.span, None);
        let ty = Type::Fun(method.params.clone(), Box::new(method.ret.clone()));
        Some(self.generic_value(expr, &method.key, &method.generics, ty, expect))
    }

    /// `callee(args)` where `callee` isn't a function's name: a local, a
    /// call, an element… It must be a function value (E0406 otherwise).
    pub(super) fn call_expr(&mut self, call: &Expr, callee: &Expr, args: &[Expr]) -> Type {
        let ty = self.value(callee, None);
        let name = match &callee.kind {
            ExprKind::Name(n) => n.clone(),
            _ => "this function".to_string(),
        };
        match ty {
            Type::Fun(..) => self.call_value(call, &name, &ty, args),
            Type::Error | Type::Never => self.unchecked_args(args),
            _ => {
                let d = match &callee.kind {
                    ExprKind::Name(n) => errors::not_function(n, callee.span),
                    _ => errors::not_callable(callee.span),
                };
                self.report(d);
                self.unchecked_args(args)
            }
        }
    }

    /// A call of a value of function type `ty`: arity, then each argument
    /// against its parameter.
    pub(super) fn call_value(&mut self, call: &Expr, name: &str, ty: &Type, args: &[Expr]) -> Type {
        let Type::Fun(params, ret) = ty else {
            unreachable!("called with a function type")
        };
        if args.len() != params.len() {
            self.report(errors::arity(name, params.len(), args.len(), call.span));
            self.unchecked_args(args);
            return (**ret).clone();
        }
        for (arg, param) in args.iter().zip(params) {
            self.expect_type(arg, &Expect::of(param.clone()));
        }
        (**ret).clone()
    }

    /// The type of `receiver`'s field `name` if it holds a function and the
    /// type has no method of that name (E0310 keeps the two apart).
    pub(super) fn fun_field(&mut self, receiver: &Type, name: &Ident) -> Option<Type> {
        let Type::Struct(key, args) = receiver else {
            return None;
        };
        let ty = self.field_type(key, args, &name.name)?;
        if !matches!(ty, Type::Fun(..)) || self.method(receiver, &name.name).is_some() {
            return None;
        }
        self.field_visible(key, &name.name, name.span);
        Some(ty)
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{diagnostics, errors, let_type, ok};

    const LIB: &str = "fun double(x: i64): i64 = x * 2;\n\
        fun apply(f: fun(i64): i64, x: i64): i64 = f(x);\n\
        fun make(): fun(i64): i64 = double;\n\
        fun max<T: Ord>(a: T, b: T): T = if a > b { a } else { b };\n\
        struct Circle { r: f64 }\n\
        impl Circle { fun make(r: f64): Circle = Circle { r: r }; fun area(self): f64 = self.r; }\n\
        struct Button { label: string, on_click: fun(i64): i64 }\n\
        extern fun abs(x: i32): i32;\n";

    fn ty(body: &str, name: &str) -> Type {
        let_type(&format!("{LIB}fun main() {{ {body} }}"), name)
    }

    fn e(body: &str) -> Vec<(&'static str, String)> {
        let src = format!("{LIB}fun main() {{ {body} }}");
        errors(&src)
            .into_iter()
            .map(|(c, t)| (c, t.to_string()))
            .collect()
    }

    fn i64_to_i64() -> Type {
        Type::Fun(vec![Type::I64], Box::new(Type::I64))
    }

    #[test]
    fn named_functions_are_values() {
        assert_eq!(ty("let f = double;", "f"), i64_to_i64());
        assert_eq!(ty("let f = double; let n = f(3);", "n"), Type::I64);
        assert_eq!(ty("let n = apply(double, 3);", "n"), Type::I64);
        assert_eq!(
            ty("let m = Circle.make;", "m"),
            Type::Fun(
                vec![Type::F64],
                Box::new(Type::Struct("Circle".into(), Vec::new()))
            )
        );
        let pair = Type::Fun(vec![Type::I64, Type::I64], Box::new(Type::I64));
        assert_eq!(ty("let m: fun(i64, i64): i64 = max;", "m"), pair);
        assert_eq!(e("let m = max;"), [("E0412", "max".to_string())]);
    }

    #[test]
    fn methods_externs_and_intrinsics_are_not_values() {
        for (body, at) in [
            ("let a = Circle.area;", "Circle.area"),
            ("let g = abs;", "abs"),
            ("let p = println;", "println"),
        ] {
            assert_eq!(e(body), [("E0406", at.to_string())], "{body}");
        }
        let src = format!("{LIB}fun main() {{ let a = Circle.area; }}");
        assert!(diagnostics(&src)[0].help.is_some());
        assert_eq!(e("let n = 3; let m = n(1);"), [("E0406", "n".to_string())]);
    }

    #[test]
    fn any_function_value_can_be_called() {
        assert_eq!(ty("let n = make()(3);", "n"), Type::I64);
        assert_eq!(
            ty("let fs = [double, double]; let n = fs[0](3);", "n"),
            Type::I64
        );
        assert_eq!(
            ty(
                "let b = Button { label: \"ok\", on_click: double }; let n = b.on_click(2);",
                "n"
            ),
            Type::I64
        );
        assert_eq!(
            e("let f = double; let n = f(1, 2);"),
            [("E0405", "f(1, 2)".to_string())]
        );
        assert_eq!(
            e("let f = double; let n = f(true);"),
            [("E0403", "true".to_string())]
        );
        // A local of function type shadows the function of that name.
        ok(&format!(
            "{LIB}fun main() {{ let double = make(); let n = double(1); }}"
        ));
    }

    #[test]
    fn functions_have_no_equality_or_casts() {
        assert_eq!(e("let b = double == double;")[0].0, "E0404");
        assert_eq!(
            e("let b = Button { label: \"\", on_click: double }; let c = b == b;")[0].0,
            // A struct is `Eq` through its fields (v17, milestone 144).
            "E0451"
        );
        assert_eq!(e("let fs = [double]; let c = fs == fs;")[0].0, "E0451");
        let eq = "fun has<T: Eq>(xs: T[], x: T): bool = false;\n";
        let src = format!("{LIB}{eq}fun main() {{ let c = has([double], double); }}");
        assert_eq!(errors(&src)[0].0, "E0419");
        // v12: function values print as `<fun>` (`check/printing.rs`).
        assert_eq!(e("let n = double as i64;")[0].0, "E0408");
        // v16: a function type in an extern's parameters is a callback; none comes back.
        let src = format!("{LIB}extern fun g(): fun(i64);\nfun main() {{}}");
        assert_eq!(errors(&src)[0].0, "E0409");
    }

    #[test]
    fn functions_of_other_modules_are_values() {
        use crate::check::modules::test_util::{errors, ok};
        let lib = (
            "m.la",
            "fun f(x: i64): i64 = x;\nprivate fun g(x: i64): i64 = x;\n",
        );
        ok(&[
            (
                "main.la",
                "import m;\nfun main() { let h = m.f; let n = h(1); }\n",
            ),
            lib,
        ]);
        let found = errors(&[("main.la", "import m;\nfun main() { let h = m.g; }\n"), lib]);
        assert_eq!(found, [("E0313", "g".to_string())]);
    }

    #[test]
    fn function_types_print_like_they_are_written() {
        let fs = Type::Array(Box::new(i64_to_i64()));
        assert_eq!(fs.to_string(), "(fun(i64): i64)[]");
        assert_eq!(
            Type::Fun(vec![Type::I64], Box::new(Type::Unit)).to_string(),
            "fun(i64)"
        );
        let d = diagnostics(&format!("{LIB}fun main() {{ let f: fun(i64) = double; }}"));
        assert_eq!(d[0].message, "expected fun(i64), found fun(i64): i64");
    }
}
