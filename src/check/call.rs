//! Names used as values, and calls: locals shadow functions, and the
//! intrinsics `print`, `println`, `panic`, `to_string` and `swap` are built
//! in (spec §5, §6).

use super::env::is_intrinsic;
use super::expr::Expect;
use super::generics::subst;
use super::{Checker, Type, errors, qualify};
use crate::ast::{Expr, ExprKind};

impl Checker {
    /// A name used as a value: locals first, then functions, which are
    /// values from v3 (spec §6; v3 draft §4).
    pub(super) fn name(&mut self, expr: &Expr, name: &str, expect: Option<&Expect>) -> Type {
        if let Some(local) = self.local(name) {
            return local.ty;
        }
        let key = self.key(name);
        if self.functions.contains_key(&key) {
            return self.function_value(expr, &key, expect);
        }
        if is_intrinsic(name) {
            let help = "intrinsics aren't values: call it inside a function, and use that function";
            self.report(errors::not_value(name, expr.span, help));
        } else {
            self.report(errors::undefined(name, expr.span));
        }
        Type::Error
    }

    /// `callee(args)`: the callee must name a function (spec §6).
    pub(super) fn call(
        &mut self,
        call: &Expr,
        callee: &Expr,
        args: &[Expr],
        expect: Option<&Expect>,
    ) -> Type {
        // A local or any other expression is called as a function value (v3).
        let ExprKind::Name(name) = &callee.kind else {
            return self.call_expr(call, callee, args);
        };
        if self.local(name).is_some() {
            return self.call_expr(call, callee, args);
        }
        // A function called by name is not a value here; `void` marks it.
        self.record(callee, Type::Void);
        let mut key = self.key(name);
        // `range(a, b)` is the prelude's unless a module declares one (v10).
        let prelude = qualify(&[crate::modules::PRELUDE_PATH.to_string()], name);
        // `char_from` (v14) is the prelude's the same way.
        if matches!(name.as_str(), "range" | "char_from")
            && !self.functions.contains_key(&key)
            && self.functions.contains_key(&prelude)
        {
            key = prelude;
        }
        if !self.functions.contains_key(&key) {
            if is_intrinsic(name) {
                return self.intrinsic(call, name, args);
            }
            self.report(errors::undefined(name, callee.span));
            return self.unchecked_args(args);
        }
        self.call_key(call, &key, args, expect)
    }

    /// A call of the function or extern with key `key`, recorded for codegen
    /// (`Checked::callees`): arity, then each argument against its
    /// parameter, inferring a generic function's type arguments (PRP-035).
    pub(super) fn call_key(
        &mut self,
        call: &Expr,
        key: &str,
        args: &[Expr],
        expect: Option<&Expect>,
    ) -> Type {
        self.callees.insert(call.id, key.to_string());
        let signature = &self.functions[key];
        let (params, ret) = (signature.params.clone(), signature.ret.clone());
        let generics = signature.generics.clone();
        let name = key.rsplit('.').next().unwrap_or(key);
        if args.len() != params.len() {
            self.report(errors::arity(name, params.len(), args.len(), call.span));
            self.unchecked_args(args);
            return if generics.is_empty() {
                ret
            } else {
                Type::Error
            };
        }
        let inputs: Vec<_> = args
            .iter()
            .zip(params)
            .map(|(a, t)| (a, Expect::of(t)))
            .collect();
        match self.infer(
            call,
            &generics,
            Default::default(),
            Some((&ret, expect)),
            &inputs,
        ) {
            Some(targs) => {
                self.used(call, key, &targs);
                subst(&ret, &generics, &targs)
            }
            None => Type::Error,
        }
    }

    /// `print(x)`, `println([x])`, `panic(msg)`, `to_string(x)` (spec §5).
    /// `panic` never returns, so its type is `Never` (spec §6).
    fn intrinsic(&mut self, call: &Expr, name: &str, args: &[Expr]) -> Type {
        if name == "swap" {
            return self.swap(call, args);
        }
        // `eprint`/`eprintln` follow the `print`/`println` rules below (v4).
        if super::input::INPUT.contains(&name) && !name.starts_with("eprint") {
            return self.input_intrinsic(call, name, args);
        }
        let result = match name {
            "print" | "println" | "eprint" | "eprintln" => Type::Void,
            "to_string" => Type::String,
            _ => Type::Never,
        };
        let allowed = if name.ends_with("println") {
            0..=1
        } else {
            1..=1
        };
        if !allowed.contains(&args.len()) {
            self.report(errors::arity(name, 1, args.len(), call.span));
            self.unchecked_args(args);
            return result;
        }
        let Some(arg) = args.first() else {
            return result;
        };
        let ty = self.value(arg, None);
        // Every value prints (v12); `panic` still takes a `string`.
        let fits = match name {
            "print" | "println" | "eprint" | "eprintln" | "to_string" => {
                super::printing::printable(&ty)
            }
            _ => ty == Type::String,
        };
        if fits && name != "panic" {
            self.printed(arg, &ty);
        }
        if !fits && !matches!(ty, Type::Error | Type::Never) {
            self.report(errors::intrinsic_argument(name, ty, arg.span));
        }
        result
    }

    /// Checks arguments of a call that is already wrong, for their own errors.
    pub(super) fn unchecked_args(&mut self, args: &[Expr]) -> Type {
        for arg in args {
            self.expr(arg, None);
        }
        Type::Error
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type};

    #[test]
    fn names_resolve_innermost_first() {
        assert_eq!(errors("fun main() { let a = y + 1; }"), [("E0301", "y")]);
        assert_eq!(errors("fun main() { g(); }"), [("E0301", "g")]);
        // A local shadows a function; `let x = x + 1` reads the outer `x`.
        assert_eq!(
            let_type(
                "fun f() {}\nfun main() { let f = 2; let x: u8 = 1; let x = x + 1; }",
                "x"
            ),
            Type::U8
        );
    }

    #[test]
    fn calls_check_arity_arguments_and_callee() {
        let f = "fun f(a: u8, b: bool): i32 { 1 }\n";
        assert_eq!(
            errors(&format!("{f}fun main() {{ f(1); }}")),
            [("E0405", "f(1)")]
        );
        assert_eq!(
            errors(&format!("{f}fun main() {{ f(300, true); }}")),
            [("E0402", "300")]
        );
        assert_eq!(
            errors(&format!("{f}fun main() {{ f(1, 2); }}")),
            [("E0401", "2")]
        );
        assert_eq!(
            let_type(&format!("{f}fun main() {{ let r = f(1, true); }}"), "r"),
            Type::I32
        );
        assert_eq!(errors("fun main() { let f = 1; f(); }"), [("E0406", "f")]);
        // v3: a function is a value (PRP-039).
        assert_eq!(
            let_type("fun g() {}\nfun main() { let h = g; }", "h"),
            Type::Fun(Vec::new(), Box::new(Type::Void))
        );
    }

    #[test]
    fn void_is_not_a_value() {
        let noop = "fun noop() {}\n";
        assert_eq!(
            errors(&format!("{noop}fun main() {{ let x = noop(); }}")),
            [("E0407", "noop()")]
        );
        assert_eq!(
            errors(&format!("{noop}fun main() {{ let y = noop() + 1; }}")),
            [("E0407", "noop()")]
        );
    }

    #[test]
    fn intrinsics_check_arity_and_argument_types() {
        crate::check::test_util::ok(
            "fun main() { print(1); println(); println(2.5); println(\"s\"); let t = to_string(true); println(t); }",
        );
        assert_eq!(errors("fun main() { print(); }"), [("E0405", "print()")]);
        assert_eq!(
            errors("fun main() { println(1, 2); }"),
            [("E0405", "println(1, 2)")]
        );
        assert_eq!(
            errors("fun f() {}\nfun main() { let s = to_string(f()); }"),
            [("E0407", "f()")]
        );
        assert_eq!(errors("fun main() { panic(1); }"), [("E0403", "1")]);
        assert_eq!(
            errors("fun noop() {}\nfun main() { println(noop()); }"),
            [("E0407", "noop()")]
        );
        assert_eq!(
            errors("fun main() { let p = println; }"),
            [("E0406", "println")]
        );
        assert_eq!(
            let_type("fun main() { let s = to_string(1); }", "s"),
            Type::String
        );
    }

    #[test]
    fn panic_counts_as_returning() {
        crate::check::test_util::ok("fun f(): i64 { panic(\"no\"); }\nfun main() {}");
        crate::check::test_util::ok(
            "fun g(): i64 { while true { } panic(\"unreachable\"); }\nfun main() {}",
        );
    }
}
