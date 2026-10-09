//! Names used as values, and calls: locals shadow functions, and the
//! intrinsics `print`, `println`, `panic` and `to_string` are built in
//! (spec §5, §6).

use super::env::INTRINSICS;
use super::expr::Expect;
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind};
use crate::span::Span;

impl Checker {
    /// A name used as a value: locals first, then globals (spec §6).
    pub(super) fn name(&mut self, name: &str, span: Span) -> Type {
        if let Some(local) = self.local(name) {
            return local.ty;
        }
        if self.functions.contains_key(name) || INTRINSICS.contains(&name) {
            self.report(errors::not_value(name, span));
        } else {
            self.report(errors::undefined(name, span));
        }
        Type::Error
    }

    /// `callee(args)`: the callee must name a function (spec §6).
    pub(super) fn call(&mut self, call: &Expr, callee: &Expr, args: &[Expr]) -> Type {
        let ExprKind::Name(name) = &callee.kind else {
            self.expr(callee, None);
            self.report(errors::not_callable(callee.span));
            return self.unchecked_args(args);
        };
        // A function name is not a value; `void` marks it in the table.
        self.record(callee, Type::Void);
        if self.local(name).is_some() {
            self.report(errors::not_function(name, callee.span));
            return self.unchecked_args(args);
        }
        let Some(signature) = self.functions.get(name) else {
            if INTRINSICS.contains(&name.as_str()) {
                return self.intrinsic(call, name, args);
            }
            self.report(errors::undefined(name, callee.span));
            return self.unchecked_args(args);
        };
        let (params, ret) = (signature.params.clone(), signature.ret.clone());
        if args.len() != params.len() {
            self.report(errors::arity(name, params.len(), args.len(), call.span));
            self.unchecked_args(args);
            return ret;
        }
        for (arg, ty) in args.iter().zip(params) {
            self.expect_type(arg, &Expect::of(ty));
        }
        ret
    }

    /// `print(x)`, `println([x])`, `panic(msg)`, `to_string(x)` (spec §5).
    /// `panic` never returns, so its type is `Never` (spec §6).
    fn intrinsic(&mut self, call: &Expr, name: &str, args: &[Expr]) -> Type {
        let result = match name {
            "print" | "println" => Type::Void,
            "to_string" => Type::String,
            _ => Type::Never,
        };
        let allowed = if name == "println" { 0..=1 } else { 1..=1 };
        if !allowed.contains(&args.len()) {
            self.report(errors::arity(name, 1, args.len(), call.span));
            self.unchecked_args(args);
            return result;
        }
        let Some(arg) = args.first() else {
            return result;
        };
        let ty = self.value(arg, None);
        let fits = match name {
            "print" | "println" => ty.is_numeric() || matches!(ty, Type::Bool | Type::String),
            "to_string" => ty.is_numeric() || ty == Type::Bool,
            _ => ty == Type::String,
        };
        if !fits && !matches!(ty, Type::Error | Type::Never) {
            self.report(errors::intrinsic_argument(name, ty, arg.span));
        }
        result
    }

    /// Checks arguments of a call that is already wrong, for their own errors.
    fn unchecked_args(&mut self, args: &[Expr]) -> Type {
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
        assert_eq!(
            errors("fun g() {}\nfun main() { let h = g; }"),
            [("E0406", "g")]
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
            errors("fun main() { let s = to_string(\"s\"); }"),
            [("E0403", "\"s\"")]
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
