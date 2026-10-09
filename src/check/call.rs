//! Names used as values, and calls: locals shadow functions; intrinsics
//! arrive in milestone 4 (spec §6).

use super::env::INTRINSICS;
use super::expr::Expect;
use super::{Checker, Checking, Type, errors, stop};
use crate::ast::{Expr, ExprKind};
use crate::span::Span;

impl Checker {
    /// A name used as a value: locals first, then globals (spec §6).
    pub(super) fn name(&mut self, name: &str, span: Span) -> Checking<Type> {
        if let Some(local) = self.local(name) {
            return Ok(local.ty);
        }
        if self.functions.contains_key(name) {
            self.report(errors::not_value(name, span));
        } else if INTRINSICS.contains(&name) {
            return Err(stop("intrinsics", 4, span));
        } else {
            self.report(errors::undefined(name, span));
        }
        Ok(Type::Error)
    }

    /// `callee(args)`: the callee must name a function (spec §6).
    pub(super) fn call(&mut self, call: &Expr, callee: &Expr, args: &[Expr]) -> Checking<Type> {
        let ExprKind::Name(name) = &callee.kind else {
            self.expr(callee, None)?;
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
                return Err(stop("intrinsics", 4, callee.span));
            }
            self.report(errors::undefined(name, callee.span));
            return self.unchecked_args(args);
        };
        let (params, ret) = (signature.params.clone(), signature.ret);
        if args.len() != params.len() {
            self.report(errors::arity(name, params.len(), args.len(), call.span));
            self.unchecked_args(args)?;
            return Ok(ret);
        }
        for (arg, ty) in args.iter().zip(params) {
            self.expect_type(arg, &Expect::of(ty))?;
        }
        Ok(ret)
    }

    /// Checks arguments of a call that is already wrong, for their own errors.
    fn unchecked_args(&mut self, args: &[Expr]) -> Checking<Type> {
        for arg in args {
            self.expr(arg, None)?;
        }
        Ok(Type::Error)
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
}
