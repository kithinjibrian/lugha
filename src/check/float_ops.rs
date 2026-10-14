//! The float built-ins (v15 draft decision 18, milestone 53):
//! `$float.sqrt(x)` and its kin, which only bundled sources can name
//! (`std.math` wraps them). Each takes one float type, `f32`, `f64` or a
//! `T: Float`, and returns it; codegen lowers it to LLVM's operation.

use super::expr::Expect;
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind};
use crate::diagnostic::Diagnostic;

/// The float built-ins and how many operands each takes.
pub const FLOAT_OPS: [(&str, usize); 12] = [
    ("sqrt", 1),
    ("pow", 2),
    ("exp", 1),
    ("ln", 1),
    ("log10", 1),
    ("sin", 1),
    ("cos", 1),
    ("tan", 1),
    ("atan2", 2),
    ("floor", 1),
    ("ceil", 1),
    ("round", 1),
];

/// The built-in `callee` names, `sqrt` for `$float.sqrt`; `None` for any
/// other callee.
pub fn float_op(callee: &Expr) -> Option<&str> {
    let ExprKind::Field(base, name) = &callee.kind else {
        return None;
    };
    matches!(&base.kind, ExprKind::Name(n) if n == "$float").then_some(name.name.as_str())
}

impl Checker {
    /// `$float.op(args)`: every operand one float type, which it returns.
    pub(super) fn float_call(
        &mut self,
        call: &Expr,
        (callee, op): (&Expr, &str),
        args: &[Expr],
        expect: Option<&Expect>,
    ) -> Type {
        self.record(callee, Type::Void);
        if let ExprKind::Field(base, _) = &callee.kind {
            self.record(base, Type::Void);
        }
        let Some(&(_, arity)) = FLOAT_OPS.iter().find(|(name, _)| *name == op) else {
            let d = Diagnostic::error("E0415", format!("no float built-in `{op}`"), callee.span);
            self.report(d);
            return self.unchecked_args(args);
        };
        if args.len() != arity {
            self.report(errors::arity(op, arity, args.len(), call.span));
            return self.unchecked_args(args);
        }
        let ty = self.value(&args[0], expect);
        let float = ty.is_float() || matches!(&ty, Type::Param(p) if self.has_float(p));
        if !float && !matches!(ty, Type::Error | Type::Never) {
            self.report(errors::bad_operands(
                op,
                std::slice::from_ref(&ty),
                args[0].span,
            ));
            return self.unchecked_args(&args[1..]);
        }
        for arg in &args[1..] {
            self.expect_type(arg, &Expect::of(ty.clone()));
        }
        ty
    }
}

#[cfg(test)]
mod tests {
    use crate::check::modules::test_util::prelude_codes;

    #[test]
    fn std_math_follows_the_import_rules() {
        let none: [&str; 0] = [];
        let found = prelude_codes(&[(
            "main.la",
            "import std.math as m { sqrt };\n\
             fun h<T: Float>(a: T): T = m.sqrt(a * a + a);\n\
             fun main() { let r = m.pow(2.0, sqrt(4.0)); let s = h(1.5); let t: f32 = h(2.0); }\n",
        )]);
        assert_eq!(found, none);
        let found = prelude_codes(&[(
            "main.la",
            "import std.math;\nfun main() { let r = math.sqrt(2); }\n",
        )]);
        assert_eq!(found, ["E0419"]);
    }

    #[test]
    fn programs_cannot_name_the_built_ins() {
        assert!(crate::lexer::lex("fun main() { let r = $float.sqrt(2.0); }").is_err());
    }
}
