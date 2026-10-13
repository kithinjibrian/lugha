//! Input and interpolation (v3 draft §5, milestone 13): the intrinsics
//! `args`, `parse_i64` and `parse_f64`, and `"a {x} b"` strings.

use super::expr::Expect;
use super::{Checker, Type, errors};
use crate::ast::{Expr, Part};

/// The input intrinsics (v3), checked here rather than in `call.rs`.
pub(super) const INPUT: [&str; 3] = ["args", "parse_i64", "parse_f64"];

impl Checker {
    /// `args()`, `parse_i64(s)` or `parse_f64(s)`.
    pub(super) fn input_intrinsic(&mut self, call: &Expr, name: &str, args: &[Expr]) -> Type {
        let (params, ret) = match name {
            "args" => (Vec::new(), Type::Array(Box::new(Type::String))),
            "parse_i64" => (vec![Type::String], Type::Option(Box::new(Type::I64))),
            _ => (vec![Type::String], Type::Option(Box::new(Type::F64))),
        };
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

    /// `"a {x} b"`: each interpolated value is a number, `bool` or `string`
    /// (E0403 otherwise), and the whole is a `string`.
    pub(super) fn interpolation(&mut self, parts: &[Part]) -> Type {
        for part in parts {
            let Part::Expr(e) = part else { continue };
            let ty = self.value(e, None);
            let printable = ty.is_numeric() || matches!(ty, Type::Bool | Type::String);
            if !printable && !matches!(ty, Type::Error | Type::Never) {
                self.report(errors::not_interpolable(&ty, e.span));
            }
        }
        Type::String
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{diagnostics, errors, let_type};

    #[test]
    fn interpolated_strings_are_strings() {
        let src = "fun main() { let n = 2; let f = 2.5; let s = \"x\"; let t = \"{n} {f} {true} {s} {n + 1}\"; }";
        assert_eq!(let_type(src, "t"), Type::String);
    }

    #[test]
    fn only_printable_values_interpolate() {
        let src = "struct P { x: i64 }\nfun main() { let p = P { x: 1 }; let t = \"{p}\"; }";
        assert_eq!(errors(src), [("E0403", "p")]);
        let help = diagnostics(src)[0].help.clone().expect("a help");
        assert!(help.contains("convert"), "{help}");
        assert_eq!(
            errors("fun main() { let t = \"{[1]}\"; }"),
            [("E0403", "[1]")]
        );
    }

    #[test]
    fn input_intrinsics_have_their_types() {
        assert_eq!(
            let_type("fun main() { let a = args(); }", "a"),
            Type::Array(Box::new(Type::String))
        );
        assert_eq!(
            let_type("fun main() { let n = parse_i64(\"1\"); }", "n"),
            Type::Option(Box::new(Type::I64))
        );
        assert_eq!(
            let_type("fun main() { let f = parse_f64(\"1.5\"); }", "f"),
            Type::Option(Box::new(Type::F64))
        );
        assert_eq!(
            errors("fun main() { let n = parse_i64(true); }"),
            [("E0403", "true")]
        );
        assert_eq!(
            errors("fun main() { let a = args(1); }"),
            [("E0405", "args(1)")]
        );
        assert_eq!(errors("fun args() {}\nfun main() {}"), [("E0302", "args")]);
    }
}
