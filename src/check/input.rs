//! Input and interpolation (v3 draft §5, milestone 13): the intrinsics
//! `args`, `parse_i64` and `parse_f64`, and `"a {x} b"` strings; and the
//! I/O intrinsics (v4 draft §5, milestone 19).

use super::expr::Expect;
use super::results::RESULT;
use super::{Checker, Type, errors};
use crate::ast::{Expr, Part};
use crate::format;

/// The input and I/O intrinsics (v3, v4), checked here rather than in
/// `call.rs` — except `eprint`/`eprintln`, which follow `print`'s rules.
pub(super) const INPUT: [&str; 16] = [
    "args",
    "parse_i64",
    "parse_u64",
    "parse_f64",
    "read_line",
    "read_file",
    "write_file",
    "append_file",
    "eprint",
    "eprintln",
    "exit",
    // v14: bytes.
    "read_bytes",
    "write_bytes",
    "append_bytes",
    "read_stdin_bytes",
    "string_from_utf8",
];

impl Checker {
    /// `args()`, `parse_i64(s)`, `parse_f64(s)`, or an I/O intrinsic.
    pub(super) fn input_intrinsic(&mut self, call: &Expr, name: &str, args: &[Expr]) -> Type {
        let result = |t: Type| Type::Enum(RESULT.to_string(), vec![t, Type::String]);
        let bytes = || Type::Array(Box::new(Type::U8));
        let (params, ret) = match name {
            "args" => (Vec::new(), Type::Array(Box::new(Type::String))),
            "parse_i64" => (vec![Type::String], Type::Option(Box::new(Type::I64))),
            "parse_f64" => (vec![Type::String], Type::Option(Box::new(Type::F64))),
            "parse_u64" => (vec![Type::String], Type::Option(Box::new(Type::U64))),
            "read_line" => (Vec::new(), Type::Option(Box::new(Type::String))),
            "read_file" => (vec![Type::String], result(Type::String)),
            "write_file" | "append_file" => (vec![Type::String, Type::String], result(Type::I64)),
            "read_bytes" => (vec![Type::String], result(bytes())),
            "write_bytes" | "append_bytes" => (vec![Type::String, bytes()], result(Type::I64)),
            "read_stdin_bytes" => (Vec::new(), bytes()),
            "string_from_utf8" => (vec![bytes()], result(Type::String)),
            // `exit` never returns (v4 draft §5).
            _ => (vec![Type::I32], Type::Never),
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

    /// `"a {x} b"`: each interpolated value prints (v12; E0403 for `void`),
    /// and the whole is a `string`.
    pub(super) fn interpolation(&mut self, parts: &[Part]) -> Type {
        for part in parts {
            let Part::Expr(e, format) = part else {
                continue;
            };
            let ty = self.value(e, None);
            if !super::printing::printable(&ty) && !matches!(ty, Type::Error | Type::Never) {
                self.report(errors::not_interpolable(&ty, e.span));
            } else {
                self.printed(e, &ty);
            }
            // `{x:spec}` (v13): the specifier must parse and fit `x`.
            if let Some(f) = format
                && !matches!(ty, Type::Error | Type::Never)
            {
                let class = if ty.is_integer() {
                    format::Value::Int
                } else if ty.is_float() {
                    format::Value::Float
                } else {
                    format::Value::Other
                };
                if let Err(why) = format::parse(&f.text).and_then(|s| format::check(&s, class)) {
                    self.report(errors::bad_format(why, f.span));
                }
            }
        }
        Type::String
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type};

    #[test]
    fn interpolated_strings_are_strings() {
        let src = "fun main() { let n = 2; let f = 2.5; let s = \"x\"; let t = \"{n} {f} {true} {s} {n + 1}\"; }";
        assert_eq!(let_type(src, "t"), Type::String);
    }

    #[test]
    fn only_printable_values_interpolate() {
        // v12: every value but `void` interpolates.
        let src = "fun f() {}\nfun main() { let t = \"{f()}\"; }";
        assert_eq!(errors(src), [("E0407", "f()")]);
        crate::check::test_util::ok(
            "struct P { x: i64 }\nfun main() { let t = \"{[1]} {P { x: 1 }}\"; }",
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

    #[test]
    fn io_intrinsics_have_their_types() {
        let result = |a: Type| Type::Enum("$prelude.Result".into(), vec![a, Type::String]);
        let t = |body: &str, name: &str| let_type(&format!("fun main() {{ {body} }}"), name);
        assert_eq!(
            t("let l = read_line();", "l"),
            Type::Option(Box::new(Type::String))
        );
        assert_eq!(t("let f = read_file(\"p\");", "f"), result(Type::String));
        assert_eq!(
            t("let w = write_file(\"p\", \"t\");", "w"),
            result(Type::I64)
        );
        assert_eq!(
            t("let a = append_file(\"p\", \"t\");", "a"),
            result(Type::I64)
        );
        crate::check::test_util::ok("fun main() { eprint(1); eprintln(\"x\"); eprintln(); }");
        let w = crate::check::test_util::warnings("fun main() { exit(1); println(2); }");
        assert_eq!(w, [("W0101", "println(2);")]);
    }

    #[test]
    fn io_intrinsics_check_their_arguments() {
        assert_eq!(
            errors("fun main() { let l = read_line(1); }"),
            [("E0405", "read_line(1)")]
        );
        assert_eq!(
            errors("fun main() { let w = write_file(1, \"t\"); }"),
            [("E0401", "1")]
        );
        assert_eq!(errors("fun main() { exit(\"x\"); }"), [("E0403", "\"x\"")]);
        assert_eq!(
            errors("fun f() {}\nfun main() { eprint(f()); }"),
            [("E0407", "f()")]
        );
        assert_eq!(errors("fun exit() {}\nfun main() {}"), [("E0302", "exit")]);
    }

    #[test]
    fn format_specifiers_fit_their_values_v13() {
        let e = |body: &str| {
            errors(&format!(
                "struct P {{ x: i64 }}\nfun main() {{ let n = 1; let f = 2.5; {body} }}"
            ))
            .into_iter()
            .map(|(c, t)| (c, t.to_string()))
            .collect::<Vec<_>>()
        };
        assert_eq!(e("let t = \"{n:.2}\";")[0].0, "E0431");
        assert_eq!(e("let t = \"{f:x}\";")[0].0, "E0431");
        assert_eq!(e("let s = \"x\"; let t = \"{s:05}\";")[0].0, "E0431");
        assert_eq!(e("let t = \"{n:#}\";")[0].0, "E0431");
        assert_eq!(e("let t = \"{n:q}\";")[0].0, "E0431");
        assert_eq!(e("let t = \"{n:2000}\";")[0].0, "E0431");
        crate::check::test_util::ok(
            "struct P { x: i64 }\nfun main() { let p = P { x: 1 }; let n: u8 = 3; let t = \"{p:>20} {n:#010b} {2.5:+<8.1}\"; }",
        );
    }
}
