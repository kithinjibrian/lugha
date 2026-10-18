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
    builtin_op(callee, "$float")
}

/// The `$os` built-in `callee` names, `seed` for `$os.seed` (v15).
pub fn os_op(callee: &Expr) -> Option<&str> {
    builtin_op(callee, "$os")
}

/// The parameters and result of `$os.op` (v15): `seed` for `std.random`,
/// `env` and `cwd` for `std.env`, the folder operations for `std.fs` (text
/// as bytes, checked as UTF-8 in Lugha).
pub fn os_signature(op: &str) -> Option<(Vec<Type>, Type)> {
    let bytes = || Type::Array(Box::new(Type::U8));
    let result = |t: Type| Type::Enum(super::results::RESULT.to_string(), vec![t, Type::String]);
    Some(match op {
        "seed" => (Vec::new(), Type::U64),
        // A monotonic clock in milliseconds, for `std.http`'s deadlines (v17, milestone 111).
        "now_ms" => (Vec::new(), Type::I64),
        // `std.time` and `std.fs` (v17, milestone 115).
        "wall_ms" => (Vec::new(), Type::I64),
        "rename" => (vec![Type::String, Type::String], result(Type::I64)),
        "chmod" => (vec![Type::String, Type::I64], result(Type::I64)),
        // v17, milestone 116.
        "sync" => (vec![Type::String], result(Type::I64)),
        "env" => (vec![Type::String], Type::Option(Box::new(bytes()))),
        "cwd" => (Vec::new(), result(bytes())),
        // v16, milestone 68: `std.env.set_current_dir`.
        "chdir" => (vec![Type::String], result(Type::I64)),
        // `std.fs` (milestone 65).
        "list_dir" => (vec![Type::String], result(Type::Array(Box::new(bytes())))),
        "make_dir" | "remove_file" | "remove_dir" => (vec![Type::String], result(Type::I64)),
        "stat_kind" => (vec![Type::String], Type::I64),
        // `std.process` (milestone 66): the result's tuple is built by
        // `os_call`, which can register it; `Unit` stands in here.
        "start" => (
            vec![
                Type::String,
                Type::Array(Box::new(Type::String)),
                Type::String,
                Type::I32,
            ],
            Type::Unit,
        ),
        // `std.task` (v17, milestone 91).
        "sleep" => (vec![Type::I64], Type::Unit),
        "yield" => (Vec::new(), Type::Unit),
        // `std.net` (v17, milestone 110): sockets as `i64` descriptors.
        "net_listen" => (vec![Type::String], result(Type::I64)),
        "net_connect" | "net_accept" => (
            vec![
                if op == "net_connect" {
                    Type::String
                } else {
                    Type::I64
                },
                Type::I64,
            ],
            result(Type::I64),
        ),
        "net_read" => (vec![Type::I64, Type::I64, Type::I64], result(bytes())),
        "net_write" => (vec![Type::I64, bytes(), Type::I64], result(Type::I64)),
        "net_close" => (vec![Type::I64], Type::Unit),
        "net_name" => (vec![Type::I64, Type::I64], result(Type::String)),
        // `std.tls` (v17, milestone 112): connections and listeners as `i64` handles.
        "tls_connect" => (
            vec![Type::String, Type::String, Type::String, Type::I64],
            result(Type::I64),
        ),
        "tls_listen" => (
            vec![Type::String, Type::String, Type::String],
            result(Type::I64),
        ),
        "tls_accept" => (vec![Type::I64, Type::I64], result(Type::I64)),
        "tls_read" => (vec![Type::I64, Type::I64, Type::I64], result(bytes())),
        "tls_write" => (vec![Type::I64, bytes(), Type::I64], result(Type::I64)),
        "tls_close" | "tls_close_listener" => (vec![Type::I64], Type::Unit),
        "tls_fd" | "tls_listener_fd" => (vec![Type::I64], Type::I64),
        // `std.crypto` (v17, milestone 113): bytes in, bytes out; a running
        // hash as an opaque `CPtr`; flags as `i64`.
        "crypto_sha256" => (vec![bytes()], bytes()),
        "crypto_sha256_new" => (Vec::new(), Type::Handle(super::input::CPTR.to_string())),
        "crypto_sha256_update" => (
            vec![Type::Handle(super::input::CPTR.to_string()), bytes()],
            Type::Unit,
        ),
        "crypto_sha256_finish" => (vec![Type::Handle(super::input::CPTR.to_string())], bytes()),
        "crypto_seed" => (Vec::new(), result(bytes())),
        "crypto_ed25519_public" => (vec![bytes()], result(bytes())),
        "crypto_ed25519_sign" => (vec![bytes(), bytes()], result(bytes())),
        "crypto_ed25519_verify" => (vec![bytes(), bytes(), bytes()], Type::I64),
        "crypto_random" => (vec![Type::I64], result(bytes())),
        "crypto_equal" => (vec![bytes(), bytes()], Type::I64),
        _ => return None,
    })
}

/// The built-in a `$string.op(…)` callee names (v15, milestone 62).
pub fn string_op(callee: &Expr) -> Option<&str> {
    builtin_op(callee, "$string")
}

pub(super) fn builtin_op<'e>(callee: &'e Expr, namespace: &str) -> Option<&'e str> {
    let ExprKind::Field(base, name) = &callee.kind else {
        return None;
    };
    matches!(&base.kind, ExprKind::Name(n) if n == namespace).then_some(name.name.as_str())
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
        self.record(callee, Type::Unit);
        if let ExprKind::Field(base, _) = &callee.kind {
            self.record(base, Type::Unit);
        }
        // `$float.from_f64(x)`: an `f64` as the float type expected (v15).
        if op == "from_f64" {
            return self.convert_from_f64(call, args, expect);
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

impl Checker {
    /// `$float.from_f64(x)`: `x` an `f64`, the result the float type the
    /// context expects (`f32`, `f64` or a `T: Float`).
    fn convert_from_f64(&mut self, call: &Expr, args: &[Expr], expect: Option<&Expect>) -> Type {
        if args.len() != 1 {
            self.report(errors::arity("from_f64", 1, args.len(), call.span));
            return self.unchecked_args(args);
        }
        self.expect_type(&args[0], &Expect::of(Type::F64));
        let target = expect.map(|e| e.ty.clone());
        match target {
            Some(ty) if ty.is_float() || matches!(&ty, Type::Param(p) if self.has_float(p)) => ty,
            _ => {
                let d =
                    Diagnostic::error("E0412", "from_f64 needs an expected float type", call.span);
                self.report(d);
                Type::Error
            }
        }
    }

    /// `$os.op(args)` in a std module (v15): the operating system's
    /// services, typed by `os_signature`.
    pub(super) fn os_call(
        &mut self,
        call: &Expr,
        (callee, op): (&Expr, &str),
        args: &[Expr],
    ) -> Type {
        self.record(callee, Type::Unit);
        if let ExprKind::Field(base, _) = &callee.kind {
            self.record(base, Type::Unit);
        }
        match os_signature(op) {
            Some((params, ret)) if params.len() == args.len() => {
                for (arg, ty) in args.iter().zip(params) {
                    self.expect_type(arg, &Expect::of(ty));
                }
                if op == "start" {
                    // `(code, stdout bytes, stderr bytes)`, or the reason it didn't start.
                    let bytes = Type::Array(Box::new(Type::U8));
                    let outcome = self.tuple_type(vec![Type::I32, bytes.clone(), bytes]);
                    return Type::Enum(
                        super::results::RESULT.to_string(),
                        vec![outcome, Type::String],
                    );
                }
                ret
            }
            _ => {
                let d = Diagnostic::error(
                    "E0415",
                    format!("no `$os` built-in `{op}` with these arguments"),
                    call.span,
                );
                self.report(d);
                self.unchecked_args(args)
            }
        }
    }
}

impl Checker {
    /// `$string.from_bytes(bytes)`: already-checked UTF-8 bytes as a
    /// string (v15, milestone 62); only bundled sources can name it.
    pub(super) fn string_call(
        &mut self,
        call: &Expr,
        (callee, op): (&Expr, &str),
        args: &[Expr],
    ) -> Type {
        self.record(callee, Type::Unit);
        if let ExprKind::Field(base, _) = &callee.kind {
            self.record(base, Type::Unit);
        }
        if op != "from_bytes" || args.len() != 1 {
            let d = Diagnostic::error(
                "E0415",
                format!("no `$string` built-in `{op}` with these arguments"),
                call.span,
            );
            self.report(d);
            return self.unchecked_args(args);
        }
        self.expect_type(&args[0], &Expect::of(Type::Array(Box::new(Type::U8))));
        Type::String
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
    fn std_random_is_typed_by_its_arguments() {
        let none: [&str; 0] = [];
        let found = prelude_codes(&[(
            "main.la",
            "import std.random;\nfun main() { let mut r = random.seeded(1); \
             let x: f32 = r.normal(0.0, 1.0); let y = r.normal(0.0, 1.0); let u = r.uniform(); \
             let n = r.below(3); let c = r.choice([1, 2]); let s = r.shuffle([\"a\"]); }\n",
        )]);
        assert_eq!(found, none);
        let found = prelude_codes(&[(
            "main.la",
            "import std.random;\nfun main() { let mut r = random.seeded(1); let z = r.normal(0, 1); }\n",
        )]);
        assert_eq!(found, ["E0419"]);
    }

    #[test]
    fn programs_cannot_name_the_built_ins() {
        assert!(crate::lexer::lex("fun main() { let r = $float.sqrt(2.0); }").is_err());
        assert!(crate::lexer::lex("fun main() { let r = $os.seed(); }").is_err());
    }
}
