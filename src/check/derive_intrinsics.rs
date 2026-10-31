//! The field intrinsics of derive blocks (v17, milestones 141–142,
//! PRP-172, PRP-173), written `derive.fields(…)` and so on. Each takes a generic function `f<F: I>` by name and
//! calls it once per field of the block's `Self`, at that field's type; the
//! body is checked once with `F` known only to meet `I`, and each instance
//! of the block reaches `f` at every field type (`instances.rs`):
//!
//! - `derive.fields(x, ctx, f)`: `R[]`, `f(ctx, name, field)` per field of
//!   a struct or tuple (tuples' names are `"0"`, `"1"`, …);
//! - `derive.build(ctx, f)`: `Result<Self, E>`, each field made by
//!   `f(ctx, name, at, of): Result<F, E>`, the first `Err` returned;
//! - `derive.variant(x, ctx, f)`: `(string, R[])`, an enum value's variant
//!   name and its payloads through `f(ctx, name, payload)`;
//! - `derive.variants()`: `(string, i64)[]`, each variant's name and payload count;
//! - `derive.build_variant(name, ctx, f)`: `Option<Result<Self, E>>`,
//!   `None` for an unknown name, else the variant built as `build` does;
//! - `derive.fold(x, init, f)` (v17, milestone 143): `init`'s type `A`,
//!   threaded through `f(acc, name, field): A` per field (an enum's: the
//!   payloads of the variant at hand, named by the variant), nothing allocated;
//! - `derive.index(x)`: an enum value's variant position, an `i64`;
//! - `derive.address(x)`: in a `for handle` block, the handle's address, a `u64`;
//! - `derive.all(a, b, f)` (v17, milestone 144): `bool`, true if `f(name, x, y)`
//!   holds for every pair of corresponding fields (an enum's: false for
//!   different variants, else every payload pair), stopping at the first false.

use super::derive::Covers;
use super::expr::Expect;
use super::float_ops::builtin_op;
use super::generics::Generic;
use super::results::RESULT;
use super::{Checker, SELF, Type, errors};
use crate::ast::{Expr, ExprKind};

/// The intrinsics' names, after `derive.`.
pub const DERIVE_OPS: [&str; 9] = [
    "fields",
    "build",
    "variant",
    "variants",
    "build_variant",
    "fold",
    "index",
    "address",
    "all",
];

/// The intrinsic `callee` would name: `fields` for `derive.fields`.
/// Whether it does depends on scope (`Checker::is_intrinsic`).
pub fn derive_op(callee: &Expr) -> Option<&str> {
    builtin_op(callee, "derive").filter(|op| DERIVE_OPS.contains(op))
}

/// What `f` must look like: its parameters after `ctx`, and whether it
/// returns `Result<F, E>` (making a field) or anything without `F` (reading one).
fn shape(op: &str) -> (Vec<Type>, bool) {
    let field = Type::Param("F".into());
    match op {
        "fields" | "variant" | "fold" => (vec![Type::String, field], false),
        _ => (vec![Type::String, Type::I64, Type::I64], true),
    }
}

impl Checker {
    /// True if `callee` is a field intrinsic here: always inside a derive
    /// block; elsewhere only when no local or import is named `derive`, so a
    /// program's own `derive.fields(…)` keeps its meaning.
    pub(super) fn is_intrinsic(&self, callee: &Expr) -> bool {
        derive_op(callee).is_some()
            && (self.deriving.is_some()
                || (self.local("derive").is_none() && !self.imports.contains_key("derive")))
    }

    /// A call of a field intrinsic, inside a derive block of the right kind.
    pub(super) fn derive_call(&mut self, call: &Expr, callee: &Expr, args: &[Expr]) -> Type {
        self.record(callee, Type::Unit);
        if let ExprKind::Field(base, _) = &callee.kind {
            self.record(base, Type::Unit);
        }
        let op = derive_op(callee).unwrap_or_default().to_string();
        match self.derive_typed(call, &op, args) {
            Ok(ty) => {
                self.derive_calls.insert(call.id, op);
                ty
            }
            Err(message) => {
                self.report(errors::bad_intrinsic(&op, &message, call.span));
                // Each argument gets a type once: `f`'s name would be E0412
                // as a value of its own.
                for arg in args {
                    if self.types[arg.id.0 as usize].is_some() {
                        continue;
                    }
                    match arg.kind {
                        ExprKind::Name(_) => self.record(arg, Type::Error),
                        _ => {
                            self.expr(arg, None);
                        }
                    }
                }
                Type::Error
            }
        }
    }

    fn derive_typed(&mut self, call: &Expr, op: &str, args: &[Expr]) -> Result<Type, String> {
        let block = self
            .deriving
            .clone()
            .ok_or("is used outside a derive block")?;
        let me = Type::Param(SELF.to_string());
        let fits = match (&block.covers, op) {
            (_, "fold" | "all") => {
                matches!(block.covers, Covers::Struct | Covers::Tuple | Covers::Enum)
            }
            (Covers::Enum, o) => matches!(o, "variant" | "variants" | "build_variant" | "index"),
            (Covers::Struct | Covers::Tuple, o) => matches!(o, "fields" | "build"),
            (Covers::Handle, o) => o == "address",
            (Covers::Type(_), _) => false,
        };
        if !fits {
            return Err(format!(
                "doesn't work in a `derive … for {}` block",
                kind_name(&block.covers)
            ));
        }
        let count = match op {
            "variants" => 0,
            "index" | "address" => 1,
            "build" => 2,
            _ => 3,
        };
        if args.len() != count {
            return Err(format!("takes {count} arguments"));
        }
        if op == "variants" {
            let pair = self.tuple_type(vec![Type::String, Type::I64]);
            return Ok(Type::Array(Box::new(pair)));
        }
        // Two values compared field by field (v17, milestone 144).
        if op == "all" {
            return self.derive_all(call, args, &block.interface);
        }
        // A variant's position, or a handle's address (v17, milestone 143).
        if op == "index" || op == "address" {
            self.value(&args[0], Some(&Expect::of(me)));
            return Ok(if op == "index" { Type::I64 } else { Type::U64 });
        }
        // The value, or the variant's name, comes before `ctx` and `f`.
        let first = match op {
            "fields" | "variant" | "fold" => Some(me.clone()),
            "build_variant" => Some(Type::String),
            _ => None,
        };
        if let Some(ty) = first {
            self.value(&args[0], Some(&Expect::of(ty)));
        }
        let ctx = self.value(&args[count - 2], None::<&Expect>);
        let f = &args[count - 1];
        self.record(f, Type::Unit);
        let ExprKind::Name(name) = &f.kind else {
            return Err("needs `f` to name a function of this module".into());
        };
        let key = self.key(name);
        let sig = self
            .functions
            .get(&key)
            .ok_or("needs `f` to name a function of this module")?;
        // `F: Eq` is written as the built-in flag (v17, milestone 144).
        let one_bound = |g: &[Generic]| {
            g.len() == 1
                && (g[0]
                    .bounds
                    .interfaces
                    .iter()
                    .any(|a| a.key == block.interface)
                    || block.interface == super::derive::EQ && g[0].bounds.eq)
        };
        if !one_bound(&sig.generics) {
            return Err(format!(
                "needs `f` generic over one `F: {}`",
                block.interface
            ));
        }
        // Read under the name `F`, whatever `f` calls its parameter.
        let map = [(sig.generics[0].name.clone(), Type::Param("F".into()))].into();
        let params: Vec<Type> = sig.params.iter().map(|p| p.substitute(&map)).collect();
        let ret = sig.ret.substitute(&map);
        let (rest, makes) = shape(op);
        if params.first() != Some(&ctx) || params[1..] != rest[..] {
            return Err("needs `f`'s parameters to be `(ctx, name: string, …)` as the intrinsic passes them".into());
        }
        let out = match (&ret, makes) {
            (Type::Enum(k, a), true) if k == RESULT && a[0] == Type::Param("F".into()) => {
                let error = a[1].clone();
                if mentions(&error, "F") {
                    return Err("needs `f`'s error type not to mention `F`".into());
                }
                let result = Type::Enum(RESULT.to_string(), vec![me, error]);
                if op == "build" {
                    result
                } else {
                    Type::Option(Box::new(result))
                }
            }
            // `fold` threads `init`'s type through every call (v17, milestone 143).
            (r, false) if op == "fold" => {
                if *r != ctx {
                    return Err("needs `f` to return the type of `init`".into());
                }
                ctx.clone()
            }
            (r, false) if !mentions(r, "F") => {
                let all = Type::Array(Box::new(r.clone()));
                if op == "fields" {
                    all
                } else {
                    self.tuple_type(vec![Type::String, all])
                }
            }
            _ => return Err("needs `f`'s result to fit the intrinsic".into()),
        };
        self.callees.insert(call.id, key.clone());
        self.field_used(call, &key);
        Ok(out)
    }
}

impl Checker {
    /// `derive.all(a, b, f)`: `f(name, x, y): bool` for each pair of
    /// corresponding fields, `F` bounded by the block's `interface`.
    fn derive_all(&mut self, call: &Expr, args: &[Expr], interface: &str) -> Result<Type, String> {
        let me = Type::Param(SELF.to_string());
        if args.len() != 3 {
            return Err("takes 3 arguments".into());
        }
        self.value(&args[0], Some(&Expect::of(me.clone())));
        self.value(&args[1], Some(&Expect::of(me)));
        let f = &args[2];
        self.record(f, Type::Unit);
        let ExprKind::Name(name) = &f.kind else {
            return Err("needs `f` to name a function of this module".into());
        };
        let key = self.key(name);
        let sig = self
            .functions
            .get(&key)
            .ok_or("needs `f` to name a function of this module")?;
        let bounded = sig.generics.len() == 1
            && sig.generics[0]
                .bounds
                .interfaces
                .iter()
                .any(|a| a.key == interface)
            || sig.generics.len() == 1
                && interface == super::derive::EQ
                && sig.generics[0].bounds.eq;
        if !bounded {
            return Err(format!(
                "needs `f` generic over one `F: {}`",
                super::shown(interface)
            ));
        }
        let field = Type::Param(sig.generics[0].name.clone());
        let shape = [Type::String, field.clone(), field];
        if sig.params[..] != shape[..] || sig.ret != Type::Bool {
            return Err("needs `f` to be `(name: string, x: F, y: F): bool`".into());
        }
        self.callees.insert(call.id, key.clone());
        self.field_used(call, &key);
        Ok(Type::Bool)
    }
}

/// `struct`, `enum`, `tuple` or the covered type, for messages.
fn kind_name(covers: &Covers) -> String {
    match covers {
        Covers::Struct => "struct".into(),
        Covers::Enum => "enum".into(),
        Covers::Tuple => "tuple".into(),
        Covers::Handle => "handle".into(),
        Covers::Type(t) => super::shown(&t.to_string()),
    }
}

/// True if `ty` mentions type parameter `name`.
fn mentions(ty: &Type, name: &str) -> bool {
    let map = [(name.to_string(), Type::Error)].into();
    ty.substitute(&map) != *ty
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::bundled;

    const COUNT: &str = "interface Count { fun count(self): i64; }\n\
        derive Count for i64 { fun count(self): i64 = 1; }\n\
        fun one<F: Count>(c: (), name: string, x: F): i64 = x.count();\n";

    /// The codes of checking `src` after `COUNT`.
    fn codes(src: &str) -> Vec<&'static str> {
        match bundled(&format!("{COUNT}{src}\nfun main() {{}}")) {
            Ok(_) => Vec::new(),
            Err(d) => d.iter().map(|d| d.code).collect(),
        }
    }

    #[test]
    fn each_intrinsic_has_its_result_type() {
        let ok = "derive Count for struct {\n\
                fun count(self): i64 { let xs: i64[] = derive.fields(self, (), one); xs.len }\n\
            }\n\
            derive Count for enum {\n\
                fun count(self): i64 {\n\
                    let (name, xs) = derive.variant(self, (), one);\n\
                    let counts: i64[] = xs;\n\
                    let all: (string, i64)[] = derive.variants();\n\
                    counts.len + all.len + name.len\n\
                }\n\
            }";
        assert_eq!(codes(ok), Vec::<&str>::new());
    }

    #[test]
    fn fold_threads_its_init_and_index_is_a_position() {
        let ok = "derive Count for struct { fun count(self): i64 = derive.fold(self, 0, plus); }\n\
            derive Count for enum { fun count(self): i64 = derive.fold(self, derive.index(self), plus); }\n\
            fun plus<F: Count>(sum: i64, name: string, x: F): i64 = sum + x.count();";
        assert_eq!(codes(ok), Vec::<&str>::new());
        // `f` must give back `init`'s type.
        let wrong = "derive Count for struct { fun count(self): i64 = derive.fold(self, 0, half); }\n\
            fun half<F: Count>(sum: i64, name: string, x: F): bool = true;";
        assert_eq!(codes(wrong), ["E0453"]);
    }

    #[test]
    fn all_compares_two_values_with_a_boolean_f() {
        let ok = "derive Count for struct { fun count(self): i64 = if derive.all(self, self, same) { 1 } else { 0 }; }\n\
            fun same<F: Count>(name: string, x: F, y: F): bool = x.count() == y.count();";
        assert_eq!(codes(ok), Vec::<&str>::new());
        let wrong = "derive Count for struct { fun count(self): i64 = if derive.all(self, self, two) { 1 } else { 0 }; }\n\
            fun two<F: Count>(name: string, x: F): bool = true;";
        assert_eq!(codes(wrong), ["E0453"]);
    }

    #[test]
    fn misuse_is_reported() {
        // Outside a block, or in the wrong kind of block.
        assert_eq!(codes("fun f(): i64 = derive.variants().len;"), ["E0453"]);
        let in_struct = "derive Count for struct { fun count(self): i64 = derive.variants().len; }";
        assert_eq!(codes(in_struct), ["E0453"]);
        // `f` must be generic over one `F` bounded by the block's interface.
        let unbounded = "fun any<F>(c: (), name: string, x: F): i64 = 1;\n\
            derive Count for struct { fun count(self): i64 = derive.fields(self, (), any).len; }";
        assert_eq!(codes(unbounded), ["E0453"]);
        let shape = "fun two<F: Count>(c: i64, name: string, x: F): i64 = 1;\n\
            derive Count for struct { fun count(self): i64 = derive.fields(self, (), two).len; }";
        assert_eq!(codes(shape), ["E0453"]);
        // A program's own `derive` keeps its meaning outside blocks.
        assert_eq!(
            codes("struct D { variants: fun(): i64 }\nfun f(derive: D): i64 = derive.variants();"),
            Vec::<&str>::new()
        );
        let named = "derive Count for struct { fun count(self): i64 { let derive = 1; derive } }";
        assert_eq!(codes(named), ["E0453"]);
    }
}
