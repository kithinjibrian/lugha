//! Arithmetic with a type that isn't yours on the left: `impl f64 { fun
//! mul(self, v: V): V }` makes `2.0 * v`, and `impl shapes.W { … }` makes
//! `w * v`. Such an `impl` holds only `add`, `sub`, `mul`, `div` and `rem`
//! of the operator's shape, and only in the module that declares the
//! right-hand type: so each pair of types has one owner, and two packages
//! can never both define `W * V` (each would have to import the other).

use super::{Checker, Type};
use crate::ast::{AppliedType, Expr, ExprKind, FunDecl, Ident, ImplDecl, TypeKind};
use crate::diagnostic::Diagnostic;
use crate::modules::overloads::{ARITHMETIC, NUMBERS, written_name};

impl Checker {
    /// The owner of an `impl` of a number or of another module's type, with
    /// its type: `None` (reported) when the type doesn't exist.
    pub(super) fn foreign_owner(&mut self, i: &ImplDecl) -> Option<(String, Type)> {
        let ty = match &i.module {
            None => number(&i.ty.name)?,
            Some(module) => {
                let written = crate::ast::Type {
                    kind: TypeKind::Applied(Box::new(AppliedType {
                        module: Some(module.clone()),
                        name: i.ty.clone(),
                        args: Vec::new(),
                    })),
                    span: i.ty.span,
                };
                // A generic type takes the `impl`'s parameters, checked after.
                let saved = std::mem::take(&mut self.type_params);
                let ty = self.resolve(&written);
                self.type_params = saved;
                ty
            }
        };
        let owner = match &ty {
            Type::Struct(k, _) | Type::Enum(k, _) => k.clone(),
            Type::Error => return None,
            number if number.is_numeric() => number.to_string(),
            _ => {
                let message = format!("`impl` of `{ty}` can only hold arithmetic for your types");
                self.report(Diagnostic::error("E0458", message, i.ty.span));
                return None;
            }
        };
        self.impl_owners.insert(i.span, ty.clone());
        Some((owner, ty))
    }

    /// E0458 unless `f`, in an `impl` of `owner` (not this module's type),
    /// is an arithmetic method whose right-hand type `param` is, or holds,
    /// one of this module's types.
    pub(super) fn foreign_method_ok(
        &mut self,
        owner: &str,
        f: &FunDecl,
        param: Option<&Type>,
    ) -> bool {
        let name = written_name(&f.name.name);
        let shaped =
            f.receiver.is_some_and(|r| !r.mutable) && f.params.len() == 1 && f.ret.is_some();
        if !ARITHMETIC.contains(&name) || !shaped {
            let message =
                format!("`{owner}` isn't declared here, so its `impl` can't hold `{name}`");
            let help = "only `add`, `sub`, `mul`, `div` and `rem` of the operator's shape \
                        (`fun mul(self, v: V): V`) can be added to a type you don't declare";
            self.report(Diagnostic::error("E0458", message, f.name.span).with_help(help));
            return false;
        }
        if param.is_some_and(|t| self.mentions_local(t)) {
            return true;
        }
        let right = param.map_or_else(|| "…".to_string(), Type::to_string);
        let message = format!(
            "`{owner} {} {right}` uses no type declared here, so it can't be defined here",
            symbol(name)
        );
        let help = "arithmetic with a type you don't declare on the left goes in the module \
                    that declares the right-hand type, so each pair of types has one place \
                    that defines it";
        self.report(Diagnostic::error("E0458", message, f.name.span).with_help(help));
        false
    }

    /// Whether `ty` is, or holds, a struct or enum this module declares.
    fn mentions_local(&self, ty: &Type) -> bool {
        match ty {
            Type::Struct(k, args) | Type::Enum(k, args) => {
                let short = k.rsplit('.').next().unwrap_or(k);
                *k == self.key(short) || args.iter().any(|a| self.mentions_local(a))
            }
            Type::Array(inner) | Type::Option(inner) => self.mentions_local(inner),
            _ => false,
        }
    }

    /// `lhs op rhs` with a number (or number literal) on the left and a
    /// struct or enum on the right, when a number type has the method:
    /// the call of the fitting one. `None` leaves the built-in operator.
    pub(super) fn number_on_left(
        &mut self,
        expr: &Expr,
        name: &str,
        (lhs, rhs): (&Expr, &Expr),
        mark: usize,
    ) -> Option<Type> {
        let owners: Vec<&str> = NUMBERS
            .iter()
            .copied()
            .filter(|n| {
                self.methods
                    .get(*n)
                    .is_some_and(|m| m.keys().any(|k| written_name(k) == name))
            })
            .collect();
        if owners.is_empty() {
            return None;
        }
        let reported = self.diagnostics.len();
        let literal = literal_of(lhs).is_some();
        let left = (!literal).then(|| self.value(lhs, None));
        let right = self.value(rhs, None);
        self.diagnostics.truncate(reported);
        if !matches!(right, Type::Struct(..) | Type::Enum(..)) {
            return None;
        }
        if left.as_ref().is_some_and(|l| !l.is_numeric()) {
            return None;
        }
        let fitting: Vec<(Type, String)> = owners
            .into_iter()
            .filter_map(|owner| {
                let ty = number(owner)?;
                let fits = match (&left, literal_of(lhs)) {
                    (Some(l), _) => *l == ty,
                    (None, Some(true)) => ty.is_float(),
                    (None, _) => ty.is_integer(),
                };
                let (method, _) =
                    self.methods.get(owner)?.iter().find(|(k, m)| {
                        written_name(k) == name && m.params.first() == Some(&right)
                    })?;
                fits.then(|| (ty, method.clone()))
            })
            .collect();
        match fitting.as_slice() {
            [(ty, method)] => {
                self.expect_type(lhs, &super::expr::Expect::of(ty.clone()));
                self.overload_choices.insert(expr.id, method.clone());
                let call = Ident {
                    name: method.clone(),
                    span: expr.span,
                };
                let ty = ty.clone();
                Some(self.on_value(
                    expr,
                    (lhs, &ty),
                    &call,
                    std::slice::from_ref(rhs),
                    (mark, None),
                ))
            }
            [] => None,
            several => {
                let types: Vec<String> = several.iter().map(|(t, _)| format!("`{t}`")).collect();
                let message = format!(
                    "this operand fits more than one `{name}` with `{right}`: {}",
                    types.join(", ")
                );
                let ways: Vec<String> = several
                    .iter()
                    .map(|(t, _)| match &lhs.kind {
                        ExprKind::Int(n) if t.is_float() => format!("`{n}.0`"),
                        ExprKind::Int(n) => format!("`{n} as {t}`"),
                        _ => format!("a `{t}`"),
                    })
                    .collect();
                let help = format!("say which: {}", ways.join(" or "));
                self.report(Diagnostic::error("E0457", message, lhs.span).with_help(help));
                Some(Type::Error)
            }
        }
    }
}

/// The number type named `name`.
fn number(name: &str) -> Option<Type> {
    Some(match name {
        "i8" => Type::I8,
        "i16" => Type::I16,
        "i32" => Type::I32,
        "i64" => Type::I64,
        "u8" => Type::U8,
        "u16" => Type::U16,
        "u32" => Type::U32,
        "u64" => Type::U64,
        "f32" => Type::F32,
        "f64" => Type::F64,
        _ => return None,
    })
}

/// For a number literal, whether it is a float one.
fn literal_of(e: &Expr) -> Option<bool> {
    match &e.kind {
        ExprKind::Int(_) => Some(false),
        ExprKind::Float(_) => Some(true),
        ExprKind::Unary(crate::ast::UnOp::Neg, inner) => literal_of(inner),
        _ => None,
    }
}

/// The operator a method stands for.
fn symbol(name: &str) -> &'static str {
    match name {
        "add" => "+",
        "sub" => "-",
        "mul" => "*",
        "div" => "/",
        _ => "%",
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    const V: &str =
        "struct V { x: f64 }\nimpl V { fun mul(self, k: f64): V = V { x: self.x * k }; }\n";

    #[test]
    fn a_number_on_the_left_calls_its_method() {
        ok(&format!(
            "{V}impl f64 {{ fun mul(self, v: V): V = v * self; }}\n\
             fun main() {{ let v = V {{ x: 1.0 }}; let a = 2.0 * v; let k = 3.0; let b = k * v; }}"
        ));
    }

    #[test]
    fn a_literal_fitting_two_number_types_is_e0457() {
        let src = format!(
            "{V}impl i64 {{ fun mul(self, v: V): V = v; }}\nimpl u8 {{ fun mul(self, v: V): V = v; }}\n\
             impl f64 {{ fun mul(self, v: V): V = v; }}\n\
             fun main() {{ let v = V {{ x: 1.0 }}; let a = 2 * v; let b = 2.0 * v; }}"
        );
        assert_eq!(errors(&src), [("E0457", "2")]);
    }

    #[test]
    fn only_arithmetic_for_this_module_s_types_is_allowed() {
        let src = format!(
            "{V}impl f64 {{\n\
             fun scale(self, v: V): V = v;\n\
             fun add(self, k: i64): f64 = self;\n\
             fun sub(mut self, v: V): V = v;\n}}\nfun main() {{}}"
        );
        assert_eq!(
            errors(&src),
            [("E0458", "scale"), ("E0458", "add"), ("E0458", "sub")]
        );
    }

    #[test]
    fn without_a_method_a_number_on_the_left_stays_e0404() {
        let src = format!("{V}fun main() {{ let v = V {{ x: 1.0 }}; let a = 2.0 * v; }}");
        assert_eq!(errors(&src)[0].0, "E0404");
    }
}
