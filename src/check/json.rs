//! `json.encode` (v17, milestone 139, PRP-171): the `json.Json` bound,
//! satisfied by a type's structure rather than an `impl` (E0450), and the
//! `$json.encode(x)` built-in std.json calls. Each use lists the Lugha
//! helpers in `std.$json_derive` that encoding its type reaches; codegen
//! writes the struct, enum and tuple walkers (`codegen/json_encode.rs`).

use std::collections::HashSet;

use super::expr::Expect;
use super::float_ops::builtin_op;
use super::generics::subst;
use super::instances::Instance;
use super::maps::{MAP, SET};
use super::tuples::elements;
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// The key of a `$json.encode` site's use: its one argument is the type.
pub(super) const JSON_ENCODE: &str = "$json.encode";

/// `std.json`'s `Value` and `Json` (declared in `std.$json_value`).
pub const VALUE: &str = "std.$json_value.Value";
pub(super) const BOUND: &str = "std.$json_value.Json";

/// The start of every helper's key in `std.$json_derive`.
pub const DERIVE: &str = "std.$json_derive.";

/// The `$json` built-in `callee` names, `encode` for `$json.encode`.
pub fn json_op(callee: &Expr) -> Option<&str> {
    builtin_op(callee, "$json")
}

/// `std.json`'s `Value` type.
pub fn value_type() -> Type {
    Type::Enum(VALUE.to_string(), Vec::new())
}

/// The helper that encodes a scalar of type `ty`, if it is one.
pub fn scalar_helper(ty: &Type) -> Option<&'static str> {
    Some(match ty {
        Type::U64 => "$u64",
        Type::F32 | Type::F64 => "$float",
        _ if ty.is_numeric() => "$int",
        Type::Bool => "$bool",
        Type::Char => "$char",
        Type::String => "$text",
        Type::Unit => "$null",
        _ => return None,
    })
}

/// The generic helper encoding a container of type `ty`, with its arguments.
pub fn container_helper(ty: &Type) -> Option<(&'static str, Vec<Type>)> {
    match ty {
        Type::Array(inner) => Some(("$enc_array", vec![(**inner).clone()])),
        Type::Option(inner) => Some(("$enc_option", vec![(**inner).clone()])),
        Type::Struct(key, args) if key == MAP => Some(("$enc_map", args.clone())),
        Type::Struct(key, args) if key == SET => Some(("$enc_set", args.clone())),
        _ => None,
    }
}

fn helper(name: &str, args: Vec<Type>) -> Instance {
    Instance {
        key: format!("{DERIVE}{name}"),
        args,
    }
}

impl Checker {
    /// `$json.encode(x)`: `x` of any type (the `Json` bound on `std.json`'s
    /// `encode` checks it at the user's call); a `Value`.
    pub(super) fn json_call(&mut self, call: &Expr, callee: &Expr, args: &[Expr]) -> Type {
        self.record(callee, Type::Unit);
        if let ExprKind::Field(base, _) = &callee.kind {
            self.record(base, Type::Unit);
        }
        if json_op(callee) != Some("encode") || args.len() != 1 {
            // A compiler bug: only std.json writes `$json`.
            let d = Diagnostic::error("E0415", "no such `$json` built-in", call.span);
            self.report(d);
            return self.unchecked_args(args);
        }
        let ty = self.value(&args[0], None::<&Expect>);
        self.used(call, JSON_ENCODE, std::slice::from_ref(&ty));
        value_type()
    }

    /// E0450 unless `arg` can be turned into JSON (the `json.Json` bound).
    pub(super) fn meets_json(&mut self, arg: &Type, span: Span) -> bool {
        match self.not_json(arg, "", &mut HashSet::new()) {
            None => true,
            Some((path, what)) => {
                self.report(errors::not_json(arg, &path, &what, span));
                false
            }
        }
    }

    /// The path to the first part of `ty` that isn't data, and what it is.
    fn not_json(
        &self,
        ty: &Type,
        path: &str,
        seen: &mut HashSet<Type>,
    ) -> Option<(String, String)> {
        let bad = |what: &str| Some((path.to_string(), what.to_string()));
        if scalar_helper(ty).is_some() || matches!(ty, Type::Error | Type::Never) {
            return None;
        }
        if !seen.insert(ty.clone()) {
            return None;
        }
        match ty {
            Type::Array(inner) => self.not_json(inner, &format!("{path}[]"), seen),
            Type::Option(inner) => self.not_json(inner, path, seen),
            Type::Param(name) => {
                let bound = self.bounds(name).interfaces.iter().any(|i| i.key == BOUND);
                if bound {
                    None
                } else {
                    bad(&format!(
                        "a type parameter without `json.Json` (write `{name}: json.Json`)"
                    ))
                }
            }
            Type::Struct(key, args) if key == MAP => {
                let keyed = args[0] == Type::String || args[0].is_integer();
                if !keyed {
                    return bad(&format!("a map with `{}` keys", args[0]));
                }
                self.not_json(&args[1], &format!("{path}[]"), seen)
            }
            Type::Struct(key, args) if key == SET => {
                self.not_json(&args[0], &format!("{path}[]"), seen)
            }
            Type::Struct(..) if elements(ty).is_some() => {
                let parts = elements(ty).unwrap_or_default().to_vec();
                parts
                    .iter()
                    .enumerate()
                    .find_map(|(i, t)| self.not_json(t, &format!("{path}.{i}"), seen))
            }
            Type::Struct(key, _) if self.holds_union(key) => bad("a struct holding a union"),
            Type::Struct(key, args) => {
                let info = self.structs.get(key)?;
                let fields: Vec<(String, Type)> = info
                    .fields
                    .iter()
                    .map(|(n, t)| (n.clone(), subst(t, &info.generics, args)))
                    .collect();
                fields
                    .iter()
                    .find_map(|(n, t)| self.not_json(t, &format!("{path}.{n}"), seen))
            }
            Type::Enum(key, _) if key == VALUE => None,
            Type::Enum(key, args) => {
                let variants = self.enum_variants(key, args).unwrap_or_default();
                variants.iter().find_map(|(name, payloads)| {
                    payloads
                        .iter()
                        .enumerate()
                        .find_map(|(i, t)| self.not_json(t, &format!("{path}.{name}.{i}"), seen))
                })
            }
            Type::Fun(..) => bad("a function"),
            Type::Interface(..) => bad("an interface value"),
            Type::Ref(_) => bad("a `Ref`"),
            Type::Task(_) => bad("a task"),
            Type::Chan(_) => bad("a channel"),
            Type::Actor(_) => bad("an actor's handle"),
            Type::Handle(_) => bad("a C handle"),
            _ => None,
        }
    }

    /// The `std.$json_derive` helpers encoding a value of concrete type
    /// `ty` reaches directly: a scalar's, a container's (whose body reaches
    /// the rest), and through a struct's, enum's or tuple's parts.
    pub(super) fn json_encode_instances(&self, ty: &Type) -> Vec<Instance> {
        let mut found = Vec::new();
        self.reach_json(ty, &mut HashSet::new(), &mut found);
        found
    }

    fn reach_json(&self, ty: &Type, seen: &mut HashSet<Type>, found: &mut Vec<Instance>) {
        if let Some(name) = scalar_helper(ty) {
            found.push(helper(name, Vec::new()));
            return;
        }
        if let Some((name, args)) = container_helper(ty) {
            found.push(helper(name, args));
            return;
        }
        if !seen.insert(ty.clone()) {
            return;
        }
        let parts: Vec<Type> = match ty {
            Type::Struct(..) if elements(ty).is_some() => {
                found.push(helper("$array", Vec::new()));
                elements(ty).unwrap_or_default().to_vec()
            }
            Type::Struct(key, args) => {
                found.push(helper("$object", Vec::new()));
                self.structs.get(key).map_or_else(Vec::new, |info| {
                    info.fields
                        .iter()
                        .map(|(_, t)| subst(t, &info.generics, args))
                        .collect()
                })
            }
            Type::Enum(key, _) if key == VALUE => Vec::new(),
            Type::Enum(key, args) => {
                let variants = self.enum_variants(key, args).unwrap_or_default();
                // Names alone are text; payloads go in a one-key object,
                // several of them in an array (externally tagged).
                found.push(helper("$text", Vec::new()));
                found.push(helper("$object", Vec::new()));
                found.push(helper("$array", Vec::new()));
                variants.into_iter().flat_map(|(_, p)| p).collect()
            }
            _ => Vec::new(),
        };
        for part in &parts {
            self.reach_json(part, seen, found);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{container_helper, scalar_helper};
    use crate::check::Type;

    #[test]
    fn scalars_pick_their_helper() {
        assert_eq!(scalar_helper(&Type::U8), Some("$int"));
        assert_eq!(scalar_helper(&Type::U64), Some("$u64"));
        assert_eq!(scalar_helper(&Type::F32), Some("$float"));
        assert_eq!(scalar_helper(&Type::Char), Some("$char"));
        assert_eq!(scalar_helper(&Type::Unit), Some("$null"));
        assert_eq!(scalar_helper(&Type::Array(Box::new(Type::I64))), None);
    }

    #[test]
    fn containers_pick_their_generic_helper() {
        let array = Type::Array(Box::new(Type::Bool));
        assert_eq!(
            container_helper(&array),
            Some(("$enc_array", vec![Type::Bool]))
        );
        let map = Type::Struct(super::MAP.to_string(), vec![Type::String, Type::I64]);
        assert_eq!(
            container_helper(&map),
            Some(("$enc_map", vec![Type::String, Type::I64]))
        );
        assert_eq!(container_helper(&Type::Struct("P".into(), vec![])), None);
    }
}
