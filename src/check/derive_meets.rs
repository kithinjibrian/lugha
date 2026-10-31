//! Meeting a derivable interface (v17, milestone 141, PRP-172): a type
//! meets it through the derive block that covers it, when the block's
//! parameters meet their bounds and, for a struct, enum or tuple block,
//! every part meets the interface in turn (recursive types included).
//! A miss names the path to the first part that doesn't, as E0450 always has.

use std::collections::HashSet;

use super::derive::Covers;
use super::generics::subst;
use super::tuples::elements;
use super::{Checker, Type, errors};
use crate::span::Span;

/// Why a part doesn't meet a derivable interface.
#[derive(Debug, Clone, PartialEq)]
pub(super) enum Unmet {
    /// No block covers this type.
    Missing(Type),
    /// A type parameter without the interface among its bounds.
    Unbounded(String),
    /// A block's parameter misses another derivable bound: `Map<bool, V>`'s
    /// keys, for one.
    Bound(String, Type),
    /// A struct holding a C union.
    Union,
    /// A struct's or enum's own method named like an item, with another
    /// signature (E0422): what it should be.
    Mismatch(String),
    /// A struct or enum with its own `eq` but no `hash`, or the other way
    /// round (E0454): the one it has (v17, milestone 144).
    HalfPair(String),
}

impl Checker {
    /// E0450 for `Json`, E0451 for any other interface, E0422 for an own
    /// method of the wrong shape, unless `arg` meets derivable interface
    /// `interface` (v17, milestone 142).
    pub(super) fn meets_derived(&mut self, interface: &str, arg: &Type, span: Span) -> bool {
        let Some((path, why)) = self.unmet_derived(interface, arg, "", &mut HashSet::new()) else {
            return true;
        };
        let name = interface.rsplit('.').next().unwrap_or(interface);
        let d = match &why {
            Unmet::HalfPair(has) => errors::half_pair(arg, has, span),
            Unmet::Mismatch(problem) => {
                let shown = if interface == super::json::BOUND {
                    "json.Json"
                } else {
                    name
                };
                // `json.Value`, as programs write it, not its key.
                let problem = problem.replace("std.$json_value.", "json.");
                errors::unsatisfied(arg, shown, &[problem], span)
            }
            _ if interface == super::json::BOUND => {
                errors::not_json(arg, &path, &super::json::describe(&why), span)
            }
            _ => {
                let covered = self.derives.covered(interface);
                let what = describe(name, &why);
                errors::not_derived(arg, name, (&path, &what), &covered, span)
            }
        };
        self.report(d);
        false
    }

    /// Whether struct or enum `owner` has its own item `name` of derivable
    /// interface `interface`: `Some(true)` with the item's exact signature,
    /// `Some(false)` with another, `None` without one (v17, milestone 142).
    pub(super) fn own_item(&self, interface: &str, owner: &str, name: &str) -> Option<bool> {
        let m = self.methods.get(owner)?.get(name)?;
        let args = super::generics::params(&m.generics[..m.outer]);
        let me = if self.enums.contains_key(owner) {
            Type::Enum(owner.to_string(), args)
        } else {
            Type::Struct(owner.to_string(), args)
        };
        let applied = super::applied::Applied {
            key: interface.to_string(),
            args: Vec::new(),
        };
        let req = self
            .items_for(&applied, &me)
            .into_iter()
            .find(|r| r.name == name)?;
        let fits = m.generics.len() == m.outer
            && req.receiver == m.receiver
            && req.params == m.params
            && req.ret == m.ret;
        Some(fits)
    }

    /// After every module's declarations: the items structs and enums
    /// supply themselves, so calls reach them instead of a block's.
    pub(super) fn derive_owners(&mut self) {
        let mut own = Vec::new();
        for interface in self.derives.interfaces() {
            let items: Vec<String> = self.interfaces[&interface]
                .items
                .iter()
                .map(|r| r.name.clone())
                .collect();
            let owners = self.structs.keys().chain(self.enums.keys());
            for owner in owners {
                for name in &items {
                    if self.own_item(&interface, owner, name) == Some(true) {
                        own.push((owner.clone(), name.clone()));
                    }
                }
            }
        }
        self.derives.own.extend(own);
    }

    /// The path to the first part of `ty` that doesn't meet `interface`, and why.
    pub(super) fn unmet_derived(
        &self,
        interface: &str,
        ty: &Type,
        path: &str,
        seen: &mut HashSet<Type>,
    ) -> Option<(String, Unmet)> {
        let at = |why: Unmet| Some((path.to_string(), why));
        if matches!(ty, Type::Error | Type::Never) || !seen.insert(ty.clone()) {
            return None;
        }
        if let Type::Param(name) = ty {
            let bounds = self.bounds(name);
            // `T: Eq` (or `Num`) is written as a flag (v17, milestone 144).
            let bound = bounds.interfaces.iter().any(|a| a.key == interface)
                || interface == super::derive::EQ && super::derive::implies_eq(&bounds);
            return if bound {
                None
            } else {
                at(Unmet::Unbounded(name.clone()))
            };
        }
        // A struct's or enum's own item must have the item's exact shape;
        // one that supplies every item needs no block (v17, milestone 142).
        if let Type::Struct(owner, _) | Type::Enum(owner, _) = ty
            && let Some(info) = self.interfaces.get(interface)
        {
            let all = info
                .items
                .iter()
                .all(|r| self.own_item(interface, owner, &r.name) == Some(true));
            if all {
                return None;
            }
            for r in &info.items {
                if self.own_item(interface, owner, &r.name) == Some(false) {
                    let applied = super::applied::Applied {
                        key: interface.to_string(),
                        args: Vec::new(),
                    };
                    let req = self.items_for(&applied, ty);
                    let req = req.iter().find(|q| q.name == r.name).unwrap_or(r);
                    let should = super::satisfy::shown(req);
                    return at(Unmet::Mismatch(format!(
                        "`{}` should be `{should}`",
                        r.name
                    )));
                }
            }
        }
        // `eq` and `hash` come together: equal values must hash equal (v17).
        if interface == super::derive::EQ
            && let Type::Struct(owner, _) | Type::Enum(owner, _) = ty
        {
            let has = |n: &str| self.own_item(interface, owner, n) == Some(true);
            match (has("eq"), has("hash")) {
                (true, false) => return at(Unmet::HalfPair("eq".into())),
                (false, true) => return at(Unmet::HalfPair("hash".into())),
                _ => {}
            }
        }
        let union = |key: &str| self.holds_union(key);
        let Some((block, args)) = self.derives.find(interface, ty, union) else {
            return match ty {
                Type::Struct(key, _) if self.holds_union(key) => at(Unmet::Union),
                _ => at(Unmet::Missing(ty.clone())),
            };
        };
        match &block.covers {
            Covers::Type(pattern) => {
                // An `Option` is its value's; anything else holds elements.
                let inner = match pattern {
                    Type::Option(_) => path.to_string(),
                    _ => format!("{path}[]"),
                };
                for (g, arg) in block.generics.iter().zip(&args) {
                    // `T: Eq` is written as the built-in flag (v17, milestone 144).
                    let eq = g.bounds.eq.then(super::derive::eq_bound);
                    for b in g
                        .bounds
                        .interfaces
                        .iter()
                        .cloned()
                        .chain(eq)
                        .collect::<Vec<_>>()
                        .iter()
                    {
                        if b.key == interface {
                            if let found @ Some(_) =
                                self.unmet_derived(interface, arg, &inner, seen)
                            {
                                return found;
                            }
                        } else if self.derives.derivable(&b.key)
                            && self
                                .unmet_derived(&b.key, arg, path, &mut HashSet::new())
                                .is_some()
                        {
                            return at(Unmet::Bound(b.key.clone(), arg.clone()));
                        }
                    }
                }
                None
            }
            // A handle has no parts (v17, milestone 143).
            Covers::Handle => None,
            Covers::Tuple => {
                let parts = elements(ty).unwrap_or_default().to_vec();
                parts.iter().enumerate().find_map(|(i, t)| {
                    self.unmet_derived(interface, t, &format!("{path}.{i}"), seen)
                })
            }
            Covers::Struct => {
                let (Type::Struct(key, targs) | Type::Enum(key, targs)) = ty else {
                    return None;
                };
                let info = self.structs.get(key)?;
                let fields: Vec<(String, Type)> = info
                    .fields
                    .iter()
                    .map(|(n, t)| (n.clone(), subst(t, &info.generics, targs)))
                    .collect();
                fields.iter().find_map(|(n, t)| {
                    self.unmet_derived(interface, t, &format!("{path}.{n}"), seen)
                })
            }
            Covers::Enum => {
                // `Option<T>` is `T`'s, at the same path.
                if let Type::Option(inner) = ty {
                    return self.unmet_derived(interface, inner, path, seen);
                }
                let (Type::Struct(key, targs) | Type::Enum(key, targs)) = ty else {
                    return None;
                };
                let variants = self.enum_variants(key, targs).unwrap_or_default();
                variants.iter().find_map(|(name, payloads)| {
                    payloads.iter().enumerate().find_map(|(i, t)| {
                        self.unmet_derived(interface, t, &format!("{path}.{name}.{i}"), seen)
                    })
                })
            }
        }
    }

    /// `Eq`'s item `name` (`eq` or `hash`) called on a value of concrete
    /// type `ty` without a method of its own by that name, when `ty` is
    /// `Eq`: the covering block's item (v17, milestone 144).
    pub(super) fn eq_item(
        &mut self,
        call: &crate::ast::Expr,
        ty: &Type,
        name: &str,
    ) -> Option<super::methods::Method> {
        let eq = super::derive::eq_bound();
        let req = self
            .items_for(&eq, ty)
            .into_iter()
            .find(|r| r.name == name)?;
        if matches!(ty, Type::Param(_) | Type::Interface(..) | Type::Error)
            || self
                .unmet_derived(super::derive::EQ, ty, "", &mut HashSet::new())
                .is_some()
        {
            return None;
        }
        let union = |key: &str| self.holds_union(key);
        let target = self.derives.target(super::derive::EQ, name, ty, union)?;
        self.derived.insert(call.id, super::derive::EQ.to_string());
        self.used(call, &target.key, &target.args);
        Some(super::methods::Method {
            receiver: req.receiver,
            params: req.params,
            ret: req.ret,
            span: req.span,
            generics: Vec::new(),
            outer: 0,
            key: String::new(),
        })
    }

    /// The `eq` an `==` on concrete type `ty` calls (v17, milestone 144):
    /// a struct's or enum's own (listed when generic or the prelude's), or
    /// the covering block's. Nothing for types compared inline.
    ///
    /// With `every`, a type's own plain (non-generic) `eq` too, for the
    /// purity of constants (milestone 147): it is compiled anyway, but runs.
    pub(super) fn eq_instances(&self, ty: &Type, every: bool) -> Vec<super::instances::Instance> {
        if !super::derive::compared_through_eq(ty) {
            return Vec::new();
        }
        if let Type::Struct(owner, targs) | Type::Enum(owner, targs) = ty
            && self
                .derives
                .own
                .contains(&(owner.clone(), "eq".to_string()))
        {
            let m = &self.methods[owner]["eq"];
            let listed = every || !m.generics.is_empty() || super::instances::lazy_key(&m.key);
            return if listed {
                vec![super::instances::Instance {
                    key: m.key.clone(),
                    args: targs.clone(),
                }]
            } else {
                Vec::new()
            };
        }
        let union = |key: &str| self.holds_union(key);
        let found = self.derives.target(super::derive::EQ, "eq", ty, union);
        found.into_iter().collect()
    }

    /// The types of `ty`'s parts a struct, enum or tuple block reaches: its
    /// fields, elements, or every variant's payloads in order.
    pub(super) fn part_types(&self, ty: &Type) -> Vec<Type> {
        if let Some(parts) = elements(ty) {
            return parts.to_vec();
        }
        match ty {
            Type::Struct(key, args) => self.structs.get(key).map_or_else(Vec::new, |info| {
                info.fields
                    .iter()
                    .map(|(_, t)| subst(t, &info.generics, args))
                    .collect()
            }),
            Type::Option(inner) => vec![(**inner).clone()],
            Type::Enum(key, args) => self
                .enum_variants(key, args)
                .unwrap_or_default()
                .into_iter()
                .flat_map(|(_, p)| p)
                .collect(),
            _ => Vec::new(),
        }
    }
}

/// What E0451 says a part is, for derivable interface `name`.
fn describe(name: &str, why: &Unmet) -> String {
    match why {
        Unmet::Missing(ty) => format!("`{ty}`, which no `derive {name}` block covers"),
        Unmet::Unbounded(p) => format!("a type parameter without `{name}` (write `{p}: {name}`)"),
        Unmet::Bound(key, ty) => {
            let other = key.rsplit('.').next().unwrap_or(key);
            format!("`{ty}`, which doesn't meet `{other}`")
        }
        Unmet::Union => "a struct holding a union".to_string(),
        Unmet::Mismatch(problem) => problem.clone(),
        Unmet::HalfPair(has) => format!("its own `{has}` alone"),
    }
}
