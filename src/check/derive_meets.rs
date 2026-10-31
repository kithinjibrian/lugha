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
}

impl Checker {
    /// E0450 for `Json`, E0419 for any other interface, unless `arg` meets
    /// derivable interface `interface`.
    pub(super) fn meets_derived(&mut self, interface: &str, arg: &Type, span: Span) -> bool {
        let Some((path, why)) = self.unmet_derived(interface, arg, "", &mut HashSet::new()) else {
            return true;
        };
        let d = if interface == super::json::BOUND {
            errors::not_json(arg, &path, &super::json::describe(&why), span)
        } else {
            let name = interface.rsplit('.').next().unwrap_or(interface);
            errors::unmet_bound(arg, name, span)
        };
        self.report(d);
        false
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
            let bound = self
                .bounds(name)
                .interfaces
                .iter()
                .any(|a| a.key == interface);
            return if bound {
                None
            } else {
                at(Unmet::Unbounded(name.clone()))
            };
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
                    for b in &g.bounds.interfaces {
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
