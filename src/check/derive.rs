//! Derive blocks (v17, milestones 141–142, PRP-172, PRP-173): `derive I for X { … }`,
//! written in interface `I`'s own module, gives `I`'s items to the types `X`
//! covers: one type (`i64`, `T[]`), or every struct, enum or tuple whose
//! parts meet `I`. A block's items are methods of a hidden owner,
//! `<I>@<label>`, generic over the block's parameters (`Self` for a struct,
//! enum or tuple block), so each body is checked once and instantiated per
//! type like any generic method. Satisfaction, with E0450-style paths, is in
//! `derive_meets.rs`; the field intrinsics in `derive_intrinsics.rs`.

use std::collections::HashMap;

use super::applied::Applied;
use super::generics::{Bounds, Generic};
use super::instances::Instance;
use super::methods::Method;
use super::tuples::elements;
use super::{Checker, SELF, Type, errors};
use crate::ast::{DeriveDecl, DeriveTarget, Item, Program};
use crate::span::Span;

/// What a block covers.
#[derive(Debug, Clone, PartialEq)]
pub enum Covers {
    /// One type, written with the block's parameters: `Option<T>`.
    Type(Type),
    /// Every struct whose fields meet the interface.
    Struct,
    /// Every enum whose payloads meet the interface.
    Enum,
    /// Every tuple whose elements meet the interface.
    Tuple,
}

/// One `derive` block.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    /// The interface's key.
    pub interface: String,
    pub covers: Covers,
    /// Its parameters: the written ones, or `Self` for a struct, enum or tuple block.
    pub(crate) generics: Vec<Generic>,
    /// The hidden owner of its items: `std.$json_value.Json@i64`.
    pub owner: String,
    /// The whole block, from `derive` to its `}`.
    pub span: Span,
}

impl Block {
    /// The type its items take as `self`: the covered type, written with
    /// the block's parameters, or `Self` for a struct, enum or tuple block.
    pub fn self_type(&self) -> Type {
        match &self.covers {
            Covers::Type(t) => t.clone(),
            _ => Type::Param(SELF.to_string()),
        }
    }
}

/// Every derive block, by interface (`Checked::derives`).
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Derives {
    blocks: Vec<Block>,
    /// Struct and enum keys with their own item of a derivable interface,
    /// by item name: calls reach those instead of a block's (v17, milestone 142).
    pub(super) own: std::collections::HashSet<(String, String)>,
}

impl Derives {
    /// True if interface `key` has derive blocks: it is met through them.
    pub fn derivable(&self, key: &str) -> bool {
        self.blocks.iter().any(|b| b.interface == key)
    }

    /// The block of `interface` that covers `ty`, with its parameters'
    /// types: a block for `ty`'s own type first, then the struct, enum or
    /// tuple block. `holds_union` keeps C unions out of struct blocks.
    pub fn find(
        &self,
        interface: &str,
        ty: &Type,
        holds_union: impl Fn(&str) -> bool,
    ) -> Option<(&Block, Vec<Type>)> {
        let mine = || self.blocks.iter().filter(|b| b.interface == interface);
        for b in mine() {
            if let Covers::Type(pattern) = &b.covers {
                let names: Vec<&str> = b.generics.iter().map(|g| g.name.as_str()).collect();
                let mut bound = HashMap::new();
                if matches(pattern, ty, &names, &mut bound) {
                    let args = names.iter().map(|n| bound[*n].clone()).collect();
                    return Some((b, args));
                }
            }
        }
        let kind = match ty {
            Type::Struct(..) if elements(ty).is_some() => Covers::Tuple,
            Type::Struct(key, _) if !holds_union(key) => Covers::Struct,
            Type::Enum(..) => Covers::Enum,
            _ => return None,
        };
        let b = mine().find(|b| b.covers == kind)?;
        Some((b, vec![ty.clone()]))
    }

    /// Every interface that has blocks, each once.
    pub(super) fn interfaces(&self) -> Vec<String> {
        let mut keys: Vec<String> = self.blocks.iter().map(|b| b.interface.clone()).collect();
        keys.sort();
        keys.dedup();
        keys
    }

    /// The types `interface` has blocks for, as written: `i64`, `T[]`, `struct`s.
    pub(super) fn covered(&self, interface: &str) -> Vec<String> {
        self.blocks
            .iter()
            .filter(|b| b.interface == interface)
            .map(|b| match &b.covers {
                Covers::Type(t) => format!("`{}`", super::shown(&t.to_string())),
                Covers::Struct => "structs".to_string(),
                Covers::Enum => "enums".to_string(),
                Covers::Tuple => "tuples".to_string(),
            })
            .collect()
    }

    /// The instance a call of item `name` of `interface` reaches at `ty`:
    /// `None` when `ty` has its own item (or no block covers it).
    pub fn target(
        &self,
        interface: &str,
        name: &str,
        ty: &Type,
        holds_union: impl Fn(&str) -> bool,
    ) -> Option<Instance> {
        if let Type::Struct(owner, _) | Type::Enum(owner, _) = ty
            && self.own.contains(&(owner.clone(), name.to_string()))
        {
            return None;
        }
        let (block, args) = self.find(interface, ty, holds_union)?;
        Some(Instance {
            key: format!("{}.{name}", block.owner),
            args,
        })
    }

    /// The hidden owner of the block declared at `span`.
    pub fn owner_at(&self, span: Span) -> Option<&str> {
        let block = self.blocks.iter().find(|b| b.span == span)?;
        Some(&block.owner)
    }

    /// The type a block's items take as `self`, at the block's arguments.
    pub fn self_type(&self, owner: &str, args: &[Type]) -> Option<Type> {
        let block = self.blocks.iter().find(|b| b.owner == owner)?;
        Some(match &block.covers {
            Covers::Type(pattern) => super::generics::subst(pattern, &block.generics, args),
            _ => args.first()?.clone(),
        })
    }
}

/// True if `ty` is `pattern` with each parameter in `names` replaced, the
/// same way every time; `bound` gets what each stands for.
fn matches(pattern: &Type, ty: &Type, names: &[&str], bound: &mut HashMap<String, Type>) -> bool {
    match (pattern, ty) {
        (Type::Param(n), _) if names.contains(&n.as_str()) => match bound.get(n) {
            Some(seen) => seen == ty,
            None => {
                bound.insert(n.clone(), ty.clone());
                true
            }
        },
        (Type::Array(p), Type::Array(t)) | (Type::Option(p), Type::Option(t)) => {
            matches(p, t, names, bound)
        }
        (Type::Struct(pk, pa), Type::Struct(tk, ta)) | (Type::Enum(pk, pa), Type::Enum(tk, ta)) => {
            pk == tk
                && pa.len() == ta.len()
                && pa.iter().zip(ta).all(|(p, t)| matches(p, t, names, bound))
        }
        _ => pattern == ty,
    }
}

/// What a block covers, as written: `T[]`, `struct`.
fn block_type(covers: &Covers) -> String {
    match covers {
        Covers::Type(t) => super::shown(&t.to_string()),
        Covers::Struct => "struct".into(),
        Covers::Enum => "enum".into(),
        Covers::Tuple => "tuple".into(),
    }
}

/// A short, symbol-safe name for what a block covers: `i64`, `array`, `Map`.
fn label(covers: &Covers) -> String {
    match covers {
        Covers::Struct => "struct".into(),
        Covers::Enum => "enum".into(),
        Covers::Tuple => "tuple".into(),
        Covers::Type(Type::Array(_)) => "array".into(),
        Covers::Type(Type::Option(_)) => "option".into(),
        Covers::Type(Type::Unit) => "unit".into(),
        Covers::Type(Type::Struct(key, _) | Type::Enum(key, _)) => {
            key.rsplit('.').next().unwrap_or(key).to_string()
        }
        Covers::Type(other) => other.to_string(),
    }
}

/// True if one type could be covered by both patterns: each pattern's
/// parameters (in `a` and `b`) stand for any type.
fn overlap(a: &Type, b: &Type, a_names: &[&str], b_names: &[&str]) -> bool {
    match (a, b) {
        (Type::Param(n), _) if a_names.contains(&n.as_str()) => true,
        (_, Type::Param(n)) if b_names.contains(&n.as_str()) => true,
        (Type::Array(x), Type::Array(y)) | (Type::Option(x), Type::Option(y)) => {
            overlap(x, y, a_names, b_names)
        }
        (Type::Struct(ka, xa), Type::Struct(kb, xb)) | (Type::Enum(ka, xa), Type::Enum(kb, xb)) => {
            ka == kb
                && xa.len() == xb.len()
                && xa
                    .iter()
                    .zip(xb)
                    .all(|(x, y)| overlap(x, y, a_names, b_names))
        }
        _ => a == b,
    }
}

/// The names of a block's parameters.
fn names(block: &Block) -> Vec<&str> {
    block.generics.iter().map(|g| g.name.as_str()).collect()
}

impl Checker {
    /// Pass 2, after methods: every derive block's items, as methods of its
    /// hidden owner, each checked against the interface's signature.
    pub(super) fn collect_derives(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Derive(d) = item else { continue };
            self.collect_derive(d);
        }
        self.type_params.clear();
    }

    fn collect_derive(&mut self, d: &DeriveDecl) {
        // Declared here, not named through an import (v17, milestone 142).
        let interface = super::qualify(&self.module, &d.interface.name);
        if !self.interfaces.contains_key(&interface) {
            let d = errors::derive_elsewhere(&d.interface.name, d.interface.span);
            self.report(d);
            return;
        }
        let (generics, covers) = match &d.target {
            DeriveTarget::Type(t) => {
                let generics = self.generics_with(&d.type_params, &[]);
                self.type_params = generics.clone();
                (generics, Covers::Type(self.resolve(t)))
            }
            kind => {
                let bounds = Bounds {
                    interfaces: vec![Applied {
                        key: interface.clone(),
                        args: Vec::new(),
                    }],
                    ..Bounds::default()
                };
                let generics = vec![Generic {
                    name: SELF.to_string(),
                    bounds,
                }];
                let covers = match kind {
                    DeriveTarget::Struct => Covers::Struct,
                    DeriveTarget::Enum => Covers::Enum,
                    _ => Covers::Tuple,
                };
                (generics, covers)
            }
        };
        self.type_params = generics.clone();
        let mut owner = format!("{interface}@{}", label(&covers));
        // Two blocks for one generic type at different arguments share a
        // label: the second is named by its whole type.
        if self.derives.blocks.iter().any(|b| b.owner == owner) {
            owner = format!("{interface}@{}", block_type(&covers));
        }
        let block = Block {
            interface: interface.clone(),
            covers: covers.clone(),
            generics: generics.clone(),
            owner: owner.clone(),
            span: d.span,
        };
        let me = block.self_type();
        let clash = self.derives.blocks.iter().find(|b| {
            b.interface == interface
                && match (&b.covers, &block.covers) {
                    (Covers::Type(x), Covers::Type(y)) => overlap(x, y, &names(b), &names(&block)),
                    (x, y) => x == y,
                }
        });
        if clash.is_some() {
            let shown = interface
                .rsplit('.')
                .next()
                .unwrap_or(&interface)
                .to_string();
            self.report(errors::derive_overlap(&shown, &block_type(&covers), d.span));
            return;
        }
        let wanted = self.items_for(
            &Applied {
                key: interface.clone(),
                args: Vec::new(),
            },
            &me,
        );
        let mut problems = Vec::new();
        for f in &d.methods {
            let params: Vec<Type> = f.params.iter().map(|p| self.resolve(&p.ty)).collect();
            let ret = f.ret.as_ref().map_or(Type::Unit, |t| self.resolve(t));
            let receiver = f.receiver.map(|r| r.mutable);
            let fits = wanted.iter().any(|r| {
                r.name == f.name.name
                    && r.receiver == receiver
                    && r.params == params
                    && r.ret == ret
            });
            if !fits || !f.type_params.is_empty() {
                problems.push(match wanted.iter().find(|r| r.name == f.name.name) {
                    Some(r) => format!("`{}` should be `{}`", r.name, super::satisfy::shown(r)),
                    None => format!("`{}` isn't one of its items", f.name.name),
                });
                continue;
            }
            // Hover shows an item like a method (v17, milestone 142).
            let sig = super::Sig::of(
                &generics,
                generics.len(),
                receiver,
                (&f.params, &params),
                &ret,
            );
            self.hover.signatures.insert(f.name.span, sig);
            self.hover.function(f.name.span, f.name.span, None);
            let method = Method {
                receiver,
                params,
                ret,
                span: f.name.span,
                generics: generics.clone(),
                outer: generics.len(),
                key: format!("{owner}.{}", f.name.name),
            };
            let methods = self.methods.entry(owner.clone()).or_default();
            methods.insert(f.name.name.clone(), method);
        }
        for r in &wanted {
            if !d.methods.iter().any(|f| f.name.name == r.name) {
                problems.push(format!("no method `{}`", r.name));
            }
        }
        if !problems.is_empty() {
            let shown = format!("derive {} for {}", d.interface.name, block_type(&covers));
            let d = errors::block_unsatisfied(&shown, &d.interface.name, &problems, d.span);
            self.report(d);
        }
        self.derives.blocks.push(block);
    }

    /// Checks every derive block's bodies once, with `self` of the block's type.
    pub(super) fn derive_bodies(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Derive(d) = item else { continue };
            let Some(block) = self.derives.blocks.iter().find(|b| b.span == d.span) else {
                continue;
            };
            let block = block.clone();
            let me = block.self_type();
            for f in &d.methods {
                let found = self
                    .methods
                    .get(&block.owner)
                    .and_then(|m| m.get(&f.name.name));
                let Some(m) = found.filter(|m| m.span == f.name.span).cloned() else {
                    continue;
                };
                self.owner = m.key.clone();
                self.type_params = m.generics.clone();
                self.deriving = Some(block.clone());
                self.body(f, m.params, m.ret, Some(me.clone()));
            }
        }
        self.deriving = None;
    }
}
