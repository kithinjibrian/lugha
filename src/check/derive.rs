//! Derive blocks (v17, milestone 141, PRP-172): `$derive I for X { … }`,
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
use super::{Checker, SELF, Type};
use crate::ast::{DeriveDecl, DeriveTarget, Item, Program};
use crate::diagnostic::Diagnostic;
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

/// One `$derive` block.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    /// The interface's key.
    pub interface: String,
    pub covers: Covers,
    /// Its parameters: the written ones, or `Self` for a struct, enum or tuple block.
    pub(crate) generics: Vec<Generic>,
    /// The hidden owner of its items: `std.$json_value.Json@i64`.
    pub owner: String,
    /// The whole block, from `$derive` to its `}`.
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

    /// The instance a call of item `name` of `interface` reaches at `ty`.
    pub fn target(
        &self,
        interface: &str,
        name: &str,
        ty: &Type,
        holds_union: impl Fn(&str) -> bool,
    ) -> Option<Instance> {
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

/// An internal error in a bundled source's derive block: only lughac's own
/// sources can write one, so this is a compiler bug, not a user mistake.
fn misuse(message: String, span: Span) -> Diagnostic {
    Diagnostic::error("E0201", format!("bad `$derive` block: {message}"), span)
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
        let interface = self.key(&d.interface.name);
        if !self.interfaces.contains_key(&interface) {
            let message = format!("`{}` isn't an interface of this module", d.interface.name);
            self.report(misuse(message, d.interface.span));
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
        let owner = format!("{interface}@{}", label(&covers));
        let block = Block {
            interface: interface.clone(),
            covers: covers.clone(),
            generics: generics.clone(),
            owner: owner.clone(),
            span: d.span,
        };
        let me = block.self_type();
        let wanted = self.items_for(
            &Applied {
                key: interface.clone(),
                args: Vec::new(),
            },
            &me,
        );
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
                let message = format!("`{}` isn't `{interface}`'s item for `{me}`", f.name.name);
                self.report(misuse(message, f.name.span));
                continue;
            }
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
                let message = format!("no `{}` for `{me}`", r.name);
                self.report(misuse(message, d.span));
            }
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

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::bundled;

    /// A derivable `Show`: blocks for `i64`, arrays, structs and enums.
    const SHOW: &str = "interface Show { fun $show(self): string; }\n\
        $derive Show for i64 { fun $show(self): string = \"{self}\"; }\n\
        $derive<T: Show> Show for T[] {\n\
            fun $show(self): string { let mut s = \"\"; for x of self { s = s + x.$show(); } s }\n\
        }\n\
        $derive Show for struct { fun $show(self): string = $join($derive.fields(self, (), $part)); }\n\
        $derive Show for enum {\n\
            fun $show(self): string { let (n, ps) = $derive.variant(self, (), $part); n + $join(ps) }\n\
        }\n\
        fun $part<F: Show>(c: (), name: string, x: F): string = name + \"=\" + x.$show();\n\
        fun $join(xs: string[]): string { let mut s = \"\"; for x of xs { s = s + x; } s }\n\
        fun show<T: Show>(x: T): string = x.$show();\n";

    /// `key<args>` of every instance of `src` after `SHOW`.
    fn monos(src: &str) -> Vec<String> {
        let checked = bundled(&format!("{SHOW}{src}")).unwrap_or_else(|d| panic!("{d:?}"));
        checked
            .monos
            .iter()
            .map(|m| {
                let args: Vec<String> = m.args.iter().map(Type::to_string).collect();
                format!("{}<{}>", m.key, args.join(", "))
            })
            .collect()
    }

    /// The codes of checking `src` after `SHOW`.
    fn codes(src: &str) -> Vec<&'static str> {
        match bundled(&format!("{SHOW}{src}")) {
            Ok(_) => Vec::new(),
            Err(d) => d.iter().map(|d| d.code).collect(),
        }
    }

    #[test]
    fn a_type_uses_its_own_block_then_the_struct_block() {
        let found = monos(
            "struct P { a: i64, b: i64[] }\nfun main() { let s = show(P { a: 1, b: [2] }); }",
        );
        for want in [
            "show<P>",
            "Show@struct.$show<P>",
            "$part<i64>",
            "$part<i64[]>",
            "Show@array.$show<i64>",
        ] {
            assert!(found.iter().any(|m| m == want), "{want}: {found:?}");
        }
        // `i64`'s block isn't generic: listed once, without arguments.
        assert!(found.iter().any(|m| m == "Show@i64.$show<>"), "{found:?}");
    }

    #[test]
    fn recursive_and_generic_types_meet_it_through_their_parts() {
        let list = "enum List { Nil, Cons(i64, List) }\n\
            fun main() { let s = show(List.Cons(1, List.Nil)); }";
        assert!(monos(list).iter().any(|m| m == "Show@enum.$show<List>"));
        let boxed = "struct Box<T> { v: T }\n\
            fun f<T: Show>(b: Box<T>): string = show(b);\nfun main() { let s = f(Box { v: 1 }); }";
        assert!(
            monos(boxed)
                .iter()
                .any(|m| m == "Show@struct.$show<Box<i64>>")
        );
    }

    #[test]
    fn a_part_no_block_covers_misses_the_bound() {
        let fun_field = "struct Job { run: fun(i64): i64 }\n\
            fun main() { let s = show(Job { run: fun(x: i64): i64 = x }); }";
        assert_eq!(codes(fun_field), ["E0419"]);
        let unbounded =
            "struct Box<T> { v: T }\nfun f<T>(b: Box<T>): string = show(b);\nfun main() {}";
        assert_eq!(codes(unbounded), ["E0419"]);
        assert_eq!(codes("fun main() { let s = show(true); }"), ["E0419"]);
    }

    #[test]
    fn a_block_must_be_for_its_own_modules_interface_and_match_it() {
        let src = "$derive Nope for i64 { fun $show(self): string = \"\"; }\nfun main() {}";
        assert_eq!(codes(src), ["E0201"]);
        let wrong = "$derive Show for bool { fun $show(self): i64 = 1; }\nfun main() {}";
        assert_eq!(codes(wrong), ["E0201"]);
    }
}
