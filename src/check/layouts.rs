//! Concrete generic types (PRP-036): every instance of a generic struct or
//! enum the compiled code uses (`Stack<i64>`), so codegen gets their fields
//! and variants already substituted, keyed by the type as it prints. A type
//! that keeps growing (`struct S<T> { xs: S<T[]>[] }`) is E0420.

use std::collections::HashSet;

use super::generics::{has_param, subst};
use super::instances::{Instance, MAX_DEPTH};
use super::{Checker, Enums, Structs, Type, errors};

impl Checker {
    /// Remembers `ty` for the generic body being checked, when it mentions
    /// a type parameter.
    pub(super) fn body_type(&mut self, ty: &Type) {
        if self.owner.is_empty() || !has_param(ty) {
            return;
        }
        let types = self.body_types.entry(self.owner.clone()).or_default();
        if !types.contains(ty) {
            types.push(ty.clone());
        }
    }

    /// Every concrete instance of a generic struct or enum that the
    /// non-generic code or an instance in `monos` uses, inside other types
    /// too; and every concrete type those bodies use, for codegen to declare.
    pub(super) fn type_instances(&mut self, monos: &[Instance]) -> (Vec<Type>, Vec<Type>) {
        let mut seeds: Vec<Type> = self.types.iter().flatten().cloned().collect();
        seeds.extend(self.annotations.values().cloned());
        seeds.retain(|t| !has_param(t));
        for mono in monos {
            let generics = self.params_of(&mono.key);
            let (params, ret) = self.signature_types(&mono.key);
            let body = self.body_types.get(&mono.key).cloned().unwrap_or_default();
            let all = body.iter().chain(&params).chain([&ret]);
            seeds.extend(all.map(|t| subst(t, &generics, &mono.args)));
        }
        let mut used: Vec<Type> = Vec::new();
        let mut unique = HashSet::new();
        seeds.retain(|t| unique.insert(t.clone()));
        let (mut found, mut seen) = (Vec::new(), HashSet::new());
        for seed in seeds {
            if !self.visit(&seed, &mut found, &mut seen) {
                return (Vec::new(), Vec::new());
            }
            used.push(seed);
        }
        (found, used)
    }

    /// Adds the generic instances inside `ty`, with their fields' and
    /// payloads'. False after E0420.
    fn visit(&mut self, ty: &Type, found: &mut Vec<Type>, seen: &mut HashSet<Type>) -> bool {
        let (key, args) = match ty {
            Type::Array(inner)
            | Type::Option(inner)
            | Type::Ref(inner)
            | Type::Task(inner)
            | Type::Chan(inner) => {
                return self.visit(inner, found, seen);
            }
            Type::Fun(params, ret) => {
                let parts: Vec<Type> = params.iter().chain([&**ret]).cloned().collect();
                return parts.iter().all(|t| self.visit(t, found, seen));
            }
            Type::Struct(key, args) | Type::Enum(key, args) if !args.is_empty() => (key, args),
            _ => return true,
        };
        if depth(ty) > MAX_DEPTH {
            let span = self.type_span(key).expect("a resolved type is declared");
            self.report(errors::type_too_deep(ty, span));
            return false;
        }
        if !seen.insert(ty.clone()) {
            return true;
        }
        found.push(ty.clone());
        let mut inner: Vec<Type> = args.clone();
        if let Some(info) = self.structs.get(key) {
            let fields = info
                .fields
                .iter()
                .map(|(_, t)| subst(t, &info.generics, args));
            inner.extend(fields.collect::<Vec<_>>());
        }
        for (_, payloads) in self.enum_variants(key, args).unwrap_or_default() {
            inner.extend(payloads);
        }
        inner.iter().all(|t| self.visit(t, found, seen))
    }

    /// The declared parameter and return types of function or method `key`.
    fn signature_types(&self, key: &str) -> (Vec<Type>, Type) {
        if let Some(f) = self.functions.get(key) {
            return (f.params.clone(), f.ret.clone());
        }
        key.rsplit_once('.')
            .and_then(|(owner, name)| self.methods.get(owner)?.get(name))
            .map_or((Vec::new(), Type::Void), |m| {
                (m.params.clone(), m.ret.clone())
            })
    }

    /// The struct and enum tables codegen lays out: every non-generic
    /// declaration by its key, and every instance in `instances` by the
    /// type as it prints (`Stack<i64>`), substituted.
    pub(super) fn tables(&self, instances: &[Type]) -> (Structs, Enums) {
        let mut structs: Structs = self
            .structs
            .iter()
            .filter(|(_, info)| info.generics.is_empty())
            .map(|(key, info)| (key.clone(), info.fields.clone()))
            .collect();
        let mut enums: Enums = self
            .enums
            .iter()
            .filter(|(_, info)| info.generics.is_empty())
            .map(|(key, info)| (key.clone(), info.variants.clone()))
            .collect();
        for ty in instances {
            match ty {
                Type::Struct(key, args) => {
                    let info = &self.structs[key];
                    let fields = info.fields.iter();
                    let fields = fields.map(|(f, t)| (f.clone(), subst(t, &info.generics, args)));
                    structs.insert(ty.to_string(), fields.collect());
                }
                Type::Enum(key, args) => {
                    let variants = self
                        .enum_variants(key, args)
                        .expect("an instance's enum is declared");
                    enums.insert(ty.to_string(), variants);
                }
                _ => {}
            }
        }
        (structs, enums)
    }
}

/// How deeply type constructors nest in `ty`: `i64` is 0, `S<i64[]>` is 2.
fn depth(ty: &Type) -> usize {
    match ty {
        Type::Array(inner)
        | Type::Option(inner)
        | Type::Ref(inner)
        | Type::Task(inner)
        | Type::Chan(inner) => 1 + depth(inner),
        Type::Fun(params, ret) => 1 + params.iter().chain([&**ret]).map(depth).max().unwrap_or(0),
        Type::Struct(_, args) | Type::Enum(_, args) | Type::Interface(_, args) => {
            args.iter().map(depth).max().map_or(0, |d| d + 1)
        }
        _ => 0,
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    #[test]
    fn every_used_instance_gets_a_substituted_layout() {
        let checked = ok("struct Box<T> { v: T }\n\
            enum Tree<T> { Leaf, Node(Tree<T>, Box<T>) }\n\
            fun wrap<T>(x: T): Box<T> = Box { v: x };\n\
            fun main() { let t: Tree<string> = Tree.Leaf; let b = wrap(1.5); }");
        assert!(
            !checked.structs.contains_key("Box"),
            "generic declarations aren't laid out"
        );
        let fields = &checked.structs["Box<string>"];
        assert_eq!(fields[0].1, crate::check::Type::String);
        assert!(
            checked.structs.contains_key("Box<f64>"),
            "{:?}",
            checked.structs.keys()
        );
        assert!(checked.enums.contains_key("Tree<string>"));
        assert!(!checked.enums.contains_key("Tree"));
    }

    #[test]
    fn a_type_that_keeps_growing_is_e0420() {
        let src = "struct S<T> { xs: S<T[]>[] }\nfun main() { let s: S<i64> = S { xs: [] }; }";
        assert_eq!(errors(src), [("E0420", "S")]);
    }
}
