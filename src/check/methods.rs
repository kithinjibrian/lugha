//! Methods (v1 §4, §6): `impl` blocks, their signatures (E0310) and bodies.
//! Calls are in `method_calls.rs`.

use std::collections::HashMap;

use super::generics::{Generic, params, with_args};
use super::{Checker, Type, errors};
use crate::ast::{Item, Program};
use crate::span::Span;

/// A method or associated function of one type.
#[derive(Debug, Clone)]
pub(super) struct Method {
    /// `None` for an associated function; `Some(mutable)` for `self` / `mut self`.
    pub receiver: Option<bool>,
    pub params: Vec<Type>,
    pub ret: Type,
    /// The method's name in its declaration.
    pub span: Span,
    /// The `impl`'s type parameters, then the method's own (PRP-035).
    pub generics: Vec<Generic>,
    /// How many of `generics` are the `impl`'s, bound by the receiver's type.
    pub outer: usize,
    /// `Type.name` (`Stack.push`; empty for array built-ins), the body's key
    /// in the instance list (PRP-036).
    pub key: String,
}

/// Methods by type name, then method name.
pub(super) type Methods = HashMap<String, HashMap<String, Method>>;

impl Checker {
    /// Pass 2: every `impl` block's signatures, after types and functions (v1 §6).
    pub(super) fn collect_methods(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Impl(i) = item else { continue };
            // An `impl` belongs to its type's module, so it never names one.
            let owner = &self.key(&i.ty.name);
            let ty = self.named_type(owner).filter(|_| i.module.is_none());
            let Some(ty) = ty else {
                self.report(errors::unknown_type(&i.ty.name, i.ty.span));
                continue;
            };
            if let Some(decl) = self.type_decl(owner) {
                self.hover.define(i.ty.span, decl);
            }
            let declared = self.generics_of(owner);
            if i.params.len() != declared.len() {
                let d = errors::type_args(&i.ty.name, declared.len(), i.params.len(), i.ty.span);
                self.report(d);
                continue;
            }
            let outer = self.impl_generics(&declared, &i.params);
            for f in &i.methods {
                let name = &f.name.name;
                let own = self.generics_with(&f.type_params, &outer);
                self.type_params = [outer.clone(), own].concat();
                self.check_params(&f.type_params, &outer);
                let params: Vec<Type> = f.params.iter().map(|p| self.resolve(&p.ty)).collect();
                let ret = f.ret.as_ref().map_or(Type::Void, |t| self.resolve(t));
                if let Some(first) = self.methods.get(owner).and_then(|m| m.get(name)) {
                    let first = first.span;
                    self.report(errors::duplicate_method(name, owner, f.name.span, first));
                    continue;
                }
                if let Some(what) = self.member_named(&ty, name) {
                    self.report(errors::method_named_like(name, what, owner, f.name.span));
                    continue;
                }
                self.register_method(owner, name, f.visibility);
                let receiver = f.receiver.map(|r| r.mutable);
                let sig = super::Sig::of(
                    &self.type_params,
                    outer.len(),
                    receiver,
                    (&f.params, &params),
                    &ret,
                );
                self.hover.signatures.insert(f.name.span, sig);
                let method = Method {
                    receiver: f.receiver.map(|r| r.mutable),
                    params,
                    ret,
                    span: f.name.span,
                    generics: self.type_params.clone(),
                    outer: outer.len(),
                    key: format!("{owner}.{name}"),
                };
                self.methods
                    .entry(owner.clone())
                    .or_default()
                    .insert(name.clone(), method);
                self.hover.function(f.name.span, f.name.span, None);
            }
        }
        self.type_params.clear();
    }

    /// Checks every method body whose signature was accepted.
    pub(super) fn method_bodies(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Impl(i) = item else { continue };
            let owner = self.key(&i.ty.name);
            let Some(ty) = self.named_type(&owner) else {
                continue;
            };
            for f in &i.methods {
                let Some(m) = self.methods.get(&owner).and_then(|m| m.get(&f.name.name)) else {
                    continue;
                };
                if m.span != f.name.span {
                    continue; // a duplicate, already reported
                }
                let (params, ret) = (m.params.clone(), m.ret.clone());
                let receiver = with_args(&ty, params_of_impl(m));
                let generics = m.generics.clone();
                self.owner = m.key.clone();
                self.type_params = generics;
                self.body(f, params, ret, Some(receiver));
            }
        }
        self.default_bodies(program);
        self.type_params.clear();
        self.owner.clear();
    }

    /// The struct or enum type with key `name`, without type arguments.
    pub(super) fn named_type(&self, name: &str) -> Option<Type> {
        if self.structs.contains_key(name) {
            Some(Type::Struct(name.to_string(), Vec::new()))
        } else if self.enums.contains_key(name) {
            Some(Type::Enum(name.to_string(), Vec::new()))
        } else if self.handles.contains_key(name) {
            Some(Type::Handle(name.to_string()))
        } else {
            None
        }
    }

    /// "field" or "variant" if `ty` already has a member called `name` (E0310).
    fn member_named(&self, ty: &Type, name: &str) -> Option<&'static str> {
        match ty {
            Type::Struct(s, _) if self.struct_field(s, name).is_some() => Some("field"),
            Type::Enum(e, _) if self.enums[e].variants.iter().any(|(v, _)| v == name) => {
                Some("variant")
            }
            _ => None,
        }
    }

    pub(super) fn method(&self, ty: &Type, name: &str) -> Option<Method> {
        let owner = match ty {
            // A handle may have methods of its own, wrapping externs (v16).
            Type::Struct(n, _) | Type::Enum(n, _) | Type::Handle(n) => n,
            _ => return None,
        };
        self.methods.get(owner).and_then(|m| m.get(name)).cloned()
    }
}

/// The `impl`'s parameters as types: `Stack<T>`'s `[T]` for `self`.
fn params_of_impl(m: &Method) -> Vec<Type> {
    params(&m.generics[..m.outer])
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    const V: &str = "struct V { x: f64, y: f64 }\n";

    #[test]
    fn methods_and_associated_functions_type_check() {
        let src = format!(
            "fun main() {{ let v = V.new(1.0, 2.0); let n = v.len_sq(); }}\n{V}\
             impl V {{ fun new(x: f64, y: f64): V = V {{ x: x, y: y }}; }}\n\
             impl V {{ fun len_sq(self): f64 = self.x * self.x; }}"
        );
        assert_eq!(let_type(&src, "v"), Type::Struct("V".into(), Vec::new()));
        assert_eq!(let_type(&src, "n"), Type::F64);
        ok(
            "enum S { A, B }\nimpl S { fun flip(self): S = match self { A => S.B, B => S.A }; }\nfun main() { let s = S.A.flip(); }",
        );
    }

    #[test]
    fn declarations_are_checked() {
        assert_eq!(
            errors("impl Q { fun f() {} }\nfun main() {}"),
            [("E0305", "Q")]
        );
        assert_eq!(
            errors("impl Option { fun f() {} }\nfun main() {}"),
            [("E0305", "Option")]
        );
        let twice = format!(
            "{V}impl V {{ fun f(self) {{}} }}\nimpl V {{ fun f(self) {{}} }}\nfun main() {{}}"
        );
        assert_eq!(errors(&twice), [("E0310", "f")]);
        assert_eq!(
            errors(&format!(
                "{V}impl V {{ fun x(self) {{}} }}\nfun main() {{}}"
            )),
            [("E0310", "x")]
        );
        assert_eq!(
            errors("enum E { A }\nimpl E { fun A() {} }\nfun main() {}"),
            [("E0310", "A")]
        );
    }
}
