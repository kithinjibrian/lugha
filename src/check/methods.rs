//! Methods (v1 §4, §6): `impl` blocks, their signatures (E0310) and bodies.
//! Calls are in `method_calls.rs`.

use std::collections::HashMap;

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
}

/// Methods by type name, then method name.
pub(super) type Methods = HashMap<String, HashMap<String, Method>>;

impl Checker {
    /// Pass 2: every `impl` block's signatures, after types and functions (v1 §6).
    pub(super) fn collect_methods(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Impl(i) = item else { continue };
            let owner = &i.ty.name;
            let Some(ty) = self.named_type(owner) else {
                self.report(errors::unknown_type(owner, i.ty.span));
                continue;
            };
            for f in &i.methods {
                let name = &f.name.name;
                let params = f.params.iter().map(|p| self.resolve(&p.ty)).collect();
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
                let method = Method {
                    receiver: f.receiver.map(|r| r.mutable),
                    params,
                    ret,
                    span: f.name.span,
                };
                self.methods
                    .entry(owner.clone())
                    .or_default()
                    .insert(name.clone(), method);
            }
        }
    }

    /// Checks every method body whose signature was accepted.
    pub(super) fn method_bodies(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Impl(i) = item else { continue };
            let Some(ty) = self.named_type(&i.ty.name) else {
                continue;
            };
            for f in &i.methods {
                let Some(m) = self
                    .methods
                    .get(&i.ty.name)
                    .and_then(|m| m.get(&f.name.name))
                else {
                    continue;
                };
                if m.span != f.name.span {
                    continue; // a duplicate, already reported
                }
                let (params, ret) = (m.params.clone(), m.ret.clone());
                self.body(f, params, ret, Some(ty.clone()));
            }
        }
    }

    /// The struct or enum type called `name`.
    pub(super) fn named_type(&self, name: &str) -> Option<Type> {
        if self.structs.contains_key(name) {
            Some(Type::Struct(name.to_string()))
        } else if self.enums.contains_key(name) {
            Some(Type::Enum(name.to_string()))
        } else {
            None
        }
    }

    /// "field" or "variant" if `ty` already has a member called `name` (E0310).
    fn member_named(&self, ty: &Type, name: &str) -> Option<&'static str> {
        match ty {
            Type::Struct(s) if self.struct_field(s, name).is_some() => Some("field"),
            Type::Enum(e) if self.enums[e].variants.iter().any(|(v, _)| v == name) => {
                Some("variant")
            }
            _ => None,
        }
    }

    pub(super) fn method(&self, ty: &Type, name: &str) -> Option<Method> {
        let owner = match ty {
            Type::Struct(n) | Type::Enum(n) => n,
            _ => return None,
        };
        self.methods.get(owner).and_then(|m| m.get(name)).cloned()
    }
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
        assert_eq!(let_type(&src, "v"), Type::Struct("V".into()));
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
