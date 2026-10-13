//! What codegen needs to build interface values (v5 draft §9, PRP-047):
//! each interface's methods in order, and the methods a conversion makes
//! reachable.

use std::collections::HashMap;

use super::{Checker, Type};
use crate::ast::Expr;

/// A method an interface declares, as codegen needs it for a dispatch
/// table slot and a dynamic call (v5).
#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceMethod {
    pub name: String,
    /// `mut self` rather than `self`.
    pub mutable: bool,
    pub params: Vec<Type>,
    pub ret: Type,
}

impl Checker {
    /// Records the methods a conversion of `ty` to interface `key` puts in
    /// its dispatch table, so generic ones are compiled even if nothing
    /// calls them directly: a generic type's methods at its arguments, a
    /// type parameter's through its bound in each instance (PRP-047).
    pub(super) fn conversion_uses(&mut self, expr: &Expr, ty: &Type, key: &str) {
        let items = self.interfaces[key].items.clone();
        for item in items {
            let name = &item.name;
            let own = match ty {
                Type::Struct(owner, _) | Type::Enum(owner, _) => {
                    self.methods.get(owner).and_then(|m| m.get(name))
                }
                _ => None,
            };
            match (ty, own.map(|m| m.key.clone())) {
                (Type::Param(p), _) => self.bound_used(expr, p, name),
                (Type::Struct(_, args) | Type::Enum(_, args), Some(method_key)) => {
                    self.used(expr, &method_key, args);
                }
                // The type lacks it: its slot is the default's instance (v5).
                (_, None) if item.default.is_some() => {
                    self.used(expr, &format!("{key}.{name}"), std::slice::from_ref(ty));
                }
                _ => {}
            }
        }
    }

    /// `?` on a `Result<_, from>` in a function returning `Result<_, to>`:
    /// true, recording the conversion, when `to` is an interface that
    /// `from`, a concrete type, satisfies (v5).
    pub(super) fn try_converts(&mut self, inner: &Expr, from: &Type, to: &Type) -> bool {
        let Type::Interface(key) = to else {
            return false;
        };
        if matches!(from, Type::Interface(_)) || !self.unmet(from, key).is_empty() {
            return false;
        }
        self.try_conversions.insert(inner.id, to.clone());
        self.conversion_uses(inner, from, key);
        true
    }

    /// Every interface's methods, for `Checked.interfaces`. Interfaces with
    /// an associated function are never values, so they have no tables.
    pub(super) fn interface_methods(&self) -> HashMap<String, Vec<InterfaceMethod>> {
        let methods = |items: &[super::interfaces::Requirement]| {
            items
                .iter()
                .filter_map(|r| {
                    Some(InterfaceMethod {
                        name: r.name.clone(),
                        mutable: r.receiver?,
                        params: r.params.clone(),
                        ret: r.ret.clone(),
                    })
                })
                .collect()
        };
        self.interfaces
            .iter()
            .filter(|(_, info)| info.not_value.is_none())
            .map(|(key, info)| (key.clone(), methods(&info.items)))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{SHAPES, ok};

    #[test]
    fn converted_generic_types_compile_their_methods() {
        let checked = ok("interface Sized { fun size(self): i64; }\n\
            struct Stack<T> { items: T[] }\n\
            impl Stack<T> { fun size(self): i64 = self.items.len; }\n\
            fun conv<T: Sized>(x: T): Sized = x;\n\
            fun main() { let s: Sized = Stack { items: [1] }; let t = conv(Stack { items: [\"a\"] }); }");
        let monos: Vec<String> = checked
            .monos
            .iter()
            .map(|i| format!("{}{:?}", i.key, i.args))
            .collect();
        assert!(monos.contains(&"Stack.size[I64]".to_string()), "{monos:?}");
        assert!(
            monos.contains(&"Stack.size[String]".to_string()),
            "{monos:?}"
        );
    }

    #[test]
    fn codegen_gets_each_interfaces_methods_in_order() {
        let checked = ok(&format!("{SHAPES}fun main() {{}}"));
        let shape = &checked.interfaces["Shape"];
        let names: Vec<&str> = shape.iter().map(|m| m.name.as_str()).collect();
        assert_eq!(names, ["area", "grow"]);
        assert!(!shape[0].mutable && shape[1].mutable);
        assert_eq!(
            (&shape[1].params, &shape[1].ret),
            (&vec![Type::F64], &Type::Void)
        );
    }
}
