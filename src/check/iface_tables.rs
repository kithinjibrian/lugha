//! What codegen needs to build interface values (v5 draft §9, PRP-047):
//! each interface's methods in order, and the methods a conversion makes
//! reachable.

use std::collections::HashMap;

use super::applied::Applied;
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

/// An interface as codegen needs it (v6): its type parameters' names and
/// its methods in declaration order, in those parameters.
#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceShape {
    pub params: Vec<String>,
    pub methods: Vec<InterfaceMethod>,
}

impl InterfaceShape {
    /// The methods at `args`, the type arguments of an interface type.
    pub fn methods_at(&self, args: &[Type]) -> Vec<InterfaceMethod> {
        let map: HashMap<String, Type> = self.params.iter().cloned().zip(args.to_vec()).collect();
        self.methods
            .iter()
            .map(|m| InterfaceMethod {
                params: m.params.iter().map(|p| p.substitute(&map)).collect(),
                ret: m.ret.substitute(&map),
                ..m.clone()
            })
            .collect()
    }
}

impl Checker {
    /// Records the methods a conversion of `ty` to interface `key` puts in
    /// its dispatch table, so generic ones are compiled even if nothing
    /// calls them directly: a generic type's methods at its arguments, a
    /// type parameter's through its bound in each instance (PRP-047).
    pub(super) fn conversion_uses(&mut self, expr: &Expr, ty: &Type, interface: &Applied) {
        let items = self.items(interface);
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
                // A generic interface's default also takes its arguments (v6).
                (_, None) if item.default.is_some() => {
                    let args = [std::slice::from_ref(ty), &interface.args].concat();
                    self.used(expr, &format!("{}.{name}", interface.key), &args);
                }
                _ => {}
            }
        }
    }

    /// `?` on a `Result<_, from>` in a function returning `Result<_, to>`:
    /// true, recording the conversion, when `to` is an interface that
    /// `from`, a concrete type, satisfies (v5).
    pub(super) fn try_converts(&mut self, inner: &Expr, from: &Type, to: &Type) -> bool {
        let Some(interface) = Applied::of(to) else {
            return false;
        };
        // An interface error converts when it declares every item (v6).
        let converts = match Applied::of(from) {
            Some(j) => j.key != interface.key && self.undeclared(&j, &interface).is_empty(),
            None => self.unmet(from, &interface).is_empty(),
        };
        if !converts {
            return false;
        }
        self.try_conversions.insert(inner.id, to.clone());
        if !matches!(from, Type::Interface(..)) {
            self.conversion_uses(inner, from, &interface);
        }
        true
    }

    /// Every interface's methods, for `Checked.interfaces`. Interfaces with
    /// an associated function are never values, so they have no tables.
    pub(super) fn interface_methods(&self) -> HashMap<String, InterfaceShape> {
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
            .filter(|(_, info)| info.not_value.is_none() && info.uses_self.is_none())
            .map(|(key, info)| {
                let shape = InterfaceShape {
                    params: info.generics.iter().map(|g| g.name.clone()).collect(),
                    methods: methods(&info.items),
                };
                (key.clone(), shape)
            })
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
        let shape = &checked.interfaces["Shape"].methods;
        let names: Vec<&str> = shape.iter().map(|m| m.name.as_str()).collect();
        assert_eq!(names, ["area", "grow"]);
        assert!(!shape[0].mutable && shape[1].mutable);
        assert_eq!(
            (&shape[1].params, &shape[1].ret),
            (&vec![Type::F64], &Type::Void)
        );
    }
}
