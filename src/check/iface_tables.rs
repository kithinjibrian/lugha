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
        let names: Vec<String> = self.interfaces[key]
            .items
            .iter()
            .map(|r| r.name.clone())
            .collect();
        for name in names {
            match ty {
                Type::Param(p) => self.bound_used(expr, p, &name),
                Type::Struct(owner, args) | Type::Enum(owner, args) => {
                    let method = self.methods.get(owner).and_then(|m| m.get(&name));
                    if let Some(method_key) = method.map(|m| m.key.clone()) {
                        self.used(expr, &method_key, args);
                    }
                }
                _ => {}
            }
        }
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
