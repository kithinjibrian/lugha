//! Enum payload slots (v1 §4, §7; v2 draft §3): whether a bare name in a
//! payload pattern names a variant, and which slots are stored indirectly.

use super::{Enums, Structs, Type};

/// Whether a bare name in a pattern's payload slot names a variant (v2 draft
/// §3): it does when `variants`, the slot type's variants (`None` unless it
/// is an enum or `Option`), has one by that name; otherwise it binds the
/// payload. The checker and codegen both decide with this.
pub fn slot_names_variant(variants: Option<&[(String, Vec<Type>)]>, name: &str) -> bool {
    variants.is_some_and(|all| all.iter().any(|(v, _)| v == name))
}

/// True if the payload `slot` of the enum or `Option` type `owner` leads back
/// to `owner` through values held inline: struct fields, enum payloads and
/// `Option`'s payload, but not arrays, which are pointers. Such a slot is
/// stored on the heap so the type has a finite size (v1 §4, §7).
pub fn indirect_slot(owner: &Type, slot: &Type, structs: &Structs, enums: &Enums) -> bool {
    let mut stack = vec![slot.clone()];
    let mut seen: Vec<Type> = Vec::new();
    while let Some(ty) = stack.pop() {
        // By name: an instance may be spelled `Tree<i64>` either way (PRP-036).
        if ty.to_string() == owner.to_string() {
            return true;
        }
        if seen.contains(&ty) {
            continue;
        }
        match &ty {
            Type::Struct(..) => {
                stack.extend(
                    structs
                        .get(&ty.to_string())
                        .into_iter()
                        .flatten()
                        .map(|(_, t)| t.clone()),
                );
            }
            Type::Enum(..) => {
                let payloads = enums
                    .get(&ty.to_string())
                    .into_iter()
                    .flatten()
                    .flat_map(|(_, ps)| ps.iter().cloned());
                stack.extend(payloads);
            }
            Type::Option(inner) => stack.push((**inner).clone()),
            _ => {}
        }
        seen.push(ty);
    }
    false
}

#[cfg(test)]
mod tests {
    use super::indirect_slot;
    use crate::check::{Enums, Structs, Type};

    fn e(name: &str) -> Type {
        Type::Enum(name.into(), Vec::new())
    }

    #[test]
    fn only_slots_that_lead_back_are_indirect() {
        let enums: Enums = [
            (
                "List".to_string(),
                vec![
                    ("Nil".into(), vec![]),
                    ("Cons".into(), vec![Type::I64, e("List")]),
                ],
            ),
            (
                "Tree".to_string(),
                vec![
                    ("Leaf".into(), vec![]),
                    ("Node".into(), vec![e("Tree"), Type::I64, e("Tree")]),
                ],
            ),
            ("A".to_string(), vec![("B".into(), vec![e("B")])]),
            (
                "B".to_string(),
                vec![("A".into(), vec![e("A")]), ("End".into(), vec![])],
            ),
            (
                "Kids".to_string(),
                vec![("K".into(), vec![Type::Array(Box::new(e("Kids")))])],
            ),
            ("Plain".to_string(), vec![("P".into(), vec![Type::F64])]),
        ]
        .into();
        let node = Type::Struct("Node".into(), Vec::new());
        let opt = Type::Option(Box::new(node.clone()));
        let structs: Structs = [(
            "Node".to_string(),
            vec![("value".into(), Type::I64), ("next".into(), opt.clone())],
        )]
        .into();
        assert!(indirect_slot(&e("List"), &e("List"), &structs, &enums));
        assert!(!indirect_slot(&e("List"), &Type::I64, &structs, &enums));
        assert!(indirect_slot(&e("Tree"), &e("Tree"), &structs, &enums));
        assert!(
            indirect_slot(&opt, &node, &structs, &enums),
            "Node reaches Option<Node> through `next`"
        );
        assert!(indirect_slot(&e("A"), &e("B"), &structs, &enums));
        assert!(indirect_slot(&e("B"), &e("A"), &structs, &enums));
        assert!(
            !indirect_slot(
                &e("Kids"),
                &Type::Array(Box::new(e("Kids"))),
                &structs,
                &enums
            ),
            "arrays break cycles"
        );
        assert!(!indirect_slot(&e("Plain"), &Type::F64, &structs, &enums));
    }
}
