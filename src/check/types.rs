//! The types the milestone 3 checker knows (spec §4).

use std::collections::HashMap;
use std::fmt;

/// Every struct's fields in declaration order, by struct name.
pub type Structs = HashMap<String, Vec<(String, Type)>>;

/// Every enum's variants and their payload types in declaration order, by enum name.
pub type Enums = HashMap<String, Vec<(String, Vec<Type>)>>;

/// A type in the checker (spec §4).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    I32,
    I64,
    U8,
    F64,
    Bool,
    /// Immutable UTF-8 text (spec §4).
    String,
    /// `T[]`: a fixed-length array on the GC heap (spec §4, §7).
    Array(Box<Type>),
    /// A user-declared struct, by key, with its type arguments (empty
    /// unless it is generic; v2 draft §4, PRP-035).
    Struct(String, Vec<Type>),
    /// A user-declared enum, by key, with its type arguments (v1 §4).
    Enum(String, Vec<Type>),
    /// A type parameter inside generic code, by name: abstract, allowing
    /// only what its bounds provide (v2 draft §4).
    Param(String),
    /// The built-in `Option<T>`: `Some(T)` or `None` (v1 §4).
    Option(Box<Type>),
    /// No value: statements, blocks without a tail, `if` without `else`,
    /// functions without a return type.
    Void,
    /// Control never gets here (`return`, `break`, `continue`, or every
    /// branch of an `if` doing so); fits any expected type (spec §6).
    Never,
    /// An expression that already produced a diagnostic. Fits anything and
    /// is never reported, so one mistake gives one error.
    Error,
}

impl Type {
    /// `i32`, `i64` or `u8`.
    pub fn is_integer(&self) -> bool {
        matches!(self, Type::I32 | Type::I64 | Type::U8)
    }

    /// An integer type or `f64`.
    pub fn is_numeric(&self) -> bool {
        self.is_integer() || *self == Type::F64
    }

    /// True if a value of type `self` may appear where `expected` is wanted.
    pub(super) fn fits(&self, expected: &Type) -> bool {
        match (self, expected) {
            // An argument that already failed doesn't make the whole type wrong.
            (Type::Struct(k, a), Type::Struct(l, b)) | (Type::Enum(k, a), Type::Enum(l, b)) => {
                k == l && a.len() == b.len() && a.iter().zip(b).all(|(x, y)| x.fits(y))
            }
            _ => {
                self == expected
                    || matches!(self, Type::Never | Type::Error)
                    || *expected == Type::Error
            }
        }
    }

    /// The literal range of an integer type: (largest positive, largest negated magnitude).
    pub(super) fn literal_range(&self) -> (u64, u64) {
        match self {
            Type::I32 => (i32::MAX as u64, 1 << 31),
            Type::U8 => (u64::from(u8::MAX), 0),
            _ => (i64::MAX as u64, 1 << 63),
        }
    }

    /// True for arrays and anything holding one: values that are deep-copied (spec §4).
    /// Terminates for valid programs: only an array can close a struct cycle (E0307).
    /// Enums and `Option` are false even with array payloads: payloads are
    /// immutable, so enum values share them and are never deep-copied (PRP-020).
    pub fn contains_array(&self, structs: &Structs) -> bool {
        match self {
            Type::Array(_) => true,
            Type::Struct(name, _) => structs
                .get(name)
                .is_some_and(|fields| fields.iter().any(|(_, ty)| ty.contains_array(structs))),
            _ => false,
        }
    }
}

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
        if ty == *owner {
            return true;
        }
        if seen.contains(&ty) {
            continue;
        }
        match &ty {
            Type::Struct(name, _) => {
                stack.extend(
                    structs
                        .get(name)
                        .into_iter()
                        .flatten()
                        .map(|(_, t)| t.clone()),
                );
            }
            Type::Enum(name, _) => {
                let payloads = enums
                    .get(name)
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

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Type::I32 => "i32",
            Type::I64 => "i64",
            Type::U8 => "u8",
            Type::F64 => "f64",
            Type::Bool => "bool",
            Type::String => "string",
            Type::Array(element) => return write!(f, "{element}[]"),
            Type::Struct(name, args) | Type::Enum(name, args) if !args.is_empty() => {
                let args: Vec<String> = args.iter().map(ToString::to_string).collect();
                return write!(f, "{name}<{}>", args.join(", "));
            }
            Type::Struct(name, _) | Type::Enum(name, _) => name,
            Type::Param(name) => name,
            Type::Option(inner) => return write!(f, "Option<{inner}>"),
            Type::Void => "void",
            Type::Never => "never",
            Type::Error => "{error}",
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{Enums, Structs, Type, indirect_slot};

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
