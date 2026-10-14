//! The types the milestone 3 checker knows (spec §4).

use std::collections::HashMap;
use std::fmt;

/// Every struct's fields in declaration order, by struct name.
pub type Structs = HashMap<String, Vec<(String, Type)>>;

/// Every enum's variants and their payload types in declaration order, by enum name.
pub type Enums = HashMap<String, Vec<(String, Vec<Type>)>>;

/// A type in the checker (spec §4).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    I64,
    U8,
    F64,
    /// v7: unsigned 32- and 64-bit integers, and a 32-bit float.
    U32,
    U64,
    F32,
    /// v10: `i8`, `i16`, `u16`.
    I8,
    I16,
    U16,
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
    /// A function value, `fun(A, B): R`; `R` is `Void` for no result (v3).
    Fun(Vec<Type>, Box<Type>),
    /// A value of an interface type, by the interface's key, with its type
    /// arguments (empty unless it is generic, v6): some type that satisfies
    /// it, chosen at run time (v5 draft §4).
    Interface(String, Vec<Type>),
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
    /// `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32` or `u64`.
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
        )
    }

    /// An integer type, `f32` or `f64`.
    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    /// `self` with each type parameter named in `map` replaced: how codegen
    /// makes a generic body's types concrete for one instance (PRP-036).
    pub fn substitute(&self, map: &HashMap<String, Type>) -> Type {
        let all = |args: &[Type]| args.iter().map(|a| a.substitute(map)).collect();
        match self {
            Type::Param(name) => map.get(name).cloned().unwrap_or_else(|| self.clone()),
            Type::Array(inner) => Type::Array(Box::new(inner.substitute(map))),
            Type::Option(inner) => Type::Option(Box::new(inner.substitute(map))),
            Type::Struct(key, args) => Type::Struct(key.clone(), all(args)),
            Type::Fun(params, ret) => Type::Fun(all(params), Box::new(ret.substitute(map))),
            Type::Enum(key, args) => Type::Enum(key.clone(), all(args)),
            Type::Interface(key, args) => Type::Interface(key.clone(), all(args)),
            other => other.clone(),
        }
    }

    /// True if a value of type `self` may appear where `expected` is wanted.
    pub(super) fn fits(&self, expected: &Type) -> bool {
        match (self, expected) {
            // An argument that already failed doesn't make the whole type wrong.
            (Type::Struct(k, a), Type::Struct(l, b))
            | (Type::Enum(k, a), Type::Enum(l, b))
            | (Type::Interface(k, a), Type::Interface(l, b)) => {
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
            Type::I8 => (i8::MAX as u64, 1 << 7),
            Type::I16 => (i16::MAX as u64, 1 << 15),
            Type::U8 => (u64::from(u8::MAX), 0),
            Type::U16 => (u64::from(u16::MAX), 0),
            Type::U32 => (u64::from(u32::MAX), 0),
            Type::U64 => (u64::MAX, 0),
            _ => (i64::MAX as u64, 1 << 63),
        }
    }

    /// True if an interface type appears anywhere in `self` (v5): in an
    /// element, a payload, a type argument or a function type.
    pub fn mentions_interface(&self) -> bool {
        match self {
            Type::Interface(..) => true,
            Type::Array(inner) | Type::Option(inner) => inner.mentions_interface(),
            Type::Struct(_, args) | Type::Enum(_, args) => {
                args.iter().any(Type::mentions_interface)
            }
            Type::Fun(params, ret) => {
                params.iter().any(Type::mentions_interface) || ret.mentions_interface()
            }
            _ => false,
        }
    }

    /// True for arrays, interface values (v5) and anything holding one inline:
    /// values that are deep-copied (spec §4).
    /// Terminates for valid programs: only an array can close a struct cycle (E0307).
    /// Enums and `Option` are false even with array payloads: payloads are
    /// immutable, so enum values share them and are never deep-copied (PRP-020).
    pub fn is_deep(&self, structs: &Structs) -> bool {
        match self {
            // An interface value's inside can change through `mut self` (v5).
            Type::Array(_) | Type::Interface(..) => true,
            // Tables are keyed by the type as it prints: `Stack<i64>` (PRP-036).
            Type::Struct(..) => structs
                .get(&self.to_string())
                .is_some_and(|fields| fields.iter().any(|(_, ty)| ty.is_deep(structs))),
            _ => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Type::I32 => "i32",
            Type::I64 => "i64",
            Type::U8 => "u8",
            Type::F64 => "f64",
            Type::U32 => "u32",
            Type::U64 => "u64",
            Type::F32 => "f32",
            Type::I8 => "i8",
            Type::I16 => "i16",
            Type::U16 => "u16",
            Type::Bool => "bool",
            Type::String => "string",
            // `(fun(i64): i64)[]`: without parentheses `[]` would be the return type's.
            Type::Array(element) if matches!(**element, Type::Fun(..)) => {
                return write!(f, "({element})[]");
            }
            Type::Array(element) => return write!(f, "{element}[]"),
            Type::Fun(params, ret) => {
                let params: Vec<String> = params.iter().map(ToString::to_string).collect();
                write!(f, "fun({})", params.join(", "))?;
                return match **ret {
                    Type::Void => Ok(()),
                    ref ret => write!(f, ": {ret}"),
                };
            }
            // The prelude's types print without their module: `Result<i64, string>` (v4).
            Type::Struct(name, args) | Type::Enum(name, args) | Type::Interface(name, args)
                if !args.is_empty() =>
            {
                let args: Vec<String> = args.iter().map(ToString::to_string).collect();
                let name = name.strip_prefix("$prelude.").unwrap_or(name);
                return write!(f, "{name}<{}>", args.join(", "));
            }
            Type::Struct(name, _) | Type::Enum(name, _) => {
                name.strip_prefix("$prelude.").unwrap_or(name)
            }
            Type::Param(name) | Type::Interface(name, _) => name,
            Type::Option(inner) => return write!(f, "Option<{inner}>"),
            Type::Void => "void",
            Type::Never => "never",
            Type::Error => "{error}",
        })
    }
}
