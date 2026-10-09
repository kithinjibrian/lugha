//! The types the milestone 3 checker knows (spec §4).

use std::fmt;

/// A type in the checker. Arrays and structs join in milestone 5.
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
        self == expected || matches!(self, Type::Never | Type::Error) || *expected == Type::Error
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
    pub fn contains_array(&self) -> bool {
        matches!(self, Type::Array(_))
    }
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
            Type::Void => "void",
            Type::Never => "never",
            Type::Error => "{error}",
        })
    }
}
