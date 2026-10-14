//! Written types (spec §3): the syntax of a type, resolved later by the checker.

use super::Ident;
use crate::span::Span;

/// A written type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    /// Which type.
    pub kind: TypeKind,
    /// Where it was written.
    pub span: Span,
}

/// The types of spec §4, as written; struct names are unresolved.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    I32,
    I64,
    U8,
    F64,
    Bool,
    String,
    /// `u32`, `u64`, `f32` (v7).
    U32,
    U64,
    F32,
    /// `i8`, `i16`, `u16` (v10).
    I8,
    I16,
    U16,
    /// `char` (v14).
    Char,
    /// A struct name, resolved by the checker.
    Named(String),
    /// `T[]`.
    Array(Box<Type>),
    /// `Option<T>` (v1).
    Option(Box<Type>),
    /// `m.T`, `T<args>` or `m.T<args>` (v2 draft §3). Boxed to keep `Type`,
    /// and so `Expr` (through casts), small for the parser's recursion.
    Applied(Box<AppliedType>),
    /// `fun(A, B): R`; no return type means `void` (v3 draft §3).
    Fun(Vec<Type>, Option<Box<Type>>),
}

/// A module-qualified type name, type arguments, or both (v2 draft §3).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AppliedType {
    /// `m` in `m.T`.
    pub module: Option<Ident>,
    /// The type's name.
    pub name: Ident,
    /// `<args>` in order; empty for a plain `m.T`.
    pub args: Vec<Type>,
}
