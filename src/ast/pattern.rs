//! `match` arms and their flat patterns (v1 draft §3).

use super::{Expr, Ident};
use crate::span::Span;

/// One `pattern => body` arm of a `match`.
#[derive(Debug, Clone, PartialEq)]
pub struct Arm {
    /// What the arm matches.
    pub pattern: Pattern,
    /// The arm's value.
    pub body: Expr,
    /// From the pattern to the end of the body.
    pub span: Span,
}

/// A flat pattern and where it was written.
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    /// Which pattern.
    pub kind: PatternKind,
    /// Its source location.
    pub span: Span,
}

/// The pattern forms: no nesting and no guards (v1 draft §3).
#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    /// `_`.
    Wildcard,
    /// `Name` or `Name(a, _, …)`; the variant is resolved by the checker.
    Variant(Ident, Option<Vec<PatSlot>>),
    /// An integer literal, `negative` for a leading `-`.
    Int { value: u64, negative: bool },
    /// A string literal.
    Str(String),
    /// `true` or `false`.
    Bool(bool),
}

/// One payload position in a variant pattern.
#[derive(Debug, Clone, PartialEq)]
pub enum PatSlot {
    /// A name bound to the payload.
    Bind(Ident),
    /// `_`: the payload is ignored.
    Ignore(Span),
}
