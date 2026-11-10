//! `match` arms, their patterns and guards.

use super::{Expr, Ident};
use crate::span::Span;

/// One `pattern => body` arm of a `match`.
#[derive(Debug, Clone, PartialEq)]
pub struct Arm {
    /// What the arm matches.
    pub pattern: Pattern,
    /// `if cond` before `=>`.
    pub guard: Option<Expr>,
    /// The arm's value.
    pub body: Expr,
    /// From the pattern to the end of the body.
    pub span: Span,
}

/// A pattern and where it was written.
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    /// Which pattern.
    pub kind: PatternKind,
    /// Its source location.
    pub span: Span,
}

/// The pattern forms; payload slots nest.
#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    /// `_`.
    Wildcard,
    /// `Name` or `Name(p, …)`; the checker resolves the name. In a payload
    /// slot, a bare `Name` may instead be a binding.
    Variant(Ident, Option<Vec<Pattern>>),
    /// An integer literal, `negative` for a leading `-`.
    Int { value: u64, negative: bool },
    /// A string literal.
    Str(String),
    /// `true` or `false`.
    Bool(bool),
    /// `name: T` or `_: T` (`None`): the interface value holds a `T`.
    Type(Option<Ident>, super::Type),
    /// `(p, q, …)`: a tuple's elements; two or more.
    Tuple(Vec<Pattern>),
    /// `p | q | …`: matches when any alternative does; two or more.
    Or(Vec<Pattern>),
    /// `mut name` or `mut name: T`: the binding is the arm's own mutable
    /// copy.
    Mut(Box<Pattern>),
}
