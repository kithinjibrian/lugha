//! The checker's output (moved verbatim from `check/mod.rs`, session 96):
//! what codegen reads instead of resolving or inferring anything itself.

use std::collections::{HashMap, HashSet};

use super::{Enums, Instance, InterfaceShape, Iteration, Structs, Type};
use crate::ast::ExprId;
use crate::span::Span;

/// The checker's output.
#[derive(Debug, Clone, PartialEq)]
pub struct Checked {
    /// The type of every expression, indexed by `ExprId`.
    pub types: Vec<Type>,
    /// Every struct's fields in declaration order.
    pub structs: Structs,
    /// Every enum's variants and payload types in declaration order (v1).
    pub enums: Enums,
    /// Values read from a place for the last time: stored without a copy (PRP-018).
    pub moves: HashSet<ExprId>,
    /// The key of the function or extern each call calls (PRP-034), so
    /// codegen never resolves names itself.
    pub callees: HashMap<ExprId, String>,
    /// The resolved type of every written type, by its span (unique in the
    /// program's one offset space).
    pub annotations: HashMap<Span, Type>,
    /// The type arguments of every use of something generic — a call,
    /// associated call, struct literal or construction — in declaration
    /// order (PRP-035), for monomorphization.
    pub instances: HashMap<ExprId, Vec<Type>>,
    /// Every generic function and method instance the compiled code
    /// reaches, in discovery order (PRP-036).
    pub monos: Vec<Instance>,
    /// Every concrete type the compiled code uses, instances' bodies
    /// included, so codegen can declare each layout once (PRP-036).
    pub used: Vec<Type>,
    /// The names each closure literal captures, in order of first mention (v3).
    pub captures: HashMap<ExprId, Vec<String>>,
    /// Expressions converted to an interface value, with the interface type
    /// (v5); the expression's own type stays in `types`.
    pub conversions: HashMap<ExprId, Type>,
    /// Every interface's methods in declaration order, by key (v5): the
    /// slots of its dispatch tables.
    pub interfaces: HashMap<String, InterfaceShape>,
    /// The operands of `?` whose error converts to an interface error, with
    /// that interface type (v5).
    pub try_conversions: HashMap<ExprId, Type>,
    /// Calls through a bound whose callee may be a generic interface's
    /// default (v6): the interface's arguments, in the caller's parameters.
    pub default_args: HashMap<ExprId, Vec<Type>>,
    /// Iterator loops (v6): the iterated expression's id, with what its
    /// `next` yields.
    pub iterations: HashMap<ExprId, Iteration>,
}
