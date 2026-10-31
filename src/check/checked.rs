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
    /// Values stored from a kept parameter on its last use: moved only in
    /// the function's owning instance (v17, milestone 149).
    pub param_moves: HashSet<ExprId>,
    /// Call arguments and receivers read from a `let` local for the last
    /// time: handed to a kept parameter without a mark (milestone 149).
    pub hands: HashSet<ExprId>,
    /// Arguments read from a kept parameter for the last time: handed over
    /// only in the owning instance (milestone 149).
    pub param_hands: HashSet<ExprId>,
    /// Which parameters each function or method keeps, by key, receiver
    /// first for a method (milestone 149); only keys keeping one.
    pub keeps: HashMap<String, Vec<bool>>,
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
    /// Whether the program has a `spawn` (v17): only then does generated
    /// code count down to preemption.
    pub uses_tasks: bool,
    /// How each extern with callbacks passes its parameters to C, by key (v16, milestone 88).
    pub c_params: super::CParams,
    /// Variants used as function values (v17, milestone 94): the enum type they
    /// build, type arguments filled in, and the variant's name.
    pub variant_values: HashMap<ExprId, (Type, String)>,
    /// Every constant, by key with its type, in evaluation order: each after
    /// the constants it reads (v17, milestone 109).
    pub consts: Vec<(String, Type)>,
    /// Every name (or `m.NAME`) that reads a constant, with its key.
    pub const_uses: HashMap<ExprId, String>,
    /// Every `include_bytes`/`include_str` call, with the file's contents
    /// (v17, milestone 128).
    pub includes: HashMap<ExprId, super::Included>,
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
    /// Every `derive` block (v17, milestones 141–142): which block's items a
    /// derivable interface's call reaches at a concrete type.
    pub derives: super::Derives,
    /// Calls through a bound of a derivable interface, with its key (v17,
    /// milestone 141): codegen finds the item in the covering block.
    pub derived: HashMap<ExprId, String>,
    /// Every field intrinsic call, with its name (`fields`, …): codegen
    /// never decides from the syntax, since a program may have its own
    /// `derive` (v17, milestone 142).
    pub derive_calls: HashMap<ExprId, String>,
    /// Iterator loops (v6): the iterated expression's id, with what its
    /// `next` yields.
    pub iterations: HashMap<ExprId, Iteration>,
    /// Under `lughac test` (milestone 84), the main file's tests in
    /// declaration order; `None` in every other build.
    pub tests: Option<Vec<String>>,
}
