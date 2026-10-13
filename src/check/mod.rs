//! Type checker — names, types and inference for all of v0 (spec §4–§6).
//!
//! Gives every expression a type, recorded in a table keyed by `ExprId` for
//! codegen (spec §9). Reports E03xx/E04xx and keeps going: an expression that
//! already failed gets `Type::Error`, which fits anything and is never
//! reported again.
//!
//! Depends on: ast, diagnostic, span.

mod access;
mod arguments;
mod array;
mod assign;
mod body;
mod bounds;
mod builtins;
mod call;
mod closures;
mod conflicts;
mod construct;
mod enums;
mod env;
mod errors;
mod expr;
mod flow;
mod fun_values;
mod generics;
mod helpers;
mod infer;
mod input;
mod instances;
mod interfaces;
mod layouts;
mod literal;
mod liveness;
mod matching;
mod method_calls;
mod methods;
mod modules;
mod ops;
mod patterns;
mod results;
mod satisfy;
mod stmt;
mod struct_literal;
mod structs;
mod swap;
mod types;
mod usefulness;
mod visibility;

use std::collections::{HashMap, HashSet};

pub use instances::Instance;
pub use modules::{Unit, check_modules, qualify, symbol};
pub use types::{Enums, Structs, Type, indirect_slot, slot_names_variant};

use crate::ast::{ExprId, Program};
use crate::diagnostic::Diagnostic;
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
}

/// Type-checks `program`, a program of one file, returning the type table
/// and any warnings. [`check_modules`] checks several.
///
/// # Errors
///
/// Every diagnostic found (errors and warnings) when there is at least one error.
pub fn check(program: &Program) -> Result<(Checked, Vec<Diagnostic>), Vec<Diagnostic>> {
    check_modules(&[Unit { path: &[], program }])
}

impl Checker {
    /// A checker with room for `count` expression types.
    fn new(count: usize) -> Self {
        Checker {
            types: vec![None; count],
            diagnostics: Vec::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
            methods: HashMap::new(),
            scopes: Vec::new(),
            ret: Type::Void,
            loops: 0,
            dead: false,
            iterating: Vec::new(),
            changes: Vec::new(),
            mentions: HashMap::new(),
            module: Vec::new(),
            imports: HashMap::new(),
            privacy: visibility::Privacy::default(),
            externs: HashMap::new(),
            callees: HashMap::new(),
            annotations: HashMap::new(),
            type_params: Vec::new(),
            instances: HashMap::new(),
            interfaces: HashMap::new(),
            closures: Vec::new(),
            captures: HashMap::new(),
            owner: String::new(),
            uses: Vec::new(),
            body_types: HashMap::new(),
        }
    }
}

/// A declared function.
pub(super) struct Signature {
    /// Type parameters, `fun max<T: Ord>` (v2 draft §4).
    pub generics: Vec<generics::Generic>,
    pub params: Vec<Type>,
    pub ret: Type,
    pub span: Span,
    /// An `extern fun`: never a value (v3).
    pub is_extern: bool,
}

/// A local variable or parameter.
#[derive(Clone)]
pub(super) struct Local {
    pub ty: Type,
    pub binding: Binding,
    /// Where the name was declared, for "declared here" labels.
    pub span: Span,
}

/// How a local was bound, which decides whether it may be assigned (spec §4).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Binding {
    Let {
        mutable: bool,
    },
    Param,
    LoopVar,
    /// A `match` payload binding: immutable and borrowed (v1 §4).
    Pattern,
    /// `mut self`: assignable, but the caller's data, so never moved from (v1 §4).
    SelfMut,
}

/// Checking state for one program.
pub(super) struct Checker {
    types: Vec<Option<Type>>,
    diagnostics: Vec<Diagnostic>,
    functions: HashMap<String, Signature>,
    structs: HashMap<String, structs::StructInfo>,
    enums: HashMap<String, enums::EnumInfo>,
    methods: methods::Methods,
    scopes: Vec<HashMap<String, Local>>,
    /// The return type of the function being checked.
    ret: Type,
    /// How many loops enclose the current statement, for `break`/`continue`.
    loops: u32,
    /// True while checking code after a divergence, so nested blocks don't
    /// repeat W0101.
    dead: bool,
    /// Places being iterated by enclosing `for … of` loops, with the loop's span (E0507).
    iterating: Vec<(array::Path, Span)>,
    /// Places changed so far in the current body, for E0510.
    changes: Vec<conflicts::Change>,
    /// The local every `Name` expression resolved to, for move on last use.
    mentions: HashMap<ExprId, liveness::Mention>,
    /// The module whose names are being resolved (PRP-034).
    module: Vec<String>,
    /// The current module's imports: last segment → module path.
    imports: HashMap<String, Vec<String>>,
    /// Who declares what, and what is `private`.
    privacy: visibility::Privacy,
    /// Every extern's C name: its signature and first declaration (E0306).
    externs: HashMap<String, (Vec<Type>, Type, Span)>,
    /// See `Checked::callees`.
    callees: HashMap<ExprId, String>,
    /// See `Checked::annotations`.
    annotations: HashMap<Span, Type>,
    /// The type parameters in scope where types are being resolved or bodies checked.
    type_params: Vec<generics::Generic>,
    /// See `Checked::instances`.
    instances: HashMap<ExprId, Vec<Type>>,
    /// Every interface, by key (milestone 12).
    interfaces: HashMap<String, interfaces::InterfaceInfo>,
    /// The closures whose bodies are being checked, innermost last (v3).
    closures: Vec<closures::Frame>,
    /// See `Checked::captures`.
    captures: HashMap<ExprId, Vec<String>>,
    /// The key of the function or method whose body is being checked.
    owner: String,
    /// Every generic call, with the body it is in (PRP-036).
    uses: Vec<instances::Use>,
    /// The types in each generic body that mention its parameters, by the
    /// body's key: each instance's types are these, substituted (PRP-036).
    body_types: HashMap<String, Vec<Type>>,
}

impl Checker {
    fn report(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    fn record(&mut self, expr: &crate::ast::Expr, ty: Type) {
        self.body_type(&ty);
        self.types[expr.id.0 as usize] = Some(ty);
    }
}

#[cfg(test)]
pub(crate) mod test_util;
