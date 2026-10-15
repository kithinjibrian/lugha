//! Type checker — names, types and inference for all of v0 (spec §4–§6).
//!
//! Gives every expression a type, recorded in a table keyed by `ExprId` for
//! codegen (spec §9). Reports E03xx/E04xx and keeps going: an expression that
//! already failed gets `Type::Error`, which fits anything and is never
//! reported again.
//!
//! Depends on: ast, diagnostic, span.

mod access;
mod applied;
mod arguments;
mod array;
mod assign;
mod body;
mod bound_args;
mod bound_infer;
mod bounds;
mod builtins;
mod call;
mod checked;
mod closures;
mod concrete_defaults;
mod conditional;
mod conflicts;
mod construct;
mod defaults;
mod enums;
mod env;
mod errors;
mod expr;
mod float_ops;
mod flow;
mod for_mut;
mod fun_values;
mod generics;
mod helpers;
mod iface_calls;
mod iface_tables;
mod iface_values;
mod impl_bounds;
mod infer;
mod input;
mod instances;
mod interfaces;
mod iter_helpers;
mod iterators;
mod layouts;
mod levels;
mod literal;
mod liveness;
mod matching;
mod members;
mod method_calls;
mod methods;
mod modules;
mod named_imports;
mod numbers;
mod operators;
mod ops;
mod or_patterns;
mod ordering;
mod partial;
mod patterns;
mod printing;
mod reexports;
mod refs;
mod results;
mod satisfy;
mod self_type;
mod slots;
mod stmt;
mod struct_literal;
mod structs;
mod swap;
mod templates;
mod tuples;
mod type_patterns;
mod types;
mod upcast;
mod usefulness;
mod visibility;

use std::collections::HashMap;

pub use checked::Checked;
pub use defaults::SELF;
pub use float_ops::{float_op, os_op, string_op};
pub use iface_tables::{InterfaceMethod, InterfaceShape};
pub use input::prelude_helper;
pub use instances::Instance;
pub use iterators::Iteration;
pub use members::{Detail, Member, MemberKind, TypeMembers, type_members, value_members};
pub use modules::{Unit, check_modules, qualify, symbol};
pub use ordering::ORDERING;
pub use partial::{Name, Partial, Target, check_partial};
pub use refs::REF_NEW;
pub use slots::{indirect_slot, slot_names_variant};
pub use templates::exact as template_exact;
pub use tuples::elements as tuple_elements;
pub use types::{Enums, Structs, Type};

use crate::ast::{ExprId, Program};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// Every intrinsic's name (`println`, `read_line`, …): names a program may
/// call that no source declares, for the language server's completion.
pub fn intrinsics() -> impl Iterator<Item = &'static str> {
    env::INTRINSICS.into_iter().chain(input::INPUT)
}

/// Type-checks `program`, a program of one file, returning the type table
/// and any warnings. [`check_modules`] checks several.
///
/// # Errors
///
/// Every diagnostic found (errors and warnings) when there is at least one error.
pub fn check(program: &Program) -> Result<(Checked, Vec<Diagnostic>), Vec<Diagnostic>> {
    check_modules(&[Unit {
        path: &[],
        program,
        imports: &[],
        package: None,
    }])
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
            package: None,
            imports: HashMap::new(),
            privacy: visibility::Privacy::default(),
            aliases: named_imports::Aliases::new(),
            reexports: HashMap::new(),
            externs: HashMap::new(),
            callees: HashMap::new(),
            annotations: HashMap::new(),
            type_params: Vec::new(),
            instances: HashMap::new(),
            interfaces: HashMap::new(),
            closures: Vec::new(),
            captures: HashMap::new(),
            conversions: HashMap::new(),
            try_conversions: HashMap::new(),
            default_args: HashMap::new(),
            iterations: HashMap::new(),
            owner: String::new(),
            param_literals: Vec::new(),
            uses: Vec::new(),
            body_types: HashMap::new(),
            hover: partial::Recorder::new(count),
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
    /// Its package; `None` for the root package (v8).
    package: Option<String>,
    /// The current module's imports: last segment → module path.
    imports: HashMap<String, Vec<String>>,
    /// Who declares what, and what is `private`.
    privacy: visibility::Privacy,
    /// Named imports and re-exports, by their key in their module (v11).
    aliases: named_imports::Aliases,
    /// Whole-module re-exports (v14): `m.v` (as a key of `m`) → `v`'s path.
    reexports: HashMap<String, Vec<String>>,
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
    /// See `Checked::conversions`.
    conversions: HashMap<ExprId, Type>,
    /// See `Checked::try_conversions`.
    try_conversions: HashMap<ExprId, Type>,
    /// See `Checked::default_args`.
    default_args: HashMap<ExprId, Vec<Type>>,
    /// See `Checked::iterations`.
    iterations: HashMap<ExprId, Iteration>,
    /// The key of the function or method whose body is being checked.
    owner: String,
    /// Integer literals typed as type parameters (v15, milestone 58).
    param_literals: Vec<literal::ParamLiteral>,
    /// Every generic call, with the body it is in (PRP-036).
    uses: Vec<instances::Use>,
    /// The types in each generic body that mention its parameters, by the
    /// body's key: each instance's types are these, substituted (PRP-036).
    body_types: HashMap<String, Vec<Type>>,
    /// Names and expression spans for the language server's hover (v16).
    hover: partial::Recorder,
}

impl Checker {
    fn report(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    fn record(&mut self, expr: &crate::ast::Expr, ty: Type) {
        self.body_type(&ty);
        self.types[expr.id.0 as usize] = Some(ty);
        self.hover.expr(expr);
    }
}

#[cfg(test)]
pub(crate) mod test_util;
