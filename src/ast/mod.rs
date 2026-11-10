//! Abstract syntax tree produced by the parser.
//!
//! A plain owned tree. Every expression carries a span and an `ExprId`; ids
//! are dense (`0..Program::expr_count`) so the checker can keep its
//! expression → type table in a `Vec`. The tree records syntax only: names
//! are unresolved and nothing is type-checked.

mod expr;
mod header;
mod pattern;
mod types;

pub use expr::{
    BinOp, Closure, Expr, ExprId, ExprKind, FormatCall, FormatText, HashKind, HashLiteral, IfLet,
    Part, SelectArm, SelectKind, StructLiteral, UnOp, spawned_call,
};
pub use header::{
    Bound, Field, Import, ImportItem, InterfaceDecl, InterfaceItem, TypeParam, Visibility,
    VisibilityKind,
};
pub use pattern::{Arm, Pattern, PatternKind};
pub use types::{AppliedType, Type, TypeKind};

use crate::span::Span;

/// A whole `.la` file.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    /// `import`s, which come before every item.
    pub imports: Vec<Import>,
    /// Top-level items in source order.
    pub items: Vec<Item>,
    /// Number of `Expr` nodes; every `ExprId` is below this.
    pub expr_count: u32,
}

/// A name and where it was written.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    /// The identifier text.
    pub name: String,
    /// Its source location.
    pub span: Span,
}

/// A top-level declaration.
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    /// `fun name(params): ret { body }` or `fun name(params): ret = expr;`.
    Fun(FunDecl),
    /// `extern fun name(params): ret;`.
    Extern(ExternDecl),
    /// `extern type Name;`: an opaque C pointer type.
    ExternType(ExternTypeDecl),
    /// `struct Name { fields }`.
    Struct(StructDecl),
    /// `enum Name { variants }`.
    Enum(EnumDecl),
    /// `impl Type { methods }`.
    Impl(ImplDecl),
    /// `interface Name { signatures }`.
    Interface(InterfaceDecl),
    /// `const NAME: T = value;`.
    Const(ConstDecl),
    /// `$derive I for X { methods }`: how interface `I` is met by a type it
    /// doesn't own, in bundled sources only.
    Derive(DeriveDecl),
}

/// What a `$derive` block covers.
#[derive(Debug, Clone, PartialEq)]
pub enum DeriveTarget {
    /// One type, possibly generic over the block's parameters: `i64`, `T[]`.
    Type(Type),
    /// Every struct whose fields meet the interface.
    Struct,
    /// Every enum whose payloads meet the interface.
    Enum,
    /// Every tuple whose elements meet the interface.
    Tuple,
    /// Every `extern type` handle.
    Handle,
}

/// `$derive<T: I> I for T[] { … }`: the interface's items for the types
/// `target` covers, written in the interface's own module.
#[derive(Debug, Clone, PartialEq)]
pub struct DeriveDecl {
    /// The block's type parameters, with their bounds.
    pub type_params: Vec<TypeParam>,
    /// The interface, declared in the same module.
    pub interface: Ident,
    /// The types it covers.
    pub target: DeriveTarget,
    /// The interface's items for those types.
    pub methods: Vec<FunDecl>,
    /// From `$derive` to the closing brace.
    pub span: Span,
}

/// A global constant: evaluated once before `main`.
#[derive(Debug, Clone, PartialEq)]
pub struct ConstDecl {
    /// `private` or `public`, if written.
    pub visibility: Option<Visibility>,
    /// The constant's name.
    pub name: Ident,
    /// Its written type.
    pub ty: Type,
    /// Its value, a constant expression.
    pub value: Expr,
    /// From `const` to the `;`.
    pub span: Span,
}

/// A function with a body.
#[derive(Debug, Clone, PartialEq)]
pub struct FunDecl {
    /// `private` or `public`, if written.
    pub visibility: Option<Visibility>,
    /// Function name.
    pub name: Ident,
    /// `<T, U: Eq>` after the name.
    pub type_params: Vec<TypeParam>,
    /// `self` or `mut self`; only methods in an `impl` have one.
    pub receiver: Option<Receiver>,
    /// Parameters in order, after any receiver.
    pub params: Vec<Param>,
    /// Declared return type; `None` means `void`.
    pub ret: Option<Type>,
    /// The body. An expression body `= e;` is stored as a block with tail `e`.
    pub body: Block,
    /// From `fun` to the end of the body.
    pub span: Span,
    /// The `@test` before it, which makes it a test.
    pub test: Option<Span>,
}

/// `extern type Name;`: a handle, an opaque C pointer.
#[derive(Debug, Clone, PartialEq)]
pub struct ExternTypeDecl {
    /// `private` or `public`, if written.
    pub visibility: Option<Visibility>,
    /// The type's name.
    pub name: Ident,
    /// From `extern` to the `;`.
    pub span: Span,
}

/// A C function declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct ExternDecl {
    /// `private` or `public`, if written.
    pub visibility: Option<Visibility>,
    /// The name Lugha calls it by: the C symbol too, unless `symbol` is given.
    pub name: Ident,
    /// `extern "symbol" fun name(…)`: the C function's own name, the
    /// string's text and span.
    pub symbol: Option<Ident>,
    /// Parameters in order.
    pub params: Vec<Param>,
    /// Declared return type; `None` means `void`.
    pub ret: Option<Type>,
    /// From `extern` to the `;`.
    pub span: Span,
}

impl ExternDecl {
    /// The C symbol this declaration calls: `symbol` when renamed, else its name.
    pub fn c_symbol(&self) -> &str {
        self.symbol.as_ref().unwrap_or(&self.name).name.as_str()
    }
}

/// A struct declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    /// `private` or `public`, if written.
    pub visibility: Option<Visibility>,
    /// Struct name.
    pub name: Ident,
    /// `<T, …>` after the name.
    pub type_params: Vec<TypeParam>,
    /// Fields in declaration order.
    pub fields: Vec<Field>,
    /// From `struct` (or `extern`) to the closing brace.
    pub span: Span,
    /// The `extern` of `extern struct`: C's layout, passed to C by value.
    pub c_layout: Option<Span>,
    /// The `union` of `extern union`: C's union, its
    /// fields sharing one place.
    pub union: Option<Span>,
    /// The `actor` keyword when this is an actor's state;
    /// its methods follow as an `impl` with `actor` set. `span` is the
    /// whole actor.
    pub actor: Option<Span>,
}

/// A method's `self` or `mut self` parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Receiver {
    /// `mut self`.
    pub mutable: bool,
    /// From `mut` (if any) to `self`.
    pub span: Span,
}

/// An enum declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl {
    /// `private` or `public`, if written.
    pub visibility: Option<Visibility>,
    /// Enum name.
    pub name: Ident,
    /// `<T, …>` after the name.
    pub type_params: Vec<TypeParam>,
    /// Variants in declaration order.
    pub variants: Vec<Variant>,
    /// From `enum` to the closing brace.
    pub span: Span,
}

/// One enum variant and its positional payload types.
#[derive(Debug, Clone, PartialEq)]
pub struct Variant {
    /// Variant name.
    pub name: Ident,
    /// Payload types in order; empty for `Empty` and for `Empty()`.
    pub payloads: Vec<Type>,
    /// From the name to the closing parenthesis, if any.
    pub span: Span,
}

/// An `impl` block.
#[derive(Debug, Clone, PartialEq)]
pub struct ImplDecl {
    /// `m` in `impl m.Type`.
    pub module: Option<Ident>,
    /// The type the methods belong to.
    pub ty: Ident,
    /// The type's parameters, `impl Stack<T>`, with bounds
    /// for every method of the block, `impl NdArray<T: Num>`.
    pub params: Vec<TypeParam>,
    /// Methods and associated functions in order.
    pub methods: Vec<FunDecl>,
    /// From `impl` to the closing brace; for an actor's methods, the whole actor.
    pub span: Span,
    /// The methods of an actor, declared in its body.
    pub actor: bool,
}

/// A typed name: a function parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    /// The name.
    pub name: Ident,
    /// Its declared type.
    pub ty: Type,
    /// `out`, `ptr` or `mut` before an extern's parameter: how it reaches C
    /// by address, and where the word is.
    pub mode: Option<(PassMode, Span)>,
}

/// How an extern's parameter reaches C by address.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PassMode {
    /// `out x`: C writes a zeroed slot; the place gets it after the call.
    Out,
    /// `ptr e`: C reads a copy through `const T *`.
    Ptr,
    /// `mut x`: C reads and writes the place's value; it goes back after the call.
    Mut,
}

impl PassMode {
    /// The word written for this mode.
    pub fn word(self) -> &'static str {
        match self {
            PassMode::Out => "out",
            PassMode::Ptr => "ptr",
            PassMode::Mut => "mut",
        }
    }
}

/// `{ stmts tail }`.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    /// Statements in order.
    pub stmts: Vec<Stmt>,
    /// The final expression without a `;`, if any — the block's value.
    pub tail: Option<Box<Expr>>,
    /// From `{` to `}` (or the expression, for an expression body).
    pub span: Span,
}

/// One statement.
#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    /// Which statement.
    pub kind: StmtKind,
    /// Its source location, including any `;`.
    pub span: Span,
}

/// The statement forms of the language.
#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    /// `let [mut] name [: ty] = init;`
    Let {
        mutable: bool,
        name: Ident,
        ty: Option<Type>,
        init: Expr,
    },
    /// `place op value;` — the checker verifies `place` is a place expression;
    /// `op_span` is the operator, where overflow panics point.
    Assign {
        op: AssignOp,
        op_span: Span,
        place: Expr,
        value: Expr,
    },
    /// An expression statement. `semicolon` is false only for a block-like
    /// (`if` or `{`) statement written without one.
    Expr { expr: Expr, semicolon: bool },
    /// `[label:] while cond { body }`
    While {
        label: Option<Ident>,
        cond: Expr,
        body: Block,
    },
    /// `[label:] while let pattern = scrutinee { body }`.
    WhileLet {
        label: Option<Ident>,
        pattern: Box<Pattern>,
        scrutinee: Expr,
        body: Box<Block>,
    },
    /// `let pattern = init else { … };`: the `else` block must leave.
    LetElse {
        pattern: Box<Pattern>,
        init: Expr,
        else_: Option<Box<Block>>,
    },
    /// `[label:] for var in a..b { body }` or `… for var of xs { body }`
    For {
        label: Option<Ident>,
        var: Ident,
        iter: ForIter,
        body: Block,
    },
    /// `return [expr];`
    Return(Option<Expr>),
    /// `break [label] [value];`
    Break {
        label: Option<Ident>,
        value: Option<Expr>,
    },
    /// `continue [label];`
    Continue { label: Option<Ident> },
    /// `defer e;`, `defer { … }` or `defer place op value;`: the statement, an
    /// `Expr` or `Assign`, runs when the enclosing block is left, on every
    /// exit but a panic.
    Defer(Box<Stmt>),
}

/// What a `for` loop iterates over.
#[derive(Debug, Clone, PartialEq)]
pub enum ForIter {
    /// `in start..end` (half-open).
    Range(Expr, Expr),
    /// `of array`.
    Array(Expr),
    /// `of mut place`: an iterator advanced in place, or an array
    /// whose elements the loop variable aliases.
    MutOf(Expr),
}

/// `=`, `+=`, `-=`, `*=`, `/=`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    /// `%=`.
    Rem,
    /// The bit assignments: `&=`, `|=`, `^=`, `<<=`, `>>=`.
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

impl AssignOp {
    /// The operator as written in source.
    pub fn symbol(self) -> &'static str {
        match self {
            AssignOp::Assign => "=",
            AssignOp::Add => "+=",
            AssignOp::Sub => "-=",
            AssignOp::Mul => "*=",
            AssignOp::Div => "/=",
            AssignOp::Rem => "%=",
            AssignOp::BitAnd => "&=",
            AssignOp::BitOr => "|=",
            AssignOp::BitXor => "^=",
            AssignOp::Shl => "<<=",
            AssignOp::Shr => ">>=",
        }
    }
}
