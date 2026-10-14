//! Abstract syntax tree produced by the parser (spec §3).
//!
//! A plain owned tree. Every expression carries a span and an `ExprId`; ids
//! are dense (`0..Program::expr_count`) so the checker can keep its
//! expression → type table in a `Vec`. The tree records syntax only: names
//! are unresolved and nothing is type-checked.
//!
//! Depends on: span.

mod expr;
mod header;
mod pattern;
mod types;

pub use expr::{
    BinOp, Closure, Expr, ExprId, ExprKind, FormatText, IfLet, Part, StructLiteral, UnOp,
};
pub use header::{
    Bound, Field, Import, InterfaceDecl, InterfaceItem, TypeParam, Visibility, VisibilityKind,
};
pub use pattern::{Arm, Pattern, PatternKind};
pub use types::{AppliedType, Type, TypeKind};

use crate::span::Span;

/// A whole `.la` file.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    /// `import`s, which come before every item (v2 draft §3).
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
    /// `struct Name { fields }`.
    Struct(StructDecl),
    /// `enum Name { variants }` (v1).
    Enum(EnumDecl),
    /// `impl Type { methods }` (v1).
    Impl(ImplDecl),
    /// `interface Name { signatures }` (v2 draft §4, milestone 12).
    Interface(InterfaceDecl),
}

/// A function with a body.
#[derive(Debug, Clone, PartialEq)]
pub struct FunDecl {
    /// `private` or `public`, if written (v2 draft §6).
    pub visibility: Option<Visibility>,
    /// Function name.
    pub name: Ident,
    /// `<T, U: Eq>` after the name (v2 draft §4).
    pub type_params: Vec<TypeParam>,
    /// `self` or `mut self`; only methods in an `impl` have one (v1).
    pub receiver: Option<Receiver>,
    /// Parameters in order, after any receiver.
    pub params: Vec<Param>,
    /// Declared return type; `None` means `void`.
    pub ret: Option<Type>,
    /// The body. An expression body `= e;` is stored as a block with tail `e`.
    pub body: Block,
    /// From `fun` to the end of the body.
    pub span: Span,
}

/// A C function declaration (spec §8).
#[derive(Debug, Clone, PartialEq)]
pub struct ExternDecl {
    /// `private` or `public`, if written (v2 draft §6).
    pub visibility: Option<Visibility>,
    /// The C symbol name.
    pub name: Ident,
    /// Parameters in order.
    pub params: Vec<Param>,
    /// Declared return type; `None` means `void`.
    pub ret: Option<Type>,
    /// From `extern` to the `;`.
    pub span: Span,
}

/// A struct declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    /// `private` or `public`, if written (v2 draft §6).
    pub visibility: Option<Visibility>,
    /// Struct name.
    pub name: Ident,
    /// `<T, …>` after the name (v2 draft §4).
    pub type_params: Vec<TypeParam>,
    /// Fields in declaration order.
    pub fields: Vec<Field>,
    /// From `struct` to the closing brace.
    pub span: Span,
}

/// A method's `self` or `mut self` parameter (v1).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Receiver {
    /// `mut self`.
    pub mutable: bool,
    /// From `mut` (if any) to `self`.
    pub span: Span,
}

/// An enum declaration (v1).
#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl {
    /// `private` or `public`, if written (v2 draft §6).
    pub visibility: Option<Visibility>,
    /// Enum name.
    pub name: Ident,
    /// `<T, …>` after the name (v2 draft §4).
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

/// An `impl` block (v1).
#[derive(Debug, Clone, PartialEq)]
pub struct ImplDecl {
    /// `m` in `impl m.Type` (v2 draft §3).
    pub module: Option<Ident>,
    /// The type the methods belong to.
    pub ty: Ident,
    /// The type's parameters, `impl Stack<T>` (v2 draft §3).
    pub params: Vec<Ident>,
    /// Methods and associated functions in order.
    pub methods: Vec<FunDecl>,
    /// From `impl` to the closing brace.
    pub span: Span,
}

/// A typed name: a function parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    /// The name.
    pub name: Ident,
    /// Its declared type.
    pub ty: Type,
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

/// The statement forms of spec §3.
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
    /// `while cond { body }`
    While { cond: Expr, body: Block },
    /// `while let pattern = scrutinee { body }` (v9).
    WhileLet {
        pattern: Box<Pattern>,
        scrutinee: Expr,
        body: Box<Block>,
    },
    /// `let pattern = init else { … };` (v9): the `else` block must leave.
    LetElse {
        pattern: Box<Pattern>,
        init: Expr,
        else_: Option<Box<Block>>,
    },
    /// `for var in a..b { body }` or `for var of xs { body }`
    For {
        var: Ident,
        iter: ForIter,
        body: Block,
    },
    /// `return [expr];`
    Return(Option<Expr>),
    /// `break;`
    Break,
    /// `continue;`
    Continue,
}

/// What a `for` loop iterates over.
#[derive(Debug, Clone, PartialEq)]
pub enum ForIter {
    /// `in start..end` (half-open).
    Range(Expr, Expr),
    /// `of array`.
    Array(Expr),
    /// `of mut place` (v13): an iterator advanced in place, or an array
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
    /// `%=` (v9).
    Rem,
    /// v7: `&=`, `|=`, `^=`, `<<=`, `>>=`.
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
