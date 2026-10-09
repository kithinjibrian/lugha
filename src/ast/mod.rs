//! Abstract syntax tree produced by the parser (spec §3).
//!
//! A plain owned tree. Every expression carries a span and an `ExprId`; ids
//! are dense (`0..Program::expr_count`) so the checker can keep its
//! expression → type table in a `Vec`. The tree records syntax only: names
//! are unresolved and nothing is type-checked.
//!
//! Depends on: span.

mod expr;

pub use expr::{BinOp, Expr, ExprId, ExprKind, UnOp};

use crate::span::Span;

/// A whole `.la` file.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
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
}

/// A function with a body.
#[derive(Debug, Clone, PartialEq)]
pub struct FunDecl {
    /// Function name.
    pub name: Ident,
    /// Parameters in order.
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
    /// Struct name.
    pub name: Ident,
    /// Fields in declaration order.
    pub fields: Vec<Field>,
    /// From `struct` to the closing brace.
    pub span: Span,
}

/// A typed name: a function parameter or a struct field.
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    /// The name.
    pub name: Ident,
    /// Its declared type.
    pub ty: Type,
}

/// A struct field has the same shape as a parameter.
pub type Field = Param;

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
    /// A struct name, resolved by the checker.
    Named(String),
    /// `T[]`.
    Array(Box<Type>),
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
    /// `place op value;` — the checker verifies `place` is a place expression.
    Assign {
        op: AssignOp,
        place: Expr,
        value: Expr,
    },
    /// An expression statement. `semicolon` is false only for a block-like
    /// (`if` or `{`) statement written without one.
    Expr { expr: Expr, semicolon: bool },
    /// `while cond { body }`
    While { cond: Expr, body: Block },
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
}

/// `=`, `+=`, `-=`, `*=`, `/=`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
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
        }
    }
}
