//! Expression nodes of the AST.

use super::{Block, Ident, Type};
use crate::span::Span;

/// Identity of one expression node, dense from 0.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ExprId(pub u32);

/// One expression.
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    /// Unique, dense id; key of the checker's type table.
    pub id: ExprId,
    /// Source location (parentheses around it are not included).
    pub span: Span,
    /// Which expression.
    pub kind: ExprKind,
}

/// The expression forms of spec §3.
#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Int(u64),
    Float(f64),
    Str(String),
    Bool(bool),
    /// A variable, function or intrinsic name.
    Name(String),
    Unary(UnOp, Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    /// `expr as Type`.
    Cast(Box<Expr>, Type),
    /// `callee(args)`.
    Call(Box<Expr>, Vec<Expr>),
    /// `base[index]`.
    Index(Box<Expr>, Box<Expr>),
    /// `base.field` (also `.len`).
    Field(Box<Expr>, Ident),
    /// `Name { field: value, … }` in source order.
    StructLit(Ident, Vec<(Ident, Expr)>),
    /// `[a, b, c]`.
    Array(Vec<Expr>),
    /// `[value; count]`.
    Repeat(Box<Expr>, Box<Expr>),
    /// `if cond { then } [else …]`; the else branch is an `If` or `Block` expression.
    If {
        cond: Box<Expr>,
        then: Block,
        else_: Option<Box<Expr>>,
    },
    /// A block used as an expression.
    Block(Block),
}

/// Prefix operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    /// `-`
    Neg,
    /// `!`
    Not,
}

/// Binary operators, lowest precedence first.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Or,
    And,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

impl BinOp {
    /// The operator as written in source.
    pub fn symbol(self) -> &'static str {
        use BinOp::*;
        match self {
            Or => "||",
            And => "&&",
            Eq => "==",
            Ne => "!=",
            Lt => "<",
            Le => "<=",
            Gt => ">",
            Ge => ">=",
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Rem => "%",
        }
    }

    /// Precedence level from the spec §3 table: 1 (`||`) to 6 (`* / %`).
    pub fn level(self) -> u8 {
        use BinOp::*;
        match self {
            Or => 1,
            And => 2,
            Eq | Ne => 3,
            Lt | Le | Gt | Ge => 4,
            Add | Sub => 5,
            Mul | Div | Rem => 6,
        }
    }
}
