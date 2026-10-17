//! Expression nodes of the AST.

use super::{Arm, Block, Ident, Type};
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
    /// `(a, b, …)`, two or more elements (v14).
    Tuple(Vec<Expr>),
    /// A `char` literal's code point (v14).
    Char(u32),
    Float(f64),
    Str(String),
    /// `"a {x} b"`: literal text and interpolated expressions in order (v3).
    Interp(Vec<Part>),
    /// `format("{} has {:>4}", a, b)` with a literal template (v14), boxed
    /// so `Expr` stays small.
    Format(Box<FormatCall>),
    /// `e?`: the `Ok`/`Some` value, or an early return (v4).
    Try(Box<Expr>),
    /// `fun(x) = e` or `fun(x: T): R { … }` (v3). Boxed: it is large and rare.
    Closure(Box<Closure>),
    Bool(bool),
    /// A variable, function or intrinsic name.
    Name(String),
    Unary(UnOp, Box<Expr>),
    /// `lhs op rhs`; the `Span` is the operator token, where overflow panics point.
    Binary(BinOp, Span, Box<Expr>, Box<Expr>),
    /// `expr as Type`.
    Cast(Box<Expr>, Type),
    /// `callee(args)`.
    Call(Box<Expr>, Vec<Expr>),
    /// `base[index]`; the `Span` is the `[`, where a bounds panic points.
    Index(Box<Expr>, Span, Box<Expr>),
    /// `base.field` (also `.len`).
    Field(Box<Expr>, Ident),
    /// `Name { field: value, … }` or `m.Name { … }` (v2 draft §3). Boxed
    /// so every `Expr` stays small: the parser recurses once per nesting
    /// level, and E0206's depth limit assumes small frames.
    StructLit(Box<StructLiteral>),
    /// `[a, b, c]`.
    Array(Vec<Expr>),
    /// `[value; count]`.
    Repeat(Box<Expr>, Box<Expr>),
    /// `out place`, `ptr e` or `mut place`: a call argument for an extern's
    /// parameter of that mode (v17, milestones 102–103).
    Pass(crate::ast::PassMode, Box<Expr>),
    /// `#{k: v, …}`, `#{x, …}` or `#{}` (v17, milestone 101). Boxed: rare.
    Hashed(Box<HashLiteral>),
    /// `if cond { then } [else …]`; the else branch is an `If` or `Block` expression.
    If {
        cond: Box<Expr>,
        then: Block,
        else_: Option<Box<Expr>>,
    },
    /// `if let pattern = scrutinee { then } [else …]` (v9), boxed whole
    /// so `Expr` stays as small as before (the parser recurses on it).
    IfLet(Box<IfLet>),
    /// A block used as an expression.
    Block(Block),
    /// `match scrutinee { arms }` (v1).
    Match(Box<Expr>, Vec<Arm>),
    /// `spawn call` (v17): the call, wrapped by the parser in a closure
    /// with no parameters, which the new green thread runs. Boxed: rare.
    Spawn(Box<Expr>),
    /// `select { arms }` (v17).
    Select(Vec<SelectArm>),
}

/// Prefix operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    /// `-`
    Neg,
    /// `!`
    Not,
    /// `~`, bitwise not (v7).
    BitNot,
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
    /// v7 bit operators: `|`, `^`, `&`, `<<`, `>>`.
    BitOr,
    BitXor,
    BitAnd,
    Shl,
    Shr,
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
            BitOr => "|",
            BitXor => "^",
            BitAnd => "&",
            Shl => "<<",
            Shr => ">>",
        }
    }

    /// `& | ^ << >>` (v7): integer operands of one type.
    pub fn is_bitwise(self) -> bool {
        use BinOp::*;
        matches!(self, BitOr | BitXor | BitAnd | Shl | Shr)
    }

    /// Precedence level from the spec §3 table: 1 (`||`) to 10 (`* / %`);
    /// the v7 bit operators sit between comparisons and `+ -`, as in Rust.
    pub fn level(self) -> u8 {
        use BinOp::*;
        match self {
            Or => 1,
            And => 2,
            Eq | Ne => 3,
            Lt | Le | Gt | Ge => 4,
            BitOr => 5,
            BitXor => 6,
            BitAnd => 7,
            Shl | Shr => 8,
            Add | Sub => 9,
            Mul | Div | Rem => 10,
        }
    }
}

/// A closure literal (v3 draft §3).
#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    /// Parameter names and their types, if written.
    pub params: Vec<(super::Ident, Option<super::Type>)>,
    /// The return type, if written.
    pub ret: Option<super::Type>,
    /// The body; `= e` is a block with tail `e`, as for functions.
    pub body: super::Block,
}

/// A `format` call with a literal template (v14).
#[derive(Debug, Clone, PartialEq)]
pub struct FormatCall {
    /// The template as the lexer kept it: escapes decoded, braces as written.
    pub template: String,
    /// The template literal, quotes included.
    pub template_span: Span,
    /// The values for its holes, in order.
    pub args: Vec<Expr>,
}

/// The contents of a struct literal.
#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral {
    /// `m` in `m.Name { … }` (v2 draft §3).
    pub module: Option<Ident>,
    /// The struct's name.
    pub name: Ident,
    /// `field: value` pairs in source order.
    pub fields: Vec<(Ident, Expr)>,
    /// `..base` after the fields: where every field not written comes
    /// from (v17, milestone 93).
    pub base: Option<Box<Expr>>,
}

/// One piece of an interpolated string (v3 draft §2).
#[derive(Debug, Clone, PartialEq)]
pub enum Part {
    /// Literal text, escapes and `{{`/`}}` already decoded.
    Text(String),
    /// A `{expr}`, or `{expr:spec}` with a format specifier (v13).
    Expr(Expr, Option<FormatText>),
}

/// A format specifier as written after an interpolation's `:` (v13).
#[derive(Debug, Clone, PartialEq)]
pub struct FormatText {
    /// The text after the `:`.
    pub text: String,
    /// From the `:` to the end of the text.
    pub span: Span,
}

impl Part {
    /// The interpolated expression, or `None` for literal text.
    pub fn expr(&self) -> Option<&Expr> {
        match self {
            Part::Expr(e, _) => Some(e),
            Part::Text(_) => None,
        }
    }
}

/// One arm of a `select` (v17): what it waits for, then `=>` and its value.
#[derive(Debug, Clone, PartialEq)]
pub struct SelectArm {
    /// What the arm waits for.
    pub kind: SelectKind,
    /// The arm's value.
    pub body: Expr,
    /// From the arm's first token to the end of its body.
    pub span: Span,
}

/// What a `select` arm waits for (v17).
#[derive(Debug, Clone, PartialEq)]
pub enum SelectKind {
    /// `name = chan.recv()`, or `_ = chan.recv()`: `name` is the
    /// `Option<T>` received. `call` is the whole `chan.recv()`.
    Recv { binding: super::Ident, call: Expr },
    /// `chan.send(value)`: `call` is the whole call.
    Send(Expr),
    /// `after(ms)`: the most to wait, in milliseconds.
    After(Expr),
    /// `default`: when nothing is ready, so the `select` never waits.
    Default(Span),
}

impl SelectKind {
    /// The channel of a `recv` or `send` arm.
    pub fn channel(&self) -> Option<&Expr> {
        match self {
            SelectKind::Recv { call, .. } | SelectKind::Send(call) => match &call.kind {
                ExprKind::Call(callee, _) => match &callee.kind {
                    ExprKind::Field(chan, _) => Some(chan),
                    _ => None,
                },
                _ => None,
            },
            SelectKind::After(_) | SelectKind::Default(_) => None,
        }
    }
}

/// The call a `spawn`'s closure runs (v17): the tail of its body.
pub fn spawned_call(closure: &Expr) -> &Expr {
    match &closure.kind {
        ExprKind::Closure(c) => c
            .body
            .tail
            .as_deref()
            .expect("the parser gives a spawn's closure the call as its tail"),
        _ => unreachable!("the parser wraps a spawn's call in a closure"),
    }
}

/// The parts of an `if let` (v9).
#[derive(Debug, Clone, PartialEq)]
pub struct IfLet {
    pub pattern: super::Pattern,
    pub scrutinee: Expr,
    pub then: Block,
    pub else_: Option<Box<Expr>>,
}

/// Which `#{ … }` a literal is (v17, milestone 101).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HashKind {
    /// `#{k: v, …}`.
    Map,
    /// `#{x, …}`.
    Set,
    /// `#{}`: a map or a set, by its expected type.
    Empty,
}

/// A `#{ … }` literal (v17, milestone 101). The parser gives its keys (a
/// set's values) and a map's values as two array expressions of their own,
/// so the checker and codegen treat the literal as a call of the prelude's
/// `$map_of(keys, values)` or `$set_of(keys)`.
#[derive(Debug, Clone, PartialEq)]
pub struct HashLiteral {
    pub kind: HashKind,
    pub keys: Expr,
    pub values: Expr,
}
