#[derive(Debug, Clone)]
pub enum AssignOp {
    Assign,             // =
    AddAssign,          // +=
    SubAssign,          // -=
    MulAssign,          // *=
    DivAssign,          // /=
    ModAssign,          // %=
    ShiftLeftAssign,    // <<=
    ShiftRightAssign,   // >>=
    AndAssign,          // &=
    XorAssign,          // ^=
    OrAssign,           // |=
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,                // +
    Subtract,           // -
    Multiply,           // *
    Divide,             // /
    Modulo,             // %
    Eq,                 // ==
    Neq,                // !=
    Lt,                 // <
    Gt,                 // >
    Leq,                // <=
    Geq,                // >=
    LeftShift,          // <<
    RightShift,         // >>
    LogicalOr,          // ||
    LogicalAnd,         // &&
    BitOr,              // |
    BitAnd,             // &
    BitXor,             // ^
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Not,                // ! 
    Negate,             // - 
}

#[derive(Debug, Clone)]
pub enum PostfixOp {
    Try,                // ? 
}

#[derive(Debug, Clone)]
pub enum RangeKind {
    Exclusive,          // 1..10
    Inclusive,          // 1..=10
}

#[derive(Debug, Clone)]
pub enum Expr {
    Float(i64),
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    Identifier(String),
    Block(Vec<Stmt>),
    Range {
        start: Box<Expr>,
        end: Box<Expr>,
        kind: RangeKind,
    },
    Assign {
        left: Box<Expr>,
        op: AssignOp,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    Postfix {
        op: PostfixOp,
        operand: Box<Expr>,
    },
    IfLet {
        pattern: String,
        expr: Box<Expr>,
        then_block: Box<Expr>,
        else_block: Option<Box<Expr>>,
    },
    If {
        condition: Box<Expr>,
        then_block: Box<Expr>,
        else_block: Option<Box<Expr>>,
    },
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
    },
    Call {
        function: Box<Expr>,
        args: Vec<Expr>,
    },
    FieldAccess {
        object: Box<Expr>,
        field: String,
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Break,
    Continue,
    Return(Option<Expr>),
    ExprStmt(Expr),
    Export(Box<Stmt>),
    Function {
        name: String,
        params: Vec<String>,
        body: Expr
    },
    Let {
        name: String,
        expr: Option<Expr>,
    },
    Enum {
        name: String,
        variants: Vec<String>
    },
    Struct {
        name: String,
        fields: Vec<(String, String)>
    },
    Module {
        name: String
    },
    Use {
        path: String
    }
}
