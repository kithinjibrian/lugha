#[derive(Debug, Clone)]
pub enum Expr {
    Number(i32),
    Variable(String),
    BinaryOp {
        op: char,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let { name: String, value: Expr },
    Return(Expr),
}