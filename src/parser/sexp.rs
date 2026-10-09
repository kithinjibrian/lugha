//! S-expression printer for the AST, used by tests and `--emit=ast`.
//!
//! Ids and spans are omitted so the output only changes when the tree's
//! shape does. Forms are listed in PRP-003.

use crate::ast::{
    Block, Expr, ExprKind, ForIter, Item, Param, Program, Stmt, StmtKind, Type, TypeKind, UnOp,
};

/// Prints every item, one per line.
///
/// # Examples
///
/// ```
/// let (tokens, _) = lugha::lexer::lex("fun f(x: i64): i64 = x;").unwrap();
/// let (program, _) = lugha::parser::parse(&tokens).unwrap();
/// assert_eq!(lugha::parser::sexp::program(&program), "(fun f ((x i64)) i64 (block x))");
/// ```
pub fn program(program: &Program) -> String {
    program
        .items
        .iter()
        .map(item)
        .collect::<Vec<_>>()
        .join("\n")
}

fn item(item: &Item) -> String {
    match item {
        Item::Fun(f) => {
            format!(
                "(fun {} {} {} {})",
                f.name.name,
                params(&f.params),
                ret(&f.ret),
                block(&f.body)
            )
        }
        Item::Extern(e) => format!(
            "(extern {} {} {})",
            e.name.name,
            params(&e.params),
            ret(&e.ret)
        ),
        Item::Struct(s) => {
            let fields: String = s.fields.iter().map(|f| format!(" {}", param(f))).collect();
            format!("(struct {}{fields})", s.name.name)
        }
    }
}

fn params(params: &[Param]) -> String {
    format!(
        "({})",
        params.iter().map(param).collect::<Vec<_>>().join(" ")
    )
}

fn param(param: &Param) -> String {
    format!("({} {})", param.name.name, ty(&param.ty))
}

fn ret(ret: &Option<Type>) -> String {
    ret.as_ref().map_or_else(|| "void".to_string(), ty)
}

fn ty(ty: &Type) -> String {
    match &ty.kind {
        TypeKind::I32 => "i32".into(),
        TypeKind::I64 => "i64".into(),
        TypeKind::U8 => "u8".into(),
        TypeKind::F64 => "f64".into(),
        TypeKind::Bool => "bool".into(),
        TypeKind::String => "string".into(),
        TypeKind::Named(name) => name.clone(),
        TypeKind::Array(element) => format!("{}[]", self::ty(element)),
    }
}

/// Prints one block: `(block stmt… tail)`.
pub fn block(block: &Block) -> String {
    let mut parts = vec!["block".to_string()];
    parts.extend(block.stmts.iter().map(stmt));
    parts.extend(block.tail.iter().map(|tail| expr(tail)));
    format!("({})", parts.join(" "))
}

fn stmt(stmt: &Stmt) -> String {
    match &stmt.kind {
        StmtKind::Let {
            mutable,
            name,
            ty: annotation,
            init,
        } => {
            let m = if *mutable { " mut" } else { "" };
            let t = annotation
                .as_ref()
                .map(|t| format!(" {}", ty(t)))
                .unwrap_or_default();
            format!("(let{m} {}{t} {})", name.name, expr(init))
        }
        StmtKind::Assign {
            op, place, value, ..
        } => {
            format!("({} {} {})", op.symbol(), expr(place), expr(value))
        }
        StmtKind::Expr {
            expr: e,
            semicolon: true,
        } => format!("(; {})", expr(e)),
        StmtKind::Expr {
            expr: e,
            semicolon: false,
        } => format!("(stmt {})", expr(e)),
        StmtKind::While { cond, body } => format!("(while {} {})", expr(cond), block(body)),
        StmtKind::For { var, iter, body } => {
            let iter = match iter {
                ForIter::Range(a, b) => format!("(range {} {})", expr(a), expr(b)),
                ForIter::Array(xs) => format!("(of {})", expr(xs)),
            };
            format!("(for {} {iter} {})", var.name, block(body))
        }
        StmtKind::Return(Some(value)) => format!("(return {})", expr(value)),
        StmtKind::Return(None) => "(return)".into(),
        StmtKind::Break => "(break)".into(),
        StmtKind::Continue => "(continue)".into(),
    }
}

/// Prints one expression.
pub fn expr(expr: &Expr) -> String {
    let list = |head: &str, items: &[&Expr]| {
        let mut parts = vec![head.to_string()];
        parts.extend(items.iter().map(|e| self::expr(e)));
        format!("({})", parts.join(" "))
    };
    match &expr.kind {
        ExprKind::Int(value) => value.to_string(),
        ExprKind::Float(value) => format!("{value:?}"),
        ExprKind::Str(value) => format!("{value:?}"),
        ExprKind::Bool(value) => value.to_string(),
        ExprKind::Name(name) => name.clone(),
        ExprKind::Unary(UnOp::Neg, e) => list("neg", &[e]),
        ExprKind::Unary(UnOp::Not, e) => list("not", &[e]),
        ExprKind::Binary(op, _, l, r) => list(op.symbol(), &[l, r]),
        ExprKind::Cast(e, t) => format!("(as {} {})", self::expr(e), ty(t)),
        ExprKind::Call(callee, args) => {
            let mut items = vec![&**callee];
            items.extend(args.iter());
            list("call", &items)
        }
        ExprKind::Index(base, _, index) => list("index", &[base, index]),
        ExprKind::Field(base, field) => format!("(. {} {})", self::expr(base), field.name),
        ExprKind::StructLit(name, fields) => {
            let fields: String = fields
                .iter()
                .map(|(f, v)| format!(" ({} {})", f.name, self::expr(v)))
                .collect();
            format!("(struct-lit {}{fields})", name.name)
        }
        ExprKind::Array(elements) => list("array", &elements.iter().collect::<Vec<_>>()),
        ExprKind::Repeat(value, count) => list("repeat", &[value, count]),
        ExprKind::If { cond, then, else_ } => {
            let otherwise = else_
                .as_ref()
                .map(|e| format!(" {}", self::expr(e)))
                .unwrap_or_default();
            format!("(if {} {}{otherwise})", self::expr(cond), block(then))
        }
        ExprKind::Block(b) => block(b),
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::program;

    #[test]
    fn items_print_one_per_line() {
        let src = "struct E {}\nextern fun abs(x: i32): i32;\nfun f() {}";
        assert_eq!(
            program(src),
            "(struct E)\n(extern abs ((x i32)) i32)\n(fun f () void (block))"
        );
    }

    #[test]
    fn literals_print_in_rust_debug_form() {
        use crate::parser::test_util::expr;
        assert_eq!(
            expr("[1, 2.5, \"a\\n\", true, false]"),
            r#"(array 1 2.5 "a\n" true false)"#
        );
    }
}
