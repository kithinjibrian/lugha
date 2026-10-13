//! S-expression printer for the AST, used by tests and `--emit=ast`.
//!
//! Ids and spans are omitted so the output only changes when the tree's
//! shape does. Forms are listed in PRP-003.

mod item;

pub use item::program;

use item::{qualified, ty};

use crate::ast::{
    Block, Expr, ExprKind, ForIter, Part, Pattern, PatternKind, Stmt, StmtKind, UnOp,
};

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
        ExprKind::Interp(parts) => {
            let shown: Vec<String> = parts
                .iter()
                .map(|p| match p {
                    Part::Text(t) => format!("{t:?}"),
                    Part::Expr(e) => self::expr(e),
                })
                .collect();
            format!("(interp {})", shown.join(" "))
        }
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
        ExprKind::StructLit(lit) => {
            let fields: String = lit
                .fields
                .iter()
                .map(|(f, v)| format!(" ({} {})", f.name, self::expr(v)))
                .collect();
            format!("(struct-lit {}{fields})", qualified(&lit.module, &lit.name))
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
        ExprKind::Match(scrutinee, arms) => {
            let arms: String = arms
                .iter()
                .map(|a| {
                    let guard = a
                        .guard
                        .as_ref()
                        .map(|g| format!(" (guard {})", self::expr(g)))
                        .unwrap_or_default();
                    format!(
                        " (=> {}{guard} {})",
                        pattern(&a.pattern),
                        self::expr(&a.body)
                    )
                })
                .collect();
            format!("(match {}{arms})", self::expr(scrutinee))
        }
    }
}

fn pattern(pattern: &Pattern) -> String {
    match &pattern.kind {
        PatternKind::Wildcard => "_".into(),
        PatternKind::Variant(name, None) => name.name.clone(),
        PatternKind::Variant(name, Some(slots)) => {
            let slots: Vec<String> = slots.iter().map(self::pattern).collect();
            format!("({} {})", name.name, slots.join(" "))
        }
        PatternKind::Int { value, negative } => {
            format!("{}{value}", if *negative { "-" } else { "" })
        }
        PatternKind::Str(text) => format!("{text:?}"),
        PatternKind::Bool(value) => value.to_string(),
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
