//! Operator overloading (v15 draft decisions 8–13, milestone 52): `a + b`
//! on a struct, enum, interface value or bounded type parameter calls
//! `a.add(b)`, and likewise `sub`, `mul`, `div`, `rem` and unary `neg`. A
//! method qualifies by its shape: a plain `self`, one parameter (none for
//! `neg`) and a result. The call is checked as a method call whose id is
//! the operator expression's, so instances and bound uses work unchanged.
//! The numbers (and `string` and arrays, for `Add`) satisfy the prelude's operator
//! bounds as built-ins.

use super::iterators::is_operator_bound;
use super::{Checker, Type, errors};
use crate::ast::{AssignOp, BinOp, Expr, Ident};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// The method `op` calls, for the overloadable operators.
pub(super) fn method_of(op: BinOp) -> Option<&'static str> {
    Some(match op {
        BinOp::Add => "add",
        BinOp::Sub => "sub",
        BinOp::Mul => "mul",
        BinOp::Div => "div",
        BinOp::Rem => "rem",
        _ => return None,
    })
}

/// The method a compound assignment calls: `a += b` is `a = a.add(b)`.
pub(super) fn method_of_assign(op: AssignOp) -> Option<&'static str> {
    Some(match op {
        AssignOp::Add => "add",
        AssignOp::Sub => "sub",
        AssignOp::Mul => "mul",
        AssignOp::Div => "div",
        AssignOp::Rem => "rem",
        _ => return None,
    })
}

/// True for the types whose operators are methods: structs, enums,
/// interface values and type parameters. Numbers and strings keep the
/// built-in operators.
pub(super) fn overloaded(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Struct(..) | Type::Enum(..) | Type::Interface(..) | Type::Param(_)
    )
}

impl Checker {
    /// What `arg` lacks to satisfy a prelude operator bound as a built-in:
    /// nothing for a number (or a `string` and `Add`), the reason for a
    /// non-number and `Num`; `None` when the ordinary rules decide.
    pub(super) fn builtin_operator(&self, arg: &Type, key: &str) -> Option<Vec<String>> {
        let name = key.strip_prefix("$prelude.")?;
        if !is_operator_bound(name) {
            return None;
        }
        // `Float` (v15, milestone 53): `f32` and `f64` only.
        if name == "Float" {
            let parameter = matches!(arg, Type::Param(p) if self.has_float(p));
            return match arg.is_float() || parameter || matches!(arg, Type::Error) {
                true => Some(Vec::new()),
                false => Some(vec!["only `f32` and `f64` are `Float`".to_string()]),
            };
        }
        // Strings and (v17, milestone 122) arrays join with `+`.
        if arg.is_numeric() || (name == "Add" && matches!(arg, Type::String | Type::Array(_))) {
            return Some(Vec::new());
        }
        let parameter = matches!(arg, Type::Param(p) if self.has_num(p));
        if name == "Num" && !parameter && !matches!(arg, Type::Error) {
            return Some(vec!["only the number types are `Num`".to_string()]);
        }
        None
    }

    /// True if type parameter `p` is bounded by the prelude's `Num`, or
    /// by `Float`, which allows everything `Num` does (v15).
    pub(super) fn has_num(&self, p: &str) -> bool {
        self.bounds(p)
            .interfaces
            .iter()
            .any(|a| a.key == "$prelude.Num" || a.key == "$prelude.Float")
    }

    /// True if type parameter `p` is bounded by the prelude's `Float`.
    pub(super) fn has_float(&self, p: &str) -> bool {
        self.bounds(p)
            .interfaces
            .iter()
            .any(|a| a.key == "$prelude.Float")
    }

    /// `lhs op rhs` (or `-lhs`, with no `rhs`) where `lhs`, already
    /// checked, has the overloaded type `l`: the call of method `name`.
    /// E0404 when `l` has no method of that name and shape.
    pub(super) fn operator_call(
        &mut self,
        expr: &Expr,
        (symbol, name, at): (&str, &str, Span),
        (lhs, l): (&Expr, &Type),
        rhs: Option<&Expr>,
        mark: usize,
    ) -> Type {
        let args: &[Expr] = rhs.map_or(&[], std::slice::from_ref);
        if let Err(help) = self.operator_method(l, name, args.len()) {
            let mut d = errors::bad_operands(symbol, std::slice::from_ref(l), expr.span);
            if let Some(help) = help {
                d = d.with_help(help);
            }
            self.report(d);
            return self.unchecked(args);
        }
        let method = Ident {
            name: name.to_string(),
            span: at,
        };
        self.on_value(expr, (lhs, l), &method, args, (mark, None))
    }

    /// `place op= value` on an overloaded `target`: `place = place.op(value)`,
    /// whose result must be `target` (E0403).
    pub(super) fn operator_assign(
        &mut self,
        (symbol, name): (&str, &str),
        (place, target): (&Expr, &Type),
        value: &Expr,
        mark: usize,
    ) {
        let span = Span::new(place.span.start, value.span.end);
        let ty = self.operator_call(
            place,
            (symbol, name, span),
            (place, target),
            Some(value),
            mark,
        );
        if ty != *target && !matches!(ty, Type::Error | Type::Never) {
            let message = format!("`{symbol}` gives {ty} here, but the place is {target}");
            self.report(Diagnostic::error("E0403", message, span));
        }
    }

    /// Whether `l` has an operator method `name` taking `arity` operands:
    /// `Err` with help when a method of that name has the wrong shape, or
    /// a type parameter lacks the bound.
    fn operator_method(&self, l: &Type, name: &str, arity: usize) -> Result<(), Option<String>> {
        let shape = || {
            let params = if arity == 0 { "" } else { ", other: …" };
            format!(
                "for an operator, `{name}` must be `fun {name}(self{params}): …`: a plain `self` and a result"
            )
        };
        match l {
            Type::Param(p) => match self.bound_entry(p, name) {
                Some((req, _)) if req.receiver == Some(false) && req.params.len() == arity => {
                    Ok(())
                }
                Some(_) => Err(Some(shape())),
                None => Err(Some(format!(
                    "add a bound that allows it, such as `{p}: Num` or `{p}: {}`",
                    bound_name(name)
                ))),
            },
            Type::Interface(..) => {
                let applied = super::applied::Applied::of(l).expect("an interface type");
                match self
                    .items_for(&applied, l)
                    .into_iter()
                    .find(|r| r.name == name)
                {
                    Some(r) if r.receiver == Some(false) && r.params.len() == arity => Ok(()),
                    Some(_) => Err(Some(shape())),
                    None => Err(None),
                }
            }
            _ => match self.method(l, name) {
                Some(m)
                    if m.receiver == Some(false)
                        && m.params.len() == arity
                        && m.ret != Type::Unit =>
                {
                    Ok(())
                }
                Some(_) => Err(Some(shape())),
                None => Err(None),
            },
        }
    }
}

/// The prelude bound for the operator method `name`.
fn bound_name(name: &str) -> &'static str {
    match name {
        "add" => "Add",
        "sub" => "Sub",
        "mul" => "Mul",
        "div" => "Div",
        "rem" => "Rem",
        _ => "Neg",
    }
}

#[cfg(test)]
#[path = "operators_tests.rs"]
mod tests;
