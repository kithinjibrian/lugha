//! Choosing among a type's arithmetic methods of one name (`mul`,
//! `mul(f64)`, …; see `modules::overloads`) by the right operand's type.
//! Only an exact type fits, and a literal must fit exactly one of them, so
//! nothing is ranked and adding one can't change an existing choice
//! silently. The choice is recorded by the call's id for codegen.

use super::generics::subst;
use super::{Checker, Type};
use crate::ast::{Expr, ExprKind, Ident, UnOp};
use crate::diagnostic::Diagnostic;
use crate::modules::overloads::{ARITHMETIC, written_name};

impl Checker {
    /// For `receiver.name(arg)` (or the operator that stands for it), the
    /// method among `name`'s overloads that `arg` fits: `None` when `name`
    /// has no overloads, `Some(Err(()))` after reporting why none fits.
    pub(super) fn chosen_overload(
        &mut self,
        call: &Expr,
        receiver: &Type,
        name: &Ident,
        args: &[Expr],
    ) -> Option<Result<Ident, ()>> {
        let (Type::Struct(owner, targs) | Type::Enum(owner, targs)) = receiver else {
            return None;
        };
        if !ARITHMETIC.contains(&name.name.as_str()) || args.len() != 1 {
            return None;
        }
        let family = self.overloads_of(owner, &name.name);
        if family.len() < 2 {
            return None;
        }
        let candidates: Vec<(String, Type)> = family
            .into_iter()
            .filter_map(|n| {
                let m = self.methods.get(owner)?.get(&n)?;
                let param = m.params.first()?;
                Some((n, subst(param, &m.generics[..m.outer], targs)))
            })
            .collect();
        let arg = &args[0];
        let fitting: Vec<&(String, Type)> = match literal_kind(arg) {
            Some(kind) => candidates.iter().filter(|(_, t)| kind.fits(t)).collect(),
            None => {
                // A probe: the call checks the argument again against its choice.
                let reported = self.diagnostics.len();
                let ty = self.value(arg, None);
                self.diagnostics.truncate(reported);
                if matches!(ty, Type::Error | Type::Never) {
                    return Some(Err(()));
                }
                candidates.iter().filter(|(_, t)| *t == ty).collect()
            }
        };
        let shown = |c: &[&(String, Type)]| {
            c.iter()
                .map(|(_, t)| format!("`{t}`"))
                .collect::<Vec<_>>()
                .join(", ")
        };
        match fitting.as_slice() {
            [(chosen, _)] => {
                self.overload_choices.insert(call.id, chosen.clone());
                Some(Ok(Ident {
                    name: chosen.clone(),
                    span: name.span,
                }))
            }
            [] => {
                let all: Vec<&(String, Type)> = candidates.iter().collect();
                let message = format!("no `{}` of `{receiver}` takes this operand", name.name);
                let help = format!("`{receiver}` has `{}` for {}", name.name, shown(&all));
                self.report(Diagnostic::error("E0404", message, arg.span).with_help(help));
                Some(Err(()))
            }
            several => {
                let message = format!(
                    "this operand fits more than one `{}` of `{receiver}`: {}",
                    name.name,
                    shown(several)
                );
                let help = match literal_kind(arg) {
                    Some(_) => format!("say which: {}", spelled(arg, several)),
                    // A generic type whose parameters became the same type.
                    None => format!(
                        "for `{receiver}` these take the same type; give one of them another name"
                    ),
                };
                self.report(Diagnostic::error("E0457", message, arg.span).with_help(help));
                Some(Err(()))
            }
        }
    }

    /// The names of `owner`'s methods declared as `name`, overloads included.
    pub(super) fn overloads_of(&self, owner: &str, name: &str) -> Vec<String> {
        let Some(methods) = self.methods.get(owner) else {
            return Vec::new();
        };
        let mut names: Vec<String> = methods
            .keys()
            .filter(|k| written_name(k) == name)
            .cloned()
            .collect();
        names.sort();
        names
    }
}

/// Ways to write `arg` as each of `types`: `2.0`, `2 as u8`.
fn spelled(arg: &Expr, types: &[&(String, Type)]) -> String {
    let ways: Vec<String> = types
        .iter()
        .map(|(_, t)| match &arg.kind {
            ExprKind::Int(n) if t.is_float() => format!("`{n}.0`"),
            ExprKind::Int(n) => format!("`{n} as {t}`"),
            _ => format!("a `{t}`"),
        })
        .collect();
    ways.join(" or ")
}

/// What kind of literal an operand is, if it takes its type from where it's used.
enum Literal {
    Integer,
    Float,
}

impl Literal {
    fn fits(&self, ty: &Type) -> bool {
        match self {
            // An integer literal can be any number, as `let x: f64 = 2;` shows.
            Literal::Integer => ty.is_numeric(),
            Literal::Float => ty.is_float(),
        }
    }
}

fn literal_kind(e: &Expr) -> Option<Literal> {
    match &e.kind {
        ExprKind::Int(_) => Some(Literal::Integer),
        ExprKind::Float(_) => Some(Literal::Float),
        ExprKind::Unary(UnOp::Neg, inner) => literal_kind(inner),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    const V: &str = "struct V { x: f64 }\nimpl V {\n\
        fun mul(self, w: V): V = V { x: self.x * w.x };\n\
        fun mul(self, k: f64): V = V { x: self.x * k };\n\
        fun mul(self, n: u8): V = V { x: self.x * (n as f64) };\n}\n";

    #[test]
    fn the_right_operand_chooses() {
        ok(&format!(
            "{V}fun main() {{ let v = V {{ x: 1.0 }}; let b: u8 = 2;\n\
             let a = v * v; let c = v * 2.0; let d = v * b; let e = v.mul(3.0);\n\
             let mut f = v; f *= 2.0; f *= v; }}"
        ));
    }

    #[test]
    fn an_integer_literal_fitting_two_is_e0457() {
        let src = format!("{V}fun main() {{ let v = V {{ x: 1.0 }}; let a = v * 2; }}");
        assert_eq!(errors(&src), [("E0457", "2")]);
    }

    #[test]
    fn no_fitting_overload_is_e0404() {
        let src = format!("{V}fun main() {{ let v = V {{ x: 1.0 }}; let a = v * \"s\"; }}");
        assert_eq!(errors(&src), [("E0404", "\"s\"")]);
    }

    #[test]
    fn a_second_mul_beside_one_of_another_shape_is_a_duplicate() {
        let src = "struct V { x: f64 }\nimpl V {\n\
                   fun mul(mut self, k: f64) { self.x *= k; }\n\
                   fun mul(self, n: u8): V = self;\n}\nfun main() {}";
        assert_eq!(errors(src), [("E0310", "mul")]);
    }

    #[test]
    fn types_that_collapse_are_e0457_where_used() {
        let src = "struct P<A, B> { a: A, b: B }\nimpl P<A, B> {\n\
                   fun mul(self, a: A): P<A, B> = self;\n\
                   fun mul(self, b: B): P<A, B> = self;\n}\n\
                   fun main() { let p = P { a: 1.0, b: 2.0 }; let x: f64 = 3.0; let q = p * x; }";
        assert_eq!(errors(src), [("E0457", "x")]);
    }

    #[test]
    fn an_interface_takes_the_fitting_one() {
        ok(&format!(
            "{V}interface Scales {{ fun mul(self, k: f64): Self; }}\n\
             fun twice<T: Scales>(s: T): T = s * 2.0;\n\
             fun main() {{ let v = twice(V {{ x: 1.0 }}); }}"
        ));
    }

    #[test]
    fn the_one_on_the_type_satisfies_its_bound() {
        ok(&format!(
            "{V}fun sq<T: Mul>(a: T): T = a * a;\nfun main() {{ let v = sq(V {{ x: 2.0 }}); }}"
        ));
    }
}
