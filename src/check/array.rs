//! Arrays: list and repeat literals, `for x of xs`, and the place paths
//! behind the "don't assign to what you're iterating" rule (spec §4, §5).

use super::expr::Expect;
use super::{Binding, Checker, Checking, Type, errors};
use crate::ast::{Block, Expr, ExprKind, Ident};
use crate::span::Span;

/// One step from a variable into its contents.
#[derive(Debug, Clone, PartialEq)]
enum Step {
    Index,
    Field(String),
}

/// A place as a root binding (name and declaration span, so shadowed names
/// differ) plus the steps taken into it.
#[derive(Debug, Clone, PartialEq)]
pub(super) struct Path {
    root: (String, Span),
    steps: Vec<Step>,
}

impl Path {
    /// True if assigning through `self` might change part of `other` or vice
    /// versa: same root, and every shared step could be the same (two index
    /// steps always might be; field steps must match).
    pub(super) fn overlaps(&self, other: &Path) -> bool {
        self.root == other.root
            && self.steps.iter().zip(&other.steps).all(|pair| match pair {
                (Step::Index, Step::Index) => true,
                (Step::Field(a), Step::Field(b)) => a == b,
                _ => false,
            })
    }
}

impl Checker {
    /// `[a, b, c]`: the expected element type, or the first element's (spec §4).
    pub(super) fn array_literal(
        &mut self,
        expr: &Expr,
        elements: &[Expr],
        expect: Option<&Expect>,
    ) -> Checking<Type> {
        let expected = match expect.map(|e| &e.ty) {
            Some(Type::Array(element)) => Some((**element).clone()),
            _ => None,
        };
        let Some(first) = elements.first() else {
            return Ok(match expected {
                Some(element) => Type::Array(Box::new(element)),
                None => {
                    self.report(errors::empty_array(expr.span));
                    Type::Error
                }
            });
        };
        let element = match expected {
            Some(element) => {
                for e in elements {
                    self.expect_type(e, &Expect::of(element.clone()))?;
                }
                element
            }
            None => {
                let ty = match self.value(first, None)? {
                    Type::Never => Type::Error,
                    ty => ty,
                };
                let why = format!("the first element is {ty}");
                for e in &elements[1..] {
                    self.expect_type(e, &Expect::because(ty.clone(), first.span, why.clone()))?;
                }
                ty
            }
        };
        Ok(if element == Type::Error {
            Type::Error
        } else {
            Type::Array(Box::new(element))
        })
    }

    /// `[value; count]`: `value` first, then an `i64` count (spec §4, §5).
    pub(super) fn repeat(
        &mut self,
        value: &Expr,
        count: &Expr,
        expect: Option<&Expect>,
    ) -> Checking<Type> {
        let element = match expect.map(|e| &e.ty) {
            Some(Type::Array(element)) => {
                self.expect_type(value, &Expect::of((**element).clone()))?;
                (**element).clone()
            }
            _ => self.value(value, None)?,
        };
        self.expect_type(count, &Expect::of(Type::I64))?;
        Ok(match element {
            Type::Error | Type::Never => Type::Error,
            element => Type::Array(Box::new(element)),
        })
    }

    /// `for var of iter { body }`: `iter` is an array, `var` an immutable
    /// element, and the body may not assign to `iter` (spec §5, E0507).
    pub(super) fn for_of(&mut self, var: &Ident, iter: &Expr, body: &Block) -> Checking<()> {
        let element = match self.value(iter, None)? {
            Type::Array(element) => *element,
            Type::Error | Type::Never => Type::Error,
            other => {
                self.report(errors::not_iterable(&other, iter.span));
                Type::Error
            }
        };
        let guard = self.place_path(iter);
        if let Some(path) = &guard {
            self.iterating.push((path.clone(), iter.span));
        }
        self.push();
        self.declare(var, element, Binding::LoopVar);
        self.loops += 1;
        self.block(body, None)?;
        self.loops -= 1;
        self.pop();
        if guard.is_some() {
            self.iterating.pop();
        }
        Ok(())
    }

    /// The path of a place expression rooted in a local, if `expr` is one.
    pub(super) fn place_path(&self, expr: &Expr) -> Option<Path> {
        match &expr.kind {
            ExprKind::Name(name) => {
                let local = self.local(name)?;
                Some(Path {
                    root: (name.clone(), local.span),
                    steps: Vec::new(),
                })
            }
            ExprKind::Index(base, _, _) => {
                let mut path = self.place_path(base)?;
                path.steps.push(Step::Index);
                Some(path)
            }
            ExprKind::Field(base, field) => {
                let mut path = self.place_path(base)?;
                path.steps.push(Step::Field(field.name.clone()));
                Some(path)
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    fn array(element: Type) -> Type {
        Type::Array(Box::new(element))
    }

    #[test]
    fn literals_take_the_expected_or_first_element_type() {
        assert_eq!(
            let_type("fun main() { let xs = [1, 2]; }", "xs"),
            array(Type::I64)
        );
        assert_eq!(
            let_type("fun main() { let xs: u8[] = [1, 255]; }", "xs"),
            array(Type::U8)
        );
        assert_eq!(
            let_type("fun main() { let g = [[1], [2, 3]]; }", "g"),
            array(array(Type::I64))
        );
        assert_eq!(
            let_type("fun main() { let r = [false; 3]; }", "r"),
            array(Type::Bool)
        );
        assert_eq!(
            let_type("fun main() { let e: string[] = []; }", "e"),
            array(Type::String)
        );
        assert_eq!(
            errors("fun main() { let xs = [1, true]; }"),
            [("E0403", "true")]
        );
        assert_eq!(errors("fun main() { let xs = []; }"), [("E0412", "[]")]);
        assert_eq!(
            errors("fun main() { let xs = [0; 2.5]; }"),
            [("E0401", "2.5")]
        );
    }

    #[test]
    fn arrays_have_len_elements_and_no_equality() {
        assert_eq!(
            let_type("fun main() { let xs = [1.5]; let x = xs[0]; }", "x"),
            Type::F64
        );
        assert_eq!(
            let_type("fun main() { let xs = [1]; let n = xs.len; }", "n"),
            Type::I64
        );
        assert_eq!(
            errors("fun main() { let a = [1]; let b = [1]; let e = a == b; }"),
            [("E0404", "a == b")]
        );
        assert_eq!(
            errors("fun main() { let a = [1]; let n = a.size; }"),
            [("E0410", "size")]
        );
    }

    #[test]
    fn element_assignment_needs_a_mutable_root() {
        ok(
            "fun main() { let mut xs = [1, 2]; xs[0] = 3; xs[1] += 1; let mut g = [[1]]; g[0][0] = 2; }",
        );
        assert_eq!(
            errors("fun main() { let xs = [1, 2]; xs[0] = 5; }"),
            [("E0501", "xs[0]")]
        );
        assert_eq!(
            errors("fun f(p: i64[]) { p[0] = 1; }\nfun main() {}"),
            [("E0501", "p[0]")]
        );
        assert_eq!(
            errors("fun main() { let mut xs = [1]; xs[0] = true; }"),
            [("E0403", "true")]
        );
        assert_eq!(
            errors("fun main() { let mut xs = [1]; xs.len = 2; }"),
            [("E0502", "xs.len")]
        );
        assert_eq!(
            errors("fun main() { let mut s = [\"ab\"]; s[0][0] = 1; }"),
            [("E0506", "s[0][0]")]
        );
    }

    #[test]
    fn for_of_iterates_arrays_and_guards_them() {
        ok("fun main() { let xs = [1, 2]; let mut t = 0; for x of xs { t += x; } }");
        assert_eq!(
            errors("fun main() { for c of \"abc\" { } }"),
            [("E0411", "\"abc\"")]
        );
        assert_eq!(
            errors("fun main() { let mut xs = [1]; for x of xs { xs[0] = x; } }"),
            [("E0507", "xs[0]")]
        );
        assert_eq!(
            errors("fun main() { let mut xs = [1]; for x of xs { xs = [2]; } }"),
            [("E0507", "xs")]
        );
        let grid = "fun main() { let mut g = [[1, 2], [3]]; for x of g[0] { g[1][0] = x; } }";
        // Index steps might be equal, so `g[1][0]` may overlap `g[0]`: rejected conservatively.
        assert_eq!(errors(grid), [("E0507", "g[1][0]")]);
        ok("fun main() { let mut a = [1]; let b = [2]; for x of b { a[0] = x; } }");
        ok("fun main() { let mut xs = [1]; for x of xs { let mut xs = [5]; xs[0] = x; } }");
        assert_eq!(
            errors("fun main() { let mut xs = [1]; for x of xs { x = 2; } }"),
            [("E0501", "x")]
        );
    }

    #[test]
    fn arrays_cant_cross_into_c() {
        assert_eq!(
            errors("extern fun f(a: i64[]);\nfun main() {}"),
            [("E0409", "i64[]")]
        );
    }
}
