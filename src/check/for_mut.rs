//! `for x of mut e` (v13 draft §5, milestone 42): over an array, `x` is a
//! mutable alias of each element; over an iterator, the loop calls `next`
//! on `e` itself, so a later loop resumes where this one stopped. Either
//! way `e` must be a place rooted in a `let mut` (E0501, E0502), never a
//! string (E0506), and the body may not change `e` itself (E0507).

use super::assign::root_name;
use super::{Binding, Checker, Type};
use crate::ast::{Block, Expr, ExprKind, Ident};
use crate::diagnostic::Diagnostic;

impl Checker {
    /// `for var of mut iter { body }`.
    pub(super) fn for_of_mut(&mut self, var: &Ident, iter: &Expr, body: &Block) {
        let ty = self.value(iter, None);
        // A map or set is never changed through its loop (v17, milestone 101).
        if super::maps::key_of(&ty).is_some() {
            let d = crate::diagnostic::Diagnostic::error(
                "E0442",
                format!("`for … of mut` can't change a `{ty}` in place"),
                iter.span,
            )
            .with_help("loop over `m.keys()` and assign `m[k] = …`");
            self.report(d);
            self.iterator_loop(var, iter, &ty, body);
            return;
        }
        self.loop_place(iter, &ty);
        let guard = self.place_path(iter);
        if let Some(path) = &guard {
            self.iterating.push((path.clone(), iter.span));
        }
        match ty {
            Type::Array(element) => {
                self.push();
                self.declare(var, *element, Binding::Let { mutable: true });
                self.loops += 1;
                self.block(body, None);
                self.loops -= 1;
                self.pop();
            }
            // Reported by `loop_place`; the body is still checked.
            Type::String | Type::Error | Type::Never => {
                let element = if ty == Type::String {
                    Type::U8
                } else {
                    Type::Error
                };
                self.push();
                self.declare(var, element, Binding::LoopVar);
                self.loops += 1;
                self.block(body, None);
                self.loops -= 1;
                self.pop();
            }
            other => self.iterator_loop(var, iter, &other, body),
        }
        if guard.is_some() {
            self.iterating.pop();
        }
    }

    /// E0506 for a string, E0502 unless `iter` is a name, field or element,
    /// E0501 unless it is rooted in a `let mut` local or `mut self`.
    fn loop_place(&mut self, iter: &Expr, ty: &Type) {
        if *ty == Type::String {
            let d = Diagnostic::error(
                "E0506",
                "a `for … of mut` loop can't change a string",
                iter.span,
            )
            .with_help("strings are immutable: loop with `for b of s` to read its bytes");
            self.report(d);
            return;
        }
        let is_place = matches!(
            iter.kind,
            ExprKind::Name(_) | ExprKind::Field(..) | ExprKind::Index(..)
        );
        let Some(path) = self.place_path(iter).filter(|_| is_place) else {
            if root_name(iter).is_none() || !is_place {
                let d = Diagnostic::error(
                    "E0502",
                    "`for … of mut` needs a variable, field or element",
                    iter.span,
                )
                .with_help("store the value in a `let mut` variable first");
                self.report(d);
            }
            return;
        };
        self.changed(path, iter.span);
        let name = root_name(iter).expect("a place path has a root name");
        if let Some(local) = self.local(name)
            && !matches!(
                local.binding,
                Binding::Let { mutable: true } | Binding::SelfMut
            )
        {
            let d = Diagnostic::error(
                "E0501",
                format!("cannot loop with `mut` over `{name}`: it is not mutable"),
                iter.span,
            )
            .with_label(local.span, "declared here")
            .with_help(format!("declare it with `let mut {name}`"));
            self.report(d);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    const LIB: &str = "struct P { n: i64 }\nimpl P { fun bump(mut self) { self.n += 1; } }\n\
        struct Count { n: i64 }\n\
        impl Count { fun next(mut self): Option<i64> { self.n += 1; Some(self.n) } }\n";

    fn e(body: &str) -> Vec<(&'static str, String)> {
        errors(&format!("{LIB}fun main() {{ {body} }}"))
            .into_iter()
            .map(|(c, t)| (c, t.to_string()))
            .collect()
    }

    #[test]
    fn elements_and_iterators_change_in_place() {
        ok(&format!(
            "{LIB}fun main() {{\n\
             let mut xs = [1, 2]; for x of mut xs {{ x += 1; }}\n\
             let mut ps = [P {{ n: 1 }}]; for p of mut ps {{ p.n = 2; p.bump(); }}\n\
             let mut c = Count {{ n: 0 }}; for n of mut c {{ if n > 3 {{ break; }} }}\n}}"
        ));
    }

    #[test]
    fn misuse_is_reported() {
        assert_eq!(
            e("let xs = [1]; for x of mut xs { }"),
            [("E0501", "xs".to_string())]
        );
        assert_eq!(
            e("for x of mut [1, 2] { }"),
            [("E0502", "[1, 2]".to_string())]
        );
        assert_eq!(
            e("let mut s = \"ab\"; for b of mut s { }"),
            [("E0506", "s".to_string())]
        );
        assert_eq!(
            e("let mut xs = [1]; for x of mut xs { xs.push(1); }")[0].0,
            "E0507"
        );
        assert_eq!(
            e("let mut c = Count { n: 0 }; for n of mut c { let m = c.next(); }")[0].0,
            "E0507"
        );
    }
}
