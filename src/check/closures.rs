//! Closures (v3 draft §4, milestone 15): `fun(x) = e` literals, their
//! parameter and return types, and the variables they capture by copy.
//! A closure body is its own function: `return` leaves it, and loops
//! outside it are out of reach (PRP-040).

use super::expr::Expect;
use super::{Binding, Checker, Type, errors};
use crate::ast::{Closure, Expr};

/// A closure whose body is being checked.
#[derive(Debug, Default)]
pub(super) struct Frame {
    /// How many scopes were open outside it: a local found below this
    /// depth is a capture.
    base: usize,
    /// Captured names, in order of first mention.
    captures: Vec<String>,
    /// True when its return type is unknown until the body is checked.
    infer_ret: bool,
}

impl Checker {
    /// A closure literal: its function type, after checking its body.
    pub(super) fn closure(&mut self, expr: &Expr, c: &Closure, expect: Option<&Expect>) -> Type {
        let expected = match expect.map(|e| &e.ty) {
            Some(Type::Fun(ps, r)) if ps.len() == c.params.len() => {
                Some((ps.clone(), (**r).clone()))
            }
            _ => None,
        };
        let mut params = Vec::new();
        for (i, (name, written)) in c.params.iter().enumerate() {
            let ty = match (written, &expected) {
                (Some(t), _) => self.resolve(t),
                (None, Some((ps, _))) if ps[i] != Type::Error => ps[i].clone(),
                (None, _) => {
                    self.report(errors::cannot_infer_param(&name.name, name.span));
                    Type::Error
                }
            };
            params.push(ty);
        }
        let ret = match (&c.ret, &expected) {
            (Some(t), _) => Some(self.resolve(t)),
            (None, Some((_, r))) if *r != Type::Error => Some(r.clone()),
            _ => None,
        };
        let saved = (self.ret.clone(), self.loops, self.dead);
        let saved_changes = std::mem::take(&mut self.changes);
        let saved_iterating = std::mem::take(&mut self.iterating);
        self.closures.push(Frame {
            base: self.scopes.len(),
            captures: Vec::new(),
            infer_ret: ret.is_none(),
        });
        (self.ret, self.loops, self.dead) = (ret.clone().unwrap_or(Type::Error), 0, false);
        self.push();
        for ((name, _), ty) in c.params.iter().zip(&params) {
            self.declare(name, ty.clone(), Binding::Param);
        }
        let body_expect = ret.clone().filter(|r| *r != Type::Void).map(Expect::of);
        let body = self.block(&c.body, body_expect.as_ref());
        self.pop();
        let frame = self.closures.pop().expect("pushed above");
        (self.ret, self.loops, self.dead) = saved;
        self.changes = saved_changes;
        self.iterating = saved_iterating;
        self.captures.insert(expr.id, frame.captures);
        let ret = match ret {
            Some(r) => {
                if r != Type::Void && !body.fits(&r) {
                    let span = c.body.tail.as_ref().map_or(c.body.span, |t| t.span);
                    self.report(errors::mismatch(r.clone(), body, span, None));
                }
                r
            }
            // A body that never finishes returns nothing.
            None if body == Type::Never => Type::Void,
            None => body,
        };
        Type::Fun(params, Box::new(ret))
    }

    /// Records a mention of local `name`: a capture of every closure being
    /// checked that it lies outside. True if the innermost one captures it.
    pub(super) fn note_capture(&mut self, name: &str) -> bool {
        let Some(depth) = self.scopes.iter().rposition(|s| s.contains_key(name)) else {
            return false;
        };
        for frame in self.closures.iter_mut().filter(|f| depth < f.base) {
            if !frame.captures.iter().any(|c| c == name) {
                frame.captures.push(name.to_string());
            }
        }
        self.closures.last().is_some_and(|f| depth < f.base)
    }

    /// True if `name` is captured by the closure being checked (E0511).
    pub(super) fn captured(&self, name: &str) -> bool {
        let depth = self.scopes.iter().rposition(|s| s.contains_key(name));
        let base = self.closures.last().map(|f| f.base);
        matches!((depth, base), (Some(d), Some(b)) if d < b)
    }

    /// E0412 for `return` in a closure whose return type isn't known yet.
    /// True if reported, so the caller just checks the value for its own errors.
    pub(super) fn closure_return_unknown(&mut self, span: crate::span::Span) -> bool {
        if !self.closures.last().is_some_and(|f| f.infer_ret) {
            return false;
        }
        self.report(errors::cannot_infer_return(span));
        true
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    fn ty(body: &str, name: &str) -> Type {
        let_type(&format!("fun main() {{ {body} }}"), name)
    }

    fn e(src: &str) -> Vec<&'static str> {
        errors(src).into_iter().map(|(c, _)| c).collect()
    }

    fn fun(params: Vec<Type>, ret: Type) -> Type {
        Type::Fun(params, Box::new(ret))
    }

    #[test]
    fn closures_have_function_types() {
        assert_eq!(
            ty("let f = fun(x: i64): i64 = x * 2;", "f"),
            fun(vec![Type::I64], Type::I64)
        );
        assert_eq!(
            ty("let g: fun(i64): i64 = fun(x) = x + 1;", "g"),
            fun(vec![Type::I64], Type::I64)
        );
        assert_eq!(
            ty("let h = fun(x: i64) = x > 1;", "h"),
            fun(vec![Type::I64], Type::Bool)
        );
        assert_eq!(ty("let k = fun() {};", "k"), fun(Vec::new(), Type::Void));
    }

    #[test]
    fn unknown_types_are_e0412() {
        assert_eq!(e("fun main() { let h = fun(x) = x; }"), ["E0412"]);
        assert_eq!(
            e("fun main() { let h = fun(x: i64) { if x > 0 { return 1; } 2 }; }"),
            ["E0412"]
        );
        ok("fun main() { let h = fun(x: i64): i64 { if x > 0 { return 1; } 2 }; }");
    }

    #[test]
    fn a_closure_body_is_its_own_function() {
        assert_eq!(
            e("fun main() { let h = fun(x: i64): i64 { return true; }; }"),
            ["E0403"]
        );
        assert_eq!(
            e("fun main() { while true { let f = fun() { break; }; } }"),
            ["E0504"]
        );
        ok("fun f(): i64 { let g = fun(x: i64): i64 { return x; }; g(1) }\nfun main() {}");
    }

    #[test]
    fn captures_are_read_only_copies() {
        ok("fun main() { let mut count = 0; let show = fun(): i64 = count + 1; count = 5; }");
        let cases = [
            "let mut count = 0; let f = fun() { count += 1; };",
            "let mut p = P { x: 1 }; let f = fun() { p.x = 2; };",
            "let mut xs = [1]; let f = fun() { xs[0] = 2; };",
            "let mut xs = [1]; let f = fun() { xs.push(2); };",
            "let mut a = 1; let mut b = 2; let f = fun() { swap(a, b); };",
        ];
        for body in cases {
            let src = format!("struct P {{ x: i64 }}\nfun main() {{ {body} }}");
            assert!(e(&src).contains(&"E0511"), "{body}: {:?}", e(&src));
        }
        ok("fun main() { let f = fun(): i64 { let mut n = 1; n += 1; n }; }");
    }

    #[test]
    fn captures_are_recorded_in_order() {
        let checked = ok(
            "fun main() { let a = 1; let b = 2; let f = fun(x: i64): i64 = b + a + b + x; \
                          let g = fun(): i64 { let h = fun(): i64 = a; h() }; }",
        );
        let mut all: Vec<Vec<String>> = checked.captures.values().cloned().collect();
        all.sort();
        assert_eq!(
            all,
            [
                vec!["a".to_string()],
                vec!["a".to_string()],
                vec!["b".to_string(), "a".to_string()]
            ]
        );
    }

    #[test]
    fn closures_meet_generics() {
        let apply = "fun apply<T>(f: fun(T): T, x: T): T = f(x);\n";
        assert_eq!(
            let_type(
                &format!("{apply}fun main() {{ let n = apply(fun(x) = x + 1, 41); }}"),
                "n"
            ),
            Type::I64
        );
        ok(
            "fun twice<T>(x: T): T[] { let f = fun(): T = x; [f(), f()] }\n\
            fun main() { let a = twice(1); let b = twice(\"s\"); }",
        );
    }
}
