//! Shared boxes (v16 draft decisions 1–4, milestone 67): `Ref<T>`, the one
//! shared, changeable object. `Ref.new(v)`, `get`, `set`, `update` and `is`
//! are its whole API; nothing reaches inside a box in place.

use super::expr::Expect;
use super::{Checker, Type, errors};
use crate::ast::{Expr, Ident};

/// What codegen finds `Ref.new(v)` by in `Checked::callees`.
pub const REF_NEW: &str = "$Ref.new";

impl Checker {
    /// Whether the name `t` in an expression means the predeclared `Ref`:
    /// not a local, and no `Ref` declared in this module.
    pub(super) fn predeclared_ref(&self, t: &str) -> bool {
        t == "Ref" && self.local(t).is_none() && self.named_type(&self.key("Ref")).is_none()
    }

    /// `Ref.new(v)`: a new box holding `v`, a `Ref<T>` for `v`'s `T`. Any
    /// other `Ref.f(…)` is E0415.
    pub(super) fn ref_new(
        &mut self,
        call: &Expr,
        name: &Ident,
        args: &[Expr],
        expect: Option<&Expect>,
    ) -> Type {
        if name.name != "new" {
            let help = Some("a box is made with `Ref.new(value)`".to_string());
            self.report(errors::no_method(
                &name.name,
                &Type::Ref(Box::new(Type::Error)),
                name.span,
                help,
            ));
            return self.unchecked(args);
        }
        let [value] = args else {
            self.report(errors::arity("new", 1, args.len(), call.span));
            return self.unchecked(args);
        };
        // `let r: Ref<f32> = Ref.new(1.5);` types the value from the box.
        let inner = match expect.map(|e| &e.ty) {
            Some(Type::Ref(inner)) => self.expect_type(value, &Expect::of((**inner).clone())),
            _ => self.value(value, None),
        };
        self.callees.insert(call.id, REF_NEW.to_string());
        Type::Ref(Box::new(inner))
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    #[test]
    fn boxes_are_made_read_and_changed_without_mut() {
        assert_eq!(
            let_type("fun main() { let r = Ref.new(1); }", "r"),
            Type::Ref(Box::new(Type::I64))
        );
        ok("fun main() {
            let r = Ref.new(1);
            let a = r.get();
            r.set(2);
            r.update(fun(x: i64): i64 = x + 1);
            let same = r.is(r);
            let s: Ref<string> = Ref.new(\"x\");
        }");
        // A closure may change a box it captured, not the captured name.
        ok(
            "fun main() { let n = Ref.new(0); let tick = fun() { n.update(fun(x: i64): i64 = x + 1); }; }",
        );
        assert_eq!(
            errors("fun main() { let mut n = Ref.new(0); let f = fun() { n = Ref.new(1); }; }")[0]
                .0,
            "E0511"
        );
        assert_eq!(
            errors("fun main() { let n = Ref.new(0); n = Ref.new(1); }"),
            [("E0501", "n")]
        );
    }

    #[test]
    fn boxes_are_not_eq_v16() {
        assert_eq!(
            errors("fun main() { let a = Ref.new(1); let b = a == a; }"),
            [("E0404", "a == a")]
        );
        assert_eq!(
            errors("struct H { r: Ref<i64> }\nfun main() { let h = H { r: Ref.new(1) }; let b = h == h; }")[0].0,
            "E0404"
        );
        assert_eq!(
            errors("fun same<T: Eq>(a: T, b: T): bool = a == b;\nfun main() { let r = Ref.new(1); let b = same(r, r); }")[0].0,
            "E0419"
        );
    }

    #[test]
    fn nothing_reaches_inside_a_box() {
        for (body, code) in [
            ("let x = r.value;", "E0410"),
            ("let x = r[0];", "E0411"),
            ("r.push(1);", "E0415"),
        ] {
            let src = format!("fun main() {{ let r = Ref.new([1]); {body} }}");
            assert_eq!(errors(&src)[0].0, code, "{body}");
        }
    }

    #[test]
    fn recursion_through_a_box_and_shadowing() {
        ok("struct Node { value: i64, next: Option<Ref<Node>> }
            fun main() { let n = Ref.new(Node { value: 1, next: None }); }");
        ok("struct Ref { x: i64 }\nfun main() { let r = Ref { x: 1 }; let y = r.x; }");
        ok(
            "fun bump<T: Num>(r: Ref<T>) { r.update(fun(x: T): T = x + 1); }
            fun main() { bump(Ref.new(1)); bump(Ref.new(1.5)); }",
        );
    }
}
