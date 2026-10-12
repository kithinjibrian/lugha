//! Method and associated-function calls (v1 §4, §5): `x.m(args)` and
//! `T.f(args)`, the `mut self` receiver rules, E0415 and E0508.

use super::assign::{PlaceUse, root_name};
use super::expr::Expect;
use super::methods::Method;
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind, Ident};
use crate::span::Span;

impl Checker {
    /// `T.f(args)` or `x.m(args)`. `None` if `callee` is a variant constructor
    /// or not a member call at all, so construction and plain calls handle it.
    pub(super) fn method_call(
        &mut self,
        call: &Expr,
        callee: &Expr,
        args: &[Expr],
    ) -> Option<Type> {
        let ExprKind::Field(base, name) = &callee.kind else {
            return None;
        };
        if let ExprKind::Name(t) = &base.kind
            && self.local(t).is_none()
            && let Some(ty) = self.named_type(t)
        {
            let method = self.method(&ty, &name.name);
            if method.is_none() && matches!(ty, Type::Enum(_)) {
                return None; // `E.V(..)`: a variant, or E0415 from construction
            }
            self.record(base, Type::Void);
            self.record(callee, Type::Void);
            return Some(self.associated(call, &ty, name, method, args));
        }
        let mark = self.changes_mark();
        let receiver = self.value(base, None);
        self.record(callee, Type::Void);
        Some(self.on_value(call, base, &receiver, name, args, mark))
    }

    fn associated(
        &mut self,
        call: &Expr,
        ty: &Type,
        name: &Ident,
        method: Option<Method>,
        args: &[Expr],
    ) -> Type {
        match method {
            Some(m) if m.receiver.is_none() => self.arguments(call, &name.name, &m, args),
            found => {
                let help = found.map(|_| format!("call it on a value: `x.{}(…)`", name.name));
                self.report(errors::no_method(&name.name, ty, name.span, help));
                self.unchecked(args)
            }
        }
    }

    fn on_value(
        &mut self,
        call: &Expr,
        base: &Expr,
        receiver: &Type,
        name: &Ident,
        args: &[Expr],
        mark: usize,
    ) -> Type {
        if matches!(receiver, Type::Error | Type::Never) {
            return self.unchecked(args);
        }
        if let Some(m) = builtin(receiver, &name.name) {
            // Built-ins grow the array in place: the `mut self` place rules
            // apply, but the value is read before the array changes, so
            // E0508 doesn't (v1 §4).
            let ret = self.arguments(call, &name.name, &m, args);
            if self.check_place_for(base, PlaceUse::Receiver(&name.name)) {
                self.no_change_inside(&[base], PlaceUse::Receiver(&name.name), mark);
            }
            return ret;
        }
        let Some(m) = self
            .method(receiver, &name.name)
            .filter(|m| m.receiver.is_some())
        else {
            let help = self
                .method(receiver, &name.name)
                .map(|_| format!("call it on the type: `{receiver}.{}(…)`", name.name));
            self.report(errors::no_method(&name.name, receiver, name.span, help));
            return self.unchecked(args);
        };
        let ret = self.arguments(call, &name.name, &m, args);
        let receiver = PlaceUse::Receiver(&name.name);
        // E0510 explains a change to the root better than E0508's "mentions it".
        if m.receiver == Some(true)
            && self.check_place_for(base, receiver)
            && !self.no_change_inside(&[base], receiver, mark)
        {
            self.no_alias(base, args);
        }
        ret
    }

    /// E0508: no argument of a `mut self` call may mention its receiver's root.
    fn no_alias(&mut self, base: &Expr, args: &[Expr]) {
        let Some(root) = root_name(base) else { return };
        let Some(local) = self.local(root) else {
            return;
        };
        for arg in args {
            let inside = |at: usize| arg.span.start <= at && at < arg.span.end;
            if let Some(m) = self
                .mentions
                .values()
                .find(|m| m.decl == local.span && inside(m.at))
            {
                let at = Span::new(m.at, m.at + root.len());
                self.report(errors::mut_self_alias(root, at, base.span));
                return;
            }
        }
    }

    fn arguments(&mut self, call: &Expr, name: &str, m: &Method, args: &[Expr]) -> Type {
        if args.len() != m.params.len() {
            self.report(errors::arity(name, m.params.len(), args.len(), call.span));
            self.unchecked(args);
            return m.ret.clone();
        }
        for (arg, ty) in args.iter().zip(&m.params) {
            self.expect_type(arg, &Expect::of(ty.clone()));
        }
        m.ret.clone()
    }

    fn unchecked(&mut self, args: &[Expr]) -> Type {
        for arg in args {
            self.expr(arg, None);
        }
        Type::Error
    }
}

/// `push`, `pop` and `clear` on an array (v1 §4, milestone 8).
fn builtin(receiver: &Type, name: &str) -> Option<Method> {
    let Type::Array(element) = receiver else {
        return None;
    };
    let (params, ret) = match name {
        "push" => (vec![(**element).clone()], Type::Void),
        "pop" => (Vec::new(), Type::Option(element.clone())),
        "clear" => (Vec::new(), Type::Void),
        _ => return None,
    };
    Some(Method {
        receiver: Some(true),
        params,
        ret,
        span: Span::new(0, 0),
    })
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    const V: &str = "struct V { x: f64, y: f64 }\n";

    #[test]
    fn calls_need_the_right_kind_of_method() {
        let base = format!(
            "{V}impl V {{ fun new(): V = V {{ x: 0.0, y: 0.0 }}; fun len(self): f64 = self.x; }}\n"
        );
        assert_eq!(
            errors(&format!(
                "{base}fun main() {{ let v = V.new(); let n = v.size(); }}"
            )),
            [("E0415", "size")]
        );
        assert_eq!(
            errors(&format!(
                "{base}fun main() {{ let v = V.new(); let w = v.new(); }}"
            )),
            [("E0415", "new")]
        );
        assert_eq!(
            errors(&format!("{base}fun main() {{ let n = V.len(); }}")),
            [("E0415", "len")]
        );
        // Arrays have only `push`, `pop` and `clear` (milestone 8).
        assert_eq!(
            errors("fun main() { let mut xs = [1]; xs.sort(); }"),
            [("E0415", "sort")]
        );
        assert_eq!(
            errors(&format!(
                "{base}fun main() {{ let v = V.new(); let n = v.len(1); }}"
            )),
            [("E0405", "v.len(1)")]
        );
    }

    #[test]
    fn receivers_follow_the_mutability_rules() {
        let base = format!(
            "{V}impl V {{ fun bump(mut self) {{ self.x += 1.0; }} fun set(self) {{ self.x = 1.0; }} fun add(mut self, o: V) {{ self.x += o.x; }} }}\n"
        );
        assert_eq!(
            errors(&format!("{base}fun main() {{}}")),
            [("E0501", "self.x")]
        );
        let calls = format!(
            "{V}impl V {{ fun bump(mut self) {{ self.x += 1.0; }} fun add(mut self, o: V) {{ self.x += o.x; }} fun reset(mut self) {{ self = V {{ x: 0.0, y: 0.0 }}; }} }}\n"
        );
        ok(&format!(
            "{calls}fun main() {{ let mut v = V {{ x: 1.0, y: 1.0 }}; let w = v; v.bump(); v.add(w); v.reset(); }}"
        ));
        assert_eq!(
            errors(&format!(
                "{calls}fun main() {{ let v = V {{ x: 1.0, y: 1.0 }}; v.bump(); }}"
            )),
            [("E0501", "v")]
        );
        assert_eq!(
            errors(&format!(
                "{calls}fun mk(): V = V {{ x: 1.0, y: 1.0 }};\nfun main() {{ mk().bump(); }}"
            )),
            [("E0502", "mk()")]
        );
        assert_eq!(
            errors(&format!(
                "{calls}fun main() {{ let mut v = V {{ x: 1.0, y: 1.0 }}; v.add(v); }}"
            )),
            [("E0508", "v")]
        );
        let field = format!(
            "{calls}struct R {{ a: V, b: V }}\nfun main() {{ let mut r = R {{ a: V {{ x: 1.0, y: 1.0 }}, b: V {{ x: 1.0, y: 1.0 }} }}; r.a.add(r.b); }}"
        );
        assert_eq!(errors(&field), [("E0508", "r")]);
        let iter = format!(
            "{calls}fun main() {{ let mut vs = [V {{ x: 1.0, y: 1.0 }}]; for v of vs {{ vs[0].bump(); }} }}"
        );
        assert_eq!(errors(&iter), [("E0507", "vs[0]")]);
        assert_eq!(
            errors("fun f() { let s = self; }\nfun main() {}"),
            [("E0301", "self")]
        );
    }

    #[test]
    fn arrays_grow_through_push_pop_and_clear() {
        use crate::check::Type;
        use crate::check::test_util::let_type;
        let src = "fun main() { let mut xs = [1]; xs.push(2); let p = xs.pop(); xs.clear(); }";
        assert_eq!(let_type(src, "p"), Type::Option(Box::new(Type::I64)));
        ok("fun main() { let mut xs = [1]; xs.push(xs[0]); let mut g = [[1]]; g.push(g[0]); }");
        assert_eq!(
            errors("fun main() { let mut xs = [1]; xs.push(2.5); }"),
            [("E0401", "2.5")]
        );
        assert_eq!(
            errors("fun main() { let mut xs = [1]; xs.push(); }"),
            [("E0405", "xs.push()")]
        );
        assert_eq!(
            errors("fun main() { let mut xs = [1]; xs.insert(0); }"),
            [("E0415", "insert")]
        );
        assert_eq!(
            errors("fun main() { let mut s = \"a\"; s.push(1); }"),
            [("E0415", "push")]
        );
        let d = crate::check::test_util::diagnostics("fun main() { let xs = [1]; xs.push(2); }");
        assert_eq!(
            (d[0].code, d[0].message.as_str()),
            (
                "E0501",
                "cannot call `mut self` method `push` on `xs`: it is not mutable"
            )
        );
        assert_eq!(
            errors("fun main() { let mut xs = [1]; for x of xs { xs.push(x); } }"),
            [("E0507", "xs")]
        );
    }
}
