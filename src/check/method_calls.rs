//! Method and associated-function calls (v1 §4, §5): `x.m(args)` and
//! `T.f(args)`, the `mut self` receiver rules, E0415 and E0508.

use super::assign::{PlaceUse, root_name};
use super::builtins::builtin;
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
        expect: Option<&Expect>,
    ) -> Option<Type> {
        let ExprKind::Field(base, name) = &callee.kind else {
            return None;
        };
        // `T.f(…)` through a bound of type parameter `T` (milestone 12).
        if let ExprKind::Name(t) = &base.kind
            && self.local(t).is_none()
            && self.type_params.iter().any(|g| g.name == *t)
        {
            self.record(base, Type::Param(t.clone()));
            self.record(callee, Type::Void);
            return Some(match self.through_bound(call, t, name, false) {
                Some(m) => self.arguments(call, &name.name, &m, args, (&[], expect)),
                None => self.unchecked(args),
            });
        }
        // `Ref.new(v)` (v16): the predeclared box, unless the module has a `Ref`.
        if let ExprKind::Name(t) = &base.kind
            && self.predeclared_ref(t)
        {
            self.record(base, Type::Void);
            self.record(callee, Type::Void);
            return Some(self.ref_new(call, name, args, expect));
        }
        // `Chan.new()` and `Chan.buffered(n)` (v17), unless the module has a `Chan`.
        if let ExprKind::Name(t) = &base.kind
            && t == "Chan"
            && self.predeclared_task(t)
        {
            self.record(base, Type::Void);
            self.record(callee, Type::Void);
            return Some(self.chan_new(call, name, args, expect));
        }
        if let Some(ty) = self.type_named_by(base) {
            let method = self.method(&ty, &name.name);
            if method.is_none() && matches!(ty, Type::Enum(_, _)) {
                return None; // `E.V(..)`: a variant, or E0415 from construction
            }
            self.record(base, Type::Void);
            self.record(callee, Type::Void);
            return Some(self.associated(call, &ty, (name, method), args, expect));
        }
        let mark = self.changes_mark();
        let receiver = self.value(base, None);
        // `b.on_click(2)`: a field holding a function, when no method has that name (v3).
        if let Some(field) = self.fun_field(&receiver, name) {
            self.record(callee, field.clone());
            return Some(self.call_value(call, &name.name, &field, args));
        }
        self.record(callee, Type::Void);
        Some(self.on_value(call, (base, &receiver), name, args, (mark, expect)))
    }

    fn associated(
        &mut self,
        call: &Expr,
        ty: &Type,
        (name, method): (&Ident, Option<Method>),
        args: &[Expr],
        expect: Option<&Expect>,
    ) -> Type {
        match method {
            Some(m) if m.receiver.is_none() => {
                self.hover.function(name.span, m.span, Some(call));
                if let Type::Struct(owner, _) | Type::Enum(owner, _) = ty {
                    self.method_visible(owner, &name.name, name.span);
                    // Codegen finds `T.f` / `m.T.f` by this key (PRP-034).
                    self.callees
                        .insert(call.id, format!("{owner}.{}", name.name));
                }
                self.arguments(call, &name.name, &m, args, (&[], expect))
            }
            found => {
                let help = found.map(|_| format!("call it on a value: `x.{}(…)`", name.name));
                self.report(errors::no_method(&name.name, ty, name.span, help));
                self.unchecked(args)
            }
        }
    }

    pub(super) fn on_value(
        &mut self,
        call: &Expr,
        (base, receiver): (&Expr, &Type),
        name: &Ident,
        args: &[Expr],
        (mark, expect): (usize, Option<&Expect>),
    ) -> Type {
        if matches!(receiver, Type::Error | Type::Never) {
            return self.unchecked(args);
        }
        if let Some(ty) = self.helper_call(call, (receiver, name), args, expect) {
            return ty;
        }
        // `it.map(f)` and the other iterator helpers (v10).
        if let Some(ty) = self.iter_helper_call(call, (base, receiver), name, args, expect) {
            return ty;
        }
        if let Some(m) = builtin(receiver, &name.name) {
            self.unwrap_prints(call, receiver, &name.name);
            // Array built-ins grow the array in place: the `mut self` place rules
            // apply, but the value is read before it changes, so E0508 doesn't
            // (v1 §4). `s.slice(…)` only reads (v4).
            let ret = self.arguments(call, &name.name, &m, args, (&[], expect));
            if m.receiver == Some(true)
                && self.check_place_for(base, PlaceUse::Receiver(&name.name))
            {
                self.no_change_inside(&[base], PlaceUse::Receiver(&name.name), mark);
            }
            return ret;
        }
        let found = match receiver {
            // `x.m(…)` through a bound of `x`'s type parameter (milestone 12).
            Type::Param(p) => match self.through_bound(call, p, name, true) {
                Some(m) => Some(m),
                None => return self.unchecked(args),
            },
            // `s.area()` on an interface value, dispatched at run time (v5).
            Type::Interface(..) => match self.through_interface(
                call,
                &super::applied::Applied::of(receiver).expect("an interface type"),
                name,
            ) {
                Some(m) => Some(m),
                None => return self.unchecked(args),
            },
            _ => self
                .method(receiver, &name.name)
                .filter(|m| m.receiver.is_some()),
        };
        // A default method on a struct or enum without its own (v10).
        let found = match found {
            None => match self.concrete_default(call, receiver, name) {
                Some(None) => return self.unchecked(args),
                found => found.flatten(),
            },
            found => found,
        };
        let Some(m) = found else {
            let help = self
                .method(receiver, &name.name)
                .map(|_| format!("call it on the type: `{receiver}.{}(…)`", name.name))
                .or_else(|| self.default_help(&name.name));
            self.report(errors::no_method(&name.name, receiver, name.span, help));
            return self.unchecked(args);
        };
        self.hover.function(name.span, m.span, Some(call));
        if let Type::Struct(owner, _) | Type::Enum(owner, _) = receiver {
            self.method_visible(owner, &name.name, name.span);
            // A prelude type's method compiles only when reached (v10).
            if m.generics.is_empty() {
                self.used(call, &m.key, &[]);
            }
        }
        // The `impl`'s parameters come from the receiver: `Stack<string>` binds `T`.
        let bound = match receiver {
            Type::Struct(_, targs) | Type::Enum(_, targs) => targs.clone(),
            _ => Vec::new(),
        };
        let ret = self.arguments(call, &name.name, &m, args, (&bound, expect));
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

    /// E0508 (v15, milestone 57): no argument of a `mut self` call may be
    /// the receiver's own storage, a place rooted at the receiver's root
    /// whose type is passed by pointer, directly or as an `if`, block or
    /// `match` tail. Any other mention is a fresh value: a call's result,
    /// an operator's, or a number or string read before the call.
    fn no_alias(&mut self, base: &Expr, args: &[Expr]) {
        let Some(root) = root_name(base) else { return };
        let Some(local) = self.local(root) else {
            return;
        };
        let mut tails = Vec::new();
        args.iter().for_each(|arg| value_tails(arg, &mut tails));
        for tail in tails {
            if root_name(tail) != Some(root) || !self.by_pointer(tail) {
                continue;
            }
            let inside = |at: usize| tail.span.start <= at && at < tail.span.end;
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

    /// Whether `e`'s value is passed as a pointer to its storage: anything
    /// but numbers, `bool`, `char`, strings and function values. A type
    /// parameter may be a struct, so it counts.
    fn by_pointer(&self, e: &Expr) -> bool {
        let ty = self.types[e.id.0 as usize].as_ref();
        ty.is_some_and(|t| {
            matches!(
                t,
                Type::Array(_)
                    | Type::Struct(..)
                    | Type::Enum(..)
                    | Type::Option(_)
                    | Type::Interface(..)
                    | Type::Param(_)
            )
        })
    }
}

/// The expressions whose value `e` may be: `e` itself, or the tails of its
/// `if`/`else` branches, blocks and `match` arms.
fn value_tails<'a>(e: &'a Expr, out: &mut Vec<&'a Expr>) {
    let block = |b: &'a crate::ast::Block, out: &mut Vec<&'a Expr>| {
        if let Some(tail) = &b.tail {
            value_tails(tail, out);
        }
    };
    match &e.kind {
        ExprKind::If { then, else_, .. } => {
            block(then, out);
            if let Some(e) = else_ {
                value_tails(e, out);
            }
        }
        ExprKind::IfLet(parts) => {
            block(&parts.then, out);
            if let Some(e) = &parts.else_ {
                value_tails(e, out);
            }
        }
        ExprKind::Block(b) => block(b, out),
        ExprKind::Match(_, arms) => arms.iter().for_each(|arm| value_tails(&arm.body, out)),
        _ => out.push(e),
    }
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
        // Arrays have only `push`, `pop`, `clear` and the prelude's helpers
        // (`sort` is one since v15).
        assert_eq!(
            errors("fun main() { let mut xs = [1]; xs.shuffle(); }"),
            [("E0415", "shuffle")]
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

    const TAPE: &str = "struct V { x: f64 }
        struct G { vals: f64[], name: string, lr: f64, inner: V, pair: (f64, f64) }
        impl G {
            fun put(mut self, x: f64): i64 { self.vals.push(x); self.vals.len - 1 }
            fun sum(mut self, a: i64, b: i64): i64 = self.put(self.vals[a] + self.vals[b]);
            fun tanh(mut self, a: i64): i64 = self.put(self.vals[a]);
            fun scale(mut self, a: i64, k: f64): i64 = self.put(self.vals[a] * k);
            fun label(mut self, s: string) { self.name = s; }
            fun set(mut self, xs: f64[]) { self.vals = xs; }
            fun take(mut self, v: V) { self.inner = v; }
            fun takep(mut self, p: (f64, f64)) { self.pair = p; }
            fun twin(self): G = self;
            fun absorb(mut self, o: G) { self.lr = o.lr; }
        }
        fun main() { let mut g = G { vals: [], name: \"g\", lr: 0.5, inner: V { x: 1.0 }, pair: (1.0, 2.0) };
            let h = g;";

    #[test]
    fn arguments_may_mention_the_receiver_unless_they_can_be_it() {
        for body in [
            "let a = g.put(1.0); let b = g.tanh(g.sum(a, a));",
            "g.sum(g.put(1.0), g.put(2.0));",
            "g.scale(0, g.lr);",
            "g.label(g.name);",
            "g.label(\"{g.name}!\");",
            "g.absorb(g.twin());",
            "g.take(V { x: g.lr });",
            "g.scale(g.put(g.vals[0] * 2.0), g.inner.x);",
        ] {
            ok(&format!("{TAPE} {body} }}"));
        }
        for body in [
            "g.absorb(g);",
            "g.take(g.inner);",
            "g.set(g.vals);",
            "g.takep(g.pair);",
            "g.absorb(if true { g } else { h });",
            "g.absorb({ g });",
            "g.absorb(match 1 { 0 => g, _ => h });",
        ] {
            assert_eq!(
                errors(&format!("{TAPE} {body} }}")),
                [("E0508", "g")],
                "{body}"
            );
        }
    }
}
