//! Green threads in the checker (v17 draft decisions 2, 3, 8, 13; milestone
//! 91): `spawn call` is a `Task<T>` (a plain `Task` when the call returns
//! nothing), `Task` and `Chan` are predeclared like `Ref`, and spawning a
//! call that changes its receiver is E0438.

use super::expr::Expect;
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind, spawned_call};
use crate::span::Span;

/// The codes that the closure around a spawned call reports when the call
/// changes its receiver; E0438 says it better.
const COPY_CHANGES: [&str; 4] = ["E0501", "E0502", "E0508", "E0511"];

impl Checker {
    /// Whether `name` in a type or expression means the predeclared `Task`
    /// or `Chan` (v17): no `name` declared in this module, and not a local.
    pub(super) fn predeclared_task(&self, name: &str) -> bool {
        matches!(name, "Task" | "Chan")
            && self.local(name).is_none()
            && self.named_type(&self.key(name)).is_none()
    }

    /// A written `Task`, `Task<T>` or `Chan<T>` (`args` as written); `None`
    /// when `name` is none of them or the module declares its own.
    pub(super) fn resolve_task_type(
        &mut self,
        name: &str,
        args: &[crate::ast::Type],
        span: Span,
    ) -> Option<Type> {
        if !self.predeclared_task(name) {
            return None;
        }
        let args: Vec<Type> = args.iter().map(|a| self.resolve(a)).collect();
        Some(match (name, args.as_slice()) {
            ("Task", []) => Type::Task(Box::new(Type::Unit)),
            ("Task", [t]) => Type::Task(Box::new(t.clone())),
            ("Chan", [t]) => Type::Chan(Box::new(t.clone())),
            _ => {
                self.report(errors::type_args(name, 1, args.len(), span));
                Type::Error
            }
        })
    }

    /// `spawn call`: the closure the parser wrapped the call in, checked
    /// as any closure, makes a `Task` of its result.
    pub(super) fn spawn(&mut self, closure: &Expr, expect: Option<&Expect>) -> Type {
        self.uses_tasks = true;
        self.effect("spawn", "starts a task", closure.span);
        // `let t: Task<u8> = spawn f(1)` types the call as a `u8`.
        let wanted = match expect.map(|e| &e.ty) {
            Some(Type::Task(inner)) if **inner != Type::Unit => {
                Some(Expect::of(Type::Fun(Vec::new(), inner.clone())))
            }
            _ => None,
        };
        let before = self.diagnostics.len();
        let call = spawned_call(closure);
        let outer = self.spawn_operand.replace(call.id);
        let ty = self.expr(closure, wanted.as_ref());
        self.spawn_operand = outer;
        // `spawn Counter { … }` and `spawn Counter.new(…)` start an actor (v17).
        if let Type::Fun(_, ret) = &ty
            && let Some(handle) = self.spawned_actor(call, ret)
        {
            return handle;
        }
        if let Some(span) = self.changes_receiver(call) {
            // The closure's own complaint (a captured variable changed, or
            // a temporary) is about the wrapper the user never wrote.
            let inside = |d: &crate::diagnostic::Diagnostic| {
                COPY_CHANGES.contains(&d.code)
                    && d.span.start >= call.span.start
                    && d.span.end <= call.span.end
            };
            let mut index = before;
            while index < self.diagnostics.len() {
                if inside(&self.diagnostics[index]) {
                    self.diagnostics.remove(index);
                } else {
                    index += 1;
                }
            }
            self.report(errors::spawn_changes_copy(span));
        }
        match ty {
            Type::Fun(_, ret) if *ret == Type::Never => Type::Task(Box::new(Type::Unit)),
            Type::Fun(_, ret) => Type::Task(ret),
            _ => Type::Error,
        }
    }

    /// The call's span if it changes its receiver: `push`, `pop` or
    /// `clear` on an array, a `mut self` method, or `swap`.
    fn changes_receiver(&self, call: &Expr) -> Option<Span> {
        let ExprKind::Call(callee, _) = &call.kind else {
            return None;
        };
        let changes = match &callee.kind {
            ExprKind::Name(name) => name == "swap" && self.local(name).is_none(),
            ExprKind::Field(base, name) => match self.types[base.id.0 as usize].as_ref() {
                Some(Type::Array(_)) => matches!(name.name.as_str(), "push" | "pop" | "clear"),
                Some(ty) => self
                    .method(ty, &name.name)
                    .is_some_and(|m| m.receiver == Some(true)),
                None => false,
            },
            _ => false,
        };
        changes.then_some(call.span)
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    const WORK: &str = "fun work(n: i64): i64 = n * n;\n";

    #[test]
    fn spawn_gives_a_task_of_the_calls_result() {
        let src = format!("{WORK}fun main() {{ let t = spawn work(3); let n = t.join(); }}");
        assert_eq!(let_type(&src, "t"), Type::Task(Box::new(Type::I64)));
        assert_eq!(let_type(&src, "n"), Type::I64);
        let void = "fun log(s: string) { println(s); }\nfun main() { let t = spawn log(\"x\"); t.join(); }";
        assert_eq!(let_type(void, "t"), Type::Task(Box::new(Type::Unit)));
        ok(
            "fun main() { let k = 2; let t = spawn fun(): i64 { k * 10 }(); let n: i64 = t.join(); }",
        );
        ok("fun main() { let t: Task = spawn println(\"hi\"); t.join(); }");
        ok(&format!(
            "{WORK}fun main() {{ let t: Task<i64> = spawn work(1); }}"
        ));
    }

    #[test]
    fn spawn_takes_any_call() {
        ok("struct P { x: i64 }
            impl P { fun area(self): i64 = self.x; fun make(): P = P { x: 1 }; }
            fun main() {
                let p = P.make();
                let a = spawn p.area();
                let b = spawn P.make();
                let c = Ref.new(0);
                let d = spawn fun() { c.update(fun(n: i64): i64 = n + 1); }();
                let fs = [fun(): i64 = 1];
                let e = spawn fs[0]();
            }");
    }

    #[test]
    fn the_callee_types_the_arguments() {
        ok(
            "fun f(x: u8) {}\nfun main() { spawn f(1); let t: Task<u8> = spawn fun(): u8 { 2 }(); }",
        );
    }

    #[test]
    fn joining_a_plain_task_gives_unit_v17() {
        assert_eq!(
            crate::check::test_util::warnings(
                "fun main() { let t = spawn println(1); let x = t.join(); }"
            ),
            [("W0106", "x")]
        );
    }

    #[test]
    fn tasks_are_not_eq() {
        let src = format!("{WORK}fun main() {{ let t = spawn work(1); let b = t == t; }}");
        assert_eq!(errors(&src), [("E0404", "t == t")]);
        let src = format!(
            "{WORK}fun same<T: Eq>(a: T, b: T): bool = a == b;\nfun main() {{ let t = spawn work(1); let b = same(t, t); }}"
        );
        assert_eq!(errors(&src)[0].0, "E0419");
    }

    #[test]
    fn spawning_a_change_to_a_copy_is_e0438() {
        assert_eq!(
            errors("fun main() { let mut xs = [1]; spawn xs.push(2); }"),
            [("E0438", "xs.push(2)")]
        );
        assert_eq!(
            errors(
                "struct P { x: i64 }\nimpl P { fun bump(mut self) { self.x += 1; } }
                fun main() { let mut p = P { x: 1 }; spawn p.bump(); }"
            ),
            [("E0438", "p.bump()")]
        );
        assert_eq!(
            errors("fun main() { let mut a = 1; let mut b = 2; spawn swap(a, b); }"),
            [("E0438", "swap(a, b)")]
        );
    }

    #[test]
    fn written_task_and_chan_types() {
        ok("fun f(t: Task<i64>, u: Task, c: Chan<string>) {}\nfun main() {}");
        assert_eq!(
            errors("fun f(c: Chan) {}\nfun main() {}"),
            [("E0418", "Chan")]
        );
        assert_eq!(
            errors("fun f(t: Task<i64, i64>) {}\nfun main() {}"),
            [("E0418", "Task<i64, i64>")]
        );
        // A module's own `Task` and `Chan` win, as for `Ref`.
        ok("struct Task { x: i64 }\nstruct Chan<T> { v: T }
            fun main() { let t = Task { x: 1 }; let c = Chan { v: 2 }; }");
    }
}
