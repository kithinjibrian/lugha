//! `select` in the checker (v17 draft decision 7, milestone 91): each arm
//! waits for a channel's `recv` or `send`, a timeout or nothing; its value
//! agrees with the others' as in `match`. E0438 for the arms a `select`
//! may not have, or lacks.

use super::expr::Expect;
use super::{Binding, Checker, Type, errors};
use crate::ast::{Expr, SelectArm, SelectKind};
use crate::span::Span;

impl Checker {
    /// A `select`: its arms in order, then their common type.
    pub(super) fn select(
        &mut self,
        expr: &Expr,
        arms: &[SelectArm],
        expect: Option<&Expect>,
    ) -> Type {
        self.select_shape(expr, arms);
        let mut result: Option<(Type, Span)> = None;
        for arm in arms {
            self.push();
            match &arm.kind {
                SelectKind::Recv { binding, call } => {
                    let ty = self.value(call, None);
                    self.on_channel(&arm.kind, "recv");
                    if binding.name != "_" {
                        self.declare(binding, ty, Binding::Pattern);
                    }
                }
                SelectKind::Send(call) => {
                    self.expr(call, None);
                    self.on_channel(&arm.kind, "send");
                }
                SelectKind::After(ms) => {
                    self.expect_type(ms, &Expect::of(Type::I64));
                }
                SelectKind::Default(_) => {}
            }
            let ty = self.arm_body(&arm.body, &result, expect);
            self.pop();
            if result.is_none() && ty != Type::Never {
                result = Some((ty, arm.body.span));
            }
        }
        result.map_or(Type::Never, |(ty, _)| ty)
    }

    /// E0438 when a `recv` or `send` arm isn't on a channel.
    fn on_channel(&mut self, kind: &SelectKind, method: &str) {
        let Some(chan) = kind.channel() else {
            return;
        };
        let ty = self.types[chan.id.0 as usize]
            .clone()
            .unwrap_or(Type::Error);
        if !matches!(ty, Type::Chan(_) | Type::Error) {
            let message = format!("`{method}` in a `select` arm needs a channel, found `{ty}`");
            self.report(errors::bad_select(message, chan.span));
        }
    }

    /// E0438: no `recv` or `send` arm, a second `default` or `after`, or
    /// both (the timeout could never fire).
    fn select_shape(&mut self, expr: &Expr, arms: &[SelectArm]) {
        let keyword = |at: usize, word: &str| Span::new(at, at + word.len());
        let waits = arms
            .iter()
            .any(|a| matches!(a.kind, SelectKind::Recv { .. } | SelectKind::Send(_)));
        if !waits {
            let message = "a `select` needs a `recv` or `send` arm";
            self.report(errors::bad_select(
                message,
                keyword(expr.span.start, "select"),
            ));
        }
        let mut default = false;
        let mut after = false;
        for arm in arms {
            let (word, seen) = match arm.kind {
                SelectKind::Default(_) => ("default", &mut default),
                SelectKind::After(_) => ("after", &mut after),
                _ => continue,
            };
            let at = keyword(arm.span.start, word);
            if *seen {
                let message = format!("a `select` has at most one `{word}` arm");
                self.report(errors::bad_select(message, at));
            }
            *seen = true;
            if default && after {
                let message = "`after` and `default` together: with `default` the `select` never waits, so the timeout could never fire";
                self.report(errors::bad_select(message, at));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    const CHANS: &str =
        "fun main() { let a: Chan<i64> = Chan.new(); let b: Chan<string> = Chan.buffered(1); ";

    #[test]
    fn select_arms_bind_and_agree() {
        ok(&format!(
            "{CHANS}
            select {{
                m = a.recv() => match m {{ Some(n) => println(n), None => println(\"closed\") }},
                _ = b.recv() => println(\"b\"),
                b.send(\"x\") => {{ println(\"sent\"); }}
                after(500) => println(\"timed out\"),
            }}
        }}"
        ));
        let src = format!("{CHANS}let v = select {{ m = a.recv() => 1, default => 2 }}; }}");
        assert_eq!(let_type(&src, "v"), Type::I64);
        let src = format!("{CHANS}let v = select {{ m = a.recv() => m, default => None }}; }}");
        assert_eq!(let_type(&src, "v"), Type::Option(Box::new(Type::I64)));
        // A sent value is typed by the channel.
        ok(
            "fun main() { let c: Chan<u8> = Chan.new(); select { c.send(200) => 0, default => 1 }; }",
        );
    }

    #[test]
    fn arm_values_must_agree() {
        let src = format!("{CHANS}let v = select {{ m = a.recv() => 1, default => \"x\" }}; }}");
        assert_eq!(errors(&src), [("E0403", "\"x\"")]);
    }

    #[test]
    fn the_binding_is_the_arms_own() {
        let src = format!("{CHANS}select {{ m = a.recv() => 1, default => 2 }}; let n = m; }}");
        assert_eq!(errors(&src), [("E0301", "m")]);
    }

    #[test]
    fn select_shapes_that_are_e0438() {
        for (arms, at) in [
            ("default => 1", "select"),
            ("after(5) => 1", "select"),
            ("m = a.recv() => 1, default => 2, default => 3", "default"),
            ("m = a.recv() => 1, after(1) => 2, after(2) => 3", "after"),
            ("m = a.recv() => 1, after(1) => 2, default => 3", "default"),
        ] {
            let src = format!("{CHANS}select {{ {arms} }}; }}");
            assert_eq!(errors(&src), [("E0438", at)], "{arms}");
        }
    }

    #[test]
    fn recv_and_send_arms_need_a_channel() {
        let src = "struct S {}
            impl S { fun recv(self): Option<i64> = None; }
            fun main() { let s = S {}; select { m = s.recv() => 1, default => 2 }; }";
        assert_eq!(errors(src), [("E0438", "s")]);
    }

    #[test]
    fn after_takes_milliseconds_as_i64() {
        let src = format!("{CHANS}select {{ m = a.recv() => 1, after(\"x\") => 2 }}; }}");
        assert_eq!(errors(&src), [("E0403", "\"x\"")]);
    }
}
