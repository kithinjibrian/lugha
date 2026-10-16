//! Channels in the checker (v17 draft decision 5, milestone 91):
//! `Chan.new()` and `Chan.buffered(n)` make a `Chan<T>` whose `T` comes
//! from the expected type; `send`, `recv`, `try_recv`, `close` and `is`
//! are built-in methods (`builtins.rs`).

use super::expr::Expect;
use super::{Checker, Type, errors};
use crate::ast::{Expr, Ident};

/// What codegen finds `Chan.new()` by in `Checked::callees`.
pub const CHAN_NEW: &str = "$Chan.new";
/// What codegen finds `Chan.buffered(n)` by in `Checked::callees`.
pub const CHAN_BUFFERED: &str = "$Chan.buffered";

impl Checker {
    /// `Chan.new()` or `Chan.buffered(n)`; any other `Chan.f(…)` is E0415.
    pub(super) fn chan_new(
        &mut self,
        call: &Expr,
        name: &Ident,
        args: &[Expr],
        expect: Option<&Expect>,
    ) -> Type {
        let (key, arity) = match name.name.as_str() {
            "new" => (CHAN_NEW, 0),
            "buffered" => (CHAN_BUFFERED, 1),
            _ => {
                let help =
                    Some("make a channel with `Chan.new()` or `Chan.buffered(n)`".to_string());
                let chan = Type::Chan(Box::new(Type::Error));
                self.report(errors::no_method(&name.name, &chan, name.span, help));
                return self.unchecked(args);
            }
        };
        if args.len() != arity {
            self.report(errors::arity(&name.name, arity, args.len(), call.span));
            return self.unchecked(args);
        }
        if let [size] = args {
            self.expect_type(size, &Expect::of(Type::I64));
        }
        self.callees.insert(call.id, key.to_string());
        match expect.map(|e| &e.ty) {
            Some(Type::Chan(inner)) => Type::Chan(inner.clone()),
            Some(Type::Error) => Type::Error,
            _ => {
                self.report(errors::chan_unknown(call.span));
                Type::Error
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    #[test]
    fn channels_are_made_and_used() {
        ok("fun main() {
            let c: Chan<i64> = Chan.new();
            c.send(1);
            let v: Option<i64> = c.recv();
            let w = c.try_recv();
            c.close();
            let same = c.is(c);
            let d: Chan<string> = Chan.buffered(4);
            let bytes: Chan<u8> = Chan.new();
            bytes.send(255);
        }");
        let src = "fun main() { let c: Chan<i64> = Chan.new(); let v = c.recv(); }";
        assert_eq!(let_type(src, "v"), Type::Option(Box::new(Type::I64)));
        ok("fun drain(c: Chan<i64>): i64 {
                let mut sum = 0;
                while let Some(x) = c.recv() { sum += x; }
                sum
            }
            fun main() {}");
    }

    #[test]
    fn a_channel_needs_its_type_written() {
        assert_eq!(
            errors("fun main() { let c = Chan.new(); }"),
            [("E0412", "Chan.new()")]
        );
    }

    #[test]
    fn channel_mistakes() {
        let pre = "fun main() { let c: Chan<i64> = Chan.new(); ";
        for (body, code, at) in [
            ("c.send(\"x\"); }", "E0403", "\"x\""),
            ("let d: Chan<i64> = Chan.make(); }", "E0415", "make"),
            (
                "let d: Chan<i64> = Chan.buffered(\"x\"); }",
                "E0403",
                "\"x\"",
            ),
            ("let d: Chan<i64> = Chan.new(1); }", "E0405", "Chan.new(1)"),
            ("let b = c == c; }", "E0404", "c == c"),
        ] {
            let src = format!("{pre}{body}");
            assert_eq!(errors(&src), [(code, at)], "{body}");
        }
    }
}
