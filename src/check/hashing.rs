//! `$hash.value(x)` (v17, milestone 101, PRP-133): a value's hash as a `u64`,
//! structural and matching `==`, for the prelude's maps and sets. Only
//! bundled sources can name it (`$` starts no name in a program).

use super::expr::Expect;
use super::float_ops::builtin_op;
use super::{Checker, Type};
use crate::ast::{Expr, ExprKind};
use crate::diagnostic::Diagnostic;

/// The `$hash` built-in `callee` names, `of` for `$hash.value`.
pub fn hash_op(callee: &Expr) -> Option<&str> {
    builtin_op(callee, "$hash")
}

impl Checker {
    /// `$hash.value(x)`: `x` of any `Eq` type; a `u64`.
    pub(super) fn hash_call(
        &mut self,
        call: &Expr,
        (callee, op): (&Expr, &str),
        args: &[Expr],
    ) -> Type {
        self.record(callee, Type::Void);
        if let ExprKind::Field(base, _) = &callee.kind {
            self.record(base, Type::Void);
        }
        if op != "value" || args.len() != 1 {
            let d = Diagnostic::error(
                "E0415",
                format!("no `$hash` built-in `{op}` with these arguments"),
                call.span,
            );
            self.report(d);
            return self.unchecked_args(args);
        }
        let ty = self.value(&args[0], None::<&Expect>);
        if !matches!(ty, Type::Error) && !self.eq_ok(&ty) {
            // A compiler bug: only the prelude writes `$hash`, with `Eq` keys.
            let d = Diagnostic::error(
                "E0419",
                format!("`{ty}` can't be hashed: it isn't `Eq`"),
                args[0].span,
            );
            self.report(d);
        }
        Type::U64
    }
}
