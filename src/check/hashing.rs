//! `$hash.text(s, h)` (v17, milestones 101 and 143): FNV-1a over string
//! `s`'s bytes, starting from state `h`, as a `u64`. Everything else hashes
//! in Lugha, through the prelude's derivable `$Hash` (`prelude/hash.la`);
//! strings keep this built-in for speed. Only bundled sources can name it.

use super::expr::Expect;
use super::float_ops::builtin_op;
use super::{Checker, Type};
use crate::ast::{Expr, ExprKind};
use crate::diagnostic::Diagnostic;

/// The `$hash` built-in `callee` names, `text` for `$hash.text`.
pub fn hash_op(callee: &Expr) -> Option<&str> {
    builtin_op(callee, "$hash")
}

impl Checker {
    /// `$hash.text(s, h)`: a `string` and a `u64`; a `u64`.
    pub(super) fn hash_call(
        &mut self,
        call: &Expr,
        (callee, op): (&Expr, &str),
        args: &[Expr],
    ) -> Type {
        self.record(callee, Type::Unit);
        if let ExprKind::Field(base, _) = &callee.kind {
            self.record(base, Type::Unit);
        }
        if op != "text" || args.len() != 2 {
            // A compiler bug: only the prelude writes `$hash`.
            let d = Diagnostic::error(
                "E0415",
                format!("no `$hash` built-in `{op}` with these arguments"),
                call.span,
            );
            self.report(d);
            return self.unchecked_args(args);
        }
        self.value(&args[0], Some(&Expect::of(Type::String)));
        self.value(&args[1], Some(&Expect::of(Type::U64)));
        Type::U64
    }
}
