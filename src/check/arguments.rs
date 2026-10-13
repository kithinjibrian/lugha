//! Arguments of method and associated-function calls (v1 §4, v2 draft
//! §4): arity, then each argument, inferring the method's type arguments.

use super::expr::Expect;
use super::generics::subst;
use super::methods::Method;
use super::{Checker, Type, errors};
use crate::ast::Expr;

impl Checker {
    /// Arity, then the arguments, inferring the method's type arguments
    /// after the `impl`'s are bound to `outer` (PRP-035).
    pub(super) fn arguments(
        &mut self,
        call: &Expr,
        name: &str,
        m: &Method,
        args: &[Expr],
        (outer, expect): (&[Type], Option<&Expect>),
    ) -> Type {
        if args.len() != m.params.len() {
            self.report(errors::arity(name, m.params.len(), args.len(), call.span));
            self.unchecked(args);
            return if m.generics.is_empty() {
                m.ret.clone()
            } else {
                Type::Error
            };
        }
        let inputs: Vec<_> = args
            .iter()
            .zip(&m.params)
            .map(|(a, t)| (a, Expect::of(t.clone())))
            .collect();
        let bound = m
            .generics
            .iter()
            .map(|g| g.name.clone())
            .zip(outer.iter().cloned())
            .collect();
        match self.infer(call, &m.generics, bound, Some((&m.ret, expect)), &inputs) {
            Some(targs) => {
                self.used(call, &m.key, &targs);
                subst(&m.ret, &m.generics, &targs)
            }
            None => Type::Error,
        }
    }

    pub(super) fn unchecked(&mut self, args: &[Expr]) -> Type {
        for arg in args {
            self.expr(arg, None);
        }
        Type::Error
    }
}
