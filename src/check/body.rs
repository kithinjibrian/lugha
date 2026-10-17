//! Function and method bodies: parameters and `self` bound, the body
//! checked against the return type (spec §6, v1 §4).

use super::expr::Expect;
use super::{Binding, Checker, Type, errors};
use crate::ast::{FunDecl, Ident};

impl Checker {
    /// Checks one function body against its signature.
    pub(super) fn function(&mut self, f: &FunDecl) {
        let signature = &self.functions[&self.key(&f.name.name)];
        let (params, ret) = (signature.params.clone(), signature.ret.clone());
        self.type_params = signature.generics.clone();
        self.owner = self.key(&f.name.name);
        self.body(f, params, ret, None);
        self.type_params.clear();
        self.owner.clear();
    }

    /// Checks a function or method body. A method's receiver is bound as
    /// `self`: immutable like a parameter, or assignable for `mut self` (v1 §4).
    pub(super) fn body(
        &mut self,
        f: &FunDecl,
        params: Vec<Type>,
        ret: Type,
        receiver: Option<Type>,
    ) {
        self.ret = ret.clone();
        self.scopes.clear();
        self.changes.clear();
        self.push();
        if let (Some(ty), Some(r)) = (receiver, f.receiver) {
            let binding = if r.mutable {
                Binding::SelfMut
            } else {
                Binding::Param
            };
            self.declare(
                &Ident {
                    name: "self".into(),
                    span: r.span,
                },
                ty,
                binding,
            );
        }
        for (param, ty) in f.params.iter().zip(params) {
            self.declare(&param.name, ty, Binding::Param);
        }
        let expect = (ret != Type::Unit).then(|| Expect::of(ret.clone()));
        let body = self.block(&f.body, expect.as_ref());
        self.pop();
        if ret != Type::Unit && body == Type::Unit {
            self.missing_return(f, ret);
        } else if ret != Type::Unit && !body.fits(&ret) {
            let span = f.body.tail.as_ref().map_or(f.body.span, |tail| tail.span);
            self.report(errors::mismatch(ret, body, span, None));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::ok;

    #[test]
    fn every_expression_of_a_valid_program_gets_a_type() {
        // `0..3` alone would make `i` an i64 (spec §4); the i32 bound types the range.
        let src = "fun fib(n: i32): i32 = if n < 2 { n } else { fib(n - 1) + fib(n - 2) };\n\
                   fun main(): i32 { let n: i32 = 3; let mut t: i32 = 0; for i in 0..n { t += i; } fib(10) + t - n }";
        let checked = ok(src);
        assert!(!checked.types.contains(&Type::Error), "{:?}", checked.types);
    }
}
