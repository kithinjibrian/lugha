//! Default methods and `?` conversion in codegen (v5 draft §9, PRP-049):
//! a default's instance stands in for a method a type lacks, and a
//! converted error is boxed like any interface value.

use inkwell::values::BasicValueEnum;

use super::instances::instance_name;
use super::lower::Lowerer;
use super::methods::Method;
use crate::ast::Expr;
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// The default's instance a call through a bound reaches for a
    /// receiver of type `ty` without its own method (v5).
    /// A generic interface's default takes the interface's arguments after
    /// the receiver's type (v6).
    pub(super) fn default_method(&self, call: &Expr, ty: &Type) -> Option<Method<'ctx>> {
        let callee = self.callees.get(&call.id)?;
        let own = self.default_args.get(&call.id).into_iter().flatten();
        let args: Vec<Type> = std::iter::once(ty.clone())
            .chain(own.map(|t| self.concrete(t)))
            .collect();
        self.monos.get(&instance_name(callee, &args)).cloned()
    }

    /// `value`, of concrete type `ty`, boxed as a value of interface `to`.
    pub(super) fn box_as(
        &mut self,
        value: BasicValueEnum<'ctx>,
        ty: &Type,
        to: &Type,
    ) -> BasicValueEnum<'ctx> {
        // Another interface's value keeps its box and finds its table (v6).
        if let Type::Interface(..) = ty {
            return self.upcast(value.into_struct_value(), ty, to).into();
        }
        let boxed = self.new_box(ty, value);
        let table = self.table(ty, to);
        self.pair(table, boxed).into()
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn a_defaulted_slot_points_at_the_defaults_instance() {
        let ir = ir(
            "interface Error { fun message(self): string; fun report(self): string = \"e: {self.message()}\"; }\n\
            struct P { t: string }\nimpl P { fun message(self): string = self.t; }\n\
            fun main() { let e: Error = P { t: \"x\" }; println(e.report()); }",
        );
        let table = ir
            .lines()
            .find(|l| l.starts_with("@lugha_vt_P.Error = "))
            .unwrap();
        assert!(table.contains("@\"lugha_fn_Error.report<P>\""), "{table}");
        assert!(
            ir.contains("define ptr @\"lugha_fn_Error.report<P>\"(ptr"),
            "the default compiles once for P: {ir}"
        );
    }

    #[test]
    fn a_generic_interfaces_default_compiles_per_type_and_argument() {
        let ir = ir(
            "interface Source<T> { fun next(mut self): Option<T>; fun skip(mut self): bool = match self.next() { Some(_) => true, None => false }; }\n\
            struct Count { n: i64 }\nimpl Count { fun next(mut self): Option<i64> = Some(self.n); }\n\
            fun drop<T, S: Source<T>>(s: S): bool { let mut t = s; t.skip() }\n\
            fun main() { let b = drop(Count { n: 1 }); }",
        );
        assert!(
            ir.contains("define zeroext i1 @\"lugha_fn_Source<i64>.skip<Count>\"(ptr"),
            "{ir}"
        );
        assert!(
            ir.contains("call zeroext i1 @\"lugha_fn_Source<i64>.skip<Count>\""),
            "{ir}"
        );
    }
}
