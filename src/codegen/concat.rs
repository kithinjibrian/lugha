//! Joining arrays (v17, milestone 122; PRP-154): `xs + ys` and `xs += ys`
//! call the prelude's `$concat` instance at the element type, which the
//! checker listed; strings keep `lugha_rt_str_concat`.

use inkwell::values::{BasicValueEnum, ValueKind};

use super::instances::instance_name;
use super::lower::{Lowerer, POSITIONED};
use crate::check::{CONCAT, Type};

impl<'ctx> Lowerer<'ctx> {
    /// `a + b` on two arrays of type `ty`: a new array, `a`'s elements then `b`'s.
    pub(super) fn join_arrays(
        &self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> BasicValueEnum<'ctx> {
        let Type::Array(element) = ty else {
            unreachable!("checked: `+` joins arrays")
        };
        let name = instance_name(CONCAT, std::slice::from_ref(element));
        let function = self
            .monos
            .get(&name)
            .unwrap_or_else(|| panic!("checked: `{name}` is listed for every `+` on arrays"))
            .signature
            .function;
        let site = self
            .builder
            .build_call(function, &[a.into(), b.into()], "concat")
            .expect(POSITIONED);
        match site.try_as_basic_value() {
            ValueKind::Basic(result) => result,
            ValueKind::Instruction(_) => unreachable!("`$concat` returns an array"),
        }
    }
}
