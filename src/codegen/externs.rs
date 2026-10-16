//! Values crossing to and from C (spec §8; v16, milestone 88, PRP-118):
//! a handle is already a C pointer; `Option` of a handle is that pointer,
//! `NULL` for `None`. Every other extern type keeps its plain lowering.

use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;

use super::lower::{Lowerer, POSITIONED};
use super::value::llvm_type;
use crate::check::Type;

/// True for `Option<H>` of a handle `H`: a nullable C pointer.
pub(super) fn is_nullable_handle(ty: &Type) -> bool {
    matches!(ty, Type::Option(inner) if matches!(**inner, Type::Handle(_)))
}

impl<'ctx> Lowerer<'ctx> {
    /// The LLVM type C uses for an extern result of type `ty`.
    pub(super) fn c_result_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        if is_nullable_handle(ty) {
            self.context.ptr_type(Default::default()).into()
        } else {
            llvm_type(self.context, ty)
        }
    }

    /// `Option<H>` → the handle, or `NULL` for `None`.
    pub(super) fn nullable_to_c(
        &mut self,
        value: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> BasicValueEnum<'ctx> {
        let slot = self.entry_alloca(ty, "c.option");
        self.builder.build_store(slot, value).expect(POSITIONED);
        let (some, payloads) = self.variant(ty, "Some");
        let tag = self.tag(ty, slot);
        let is_some = self
            .builder
            .build_int_compare(
                inkwell::IntPredicate::EQ,
                tag,
                self.context.i32_type().const_int(u64::from(some), false),
                "is.some",
            )
            .expect(POSITIONED);
        let ptr = self.context.ptr_type(Default::default());
        let address = self.payload_address(ty, slot, &payloads, 0);
        let handle = self
            .builder
            .build_load(ptr, address, "handle")
            .expect(POSITIONED);
        self.builder
            .build_select(is_some, handle, ptr.const_null().into(), "c.ptr")
            .expect(POSITIONED)
    }

    /// A C pointer → `Option<H>`: `None` for `NULL`, else `Some`.
    pub(super) fn nullable_from_c(
        &mut self,
        value: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> BasicValueEnum<'ctx> {
        let pointer = value.into_pointer_value();
        let is_null = self
            .builder
            .build_is_null(pointer, "is.null")
            .expect(POSITIONED);
        let none = self.construct_values(ty, "None", Vec::new());
        let some = self.construct_values(ty, "Some", vec![value]);
        self.builder
            .build_select(is_null, none, some, "option")
            .expect(POSITIONED)
    }
}
