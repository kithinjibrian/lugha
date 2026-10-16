//! Values crossing to and from C (spec §8; v16, milestones 88–89): a
//! handle is already a C pointer; `Option` of a handle is that pointer,
//! `NULL` for `None`. C's text becomes a copied Lugha string
//! (`lugha_rt_str_from_c`), and `Option<string>` goes to C as its bytes or
//! `NULL`. Every other extern type keeps its plain lowering.

use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;

use super::lower::{Lowerer, POSITIONED};
use super::value::llvm_type;
use crate::check::Type;

/// True for `Option<H>` of a handle `H`, or `Option<string>` (v16): a
/// nullable C pointer.
pub(super) fn is_nullable_handle(ty: &Type) -> bool {
    matches!(ty, Type::Option(inner) if matches!(**inner, Type::Handle(_) | Type::String))
}

/// True for a value C hands Lugha that needs converting: a nullable
/// pointer, or C's text as a `string`.
pub(super) fn converts_from_c(ty: &Type) -> bool {
    is_nullable_handle(ty) || *ty == Type::String
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
        // A string goes to C as its bytes; a `None`'s slot is never read by C.
        let handle = match ty {
            Type::Option(inner) if **inner == Type::String => self.c_string(handle),
            _ => handle,
        };
        self.builder
            .build_select(is_some, handle, ptr.const_null().into(), "c.ptr")
            .expect(POSITIONED)
    }

    /// A value from C of type `ty` (see `converts_from_c`) as Lugha's.
    pub(super) fn value_from_c(
        &mut self,
        value: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> BasicValueEnum<'ctx> {
        match ty {
            Type::String => self.copy_c_text(value, false),
            _ => self.nullable_from_c(value, ty),
        }
    }

    /// `lugha_rt_str_from_c(text, nullable)`: a copy of C's text, `NULL`
    /// kept as `NULL` when `nullable` (it panics otherwise).
    fn copy_c_text(&mut self, text: BasicValueEnum<'ctx>, nullable: bool) -> BasicValueEnum<'ctx> {
        let copy = self.runtime(
            "lugha_rt_str_from_c",
            &[Type::String, Type::I32],
            Some(Type::String),
        );
        let flag = self
            .context
            .i32_type()
            .const_int(u64::from(nullable), false);
        let call = self
            .builder
            .build_call(copy, &[text.into(), flag.into()], "c.text")
            .expect(POSITIONED);
        match call.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(copy) => copy,
            inkwell::values::ValueKind::Instruction(_) => {
                unreachable!("lugha_rt_str_from_c returns a pointer")
            }
        }
    }

    /// A C pointer → `Option<H>` or `Option<string>`: `None` for `NULL`, else `Some`.
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
        let payload = match ty {
            Type::Option(inner) if **inner == Type::String => self.copy_c_text(value, true),
            _ => value,
        };
        let none = self.construct_values(ty, "None", Vec::new());
        let some = self.construct_values(ty, "Some", vec![payload]);
        self.builder
            .build_select(is_null, none, some, "option")
            .expect(POSITIONED)
    }
}
