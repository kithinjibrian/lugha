//! `out` parameters in codegen (v17, milestone 102): each `out place`
//! argument is a zeroed slot of C's type whose address C gets; after the
//! call the slot's value is stored into the place, as an assignment would.

use inkwell::values::{BasicValueEnum, PointerValue};

use super::CodegenError;
use super::externs::is_nullable_handle;
use super::lower::{Lowerer, POSITIONED};
use super::places::Plan;
use super::value::llvm_type;
use crate::ast::{Expr, ExprKind};
use crate::check::Type;

/// An `out` argument waiting for the call to return.
pub(super) struct OutSlot<'ctx> {
    place: Expr,
    /// The place's indexes, fixed when the argument was evaluated (spec §5).
    plan: Plan<'ctx>,
    slot: PointerValue<'ctx>,
    ty: Type,
}

/// What C writes for a value of `ty`: a pointer for `Option` of a handle,
/// a byte for `bool` (C99), the type itself otherwise.
fn c_slot_type(ty: &Type) -> Type {
    match ty {
        Type::Bool => Type::U8,
        _ if is_nullable_handle(ty) => Type::Handle(String::new()),
        _ => ty.clone(),
    }
}

impl<'ctx> Lowerer<'ctx> {
    /// `out place` for an `out` parameter of type `ty`: the address of a
    /// zeroed slot, the write-back kept for `write_outs`.
    pub(super) fn out_argument(
        &mut self,
        arg: &Expr,
        ty: &Type,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let ExprKind::Out(place) = &arg.kind else {
            unreachable!("checked: an `out` parameter takes `out place`")
        };
        let plan = self.place_plan(place)?;
        let c_type = c_slot_type(ty);
        let slot = self.entry_alloca(&c_type, "out");
        let zero = llvm_type(self.context, &c_type).const_zero();
        self.builder.build_store(slot, zero).expect(POSITIONED);
        self.outs.push(OutSlot {
            place: (**place).clone(),
            plan,
            slot,
            ty: ty.clone(),
        });
        Ok(slot.into())
    }

    /// After an extern returns: each `out` slot's value stored into its
    /// place, left to right.
    pub(super) fn write_outs(&mut self) {
        for out in std::mem::take(&mut self.outs) {
            let c_type = c_slot_type(&out.ty);
            let raw = self
                .builder
                .build_load(llvm_type(self.context, &c_type), out.slot, "out.value")
                .expect(POSITIONED);
            let value = match &out.ty {
                ty if is_nullable_handle(ty) => self.nullable_from_c(raw, ty),
                Type::Bool => self.load_element(out.slot, &Type::Bool),
                _ => raw,
            };
            let (address, ty, element) = self.place_commit(&out.place, &out.plan);
            if element {
                self.store_element(address, &ty, value);
            } else {
                self.builder.build_store(address, value).expect(POSITIONED);
            }
        }
    }
}
