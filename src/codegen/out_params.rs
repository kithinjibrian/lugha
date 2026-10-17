//! Pointer parameters in codegen (v17, milestones 102–104): each `out`,
//! `ptr` or `mut` argument is a slot of C's type whose address C gets (`out`
//! zeroed, `ptr`/`mut` holding the value); after the call `out` and `mut`
//! slots are stored into their places, as an assignment would. Array
//! arguments pass a pointer to their elements.

use inkwell::values::{BasicValueEnum, PointerValue};

use super::CodegenError;
use super::externs::is_nullable_handle;
use super::heap::element_of;
use super::lower::{Lowerer, POSITIONED};
use super::places::Plan;
use super::value::llvm_type;
use crate::ast::{Expr, ExprKind, PassMode};
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
    /// An argument for an extern's `out`, `ptr` or `mut` parameter of type
    /// `ty`: the address of a slot of C's type — zeroed for `out`, holding a
    /// copy of the value for `ptr` and `mut` — the write-back of `out` and
    /// `mut` kept for `write_outs`.
    pub(super) fn pointer_argument(
        &mut self,
        arg: &Expr,
        ty: &Type,
        mode: PassMode,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let ExprKind::Pass(_, inner) = &arg.kind else {
            unreachable!("checked: a pointer parameter takes `out`, `ptr` or `mut`")
        };
        let c_type = c_slot_type(ty);
        let slot = self.entry_alloca(&c_type, mode.word());
        let first = match mode {
            PassMode::Out => llvm_type(self.context, &c_type).const_zero(),
            PassMode::Ptr => {
                let value = self.get(inner, ty)?;
                self.c_slot_value(value, ty)
            }
            PassMode::Mut => {
                let plan = self.place_plan(inner)?;
                let current = self.place_read(inner, &plan);
                self.outs.push(OutSlot {
                    place: (**inner).clone(),
                    plan,
                    slot,
                    ty: ty.clone(),
                });
                self.c_slot_value(current, ty)
            }
        };
        if mode == PassMode::Out {
            let plan = self.place_plan(inner)?;
            self.outs.push(OutSlot {
                place: (**inner).clone(),
                plan,
                slot,
                ty: ty.clone(),
            });
        }
        self.builder.build_store(slot, first).expect(POSITIONED);
        Ok(slot.into())
    }

    /// An argument for an extern's `xs: T[]` (or `mut xs: T[]`) parameter:
    /// a pointer to the array's first element, valid for the call. A `mut`
    /// array is made unique first, so no copy of it sees what C writes.
    pub(super) fn elements_argument(
        &mut self,
        arg: &Expr,
        ty: &Type,
        mutable: bool,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let array = if mutable {
            let ExprKind::Pass(_, place) = &arg.kind else {
                unreachable!("checked: a `mut` array parameter takes `mut place`")
            };
            let plan = self.place_plan(place)?;
            let (address, _, _) = self.place_commit(place, &plan);
            self.own_place(address, place, ty)
        } else {
            self.get(arg, ty)?.into_pointer_value()
        };
        let element = element_of(ty);
        let size = self.element_size(&element);
        let first = self.context.i64_type().const_zero();
        Ok(self.element_address(array, first, size).into())
    }

    /// `value` of type `ty` as C holds it: `bool` as a byte, `Option` of a
    /// handle as a nullable pointer.
    fn c_slot_value(&mut self, value: BasicValueEnum<'ctx>, ty: &Type) -> BasicValueEnum<'ctx> {
        match ty {
            Type::Bool => self
                .builder
                .build_int_z_extend(value.into_int_value(), self.context.i8_type(), "c.bool")
                .expect(POSITIONED)
                .into(),
            _ if is_nullable_handle(ty) => self.nullable_to_c(value, ty),
            _ => value,
        }
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
