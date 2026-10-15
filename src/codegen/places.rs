//! Places written to (spec §5), with copy-on-write arrays (v15, milestone
//! 55): a write reaches its target through the slot of every array on the
//! way, making each unique (`own`) just before the write.
//!
//! A write happens in two phases. `place_plan` evaluates the place's index
//! operands, root outward, and bounds-checks them: the order §5 requires.
//! `place_commit` then walks the place again with those indexes, owning
//! each array, and gives the target's address. Statements that evaluate
//! values between the two (assignment, `push`, a `mut self` call's
//! arguments) commit after them, so a value that shares the target's array
//! (`xs[0] = { c = xs; 1 }`) is never written through.

use inkwell::values::{IntValue, PointerValue};

use super::CodegenError;
use super::heap::element_of;
use super::lower::{Lowerer, POSITIONED};
use super::value::llvm_type;
use crate::ast::{Expr, ExprKind};
use crate::check::Type;

/// A place's index values, root outward, evaluated and bounds-checked.
pub(super) type Plan<'ctx> = Vec<IntValue<'ctx>>;

/// A `for x of mut xs` variable (v15): it names element `counter` of the
/// array at `place`, re-reached through `place` on every use, so a copy of
/// the array made in the loop's body is never written through.
#[derive(Debug, Clone)]
pub(super) struct Alias<'ctx> {
    pub place: Expr,
    pub plan: Plan<'ctx>,
    pub counter: PointerValue<'ctx>,
    pub element: Type,
}

impl<'ctx> Lowerer<'ctx> {
    /// Evaluates `place`'s index operands in order and bounds-checks each
    /// against its array as it is now.
    pub(super) fn place_plan(&mut self, place: &Expr) -> Result<Plan<'ctx>, CodegenError> {
        match &place.kind {
            ExprKind::Name(_) => Ok(Vec::new()),
            ExprKind::Field(base, _) => self.place_plan(base),
            ExprKind::Index(base, open, index) => {
                let mut plan = self.place_plan(base)?;
                let (slot, _, _) = self.walk(base, &plan, &mut 0, false);
                let array = self.load_pointer(slot);
                let index_ty = self.ty(index);
                let i = self.get(index, &index_ty)?.into_int_value();
                let i = self.widen_index(i, &index_ty);
                let len = self.length(array);
                self.bounds_check(len, i, open.start);
                plan.push(i);
                Ok(plan)
            }
            _ => unreachable!("checked: E0502 rejects other places"),
        }
    }

    /// The address, type and element-ness of `place` with the indexes of
    /// `plan`, every array on the way made unique.
    pub(super) fn place_commit(
        &mut self,
        place: &Expr,
        plan: &Plan<'ctx>,
    ) -> (PointerValue<'ctx>, Type, bool) {
        self.walk(place, plan, &mut 0, true)
    }

    /// The address of `place` from its root, taking indexes from `plan` in
    /// order; with `own`, each array is made unique before it is indexed.
    fn walk(
        &mut self,
        place: &Expr,
        plan: &Plan<'ctx>,
        next: &mut usize,
        own: bool,
    ) -> (PointerValue<'ctx>, Type, bool) {
        match &place.kind {
            ExprKind::Name(name) => {
                let local = self.scopes.lookup(name);
                (self.local_address(local.ptr, own), local.ty, false)
            }
            ExprKind::Field(base, field) => {
                let (base_address, base_ty, _) = self.walk(base, plan, next, own);
                let (index, field_ty) = self.field_index(&base_ty, &field.name);
                let llvm = llvm_type(self.context, &base_ty).into_struct_type();
                let address = self
                    .builder
                    .build_struct_gep(llvm, base_address, index, &field.name)
                    .expect("checked: the field exists");
                (address, field_ty, true)
            }
            ExprKind::Index(base, _, _) => {
                let (slot, base_ty, _) = self.walk(base, plan, next, own);
                let array = if own {
                    self.own_place(slot, base, &base_ty)
                } else {
                    self.load_pointer(slot)
                };
                let i = plan[*next];
                *next += 1;
                let element = element_of(&base_ty);
                let address = self.element_address(array, i, self.element_size(&element));
                (address, element, true)
            }
            _ => unreachable!("checked: E0502 rejects other places"),
        }
    }

    /// Where local `ptr` is: itself, or for a `for … of mut` variable the
    /// element it names, reached afresh (with `own`, for writing).
    pub(super) fn local_address(
        &mut self,
        ptr: PointerValue<'ctx>,
        own: bool,
    ) -> PointerValue<'ctx> {
        let Some(alias) = self.aliases.get(&ptr).cloned() else {
            return ptr;
        };
        let (slot, array_ty, _) = self.walk(&alias.place, &alias.plan, &mut 0, own);
        let array = if own {
            self.own_place(slot, &alias.place, &array_ty)
        } else {
            self.load_pointer(slot)
        };
        let i64_type = self.context.i64_type();
        let k = self
            .builder
            .build_load(i64_type, alias.counter, "k")
            .expect(POSITIONED)
            .into_int_value();
        self.element_address(array, k, self.element_size(&alias.element))
    }

    /// The current value of `place` with the indexes of `plan`, owning nothing.
    pub(super) fn place_read(
        &mut self,
        place: &Expr,
        plan: &Plan<'ctx>,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        let (address, ty, element) = self.walk(place, plan, &mut 0, false);
        if element {
            self.load_element(address, &ty)
        } else {
            let llvm = llvm_type(self.context, &ty);
            self.builder
                .build_load(llvm, address, "current")
                .expect(POSITIONED)
        }
    }

    /// The array in `slot`, which holds the array at `path`, made unique:
    /// by `own`, unless an enclosing loop already did (`hoist.rs`, v15).
    pub(super) fn own_place(
        &mut self,
        slot: PointerValue<'ctx>,
        path: &Expr,
        ty: &Type,
    ) -> PointerValue<'ctx> {
        if self.hoisted(path) {
            self.load_pointer(slot)
        } else {
            self.own(slot, ty)
        }
    }

    /// The pointer stored at `slot`.
    pub(super) fn load_pointer(&self, slot: PointerValue<'ctx>) -> PointerValue<'ctx> {
        let ptr = self.context.ptr_type(Default::default());
        self.builder
            .build_load(ptr, slot, "array")
            .expect(POSITIONED)
            .into_pointer_value()
    }
}
