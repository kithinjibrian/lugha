//! Structs too big to split (v17, milestone 126, PRP-158 decision 304): a
//! `load` of a struct with more than `MAX_LEAVES` leaves whose uses are
//! only copies and field reads goes through memory — `memcpy` for the
//! copies, one load per field read — so its blocks keep the fast
//! instruction selector without the IR growing by hundreds of leaves.

use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum, InstructionOpcode, InstructionValue};

use super::Splitter;
use crate::codegen::lower::POSITIONED;
use crate::codegen::rebuild::replace_uses;
use crate::codegen::split_leaves::{aligned, basic_type, leaf_types, operand, value_of};

impl<'ctx> Splitter<'_, 'ctx> {
    /// `load` of a struct too big to split, when its every use is a `store`
    /// of it or a field read from it: copied once into a fresh slot by
    /// `memcpy` (or straight to a single `store`'s destination when nothing
    /// writes between), each `store` a `memcpy` from the slot, each field
    /// read a load of that field alone. Any other use leaves it whole.
    pub(super) fn big_load(&mut self, load: InstructionValue<'ctx>) {
        let value = value_of(load);
        let users = users_of(value);
        let handled = |u: &InstructionValue| match u.get_opcode() {
            InstructionOpcode::Store => operand(*u, 0) == value,
            InstructionOpcode::ExtractValue => true,
            _ => false,
        };
        if users.is_empty() || !users.iter().all(handled) {
            return;
        }
        let ty = basic_type(load);
        let src = operand(load, 0).into_pointer_value();
        let align = load.get_alignment().unwrap_or(1).max(1);
        let size = ty.size_of().expect("a struct has a size");
        self.replaced.push(load);
        if let [store] = users.as_slice()
            && store.get_opcode() == InstructionOpcode::Store
            && nothing_written_between(load, *store)
        {
            self.builder.position_before(store);
            let dst = operand(*store, 1).into_pointer_value();
            let dst_align = store.get_alignment().unwrap_or(1).max(1);
            self.builder
                .build_memcpy(dst, dst_align, src, align, size)
                .expect(POSITIONED);
            self.replaced.push(*store);
            return;
        }
        let entry = self
            .function
            .get_first_basic_block()
            .expect("an entry block");
        self.position_at_start(entry);
        let slot = self.builder.build_alloca(ty, "copy").expect(POSITIONED);
        self.builder.position_before(&load);
        self.builder
            .build_memcpy(slot, align, src, align, size)
            .expect(POSITIONED);
        for user in users {
            self.builder.position_before(&user);
            if user.get_opcode() == InstructionOpcode::Store {
                let dst = operand(user, 1).into_pointer_value();
                let dst_align = user.get_alignment().unwrap_or(1).max(1);
                self.builder
                    .build_memcpy(dst, dst_align, slot, align, size)
                    .expect(POSITIONED);
                self.replaced.push(user);
                continue;
            }
            let path = user.get_indices();
            let field = field_type(ty, &path);
            let at = self.address(slot, ty, &path);
            match leaf_types(field) {
                // A small struct field: its leaves, loaded one by one.
                Some(types) => {
                    let leaves = types
                        .into_iter()
                        .map(|(p, leaf_ty)| {
                            let leaf_at = self.address(at, field, &p);
                            let l = self
                                .builder
                                .build_load(leaf_ty, leaf_at, "f")
                                .expect(POSITIONED);
                            aligned(l.as_instruction_value().expect("a load"), align, leaf_ty);
                            (p, l)
                        })
                        .collect();
                    self.split(user, leaves);
                }
                // A scalar, or a big field (itself loaded and handled again).
                None => {
                    let l = self.builder.build_load(field, at, "f").expect(POSITIONED);
                    let new = l.as_instruction_value().expect("a load");
                    aligned(new, align, field);
                    replace_uses(value_of(user), l);
                    self.replaced.push(user);
                    if is_big(field) {
                        self.big_load(new);
                    }
                }
            }
        }
    }
}

/// True for a struct or array type too big to split.
pub(super) fn is_big(ty: BasicTypeEnum<'_>) -> bool {
    matches!(
        ty,
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)
    ) && leaf_types(ty).is_none()
}

/// The type at `path` inside `ty`.
fn field_type<'ctx>(ty: BasicTypeEnum<'ctx>, path: &[u32]) -> BasicTypeEnum<'ctx> {
    path.iter().fold(ty, |t, &i| match t {
        BasicTypeEnum::StructType(s) => s.get_field_type_at_index(i).expect("a field"),
        BasicTypeEnum::ArrayType(a) => a.get_element_type(),
        _ => unreachable!("a path leads through structs and arrays"),
    })
}

/// The instructions using `value`, each once.
fn users_of<'ctx>(value: BasicValueEnum<'ctx>) -> Vec<InstructionValue<'ctx>> {
    let mut out: Vec<InstructionValue> = Vec::new();
    let mut next = value.get_first_use();
    while let Some(u) = next {
        let user = match u.get_user() {
            inkwell::values::AnyValueEnum::InstructionValue(i) => Some(i),
            any => BasicValueEnum::try_from(any)
                .ok()
                .and_then(|b| b.as_instruction_value()),
        };
        if let Some(user) = user.filter(|i| !out.contains(i)) {
            out.push(user);
        }
        next = u.get_next_use();
    }
    out
}

/// True if `store` follows `load` in its block with no store or call
/// between, so the memory `load` read is unchanged at `store`.
fn nothing_written_between(load: InstructionValue<'_>, store: InstructionValue<'_>) -> bool {
    let mut at = load.get_next_instruction();
    while let Some(i) = at {
        if i == store {
            return true;
        }
        if matches!(
            i.get_opcode(),
            InstructionOpcode::Store | InstructionOpcode::Call
        ) {
            return false;
        }
        at = i.get_next_instruction();
    }
    false
}
