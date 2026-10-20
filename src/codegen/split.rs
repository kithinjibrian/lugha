//! Struct values split into their fields (v17, milestone 126, PRP-158).
//! At `-O0`, LLVM's fast instruction selector gives up on a block at its
//! first whole-struct instruction and hands it to the slow one. So once a
//! module is lowered (and results go through pointers, `sret.rs`), every
//! struct-typed value is replaced by its *leaves* — its fields, recursively
//! through nested structs and arrays, down to integers, floats and
//! pointers: a struct `load` or `store` becomes one per leaf, `insertvalue`
//! and `extractvalue` pick leaves without any instruction, and `phi` and
//! `select` work leaf by leaf. Struct parameters become one parameter per
//! leaf (`split_params.rs`). Where a whole struct must still exist — given
//! to C, or of a type with more than [`MAX_LEAVES`] leaves — it is rebuilt
//! from its leaves just there.

use std::collections::HashMap;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicValue, BasicValueEnum, FunctionValue, InstructionOpcode, InstructionValue, PhiValue,
};

use super::lower::POSITIONED;
use super::rebuild::replace_uses;
use super::split_leaves::{
    Path, aligned, basic_type, byte_size, extract, insert, leaf_types, operand, order,
    result_leaves, undef, value_of,
};
use big::is_big;

/// A struct value as its leaves, in order.
type Leaves<'ctx> = Vec<(Path, BasicValueEnum<'ctx>)>;

/// Splits every struct value of every function `module` defines.
pub(super) fn split_structs<'ctx>(context: &'ctx Context, module: &Module<'ctx>) {
    super::split_params::split_params(context, module);
    let builder = context.create_builder();
    for function in module.get_functions() {
        if function.count_basic_blocks() > 0 {
            Splitter::new(context, &builder, function).run();
        }
    }
}

/// One function's rewrite.
struct Splitter<'a, 'ctx> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    function: FunctionValue<'ctx>,
    /// Each split value's leaves.
    leaves: HashMap<BasicValueEnum<'ctx>, Leaves<'ctx>>,
    /// The struct instructions replaced, in the order they were met.
    replaced: Vec<InstructionValue<'ctx>>,
    /// Each struct `phi` with its leaf `phi`s, filled in at the end.
    phis: Vec<(PhiValue<'ctx>, Vec<PhiValue<'ctx>>)>,
}

impl<'a, 'ctx> Splitter<'a, 'ctx> {
    fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> Self {
        Splitter {
            context,
            builder,
            function,
            leaves: HashMap::new(),
            replaced: Vec::new(),
            phis: Vec::new(),
        }
    }

    fn run(mut self) {
        for block in order(self.function) {
            let instructions: Vec<InstructionValue> = block.get_instructions().collect();
            for inst in instructions {
                self.instruction(inst);
            }
        }
        for (phi, leaf_phis) in std::mem::take(&mut self.phis) {
            for i in 0..phi.count_incoming() {
                let (value, from) = phi.get_incoming(i).expect("counted");
                let leaves = self.leaves_of(value);
                for (leaf_phi, (_, leaf)) in leaf_phis.iter().zip(leaves) {
                    leaf_phi.add_incoming(&[(&leaf, from)]);
                }
            }
        }
        // Users before what they use: what still has a use is rebuilt whole.
        for inst in std::mem::take(&mut self.replaced).into_iter().rev() {
            if inst.get_first_use().is_some() {
                self.rebuild_whole(inst);
            }
            inst.erase_from_basic_block();
        }
    }

    /// Splits `inst` if it is a struct instruction with leaves.
    fn instruction(&mut self, inst: InstructionValue<'ctx>) {
        let result = result_leaves(inst);
        match inst.get_opcode() {
            // Too big to split: copies and field reads go through memory (decision 304).
            InstructionOpcode::Load if result.is_none() && is_big(basic_type(inst)) => {
                self.big_load(inst);
            }
            InstructionOpcode::Load if let Some(types) = result => {
                let ptr = operand(inst, 0).into_pointer_value();
                let ty = basic_type(inst);
                let align = inst.get_alignment().unwrap_or(1);
                self.builder.position_before(&inst);
                let leaves = types
                    .into_iter()
                    .map(|(path, leaf_ty)| {
                        let at = self.address(ptr, ty, &path);
                        let load = self.builder.build_load(leaf_ty, at, "f").expect(POSITIONED);
                        aligned(load.as_instruction_value().expect("a load"), align, leaf_ty);
                        (path, load)
                    })
                    .collect();
                self.split(inst, leaves);
            }
            InstructionOpcode::Store => {
                let value = operand(inst, 0);
                if leaf_types(value.get_type()).is_none() {
                    return;
                }
                let ptr = operand(inst, 1).into_pointer_value();
                let align = inst.get_alignment().unwrap_or(1);
                let leaves = self.leaves_of(value);
                self.builder.position_before(&inst);
                for (path, leaf) in leaves {
                    let at = self.address(ptr, value.get_type(), &path);
                    let store = self.builder.build_store(at, leaf).expect(POSITIONED);
                    aligned(store, align, leaf.get_type());
                }
                self.replaced.push(inst);
            }
            InstructionOpcode::InsertValue if result.is_some() => {
                let base = self.leaves_of(operand(inst, 0));
                let element = operand(inst, 1);
                let at = inst.get_indices();
                let inserted = match leaf_types(element.get_type()) {
                    Some(_) => self.leaves_of(element),
                    None => vec![(Vec::new(), element)],
                };
                let leaves = base
                    .into_iter()
                    .map(|(path, leaf)| match path.strip_prefix(at.as_slice()) {
                        Some(rest) => {
                            let found = inserted.iter().find(|(p, _)| p == rest);
                            (
                                path.clone(),
                                found.expect("the inserted value has this leaf").1,
                            )
                        }
                        None => (path, leaf),
                    })
                    .collect();
                self.split(inst, leaves);
            }
            InstructionOpcode::ExtractValue => {
                let whole = operand(inst, 0);
                // A field of a call's result (C's, or an overflow intrinsic's
                // `{ i64, i1 }`, which the fast selector handles) stays as written.
                let from_call = !self.leaves.contains_key(&whole)
                    && whole
                        .as_instruction_value()
                        .is_some_and(|d| d.get_opcode() == InstructionOpcode::Call);
                if leaf_types(whole.get_type()).is_none() || from_call {
                    return;
                }
                let at = inst.get_indices();
                let inner: Leaves = self
                    .leaves_of(whole)
                    .into_iter()
                    .filter_map(|(path, leaf)| {
                        path.strip_prefix(at.as_slice()).map(|r| (r.to_vec(), leaf))
                    })
                    .collect();
                match result {
                    Some(_) => self.split(inst, inner),
                    None => {
                        let leaf = inner.first().expect("a scalar field is one leaf").1;
                        replace_uses(value_of(inst), leaf);
                        self.replaced.push(inst);
                    }
                }
            }
            InstructionOpcode::Phi if let Some(types) = result => {
                self.builder.position_before(&inst);
                let leaf_phis: Vec<PhiValue> = types
                    .iter()
                    .map(|(_, ty)| self.builder.build_phi(*ty, "f").expect(POSITIONED))
                    .collect();
                let leaves = types
                    .into_iter()
                    .zip(&leaf_phis)
                    .map(|((path, _), p)| (path, p.as_basic_value()))
                    .collect();
                let phi = PhiValue::try_from(inst).expect("a phi instruction");
                self.phis.push((phi, leaf_phis));
                self.split(inst, leaves);
            }
            InstructionOpcode::Select if result.is_some() => {
                let condition = operand(inst, 0).into_int_value();
                let (a, b) = (
                    self.leaves_of(operand(inst, 1)),
                    self.leaves_of(operand(inst, 2)),
                );
                self.builder.position_before(&inst);
                let leaves = a
                    .into_iter()
                    .zip(b)
                    .map(|((path, x), (_, y))| {
                        let s = self
                            .builder
                            .build_select(condition, x, y, "f")
                            .expect(POSITIONED);
                        (path, s)
                    })
                    .collect();
                self.split(inst, leaves);
            }
            _ => {}
        }
    }

    /// Records `inst`'s leaves; `inst` goes once its users are rewritten.
    fn split(&mut self, inst: InstructionValue<'ctx>, leaves: Leaves<'ctx>) {
        self.leaves.insert(value_of(inst), leaves);
        self.replaced.push(inst);
    }

    /// `value`'s leaves: those recorded, a constant's (folded), or for a
    /// value split nowhere else (a parameter or a C call's result) the
    /// `extractvalue`s just after it. It moves the builder: callers
    /// position it afterwards.
    fn leaves_of(&mut self, value: BasicValueEnum<'ctx>) -> Leaves<'ctx> {
        if let Some(found) = self.leaves.get(&value) {
            return found.clone();
        }
        let types = leaf_types(value.get_type()).expect("a split value has leaves");
        match value.as_instruction_value() {
            Some(def) => self.position_after(def),
            // A parameter's leaves come first; a constant's fold (no
            // instruction), but the builder still needs a position.
            None => {
                let entry = self
                    .function
                    .get_first_basic_block()
                    .expect("an entry block");
                self.position_at_start(entry);
            }
        }
        let leaves: Leaves = types
            .into_iter()
            .map(|(path, _)| {
                let leaf = extract(self.builder, value, &path);
                (path, leaf)
            })
            .collect();
        self.leaves.insert(value, leaves.clone());
        leaves
    }

    /// The whole value of `inst`, rebuilt from its leaves for a use that
    /// needs it (C), replacing `inst`'s remaining uses.
    fn rebuild_whole(&mut self, inst: InstructionValue<'ctx>) {
        let value = value_of(inst);
        let leaves = self
            .leaves
            .get(&value)
            .cloned()
            .expect("a split value has leaves");
        match inst.get_opcode() {
            InstructionOpcode::Phi => {
                let block = inst.get_parent().expect("in a block");
                let first = block
                    .get_instructions()
                    .find(|i| i.get_opcode() != InstructionOpcode::Phi)
                    .expect("a block ends with a terminator");
                self.builder.position_before(&first);
            }
            _ => self.builder.position_before(&inst),
        }
        let mut whole = undef(value.get_type());
        for (path, leaf) in leaves {
            whole = insert(self.builder, whole, &path, leaf);
        }
        replace_uses(value, whole);
    }

    /// The address of the leaf at `path` inside the `ty` at `ptr`: struct
    /// fields by `getelementptr`, array elements by their byte offset
    /// (inkwell's array `getelementptr` is `unsafe`).
    fn address(
        &self,
        ptr: inkwell::values::PointerValue<'ctx>,
        ty: BasicTypeEnum<'ctx>,
        path: &[u32],
    ) -> inkwell::values::PointerValue<'ctx> {
        let (mut at, mut ty) = (ptr, ty);
        for &i in path {
            match ty {
                BasicTypeEnum::StructType(s) => {
                    at = self
                        .builder
                        .build_struct_gep(s, at, i, "f")
                        .expect("a field");
                    ty = s.get_field_type_at_index(i).expect("a field");
                }
                BasicTypeEnum::ArrayType(a) => {
                    let element = a.get_element_type();
                    at = self.offset(at, element, i);
                    ty = element;
                }
                _ => unreachable!("a path leads through structs and arrays"),
            }
        }
        at
    }

    /// `at` moved `i` elements of type `element` along.
    fn offset(
        &self,
        at: inkwell::values::PointerValue<'ctx>,
        element: BasicTypeEnum<'ctx>,
        i: u32,
    ) -> inkwell::values::PointerValue<'ctx> {
        if i == 0 {
            return at;
        }
        let i64_type = self.context.i64_type();
        let b = self.builder;
        let size = match byte_size(element) {
            Some(n) => i64_type.const_int(n * i as u64, false),
            None => {
                let one = element.size_of().expect("a sized element");
                b.build_int_mul(one, i64_type.const_int(i as u64, false), "size")
                    .expect(POSITIONED)
            }
        };
        let base = b.build_ptr_to_int(at, i64_type, "base").expect(POSITIONED);
        let address = b.build_int_add(base, size, "address").expect(POSITIONED);
        b.build_int_to_ptr(
            address,
            self.context.ptr_type(Default::default()),
            "element",
        )
        .expect(POSITIONED)
    }

    fn position_after(&self, def: InstructionValue<'ctx>) {
        let block = def.get_parent().expect("in a block");
        let next = match def.get_opcode() {
            InstructionOpcode::Phi => block
                .get_instructions()
                .find(|i| i.get_opcode() != InstructionOpcode::Phi),
            _ => def.get_next_instruction(),
        };
        match next {
            Some(next) => self.builder.position_before(&next),
            None => self.builder.position_at_end(block),
        }
    }

    fn position_at_start(&self, block: BasicBlock<'ctx>) {
        match block.get_first_instruction() {
            Some(first) => self.builder.position_before(&first),
            None => self.builder.position_at_end(block),
        }
    }
}

#[path = "split_big.rs"]
mod big;

#[cfg(test)]
#[path = "split_tests.rs"]
mod tests;
