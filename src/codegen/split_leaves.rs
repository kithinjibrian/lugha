//! Leaves of struct types and the helpers of the splitting pass (v17,
//! milestone 126, PRP-158; `split.rs`): a type's leaves and their paths,
//! reading and writing a leaf of a whole value, the order blocks are
//! visited in, and the alignment of a leaf's access.

use std::collections::HashSet;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{AnyValue, BasicValue, BasicValueEnum, FunctionValue, InstructionValue};

use super::lower::POSITIONED;

/// Types with more leaves than this stay whole (their instructions keep
/// the slow path); kivuli's big types (300–608 leaves) go through memory instead (decision 304).
pub(super) const MAX_LEAVES: usize = 64;

/// Where a leaf sits: field and element indexes from the outer type in.
pub(super) type Path = Vec<u32>;

/// The leaves of `ty` with their types, when `ty` is a struct or array of
/// at most [`MAX_LEAVES`] leaves; `None` for anything else.
pub(super) fn leaf_types(ty: BasicTypeEnum<'_>) -> Option<Vec<(Path, BasicTypeEnum<'_>)>> {
    if !matches!(
        ty,
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)
    ) {
        return None;
    }
    let mut out = Vec::new();
    collect(ty, &mut Vec::new(), &mut out).then_some(out)
}

/// Appends `ty`'s leaves under `path`; false once there are too many.
fn collect<'ctx>(
    ty: BasicTypeEnum<'ctx>,
    path: &mut Path,
    out: &mut Vec<(Path, BasicTypeEnum<'ctx>)>,
) -> bool {
    let children: Vec<BasicTypeEnum> = match ty {
        BasicTypeEnum::StructType(s) => s.get_field_types(),
        BasicTypeEnum::ArrayType(a) if a.len() as usize <= MAX_LEAVES => {
            vec![a.get_element_type(); a.len() as usize]
        }
        BasicTypeEnum::ArrayType(_) => return false,
        leaf => {
            out.push((path.clone(), leaf));
            return out.len() <= MAX_LEAVES;
        }
    };
    for (i, child) in children.into_iter().enumerate() {
        path.push(i as u32);
        let fits = collect(child, path, out);
        path.pop();
        if !fits {
            return false;
        }
    }
    true
}

/// The value at `path` inside `value`, through `extractvalue`s (folded
/// away later, or for a constant at once).
pub(super) fn extract<'ctx>(
    builder: &Builder<'ctx>,
    value: BasicValueEnum<'ctx>,
    path: &[u32],
) -> BasicValueEnum<'ctx> {
    let mut v = value;
    for &i in path {
        v = match v {
            BasicValueEnum::StructValue(s) => builder.build_extract_value(s, i, "leaf"),
            BasicValueEnum::ArrayValue(a) => builder.build_extract_value(a, i, "leaf"),
            _ => unreachable!("a path leads through structs and arrays"),
        }
        .expect(POSITIONED);
    }
    v
}

/// `whole` with the value at `path` replaced by `leaf`.
pub(super) fn insert<'ctx>(
    builder: &Builder<'ctx>,
    whole: BasicValueEnum<'ctx>,
    path: &[u32],
    leaf: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let Some((&first, rest)) = path.split_first() else {
        return leaf;
    };
    let inner = match rest.is_empty() {
        true => leaf,
        false => insert(builder, extract(builder, whole, &[first]), rest, leaf),
    };
    let built = match whole {
        BasicValueEnum::StructValue(s) => builder.build_insert_value(s, inner, first, "whole"),
        BasicValueEnum::ArrayValue(a) => builder.build_insert_value(a, inner, first, "whole"),
        _ => unreachable!("a path leads through structs and arrays"),
    }
    .expect(POSITIONED);
    built.as_basic_value_enum()
}

/// The function's blocks, reachable ones in reverse post-order (each
/// value's definition before its uses), then any unreachable ones.
pub(super) fn order(function: FunctionValue<'_>) -> Vec<BasicBlock<'_>> {
    let Some(entry) = function.get_first_basic_block() else {
        return Vec::new();
    };
    let mut seen = HashSet::new();
    let mut post = Vec::new();
    // An explicit stack: (block, successors still to visit).
    let mut stack = vec![(entry, successors(entry))];
    seen.insert(entry);
    while let Some((block, next)) = stack.last_mut() {
        match next.pop() {
            Some(succ) if seen.insert(succ) => {
                let succs = successors(succ);
                stack.push((succ, succs));
            }
            Some(_) => {}
            None => {
                post.push(*block);
                stack.pop();
            }
        }
    }
    post.reverse();
    for block in function.get_basic_blocks() {
        if !seen.contains(&block) {
            post.push(block);
        }
    }
    post
}

/// The blocks `block`'s terminator may branch to, in reverse order (popped last first).
fn successors(block: BasicBlock<'_>) -> Vec<BasicBlock<'_>> {
    let Some(term) = block.get_terminator() else {
        return Vec::new();
    };
    let mut out: Vec<BasicBlock> = (0..term.get_num_operands())
        .filter_map(|i| term.get_operand(i).and_then(|o| o.block()))
        .collect();
    out.reverse();
    out
}

/// The leaves of `inst`'s result type, if it has a struct result to split.
pub(super) fn result_leaves(inst: InstructionValue<'_>) -> Option<Vec<(Path, BasicTypeEnum<'_>)>> {
    let ty = BasicTypeEnum::try_from(inst.get_type()).ok()?;
    leaf_types(ty)
}

/// `inst`'s result type, for an instruction with a value.
pub(super) fn basic_type(inst: InstructionValue<'_>) -> BasicTypeEnum<'_> {
    BasicTypeEnum::try_from(inst.get_type()).expect("an instruction with a value")
}

/// `inst`'s result as a value.
pub(super) fn value_of(inst: InstructionValue<'_>) -> BasicValueEnum<'_> {
    BasicValueEnum::try_from(inst.as_any_value_enum()).expect("an instruction with a value")
}

/// Operand `i` of `inst`, a value.
pub(super) fn operand(inst: InstructionValue<'_>, i: u32) -> BasicValueEnum<'_> {
    inst.get_operand(i)
        .and_then(|o| o.value())
        .expect("the operand is a value")
}

/// An undefined value of the struct or array type `ty`, to build on.
pub(super) fn undef(ty: BasicTypeEnum<'_>) -> BasicValueEnum<'_> {
    match ty {
        BasicTypeEnum::StructType(s) => s.get_undef().into(),
        BasicTypeEnum::ArrayType(a) => a.get_undef().into(),
        _ => unreachable!("only structs and arrays are rebuilt"),
    }
}

/// Sets a leaf access's alignment: the whole access's, at most the
/// leaf's size (a leaf's offset is a multiple of its size).
pub(super) fn aligned(access: InstructionValue<'_>, whole: u32, leaf: BasicTypeEnum<'_>) {
    let own = byte_size(leaf).map_or(whole, |n| n.max(1) as u32);
    let align = whole.min(own).max(1);
    access
        .set_alignment(align.next_power_of_two())
        .expect("loads and stores take an alignment");
}

/// The size in bytes of a scalar type on x86-64 (`None` for aggregates).
pub(super) fn byte_size(ty: BasicTypeEnum<'_>) -> Option<u64> {
    match ty {
        BasicTypeEnum::IntType(i) => Some(u64::from(i.get_bit_width()).div_ceil(8)),
        BasicTypeEnum::FloatType(f) => Some(if f == f.get_context().f32_type() {
            4
        } else {
            8
        }),
        BasicTypeEnum::PointerType(_) => Some(8),
        _ => None,
    }
}
