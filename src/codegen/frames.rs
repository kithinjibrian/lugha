//! Small stack frames (v17, milestone 135, PRP-167). Every function's
//! struct and array slots are shared: two slots of one type whose live
//! blocks never meet become one, so a frame costs its largest match arm or
//! branch, not the sum of them all. At `-O0` (and for `--emit=ir`) that
//! happens once a module is lowered — results through pointers (`sret.rs`),
//! structs split (`split.rs`); at `-O2` after LLVM's optimiser, which
//! works much harder on merged slots and removes many slots itself. And
//! every function probes its stack a page at a time as it enters, so a
//! frame of any size reaches the guard area before the memory past it, and
//! running out of stack panics instead of crashing (spec §7).

use std::collections::HashMap;

use inkwell::attributes::AttributeLoc;
use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{
    AnyValueEnum, BasicValue, CallSiteValue, FunctionValue, InstructionOpcode, InstructionValue,
};

use super::split_leaves::successors;

/// Adds stack probes to every function `module` defines.
pub(super) fn probe_stacks<'ctx>(context: &'ctx Context, module: &Module<'ctx>) {
    let probe = context.create_string_attribute("probe-stack", "inline-asm");
    for function in module
        .get_functions()
        .filter(|f| f.count_basic_blocks() > 0)
    {
        function.add_attribute(AttributeLoc::Function, probe);
    }
}

/// Shares slots in every function `module` defines.
pub(super) fn share_all_slots(module: &Module<'_>) {
    for function in module
        .get_functions()
        .filter(|f| f.count_basic_blocks() > 0)
    {
        share_slots(function);
    }
}

/// A set of blocks, by index.
#[derive(Clone)]
struct Blocks(Vec<u64>);

impl Blocks {
    fn new(n: usize) -> Self {
        Blocks(vec![0; n.div_ceil(64)])
    }

    fn insert(&mut self, i: usize) -> bool {
        let (word, bit) = (i / 64, 1u64 << (i % 64));
        let fresh = self.0[word] & bit == 0;
        self.0[word] |= bit;
        fresh
    }

    fn meets(&self, other: &Blocks) -> bool {
        self.0.iter().zip(&other.0).any(|(a, b)| a & b != 0)
    }

    fn intersect(&mut self, other: &Blocks) {
        self.0.iter_mut().zip(&other.0).for_each(|(a, b)| *a &= b);
    }

    fn union(&mut self, other: &Blocks) {
        self.0.iter_mut().zip(&other.0).for_each(|(a, b)| *a |= b);
    }
}

/// `function`'s control flow: its blocks' indices and edges both ways.
struct Graph<'ctx> {
    index: HashMap<BasicBlock<'ctx>, usize>,
    succs: Vec<Vec<usize>>,
    preds: Vec<Vec<usize>>,
}

impl<'ctx> Graph<'ctx> {
    fn new(function: FunctionValue<'ctx>) -> Self {
        let blocks = function.get_basic_blocks();
        let index: HashMap<BasicBlock, usize> =
            blocks.iter().enumerate().map(|(i, b)| (*b, i)).collect();
        let mut succs = vec![Vec::new(); blocks.len()];
        let mut preds = vec![Vec::new(); blocks.len()];
        for (i, block) in blocks.iter().enumerate() {
            for s in successors(*block) {
                let j = index[&s];
                succs[i].push(j);
                preds[j].push(i);
            }
        }
        Graph {
            index,
            succs,
            preds,
        }
    }

    /// The blocks reached from `from` through at least one edge.
    fn reach(&self, from: &[usize], edges: &[Vec<usize>]) -> Blocks {
        let mut seen = Blocks::new(edges.len());
        let mut stack: Vec<usize> = from
            .iter()
            .flat_map(|b| edges[*b].iter().copied())
            .collect();
        while let Some(b) = stack.pop() {
            if seen.insert(b) {
                stack.extend(edges[b].iter().copied());
            }
        }
        seen
    }

    /// Where a slot used in blocks `uses` may hold a value someone will
    /// still read: the use blocks, and every block on a path from one use to
    /// another. A value written in one use block and read in another is
    /// held across every block between them, loops included.
    fn live(&self, uses: &[usize]) -> Blocks {
        let mut live = self.reach(uses, &self.succs);
        live.intersect(&self.reach(uses, &self.preds));
        for b in uses {
            live.insert(*b);
        }
        live
    }
}

/// True if `call` is `llvm.lifetime.start` or `llvm.lifetime.end`.
fn lifetime_marker(call: InstructionValue<'_>) -> bool {
    CallSiteValue::try_from(call)
        .ok()
        .and_then(|site| site.get_called_fn_value())
        .is_some_and(|f| f.get_name().to_bytes().starts_with(b"llvm.lifetime."))
}

/// Shares `function`'s struct and array slots whose live blocks never meet.
/// Only entry-block slots qualify whose address is used just by loads,
/// stores to it, element addresses and calls other than lifetime markers:
/// one stored or merged into another pointer may be read anywhere, so it
/// keeps its own slot.
fn share_slots(function: FunctionValue<'_>) {
    let Some(entry) = function.get_first_basic_block() else {
        return;
    };
    let graph = Graph::new(function);
    phi::merge_phi_groups(function, &graph);
    // Per type: the shared slots so far, each with its live blocks.
    let mut shared: Vec<(BasicTypeEnum, Vec<(InstructionValue, Blocks)>)> = Vec::new();
    let slots: Vec<InstructionValue> = entry
        .get_instructions()
        .filter(|i| i.get_opcode() == InstructionOpcode::Alloca)
        .collect();
    for slot in slots {
        let Ok(ty) = slot.get_allocated_type() else {
            continue;
        };
        if !matches!(
            ty,
            BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)
        ) {
            continue;
        }
        let Some(uses) = use_blocks(slot, &graph, None) else {
            continue;
        };
        let live = graph.live(&uses);
        let group = match shared.iter_mut().find(|(t, _)| *t == ty) {
            Some((_, group)) => group,
            None => {
                shared.push((ty, Vec::new()));
                &mut shared.last_mut().expect("just pushed").1
            }
        };
        match group.iter_mut().find(|(_, taken)| !taken.meets(&live)) {
            Some((into, taken)) => {
                let align = slot.get_alignment().unwrap_or(0);
                if align > into.get_alignment().unwrap_or(0) {
                    into.set_alignment(align)
                        .expect("an alloca has an alignment");
                }
                slot.replace_all_uses_with(into);
                slot.erase_from_basic_block();
                taken.union(&live);
            }
            None => group.push((slot, live)),
        }
    }
}

/// The blocks using `slot`'s address, through element addresses, apart from
/// the instruction `skip`; `None` if the address is stored or flows anywhere
/// else.
fn use_blocks<'ctx>(
    slot: InstructionValue<'ctx>,
    graph: &Graph<'ctx>,
    skip: Option<InstructionValue<'ctx>>,
) -> Option<Vec<usize>> {
    let mut blocks: Vec<usize> = Vec::new();
    let mut pending = vec![slot];
    while let Some(address) = pending.pop() {
        let mut next = address.get_first_use();
        while let Some(u) = next {
            next = u.get_next_use();
            let user = match u.get_user() {
                AnyValueEnum::InstructionValue(i) => i,
                any => inkwell::values::BasicValueEnum::try_from(any)
                    .ok()
                    .and_then(|b| b.as_instruction_value())?,
            };
            if Some(user) == skip {
                continue;
            }
            match user.get_opcode() {
                InstructionOpcode::Load => {}
                // LLVM's stack colouring merges a slot with lifetime markers
                // (from -O2's inlining) by them: merged with another here,
                // its markers would no longer cover every use.
                InstructionOpcode::Call if lifetime_marker(user) => return None,
                InstructionOpcode::Call => {}
                // Stored *to*, not stored as a value somewhere.
                InstructionOpcode::Store => {
                    let value = user.get_operand(0).and_then(|o| o.value())?;
                    if value.as_instruction_value() == Some(address) {
                        return None;
                    }
                }
                InstructionOpcode::GetElementPtr => pending.push(user),
                _ => return None,
            }
            blocks.push(graph.index[&user.get_parent()?]);
        }
    }
    Some(blocks)
}

#[path = "frames_phi.rs"]
mod phi;

#[cfg(test)]
#[path = "frames_tests.rs"]
mod tests;
