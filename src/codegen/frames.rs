//! Small stack frames (v17, milestone 135, PRP-167). Every function's
//! struct and array slots are shared: two slots of one type that never hold
//! a needed value at the same point — per block, down to the instruction
//! (milestone 136, PRP-168) — become one, so a frame costs its largest
//! match arm or branch, not the sum of them all. At `-O0` (and for `--emit=ir`) that
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

    fn contains(&self, i: usize) -> bool {
        self.0[i / 64] & (1u64 << (i % 64)) != 0
    }

    fn members(&self) -> impl Iterator<Item = usize> + '_ {
        (0..self.0.len() * 64).filter(|i| self.contains(*i))
    }
}

/// Where a slot may hold a value someone will still read: per block, the
/// first and last instruction positions, both included (PRP-168).
type Ranges = Vec<(usize, u32, u32)>;

/// The ranges a shared slot covers so far, per block.
#[derive(Default)]
struct Taken(HashMap<usize, Vec<(u32, u32)>>);

impl Taken {
    /// True if any of `ranges` overlaps a range taken in its block.
    fn meets(&self, ranges: &Ranges) -> bool {
        ranges.iter().any(|(b, start, end)| {
            self.0
                .get(b)
                .is_some_and(|taken| taken.iter().any(|(s, e)| s <= end && start <= e))
        })
    }

    fn add(&mut self, ranges: Ranges) {
        for (b, start, end) in ranges {
            self.0.entry(b).or_default().push((start, end));
        }
    }
}

/// `function`'s control flow: its blocks' indices and edges both ways.
struct Graph<'ctx> {
    index: HashMap<BasicBlock<'ctx>, usize>,
    succs: Vec<Vec<usize>>,
    preds: Vec<Vec<usize>>,
    /// Each instruction's position in its block, and each block's length.
    position: HashMap<InstructionValue<'ctx>, u32>,
    len: Vec<u32>,
}

impl<'ctx> Graph<'ctx> {
    fn new(function: FunctionValue<'ctx>) -> Self {
        let blocks = function.get_basic_blocks();
        let index: HashMap<BasicBlock, usize> =
            blocks.iter().enumerate().map(|(i, b)| (*b, i)).collect();
        let mut succs = vec![Vec::new(); blocks.len()];
        let mut preds = vec![Vec::new(); blocks.len()];
        let mut position = HashMap::new();
        let mut len = Vec::with_capacity(blocks.len());
        for (i, block) in blocks.iter().enumerate() {
            for s in successors(*block) {
                let j = index[&s];
                succs[i].push(j);
                preds[j].push(i);
            }
            let mut n = 0;
            for (at, inst) in block.get_instructions().enumerate() {
                position.insert(inst, at as u32);
                n = at as u32;
            }
            len.push(n);
        }
        Graph {
            index,
            succs,
            preds,
            position,
            len,
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

    /// [`Graph::live`] down to the instruction, for a slot used at `uses`
    /// (block, position). A block between uses holds the value throughout.
    /// In a block using the slot, it is held from the block's start if the
    /// block is reached from a use (a value may come in), else from its
    /// first use there; and to the block's end if it reaches a use (the
    /// value may be read later), else to its last use there.
    fn ranges(&self, uses: &[(usize, u32)]) -> Ranges {
        let blocks: Vec<usize> = uses.iter().map(|(b, _)| *b).collect();
        let reached = self.reach(&blocks, &self.succs);
        let reaching = self.reach(&blocks, &self.preds);
        let mut between = reached.clone();
        between.intersect(&reaching);
        let mut out: Ranges = between
            .members()
            .filter(|b| !blocks.contains(b))
            .map(|b| (b, 0, self.len[b]))
            .collect();
        let mut used: Vec<usize> = blocks.clone();
        used.sort_unstable();
        used.dedup();
        for b in used {
            let at = uses.iter().filter(|(ub, _)| *ub == b).map(|(_, p)| *p);
            let (first, last) = at.fold((u32::MAX, 0), |(f, l), p| (f.min(p), l.max(p)));
            let start = if reached.contains(b) { 0 } else { first };
            let end = if reaching.contains(b) {
                self.len[b]
            } else {
                last
            };
            out.push((b, start, end));
        }
        out
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
    // Per type: the shared slots so far, each with the ranges it covers.
    let mut shared: Vec<(BasicTypeEnum, Vec<(InstructionValue, Taken)>)> = Vec::new();
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
        let Some(uses) = uses_of(slot, &graph, None) else {
            continue;
        };
        let live = graph.ranges(&uses);
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
                taken.add(live);
            }
            None => {
                let mut taken = Taken::default();
                taken.add(live);
                group.push((slot, taken));
            }
        }
    }
}

/// The places (block, position) using `slot`'s address, through element
/// addresses, apart from the instruction `skip`; `None` if the address is
/// stored or flows anywhere else.
fn uses_of<'ctx>(
    slot: InstructionValue<'ctx>,
    graph: &Graph<'ctx>,
    skip: Option<InstructionValue<'ctx>>,
) -> Option<Vec<(usize, u32)>> {
    let mut places: Vec<(usize, u32)> = Vec::new();
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
            let at = *graph
                .position
                .get(&user)
                .expect("every instruction is numbered");
            places.push((graph.index[&user.get_parent()?], at));
        }
    }
    Some(places)
}

/// The blocks using `slot`'s address, as [`uses_of`] finds them.
fn use_blocks<'ctx>(
    slot: InstructionValue<'ctx>,
    graph: &Graph<'ctx>,
    skip: Option<InstructionValue<'ctx>>,
) -> Option<Vec<usize>> {
    Some(
        uses_of(slot, graph, skip)?
            .into_iter()
            .map(|(b, _)| b)
            .collect(),
    )
}

#[path = "frames_phi.rs"]
mod phi;

#[cfg(test)]
#[path = "frames_tests.rs"]
mod tests;
