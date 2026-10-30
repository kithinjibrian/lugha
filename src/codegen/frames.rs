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
        let Some(uses) = use_blocks(slot, &graph) else {
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

/// The blocks using `slot`'s address, through element addresses; `None` if
/// the address is stored or flows anywhere else.
fn use_blocks(slot: InstructionValue<'_>, graph: &Graph<'_>) -> Option<Vec<usize>> {
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

#[cfg(test)]
mod tests {
    use inkwell::context::Context;
    use inkwell::memory_buffer::MemoryBuffer;
    use inkwell::module::Module;

    use super::{probe_stacks, share_all_slots};
    use crate::codegen::Target;
    use crate::codegen::test_util::{ir, ir_for};

    /// `ir` parsed, the pass run and the module verified, printed back.
    fn run(ir: &str) -> String {
        let context = Context::create();
        let buffer = MemoryBuffer::create_from_memory_range_copy(ir.as_bytes(), "test");
        let module: Module = context
            .create_module_from_ir(buffer)
            .expect("the test's IR parses");
        share_all_slots(&module);
        probe_stacks(&context, &module);
        module.verify().expect("the module verifies");
        module.print_to_string().to_string()
    }

    /// The number of `alloca`s of `ty` in `ir`.
    fn slots(ir: &str, ty: &str) -> usize {
        ir.lines()
            .filter(|l| {
                l.contains(&format!("= alloca {ty},")) || l.ends_with(&format!("= alloca {ty}"))
            })
            .count()
    }

    /// Twelve arms, each building a value holding a 1 KB struct (PRP-167).
    fn twelve_arms() -> String {
        let fields: String = (0..128).map(|i| format!("g{i}: i64, ")).collect();
        let variants: String = (0..12).map(|i| format!("A{i}(V, Big), ")).collect();
        let arms: String = (0..12)
            .map(|i| format!("A{i}(x, b) => V.A{}(walk(x), b), ", (i + 1) % 12))
            .collect();
        format!(
            "struct Big {{ {fields} }}\nenum V {{ Leaf, {variants} }}\n\
             fun walk(v: V): V = match v {{ {arms} _ => v }};\n\
             fun main() {{ let w = walk(V.Leaf); }}\n"
        )
    }

    /// The `alloca`s of `%V` in `lugha_fn_walk`.
    fn walk_slots(ir: &str) -> usize {
        let start = ir.find("@lugha_fn_walk(").expect("walk is defined");
        let body = &ir[start..start + ir[start..].find("\n}\n").expect("walk ends")];
        slots(body, "%V")
    }

    #[test]
    fn a_match_costs_its_largest_arm() {
        // Was 63: five slots in each of twelve arms, and three outside them.
        let n = walk_slots(&ir(&twelve_arms()));
        assert!(n <= 8, "{n} slots of %V");
    }

    #[test]
    fn arm64_functions_share_slots_and_probe_too() {
        let ir = ir_for(&twelve_arms(), Target::Aarch64Linux);
        assert!(walk_slots(&ir) <= 8);
        assert!(ir.contains("\"probe-stack\"=\"inline-asm\""));
    }

    const HEAD: &str = "%Big = type { [32 x i64] }\n%Other = type { [32 x i32] }\n\
        declare void @use(ptr)\n";

    #[test]
    fn slots_of_different_arms_are_shared() {
        let ir = format!(
            "{HEAD}define void @f(i1 %c) {{
entry:
  %a = alloca %Big
  %b = alloca %Big
  br i1 %c, label %one, label %two
one:
  call void @use(ptr %a)
  br label %end
two:
  call void @use(ptr %b)
  br label %end
end:
  ret void
}}"
        );
        assert_eq!(slots(&run(&ir), "%Big"), 1);
    }

    #[test]
    fn slots_used_in_one_block_stay_apart() {
        let ir = format!(
            "{HEAD}define void @f() {{
entry:
  %a = alloca %Big
  %b = alloca %Big
  call void @use(ptr %a)
  call void @use(ptr %b)
  ret void
}}"
        );
        assert_eq!(slots(&run(&ir), "%Big"), 2);
    }

    #[test]
    fn a_slot_live_across_another_stays_apart() {
        // `a` is written before `mid` and read after it.
        let ir = format!(
            "{HEAD}define void @f() {{
entry:
  %a = alloca %Big
  %b = alloca %Big
  br label %first
first:
  store %Big zeroinitializer, ptr %a
  br label %mid
mid:
  call void @use(ptr %b)
  br label %last
last:
  call void @use(ptr %a)
  ret void
}}"
        );
        assert_eq!(slots(&run(&ir), "%Big"), 2);
    }

    #[test]
    fn slots_in_one_loop_stay_apart_and_after_it_are_shared() {
        // `a` and `b` both in the loop: each block reaches the other. `c`
        // only after the loop ends shares with one of them.
        let ir = format!(
            "{HEAD}define void @f(i1 %c) {{
entry:
  %a = alloca %Big
  %b = alloca %Big
  %c2 = alloca %Big
  br label %head
head:
  call void @use(ptr %a)
  br label %body
body:
  call void @use(ptr %b)
  br i1 %c, label %head, label %exit
exit:
  call void @use(ptr %c2)
  ret void
}}"
        );
        assert_eq!(slots(&run(&ir), "%Big"), 2);
    }

    #[test]
    fn element_addresses_count_as_uses() {
        // `a`'s field is written in `first` through a GEP and read in `last`.
        let ir = format!(
            "{HEAD}define i64 @f() {{
entry:
  %a = alloca %Big
  %b = alloca %Big
  br label %first
first:
  %at = getelementptr %Big, ptr %a, i32 0, i32 0, i32 3
  store i64 7, ptr %at
  br label %mid
mid:
  call void @use(ptr %b)
  br label %last
last:
  %again = getelementptr %Big, ptr %a, i32 0, i32 0, i32 3
  %v = load i64, ptr %again
  ret i64 %v
}}"
        );
        assert_eq!(slots(&run(&ir), "%Big"), 2);
    }

    #[test]
    fn a_slot_whose_address_is_stored_is_never_shared() {
        let ir = format!(
            "{HEAD}define void @f(i1 %c, ptr %out) {{
entry:
  %a = alloca %Big
  %b = alloca %Big
  br i1 %c, label %one, label %two
one:
  store ptr %a, ptr %out
  br label %end
two:
  call void @use(ptr %b)
  br label %end
end:
  ret void
}}"
        );
        assert_eq!(slots(&run(&ir), "%Big"), 2);
    }

    #[test]
    fn slots_of_different_types_and_scalars_are_not_shared() {
        let ir = format!(
            "{HEAD}define void @f(i1 %c) {{
entry:
  %a = alloca %Big
  %b = alloca %Other
  %x = alloca i64
  %y = alloca i64
  br i1 %c, label %one, label %two
one:
  call void @use(ptr %a)
  store i64 1, ptr %x
  br label %end
two:
  call void @use(ptr %b)
  store i64 2, ptr %y
  br label %end
end:
  ret void
}}"
        );
        let out = run(&ir);
        assert_eq!(
            (
                slots(&out, "%Big"),
                slots(&out, "%Other"),
                slots(&out, "i64")
            ),
            (1, 1, 2)
        );
    }

    #[test]
    fn slots_with_lifetime_markers_are_left_to_llvm() {
        // After -O2's inlining: LLVM's own stack colouring merges these by
        // their markers, which must keep describing every use of the slot.
        let ir = format!(
            "{HEAD}declare void @llvm.lifetime.start.p0(i64, ptr)
declare void @llvm.lifetime.end.p0(i64, ptr)
define void @f(i1 %c) {{
entry:
  %a = alloca %Big
  %b = alloca %Big
  br i1 %c, label %one, label %two
one:
  call void @llvm.lifetime.start.p0(i64 256, ptr %a)
  call void @use(ptr %a)
  call void @llvm.lifetime.end.p0(i64 256, ptr %a)
  br label %end
two:
  call void @use(ptr %b)
  br label %end
end:
  ret void
}}"
        );
        assert_eq!(slots(&run(&ir), "%Big"), 2);
    }

    #[test]
    fn a_shared_slot_keeps_the_largest_alignment() {
        let ir = format!(
            "{HEAD}define void @f(i1 %c) {{
entry:
  %a = alloca %Big, align 8
  %b = alloca %Big, align 32
  br i1 %c, label %one, label %two
one:
  call void @use(ptr %a)
  br label %end
two:
  call void @use(ptr %b)
  br label %end
end:
  ret void
}}"
        );
        assert!(run(&ir).contains("alloca %Big, align 32"));
    }

    #[test]
    fn defined_functions_probe_their_stack_and_declarations_dont() {
        let out = run(&format!("{HEAD}define void @f() {{\n  ret void\n}}"));
        let define = out
            .lines()
            .find(|l| l.starts_with("define void @f"))
            .expect("@f");
        let group = define
            .split_whitespace()
            .find_map(|w| w.strip_prefix('#'))
            .expect("@f has attributes");
        assert!(
            out.contains(&format!(
                "attributes #{group} = {{ \"probe-stack\"=\"inline-asm\" }}"
            )),
            "{out}"
        );
        assert!(out.lines().any(|l| l == "declare void @use(ptr)"), "{out}");
    }
}
