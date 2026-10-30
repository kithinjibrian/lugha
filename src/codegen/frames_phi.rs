//! Slots joined by a `phi` (v17, PRP-167, session 324). After `-O2`, LLVM
//! merges the arms' tails into one block that picks among the arms' slots
//! through a `phi` of their addresses, so to `use_blocks` each slot's
//! address escapes and none could be shared. Here such slots are merged
//! with each other first, when that is safe: the `phi` then picks one slot
//! whatever the path, and is replaced by it.

use inkwell::values::{BasicValue, FunctionValue, InstructionOpcode, InstructionValue, PhiValue};

use super::{Blocks, Graph, use_blocks};

/// Merges, in `function`, the slots each `phi` of slot addresses picks
/// from, and replaces the `phi` by the merged slot.
pub(super) fn merge_phi_groups(function: FunctionValue<'_>, graph: &Graph<'_>) {
    let phis: Vec<InstructionValue> = function
        .get_basic_blocks()
        .into_iter()
        .flat_map(|b| b.get_instructions().collect::<Vec<_>>())
        .filter(|i| i.get_opcode() == InstructionOpcode::Phi)
        .collect();
    for phi in phis {
        merge(phi, graph);
    }
}

/// Merges the slots `phi` picks from when nothing can tell them apart: each
/// slot's own live blocks — from its uses, and the block the `phi` takes it
/// from — meet no other's, nor the blocks from the `phi` to its last use.
/// Then a value one slot held when the `phi` picked it is still there, the
/// others never written on that path. `None` when anything doesn't qualify.
fn merge(phi: InstructionValue<'_>, graph: &Graph<'_>) -> Option<()> {
    let picks = PhiValue::try_from(phi).ok()?;
    let entry = phi.get_parent()?.get_parent()?.get_first_basic_block()?;
    // Each distinct slot, with the blocks using it other than through `phi`.
    let mut members: Vec<(InstructionValue, Vec<usize>)> = Vec::new();
    let mut ty = None;
    for i in 0..picks.count_incoming() {
        let (value, from) = picks.get_incoming(i)?;
        let slot = value
            .as_instruction_value()
            .filter(|s| s.get_opcode() == InstructionOpcode::Alloca)
            .filter(|s| s.get_parent() == Some(entry))?;
        let slot_ty = slot.get_allocated_type().ok()?;
        if ty.is_some_and(|t| t != slot_ty) {
            return None;
        }
        ty = Some(slot_ty);
        let from = graph.index[&from];
        match members.iter_mut().find(|(s, _)| *s == slot) {
            Some((_, uses)) => uses.push(from),
            None => {
                let mut uses = use_blocks(slot, graph, Some(phi))?;
                uses.push(from);
                members.push((slot, uses));
            }
        }
    }
    let mut read = use_blocks(phi, graph, None)?;
    read.push(graph.index[&phi.get_parent()?]);
    let read = graph.live(&read);
    let lives: Vec<Blocks> = members.iter().map(|(_, uses)| graph.live(uses)).collect();
    for (i, live) in lives.iter().enumerate() {
        if live.meets(&read) || lives[i + 1..].iter().any(|other| live.meets(other)) {
            return None;
        }
    }
    let (into, _) = *members.first()?;
    for (slot, _) in &members[1..] {
        let align = slot.get_alignment().unwrap_or(0);
        if align > into.get_alignment().unwrap_or(0) {
            into.set_alignment(align)
                .expect("an alloca has an alignment");
        }
        slot.replace_all_uses_with(&into);
        slot.erase_from_basic_block();
    }
    phi.replace_all_uses_with(&into);
    phi.erase_from_basic_block();
    Some(())
}

#[cfg(test)]
mod tests {
    use super::super::tests::{HEAD, run, slots};

    /// Two arms filling their own slot, a tail picking one by `phi`, as -O2
    /// leaves them; `after` follows the tail's read.
    fn joined(after: &str) -> String {
        format!(
            "{HEAD}define void @f(i1 %c) {{
entry:
  %a = alloca %Big
  %b = alloca %Big
  br i1 %c, label %one, label %two
one:
  call void @use(ptr %a)
  br label %tail
two:
  call void @use(ptr %b)
  br label %tail
tail:
  %p = phi ptr [ %a, %one ], [ %b, %two ]
  call void @use(ptr %p)
{after}  ret void
}}"
        )
    }

    #[test]
    fn slots_meeting_only_in_a_phi_become_one() {
        let out = run(&joined(""));
        assert_eq!(slots(&out, "%Big"), 1, "{out}");
        assert!(!out.contains(" = phi "), "{out}");
    }

    #[test]
    fn a_slot_used_again_after_the_phi_keeps_the_group_apart() {
        // `a` is read after the tail: merged, `b`'s value could replace it.
        let out = run(&joined("  call void @use(ptr %a)\n"));
        assert_eq!(slots(&out, "%Big"), 2, "{out}");
    }

    #[test]
    fn a_slot_written_on_the_way_to_another_ones_pick_keeps_them_apart() {
        // `b` is written in `two`, on the path along which the `phi` later
        // picks `a` (from `three`): merged, `a`'s value would be lost.
        let ir = format!(
            "{HEAD}define void @f(i1 %c) {{
entry:
  %a = alloca %Big
  %b = alloca %Big
  call void @use(ptr %a)
  br i1 %c, label %two, label %four
two:
  call void @use(ptr %b)
  br label %three
three:
  br label %tail
four:
  call void @use(ptr %b)
  br label %tail
tail:
  %p = phi ptr [ %a, %three ], [ %b, %four ]
  call void @use(ptr %p)
  ret void
}}"
        );
        let out = run(&ir);
        assert_eq!(slots(&out, "%Big"), 2, "{out}");
    }

    #[test]
    fn a_phi_of_other_pointers_or_types_is_left_alone() {
        let ir = format!(
            "{HEAD}define void @f(i1 %c, ptr %q) {{
entry:
  %a = alloca %Big
  %b = alloca %Other
  br i1 %c, label %one, label %two
one:
  call void @use(ptr %a)
  br label %tail
two:
  call void @use(ptr %b)
  br label %tail
tail:
  %p = phi ptr [ %a, %one ], [ %q, %two ]
  %r = phi ptr [ %a, %one ], [ %b, %two ]
  call void @use(ptr %p)
  call void @use(ptr %r)
  ret void
}}"
        );
        let out = run(&ir);
        assert_eq!((slots(&out, "%Big"), slots(&out, "%Other")), (1, 1));
        assert_eq!(out.matches(" = phi ").count(), 2, "{out}");
    }
}
