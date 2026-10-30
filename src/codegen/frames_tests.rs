//! Tests of `frames.rs` (v17, milestone 135, PRP-167).

use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;

use super::{probe_stacks, share_all_slots};
use crate::codegen::Target;
use crate::codegen::test_util::{ir, ir_for};

/// `ir` parsed, the pass run and the module verified, printed back.
pub(super) fn run(ir: &str) -> String {
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
pub(super) fn slots(ir: &str, ty: &str) -> usize {
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

pub(super) const HEAD: &str = "%Big = type { [32 x i64] }\n%Other = type { [32 x i32] }\n\
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
