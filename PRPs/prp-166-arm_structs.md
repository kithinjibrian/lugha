## FEATURE: Extern structs go to and from C by value on ARM64 as AAPCS64 passes them, lifting E0449; milestone 134 (v17), portability roadmap step B2.

**Status:** proposed 2026-10-24 (session 318). Discovery: (1) the rules are verified against a real C compiler. `tests/extern_structs.rs`'s shapes move to `tests/common/` and also run in `tests/arm.rs`, with the C side built by `aarch64-linux-gnu-gcc` and the program run under qemu. The design follows AAPCS64 (Arm's "Procedure Call Standard for the Arm 64-bit Architecture") the way clang lowers it, so it was stated rather than asked.

## OBJECTIVE
Every program `tests/extern_structs.rs` runs on x86-64 builds with `--target aarch64-unknown-linux-gnu` and prints the same numbers under qemu as on x86-64. That covers each shape as an argument and as a result, big structs, full registers, and callbacks taking and returning structs. E0449 is retired: no target refuses extern structs by value any more.

## CONTEXT
- **Starting state:**
  - `src/codegen/c_abi.rs` (298 lines) classifies an extern struct the x86-64 System V way: `Passing::Registers(pieces)` (eightbytes as INTEGER or SSE) or `Passing::Memory`. `Free` counts the registers left.
  - `src/codegen/c_calls.rs` (418 lines) builds a `CSig` from that:
    - `Arg::Split` / `Arg::ByVal`;
    - `Ret::Split` / `Ret::Memory` (`sret`);
    - lowering calls (`c_sig_call`), callback entries (`c_sig_params`) and returns (`c_sig_return`).
  - Lowering doesn't know the target.
  - `src/codegen/targets.rs` reports E0449 on ARM, from `Checked::c_by_value`.
- **AAPCS64, as clang lowers it (the rules this PRP implements):**
  - **HFA:** a struct whose flattened scalars are 1–4 values of one floating type, all `f32` or all `f64`, is a homogeneous floating-point aggregate. As an argument and as a result it is `[N x float]` or `[N x double]`. LLVM puts the block in consecutive FP registers, or the whole block on the stack when too few are left.
  - **Small:** any other struct of 1–16 bytes is `i64` (≤ 8 bytes) or `[2 x i64]` (9–16 bytes). LLVM keeps a `[2 x i64]` together in a register pair, or puts it on the stack.
  - **Big:** a struct over 16 bytes, as an argument, goes in a copy the caller makes, passed as a plain `ptr` (no `byval`); the callee reads it in place. As a result it is `sret`, which LLVM passes in `x8`.
  - **No counting:** registers aren't counted by lughac on ARM. LLVM's AArch64 lowering does the "fits or goes on the stack" rule for arrays itself.
  - **Size mismatches:** a value narrower than its register (`i64` for a 4-byte struct) is moved through a temporary of the register's size, never by a store wider than the struct.
- **Ending state:**
  - `src/codegen/c_abi.rs` keeps the x86-64 rules.
  - `src/codegen/c_abi_arm.rs` (new) has `classify_arm(ty) -> ArmPassing`: `Floats(count, Piece::Float | Piece::Double)`, `Ints(1 | 2)` or `Indirect`.
  - `c_calls.rs` chooses by target.
    - `Arg` gains `Indirect` (a pointer to a fresh copy, no attribute).
    - `Split` and `Ret::Split` carry the ARM piece types (`[N x float]`, `i64`, `[2 x i64]`).
    - `Free` is used only on x86-64.
  - Lowering gets the `Target`: `lower::lower(…, target)`, kept on the code generator. `emit_ir*` take one too, so `build --emit=ir --target …` shows that target's signatures.
  - `codegen::by_value_errors` and `Checked::c_by_value` are removed. E0449 stays in the code tables as retired, like E0407: the code is never reused, and its page says it is retired (v17, milestone 134).
  - `tests/common/c_shapes.rs` holds `SIDE` (C) and `DECLS` (Lugha), and the programs, moved from `tests/extern_structs.rs`, which uses them unchanged.
- **Related existing code:** `src/codegen/c_abi.rs`, `src/codegen/c_calls.rs`, `src/codegen/callbacks.rs` (`lugha_cb_` trampolines and `lugha_cfn_` entries use `c_sig_params`), `src/codegen/lower.rs`, `src/codegen/mod.rs`, `src/codegen/targets.rs`, `src/check/env.rs`, `src/check/callbacks.rs`, `tests/extern_structs.rs`, `tests/arm.rs`.
- **Spec:**
  - §8 "Passing structs" gets the AArch64 rules beside x86-64's, and its E0449 sentence is removed.
  - §9's `--target` row loses its E0449 clause.
  - The error table marks E0449 retired.
  - §11 gets row 134, and §0 a line.
  - Approving this PRP approves these edits.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **HFA detection:** over the flattened scalars (nested structs included, as `c_abi::scalars` does). Every scalar is the same float type, there are 1–4 of them, and the struct has no padding beyond what those floats imply.
- **Small structs:** `i64` / `[2 x i64]` by the struct's size, with no special case for 16-byte alignment, since no extern struct field aligns past 8.
- **Big arguments:** the caller copies the struct to a fresh `alloca` and passes its address. The callee (a callback entry) copies out of the pointer it is given, so a Lugha callback never writes C's memory.
- **x86-64 unchanged:** the same IR for every existing program (the `extern_structs`, `callbacks` and `codegen` suites).
- Keep each file under 500 lines. `c_calls.rs` is at 418; the ARM lowering goes in `c_abi_arm.rs` where it can.
- **Docs:** README (the E0449 sentence goes), CLAUDE.md (STACK's target line), CHANGELOG, `TODO.md` (B2 ticked).

### Must NOT Do
- No guessing past AAPCS64: unions by value stay E0409 on both targets, as today.
- No register counting on ARM (LLVM does it). Don't reuse `Free` there.
- No `byval` on ARM: AAPCS64 passes big structs by plain pointer.
- No macOS ARM (Apple's variant differs for variadics and small integers): that's stage C.

### New dependencies
- none

## ERROR HANDLING REQUIREMENTS
- No new user-facing errors. E0449 can no longer be produced.
- A struct shape the classifier can't place (a zero-sized struct) is already refused by the checker (E0437). The classifier asserts that as an invariant, with `expect` naming it.

## SECURITY CONSIDERATIONS
- No new input, processes or `unsafe`. The generated code's memory is that of the struct being passed. The callee-side copy keeps a callback from writing through C's pointer.

## TESTS TO WRITE
- [ ] `c_abi_arm.rs` units, written before the code:
  - `{f32, f32}` → `Floats(2, Float)`; `{f64 ×4}` → `Floats(4, Double)`; `{f32 ×5}` → `Indirect` (20 bytes); `{f32, f64}` → `Ints(2)`;
  - nested `{V2, V2}` of `f32` → `Floats(4, Float)`;
  - `{u8 ×4}` → `Ints(1)`; `{i64, i32}` → `Ints(2)`; `{i64 ×3}` → `Indirect`.
- [ ] IR units: on ARM, an extern taking `{f32, f32}` is declared with `[2 x float]`, one taking a 24-byte struct with a plain `ptr` without `byval`, and one returning it with `sret`. On x86-64 the IR is unchanged (the existing IR tests).
- [ ] `tests/common/c_shapes.rs`: the shapes moved out of `tests/extern_structs.rs`, whose tests pass unchanged.
- [ ] `tests/arm.rs`: every `extern_structs` program built for ARM with the C side from `aarch64-linux-gnu-gcc`, run under qemu, matching the x86-64 expectations (arguments, results, big, full registers, callbacks both ways).
- [ ] `tests/arm.rs`: the E0449 test becomes "by value builds and runs on ARM".
- [ ] `tests/explain.rs`: E0449's page says retired, and the code stays listed.

## ROLLBACK PLAN
- Branch to return to: `main`.
- State: E0449 on ARM, as milestone 133 left it. Revert the branch.

## ACCEPTANCE CRITERIA
- [ ] Every extern-struct shape gives the same results on ARM (qemu, gcc's C) as on x86-64
- [ ] Callbacks taking and returning structs work on ARM
- [ ] x86-64 IR and behaviour unchanged
- [ ] E0449 retired, never reused
- [ ] `cargo clippy --all-targets -- -D warnings` passes
- [ ] `cargo fmt --check` passes
- [ ] Every `pub` item has a doc comment
- [ ] CHANGELOG.md updated

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test --lib`, `--test arm`, `--test extern_structs`, `--test callbacks`, `--test c_strings`, `--test out_params`, `--test c_arrays`, `--test c_memory`, `--test codegen`, `--test explain`, `--test spec`
