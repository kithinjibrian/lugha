## FEATURE: `lughac build --target aarch64-unknown-linux-gnu` writes a Linux ARM64 executable, green threads included; milestone 133 (v17), portability roadmap steps B0 and B1.

**Status:** proposed 2026-10-24 (session 316). Discovery:
1. One milestone covers "hello from ARM": `--target`, LLVM for ARM, sizes from LLVM, and the ARM stack switch. Extern structs passed by value to or from C on ARM are a compile error until B2, never miscompiled.
2. With no `$LUGHA_CC`, the ARM target's compiler is `aarch64-linux-gnu-gcc`.
3. Only `build` takes `--target`; `run` and `test` wait for a later step.

## OBJECTIVE
On an x86-64 Linux machine with `gcc-aarch64-linux-gnu` installed, `lughac build app.la --target aarch64-unknown-linux-gnu` writes `app`, an ARM64 Linux executable. It prints the same output and exits with the same code under `qemu-aarch64 -L /usr/aarch64-linux-gnu` (or on a real ARM machine) as the x86-64 build does. That includes green threads, channels, actors, panics (exit 101) and the stack-overflow panic.

## CONTEXT
- **Experiments (session 316, scratchpad):**
  - `aarch64-linux-gnu-gcc` 15.2 compiles bdwgc (4.5 s) and the whole runtime, except `tasks.c`'s x86-64 assembly.
  - `qemu-aarch64` 10.2 runs ARM binaries with `-L /usr/aarch64-linux-gnu`; `binfmt_misc` is registered, so `QEMU_LD_PREFIX` lets them run directly.
  - No other runtime file needs a change for ARM: `-Wall` gives no warnings.
- **Starting state:**
  - `src/codegen/mod.rs` `target_machine(opt, cpu)` always uses the host triple and `Target::initialize_native`.
  - `src/codegen/split_leaves.rs` `byte_size` hard-codes 8-byte pointers ("on x86-64").
  - `runtime/tasks.c` has `lugha_rt_task_swap` and `lugha_rt_task_boot` in x86-64 assembly.
  - `runtime/spawn.c` lays out a new fiber's first frame for that assembly: MXCSR and x87 words, r15…rbp, the return address.
  - `src/codegen/c_abi.rs` and `c_calls.rs` implement only x86-64 System V for extern structs by value.
  - `link::compiler::Compiler::from_env` falls back to `cc`.
- **Ending state:**
  - **The `Target` type:** `codegen::Target` (`pub` enum), `X86_64Linux` (the default) or `Aarch64Linux`, with `triple()` giving `x86_64-unknown-linux-gnu` or `aarch64-unknown-linux-gnu`.
    - `target_machine(opt, cpu, target)` initialises that LLVM target (`Target::initialize_aarch64`, …), with CPU `generic` and no features on ARM.
    - `emit_object*` take the `Target`.
  - **The flag:** `--target` is a clap `value_enum` on `build` only, taking those two names. Any other is exit 2 listing them.
    - `--cpu` stays x86-64-only. Giving it with the ARM target is exit 2: "`--cpu` takes x86-64 levels; aarch64 builds for the baseline ARMv8-A".
  - **The compiler:** `Compiler::for_target(target)` gives `$LUGHA_CC` when set, else `cc` for x86-64 and `aarch64-linux-gnu-gcc` for ARM. The object cache keys on the words already (milestone 132), so ARM objects are cached apart.
  - **Sizes:** `byte_size` stays as it is. Both targets are LP64 with the same scalar sizes; the doc says so and `target_machine` asserts the data layout's pointer size is 8.
  - **The ARM stack switch:**
    - `runtime/tasks.c` keeps the x86-64 assembly under `#if defined(__x86_64__)` and adds an AArch64 one under `#elif defined(__aarch64__)`. It saves x19–x28, x29 (frame pointer), x30 (link register), d8–d15 and FPCR (the rounding mode, as MXCSR is saved today). `lugha_rt_task_boot` takes the fiber from x19.
    - Any other architecture is `#error`.
    - `runtime/spawn.c`'s first frame is laid out per architecture under the same `#if`s.
  - **The rejection:** on the ARM target, an extern function or callback taking or returning an extern struct or union by value is **E0449** at its declaration. It's checked in the driver before codegen, from what the checker records about extern signatures, with help naming B2's absence ("not supported on aarch64 yet; pass it by `ptr`, or build for x86-64"). Pointers to structs (`ptr`, `mut`, `out`, arrays) are fine.
- **Related existing code:** `src/codegen/mod.rs`, `src/codegen/split_leaves.rs`, `src/driver/mod.rs`, `src/driver/build.rs`, `src/driver/pipeline.rs`, `src/link/compiler.rs`, `src/link/mod.rs`, `runtime/tasks.c`, `runtime/spawn.c`, `src/check/callbacks.rs` (`extern_params`), `src/codegen/c_calls.rs`.
- **Spec:**
  - §9 gets a `--target` row and a short "Targets" paragraph: the two triples, the compiler default, and what ARM lacks (E0449).
  - §8 (C boundary) notes E0449.
  - §11 gets row 133, and §0 a line.
  - `docs/errors/E0449.md` is added.
  - Approving this PRP approves these edits.
- **Open decisions that must be resolved first:** none. B2 (ARM's struct rules) will lift E0449.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- The default target is x86-64 Linux, and its output is unchanged: the same IR, objects and compiler (`cc`).
- The AArch64 switch follows AAPCS64's callee-saved set exactly, with a comment citing it. The fiber's first frame leaves `sp` 16-byte aligned, as AAPCS64 requires.
- Restore FPCR on each switch, so a green thread's rounding mode is its own. `tests/tasks_runtime.rs` checks that on x86-64 today.
- Keep each touched file under 500 lines.
- Docs: README (building for ARM: the apt packages, the qemu line), CLAUDE.md (COMMANDS, STACK: two targets; rule 7 unchanged), CHANGELOG, `TODO.md` (B0 and B1 ticked).

### Must NOT Do
- No `--target` on `run` or `test`, and no emulator started by lughac (discovery answer 3).
- No guessing at ARM's struct rules. E0449 until B2.
- No other targets: macOS and Windows are stages C and D.
- No `pkg-config` changes. On a cross target `$PKG_CONFIG` is the user's to point at the target's files, and the README says so.
- Don't change the x86-64 assembly's behaviour; only wrap it in `#if`.

### New dependencies
- none. The cross gcc and qemu are user-installed tools, needed only for ARM builds and their tests.

## ERROR HANDLING REQUIREMENTS
- An unknown `--target`, or `--cpu` with ARM, is exit 2 (CLI usage).
- E0449 (exit 1, a `Diagnostic` at the declaration's span) for each extern struct or union by value on ARM. Every one is reported, not just the first.
- `aarch64-linux-gnu-gcc` missing: `LinkError::CcNotFound` names it (milestone 132), plus help: `sudo apt install gcc-aarch64-linux-gnu`.

## SECURITY CONSIDERATIONS
- `--target` is checked against a fixed list before reaching LLVM or the compiler choice.
- The compiler is chosen as in milestone 132; a new fixed default name is added, run without a shell.
- No new process kinds: lughac starts no emulator.
- No `unsafe` in Rust. The new assembly runs inside programs, as the x86-64 switch does.

## TESTS TO WRITE
- [ ] `src/codegen/mod.rs` units: `Target::default()` is x86-64; each target's machine has its triple, and ARM's has CPU `generic`; both data layouts have 8-byte pointers.
- [ ] `src/link/compiler.rs` units: `for_target` gives `cc` and `aarch64-linux-gnu-gcc` without `$LUGHA_CC`, and `$LUGHA_CC` with it.
- [ ] `tests/cli.rs`: `--target=sparc` is exit 2 listing both names; `--target aarch64-unknown-linux-gnu --cpu=native` is exit 2; `run --target …` is a usage error.
- [ ] `tests/arm.rs` (new), skipped with a printed note when `aarch64-linux-gnu-gcc` or `qemu-aarch64` is missing:
  - builds a fixed list of existing `tests/programs/` cases with `--target aarch64-unknown-linux-gnu`: arithmetic and printing; floats and formatting; strings and arrays; maps and sets; green threads (`m91`); channels and `select`; actors (`m92`); a panic (exit 101); deep recursion (the stack-overflow panic); `m131`'s allocating threads;
  - runs each under `qemu-aarch64 -L /usr/aarch64-linux-gnu` and compares stdout and exit code with the case's own expectation files;
  - checks the binary is ARM64 (ELF `e_machine` 183).
- [ ] `tests/arm.rs`: a program with `extern fun f(p: P): i64` for an `extern struct P` is E0449 on ARM and builds on x86-64; one with `ptr p: P` builds on both.
- [ ] `docs/errors/E0449.md`, with its example checked by `tests/explain.rs`.
- [ ] CI installs `gcc-aarch64-linux-gnu` and `qemu-user`, so `tests/arm.rs` runs there.

## ROLLBACK PLAN
- Branch to return to: `main`.
- State: host-only codegen and the x86-64 switch. Revert the branch.

## ACCEPTANCE CRITERIA
- [ ] The listed programs give the same stdout and exit code on ARM under qemu as on x86-64
- [ ] Green threads, channels, actors and the collector work on ARM
- [ ] E0449 for extern structs by value on ARM; nothing miscompiled
- [ ] x86-64 builds unchanged (existing suites pass)
- [ ] `cargo clippy --all-targets -- -D warnings` passes
- [ ] `cargo fmt --check` passes
- [ ] Every `pub` item has a doc comment
- [ ] CHANGELOG.md updated

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test --lib`, `--test arm`, `--test cli`, `--test explain`, `--test tasks_runtime`, `--test programs`, `--test codegen`, `--test extern_structs`, `--test callbacks`
