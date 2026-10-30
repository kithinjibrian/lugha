## FEATURE: A function's stack slots are shared between match arms, branches and blocks that never run together, and every function probes its stack, so deep recursion through big values fits the stack or panics with `stack overflow`, never a segfault; milestone 135 (v17).

**Status:** approved 2026-10-24 ("build", session 321); proposed 2026-10-24 (session 320). Requested by Rapidkoi (its DECISION-059, 2026-10-24): drawing Notes 0.3.0 segfaults in kivuli's `view/lazies/resolve`, whose frame is 276 KB at -O0 (122 KB at -O2). Discovery:
1. Fix it in lughac rather than kivuli. The cause is general (any enum holding a big struct, matched over many arms), and kivuli has no clean workaround: there is no `Box<T>`, and `Ref<T>` is shared and not `Eq`.
2. Scope: slot sharing plus stack probes. Storing large enum payloads behind a pointer is left out, and becomes a possible PRP-168 decided from measurements taken after this one.

## RESULT (session 321)
Built, with one change of approach: slots are shared by one pass over each lowered function (`src/codegen/frames.rs`, after `sret.rs` and `split.rs`), not by a pool in `scope.rs` during lowering. Half of each arm's slots are made by those later passes (`sret`'s `result` slots, `split_big.rs`'s `copy` slots), which a lowering-time pool never sees.
- **How sharing works:**
  - A slot's *live blocks* are the blocks using its address (through GEPs), plus every block on a path from one use to another, loops included.
  - Two entry-block slots of the same struct or array type share one `alloca` (keeping the larger alignment) when their live blocks are disjoint.
  - A slot whose address is stored, or flows into a `phi`, `select` or anything other than a load, store, GEP or call, keeps its own slot.
  - Scalars aren't shared. This covers the PRP's "never pooled" list: arrays and aliases are pointer-typed slots, and an escaping address keeps its slot.
- **Probes:** `"probe-stack"="inline-asm"` on every function the module defines, on both targets.
- **Measured** (`sub …, %rsp` in the prologue, summed over the probe steps):

| function | -O0 before | -O0 after | -O2 before | -O2 after |
|---|---|---|---|---|
| kivuli `resolve<()>` (`examples/backgrounds.la`) | 276 KB (0x43688) | 8.0 KB (0x1f48) | 122 KB | 6.3 KB (0x18c8) |
| kivuli `resolve_children<()>` | 10 KB | 10.6 KB (0x2968) | 7.6 KB | 7.8 KB (0x1e88) |
| the 12-arm toy's `walk` | 94 KB (0x16fc8) | 11.3 KB (0x2c28) | 26 KB (0x6568) | 3.9 KB (0xf30) |

  At 64 levels with two `resolve` frames and one `resolve_children` per level, that is about 1.7 MB at -O0. Boxing large payloads (a possible PRP-168) isn't needed for Rapidkoi's depth.
- **Tests:**
  - 11 unit tests in `frames.rs`: hand-written IR for arms, one block, live across, loops, GEPs, escapes, types and scalars, alignment, probes; plus the 12-arm match at ≤ 8 `%V` slots on x86-64 and ARM64.
  - `tests/programs/m135/`: `deep_match`, `arm_results` (checked against a Python model), `big_frame_overflow`, `green_big_frame_overflow`. With the pass switched off, `deep_match` overflows, the green overflow segfaults (exit 139), and `big_frame_overflow` (200 KB frames) segfaults.

## OBJECTIVE
A function's frame costs its largest arm or branch, not the sum of all of them, at -O0 and -O2 alike. kivuli's `resolve` frame falls from 276 KB to an estimated ~40 KB at -O0, so a 64-deep Rapidkoi screen fits the main thread's 8 MB stack. Recursion that does run out of stack prints `panic: stack overflow` and exits 101, however large its frames, on the main stack and on green threads. Lugha programs and their output don't change.

## CONTEXT
- **Measured (session 320):**
  - kivuli's `resolve<()>` (from `examples/backgrounds.la`, `--emit=ir -O0`) has 107 `View` slots and 80 `Option<View>` slots in its entry block. One `View` is `{ i32, [323 x i64] }`, 2.6 KB, because `View.Styled` holds a `Style` (with two `Sheet`s) inline.
  - A toy program (12 arms, each `A{i}(x, b) => V.A{j}(walk(x, n - 1), b)`, `Big` = 128 × `i64`) gives `walk` 63 `%V` and 24 `%Big` slots: a 94 KB frame at -O0, 26 KB at -O2. lughac emits no `llvm.lifetime` markers, so LLVM merges nothing even at -O2.
  - `runtime/stack.c` counts a fault as an overflow only within 64 KiB (`GUARD_WINDOW`) of the main stack's end. A frame over 64 KiB can first touch memory past that window, which is a plain SIGSEGV. On a green thread (64 KiB of guard pages, spec §7) such a frame can step over the guard pages into another mapping.
- **Starting state:**
  - `src/codegen/scope.rs` (93 lines): `entry_alloca` puts every slot at the top of the entry block and never reuses one; `Scopes` pushes and pops name scopes only.
  - The 62 `entry_alloca` call sites across codegen: locals, pattern bindings, temporaries, `sret` result slots.
  - Match lowering in `matching.rs` (257), `patterns.rs` (306), `or_patterns.rs`; branches and blocks in `control.rs` (301) and `conditional.rs` (146); function declaration in `declare.rs` (220).
  - `hoist.rs` / `places.rs`: slots whose address outlives the expression that made them (hoisted arrays, `for … of mut` aliases).
- **Ending state:**
  - `scope.rs` keeps a pool of free slots per LLVM type. A *region* (a match arm, an `if`/`else` branch, an `if let`/`while let` body, a `{ }` block, a loop body) frees the slots it allocated when it ends, so the next region reuses them.
  - Every Lugha function, closure body, task entry and actor handler carries LLVM's `"probe-stack"="inline-asm"`, so its frame is touched one page at a time from the top.
  - Spec §7 **Stack** and **Recursion depth** paragraphs updated (text below); §11 row 135.
  - `tests/programs/m135/`, the slot-count IR test, scope.rs unit tests.
- **Related existing code:** `src/codegen/scope.rs`, `matching.rs`, `patterns.rs`, `control.rs`, `conditional.rs`, `declare.rs`, `hoist.rs`, `places.rs`, `sret.rs`, `split.rs`; `runtime/stack.c`, `runtime/tasks.c` (guard pages).
- **Spec:** Language v17 Specification §7 (Stack, Green threads' stacks, Recursion depth), §5 (panics: `stack overflow`).
- **Open decisions that must be resolved first:** none.

### Proposed spec text (needs approval with this PRP)
- §7 **Stack**, after its first sentence: "Slots are shared: a slot used only inside one match arm, `if`/`else` branch, `if let`/`while let` body, block or loop body is reused by the next such region that needs a slot of the same type, since two of them are never live at once. A function's frame is therefore as large as its largest region, not the sum of them (v17, milestone 135)."
- §7 **Recursion depth**, replacing "The runtime detects it when the stack's guard area is hit, so ordinary calls cost nothing.": "The runtime detects it when the stack's guard area is hit. Every Lugha function probes its frame a page at a time as it enters (LLVM's inline stack probes, v17, milestone 135), so a frame of any size reaches the guard area before memory past it; a frame under one page costs nothing extra."

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Regions.** Lowering a match arm (guard, bindings and body), each branch of `if`/`else` and `if let`, the body of `while`, `while let`, `for … of` and `loop`, and a block expression opens a region. Slots allocated while it is open go back to the pool when it closes. The innermost region owns a slot.
- **Free only after the result is out.** A region closes after its value has been loaded or stored into a slot of the enclosing region. A slot is never handed out while an LLVM value still points into it. Where a region's result is a pointer to one of its own slots, that slot belongs to the enclosing region instead.
- **Same type only.** A freed slot is reused only for the identical LLVM type, keeping alignment and the split/`sret` passes' view of each slot unchanged.
- **Never pooled:** slots whose address outlives their region. That covers hoisted arrays (`hoist.rs`), `for … of mut` aliases and `places.rs` write plans, `out` argument slots, and anything a closure environment or task preset points at. These are allocated as today.
- **Loops:** a loop body's slots are freed when the loop ends, not per iteration (they already live in the entry block, so iterations reuse them as today).
- **Probes:** add `"probe-stack"="inline-asm"` to every function lughac defines (Lugha functions, methods, instances, closures, `lugha_ref_`/`lugha_task_`/`lugha_actor_` entries, callback entries, C `main`) on both targets. `runtime/stack.c`'s 64 KiB window is left as it is: with probes, the first fault is always within a page of the stack's end.
- **Measure and record** in the PRP's RESULT: `resolve<()>`'s slot counts and `sub …, %rsp` at -O0 and -O2 (kivuli `examples/backgrounds.la`), and the toy's, before and after.
- Spec §7 text above, §11 row 135, CHANGELOG, CLAUDE.md (milestone list), README if it describes frames, TODO.

### Must NOT Do
- No change to value layout: enum payloads stay inline (boxing large payloads is the separate, later PRP-168, decided from measurements). *Why:* it changes copying, GC pressure and every payload access; one PRP at a time.
- No change to Lugha syntax, semantics, output or diagnostics; no new error codes.
- Don't rely on `llvm.lifetime.start/end` alone. *Why:* LLVM's stack colouring doesn't run at -O0, the default build and the one Rapidkoi crashes in.
- Don't share slots of different types through casts or byte arrays. *Why:* it breaks `split.rs`/`sret.rs`, which read slot types.
- Don't grow `runtime/stack.c`'s window or the main stack's size as the fix. *Why:* both only move the cliff.
- Don't touch kivuli or Rapidkoi.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- No new error paths. A region closed twice, or a slot released by a region that doesn't own it, is a compiler bug: `.expect("a region is closed once, by its owner")`, exit 2 through the guard.
- `module.verify()` must still pass for every program, -O0 and -O2.

## SECURITY CONSIDERATIONS
- No external input, `unsafe`, crypto or credentials.
- Probes are a safety improvement: without them a big frame on a green thread can step over its guard pages into other memory.
- The real risk is a slot reused while still in use, which would be silent memory corruption. That's why the "never pooled" list is conservative, and why the tests below run every existing program at -O0 and -O2.

## TESTS TO WRITE
- [ ] Unit (`scope.rs`): a released slot is handed out again for the same type; never for another type; a slot from an outer region isn't freed when an inner one closes; nested regions free in order.
- [ ] IR (`tests/codegen.rs`): the 12-arm toy's `walk` has at most the largest arm's worth of `%V` slots (≤ 8, was 63) at `--emit=ir -O0`.
- [ ] IR: a function with a hoisted array or a `for … of mut` alias inside a match arm keeps that slot out of the pool (the next arm gets a fresh one).
- [ ] E2E `tests/programs/m135/deep_match.la`: the toy shape recursing 400 deep prints its result (needs ~37 MB today, which overflows 8 MB; ~3 MB after).
- [ ] E2E `m135/big_frame_overflow.la`: unbounded recursion in a function holding about 100 KB of simultaneously live struct locals → stderr `panic: stack overflow`, exit 101 (today: SIGSEGV).
- [ ] E2E `m135/green_big_frame_overflow.la`: the same inside `spawn` → `panic: stack overflow`, exit 101.
- [ ] E2E `m135/arm_results.la`: values from different arms and branches flowing into one result, through nested matches inside loops, with a value built in one arm read after a later match: output unchanged.
- [ ] `tests/arm.rs` runs the m135 cases under qemu (probes on AArch64).
- [ ] Existing: `cargo test --lib`, `--test programs`, `--test codegen`, `--test spec`, `--test tasks_runtime`, `--test arm`. The full `cargo test` too, since this reaches every function's codegen.

## ROLLBACK PLAN
- Branch to return to: `main` at `e9420c8`.
- State: slots allocated once each in the entry block as today, and no probe attribute. The work lives only on `prp-167-small_frames` until merged.

## ACCEPTANCE CRITERIA
- [ ] kivuli's `resolve<()>` frame at -O0 is under 64 KB (from 276 KB), with the before/after numbers in RESULT.
- [ ] The toy's `walk` frame at -O0 is under 12 KB (from 94 KB).
- [ ] Every m135 case passes on x86-64 and under qemu on ARM64.
- [ ] All existing tests pass (full `cargo test`).
- [ ] New tests written and passing.
- [ ] `cargo clippy --all-targets -- -D warnings` passes.
- [ ] `cargo fmt --check` passes.
- [ ] Every `pub` item has a doc comment; no file over 500 lines.
- [ ] CHANGELOG.md updated; spec §7 and §11 updated as approved.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `objdump -d` on the toy and on kivuli's example: `sub $N,%rsp` in `resolve`/`walk`.
- Rapidkoi's `./main rapidkoi://notes.dafifi.net`, rebuilt with the new lughac (the user, by hand; it needs a display).

## BENCHMARK (session 322)
`m135`'s release `lughac` against the one before it (`e9420c8`), medians of 10 runs (runtime) and 5 builds (3 for kivuli). The programs were `match_heavy` (12 arms building 1 KB values, 40 deep, 3000 times), `fib(32)` and `structs` (small structs through matches, 3 million times).

| | old | m135 |
|---|---|---|
| `match_heavy` run, -O0 | 0.253 s | **0.159 s** |
| `match_heavy` run, -O2 | 0.091 s | 0.087 s |
| `fib` run, -O0 / -O2 | 0.027 / 0.014 s | 0.029 / 0.014 s |
| `structs` run, -O0 / -O2 | 0.149 / 0.085 s | 0.146 / 0.084 s |
| kivuli `backgrounds.la` build, -O0 | 5.6 s | **5.0 s** |
| kivuli `backgrounds.la` build, -O2 | 33.0 s | **50.4 s** (regression) |
| `match_heavy` build, -O2 | 0.23 s | **0.67 s** (regression) |

Runtime is the same or better, and -O0 builds are faster. **-O2 builds are slower, and the slot sharing is the cause.** Switching each part off in turn: without sharing, `match_heavy` builds at -O2 in 0.23–0.25 s; without probes, it still takes 0.58–1.46 s. LLVM's -O2 optimizer works harder on the merged slots.

**Tried, not committed:** at -O2, sharing slots after LLVM's optimizer instead of before it. `match_heavy` then builds in 0.23–0.25 s and kivuli in 35–37 s. Frames stay small: `walk` 3 KB (was 3.9 KB with sharing before the optimizer), `resolve` 6.8 KB, `resolve_children` 7.6 KB. -O0 keeps sharing right after lowering, as now.

## FIX (session 323)
Approved ("yes make the fix") and built:
- **When slots are shared:** `frames.rs` now has `probe_stacks`, called by `lower`, and `share_all_slots`. `emit_object_modules` calls `share_all_slots` before `run_passes` at -O0 and after them at -O2; `--emit=ir` shares right after lowering.
- **Lifetime markers:** after -O2's inlining, a slot with `llvm.lifetime` markers is never shared. LLVM's own stack colouring merges such slots by their markers, so merging one with another slot here would leave markers that no longer cover every use.
- **Tests:**
  - a frames unit test for the lifetime-marker rule;
  - `tests/codegen.rs` `optimised_frames_share_their_slots_v17`: `deep_match` at 1500 levels, built at -O2. It overflows without sharing at -O2.
- **Measured** (old, m135 and the fix alternating under the same load; medians):

| build | before m135 | m135 | fix |
|---|---|---|---|
| `match_heavy` -O2 | 0.24 s | 0.61 s | **0.24 s** |
| kivuli `backgrounds.la` -O2 | 36.2 s | 57.4 s | **28.8 s** |
| kivuli `backgrounds.la` -O0 | 4.04 s | 4.08 s | **3.73 s** |

  Frames with the fix: `resolve` 6.5 KB (-O0) / 6.8 KB (-O2), `resolve_children` 10.4 / 7.6 KB, `walk` 9.2 / 3.0 KB.

## PHI GROUPS AND CORRECTED FRAME SIZES (session 324)
- **Measurement error:** from session 321 on, frame sizes were read by adding the prologue's `sub $N,%rsp`s. For a frame of many pages, LLVM's stack probe is a loop (`sub $L,%r11`, then `sub $0x1000,%rsp` until `%rsp` reaches `%r11`), and that sum counted one 4 KB step instead of `L`. So **every large frame above was undercounted**, `resolve` most of all. The corrected script adds the loop's bound.
- **`phi` groups (the user chose "merge phi groups"):**
  - After -O2, LLVM merges the arms' tails into one block that picks among the arms' `copy` slots through a `phi`, and `use_blocks` saw those slots as escaping.
  - `frames_phi.rs` merges the slots one `phi` picks from when each slot's own live blocks (its uses, plus the block the `phi` takes it from) meet no other's, nor the blocks from the `phi` to its last use. The `phi` is then replaced by the merged slot.
  - Unit tests: merged; kept apart when a slot is used after the `phi`; kept apart when one slot is written on the path to another's pick; other pointers and other types left alone.
  - `frames.rs` was split per the user: tests in `frames_tests.rs`, the `phi` rule in `frames_phi.rs`.
- **Corrected frames** (kivuli `examples/backgrounds.la`, and `match_heavy`):

| function | before m135 | m135 | fix |
|---|---|---|---|
| `resolve<()>` -O0 | 313.4 KB | 66.5 KB | 66.5 KB |
| `resolve<()>` -O2 | 124.9 KB | 40.8 KB | **30.5 KB** |
| `resolve_children<()>` -O0 / -O2 | 10.4 / 7.6 KB | 10.4 / 7.6 KB | 10.4 / 7.6 KB |
| `walk` -O0 / -O2 | 87.1 / 25.4 KB | 9.2 / 4.0 KB | 9.2 / 3.0 KB |
| `build` -O0 / -O2 | 67.6 / 12.2 KB | 10.0 / 1.8 KB | 10.0 / **1.0** KB |

  `deep_match` at 1500 levels now runs at -O2 (`tests/codegen.rs`).
- **Not yet enough for Rapidkoi at -O0.** A screen level costs two `resolve` frames and one `resolve_children`, about 143 KB at -O0, so 8 MB holds about 57 levels, not 64. -O2, at about 69 KB a level, holds about 118.
  - After sharing, `resolve`'s IR keeps only about 35 KB of slots: eleven `View`s and two `Option<View>`s.
  - The rest of the 66.5 KB is LLVM's -O0 spill area for whole 2.6 KB struct values left in the IR (a `phi` or `select` of a `View`, which `split_big.rs` leaves whole). Shrinking that, or boxing large payloads (PRP-168), is the next step, and needs its own PRP.
- **Benchmark** (old, m135 and the fix alternating; load average 6–10, so absolute times are higher than in the tables above):

| | before m135 | m135 | fix |
|---|---|---|---|
| build `match_heavy` -O2 | 0.273 s | 1.535 s | **0.269 s** |
| build `match_heavy` -O0 | 0.157 s | 0.151 s | 0.146 s |
| build `toml_big` -O2 / -O0 | 2.94 / 0.27 s | 2.82 / 0.26 s | 2.83 / 0.28 s |
| build kivuli -O0 | 7.04 s | 5.96 s | 6.52 s |
| build kivuli -O2 | 58.0 s | 86.1 s | **56.6 s** |
| run `match_heavy` -O0 / -O2 | 0.220 / 0.095 s | 0.159 / 0.088 s | 0.156 / **0.076** s |
| run `structs` -O0 / -O2 | 0.154 / 0.083 s | 0.150 / 0.083 s | 0.149 / 0.084 s |
| run `toml_big` -O0 / -O2 | 1.87 / 1.53 s | 1.81 / 1.74 s | 1.85 / **1.16** s |

  -O2 builds are back to the old speed; runtime is the same or faster. `tests/toml.rs`'s 20-second linear-time test once took 23.8 s in a full parallel run at load 6.5. Run alone it takes 8.8 s, as before.
