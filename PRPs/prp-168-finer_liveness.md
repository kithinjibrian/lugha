## FEATURE: Stack slots used one after another inside the same block share one slot, so kivuli's `resolve` fits a 64-deep Rapidkoi screen at -O0; milestone 136 (v17).

**Status:** proposed 2026-10-30 (session 325). It follows PRP-167, whose corrected measurements (session 324) left `resolve` at 66.5 KB at -O0, about 57 levels in 8 MB rather than Rapidkoi's 64. Discovery: (1) the approach is finer liveness in `frames.rs`, tracking positions inside a block. Not chosen: copy reduction in lowering, and boxing large payloads.

## OBJECTIVE
A function's frame keeps a big slot only while some value in it may still be read, down to the instruction, not the whole block. kivuli's `resolve<()>` falls below 53 KB at -O0, so a 64-deep screen (two `resolve` frames and one `resolve_children` per level) fits the main thread's 8 MB with room to spare. Nothing visible changes: same output, same build times (±5%), -O2 frames no larger than today.

## CONTEXT
- **Measured (session 325)**, kivuli `examples/backgrounds.la`, IR before sharing:
  - `resolve<()>` has 204 `View`/`Option<View>` slots (2.6 KB each). After today's sharing, 21 are left, about 55 KB of the 66.5 KB frame; the rest is LLVM's -O0 spill space for whole values (one `phi`, two loads).
  - The floor is set by the `Popover` arm: 18 slots live in one block, `variant.ok538`. They are the bindings `trigger`, `content`, `t` and `c` (copied out of the scrutinee), `arg` copies made to pass pointers, `result`s, `enum` temporaries and `copy` slots. Many are used one after another, but per-block liveness counts them all as live through the whole block.
  - Merging in a different order changed nothing (66.5 KB either way).
- **Starting state:**
  - `src/codegen/frames.rs` (245 lines): `Graph`, block-set liveness (`Blocks`, `live`), `share_slots`, `use_blocks`.
  - `frames_phi.rs` (178): slots joined by a `phi`.
  - `frames_tests.rs` (300).
- **Ending state:**
  - Liveness keeps, for each slot and each block it touches, the range of instruction positions where it may hold a value someone will read:
    - the whole block when the block is reached from a use and reaches a use (as today), and the slot isn't used in it;
    - in a block using it: from the block's start if the block is reached from a use (otherwise from the first use), to the block's end if it reaches a use (otherwise to the last use).
  - A use is any instruction touching the slot's address, through element addresses (GEPs) included.
  - Two slots of one type share when no block has overlapping ranges. Ranges are closed: one instruction using both, such as a `memcpy` between them, keeps them apart.
  - The range code goes in a new `src/codegen/frames_live.rs` if `frames.rs` would pass 500 lines.
  - `frames_phi.rs` keeps per-block liveness, which is coarser but sound.
- **Related code:** `src/codegen/frames.rs`, `frames_phi.rs`, `frames_tests.rs`, `src/codegen/mod.rs` (where sharing runs), `tests/codegen.rs`, `tests/programs/m135/`.
- **Spec:** none needed. §7 already says that slots of regions never live at once are shared and a frame is as large as its largest region; sharing inside a region keeps that true.
- **Open decisions:** none.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Positions:** number every instruction within its block once per function. A slot's uses are its users and its GEPs' users, transitively, each with its block and position.
- **Ranges:** per slot and block, the rules in Ending state. Blocks fully live use `(0, len)`.
- **Merging:** a merged slot keeps the union of its members' ranges, per block. A candidate joins the first same-type slot it overlaps nowhere, keeping the larger alignment, as today.
- **Eligibility is unchanged:** entry-block allocas of struct or array type, with the address used only by loads, stores *to* it, GEPs and calls other than lifetime markers. `phi` groups merge first, as today.
- **Both levels:** -O0 (before the passes) and -O2 (after them), as today.
- **Measure and record** in RESULT, with the corrected frame script (probe loops counted):
  - `resolve<()>` and `resolve_children<()>` at -O0 and -O2;
  - `match_heavy` and the toy's frames;
  - build times for kivuli -O0/-O2 and `match_heavy` -O2, alternating with the m135-fix compiler.
- CHANGELOG, CLAUDE.md (the frames line), TODO (tick the item added in session 324), §11 row 136.

### Must NOT Do
- Don't change lowering, copies, layouts or the spec. *Why:* the user chose the pass-only fix; copy reduction and boxing are separate PRPs if ever needed.
- Don't treat an instruction using two slots as freeing one before the other. *Why:* a `memcpy` from one into the other needs both at once.
- Don't share slots of different types. *Why:* PRP-167's rule.
- Don't let a probe or a lifetime marker count as freeing a slot. *Why:* markers stay excluded as in session 323.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- No new error paths. A position missing for an instruction of the function is a compiler bug: `.expect("every instruction is numbered")`.
- `module.verify()` must still pass for every test program at -O0 and -O2.

## SECURITY CONSIDERATIONS
- No external input, `unsafe`, crypto or credentials.
- The risk is a slot reused while its value is still needed, which is silent memory corruption. Hence the conservative edges (a block reached from a use is live from its start; one reaching a use is live to its end), the hand-written unsafe shapes in the unit tests, and every program run at both levels.

## TESTS TO WRITE
- [ ] Unit (`frames_tests.rs`, hand-written IR):
  - two slots used one after another in one block are shared (the existing `slots_used_in_one_block_stay_apart` becomes this test, since that sharing is now intended);
  - a `memcpy` from one slot into another keeps them apart;
  - interleaved uses (a, b, a) keep them apart;
  - a slot live into a block from a predecessor conflicts with one used early in that block;
  - a slot whose last use comes before another's first use in the block shares with it;
  - a load through a GEP late in the block extends the slot's range;
  - all of today's cross-block cases still hold.
- [ ] `tests/programs/m136/sequential_values.la`: big values made one after another in one block, some read at the end and some not, printed. Same output at both levels.
- [ ] `tests/codegen.rs`: an arm with two recursive children (the `Popover` shape) recursing deep enough at -O0 to overflow with per-block liveness and fit with ranges. The depth is set during the build by measuring both, and recorded in RESULT.
- [ ] Existing: `cargo test --lib`, `--test programs`, `--test codegen`, `--test arm`, and the full `cargo test` (every function's frame changes).

## ROLLBACK PLAN
- Branch to return to: `main` at `5d2f400`.
- State: PRP-167's per-block sharing; the work lives only on `prp-168-big_values` until merged.

## ACCEPTANCE CRITERIA
- [ ] kivuli `resolve<()>` under 53 KB at -O0 (from 66.5 KB), measured with probe loops counted. If finer liveness alone falls short, stop and report the measurement before going further (scope rule).
- [ ] -O2 `resolve<()>` no larger than 30.5 KB; build times within ±5% of the m135-fix compiler.
- [ ] Every new test passes; all existing tests pass (full `cargo test`).
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` pass; every file ≤ 500 lines.
- [ ] CHANGELOG, CLAUDE.md, TODO, spec §11 row 136 updated.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- The frame script on kivuli's example at -O0 and -O2.
- Rapidkoi: `./main rapidkoi://notes.dafifi.net` and a 64-deep screen, by hand (needs a display).
