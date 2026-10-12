## FEATURE: Nested patterns and `if` guards in `match`, with a usefulness-based exhaustiveness check and W0103 — completes milestone 9.

**Status:** approved 2026-10-13 — session 39
**Milestone:** 9 (third of three PRPs: 030 syntax, 031 equality and string order, **032 nested patterns and guards**). Milestone 9 is done when the v2 draft's patterns program passes.
**Spec:** `Language v2 Draft.md` §3 (pattern grammar, bare names in slots), §5 (`match` with nested patterns and guards), §9 (lowering notes, codes), §10 (patterns program), §12 (unused-binding question). These fold into `Language v2 Specification.md`.
**Decisions:**
- MEMORY 25 (enums in the compiler), MEMORY 26 (indirect slots);
- MEMORY 33 (v2 direction), MEMORY 34 (gate and pattern AST), MEMORY 35 (equality, the v2 spec file).

## OBJECTIVE
`match` arms can nest patterns at any depth (`Some(Circle(r))`, `Pair(1, Some(_))`) and carry a guard (`Circle(r) if r > 1.0 =>`).
- **Checking:** exhaustiveness and unreachable arms are computed by a real usefulness check over nested patterns. E0417 lists concrete missing cases.
- **W0103:** a new warning for a pattern binding that is never used. It catches a misspelled variant that silently became a binding.
- **Milestone 9:** the draft's patterns program prints its six lines, and the M9 gate entries are gone.

## CONTEXT

- **Starting state:**
  - Patterns are flat in the checker (`Covers` sets) and in codegen (one `switch`).
  - PRP-030 parses nested patterns and guards, and the gate stops them as milestone 9.
  - `Pattern::slot_binding` treats every bare slot name as a binding.
  - `check/matching.rs` is 292 lines, `codegen/matching.rs` 248.
- **Ending state:**
  - **Checker:** pattern typing is recursive, and a usefulness check gives E0417 and W0102. Guards are checked, and W0103 is added.
  - **Codegen:** flat unguarded matches keep the `switch`; others are lowered arm by arm.
  - **Gate:** the M9 entries are deleted.
  - **Spec:** the pattern sections are folded in, and milestone 9 is marked done.
- **Related existing code:**
  - `src/check/{matching,enums,types,liveness}.rs`, `src/check/errors/{types,places}.rs`;
  - `src/codegen/{matching,enums,heap}.rs`;
  - `src/ast/pattern.rs`, `src/driver/gate/`;
  - `tests/programs/m6/` (W0102, E0417 goldens).
- **Open decisions that must be resolved first:** none.

### Discovery answers (session 39)
1. **W0103, unused binding:**
   - a `match` binding never used in its arm (guard or body) is a warning, with help: "if you meant a variant, check its spelling; otherwise write `_`";
   - it applies to all pattern bindings, nested or not; `let` and parameters are out of scope.
2. **Lowering:**
   - flat, unguarded matches keep today's `switch` (v1's IR is unchanged);
   - a match with any nested pattern or guard is lowered arm by arm: test the whole pattern outside-in, bind, run the guard, and on any failure jump to the next arm;
   - the draft's "decision tree" lowering note is updated to say this.
3. **E0417:** list up to three missing patterns, generalized with `_` as far as possible, then "and N more". The existing message format is kept, so v1 goldens are unchanged when three or fewer are missing.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Pattern typing** — new `check/patterns.rs` (moved out of `matching.rs` and made recursive)
- **Result:** typing a pattern against a type gives its bindings and a simplified `Pat` for the usefulness check:
  - `Wild` (for `_` and bindings);
  - `Ctor(Ctor, Vec<Pat>)`, where `Ctor` is `Variant(index)`, `Bool(b)`, `Int(value)` or `Str(text)`.
- **Bare names in a slot:**
  - a variant if the slot's type is an enum or `Option` with a variant of that name (payload-less: `Some(Empty)`);
  - otherwise a binding.
  - At the top level, a bare name must still be a variant (E0415, as today).
- **Shared rule:** the variant-or-binding rule lives in one function, `check::slot_names_variant(ty, name, enums)`. Codegen uses it too, so the two stages can't disagree.
- **Errors:**
  - a pattern of the wrong kind for its slot is E0403 (`pattern_mismatch`), at any depth;
  - a wrong payload count is E0416;
  - an out-of-range integer literal is E0402, as today.
- **Payload types:** they come from the enum or `Option`, so bindings get the payload's type.

**Usefulness** — new `check/usefulness.rs`, the classic pattern-matrix algorithm (Maranget, "Warnings for pattern matching")
- **Complete constructor sets:** an enum's or `Option`'s variants, and `bool`'s `true` and `false`. Integers and strings never have a complete set.
- **`useful(rows, row)`:** used for W0102. Each arm is checked against the earlier *unguarded* arms. A guarded arm can itself be unreachable, but it never makes later arms unreachable.
- **`missing(rows, types)`:** missing patterns over the unguarded arms, for E0417.
  - **Witnesses** are built from the missing constructors, with `_` for every payload, and printed in source syntax without the enum name (`Some(Rect(_, _))`).
  - **Integers and strings** at the top level keep today's message ("other values of `i64` are not covered"). Nested inside, they show as `_`.
  - **Bound:** the search stops after 100 witnesses. This bounds work on hostile input, and the "and N more" count is capped to match.
- **W0102 label:** a single earlier arm (the first that covers it on its own) when one exists; otherwise "covered by earlier arms".

**`match` checking** — `check/matching.rs`
- **Per arm:**
  - type the pattern, then declare its bindings in the arm's scope;
  - check the guard as `bool` (E0403 otherwise), with the bindings visible;
  - then check the body.
  - Arm-type agreement is unchanged.
- **W0103:** after an arm, report each binding that no expression in its guard or body mentions. The checker's `mentions` table, keyed by declaration span, already records this.
  - Code `W0103`, message "unused binding `x`", with the help above.
  - Bindings named `_x` are not exempt: Lugha has no such convention.
- **Liveness:** no change is needed. Mentions inside guards are already recorded by the checker, and liveness's arm walk only finds store sites, so a store site inside a guard simply copies.

**Codegen** — `codegen/matching.rs` keeps the `switch` path; new `codegen/patterns.rs`
- **Flat test:** a match is *flat* when it has no guard and every payload slot is `_` or a binding (by `slot_names_variant`). Flat matches use today's lowering, unchanged.
- **The arm-by-arm chain**, otherwise:
  - **Scrutinee:** stored once, in storage form (`bool` as `i8`), in an entry-block slot.
  - **Testing an arm:** the pattern is tested outside-in. A variant compares the tag at its address, then tests each payload at its address (loading the pointer first for indirect slots). Literals compare the loaded value (integers and `bool` inline, strings with `lugha_rt_str_eq`). Any failure jumps to the arm's `next` block.
  - **Bindings:** they are borrowed locals loaded from their addresses, as in `bind_payloads`.
  - **Guards:** `cond_br(guard, body, next)`.
  - **Bodies:** they store the arm value and branch to the end, as today.
  - **After the last arm:** `unreachable`, because the checker guarantees exhaustiveness.
- **`Pattern::slot_binding`** (`ast/pattern.rs`) goes away. Its two users switch to `slot_names_variant`.

**Gate** — `driver/gate/`
- Delete the `nested patterns` and `match guards` entries, their walker hits, and the milestone 9 tests. Milestones 10 and 11 remain.

**Spec**
- **`Language v2 Specification.md`:**
  - the title becomes "in progress: milestone 10";
  - the "Changes since v1.2" list gains nested patterns, guards and W0103;
  - **§3:** the pattern grammar (`pattern` slots, `arm` with a guard) and the bare-name rule;
  - **§5 (`match`):** nested patterns, guards, exhaustiveness with guards, E0417 witness lists;
  - **§9:** W0103 in the codes table, plus the lowering note;
  - **§11:** milestone 9 marked done, and the draft's patterns program is added to §10 as an acceptance program, so `tests/spec.rs` runs it.
- **`Language v2 Draft.md`:**
  - §9's lowering note says "arm by arm" (approved answer 2);
  - §12's unused-binding question is marked resolved (W0103).

**Docs**
- CLAUDE.md tree (`check/patterns.rs`, `check/usefulness.rs`, `codegen/patterns.rs`).
- README.
- TODO (milestone 9 done; tag `m9` after merging).
- CHANGELOG, MEMORY (decision 36), CONTEXT.

### Must NOT Do
- No or-patterns, binding patterns at the top level, `mut` bindings, or `f64` literal patterns (all v3 or still excluded).
- No change to flat matches' IR, copy rules, or the borrowing of bindings.
- No `unsafe`, no new dependencies.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- **Diagnostics:** E0403, E0415, E0416, E0417, E0402, W0102 and W0103 are records, and checking continues after each.
- **Failed patterns:** a pattern that failed to type (`Error`) counts as `_` for usefulness, so one mistake doesn't add E0417 or W0102 noise.

## SECURITY CONSIDERATIONS

- **Recursion depth:** pattern typing, usefulness and lowering recurse over pattern depth. The parser already bounds that at 256 (E0206), so there is no stack overflow from hostile input.
- **Bounded output:** witness enumeration stops at 100.
- **No `unsafe`.**

## TESTS TO WRITE

Unit tests — `check/patterns.rs`:
- [ ] **Bindings:** `Some(Circle(r))` binds `r: f64`.
- [ ] **Bare names:** `Some(Empty)` is the variant, and `Some(x)` binds `x: Shape`.
- [ ] **Literal slots:** `Pair(1, Some("a"))` checks literal types at depth.
- [ ] **E0403:** `Some(1)` on `Option<string>`, and `Circle(Rect(_, _))` on an `f64` slot.
- [ ] **E0416:** `Rect(_)` nested.
- [ ] **E0415:** a bare top-level name that isn't a variant.

Unit tests — `check/usefulness.rs`:
- [ ] **Exhaustive matches:** nested matches over `Option<Shape>`, and a `bool` pair (`Pair(true, _)`, `Pair(false, _)`), are complete.
- [ ] **E0417 witnesses:** `Some(Rect(_, _))` and `None`; four or more missing gives "and N more".
- [ ] **Guards:** a guarded arm doesn't count, so E0417 is still reported.
- [ ] **W0102:**
  - after `Some(_)` and `None`;
  - for an arm covered only by a combination of earlier arms;
  - not for an arm after a guarded one.
- [ ] **Literals:** integer and string literals inside variants (`Some(1)`, then `Some(_)`) don't make the match complete without a catch-all slot.

Unit tests — `check/matching.rs`:
- [ ] **Guards:** a guard must be `bool` (E0403), and bindings are visible in it.
- [ ] **W0103:**
  - reported for an unused binding and for a misspelled variant `Some(Emtpy)`;
  - not reported when the binding is used only in the guard.

Codegen IR — `codegen/patterns.rs`:
- [ ] **Flat matches** still lower to one `switch` (existing tests stay green).
- [ ] **Nested matches:** a nested match tests the inner tag after the outer one, and a guard branches to the next arm.
- [ ] **Indirect payloads:** a recursive enum's indirect payload is loaded through its pointer before the inner test.

Acceptance (all cross-checked at `-O0`/`-O2`):
- [ ] **`tests/programs/m9/patterns.la`:** the draft's patterns program, printing the six lines, plus:
  - a recursive list matched two levels deep;
  - guards falling through to later arms;
  - nested `bool` and integer slots.

  The expected output is written by hand.
- [ ] **Goldens, captured and reviewed:**
  - `e0417_nested.la` (the witness list);
  - `w0103.la` (a misspelled variant: W0103 plus W0102; the program still runs);
  - `e0403_guard.la` (a non-`bool` guard).
- [ ] **Spec:** `tests/spec.rs` runs the new §10 patterns program from the v2 spec.
- [ ] **Gate:** the M9 rows are removed from the gate tests; the CLI gate test (generics) stays.
- [ ] **Regression:** every earlier test passes, including the `m6` W0102 and E0417 goldens, unchanged.

## ROLLBACK PLAN

- Branch `prp-032-nested_patterns`, merged into `main` after green CI; then tag `m9`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The draft's patterns program passes from the v2 spec's §10; milestone 9 is done.
- [ ] Nested patterns, guards, E0417 witnesses, W0102 and W0103 behave as above.
- [ ] The v2 spec and draft are updated; CLAUDE.md, README, TODO, CHANGELOG, MEMORY updated.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `lughac run tests/programs/m9/patterns.la`
