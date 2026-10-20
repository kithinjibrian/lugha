## FEATURE: Struct values inside Lugha functions are split into their scalar fields, and struct parameters into one parameter per field — step 2 of "aggregates through memory"; milestone 126 (v17).

**Status:** proposed 2026-10-20 (session 299), following PRP-157 (milestone 125). After step 1, kivuli's `examples/backgrounds.la` still spends ~86% of `llc -O0` in the DAG instruction selector: its IR holds 3,603 struct `load`s, 2,855 `store`s, 860 `insertvalue`, 1,590 `extractvalue`, 62 `phi`, 13 `select`, 136 calls passing a struct and 66 functions taking one. The fast selector gives up on a block at its first such instruction. Discovery: (1) split struct values into fields, as a pass after lowering; (2) struct parameters split too; (3) always, at `-O0` and `-O2`; (4) done = no fast-selector miss on a struct-typed instruction in kivuli's example, full suite unchanged, timings recorded; (5) one approved `unsafe` delete of rewritten functions' leftover declarations (step 1's too).

## OBJECTIVE
No change a program can see. In the IR, `%v = load %P, ptr %p` becomes one `load` per field through `getelementptr`, `insertvalue`/`extractvalue` disappear (fields are picked directly), and `define i1 @lugha_eq_P(%P %a, %P %b)` becomes `define i1 @lugha_eq_P(i64 %a.0, i64 %a.1, i64 %b.0, i64 %b.1)`. On kivuli's `examples/backgrounds.la`, `llc -O0 -pass-remarks-missed=sdagisel` reports no miss on a struct-typed `load`, `store`, `insertvalue`, `extractvalue`, `phi`, `select`, `ret` or call argument.

## CONTEXT
- `src/codegen/sret.rs` (milestone 125): the pattern — one pass over the lowered module, a function rewritten by moving its blocks into a new one; C-facing functions (`main`, `lugha_cb_*`, `lugha_cfn_*`) and calls to declarations untouched.
- Layouts: structs are LLVM structs of their fields; enums `{ i32, [N x i64] }` (tag, payload words); `Option`, tuples (`$tuple<n>`), function values `{ ptr, ptr }`, interface values: all LLVM structs. Arrays and strings are pointers.
- `codegen/lower.rs` runs `sret::results_through_pointers` before `module.verify()`; the new pass runs after it.
- Spec v17 §9 (lowering; the "Stack" paragraph and "Calling a function value" bullet, both edited in milestone 125). Open decisions: none.

## DECISIONS (draft decisions 293–303)
293. **Leaves**: a struct type's leaves are its fields in order, recursively through nested structs and LLVM arrays (`[10 x i64]` is ten leaves), down to integers, floats and pointers. A type with more than 64 leaves is left whole (its instructions keep the slow path); kivuli's largest is 11.
294. **Values**: every struct-typed SSA value in a rewritten function becomes its list of leaf values; the struct instruction is removed once nothing uses it.
295. **Instructions**: `load` → one `getelementptr` + `load` per leaf; `store` → one per leaf; `insertvalue` → the base's leaves with the inserted range replaced (no instruction); `extractvalue` → the leaf, or the sub-range (no instruction); `phi` → one `phi` per leaf; `select` → one `select` per leaf; constants (`undef`, `poison`, `zeroinitializer`, constant structs) → their leaves as constants.
296. **Order**: blocks are processed in reverse post-order so definitions come before uses; `phi`s are created first and given their incoming leaves at the end.
297. **Parameters** (Q2): a Lugha function taking a struct by value takes one parameter per leaf instead (names `a.0`, `a.1`, …), at its definition and at every call, direct or indirect (indirect calls are always Lugha's); the function is rebuilt as in step 1. The `sret` result slot from step 1 stays first.
298. **Edges with C**: where a struct value must stay whole — passed to or returned from a declaration (C), returned by a C-facing function, or of a type over 64 leaves — it is rebuilt from its leaves with `insertvalue` just before that use, and a whole struct coming back from C is split with `extractvalue` right after the call. These are the only struct instructions left.
299. **Always** (Q3): the pass runs for `-O0` and `-O2` builds alike; LLVM's SROA does the same splitting at `-O2`, so optimised code should not change. Spec programs' `-O2` run times are recorded before and after.
300. **Leftovers** (Q5, approved): a rewritten function's old version is deleted with one `unsafe { old.delete() }`, after its blocks moved and `replace_all_uses_with` redirected every use, with a `// SAFETY:` comment stating that and a `debug_assert!` that it has no uses. Step 1's `lugha_old_` declarations go the same way; the rename to `lugha_old_` stays only until the delete.
301. **Not**: memory-slot (`memcpy`) lowering, changes inside `codegen/` lowering code beyond calling the pass, the C ABI, a new calling convention for C, `byval`.
302. **Measurement** (Q4): on kivuli `examples/backgrounds.la` and `test/diff.la`, before and after: `lughac build`/`test` wall time, `llc -O0 -time-passes` (total and DAG selection), fast-selector misses by kind; recorded in the PRP and CONTEXT.md.
303. **Docs**: spec §9's lowering notes; Draft; CHANGELOG (Changed: faster builds); CLAUDE.md tree. **Success**: milestone 126, tag `m126`.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- `src/codegen/split.rs`: leaves of a type, the per-function rewrite (RPO, `phi` placeholders), parameter splitting with call rewriting, rebuilding at C edges.
- `sret.rs` and `split.rs` share the "rebuild a function with a new type" helper and the `unsafe` delete (decision 300), in one place.
- Every test program's IR passes `module.verify()`.

### Must NOT Do
- No behaviour, output, symbol or C-ABI change.
- No `unsafe` other than the approved delete; it gets the user's review in the PR.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- None new; a violated invariant (a leaf missing, a struct use not covered) is a compiler bug: `.expect` naming the invariant.

## SECURITY CONSIDERATIONS
- The one `unsafe` block (decision 300): only after every use is gone; reviewed by the user.

## TESTS TO WRITE
- [ ] `codegen::split` unit tests (IR text): a struct `load`/`store` pair becomes per-field `load`s/`store`s; field reads and literal building emit no `extractvalue`/`insertvalue`; an `if` producing a struct gives one `phi` per field; an enum's payload words are leaves; a function taking a struct takes its fields; an indirect call (closure) passes fields; an extern taking/returning a C struct is untouched (`extern struct`); a struct over 64 leaves stays whole; no `lugha_old_` declaration remains.
- [ ] `tests/codegen.rs`: programs passing and returning structs, enums, `Option`s, tuples and function values through every call kind, `phi`s from `if`/`match`, loops carrying a struct, nested structs and enum payloads — same results at `-O0` and `-O2`.
- [ ] Full `cargo test` unchanged (`programs`, `spec`, `extern_structs`, `callbacks`, `c_arrays`, `c_memory`, `out_params`, `tasks_runtime`, …).
- [ ] Measurement (decision 302) and spec programs' `-O2` run times recorded.

## ROLLBACK PLAN
- Branch to return to: `main`; the work lives on `prp-158-split_structs` until merged.

## ACCEPTANCE CRITERIA
- [ ] No fast-selector miss on a struct-typed instruction in kivuli's `examples/backgrounds.la` (except decision 298's C edges, counted and listed)
- [ ] Full `cargo test` passes; clippy and fmt pass; no file over 500 lines
- [ ] Timings recorded; spec §9, Draft, CHANGELOG, CLAUDE.md updated

## VALIDATION
- `cargo fmt --check`; `cargo clippy --all-targets -- -D warnings`; `cargo test`
- In kivuli (read only): `lughac build --emit=ir examples/backgrounds.la`, then `llc -O0 -pass-remarks-missed=sdagisel` and `-time-passes`
