## FEATURE: Copy-on-write arrays — copies share arrays until one side writes, with no change to any program's behaviour; milestone 55.

**Status:** approved 2026-10-15 — session 117, with the milestone 55 additions and DECISION-012, "in one go".
**Milestone:** 55 (one PRP; tag `m55` at merge)
**Spec:** `Language v15 Draft.md` (decisions 22–24, the milestone 55 choices); spec §4 (Array copies), §5 (assignment order, `for … of mut`), §7 (heap layout), §9 (lowering), §12 ("Cost of array copies", reversed).

**Decisions:**
- this reverses **DECISION-010** / MEMORY 20 (eager copies, copy-on-write rejected), through **DECISION-012** (resolved in session 117);
- keeps MEMORY 22 (move on last use, PRP-018);
- extends MEMORY 16 (array value semantics, PRP-014).

## OBJECTIVE
Copying an array costs O(1) until someone writes to a copy. firefly's `let g = self.grads[i];` (read-only) never copies, and every program's output is unchanged.

## CONTEXT

- **Starting state:**
  - each copy site deep-copies (`codegen/copy.rs`: `value_for_store`, `value_for_return`, `deep_copy`, `lugha_copy_<type>`);
  - writes compute element addresses from the array's pointer (`structs.rs::place_address`, `heap.rs::element_place`);
  - `for … of mut` aliases elements by address;
  - the header is `{ len, cap }`.
- **Ending state:** decisions 22–24, implemented.
- **Related existing code** (`src/codegen/` unless noted):
  - **Copies:** `copy.rs`.
  - **Places and arrays:** `heap.rs`, `structs.rs`, `array.rs`, `growth.rs`, `swap.rs`.
  - **Loops and receivers:** `iterators.rs`, the `for … of mut` lowering, `methods.rs` (receivers).
  - **Calls and other copies:** `declare.rs`/`function.rs` (extern calls), `interfaces.rs` (box copies), `closures.rs` (captures).
  - **Statements:** `stmt.rs` (assignment), `conflicts`/E0510 interplay in the checker (unchanged).
  - **Runtime:** `runtime/lugha_rt.c` (`lugha_rt_array_grow`).
- **Open decisions that must be resolved first:** none.

### Design
1. **The flag:** the top bit of `cap` (`SHARED = 1 << 63`). A helper `capacity()` masks it. `lugha_rt_array_grow` masks it, and the new array is unshared.
2. **Sharing at copy sites:**
   - `deep_copy` becomes `share`. For `T[]`, it sets the flag on the object and returns the same pointer.
   - For a struct holding arrays, `lugha_share_<type>` walks the fields and marks each array reachable without passing through another array.
   - Interface box copies and closure captures share the same way.
   - Move on last use is unchanged: no flag is needed when the source dies.
3. **Cloning:**
   - `lugha_own_<type>(ptr) -> ptr` returns `ptr` if unshared. Otherwise it returns a shallow clone (memcpy for plain elements), marks every array reachable from the elements, and the clone has `cap == len` and no flag.
   - Generated once per array type, beside the copy helpers.
4. **Writes through slots:**
   - A new two-phase place lowering:
     - `place_plan(place)` evaluates index operands (in source order) and bounds-checks them against the current lengths;
     - `place_commit(plan)` walks from the root slot, for each array on the path loads it, calls `own` and stores the result back, then returns the element address.
   - Assignment and compound assignment: plan, then the value, then commit, then store.
   - `push`/`pop`/`clear`/`swap`/`mut self` receivers commit right away, since nothing runs between them and the write.
5. **`for x of mut xs`:** the loop keeps `xs`'s slot and the index `k`. Reads of `x` load `xs[k]`, and writes commit `xs[k]` (making `xs` unique once per loop in practice, since the flag is clear after the first clone).
6. **Extern calls:** each array argument passes through `own` (it may be stored back to its slot when it's a place).
7. **Tests first:** the sharing tests below and the full existing suite must pass. Codegen unit tests check that a copy site emits no `lugha_copy_` call for arrays, and that a write emits an `own` check.

### Risk
This touches every array write in codegen. A missed write path would silently break value semantics: two variables would change together. Mitigations:
- the write-path tests below, one per path;
- the whole existing program suite, which already exercises value semantics heavily (`m1`…`m54`, several hundred programs);
- an audit list of every `place_address`/`element_place`/`element_address` use, recorded in the PRP amendments.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** everything above.
- **Spec:**
  - §4: the Array copies table now says "shared until written", with the same behaviour;
  - §5: assignment order spelled out (indices, then the value, then the write); `for … of mut` wording;
  - §7: the header's flag, copy-on-write, and the ownership sentence;
  - §8: arrays to C unshared;
  - §9: share/own helpers and the two-phase places;
  - §10 program;
  - §11 row 55;
  - §12: "Cost of array copies" reversed with the reason;
  - changes summary.
- **Docs:** CLAUDE.md (tree, STACK), README, TODO (mark the "read without copying" item), CHANGELOG, MEMORY decisions 106 (plan) and 107 (built), CONTEXT. The tag `m55` is added at the merge.
- **DECISION-012** resolved and moved to RESOLVED; DECISION-010 marked "superseded by DECISION-012".
- **Timings:** DECISION-010's benchmark programs (`docs/decisions/decision-010.md`), firefly `./gpt data/names-32k.txt 6000 32 4 0.1` (1 m 43 s before), `./mlp data/names-32k.txt`, and glowworm's examples, before and after, recorded in the PRP and the CHANGELOG.

### Must NOT Do
- **No reference counting,** no new syntax, no references.
- **No change to any program's output or exit code.**
- **No change to strings or enum payloads.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **No new diagnostics:** E0507/E0508/E0510 stay as they are.
- **Runtime:** a clone allocates through `lugha_rt_alloc`, as copies do today; out of memory behaves as before.

## SECURITY CONSIDERATIONS
- **No `unsafe`, no new dependencies.**
- **Memory safety:** every write path must make its array unique, or value semantics silently break. That makes it a correctness risk, not a security boundary (no untrusted input), and it is covered by the risk mitigations above.
- **C interop:** arrays are unshared before extern calls, so C can never write a shared array.

## TESTS TO WRITE

Unit (codegen):
- [ ] `let ys = xs;` emits no deep copy, just the flag store.
- [ ] `ys[0] = 1` emits the `own` check before the store.
- [ ] Extern array arguments go through `own`.

Acceptance — `tests/programs/m55/` (each prints both sides after the write, with the v14 output):
- [ ] **`writes.la`:**
  - write after `let`, after passing, and after returning a parameter;
  - `push`/`pop`/`clear` after a copy;
  - `swap` of shared arrays;
  - compound assignment;
  - a nested `xs[i][j] =` with shared outer and inner;
  - struct fields holding arrays;
  - a `mut self` method writing a shared field.
- [ ] **`hazards.la`:**
  - `xs[0] += { let c = xs; 1 }`;
  - `for x of mut xs` whose body copies `xs` (directly and through a function returning its parameter) and then writes `x`;
  - closures and interface values holding shared arrays.
- [ ] **`extern_write.la`:** a C function from libc that writes into a byte array (e.g. `memset` via `extern fun`) on a shared array, leaving the other copy intact.
- [ ] The §10 program from the spec.

Regression:
- [ ] Every earlier program passes with unchanged output.

## ROLLBACK PLAN

- Branch `prp-084-copy_on_write`, merged into `main` after CI passes; tag `m55`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] All tests pass at `-O0`, and the `m55/` programs at `-O2` too.
- [ ] Timings recorded.
- [ ] The spec is at milestone 55; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m55/hazards.la
```
