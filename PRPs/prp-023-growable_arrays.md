## FEATURE: Growable arrays — `xs.push(v)`, `xs.pop()` and `xs.clear()` on `T[]`, a capacity header, and the complete v1 specification. Completes milestone 8 and v1.

**Status:** approved 2026-10-12 — session 29
**Milestone:** 8 (one PRP), the last of v1. Done-when: the stack program prints `3`, `2`, `empty`; then tag `m8`.
**Spec:** `docs/specs/Language v1 Draft.md` §4 "Growable arrays", §5, §7 (array layout), §9 (`lugha_rt_array_grow`), §10 (stack program), folded into `Language v1 Specification.md`
**Decisions:**
- MEMORY 23 (one sequence type: `T[]` grows);
- MEMORY 27 (methods: `mut self` place rules, E0508);
- MEMORY 16/22 (copies, move on last use);
- DECISION-009 (the runtime is embedded C).

## OBJECTIVE
Arrays can grow and shrink:
- `xs.push(v)` appends;
- `xs.pop()` returns `Some(last)` or `None`;
- `xs.clear()` empties the array.

Each needs a `let mut` array, and none may run on an array being walked by `for … of`. Everything else about arrays is unchanged, and every existing program prints what it did. `lughac spec` prints the complete v1 specification, the draft is retired, and the stack program prints `3`, `2`, `empty`.

## CONTEXT

- **Starting state:**
  - `T[]` is a pointer to `{ i64 len; T elems[len] }`, and its length is fixed.
  - `xs.push(1)` is E0415.
  - The v1 spec is "in progress", with growable arrays only in the draft.
- **Ending state:**
  - The array header becomes `{ i64 len; i64 cap }`, and elements start at byte 16.
  - The new built-ins and a `lugha_rt_array_grow` runtime function.
  - A new `codegen/growth.rs`.
  - The complete v1 spec; `Language v1 Draft.md` deleted.
- **Related existing code:**
  - `src/check/{method_calls,liveness}.rs`;
  - `src/codegen/{heap,array,copy,methods,construct}.rs`;
  - `runtime/lugha_rt.c`, `src/codegen/runtime.rs` (`RUNTIME_SYMBOLS`);
  - `tests/common/v1_programs.rs`, `tests/parser.rs`.
- **Open decisions that must be resolved first:** none.

### Discovery answers (session 29)
1. **`xs.push(xs[0])` is allowed.** The place `xs` is evaluated first, then the value, which is copied from a place like an assignment's. Only then does the array grow, so no half-done edit can be observed and E0508 doesn't apply to the built-ins.
2. **The draft is retired.**
   - The v1 specification drops "(in progress)" and its status note, and becomes the complete v1 definition.
   - `Language v1 Draft.md` is deleted; git history keeps it.
   - The v0 file stays frozen.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Layout** (v1 §7)
- An array is a pointer to `{ i64 len; i64 cap; T elems[cap] }`.
  - Literals, repeat literals and copies have `cap == len`.
  - Elements start at byte 16, which keeps 8-byte alignment.
  - Strings keep their v0 layout (`{ i64 len; bytes; NUL }`, data at byte 8).
- `heap::element_address` takes the header size: 16 for arrays, 8 for strings. Every array access uses 16:
  - indexing, element places and bounds checks;
  - literals, repeat literals, `for … of`;
  - flat copies and copy helpers.
- `new_array` stores both `len` and `cap`.
- `copy_flat` allocates a fresh array (`cap == len`) and copies only the `len` elements (`llvm.memcpy` from byte 16), never the spare capacity.

**Runtime** — `runtime/lugha_rt.c`
- `void *lugha_rt_array_grow(void *array, int64_t elem_size)`:
  - allocates a new array with `cap = max(4, 2 * cap)` through `lugha_rt_alloc`;
  - copies the header's `len` and the `len` elements;
  - sets `cap`;
  - returns the new pointer.
- The old object is left for the GC: the array's single owner stops pointing at it (spec §7: one owner per array).
- Added to `RUNTIME_SYMBOLS`. Allocation failure keeps panicking with "out of memory".

**Checker** — `check/method_calls.rs`
- A method call on a value of type `T[]` resolves the built-ins:
  - `push(v: T): void`, where `v` expects `T`;
  - `pop(): Option<T>`;
  - `clear(): void`.

  Any other name is E0415 "no method `m` on `T[]`".
- **Receiver rules:** all three need a place receiver, with the `mut self` rules: E0501 (the receiver wording names the method), E0502, E0506 and E0507 (during `for … of`). E0508 doesn't apply (discovery answer 1).
- **Argument count:** wrong counts are E0405.
- **Strings and other types:** they still have no methods.
- **Liveness:** `push`'s argument becomes a store site, so a dead local moves into the array instead of being copied (PRP-018).

**Codegen** — new `codegen/growth.rs`, called from `codegen/methods.rs`
- **`push`:**
  1. Evaluate the receiver's place address once, then the value (`value_for_store`: copied from a place, moved on last use).
  2. Load the array. If `len == cap`, call `lugha_rt_array_grow` and store the new pointer into the place.
  3. Store the element at index `len` (`store_element`, so `bool` is an `i8`), then set `len + 1`.
- **`pop`:**
  - Load the array. If `len == 0`, the result is `None`.
  - Otherwise decrement `len`, load the element at the new `len`, and build `Some(element)`. The element moves out, with no copy: it is no longer in the array.
  - Building the `Option` uses a value-based constructor shared with `construct.rs`.
- **`clear`:** sets `len` to 0; `cap` is kept.
- **Capacity:** it is an implementation detail. No program can read it, so there is no `.cap`.

**Spec** — fold into `Language v1 Specification.md`, then retire the draft
- **§1:** the title becomes "Lugha v1 Specification". The v1-status note is replaced by a one-line "v1 = v0 plus enums, `match`, `Option`, recursive data, methods and growable arrays". The overview's non-goals are updated.
- **§4 Arrays:** "a length fixed at creation" becomes a length that changes through `push`, `pop` and `clear` (the draft's table and rules). The Methods paragraph's "Arrays … have no methods" names the three built-ins.
- **§5:** `push` evaluation order.
- **§7:** the heap-object table row for `T[]` (`{ i64 len; i64 cap; T elems[cap] }`), and growth by doubling with a minimum of 4.
- **§9:** the runtime table gains `lugha_rt_array_grow`; the lowering notes add `push`/`pop`.
- **§10:** the stack program.
- **§11:** the milestone 8 row is done; the table caption says v1 is complete.
- **§12:** the open-question "Array growth factor" is decided: doubling, minimum 4, measured as adequate on the stack and DECISION-010 programs.
- **Delete** `docs/specs/Language v1 Draft.md`.
- **Test fixtures:** delete `tests/common/v1_programs.rs`, whose last program, `STACK`, moves into the spec, and its use in `tests/parser.rs`.

**Docs**
- CLAUDE.md:
  - the tree loses the draft and `v1_programs.rs`, and gains `codegen/growth.rs`;
  - architecture rule 1 points at the v1 spec only;
  - anti-pattern 5's v1 note becomes "v1 is complete; anything else is v2 and needs a spec change first".
- README: the v1 feature list.
- MEMORY, CHANGELOG, TODO, CONTEXT. Tag `m8` after merging.

### Must NOT Do
- No other array methods (`insert`, `remove`, slicing, `.cap`) and no methods on strings: v2.
- No change to string layout, to value semantics, or to copy rules beyond `push` as a store site.
- No `unsafe`, no new dependencies.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- The new rules report codes and keep going, as for other calls.
- `pop` on an empty array is `None`, not a panic. Indexing past `len` still panics with the PRP-013 message.

## SECURITY CONSIDERATIONS

- **Growth:** it copies exactly `len` elements into a fresh allocation of `16 + cap × size` bytes. The capacity doubles from `cap`, an `i64`. Overflow is impossible in practice (allocation fails first, with "out of memory"). The runtime checks the multiplication anyway, and panics with "out of memory" if it overflows.
- **Bounds checks:** they compare against `len`, never `cap`, so spare capacity is never readable.
- **No `unsafe`.**

## TESTS TO WRITE

Unit tests:
- [ ] **Checker:**
  - `push` expects the element type (E0401/E0403);
  - `pop` is `Option<T>`;
  - `clear`;
  - E0415 for another name and for `s.push` on a string;
  - E0405 for counts;
  - E0501 on an immutable root, with the method in the message;
  - E0507 during `for … of`;
  - `xs.push(xs[0])` is accepted;
  - `push`'s argument moves on last use.
- [ ] **Codegen IR:**
  - literals store `cap`;
  - element addresses add 16;
  - `push` calls `lugha_rt_array_grow` only on the full path;
  - `pop` builds `Option` with tags 0/1.
- [ ] **Runtime:** `lugha_rt_array_grow` is in `RUNTIME_SYMBOLS`; `lugha_rt.c` still compiles with `-Werror`.

Acceptance:
- [ ] **Spec §10:** the stack program prints `3`, `2`, `empty` (from the spec, at `-O0`/`-O2`).
- [ ] **`m8/growth.la`:**
  - 100 000 pushes, then summed;
  - `pop` until `None`;
  - `clear`, then reuse;
  - `push` of `bool`, `string`, struct and nested-array values;
  - `xs.push(xs[0])`;
  - a copy, made before pushes, keeps its own length;
  - an array grown inside a struct field (`s.items.push(…)`).

  Expected stdout written by hand.
- [ ] **Reject programs:** `e0501_push.la`, `e0507_push.la`, `e0415_string_push.la`. Goldens captured and reviewed.
- [ ] **Regression:** every earlier test still passes after the layout change, in particular every `m5`–`m7` program and DECISION-010's benchmark programs, which are built and run.

## ROLLBACK PLAN

- Branch `prp-023-growable_arrays`, merged into `main` after green CI; then tag `m8` (v1 complete).
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The stack program prints `3`, `2`, `empty`; every test above passes.
- [ ] The v1 spec is complete and the draft deleted; CLAUDE.md, README, MEMORY.md, TODO.md, CHANGELOG.md updated.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `lughac run` on the stack program prints `3`, `2`, `empty`; `lughac spec | head -1` → `# Lugha v1 Specification`
