## FEATURE: The `swap(a, b)` intrinsic — exchange the contents of two places without copying — as Lugha v1.1.

**Status:** implemented 2026-10-12 — session 32
**Milestone:** none; Lugha v1.1 (a small compatible amendment to v1). Tag `v1.1` after merge.
**Spec:** `Language v1 Specification.md` §4 (copy table, places, mutability), §5 (intrinsics), §9 (codes)
**Decisions:**
- DECISION-010 / MEMORY 20 (eager copies; a swap or move operation noted for later);
- MEMORY 27 (the `mut self` place rules);
- MEMORY 22 (move on last use).

## OBJECTIVE
`swap(ys, buf)` exchanges two arrays in constant time, with no copy. The merge sort that DECISION-010 measured at 152 MB of copying stops copying.

`swap` works on any two places of the same type: variables, fields and elements, so `swap(xs[i], xs[j])` replaces the three-line temp dance. Value semantics are unchanged: afterwards each place owns its own data, exactly as if the swap had been written with a temporary.

The spec becomes "Lugha v1.1".

## CONTEXT

- **Starting state:**
  - v1 is complete (tag `m8`).
  - Intrinsics are `print`, `println`, `panic` and `to_string`.
  - The `mut self` place checks exist (`check_place_for`).
  - `decision-010/sort.la` copies `buf` into `ys` each pass.
- **Ending state:**
  - The `swap` intrinsic, with checker (new `check/swap.rs`) and codegen (new `codegen/swap.rs`).
  - E0509.
  - The spec is retitled v1.1 with a changes note.
  - A measured `sort` before/after in the DECISION-010 doc.
- **Related existing code:**
  - `src/check/{call,env,assign,array}.rs`, `src/check/errors/places.rs`;
  - `src/codegen/{runtime,structs}.rs` (`place_address`);
  - `docs/decisions/decision-010*`.
- **Open decisions that must be resolved first:** none.

### Amendments during implementation (session 32)
- **Wording:** `PlaceUse` (Assign, Receiver(m), Swap) replaced `check_place_for`'s `Option<&str>`, and `errors::for_use` replaced `for_receiver`. Swap also rewords E0502 ("cannot swap this expression") and E0506 ("cannot swap inside a string").
- **E0509 text:** the message names the containing place as written, with `[…]` for indexes (e.g. "cannot swap `n.kids` with a part of itself").
- **Spec placement:** the swap rules, E0509 included, are a paragraph under the §5 intrinsics table. §4's mutability paragraph points to it.
- **Measurement:** the copy counter is an uncommitted patch, so the copies (95 → 5) were derived from the program's structure and confirmed in the IR. Times are medians of 21 runs at `-O2`: 154 → 109 ms.
- **Found, not fixed:** an element place whose array grows while the rest of the statement runs loses the write (`xs[0] = { xs.push(2); 9 };`). This predates `swap`, which shares it. Added to TODO "Found while working"; it needs a spec ruling.

### Discovery answers (session 32)
1. **Form:** a `swap(a, b)` intrinsic. No new syntax; it works on any type.
2. **Overlap:** only "a value with a part of itself" is rejected, as **E0509** "cannot swap `n` with a part of itself": the same root, with one path a prefix of the other (any two indexes counted as possibly equal). `swap(xs[i], xs[j])`, `swap(p.a, p.b)` and `swap(a, a)` are allowed.
3. **Versioning:** Lugha **v1.1**, a compatible amendment to the v1 spec. Only a program that defines its own function named `swap` stops compiling (E0302).

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Checker** — new `check/swap.rs`, dispatched from the intrinsic path in `check/call.rs`
- `swap` joins the reserved intrinsic names (E0302 for a function, struct or enum named `swap`).
- **Arguments:**
  - exactly two (E0405);
  - each must be a place: a variable, field or element (E0502);
  - the second expects the first's type (E0403, labelled "`a` is T").
- **Place rules for both arguments,** as for assignment:
  - the root is `let mut` (E0501, worded "cannot swap `x`: it is not mutable");
  - not inside a string (E0506);
  - not overlapping an array being iterated (E0507, worded "cannot swap `xs` while iterating over it").

  `check_place_for` gains a "use" parameter (assign, call method `m`, swap) for these wordings.
- **E0509:** reported when one argument's place path is a prefix of the other's. This reuses `array::Path`, with any two index steps counting as equal.
- **Type and liveness:** the call's type is `void`. Arguments are places, not values, so they are not store sites and nothing moves.

**Codegen** — new `codegen/swap.rs`, called from `runtime.rs`'s intrinsic dispatch
- Evaluate place `a`, then place `b` (left to right, spec §5; element places are bounds-checked).
- Load both values (in element layout for fields and elements, local-slot layout for variables) and store them crossed.
- For arrays this exchanges two pointers. No allocation and no `memcpy` happen for any type.

**Spec** — `Language v1 Specification.md` (file name unchanged)
- **Title:** "Lugha v1.1 Specification", with a one-line "Changes since v1.0: the `swap(a, b)` intrinsic (§5); E0509" under it.
- **§4 copy table:** row "`swap(a, b)` — No — the two places exchange their contents".
- **§4:** E0509 in the place rules.
- **§5 intrinsics table:** `swap(a, b)` — two places of the same type — exchanges their contents.
- **§9:** an E0509 row.
- **§12:** the DECISION-010 swap/move idea is resolved.

**Measurement** — DECISION-010
- Add `docs/decisions/decision-010/sort_swap.la` (`ys = buf;` becomes `swap(ys, buf);`).
- Record its copy count, time and output beside `sort.la` in `decision-010.md`, "After PRP-026". The output must equal `sort.la`'s.

**Docs**
- README: mention v1.1 and `swap`.
- CLAUDE.md: tree (`check/swap.rs`, `codegen/swap.rs`).
- MEMORY, CHANGELOG, TODO (tick the swap idea), CONTEXT. Tag `v1.1` after merging.

### Must NOT Do
- No `take`/move-out intrinsic, no swap operator, no swapping through method receivers.
- No change to any copy rule other than adding `swap`, which copies nothing.
- No `unsafe`, no new dependencies.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- Every rule reports a code and keeps going. A `swap` with errors has type `void`, and its arguments are still checked.

## SECURITY CONSIDERATIONS

- **Bounds:** element places are bounds-checked before their addresses are formed. `swap` writes only into those two places.
- **No cycles:** E0509 prevents a value from being stored inside itself, so ownership stays a tree.
- **No `unsafe`.**

## TESTS TO WRITE

Unit tests — `check/swap.rs`:
- [x] variables, fields, elements and same-root elements (`swap(xs[i], xs[j])`) accepted;
- [x] E0405 (one or three arguments);
- [x] E0502 (`swap(a, 1)`);
- [x] E0403 (different types);
- [x] E0501 with the swap wording;
- [x] E0506 (`swap(s[0], t[0])` on strings);
- [x] E0507 during `for … of`;
- [x] E0509 for `swap(n, n.kids[0])`, and none for `swap(xs[0], xs[1])`;
- [x] E0302 for `fun swap()`.

Codegen IR — `codegen/swap.rs`:
- [x] `swap(a, b)` on arrays emits two loads and two stores of `ptr`, and no `memcpy` or `lugha_rt_alloc`;
- [x] element swaps are bounds-checked first.

Acceptance — `tests/programs/v1_1/` (also cross-checked at `-O2`):
- [x] **`swap.la`:**
  - two arrays swapped, then each mutated independently;
  - `swap(xs[i], xs[j])` in a simple sort;
  - two struct fields;
  - two `bool`s and two `u8`s (element layout);
  - enums;
  - `swap(a, a)`;
  - a merge sort using `swap(ys, buf)`.

  Expected stdout written by hand.
- [x] **Reject programs:** `e0509.la`, `e0501_swap.la`. Goldens captured and reviewed.
- [x] All earlier tests pass.

## ROLLBACK PLAN

- Branch `prp-026-swap`, merged into `main` after green CI; then tag `v1.1`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] `swap` behaves as above; `sort_swap.la` matches `sort.la`'s output with no buffer copies (measured and recorded).
- [x] The spec is v1.1; README, CLAUDE.md, MEMORY.md, TODO.md, CHANGELOG.md updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `lughac run docs/decisions/decision-010/sort_swap.la` prints `2972`
