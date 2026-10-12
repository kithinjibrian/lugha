## FEATURE: E0510 — a statement may not change what contains the place it writes — as Lugha v1.2.

**Status:** implemented 2026-10-12 — session 33
**Milestone:** none; Lugha v1.2. Tag `v1.2` after merge.
**Spec:** `Language v1 Specification.md` §4 (places, mutability), §5 (evaluation order, compound assignment, method calls, `swap`), §9 (codes)
**Decisions:**
- MEMORY 27 (`mut self` place rules, E0508);
- MEMORY 28 (growable arrays);
- MEMORY 31 (`swap`, E0509, `PlaceUse`, `Path` prefix checks).

## OBJECTIVE
A write can no longer land in an array that the same statement has already replaced, grown or shrunk. Today `xs[0] = { xs.push(2); xs.push(3); 9 };` leaves `xs[0]` as 1: the element's address is computed first (spec §5), then growth moves the elements, and the 9 goes to the old array. After this PRP, the compiler rejects such statements with **E0510**. No run-time cost, no codegen change.

## CONTEXT

- **Starting state:** three statements write through a place, and each fixes the place's address before the rest of the statement runs:
  - an assignment (plain or compound): the place, then the value;
  - `swap`: both places, then the loads and stores;
  - a `mut self` call, built-in or user: the receiver, then the arguments.

  A function can't reach the caller's locals, so any change to the place's array must appear in the statement itself: in the value, an argument, or the place's own index expressions. Such a change is not checked today.
- **Ending state:**
  - The checker records every place each statement changes, with its path.
  - For a statement writing place P, any change made by the rest of the statement to a place that strictly contains P (by the E0509 prefix test) is E0510.
  - The spec becomes v1.2.
- **Related existing code:**
  - `src/check/{assign,swap,method_calls,array}.rs` (`check_place_for`, `PlaceUse`, `Path`);
  - `src/check/errors/places.rs`;
  - `src/codegen/growth.rs` (push re-reads the array after its argument, so a same-depth change is safe).
- **Open decisions that must be resolved first:** none.

### Amendments during implementation (session 33)
- **One error per change:** `xs[0] = { xs.push(2); xs.push(3); 9 };` reports two E0510s, one per `push`. Each is a separate change, and the "once" rule only drops the same change being reported against two enclosing statements.
- **Compound assignment:** with bad operands (E0404) it still checks E0510, rather than returning early.
- **Reset:** `Checker.changes` is cleared in `body()`, next to `scopes`.

### Discovery answers (session 33)
1. **Approach:** reject at compile time with a new error, E0510, in the style of E0507/E0508. Not a second lookup at run time, and not documenting the lost write.
2. **Precision:** only changes to something that *strictly contains* the written place are rejected, using the same prefix test as E0509 (any two indexes might be equal).
   - Rejected: `xs` for `xs[0]`; `g[1]` for `g[0][0]`.
   - Allowed: changing a sibling (`p.n += 1` while writing `p.items[0]`), or the place itself or a same-depth place (`xs[0] = { xs[1] = 5; 9 }`).
3. **Versioning:** Lugha **v1.2**. The spec is retitled with a "Changes since v1.1" line; tag `v1.2`.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Checker** — new `check/conflicts.rs`
- **Recording changes:** `check_place_for` records each changed place as `(Path, span)` in a list on the checker, whenever the place has a path. That covers:
  - an assignment's place;
  - a `mut self` receiver, built-in or user;
  - both `swap` arguments.

  The list is reset for each function body.
- **Checking a statement:** each statement notes the list's length before checking its parts (`mark`). After checking them, it calls `no_change_inside(target, place_use, mark)`, which looks at every change recorded since `mark` except the target's own. A change whose path strictly contains the target's path (`overlaps`, smaller depth) is E0510. The three statements:
  - **Assignment:** the place (including its index expressions), then the value. `assign.rs`.
  - **`swap(a, b)`:** checked against both `a` and `b`. `swap.rs`.
  - **`mut self` call:** the receiver's index expressions and the arguments. `method_calls.rs`. For a user method E0508 already rejects any mention of the root in an argument, so E0510 is checked first and E0508 is skipped when it fires (one error per problem).
- **Reporting:** each change is reported at most once. In nested statements the same inner change could conflict with two outer places; the duplicate is dropped.
- **Wording, by `PlaceUse`:**

  > error[E0510]: cannot change `xs` while assigning to `xs[…]`

  - **Paths:** both are written as `Path::describe` gives them, with `[…]` for indexes.
  - **The verb:** "assigning to", "swapping" or "calling `m` on".
  - **Spans:** the primary span is the changing place. The label is on the target: "this place's address was already taken".
  - **Help:** "make the change in its own statement, before or after this one".

**Codegen**
- No change. The addresses that E0510 now protects were already correct whenever nothing containing them changed.

**Spec** — `Language v1 Specification.md` (file name unchanged)
- **Title:** "Lugha v1.2 Specification". Add a "Changes since v1.1: E0510 (section 5)" line above the v1.1 line.
- **§5:** a paragraph "**Changing what a statement writes.**" after compound assignment. It says:
  - an assignment, `swap` or `mut self` call fixes its place before the rest runs;
  - so the rest may not change anything containing that place (E0510);
  - examples of what is rejected and what is allowed.
- **§9:** an E0510 row.

**Docs**
- README "Lugha v1.2".
- CLAUDE.md tree (`check/conflicts.rs`, `tests/programs/v1_2/`).
- MEMORY decision 32, CHANGELOG, TODO (tick the found item), CONTEXT. Tag `v1.2` after merging.

### Must NOT Do
- No run-time checks or codegen changes, and no change to evaluation order.
- No rejection of same-depth or sibling changes, or of reading the root (`xs[i] = xs[i] + 1`, `xs.push(xs[0])` stay valid).
- No `unsafe`, no new dependencies.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- E0510 is reported and checking continues. It never stops the checker, and it doesn't change the statement's type.

## SECURITY CONSIDERATIONS

- **Memory safety:** the lost write was never unsafe. The old array stays alive under Boehm, and a write past `len` after `clear` is still inside `cap`. This is a correctness fix.
- **Gaps:** none expected. Only a statement's own sub-expressions can change a local, since there are no closures and no globals.
- **No `unsafe`.**

## TESTS TO WRITE

Unit tests — `check/conflicts.rs`:
- [x] **Assignment, rejected:**
  - `xs[0] = { xs.push(2); 9 };`
  - `xs[{ xs.clear(); 0 }] = 1;`
  - `g[0][0] = { g[1] = [5]; 1 };`
  - `g[0][0] = { g = [[5]]; 1 };`
  - `xs[0] += { xs.pop(); 1 };`
- [x] **Assignment, allowed:**
  - `xs[0] = { xs[1] = 5; 9 };`
  - `p.items[0] = { p.n += 1; 7 };`
  - `xs[i] = xs[i] + 1;`
  - `xs = { xs.push(1); xs };`, where the target is the variable itself
  - a shadowing `let mut xs` inside the value
- [x] **`swap`:** `swap(xs[0], ys[{ xs.push(1); 0 }])` rejected; `swap(xs[0], ys[{ xs[1] = 2; 0 }])` allowed.
- [x] **`mut self`:**
  - `g[0].push({ g.clear(); 1 })` rejected, and `g[0].push({ g[0] = [7]; 1 })` allowed;
  - `vs[{ vs.push(v); 0 }].bump()` rejected;
  - a user `mut self` call whose argument changes the receiver's root gives E0510 only, not E0508 as well.
- [x] **Message and labels:** the exact message for each verb, the label, and one report for a doubly nested change.

Acceptance — `tests/programs/v1_2/` (also cross-checked at `-O2`):
- [x] **`places.la`:** every allowed case above runs and prints the expected values (expected output written by hand).
- [x] **Reject goldens:** `e0510.la` (the TODO example), `e0510_swap.la`. Captured and reviewed.
- [x] **Regression:** every earlier test passes, including all spec programs.

## ROLLBACK PLAN

- Branch `prp-027-place_change_check`, merged into `main` after green CI; then tag `v1.2`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The TODO example is E0510; the allowed cases compile and run correctly.
- [x] The spec is v1.2; README, CLAUDE.md, MEMORY.md, TODO.md, CHANGELOG.md updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `lughac check tests/programs/v1_2/e0510.la` → E0510, exit 1
