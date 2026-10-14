## FEATURE: More iterator helpers — `zip` (`Pair`), `chain`, `take_while`, `skip_while`, `flat_map`, `min`, `max`, `position`; milestone 43.

**Status:** approved 2026-10-14 — session 93. This is part of the user's "in one go" (milestones 41–44), merged once CI is green.
**Milestone:** 43 (one PRP; tag `m43` at merge)
**Spec:** `Language v13 Draft.md`:
- decision 3, and the choices on `flat_map`, `min`/`max`, `position`, `chain`, `zip` and `Pair`;
- §4 (`Pair`);
- §5 helper table;
- §9;
- §10 "More iterator helpers";
- §11 (milestone 43).

**Decisions:** MEMORY 86 (v13), MEMORY 79 (v10 helpers in the prelude).

## OBJECTIVE
The §10 more-helpers program prints its eight lines: `[Pair { first: 1, second: "a" }, Pair { first: 2, second: "b" }]`, `[1, 2, 3, 4]`, `[1, 2]`, `[3, 1]`, `[0, 0, 1, 0, 1, 2]`, `Some(1)`, `Some(9)`, `Some(2)`.

## CONTEXT

- **Starting state:**
  - v10's helpers are Lugha in `src/prelude_iter.la`, picked by `check/iter_helpers.rs` (`HELPERS`, `ITER_TYPES`) as `iter_<name>`, with the receiver converted to `Iter<T>`.
  - `prelude_iter.la` is at 268 lines.
- **Ending state:** eight more helpers, and `Pair`.
- **Related existing code:** `src/prelude_iter.la`, `src/lib.rs` (`PRELUDE`), `src/check/iter_helpers.rs`.
- **Open decisions that must be resolved first:** none.

### Design
- **A third prelude file, `src/prelude_more.la`,** is joined after the other two. It holds:
  - `struct Pair<A, B> { public first: A, public second: B }`;
  - **`Zip<A, B> { a: Iter<A>, b: Iter<B> }`** with `iter_zip(it, other: Iter<B>)`. `B` is read off the argument by the v6 rule for interface-typed parameters.
  - **`Chain<T> { a, b, second: bool }`** with `iter_chain(it, other: Iter<T>)`.
  - **`TakeWhile<T> { src, keep, done }`** and **`SkipWhile<T> { src, skip, started }`**.
  - **`FlatMap<T, U, I: Iter<U>> { src: Iter<T>, f: fun(T): I, cur: I[] }`** with `iter_flat_map<T, U, I: Iter<U>>(it, f: fun(T): I)`. The current inner iterator is held in a 0- or 1-element array, so `self.cur[0].next()` steps it in place (a binding from `Option` would be a copy).
  - **Consumers:** `iter_min<T: Ord>` and `iter_max<T: Ord>` (`Option<T>`, the first extreme), and `iter_position` (`Option<i64>`).
- **Checker:** `HELPERS` gains the eight names, and `ITER_TYPES` gains `Pair`, `Zip`, `Chain`, `TakeWhile`, `SkipWhile` and `FlatMap`. Bounds (`Ord`, `I: Iter<U>`) are checked by ordinary inference (E0419 / E0422).
- **Programs not using them** give byte-identical IR, since prelude items compile only when reached.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** `src/prelude_more.la`, `PRELUDE` joining three files, and the two lists.
- **Spec:**
  - §4 `Pair`;
  - §5 the helper table and the names list;
  - §9;
  - the §10 program;
  - §11;
  - the changes summary.
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 89, CONTEXT. The tag `m43` is added at the merge.

### Must NOT Do
- **No tuples, `unzip`, `rev`, `step_by` or `windows`.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0419:** `min`/`max` on a type that isn't `Ord`.
- **E0403:** `chain` with another element type.
- **E0405:** the wrong argument count.
- **E0415:** a helper on something that isn't an iterator.

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — checker (`check/iter_helpers.rs`):
- [ ] **Types:**
  - `zip` yields `$prelude.Pair<i64, string>`;
  - `chain`, `take_while` and `skip_while` keep `T`;
  - `flat_map` yields `U`;
  - `min` gives `Option<T>`;
  - `position` gives `Option<i64>`.
- [ ] **Errors:** E0419 for `min` on a struct; E0403 for `chain` of `i64` and `string`.

Acceptance — `tests/programs/m43/`:
- [ ] `helpers.la`: the §10 program.
- [ ] `more.la`:
  - `zip` of different lengths both ways;
  - `chain` of empty iterators;
  - `take_while` that never fails;
  - `skip_while` that always holds;
  - `flat_map` with empty inner iterators;
  - `min`/`max` of strings, ties (the first), and empty;
  - `position` not found;
  - `Pair` built by the program and printed;
  - laziness: `zip` with an endless iterator.
- [ ] The §10 program runs from the spec.

Regression:
- [ ] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-072-more_iterator_helpers`, merged into `main` after CI passes; tag `m43`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The §10 program and the `m43/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [ ] The spec is at milestone 43; docs updated.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m43/helpers.la
```
