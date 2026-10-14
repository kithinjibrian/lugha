## FEATURE: `for … of mut` — iterators advanced in place, array elements as mutable aliases; milestone 42.

**Status:** merged 2026-10-14 — session 92. This is part of the user's "in one go" (milestones 41–44), merged once CI is green.
**Milestone:** 42 (one PRP; tag `m42` at merge)
**Spec:** `Language v13 Draft.md`:
- decision 4, and the choices on `for x of mut e`;
- §3 (`for_stmt`);
- §5;
- §9;
- §10 "`for … of mut`";
- §11 (milestone 42).

**Decisions:** MEMORY 86 (v13), MEMORY 62 (iterators work on a copy).

## OBJECTIVE
The §10 program prints `[2, 4, 6]`, `a b`, `c d`: a loop doubles every element in place, and a second loop over the same iterator resumes after a `break`.

## CONTEXT

- **Starting state:**
  - `for x of xs` binds a borrowed, immutable element;
  - `for x of it` loops over a hidden copy (`check/array.rs::for_of`, `check/iterators.rs::iterator_loop`, `codegen/array.rs::for_of`, `codegen/iterators.rs::iterator_loop`).
- **Ending state:** `for x of mut e` on arrays and iterators.
- **Related existing code:**
  - `src/ast/mod.rs` (`ForIter`), `src/parser/stmt.rs`, `src/parser/sexp/mod.rs`;
  - `src/check/{stmt,array,iterators,assign}.rs`, `src/check/liveness/walk.rs`;
  - `src/codegen/{stmt,array,iterators,structs}.rs` (`place_address`).
- **Open decisions that must be resolved first:** none.

### Design
- **AST and parser:**
  - a new `ForIter::MutOf(Expr)`, read when `mut` follows `of`;
  - the sexp is `(of-mut e)`;
  - every match on `ForIter` gains the arm.
- **Checker** (new `check/for_mut.rs`, `for_of_mut`):
  - **`e`** must be a place (a name, field or element; E0502 otherwise) rooted in a `let mut` local or `mut self` (E0501 otherwise), with loop-specific messages. It is recorded as changed.
  - **A string** is E0506.
  - **An array:**
    - `x` is declared as a mutable local of the element type, so assignments, compound assignments and `mut self` calls on it check;
    - `e`'s path is pushed on the iteration guard, so changing `e` itself in the body is E0507, as for `for … of`.
  - **An iterator:** `e`'s path is guarded too, and `iterator_loop` checks the rest, with `x` an immutable value as before.
- **Codegen:**
  - `codegen/array.rs::for_of` and `codegen/iterators.rs::iterator_loop` take an `in_place` flag:
    - **array:** `x`'s local *is* the element's address (an alias, so every write reaches the array). An `i1` in memory is one byte, so `bool` elements alias too;
    - **iterator:** `next` is called on `place_address(e)`, not on a copy.
  - `codegen/stmt.rs` dispatches `MutOf` with the flag set.
- **Programs without `of mut`** give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- The AST variant, parser, sexp, liveness walk, checker file, codegen flags, and the stmt dispatch in the checker and codegen.
- **Spec:**
  - §3;
  - §5 (for over an array and over an iterator, with `mut`);
  - §9;
  - the E0501, E0502, E0506 and E0507 rows;
  - the §10 program;
  - §11;
  - the changes summary.
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 88, CONTEXT. The tag `m42` is added at the merge.

### Must NOT Do
- **No `mut` on `for … in` ranges.**
- **No change to `for x of e` without `mut`.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0501:** `for x of mut xs` where `xs` isn't `let mut`.
- **E0502:** `for x of mut f()` (not a place).
- **E0506:** `for b of mut s` on a string.
- **E0507:** assigning to `xs`, growing it, or calling a `mut self` method on the iterator, inside its own `of mut` loop.
- **E0411:** not iterable (unchanged).

## SECURITY CONSIDERATIONS
- No new input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — parser: `for x of mut xs { }` prints `(for x (of-mut xs) …)`.

Unit tests — checker (`check/for_mut.rs`):
- [x] **Allowed:**
  - `x += 1` on an array element;
  - `x.n = 2` on a struct element;
  - a `mut self` call on `x`;
  - an iterator loop.
- [x] **E0501, E0502, E0506.**
- [x] **E0507:** `xs.push(1)` inside the loop; `it.next()` inside an iterator loop.

Unit tests — codegen: an array `of mut` loop stores through the element's address (no copy of the element into its own slot).

Acceptance — `tests/programs/m42/`:
- [x] `mut.la`: the §10 program.
- [x] `more.la`:
  - struct elements changed through a field and a `mut self` method;
  - `bool` elements;
  - `continue`/`break` inside an array loop (writes kept);
  - nested arrays (`for row of mut grid { for c of mut row { … } }`);
  - an iterator held in a struct field (`for x of mut s.it`);
  - an interface-typed iterator (`let mut it: Iter<i64> = …`) resumed.
- [x] `e0501.la`: reject golden.
- [x] The §10 program runs from the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-071-for_of_mut`, merged into `main` after CI passes; tag `m42`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m42/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] The spec is at milestone 42; docs updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m42/mut.la
```

## AMENDMENTS (during build, session 92)

- **The parser test** is in `parser/conditional.rs`, since `parser/stmt.rs` is at 296 lines.
- **The codegen unit test** checks that the loop variable gets no `alloca` of its own; it is in `codegen/iterators.rs`, since `codegen/array.rs` is at 293 lines.
- **`check/mod.rs`** is now at 300 lines, the limit. The next module needs a split proposal or another home.
- **IR:** the 151 earlier programs that compile give byte-identical IR. `-O2` was checked by hand for both `m42` programs.
- **No file splits.**
