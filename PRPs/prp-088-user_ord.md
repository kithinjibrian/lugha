## FEATURE: User-defined `Ord` — a type with `cmp(self, other: Self): Ordering` gets `<`, `<=`, `>`, `>=` and satisfies `T: Ord`; tuples compare through it; `xs.sort()`; milestone 59.

**Status:** merged 2026-10-15 — session 124 (approved in session 122, "approved. build in one go"; proposed in session 121); tag `m59`.
**Milestone:** 59 (one PRP; tag `m59` at merge)
**Spec:** `Language v15 Draft.md` (decisions 37–40); spec §4 (bounds, `Ord`, tuples, operator methods, array helpers), §5 (comparison evaluation), §9 (E0404, E0419 rows; lowering), §10 (a program), §11 (row 59).

**Decisions:**
- follows the session-112 TODO recommendation;
- mirrors M52's operator methods (MEMORY 100–101);
- `Ord` doesn't imply `Eq` (v2) and `==` stays structural;
- no open entries in DECISIONS.md apply.

## OBJECTIVE
Structs and enums can be ordered:
- `a < b` on a `Score`, `math.min(a, b)`, `scores.iter().min()`;
- `(score, i) < (other, j)`;
- `scores.sort()`.

All of them work through one `cmp` method.

## CONTEXT

- **Starting state:**
  - **Bound:** `Ord` is a built-in bound (`check/bounds.rs::ord_ok`, `interfaces.rs`), held by the numbers, `string`, `char` and tuples of `Ord` elements.
  - **Comparisons:** `<` on structs is E0404. Tuples compare through `lugha_cmp_<type>` (`codegen/tuples.rs`).
  - **Operator methods:** M52 maps `+` etc. to methods (`check/operators.rs`, `codegen/operators.rs`).
  - **Sorting:** `sort_by(less)` is a prelude helper (`src/prelude/core.la`, `check/helpers.rs`).
- **Ending state:** decisions 37–39.
- **Related existing code:**
  - **Checker (`src/check/`):** `bounds.rs` (`ord_ok`), `ops.rs` (comparisons), `operators.rs` (method shapes), `tuples.rs`, `helpers.rs` (`sort`), `errors/`.
  - **Codegen (`src/codegen/`):** `operators.rs`, `tuples.rs` (`lugha_cmp_`), `arith.rs` (comparisons).
  - **Prelude:** `src/prelude/ops.la` or `core.la` (`Ordering`, `sort`).
- **Open decisions that must be resolved first:** none.

### Design
1. **Prelude:** `enum Ordering { Less, Equal, Greater }` and `fun sort<T: Ord>(xs: T[]): T[]`, a stable merge sort like `sort_by`, using `<`. A module's own `Ordering` shadows it, as for `Iter`.
2. **`ord_ok`:** a struct or enum is `Ord` when it has a method `cmp` with exactly the shape `(self, other: Self): Ordering`, the prelude's `Ordering`; an `impl` with bounds applies only where they hold. Tuples are `Ord` when every element is. Interface values, arrays, `Option` and function values are not.
3. **Comparisons:** `a < b` on such a type type-checks like M52's operators and lowers to `a.cmp(b)`. The result's tag is then compared: `<` is `== Less`, `<=` is `!= Greater`, `>` is `== Greater`, `>=` is `!= Less`. `cmp` with another shape gives E0404 for `<`, with help naming the shape. In generic code, `<` on `T: Ord` lowers per instance, built in or through `cmp`.
4. **Tuples:** `lugha_cmp_<tuple>` calls `cmp` for user-type elements and maps `Less`/`Equal`/`Greater` to −1/0/1.
5. **`sort`:** a prelude helper on arrays, registered beside `sort_by`. `xs.sort()` with a non-`Ord` element is E0419.

### Risk
- **Enum tag order:** `Ordering`'s variants are numbered in declaration order, `Less` = 0. Codegen must compare tags by variant, not by assumed numbers; a test covers all four operators.
- **Generic instances:** `<` inside `math.min`, `clamp`, `it.min()` and `sort` at a user type must reach `cmp`. Each one gets a test.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** everything above.
- **Spec v15:**
  - §4: the `Ord` paragraph and bounds at a use; `Ordering`; tuples; the array-helper table (`sort`);
  - §9: E0404 and E0419 examples, and a lowering note;
  - §10: program "Ordering";
  - §11: row 59;
  - the changes summary, with the compatibility note: a module's own `Ordering`, or an array helper named `sort`, keeps its meaning.
- **Docs:** CLAUDE.md (STACK; prelude contents), README, TODO (mark "User-defined `Ord`" done), CHANGELOG, MEMORY 114 (plan) and 115 (built), CONTEXT; tag `m59`.

### Must NOT Do
- **`==` stays structural:** `cmp` never changes `==`.
- **No `cmp` on numbers, `string` or `char`.**
- **No unprefixed `Less`/`Equal`/`Greater`.**
- **No implicit `Eq` from `Ord`.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0404:** `<` on a type without a `cmp` of the right shape, with help naming the shape.
- **E0419:** `T: Ord` (or `sort`, `min`, `max`) with a type lacking it, with the same help.

## SECURITY CONSIDERATIONS
- **No `unsafe`, no dependencies, no external input.**

## TESTS TO WRITE

Checker unit tests:
- [x] **Satisfies `Ord`:** a struct with `cmp`; an enum with `cmp`; a generic struct with `cmp` in an `impl` with bounds.
- [x] **Not `Ord`:**
  - `cmp` returning `i64`;
  - `cmp` taking `mut self`;
  - `cmp` with a parameter other than `Self`;
  - a struct without `cmp` (E0404 for `<`, E0419 for `math.min`);
  - interface values;
  - an array of an `Ord` type.
- [x] **Shadowing:** a module's own `Ordering` enum means its own; `sort` on a non-`Ord` element is E0419.

Codegen unit test:
- [x] `a < b` on a struct calls `lugha_fn_<Type>.cmp`.

Acceptance (`tests/programs/m59/`):
- [x] **`ordering.la`:**
  - `<`, `<=`, `>`, `>=` on a struct and an enum, all four outcomes;
  - `math.min`/`max`/`clamp`, `it.min()`/`max()`;
  - tuple tie-breaking;
  - `sort()`, stable on equal keys;
  - a generic `largest<T: Ord>`.
- [x] **`e0404.la`, `e0419.la`:** rejected programs.
- [x] The §10 program.

Regression:
- [x] Every earlier program passes unchanged.

## ROLLBACK PLAN
- Branch `prp-088-user_ord`, merged into `main` after CI passes; tag `m59`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] All tests pass at `-O0`, and the `m59/` programs at `-O2`.
- [x] The spec is at milestone 59; docs updated.
- [x] No file over 500 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m59/ordering.la
```

## AMENDMENTS

Session 124, while building:
1. **Printing:** a non-generic enum now prints as its key, like a struct, so the prelude's is `$prelude.Ordering` in messages. Codegen's layout tables and LLVM type names use the printed name; with the prefix stripped, `$prelude.Ordering` was looked up as `Ordering`, which wasn't in the table (an internal error) and could have collided with a module's own `Ordering`. `Ordering` was the only non-generic prelude enum; `Result` is generic and still prints bare.
2. **`Ordering` and `sort` live at the end of `src/prelude/ops.la`,** not `core.la`, so earlier prelude line numbers (in panic goldens such as `m18/split_panic`, `m36/overflow`) don't move.
3. **Resolution:** `prelude_type` knows `Ordering`, so the bare name works as a type and in `Ordering.Less` (`construct.rs::enum_named`) unless the module declares its own.
4. **Instances:** `order_uses` records `cmp` for generic types (through `used`) and through `T: Ord` (`bound_used(…, "cmp")`). For the latter, `monomorphize` lists `cmp_targets`, which include a tuple's elements', so `sort` over `(Box<i64>, i64)` compiles every `cmp` it calls.
5. **`impl` bounds** are judged quietly (`bounds_hold`: `Eq`, `Ord` and the operator bounds). A `cmp` in an `impl` bounded by any other interface doesn't make the type `Ord`; conservative, and noted here.
6. **The codegen IR test is an integration test,** `tests/ordering.rs`, because codegen unit tests check without the prelude.
7. **Existing tests updated:**
   - `m9/e0404_struct_order.stderr` gains the new help line;
   - `calls_need_the_right_kind_of_method` uses `xs.shuffle()` (`sort` is a helper now);
   - `captures_are_recorded_in_order` ignores capture-less closures (the prelude's `sort` has one).

