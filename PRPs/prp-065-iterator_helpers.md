## FEATURE: Iterator helpers — lazy `map`, `filter`, `take`, `skip`, `enumerate`; `collect`, `count`, `fold`, `any`, `all`, `find`, `sum`; `xs.iter()`, `s.bytes()`, `range(a, b)`, `for b of s`; milestone 36, v10 complete.

**Status:** approved 2026-10-14 — session 83. This is the last part of the user's "in one go" (milestones 34–36), merged once CI is green.
**Milestone:** 36 (one PRP; completes v10; tags `m36` and `v10` at merge)
**Spec:** `Language v10 Draft.md`:
- decisions 2–4, and the choices on adapters, `enumerate`, receivers, copies, `take`/`skip` and `sum`;
- §4 "Iterator sources" and "Iterator helpers";
- §5 (`for b of s`);
- §9;
- §10 "Iterator helpers";
- §11 (milestone 36);
- §12.

**Decisions:** MEMORY 76 (v10), MEMORY 62 (iterators), MEMORY 44 (array helpers in the prelude).

## OBJECTIVE
The §10 iterator-helpers program prints `16,4,36`, `5050`, `63`, `0=104;1=101;2=121;`, `3`, `true`, `5`, `31`:

```
xs.iter().filter(even).map(fun(n) = "{n * n}").collect()
range(10, 1000000000).filter(fun(n) = n % 7 == 0).take(3).sum()   // stops early
for p of "hey".bytes().enumerate() { … }
for b of "banana" { … }
```

## CONTEXT

- **Starting state:**
  - `Iter<T>` is a prelude interface, and `for … of` loops over anything with `next` (`check/iterators.rs`, `codegen/iterators.rs`).
  - Array, string, `Option` and `Result` helpers are prelude functions that `check/helpers.rs` picks by receiver type.
  - `for … of` over a string is E0411.
- **Ending state:**
  - iterator helpers and sources in a new prelude file;
  - `for b of s`;
  - v10 complete.
- **Related existing code:**
  - `src/prelude.la`, `src/lib.rs` (`PRELUDE`);
  - `src/check/{helpers,iterators,method_calls,call,array,types,iface_values}.rs`, `src/check/results.rs` (`prelude_type`);
  - `src/codegen/{array,methods,function}.rs`.
- **Open decisions that must be resolved first:** none.

### Design
- **A new prelude file, `src/prelude_iter.la`,** is joined after `prelude.la` into the one `$prelude` module: `PRELUDE = concat!(include_str!(…), include_str!(…))`. This keeps `prelude.la` under 300 lines. It holds:
  - **Sources:**
    - `struct ArrayIter<T> { xs: T[], at: i64 }` and `fun array_iter<T>(xs: T[])`;
    - `struct Bytes { s: string, at: i64 }` and `fun str_bytes(s: string)`;
    - `struct Range { at: i64, end: i64 }` and `fun range(from: i64, to: i64)`;
    - each with its `next`.
  - **Adapters:**
    - `Map<T, U> { src: Iter<T>, f: fun(T): U }`;
    - `Filter<T> { src: Iter<T>, keep: fun(T): bool }`;
    - `Take<T>` and `Skip<T>` (`{ src: Iter<T>, left: i64 }`);
    - `Enumerate<T> { src: Iter<T>, at: i64 }`, yielding `Indexed<T> { public i: i64, public value: T }`;
    - each with its `next`, and a constructor `iter_map`, `iter_filter`, … taking `it: Iter<T>` first.
  - **Consumers:**
    - `iter_collect`, `iter_count`, `iter_fold`, `iter_any`, `iter_all` and `iter_find`, each a `for … of` over its `Iter<T>`;
    - `iter_sum_<N>` for each of the ten number types.
- **Choosing a helper:**
  - **Arrays and strings:** `helper_function` maps `xs.iter()` to `array_iter` and `s.bytes()` to `str_bytes`.
  - **Iterators** (new `check/iter_helpers.rs`, hooked in `on_value` after the array helpers): for a receiver whose type has no own method or bound or interface item by that name, and whose `next_of` gives `T`:
    - the helper's prelude function (`iter_<name>`, or `iter_sum_<T>` for a number `T`) is called with the receiver first;
    - the receiver converts to `Iter<T>` (`convert`, recorded in `Checked.conversions`), and `bind` binds the function's `T` from `Iter<T>`;
    - the rest is inferred as for the array helpers.

    Codegen's existing helper path (`$prelude.` callees through `prelude_target`) calls it, and the conversion boxes the receiver.
  - **`sum` over non-numbers,** and a name that isn't a helper, fall through to E0415.
- **`range(a, b)`:** in `check/call.rs`, a bare call of `range` that no module declares calls `$prelude.range`. Like every prelude function, it is compiled only when reached.
- **`for b of s`:**
  - **Checker:** `for_of` gives a `string` the element `u8`, with the array loop's checks.
  - **Codegen:** `for_of` treats a string as an array of `u8` behind the string header (8 bytes, not the array's 16).
- **Prelude type names:**
  - `prelude_type` resolves `Map`, `Filter`, `Take`, `Skip`, `Enumerate`, `Indexed`, `ArrayIter`, `Bytes` and `Range` to the prelude's, unless a module declares that name (as `Result`).
  - **Prelude structs print with their module,** `$prelude.Map<i64, string>`. Codegen names LLVM types and symbols by the printed type, so stripping the prefix, as `Result` (an enum) and `Iter` (an interface) are stripped, would let a program's own `struct Range` share LLVM names with the prelude's. No program can declare a name with `$`, so the printed names never clash.
- **Programs without these** give byte-identical IR. The prelude's new items are compiled only when reached.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Prelude:** `src/prelude_iter.la`, and `PRELUDE` joining the two files.
- **Checker:**
  - `check/iter_helpers.rs` and its hook;
  - `helper_function`'s `iter`/`bytes`;
  - `range` in `call.rs`;
  - strings in `for_of`;
  - `prelude_type` names;
  - `Display` of prelude structs.
- **Codegen:** strings in `for_of`.
- **Spec:**
  - the v10 changes summary (with `range` as a prelude name);
  - §4 (sources, helpers, prelude type names);
  - §5 (`for b of s`);
  - §9;
  - the §10 program;
  - §11 row 36 done;
  - the title and status say v10 is complete;
  - §12 (the two v10 questions).
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 79, CONTEXT. The tags `m36` and `v10` are added at the merge.

### Must NOT Do
- **No `zip`, `chain`, `take_while`, `skip_while`, `flat_map`, `min`/`max` or `position`.**
- **No `for n of mut c`.**
- **No change to the eager array helpers.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0415:** `sum()` over non-numbers; a helper name on something that isn't an iterator (`(5).map(f)`).
- **E0403:** a closure of the wrong type (`it.filter(fun(x) = 1)`), or a non-`i64` `take`/`skip`/`range` argument.
- **E0405:** the wrong argument count.
- **E0411:** `for … of` over something that isn't iterable (unchanged; no longer a string).
- **At run time:** `sum` panics on overflow (`integer overflow`), as `+` does.

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — checker (`check/iter_helpers.rs`):
- [x] **Types:**
  - `map` gives `$prelude.Map<i64, string>`, `filter` gives `Filter<i64>`, `enumerate` yields `Indexed<i64>`;
  - `collect` gives `T[]`, `count` an `i64`, `find` an `Option<T>`, `sum` the element type;
  - `xs.iter()` gives `ArrayIter<i64>`, `s.bytes()` gives `Bytes`, `range(0, 3)` gives `Range`.
- [x] **Receivers:**
  - a user iterator type;
  - an `Iter<i64>` value;
  - a type parameter bound by `Iter<T>`;
  - a type's own `map` wins.
- [x] **Errors:**
  - E0415 for `sum` on strings and for `(5).map(f)`;
  - E0405 for `take()`;
  - E0403 for `take(true)`.
- [x] **`for b of s`** binds `u8`, and a user `fun range` is called instead of the prelude's.

Unit tests — codegen: `for b of s` loads bytes 8 bytes past the string pointer.

Acceptance — `tests/programs/m36/`:
- [x] `helpers.la`: the §10 program.
- [x] `more.la`:
  - every helper;
  - laziness: a side-effecting `map` runs only for values pulled, on an endless user iterator;
  - `skip` past the end;
  - negative `take`/`skip`;
  - `enumerate` over a user iterator;
  - chains through `Iter<T>` variables;
  - `sum` of `f64` and `u8`;
  - `collect` of strings;
  - helpers called in generic code bound by `Iter<T>`;
  - `range` with `a >= b`.
- [x] `overflow.la`: `sum` overflow panics (exit 101).
- [x] `e0415.la`: reject golden (`sum` over strings).
- [x] The §10 program runs from the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-065-iterator_helpers`, merged into `main` after CI passes; tags `m36` and `v10`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m36/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] The v10 spec is complete; docs updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m36/helpers.la
```

## AMENDMENTS (during build, session 83)

- **Prelude methods compile only when reached.** A plain program got `Bytes.next` and `Range.next`, because codegen compiled every non-generic method.
  - **Codegen:** it now skips the prelude's `impl`s in `declare_methods`/`define_methods` and takes those methods from `monos`. The method-call, iterator-loop and table lookups fall back to `monos`.
  - **Checker:** it lists them where reached: method calls (`on_value`), loops (`iterator_element`), conversions (already), and calls through bounds (`bound_target`).
  - **New program:** `m36/sources.la` reaches them each way.
- **The `sum` helpers are loops,** not `iter_fold` with closures. Prelude closures are checked with every program, and `check::closures::tests::captures_are_recorded_in_order` counts them all.
- **Earlier tests changed:**
  - `check::array` `for c of "abc"` is no longer E0411; it now uses `for c of 5`;
  - the E0411 example in the spec's code table likewise.
- **`take(true)` is E0403,** as the PRP said; the first test draft expected E0401.
- **A `sum` overflow panics in the prelude:** `<prelude>:L:C`, in the `m36/overflow` golden.
- **IR:** the 138 earlier programs that compile give byte-identical IR. `-O2` was checked by hand for `m36/helpers`, `m36/more`, `m36/sources` and `m36/overflow`.
- **No file splits:** the new prelude code is a new file, `src/prelude_iter.la` (263 lines).
