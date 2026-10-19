## FEATURE: `xs + ys` joins two arrays, `+=` on arrays and strings, and `T[]` satisfies `Add`; milestone 122 (v17).

**Status:** proposed 2026-10-20 (session 292), from the user's brief "add a concat method to arrays". Discovery: (1) an operator, `xs + ys`, rather than a `concat` method; (2) reaching as far as `string`'s `+` does, `Add` bound included; (3) `+=` for arrays and for strings (today `s += t` is E0404).

## OBJECTIVE
```lugha
let xs = [1, 2] + [3];            // [1, 2, 3]; both operands unchanged
let mut log: string[] = [];
log += ["started"];               // log = log + ["started"]
let mut s = "a";
s += "b";                         // "ab" (E0404 before)
fun both<T: Add>(a: T, b: T): T = a + b;
println(both([1], [2]));          // [1, 2]
```

## CONTEXT
- `check/ops.rs` allows `+` on two equal types when they are `string` (`BinOp::Add if l == Type::String`); `codegen/heap.rs` lowers it to `lugha_rt_str_concat`.
- `check/operators.rs` `builtin_satisfies` (around line 67): `string` satisfies `Add` as a built-in; generic instances keep the built-in operator at a number or string type (`codegen/operators.rs`, `codegen/instances.rs`).
- `check/assign.rs`: `+=`…`%=` need `target.is_numeric()`; `codegen/stmt.rs` maps `AssignOp::Add` to `BinOp::Add`.
- `check/helpers.rs`: array helpers become calls of `$prelude` functions (`src/prelude/core.la`); this is the model for lowering `xs + ys`.
- Arrays are copy-on-write (`codegen/cow.rs`), so a prelude body built from `push` gets nested sharing right with no new codegen.
- Spec v17 §4 "Strings" (line ~632, "`+` concatenates two strings"), "Arrays" (~609), "Operator bounds" (~763), "Satisfying one" (~783), the v15 summary line (~75); §11 milestones.
- Open decisions: none.

## DECISIONS (draft decisions 262–268)
262. **`xs + ys`** on two arrays of exactly the same type `T[]` is a new `T[]`: `xs`'s elements, then `ys`'s, each copied as any value is. Neither operand changes. Evaluation: `xs`, then `ys`. Different element types are E0404 (as `"a" + 1` is); a `[]` operand takes the other side's type (`xs + []`, `[] + xs`).
263. **Lowering**: the checker records `xs + ys` as a call of a new prelude function `$concat<T>(a: T[], b: T[]): T[]` in `core.la` (copy `a`, push every element of `b`), the way `check/helpers.rs` records `xs.map(f)`. Codegen needs no array-specific operator code. `concat` is not a method, and a program's own `concat` is unaffected. The prelude name starts with `$` so no program can name or clash with it.
264. **`Add`**: `T[]` satisfies the prelude's `Add` for every `T`, as a built-in, like `string`. A `T: Add` instance at an array type uses the same `$concat` instance. `xs.add(ys)` on an array stays E0415 (arrays still have no `add` method, as `string` has none).
265. **`+=`**: `a += b` where `a` is an array or a `string` means `a = a + b`, with the same place rules as `+=` on numbers (`a` must be mutable, E0501…). The other compound operators stay numeric-only. `s += t` lowers to `lugha_rt_str_concat`; `xs += ys` to `$concat`.
266. **Not**: `concat`/`extend` methods, `+` between an array and a single element (`xs + 1`), `*` repeat, `+` on maps or sets, in-place appending as a promise (`xs += ys` may reuse `xs`'s buffer when copy-on-write allows, but the spec only promises the value).
267. **Docs**: spec §4 "Arrays" (`+`, `+=`), "Strings" (`+=`), "Operator bounds" and "Satisfying one" (arrays satisfy `Add`), the v15 summary line, the `+=` row of §3 if needed; §11 row 122; the Draft's decisions and changes by section; CHANGELOG (Added; Changed: `s += t` is accepted).
268. **Success**: milestone 122, tag `m122`.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- Checker: allow `BinOp::Add` on equal array types; record the `$concat` call (and its instance) for codegen; arrays satisfy `Add`; `+=` on arrays and strings.
- Prelude: `$concat` in `src/prelude/core.la`, with a `/** */` doc comment like its neighbours.
- Codegen: `xs + ys` and `xs += ys` call the recorded `$concat` instance; `s += t` calls `lugha_rt_str_concat`; generic `T: Add` instances at an array type call `$concat`.
- Hover and the language server show `xs + ys` as an array, with no special casing (the type comes from the checker's side table).

### Must NOT Do
- No `concat` method or any other new array method (the user chose the operator).
- No implicit conversion: `i64[] + i32[]` is E0404, not a widening.
- No new runtime C function (the prelude does the work).

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- No new diagnostic codes. E0404 for `+`/`+=` on arrays of different types or an array and a non-array; E0501 (and the other place codes) for `+=` on an immutable place, as today.
- Lowering a `+` the checker accepted with no recorded `$concat` instance is a compiler bug: `.expect` naming that invariant.

## SECURITY CONSIDERATIONS
- None: no external input, `unsafe`, crypto or credentials. Allocation goes through `push`, i.e. `lugha_rt_alloc`.

## TESTS TO WRITE
- [ ] Checker (`src/check/ops.rs` or `operators.rs` tests): `[1] + [2]` is `i64[]`; `xs + []` and `[] + xs` type; `string[] + string[]`; nested `i64[][]`.
- [ ] Checker errors: `[1] + ["a"]` E0404; `[1] + 1` E0404; `let xs = [1]; xs += [2];` E0501; `let mut x = 1; x += [1];` E0403/E0404 as numbers give today.
- [ ] Checker: `fun both<T: Add>(a: T, b: T): T = a + b;` called with arrays; `xs.add(ys)` stays E0415.
- [ ] Checker: `let mut s = "a"; s += "b";` accepted; `s -= "b"` still E0404.
- [ ] End to end `tests/programs/m122/concat.la`: values and order; operands unchanged after `+`; `+=` in a loop; an element array changed after joining doesn't change the joined copy (copy-on-write); structs as elements; empty sides; `T: Add` generic at arrays and strings; `s += t`; a function `concat` of the program's own still works.
- [ ] End to end `tests/programs/m122/concat_mismatch.la` + `.stderr` + `.exit` (E0404, exit 1).
- [ ] `tests/fmt_corpus.rs` picks the new programs up as it does every `tests/programs` file.

## ROLLBACK PLAN
- Branch to return to: `main`.
- State: as at the start of the milestone; the feature lives on `prp-154-array_concat` until merged.

## ACCEPTANCE CRITERIA
- [ ] The OBJECTIVE program runs and prints as written.
- [ ] All existing tests pass (`cargo test --lib`, `--test programs`, `--test interface_values`, `--test fmt_corpus`)
- [ ] New tests written and passing
- [ ] `cargo clippy --all-targets -- -D warnings` passes
- [ ] `cargo fmt --check` passes
- [ ] Every `pub` item has a doc comment
- [ ] Spec, Draft and CHANGELOG updated (decision 267); no file over 500 lines

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test --lib`, `cargo test --test programs`, `cargo test --test fmt_corpus`
- `cargo run -q -- run tests/programs/m122/concat.la`
