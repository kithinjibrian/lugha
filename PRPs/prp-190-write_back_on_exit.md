## FEATURE: `for x of mut g[i]`, `swap(g[i], …)` and `out g[i]` through a type's `index` (milestone 157)

## OBJECTIVE

```
struct Rows { rows: i64[][] }

impl Rows {
    fun index(self, i: i64): i64[] = self.rows[i];
    fun set_index(mut self, i: i64, v: i64[]) { self.rows[i] = v; }
}

fun main() {
    let mut r = Rows { rows: [[1, 2, 3], [9]] };
    for x of mut r[0] {
        x *= 10;
        if x == 20 { break; }          // the copy is still handed back
    }
    println(r.rows);                   // [[10, 20, 3], [9]]

    let mut spare = [7];
    swap(r[1], spare);
    println((r.rows, spare));          // ([[10, 20, 3], [7]], [9])
}
```

After milestone 156, these three are the only writes through `[ ]` on a type that are still E0502. Each one gets the same read, change and hand-back as `g[i].x = v`. For the loop, the copy is handed back on every way out of it.

## CONTEXT

- Read first: `src/check/assign.rs` (`check_place_for`, whose `Swap` arm keeps E0502), `src/check/for_mut.rs` (`loop_place`), `src/check/out_params.rs` (`out_argument`), `src/check/read_modify_write.rs` (`write_through`), `src/codegen/through_index.rs`, `src/codegen/swap.rs`, `src/codegen/out_params.rs`, the `for … of mut` lowering, and `src/codegen/defer.rs` (how pending `defer`s run on `break`, `continue`, `return` and `?`).
- Spec: §4 Indexing bullet (its "`swap` and `for … of mut` … are E0502" sentence), §5 `for … of mut` and `swap`, §8 `out` parameters and lowering, §12 E0502 row, §11 row 157, "Changes since v16".
- DECISIONS: milestone 151 (`defer`: runs at every exit of its block but a panic) and 156's write-back are followed. Nothing open blocks this.

## DECISIONS MADE IN DISCOVERY (for review)

1. **The loop hands the copy back on every way out, as a `defer` would:** `for x of mut g[i] { … }` is `{ let mut e = g.index(i); defer g.set_index(i, e); for x of mut e { … } }`. So the loop's end, `break`, `continue` to an outer loop, `return` and `?` all write back, and a panic doesn't, the same as `defer`. **Why:** that's the existing rule for "run this when the block is left", and with it a `return` from inside the loop never loses the changes made so far.
2. **`swap(g[i], b)` and `out g[i]` read, swap or fill the copy, then hand it back**, right after the call, as 156's writes do. `swap(g[i], g[j])` copies both, swaps them, then hands back `g[i]`'s copy first and `g[j]`'s second, left to right like the arguments. With `i == j` the result is unchanged, as for arrays.
3. **Rules inside the loop stay those of `for … of mut` over an array:** changing `g` itself in the body is E0507, as it is for `for x of mut xs` with `xs.push(…)` in the body. **Why:** the body changes the copy, and a change to `g` would be overwritten by the hand-back at the loop's end.
4. **Nesting works as in 156:** `for x of mut g[i][j]` and `for x of mut m[k][i]` (a map of such types) hand back at each `[ ]` through `index`, innermost first. A map entry is still changed in place, as now.
5. **No new diagnostic codes.** The E0502 for these forms goes away; a type without a fitting `set_index` gets 156's E0411 ("cannot change an element of `T`: it has no `set_index`").

## REQUIREMENTS

### Must Do
- `for … of mut`, `swap` and `out` through `index`, with decisions 1–4.
- Update spec §4 (Indexing), §5, §8, the E0502 row and page, §11 row 157 and "Changes since v16". Update the Draft, CHANGELOG and TODO.
- Generated code is unchanged for programs that don't use these forms.

### Must NOT Do
- No write-back on a panic (`defer` doesn't run then either).
- No change to `for … of mut` over arrays, map entries or iterators.

## DESIGN

- Checker: `check_place_for`'s `Swap` arm and `loop_place` call `write_through`, as assignments do; `out_argument` follows `check_place_for`, so it comes along.
- Codegen: `swap` and `out` wrap their place arguments with `through_index`. `for … of mut` over an element read through `index` holds the copy in a hidden local, registers its `set_index` as a pending exit action of the loop's block (the mechanism `defer` uses), then loops over the copy as over any array.
- Security: none.

## TESTS

- [ ] `tests/programs/interfaces/read_modify_write/loops.la`: the OBJECTIVE, plus `return` and `?` from inside the loop (the hand-back has happened when the caller looks), `continue outer`, a nested `g[i][j]`, `swap(g[0], g[1])`, and `out g[i]` with a small extern if one is at hand among the existing test externs.
- [ ] `writes.la`/`.stderr`: drop the two E0502s and add E0507 for changing `g` inside `for x of mut g[i]`.
- [ ] Unit tests in `src/check/read_modify_write.rs`: the three forms accepted, E0411 without `set_index`, E0507 in the body.

## ACCEPTANCE
- [ ] The OBJECTIVE prints `[[10, 20, 3], [9]]` and `([[10, 20, 3], [7]], [9])`.
- [ ] `cargo test --lib`, `--test programs` and `--test explain` pass; fmt and clippy are clean.
- [ ] Spec, Draft, CHANGELOG and TODO are updated; tag `m157`.

## BUILT (deviations and notes)

- `swap(g[i], g[j])` hands back `g[j]`'s copy first, then `g[i]`'s (the wrapping nests left to right, so the inner one finishes first), not `g[i]`'s first as decision 2 said. The result is the same, including `i == j`; only the order of the two `set_index` calls differs.
- The loop's hand-back is an ordinary deferred statement: codegen wraps the loop in a block holding `defer g.set_index(i, e);`, so it reuses `defer`'s exits as they are.
- `out` and `mut` arguments wrap the whole extern call. They passed the checker since milestone 156 and crashed in codegen; `tests/out_params.rs` covers them now. The swap and loop E0502 helps are gone with them.
- `for x of mut g[i]` guards all of `g` (E0507), not just the element's path.
