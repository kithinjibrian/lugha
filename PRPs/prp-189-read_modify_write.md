## FEATURE: read-modify-write through `[ ]`: `m[k] += v`, `m[k].push(x)`, `g[i].x = v` (milestone 156)

## OBJECTIVE

```
struct Grid { w: i64, cells: f64[] }

impl Grid {
    fun index(self, at: (i64, i64)): f64 { let (r, c) = at; self.cells[r * self.w + c] }
    fun set_index(mut self, at: (i64, i64), v: f64) { let (r, c) = at; self.cells[r * self.w + c] = v; }
}

struct P { x: i64 }

fun main() {
    let mut counts: Map<string, i64> = #{};
    for w of ["a", "b", "a"] {
        if !counts.has(w) { counts[w] = 0; }
        counts[w] += 1;                    // in place: no copy of the entry
    }
    println(counts["a"]);                  // 2

    let mut groups: Map<string, i64[]> = #{"odd": [], "even": []};
    for n in 0..6 {
        groups[if n % 2 == 0 { "even" } else { "odd" }].push(n);   // in place
    }
    println(groups["even"]);               // [0, 2, 4]

    let mut g = Grid { w: 2, cells: [0.0; 4] };
    g[1, 1] += 2.5;                        // g.set_index((1, 1), g.index((1, 1)) + 2.5)
    println(g[1, 1]);                      // 2.5

    let mut ps: Map<i64, P> = #{1: P { x: 1 }};
    ps[1].x = 7;                           // in place
    println(ps[1].x);                      // 7
    counts["z"] += 1;                      // panics: no key "z" in the map, as a read does
}
```

Today every line that changes an entry or element is E0404 or E0502 and needs `let mut e = m[k]; …; m[k] = e;`. After this, a map entry is a place, and an element a type reads through `index` is changed by reading it, changing the copy and writing it back through `set_index`, all in one expression.

## CONTEXT

- Read first: `src/check/indexing.rs` (`index_assign`, `read_by_index`, `copy_help`, `compound_help`), `src/check/maps.rs` (`map_assign`), `src/check/assign.rs` (`check_place_for`), `src/check/for_mut.rs` (`loop_place`), `src/prelude/maps.la` (`$map_find`, `$vals`), `src/codegen/places.rs` (where today's crash was), `src/codegen/stmt.rs` (assignment through `set_index`) and `src/codegen/operators.rs` (compound assignment).
- Spec: §4 Operator overloading's Indexing bullet and §4 Maps (both say E0404 / E0502 today), §5 places and `swap`, §8 lowering, the §12 rows for E0404 and E0502, §11 row 156, "Changes since v16".
- DECISIONS: 187 (maps change themselves through `mut self`; copies of arrays cost a copy-on-write), 213 (moves on last use), 100–101 and m155's indexing decision are followed. Nothing open blocks this.

## DECISIONS MADE IN DISCOVERY (for review)

1. **A map entry is a real place.** `m[k]` as a place finds the entry's slot (`$map_find`; a missing key panics `no key "k" in the map` at the `[`, exactly as a read does) and then is `m.$vals[slot]`. So `m[k] += v`, `m[k].x = v`, `m[k][i] = v`, `m[k].push(x)`, `swap(m[a], m[b])`, `for x of mut m[k]` and `out m[k]` all work in place, under the rules any element follows (E0501, E0507 when the loop changes `m`, E0508, E0509). **Why:** a copy-out/write-back would copy the entry's arrays on every change (copy-on-write sees two owners), making the common `groups[k].push(x)` quadratic. Same results either way; in place is just faster.
2. **A missing key panics, it doesn't insert.** `counts[w] += 1` on a new word panics, consistent with `counts[w]` as a read. Lugha has no default value for a `V`, so "insert a zero" has nothing to insert. A later `m.entry(k, default)` or similar is its own decision.
3. **A user type's element is read, changed and written back.** `a[i] op= v` is `a.set_index(i, a.index(i) op v)`; `a[i].x = v`, `a[i][j] = v` and `a[i].m(…)` (a `mut self` method; its result is kept) are `let mut e = a.index(i); e.… ; a.set_index(i, e);`. Chains nest in either order: `m[k][i].x = v` (a map of grids) and `g[i][j] = v` (a grid of grids) each split at every user-index link. **Why:** values are copies in Lugha, so there's no reference `index` could hand out; Swift's subscripts work the same way.
4. **Evaluation order** follows `xs[i] op= v` and m155's `a[i] = v`: the place's root and indexes left to right, each once, then `v`, then `index`, the change, then `set_index`. `i` is evaluated once even though it's used twice.
5. **A type with `index` but no fitting `set_index`** gets E0411 on any read-modify-write ("cannot assign by index into a value of type `T`", the same message as for `a[i] = v`), with help naming the `set_index` shape. `a[i] op= v` also needs `op` on the element's type (E0404 as for any operand).
6. **What still is E0502 for user types:** `for x of mut a[i]`, `swap(a[i], …)` and `out a[i]`. A loop body can `break`, `return` or `?` out, and writing back on every exit is a new mechanism; `swap` and `out` are rare on computed elements. Their help stays the copy-out one. Map entries don't have this limit (decision 1).
7. **No new diagnostic codes.** E0404 and E0502 stop applying to these forms; their `docs/errors/` pages and the §12 rows lose those examples, and the indexing E0502 help remains for decision 6's forms.
8. **The mutable-place rules apply to the root:** `m[k] += 1` needs `let mut m` (E0501), as `m[k] = v` does.

## REQUIREMENTS

### Must Do
- Map entries as places, in place, for every place use (decision 1).
- Read-modify-write through `index`/`set_index` for compound assignment, writes inside the element and `mut self` calls (decision 3), nested to any depth and mixed with map entries.
- Update spec §4 (Indexing, Maps), §5, §8, §12, §11 row 156 and "Changes since v16"; the E0404 and E0502 pages.
- Programs that don't use these forms produce byte-identical IR.

### Must NOT Do
- No default-inserting entries, no `entry` API (decision 2).
- No write-back on early loop exit: `for … of mut`, `swap` and `out` through a user `index` stay E0502 (decision 6).
- No change to how a plain read `m[k]` or `a[i]` behaves.

## DESIGN

- Checker: `read_by_index` stops treating map entries as copies; a place through a map index is checked like an array element. A read-modify-write through `index` is recorded per place in the checked side table (which links are `index`/`set_index` calls) so codegen doesn't infer it.
- Codegen: a map entry place is `$map_find` + a panic on a missing key + the `$vals` element's address (a hidden prelude method, e.g. `$slot(k): i64`, so the lookup stays Lugha). A user-index link lowers to `index`, the change on a temporary, then `set_index` on the outer place.
- Security: none. No external input, no `unsafe`, no new dependency.

## TESTS

- [ ] `tests/programs/collections/maps_and_sets/entry_writes.la` becomes a passing program (its `.stderr` replaced by `.stdout`): `m[k].x = 5`, `n[k][0] = 3`, `n[k].push(4)`, `swap(n[a], n[b])`, `for x of mut n[k]`, `m[k] += 1`; `entry_missing.la` panics with `no key` and exit 101.
- [ ] `tests/programs/interfaces/indexing/rmw.la`: `g[i] += v` on a grid, `g[i].x = v`, `g[i][j] = v` (grid of grids), `g[i].push(x)` keeping the result of `pop`, a map of grids `m[k][i] += 1`, an index with a side effect evaluated once.
- [ ] `writes.la`/`.stderr` shrink to the remaining rejections: E0501, E0411 for a type without `set_index`, E0502 for `for x of mut g[i]`, `swap(g[i], …)`.
- [ ] Many-inserts style regression: 100 000 `groups[k].push(x)` within the runner's time limit.
- [ ] Unit tests in `src/check/indexing.rs` and `maps.rs` for which forms are accepted.

## ACCEPTANCE
- [ ] The OBJECTIVE program prints `2`, `[0, 2, 4]`, `2.5`, `7`, then panics at `counts["z"]`.
- [ ] `cargo test --lib`, `--test programs`, `--test explain` pass; `cargo fmt --check` and clippy are clean.
- [ ] Spec, Draft, CHANGELOG, TODO updated; tag `m156`.

## BUILT (deviations and notes)

- **Evaluation order** differs from decision 4: the place's indexes are evaluated once, root outward, then `index`, then the change (which evaluates `v`), then `set_index`, the order `let mut e = a[i]; e.x = v; a[i] = e;` reads and Swift's subscripts use. Map entries keep the array order (indexes, slot lookup, then `v`).
- **Ids:** the parser leaves three ids after every `[ ]` (`Expr::write_id`, `element_id`, `update_id`) for `set_index` or a map's `$slot`, the copy handed back, and a compound operator's method. The checker records `Checked::through_index` (places written through `index`); codegen holds indexes and the copy in hidden locals with fresh ids.
- **IR:** generated code is the same for programs without these writes, compared across every `tests/programs` case; the textual IR differs in value names that carry expression ids and in prelude line numbers in panic locations (`$slot` was added to the prelude), so it isn't byte-identical.
- With `[ ]`s through `index` nested (`g[i][j] = v`), handing back reads the outer element a second time.
- `swap`'s E0502 keeps the copy-out help instead of replacing it; `writes.la` became the rejection set (E0501, E0508, E0502 for `swap`/`for … of mut`, E0411 without `set_index`); the new cases are `tests/programs/interfaces/read_modify_write/` and `maps_and_sets/entry_writes`, `entry_missing`, `entry_pushes`.
