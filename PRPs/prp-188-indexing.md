## FEATURE: `a[i]` and `a[i] = v` on your own types, through `index` and `set_index` methods (milestone 155)

## OBJECTIVE

```
struct Grid { w: i64, cells: f64[] }

impl Grid {
    fun index(self, at: (i64, i64)): f64 {
        let (r, c) = at;
        self.cells[r * self.w + c]
    }
    fun set_index(mut self, at: (i64, i64), v: f64) {
        let (r, c) = at;
        self.cells[r * self.w + c] = v;
    }
}

interface Table { fun index(self, k: string): i64; }

fun first<T: Table>(t: T): i64 = t["a"];

fun main() {
    let mut g = Grid { w: 3, cells: [0.0; 9] };
    g[1, 2] = 5.0;                  // g.set_index((1, 2), 5.0)
    println(g[1, 2]);               // 5.0   — g.index((1, 2))
    g[0, 0] += 1.0;                 // E0404, as for a map entry

    let mut d: Map<(i64, i64), string> = #{};
    d[0, 0] = "origin";             // `a[x, y]` is `a[(x, y)]` for every type
    println(d[0, 0]);               // origin
}
```

The same overloading rule that `+` (`add`) and `<` (`cmp`) already use: the operator calls a method that has the right shape, and nothing has to be declared. A comma inside `[ ]` builds a tuple, so there is one way to write a multi-index method and one way to call it.

## CONTEXT

- Read first: `src/parser/expr.rs` (postfix `[`, line ~102), `src/parser/tuples.rs`, `src/formatter/exprs.rs` (`Index`), `src/check/access.rs` (`index`; `m[k]` already lowers to a method call, `$entry`), `src/check/maps.rs` (`map_assign`: `m[k] = v` is `insert`), `src/check/operators.rs` (`operator_call`, `operator_method`: shape checks for struct, enum, interface and `T` operands), and the codegen for `$entry`/`insert` and `codegen/operators.rs`.
- Creates `src/check/indexing.rs` (finding the method by its shape, the read path and the write path) and `tests/programs/interfaces/indexing/`. Changes `access.rs`, `maps.rs` (or `assign.rs`) so that a user-type base goes to the new path, plus the matching codegen.
- Spec: §4 Operator overloading (lines 878–883). That section's "Unchanged" line is out of date: `<` has called `cmp` since milestone 59 and `==` has called `eq` since milestone 144. It gets fixed as part of this work. Also §4 Maps (`m[k]`, `m[k] = v`, `m[k] += v` is E0404), §5 places (E0501, E0502, E0508), §8 lowering of overloaded operators (line 1567), and the §12 rows for E0411, E0404 and E0502.
- Draft: on approval this gets a new section, "Decisions behind milestone 155".
- DECISIONS: 100–101 (operator overloading), 114–115 (`cmp`) and 144 (`eq`) are followed. None of the decisions it depends on are open.

## DECISIONS MADE IN DISCOVERY (for review)

These are judgement calls, recorded here so you can review them instead of answering them one by one:

1. **Names: `index` and `set_index`, not `get` and `set`.** `Map.get(k)` returns `Option<V>` and `Ref.get()` takes no argument. Because the rule is "the right shape is enough", any type that copies those patterns would become indexable by accident.
2. **Read shape: `fun index(self, i: I): T`.** It needs a plain `self`, exactly one parameter of any type and a result. Write shape: `fun set_index(mut self, i: I, v: T)`, with two parameters and no result. Any other shape doesn't make the operator, as with `add`. The two methods are independent of each other, like `add` and `sub`.
3. **`a[x, y]` means `a[(x, y)]`, for every type** (your choice in discovery: only one way to do it). The parser builds an `ExprKind::Tuple` spanning the elements, so `Index` still holds one index and no later stage changes for it. `index` and `set_index` always take exactly one index parameter; several indexes mean a tuple parameter, unpacked with `let (r, c) = at;`. Maps with tuple keys get `m[1, 2]` too. A trailing comma (`a[x,]`) is the existing parse error, since Lugha has no 1-tuples. The formatter writes a tuple index without its parentheses: `m[(1, 2)]` becomes `m[1, 2]`. An array or string given a tuple index is E0403, as today.
4. **Writes behave exactly like maps' writes today.** `g[i] = v` is allowed. `g[i] += v` is E0404, with the same help as for maps. Writing inside an element (`g[i].x = v`, `g[i][j] = v`, `g[i].push(x)`) is E0502, with help: `let mut row = g[i]; row[j] = v; g[i] = row;`. A read-modify-write version (Swift's subscripts) should cover maps and user types together, as its own PRP later.
5. **`g[i] = v` is a `mut self` call.** It needs a `let mut` place (E0501), it follows E0508/E0510 like any `mut self` call, and it evaluates the base's place, then `i`, then `v`, as `m[k] = v` does.
6. **Interface values and `T` are covered**, wherever the interface or a bound declares the method, as with arithmetic operators.
7. **Diagnostics reuse existing codes.** Reading from a type without a fitting `index` is E0411 ("not indexable"). Writing to a type without a fitting `set_index` is also E0411. In both cases the help names the shape, and also points out a method with that name but the wrong shape. A wrong index type is E0403, and a literal index takes the parameter's type.
8. **Arrays, strings and maps keep their built-in indexing** and gain no `index` method, the same way numbers have no `add`. There is no prelude `Index` bound; a program can declare its own interface (see the OBJECTIVE). This keeps the scope small; a bound can be added later if a generic function needs to index both arrays and user types.

## REQUIREMENTS

### Must Do
- `a[x, y, …]` parses as `a[(x, y, …)]`; the formatter prints a tuple index without parentheses.
- `a[i]` on a struct, enum, interface value or bounded `T` calls `index`; `a[i] = v` calls `set_index`, with the shapes and rules listed above.
- Fix spec §4's out-of-date "Unchanged" line, and update §8 lowering, the §12 E0411 and E0502 rows, the §11 row 155 and "Changes since v16".
- Programs that don't index user types produce byte-identical IR.

### Must NOT Do
- No `get`/`set` lookup, no `index` with several parameters (the comma builds a tuple), no read-modify-write (`+=`, nested writes), and no prelude `Index` bound. Each of these is a separate decision, listed above.
- No change to how arrays, strings or maps index.

## DESIGN

- No new diagnostic codes. The new messages and help for E0411, E0404 and E0502 are added to their `docs/errors/` pages.
- Lowering: `a[i]` becomes `lugha_fn_<Type>.index(self, i)`, called the same way any `self` method is, or through the interface's table for an interface value. `a[i] = v` becomes the `mut self` call `set_index`, reached through the place. No new runtime, ABI or symbol kind.
- Security: none. There's no external input, no `unsafe` and no new dependency.

## TESTS

As built: `indexing.la` holds every happy path (grid, literal `u8` index, enum, `T: Table`, interface value, loop writes, map tuple keys); `e0411.la`, `writes.la` (E0501, E0404, E0502, E0508) and `e0403.la` hold the rejections. The list below is the plan.

- [ ] Parser unit tests: `a[1, 2]` gives `(index a (tuple 1 2))`; `a[1,]` is the existing parse error. Formatter tests: `m[(1, 2)]` prints as `m[1, 2]` and stays stable.
- [ ] `tests/programs/interfaces/indexing/`:
  - `grid.la`: read and write with `g[1, 2]`.
  - `tuple_keys.la`: `d[0, 0]` on a `Map<(i64, i64), string>`.
  - `array_tuple.la`: `xs[1, 2]` is E0403.
  - `generic.la`: `T: Table` and an interface value.
  - `literal_index.la`: a `u8` index parameter with `g[3]`.
  - `enum.la`: `index` on an enum.
- [ ] Rejected programs in the same folder, each with a `.stderr`:
  - `no_index.la` (E0411), and `wrong_shape.la` (E0411 with the shape help, for `index(mut self, …)`).
  - `read_only.la` (E0411 on write).
  - `compound.la` (E0404).
  - `nested_write.la` (E0502).
  - `immutable.la` (E0501).
  - `bad_index.la` (E0403).
  - `alias.la` (E0508).
- [ ] Unit tests in `src/check/indexing.rs`:
  - finding the methods by shape, including one with type parameters of its own, which never matches;
  - that arrays, strings and maps are unchanged.
- [ ] An IR test: an existing program with no user indexing gives the same IR as before.

## ACCEPTANCE
- [ ] Without the `+=` line, the OBJECTIVE program prints `5.0` and `origin`; with it, that line is reported as E0404.
- [ ] `cargo test --lib`, `--test programs` and `--test explain` pass. `cargo fmt --check` and `cargo clippy --all-targets -- -D warnings` are clean.
- [ ] Every `pub` item is documented. The spec, Draft, CHANGELOG and TODO are updated; tag `m155`.
