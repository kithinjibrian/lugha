## FEATURE: Owning arrays once per loop — writes inside a loop skip the copy-on-write flag test when the loop can't share their arrays; milestone 56.

**Status:** approved 2026-10-15 — session 119 ("build in one go"); proposed in session 118.
**Milestone:** 56 (one PRP; tag `m56` at merge)
**Spec:** `Language v15 Draft.md` (decisions 25–28, the milestone 56 choices); spec §7 (copy-on-write), §9 (lowering), §11 (row 56), §12 ("Cost of array copies").

**Decisions:**
- builds on **DECISION-012** / MEMORY 106–107 (copy-on-write, milestone 55);
- no open entries in DECISIONS.md apply.

## OBJECTIVE
A loop that writes into arrays it can't share pays the copy-on-write test once, before the loop, instead of on every write. DECISION-010's histogram and matmul get back near their pre-copy-on-write times, and matmul's inner loop can vectorize again. No program's output changes.

## CONTEXT

- **Starting state (M55):**
  - every write walks its place from the root and calls `own` on each array on the path (`codegen/places.rs::walk`, `codegen/cow.rs::own`): load `cap`, test the sign bit, branch;
  - `for … of mut` aliases re-reach their element through `Lowerer.aliases`.
- **Ending state:** decisions 25–28 implemented.
- **Measured cost to remove:** histogram 140 → 217 ms, matmul 143 → 188 ms (best of 5, `-O2`).
- **Related existing code** (`src/codegen/` unless noted):
  - **Writes:** `places.rs` (`place_plan`, `place_commit`, `walk`, `local_address`), `cow.rs` (`own`), `growth.rs`, `stmt.rs`.
  - **Loops:** `control.rs` (`while_loop`, `for_range`), `array.rs` (`count_loop`, `for_of`), `iterators.rs` (`iterator_loop`), `conditional.rs` (`while_let`).
  - **State:** `lower.rs` (`Lowerer`).
  - **Types:** the checker's side table (architecture rule 4); `Type::is_deep` tells whether a value holds arrays.
- **Open decisions that must be resolved first:** none.

### Design
1. **Analysis** (new `codegen/hoist.rs`), run on a loop's AST before lowering it:
   - **Candidate paths:** every written place in the loop (condition, header, body, nested loops), cut before its last index or field: for `out[i][j] += …` the arrays `out` and `out[i]`; for `counts[c] += 1`, `counts`; for `self.grads[g][k] = …`, `self.grads` and `self.grads[g]`. `push`/`pop`/`clear` on `p` add `p` itself.
   - **Fixed paths:** a candidate whose every index is a literal or a variable that the loop never assigns or redeclares (`let`, patterns, loop variables), with no calls.
   - **Root whitelist (decision 26):** every mention of the root in the loop is either the root of a written place, or a read whose type holds no array (`!is_deep`), such as `xs[i]` for `xs: f64[]`, `g[i][j]`, `xs.len` and `p.x`. A `for … of mut` alias of the root counts as the root. Any other mention drops every path of that root.
   - The analysis is conservative and only ever drops hoists. A wrong answer would break value semantics, so it is the part that gets the most tests.
2. **Hoisting:** just before the loop's first block, for each fixed path (shortest first), walk the path with `own`. Index steps are guarded: if the index is out of bounds, the remaining hoists of that path are skipped, with no panic.
3. **Inside the loop:** `Lowerer.owned` holds the hoisted paths (root slot plus the path's AST shape) while the loop is lowered. `walk(own = true)` skips `own` for an array whose path is in the set, and still loads it from its slot, so `push` growth stays correct. Nested loops push and pop their own entries.
4. **Unchanged:**
   - evaluation order, bounds checks and panic locations (writes still bounds-check every level);
   - the copy sites;
   - `own` outside loops.

### Risk
The danger is a hoist when the loop can in fact share the array: a later write would then go into a shared array without cloning it, so two values would change together silently. Mitigations:
- the whitelist is syntactic and narrow; anything unlisted turns hoisting off;
- every exclusion in decision 26 gets a hazard program that copies inside the loop and then writes, with its output checked against the M55 compiler (`~/.cargo/bin/lughac`, eager-equivalent);
- the full suite, `m55/` included.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** everything above.
- **Spec v15:**
  - §7 copy-on-write paragraph: one sentence saying a loop may make arrays unique once before it runs;
  - §9: a lowering note (fixed paths, the whitelist, guarded hoists);
  - §11: row 56;
  - §12: "Cost of array copies" updated with the new timings;
  - the changes summary.
- **Docs:** CLAUDE.md (tree: `codegen/hoist.rs`; STACK line), README, TODO (mark "Own an array once per loop" done), CHANGELOG, MEMORY decisions 108 (plan) and 109 (built), CONTEXT. The tag `m56` is added at the merge.
- **Timings:** DECISION-010's benchmarks (best of 5), firefly `./mlp data/names-32k.txt` and `./gpt data/names-32k.txt 1000 32 4 0.1`, before (M55) and after, recorded in this PRP and the CHANGELOG.

### Must NOT Do
- **No change to any program's output, exit code or panic message and location.**
- **No interprocedural analysis:** a call that mentions the root always turns hoisting off for it.
- **No change to copy sites,** to `own` outside loops, or to strings.
- **No new syntax and no new diagnostics.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **No new diagnostics.**
- **Runtime:** a hoist that clones allocates through `lugha_rt_alloc`, as `own` does. An out-of-bounds hoist index skips the hoist and never panics.
- **Codegen invariants** (a path in `owned` must be a place) use `expect` with the invariant stated.

## SECURITY CONSIDERATIONS
- **No `unsafe`, no new dependencies, no external input.**
- **Memory safety:** a wrong hoist would break value semantics, not memory safety. Every write still bounds-checks every level, and arrays still grow through `lugha_rt_array_grow`.

## TESTS TO WRITE

Unit (codegen, `hoist.rs` and IR):
- [ ] `counts[c] += 1` in a loop: `counts` is owned before the loop, and no `cow.clone` block is inside the loop.
- [ ] matmul's `out[i][j] +=`: the `i` loop hoists `out`, the `j` loop hoists `out[i]`.
- [ ] Each exclusion keeps the per-write test: `let c = xs;`, `f(xs)`, `xs.m()`, a closure capturing `xs`, `swap(xs, ys)`, `xs = ys`, `g[i] = row`, `let row = g[i];`, `for x of xs`, an alias `x` copied.
- [ ] A variable index that the loop assigns, or that a `let` redeclares, is not fixed; an index that contains a call is not fixed.
- [ ] Reads with no arrays (`xs[i]`, `xs.len`, `p.x`) don't block hoisting.

Acceptance (`tests/programs/m56/`):
- [ ] **`hoisting.la`:** histogram- and matmul-shaped loops over arrays that were copied before the loop; both copies printed after.
- [ ] **`hazards.la`:** one case per exclusion, each copying inside the loop and then writing, with both sides printed. Expected output from the M55 compiler.
- [ ] **`bounds.la`:** a hoisted `out[i]` with `i` out of bounds in a loop that runs zero times prints normally; the same in a loop that runs panics at the write (`.stderr` and `.exit` goldens as M55 produces).

Regression:
- [ ] Every earlier program passes with unchanged output.

## ROLLBACK PLAN

- Branch `prp-085-loop_ownership`, merged into `main` after CI passes; tag `m56`.
- To abandon: delete the branch. M55 behaviour is unaffected.

## ACCEPTANCE CRITERIA
- [ ] All tests pass at `-O0`, and the `m56/` programs at `-O2` too.
- [ ] Timings recorded.
- [ ] The spec is at milestone 56; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m56/hazards.la
```
