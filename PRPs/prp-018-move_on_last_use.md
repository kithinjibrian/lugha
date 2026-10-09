## FEATURE: Move on last use — store a dead owned local's array without copying it.

**Status:** approved 2026-10-09 — session 23
**Milestone:** none; post-v0 optimization (TODO "Ideas", DECISION-010 follow-up)
**Spec:** §4 (array copies table, value semantics), §5 (evaluation order: places before values, left to right)
**Decisions:** DECISION-010 / MEMORY 20 (eager copies stay; move on last use noted), MEMORY 14 (codegen reads the checker's tables), MEMORY 16 (copy sites)

## OBJECTIVE
Copies that no program could ever observe disappear: storing a value read from a `let` local that is never mentioned again moves it instead of deep-copying it. `grid = next;` in a double-buffered loop and `ps[i] = p;` after editing a record copy become free. Every program prints exactly what it printed before. Only fewer bytes get copied.

## CONTEXT

- Starting state:
  - `codegen/copy.rs::value_for_store` deep-copies any array-holding value read from a place.
  - `return`/tail already move from owned locals (spec §4).
  - DECISION-010 measured `life` at 40 400 copies and `particles` at 8 004 000.
- Ending state:
  - New `check/liveness.rs` marks last uses, and `Checked` gains `moves: HashSet<ExprId>`.
  - Codegen skips the copy for marked expressions.
  - The spec §4 note is added. New `m5/` programs pin that semantics are unchanged.
- Related existing code: `src/check/{mod,call,stmt,array,structs,assign}.rs`, `src/codegen/{copy,lower,stmt,array,structs}.rs`, `docs/decisions/decision-010*`.
- Open decisions that must be resolved first: none.

### Discovery answers (session 23)
1. **Rule: dead owned local.** A store site's value moves when it is a plain place (`p`, `w.data`, `g[0]`) rooted in a `let` local and both of these hold:
   - no other mention of that local follows it in the local's scope;
   - no loop encloses the store site without also enclosing the local's declaration (the next iteration could reach it again).

   Parameters and `for … of` variables are never moved from.
2. **The checker records it.** It already resolves every name to its binding, including shadowing. `Checked.moves: HashSet<ExprId>`; codegen only asks `moves.contains(id)` (rule 4).
3. **Spec note:** one sentence under the §4 copy table, approved with this PRP.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Binding resolution record** — `check/call.rs`, `check/mod.rs`
- When `name()` resolves a `Name` to a local, the checker records `ExprId → binding`. The binding is identified by its declaration span (`Local.span`, unique per binding) plus its `Binding` kind.

**Liveness** — new `check/liveness.rs`, run once per function after its body is checked
- Walk the body in source order, keeping a stack of enclosing loop spans (`while`, `for … in`, `for … of`).
- **Store sites** (the spec §4 copy sites, minus `return`/tail, which already move):
  - a `let` initializer;
  - the value of an assignment (`=` to a name, field or element);
  - a list-literal element;
  - a struct-literal field value.

  Repeat literals are excluded: their value fills many elements, so copies are needed anyway.
- **When a value moves:** a store-site value `E` that is a place chain (`Name`, `.f`, `[i]`) rooted in a `Name` bound to a `Let` local `b` is a move iff:
  - no recorded mention of `b` starts after `E` ends (source order is evaluation order, spec §5: places are evaluated before values, and operands left to right);
  - every loop on the stack at `E` also contains `b`'s declaration span.
- Index expressions inside the chain (`g[i]`) are themselves mentions of their own names. They don't affect `b`.
- Values that are not plain places (an `if`/block tail, a call, a literal) are never marked. They are fresh, or still copied as today.
- Mark `E.id` in `moves`. Types are not consulted: codegen only looks at `moves` where it would otherwise copy.

**Codegen** — `codegen/copy.rs`, `codegen/lower.rs`
- `Lowerer` keeps `moves` from `Checked`.
- `value_for_store` skips `deep_copy` when `moves.contains(&expr.id)`.
- Nothing else changes: call arguments, fresh values, `return`/tail and repeat fill behave as before.

**Spec** — §4, below the copy table and its tail paragraph:
- "When the value comes from a local that is never used again, the compiler may move it instead of copying; no program can tell the difference."
- §12 is unchanged. `docs/decisions/decision-010.md` gains an "After PRP-018" note with the new copy counts for `life` and `particles`.

**Docs**
- CLAUDE.md tree: `check/liveness.rs`.
- MEMORY: a new decision (move on last use), and decision 16's copy-site description updated.
- TODO: tick the idea.
- CHANGELOG, CONTEXT.

### Must NOT Do
- No flow-sensitive dataflow analysis (discovery answer 1). Mentions on exclusive branches still count, which is conservative.
- No moves from parameters, `for … of` variables or repeat values; no change to call arguments.
- No new runtime support, reference counts or `unsafe`; no new dependencies.
- No change to what any program prints.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- Pure analysis; no new diagnostics. The analysis can only *skip* copies, and it errs on the side of copying: any doubt (a later mention, an enclosing loop, a non-place value) means "copy".

## SECURITY CONSIDERATIONS

- A wrong move would make two places share an array, which is a correctness bug, not memory-unsafe. Boehm keeps shared objects alive. The tests below target the cases where a wrong move would change output.
- No `unsafe`.

## TESTS TO WRITE

Unit tests — `check/liveness.rs` (via the checker's `ok()`, asserting which source spans are in `moves`):
- [ ] `let ys = xs;` with `xs` unused afterwards → moved; with a later `xs[0]` read, or a later write `xs[0] = 1` → not moved.
- [ ] `grid = next;` at the end of a loop body where `next` is declared in that body → moved.
- [ ] A store inside a loop from a local declared outside the loop → not moved, even with no later mention.
- [ ] Shadowing: `let a = [1]; let b = a; let a = [2]; println(a[0]);` → the first `a` moves (the later `a` is another binding).
- [ ] `ps[i] = p;` with `p` dead → moved; `let mut p = ps[i];` → not moved (`ps` is used again).
- [ ] Parameters and `for … of` variables → never moved; a field place of a dead local (`let d = w.data;`) → moved.
- [ ] A list-literal element and a struct-literal field from dead locals → moved; the same local used twice in one literal (`[a, a]`) → the first not moved, the last moved.

Codegen IR tests — `codegen/copy.rs`:
- [ ] `let ys = xs;` with `xs` dead → no `llvm.memcpy` in that function; with `xs` used later → `llvm.memcpy`.

Acceptance — `tests/programs/m5/` (also at `-O2`):
- [ ] `moves.la`: programs where a *wrong* move would change the output:
  - a loop copying from an outer local and then mutating the copy;
  - a later read after the store;
  - `[a, a]` with one element mutated;
  - shadowing;
  - double buffering whose result is checked.

  Expected stdout is written by hand from value semantics.
- [ ] All earlier tests pass unchanged; `value_semantics.la` and `struct_copies.la` still print their originals.

Measurement:
- [ ] Rerun the DECISION-010 `count` build for `life` and `particles` and record the copy counts before and after in the decision doc.

## ROLLBACK PLAN

- Branch `prp-018-move_on_last_use`, merged into `main` after green CI.
- To abandon: delete the branch. To disable after merging, make `moves` always empty; the copies return.

## ACCEPTANCE CRITERIA
- [ ] Every test above exists and passes; outputs are unchanged.
- [ ] `life` and `particles` copy counts drop (recorded).
- [ ] Spec §4 note, CLAUDE.md, MEMORY.md, TODO.md, CHANGELOG.md updated.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- CI green on the branch
