## FEATURE: Iterators — `Iter<T>` in the prelude and `for x of e` over anything with `next`; milestone 24.

**Status:** approved 2026-10-13 — session 66
**Milestone:** 24 (one PRP; tag `m24` at merge)
**Spec:** `Language v6 Draft.md`:
- decision 5 and the choices made while writing the draft ("`for … of` iterates a copy", "A module's own `Iter` hides the prelude's");
- §4 "Iterators";
- §5 (the desugaring);
- §9 "Iterator loops";
- §10 "Iterators";
- §11 (milestone 24).

**Decisions:** MEMORY 59–61 (v6, generic interfaces), MEMORY 18/20 (copy sites, move on last use), MEMORY 50 (prelude types like `Result`).

## OBJECTIVE
Anything with `fun next(mut self): Option<T>` can be looped over. The §10 iterators program prints `1`, `2`, `3`, `0`, `88`:

```
let c = Count { n: 0, end: 3 };
for n of c { println(n); }      // 1 2 3
println(c.n);                   // 0: the loop used a copy

fun total(it: Iter<i64>): i64 { let mut sum = 0; for n of it { sum += n; } sum }
total(Fib { a: 0, b: 1, left: 10 })   // 88
```

## CONTEXT

- **Starting state:**
  - `for x of e` needs an array; anything else is E0411.
  - Generic interfaces work as bounds and values (milestone 23).
  - `Result` is a prelude type that a module may shadow (`check/modules/lookup.rs`).
- **Ending state:**
  - The prelude declares `Iter<T>`, and iterator loops check and run.
  - The spec is at milestone 24.
- **Related existing code:**
  - `src/prelude.la`;
  - `src/check/array.rs` (`for_of`), `check/modules/lookup.rs` (`prelude_type`), `check/interfaces.rs` (`interface_key`), `check/iface_values.rs` (`interface_type`), `check/bound_infer.rs` (`offered`), `check/instances.rs` (`used`, `bound_used`), `check/mod.rs` (`Checked`);
  - `src/codegen/array.rs` (`for_of`), `codegen/interfaces.rs` (`method_of`), `codegen/dispatch.rs` (`dynamic_call`), `codegen/copy.rs` (`value_for_store`).
- **Open decisions that must be resolved first:** none.

### Discovery
- **No interview:** the approved v6 draft settles every behaviour of this milestone (decision 5 and the two choices on copies and shadowing). The design below only maps it onto the compiler.

### Design
- **The prelude** gains `interface Iter<T> { fun next(mut self): Option<T>; }`.
  - **Names:** `Iter` resolves to the prelude's in types and bounds, unless the module declares its own. That is `Result`'s rule (`prelude_type`), extended to interfaces.
  - **Programs that don't use it** get no new code, since interfaces compile nothing on their own.
- **Checking `for x of e`** (new file `check/iterators.rs`, called from `for_of` when `e` isn't an array):
  - **When `e` iterates:** its type offers `next` with a `mut self` receiver, no parameters, and the result `Option<X>`. That covers:
    - a struct or enum with its own method;
    - a type parameter whose bounds declare it (a default included);
    - an interface value whose interface declares it (`Iter<i64>`, or the program's own `Source<i64>`).

    This is structural satisfaction of the prelude's `Iter<X>`, so a module that hides `Iter` still loops.
  - **The loop variable:** `x: X`, an immutable `LoopVar`. The array guard (E0507) doesn't apply, because the loop works on a copy.
  - **Anything else** is E0411, with help: "a type iterates when it has `fun next(mut self): Option<T>`".
  - **What codegen needs:** `Checked.iterations` maps the `e` expression's id to `Iteration { element, default }`. `default` is the bound interface (key and arguments) when `e`'s type is a type parameter whose `next` comes from a bound with a default.
  - **Reachability:**
    - a generic struct's `next` at its arguments (`used`);
    - a type parameter's `next` through its bound (`bound_used`, which already reaches defaults);
    - an interface value needs nothing.
- **Codegen** (new file `codegen/iterators.rs`, called from `array.rs::for_of`):
  - **The hidden copy:** evaluate `e` once into a hidden mutable slot, `value_for_store`, which copies a place and moves a dead local.
  - **The loop:** each iteration calls `next` with the slot's address:
    - for a struct or enum, the method directly: its own, or a generic instance by its arguments, or the bound default's instance;
    - for an interface value, through its table, as `dynamic_call` does, with the box of the slot's value.
  - **Each result:** a `Some` binds its payload to `x` and runs the body; a `None` leaves the loop. `break` and `continue` work as in other loops (`continue` goes back to the call).
- **Not changed:** arrays keep their own loop, and programs without iterator loops give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Prelude:** `Iter<T>`, with `Iter` resolving to it unless a module declares its own.
- **Checker:**
  - `check/iterators.rs`;
  - `for_of` falls back to it;
  - `Checked.iterations`;
  - the E0411 help.
- **Codegen:** `codegen/iterators.rs`; `for_of` falls back to it.
- **Spec** (`Language v6 Specification.md`):
  - fold in §4 "Iterators" and §5 (the desugaring and the copy);
  - §9 iterator loops;
  - the §10 iterators program;
  - §11 milestone 24 done;
  - E0411's row gains the iterator case;
  - "Changes since v5" gains iterators.
- **Docs:** CLAUDE.md (tree, STACK), README, TODO, CHANGELOG, MEMORY decision 62, CONTEXT. The tag `m24` is added at the merge.

### Must NOT Do
- **No iterator helpers** (`map`, `filter`, `collect` on `Iter<T>`) and **no strings as iterators:** both are out of scope for v6.
- **No `for n of mut c`:** that is v6 §12's open question.
- **No `Self`, and no conversion between interfaces:** those are milestones 25–26.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0411** for a non-iterable value, now with the help above. There are no new codes.
- **A `next` with the wrong shape** (a parameter, no `mut self`, or a result that isn't an `Option`) makes the type not iterable: E0411, with help naming the expected signature.
- **Codegen invariants** are `expect`s: an iterated type has its `next` compiled.

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — checker (`check/iterators.rs`):
- [ ] **What iterates:**
  - `for n of Count {…}` binds `n: i64`;
  - a generic `Repeat<string>` binds `string`;
  - an `Iter<i64>` value binds `i64`;
  - a program's own `Source<string>` value binds `string`;
  - `I: Iter<T>` in generic code binds `T`.
- [ ] **E0411:**
  - an `i64`;
  - a struct without `next`;
  - a `next` without `mut self`, with a parameter, or returning `i64`;

  each with the help.
- [ ] **The loop variable** is immutable (E0501), and assigning to `c` inside `for n of c` is fine (no E0507).
- [ ] **Names:** a module's own `interface Iter { … }` hides the prelude's, and `Iter<i64>` resolves to the prelude's otherwise.

Unit tests — codegen (`codegen/iterators.rs`):
- [ ] A loop over `Count` calls `@lugha_fn_Count.next` with the slot's address, and a loop over an `Iter<i64>` value calls through a table slot.

Acceptance — `tests/programs/m24/`:
- [ ] `iterators.la`: the §10 program.
- [ ] `loops.la`:
  - `break` and `continue`;
  - a nested loop over two iterators;
  - a generic `Repeat<T>`;
  - a type parameter `I: Iter<T>`;
  - a program's own `Source<i64>` value;
  - the iterator unchanged after the loop, and an iterator in an array (`for it of its { for n of it {…} }`);
  - a module that declares its own `Iter`.
- [ ] `e0411.la`: reject golden.
- [ ] The §10 iterators program runs from the spec.

Regression:
- [ ] Every earlier test passes, and programs without iterator loops give byte-identical IR (compared with `main`).

## ROLLBACK PLAN

- Branch `prp-052-iterators`, merged into `main` after CI passes; tag `m24`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The §10 iterators program and the `m24/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [ ] The spec is at milestone 24; docs updated.
- [ ] No file over 300 lines: new code goes in new files, and any split of an existing file is proposed first. No new dependencies, no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m24/iterators.la
```
