## FEATURE: Array helpers — `map`, `filter`, `fold`, `sort_by`, `any`, `all`, written in a Lugha prelude; v3 complete.

**Status:** approved 2026-10-13 — session 51
**Milestone:** 16 (one PRP; completes milestone 16 and the v3 spec, tags `m16` and `v3`)
**Spec:** `Language v3 Draft.md`:
- decision 5;
- §4 "Built-in array methods";
- §9 lowering notes (amended below);
- §10 "Array helpers";
- §11 (milestone 16);
- §12 (`sort_by` with an inconsistent `less`).

**Decisions:** MEMORY 44 (v3 direction), MEMORY 42 (instances, symbols), MEMORY 47 (closures).

## OBJECTIVE
Arrays get six helpers that take function values. The §10 program prints `ann, cy`, `61`, `cy`.

| Method | Type | Result |
| --- | --- | --- |
| `xs.map(f)` | `f: fun(T): U` | `U[]`, `f` applied to each element in order |
| `xs.filter(f)` | `f: fun(T): bool` | `T[]`, the elements for which `f` is true, in order |
| `xs.fold(init, f)` | `init: A`, `f: fun(A, T): A` | `A` |
| `xs.sort_by(less)` | `less: fun(T, T): bool` | `T[]`, stably sorted |
| `xs.any(f)` / `xs.all(f)` | `f: fun(T): bool` | `bool`, stopping at the first element that decides |

Each takes `self`: the receiver never changes, and each array result is new. When this is done, the v3 spec is complete.

## CONTEXT

- **Starting state:**
  - Arrays have the built-in methods `push`, `pop` and `clear`, lowered inline.
  - Generics, function values and closures all work.
- **Ending state:**
  - The six helpers are generic functions in a prelude written in Lugha, bundled with `lughac` and loaded with every program.
  - `xs.map(f)` checks and compiles as a call of the prelude's `map` at the receiver's element type.
- **Related existing code:**
  - `src/modules/` (loading);
  - `src/check/` (`builtins.rs`, `method_calls.rs`, `call.rs`, `infer.rs`, `instances.rs`);
  - `src/codegen/methods.rs`;
  - `src/driver/` (`pipeline.rs`, `build.rs` emit, `source.rs`);
  - `src/lib.rs`.
- **Open decisions that must be resolved first:** none.

### Discovery answers (session 51)
1. **The helpers are written in Lugha:**
   - they live in a prelude, `src/prelude.la`, embedded in `lughac` like the runtime;
   - they are checked with every program and monomorphized per use like any generic code;
   - to the user they are still array methods.

   This amends the draft's §9 note that they are "emitted inline".

### Design choices for review
- **The prelude module:**
  - It is loaded after the user's modules, with module path `$prelude`; `$` can't appear in a module name, so no user file can clash. Its items are keyed `$prelude.map`, and its symbols are `lugha_fn_$prelude/map<Person, string>`.
  - No program can name it: there's no `import`, and `map(xs, f)` is not in scope.
  - Its diagnostics name the file `<prelude>`. Any error in it is a compiler bug (exit 2), since it is tested with the compiler.
  - `--emit=tokens|ast` doesn't show it. `--emit=ir` does show its instances, as compiled code.
- **Method calls:**
  - `xs.m(args)` on an array, where `m` is one of the six, is a call of `$prelude.m` with `xs` as its first argument;
  - the element type comes from the receiver's type, and the other type arguments (`U` of `map`, `A` of `fold`) are inferred as for any generic call;
  - closures as arguments get their parameter types from it (`xs.map(fun(x) = x * 2)`);
  - the receiver is checked once, before the arguments, as for every method call.
- **Partly-known closure types:** when a closure argument's expected type still has an undecided parameter (`U` in `fun(T): U` while checking `map`'s `f`), the closure gets the known parts. Its parameter types come from `T`, and its return type from its body, which then decides `U`. A closure parameter whose type is still undecided is E0412, as today.
- **Prelude names don't touch user names:** user functions named `map`, `filter` and so on are fine. A user method named `map` on a user type is unrelated, since the helpers exist only on arrays.
- **Behaviour:**
  - `sort_by` is a stable bottom-up merge sort. If `less` is inconsistent, the result is some order of the same elements, never a crash or a lost element (the draft's §12 question, resolved by documenting it).
  - `any`/`all` stop at the first deciding element; `fold` goes left to right; `map` and `filter` call `f` once per element, in order.
  - A panic inside `f` stops the program as usual.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Prelude** — `src/prelude.la`, embedded as `lugha::PRELUDE`
- The six functions, generic, with comments.
- `sort_by` merges into a new array and never mutates its argument.

**Loading**
- `modules::load` appends the prelude module (path `$prelude`) to every program, with one offset space as today.
- The source map names it `<prelude>`.
- A diagnostic inside the prelude is reported as an internal compiler error (exit 2).

**Checker**
- **Array receivers:** `on_value` recognises the six names and checks the call through the prelude function. The receiver's element type is bound first, then the arguments are inferred with closures last, as today. Arity errors (E0405) and argument errors (E0403/E0401) are reported at the call.
- **Partly-known expectations:** `infer` gives a closure argument its expected type with the still-undecided callee parameters replaced by "unknown", so the closure takes what is known.
- **Side tables:** the call records its key and type arguments (`Checked.callees`, `Checked.instances`) and a use for the instance pass.

**Codegen**
- An array method call to a prelude helper calls the instance with the receiver value first, then the arguments.
- `push`, `pop` and `clear` are unchanged.

**Driver**
- `--emit=tokens|ast` skips the prelude.

**Spec** — `Language v3 Specification.md`
- **Folded in:**
  - §4: the helper table and its rules, with the `sort_by` note;
  - §9: the prelude as a lowering note, `lugha_fn_$prelude/…` in the symbol table, and the amended draft text;
  - §10: the array-helpers program;
  - §11: milestone 16 done.
- **Completion:** the title drops "(in progress: …)", the status says v3 is complete, and "Changes since v2" gains the helpers.
- **The draft:** its §9 note and §12 question are amended or resolved.

**Docs**
- CLAUDE.md:
  - STACK: the v3 spec is complete;
  - the tree: `src/prelude.la`, new files;
  - anti-pattern 5: v3 is complete.
- README, TODO (milestone 16 ✅, v3 complete), CHANGELOG, MEMORY decision 48, CONTEXT; tags `m16` and `v3`.

### Must NOT Do
- **No other built-in methods** (no `len()`, `reverse`, `find`), and no user-visible prelude names.
- **No mutation of the receiver** by any of the six.
- **No stdin or files.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **User mistakes** (wrong arity, wrong closure type, an E0412 closure parameter) are ordinary records at the user's call.
- **Prelude errors:** a diagnostic located in the prelude, or a missing prelude instance in codegen, is a compiler bug (exit 2, internal error).

## SECURITY CONSIDERATIONS
- The prelude is compiler-owned source, not external input.
- No `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — checker (a new `check/helpers.rs`, or in `builtins.rs`):
- [ ] **Types:**
  - `xs.map(fun(x) = x * 2)` is `i64[]`;
  - `xs.map(fun(x) = "{x}")` is `string[]` (`U` from the body);
  - `xs.filter(fun(x) = x > 1)` is `i64[]`;
  - `xs.fold(0, fun(acc, x) = acc + x)` is `i64`;
  - `xs.sort_by(fun(a, b) = a < b)` is `i64[]`;
  - `xs.any(...)` and `xs.all(...)` are `bool`.
- [ ] **Named functions as arguments:** `xs.map(double)` works.
- [ ] **Errors:**
  - `xs.map()` is E0405;
  - `xs.filter(fun(x) = x)` on `i64[]` is E0403;
  - `xs.map(fun(x: string) = x)` on `i64[]` is E0403.
- [ ] **Chaining:** `people.filter(…).map(…)` checks.
- [ ] **Names:** user functions named `map` and `filter` are still allowed and callable; `map(xs, f)` with no user `map` is E0301.
- [ ] **The prelude** checks with no diagnostics.

Acceptance — `tests/programs/m16/` (run at `-O0` and `-O2`):
- [ ] `helpers.la`: each helper on `i64[]` and `string[]`, including empty arrays and chaining.
- [ ] `sort.la`: `sort_by` on numbers, strings and structs, both orders.
  - Stability: equal keys keep their order.
  - The receiver is unchanged.
  - 1,000 elements in a pseudo-random order come out sorted.
- [ ] `shortcircuit.la`: `any`/`all` stop early (a counter in a captured array can't count, so the `f` passed panics on the element after the deciding one).
- [ ] `structs.la`: helpers over structs and closures capturing locals (`filter(fun(p) = p.age >= limit)`).
- [ ] `e0403.la`: reject golden for a wrongly typed closure.
- [ ] The §10 array-helpers program runs from the spec.

Driver:
- [ ] `--emit=ast` on a user program doesn't show the prelude.
- [ ] `--emit=ir` contains `lugha_fn_$prelude/map<i64, i64>` when `map` is used, and no prelude code otherwise.

Regression:
- [ ] Every earlier test passes. Programs that don't use the helpers give byte-identical IR, since the prelude's generic functions compile only when used.

## ROLLBACK PLAN

- Branch `prp-041-array_helpers`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The §10 array-helpers program and the `m16/` programs pass.
- [ ] The spec is complete (no "in progress"); CLAUDE.md, README, TODO, CHANGELOG and MEMORY are updated; tags `m16` and `v3`.
- [ ] No file over 300 lines (splits proposed first); no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m16/helpers.la
```
