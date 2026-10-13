## FEATURE: Checking interface values — interfaces as types, conversion, dynamic calls.

**Status:** approved 2026-10-13 — session 58
**Milestone:** 20 (first of two PRPs: **046 checking**, 047 compiling — dispatch tables, boxes, dynamic calls, copies, gate removed, `Language v5 Specification.md` begins, tag `m20`)
**Spec:** `Language v5 Draft.md`:
- decisions 1–3 and 7;
- §3 (interface names as types);
- §4 "Interface values" (all of it except printing's codegen side);
- §9 (E0425; reused E0305, E0403, E0404, E0419, E0422);
- §11 (milestone 20).

No spec file changes in this PRP: `lughac spec` keeps printing v4 until PRP-047 starts the v5 specification, because `build` and `run` don't accept interface values yet.
**Decisions:** MEMORY 54 (v5 direction), MEMORY 34 (the gate pattern), MEMORY 39 (keys and side tables).

## OBJECTIVE
`lughac check` accepts programs that use interface values and reports their mistakes:
- an interface whose items are all methods is a type: `Shape`, `Shape[]`, `Option<Shape>`, `m.Shape`, as fields, payloads, parameters, results, function types and type arguments;
- a value converts where the expected type is exactly an interface it satisfies;
- `s.area()` on a `Shape` is checked against the interface's signature;
- `build`, `run` and `--emit=ir` stop at a gate, exit 2: "not implemented yet: interface values (milestone 20)", until PRP-047.

## CONTEXT

- **Starting state:** an interface written as a type is E0305 ("use it as a bound"), reported in `check/modules/lookup.rs`. Interfaces exist only as bounds (`check/interfaces.rs`, `check/satisfy.rs`).
- **Ending state:** `Type::Interface(key)`, a `Checked.conversions` side table, dynamic method calls checked, and a driver gate before codegen.
- **Related existing code:**
  - `src/check/types.rs` (`Type`, `fits`, `substitute`, `contains_array`, `Display`);
  - `src/check/expr.rs` (`expr`, `expect_type`), `src/check/infer.rs` (arguments), `src/check/body.rs`, `src/check/closures.rs`;
  - `src/check/modules/lookup.rs` (E0305), `src/check/generics.rs` (`resolve_named`, `resolve_applied`);
  - `src/check/interfaces.rs`, `src/check/satisfy.rs` (`satisfies`, `offer`), `src/check/bounds.rs` (`eq_ok`, `holds_fun`);
  - `src/check/method_calls.rs` (`on_value`), `src/check/access.rs` (fields);
  - `src/check/mod.rs` (`Checked`), `src/driver/pipeline.rs`.
- **Open decisions that must be resolved first:** none.

### Discovery answers (session 58)
1. **Milestone 20 is two PRPs:** this one checks, PRP-047 compiles. Between them `check` accepts interface values and `build`/`run` stop at the gate.

### Design choices for review
- **The type:** `Type::Interface(key)`, keyed like structs (`shapes.Shape` for another module's). It prints as other named types do. It has no type arguments (`Shape<i64>` is E0418).
- **E0425:** whether an interface can be a type is known from its declaration (every item has a receiver), so it is recorded when the interface is collected. Writing one with an associated function as a type is E0425, naming the associated function, and becomes `Error`.
- **Where conversion happens — one place:** `Checker::expr`. When the expected type is exactly `Interface(I)` and the expression's type is something else:
  - if the type satisfies `I` (the existing `satisfies`, with the current module's visibility), the expression keeps its own type in `Checked.types`, `Checked.conversions` records `ExprId → Interface(I)`, and `expr` returns `Interface(I)` to its caller;
  - otherwise it is E0422 (the existing message, naming each missing or mismatched item) and the result is `Error`;
  - `Error`, `Never` and `Interface(I)` itself pass through unchanged.

  Because blocks, `if` and `match` hand their expected type to their tails and arms, each branch converts by itself: `if c { circle } else { square }` with `Shape` expected works.
- **Expected types reach arguments:** `infer.rs` passes a closure-only hint today. An argument whose declared type, after substituting what is bound, is exactly an interface gets it as its expected type, so `show(Circle { r: 2.0 })` converts.
- **Not inside other types:** `fits` stays structural, so `Circle[]` where `Shape[]` is expected stays E0403. An `Interface(J)` where `Interface(I)` is expected is E0403 too, with help that interface values don't convert to another interface.
- **No expected type, no conversion:** `[c, sq]` alone is E0403 (elements disagree). The mismatch help suggests annotating the array when both element types satisfy an interface. This is help text only; no search for one.
- **Type parameters:** `T` converts to `I` when its bounds satisfy `I` (the existing `offer` rule). PRP-047 converts each instance's concrete type.
- **Calls:** in `on_value`, a receiver of type `Interface(I)` looks the method up in `I`'s items, like `through_bound` does for a parameter:
  - an item without a receiver, or an unknown name, is E0415 (help: list the interface's methods);
  - arguments and result are checked against the item's signature;
  - a `mut self` item follows the receiver place rules (E0501, E0502, E0508), as for any method.

  The callee is recorded in `Checked.callees` as `<interface key>.<method>`, so codegen knows it is a dynamic call.
- **Fields:** `s.r` on a `Shape` is the existing no-field error, with help to use a type pattern (v5 milestone 21).
- **Equality:** `eq_ok` is false for `Interface`, and so for anything holding one, so `==` is E0404 (help: match on the concrete types) and `Eq`/`Ord` bounds are E0419.
- **As a type argument:** `Interface(I)` satisfies interface `J` when `I` declares each of `J`'s items the same way, so `I` satisfies `I` and `total_area<Shape>(shapes)` checks.
- **Copies and moves:** an interface value is deep-copied like an array (its inside can change through `mut self`). `contains_array` (renamed `is_deep`, as it now covers both) is true for it and for anything holding one inline, so move on last use and copy sites treat it like any deep value.
- **Printing:** `print`/`println`/`eprint`/`eprintln` and interpolation already accept only the types they list, so an interface value is E0403 without new code. A test pins this.
- **C interop:** an interface type in an `extern` signature is E0409, as for every aggregate.
- **The gate:** after checking, `driver/pipeline.rs` returns an internal "not implemented yet: interface values (milestone 20)" (exit 2) for `build`, `run` and `--emit=ir` when any recorded annotation contains `Interface`. Every interface type in a program is written somewhere, so annotations are enough. `check`, `--emit=tokens` and `--emit=ast` are unaffected.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Types** (`check/types.rs`): `Type::Interface(String)`; `Display`, `substitute`, `fits` (equal keys), `is_deep`.
- **Interfaces** (`check/interfaces.rs`): `InterfaceInfo.value_ok` (no associated functions), set when collected.
- **Resolution** (`check/modules/lookup.rs`, `check/generics.rs`): an interface name in a type position is `Interface(key)`, or E0425; type arguments on one are E0418.
- **Conversion** (new `check/iface_values.rs`):
  - `convert(expr, ty, expect)`, called from `Checker::expr`;
  - `Checked.conversions: HashMap<ExprId, Type>`;
  - the argument hint in `infer.rs`.
- **Calls** (`check/iface_values.rs`): `through_interface`, used by `on_value`; the callee key.
- **Satisfaction** (`check/satisfy.rs`): an `Interface(I)` offer against interface `J`.
- **Equality** (`check/bounds.rs`): `eq_ok` and the help text.
- **Errors** (new `check/errors/v5.rs`): E0425; the E0403 help texts above.
- **Gate:** `driver/pipeline.rs` (or a small `driver/gate.rs` if `pipeline.rs` would pass 300 lines).

### Proposed split (approved with this PRP)
`check/method_calls.rs` is at 300 lines and `on_value` gains a branch. `arguments` and `unchecked` (about 45 lines, with their doc comments) move verbatim into a new `check/arguments.rs`. Nothing else moves.

### Must NOT Do
- **No codegen:** nothing in `src/codegen/`. The gate stops `build` and `run`.
- **No type patterns** (milestone 21), **default methods** or **`?` conversion** (milestone 22).
- **No conversion inside other types,** and none between interfaces.
- **No spec file change** (PRP-047 starts `Language v5 Specification.md`).

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **Every mistake is a `Diagnostic`,** and checking continues after it.
- **New code:** E0425. **Reused:** E0305 (unknown names only), E0403, E0404, E0409, E0415, E0418, E0419, E0422.
- **The gate** is an internal-error exit 2, as the generics gate was (MEMORY 34).

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — checker (`check/iface_values.rs` unless noted):
- [ ] **Types:** `let s: Shape = c;`, `Shape[]`, `Option<Shape>`, a struct field and an enum payload of type `Shape`, a `fun(Shape): f64`, and `shapes.Shape` from another module all check.
- [ ] **Conversion sites:** a `let`, an argument (plain, method and generic call), a struct-literal field, array elements, `Some(c)` for `Option<Shape>`, `Ok(c)`, an assignment, `return`, a tail, and both branches of an `if` and a `match`.
- [ ] **No conversion:** `let s = c;` keeps `Circle`; `[c, sq]` alone is E0403; `Circle[]` for `Shape[]` is E0403; a `Named` value for `Shape` is E0403.
- [ ] **E0422:** converting an `i64`, and a type missing one method, name what's missing; another module's private method doesn't count.
- [ ] **E0425** (`interfaces.rs`): an interface with `fun make(): i64;` used as a type; the same interface as a bound still works.
- [ ] **Calls:** `s.area()` is `f64`; `s.size()` is E0415; wrong arity E0405; a wrong argument E0403; a `mut self` call on an immutable `s` is E0501 and on `mk_shape().grow(1.0)` E0502.
- [ ] **Fields, equality, printing:** `s.r` is the no-field error with type-pattern help; `a == b` is E0404; `Shape` for `T: Eq` is E0419; `println(s)` and `"{s}"` are E0403.
- [ ] **Generics:** `total_area<T: Shape>` called with a `Shape[]` checks; a `T: Shape` value converts to `Shape`.
- [ ] **Copies:** `is_deep` is true for `Shape` and for a struct holding one (`types.rs`).
- [ ] **E0409:** `extern fun f(s: Shape);`.

Driver / CLI (`tests/cli.rs`):
- [ ] `check` on a valid interface-values program exits 0; `build`, `run` and `--emit=ir` exit 2 with the gate message.

Acceptance — `tests/programs/m20/` (reject goldens; run cases come with PRP-047):
- [ ] `e0425.la`: an interface with an associated function used as a type.
- [ ] `e0422.la`: converting a type that lacks a method.

Regression:
- [ ] Every earlier test passes; programs without interface types give byte-identical IR. The old E0305 interface test becomes an E0425 test or a passing case.

## ROLLBACK PLAN

- Branch `prp-046-interface_value_checking`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] `lughac check` accepts the §10 interface-values program; `build` stops at the gate.
- [ ] Every test above passes.
- [ ] No file over 300 lines (the split above only); no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo test --test cli interface   # check accepts the §10 program; build stops at the gate
```
