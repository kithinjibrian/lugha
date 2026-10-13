## FEATURE: Compiling interface values — dispatch tables, boxes, dynamic calls; milestone 20 complete.

**Status:** approved 2026-10-13 — session 59
**Milestone:** 20 (second of two PRPs: 046 checking, **047 compiling** — gate removed, `Language v5 Specification.md` begins, tag `m20`)
**Spec:** `Language v5 Draft.md`:
- decisions 1–3 and 7;
- §4 "Interface values" (copies);
- §7 (two words, the dispatch table);
- §8 (no C boundary);
- §9 (symbols, conversion, calls, tables only for converted pairs);
- §10 "Interface values";
- §11 (milestone 20).

**Decisions:** MEMORY 54 (v5 direction), MEMORY 55 (checking interface values), MEMORY 34 (gates), MEMORY 39 (side tables), MEMORY 18/20 (copy sites, move on last use).

## OBJECTIVE
Programs with interface values build and run. The §10 interface-values program prints `16.0`, `1.0`, `circle`. The milestone 20 gate is gone, and `lughac spec` prints the new `Language v5 Specification.md` (in progress: milestone 20).

## CONTEXT

- **Starting state:** the checker accepts interface values and records each conversion (`Checked.conversions`) and each dynamic callee (`<Interface>.<method>` in `Checked.callees`). `driver/pipeline.rs::stop_gated` stops `build`/`run`/`--emit=ir`, and `codegen/value.rs` has an `unreachable!` arm for `Type::Interface`.
- **Ending state:** codegen lowers interface values; the gate and the `unreachable!` arm are removed.
- **Related existing code:**
  - `src/codegen/value.rs` (`llvm_type`), `structs.rs` (`layout`), `expr.rs` (`expr`), `methods.rs` (`method_call`), `copy.rs` (copies, `mangle`), `function.rs` (`arguments`), `fun_values.rs` (the `{ code, env }` precedent);
  - `src/check/iface_values.rs` (`convert`), `check/instances.rs` (`used`, `bound_used`), `check/mod.rs` and `check/modules/mod.rs` (`Checked`);
  - `src/driver/pipeline.rs` (the gate), `tests/interface_values.rs`;
  - `src/lib.rs` (`SPEC`), `docs/specs/`.
- **Open decisions that must be resolved first:** none.

### Discovery
No new questions. The semantics were settled by the v5 draft (session 57) and the milestone split (session 58).

### Design choices for review
- **An interface value is `{ ptr table, ptr box }`,** like a function value's `{ code, env }`: 16 bytes, aligned 8, passed and returned by value (not an aggregate in `is_aggregate`'s sense).
- **The box** is a `lugha_rt_alloc` allocation holding one value of the concrete type, primitives included.
- **The dispatch table** is a constant global `lugha_vt_<Type>.<Interface>` (types and interfaces printed as in other symbols: `lugha_vt_Circle.Shape`, `"lugha_vt_Stack<i64>.Sized"`), emitted once per pair a program converts:
  - **slot 0, identity:** the address of a constant `lugha_type_<Type>` global, one per concrete type, for type patterns (milestone 21);
  - **slot 1, copy:** `lugha_copy_<mangled>_box(ptr) -> ptr`, which allocates a new box holding a deep copy of the old box's value;
  - **slots 2…, methods:** one per interface item, in declaration order.
- **No `lugha_dyn_` wrappers** (amends the draft's §9). Every method already takes its receiver as a pointer: to a copy for `self`, to the caller's place for `mut self`. So the box pointer can be passed straight to `lugha_fn_<Type>.<method>`, and the table points at the method itself.
  - A `mut self` method writes into the box, which belongs to that interface value alone.
  - A `self` method can't write to its receiver (E0501), so passing the box itself is safe.
- **Conversion** happens where `Checked.conversions` records it, centrally in `Lowerer::expr`:
  - lower the value at its own type;
  - deep-copy it unless it is fresh or moved (the rules `value_for_store` uses);
  - allocate the box, store the value, and pair it with the table.

  Inside a generic instance whose type argument is already the interface (`T = Shape`), the value passes through unchanged. A converted expression is a fresh value, so the copy sites don't copy it again.
- **Dynamic calls:** for a callee `<Interface>.<method>`:
  - take the table and box from the receiver's value (for `mut self` too: the box, not the place, is what changes);
  - load slot `2 + index`, and call it indirectly with the box first and then the arguments, passed as for any call.
- **Copies:** an interface value is `is_deep`. Its copy helper `lugha_copy_<mangled interface>` calls the table's copy slot and pairs the same table with the new box. Structs, arrays and payloads holding interface values copy through it, as for arrays.
- **Reachability** (checker): a converted type's methods must be compiled even if nothing calls them directly. When `convert` records a conversion:
  - for a struct or enum whose methods are generic, it records each interface method as used (`used`, with the type's arguments);
  - for a type parameter, it records each through its bound (`bound_used`).

  So `Stack<i64>.size` is instantiated when a `Stack<i64>` becomes a `Sized`.
- **Interface metadata for codegen:** `Checked.interfaces`, giving each interface key's methods in declaration order (name, receiver, params, return type), so codegen never re-derives them (architecture rule 4).
- **Programs without interface values give byte-identical IR:** nothing is emitted unless a conversion exists.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Checker:**
  - `Checked.interfaces`;
  - the reachability uses in `convert`.
- **Codegen** (new `codegen/interfaces.rs`, with a second new file if it passes 300 lines):
  - `llvm_type` and `layout` for `Interface`;
  - `type_id`, `table` (`lugha_vt_…`), `box_copy` (`lugha_copy_…_box`), `convert`, `dynamic_call`;
  - the copy helper for interface types;
  - `mangle` for interface types (length-prefixed, like structs).
- **Hooks into existing files** (one or two lines each):
  - `expr.rs` (conversion);
  - `methods.rs` (dynamic call);
  - `copy.rs` (the `Interface` arm in `copy_function`, `mangle`, and no copy of converted expressions).
- **Driver:** delete `stop_gated` and its calls.
- **Spec:** create `Language v5 Specification.md` from v4, titled "Lugha v5 Specification (in progress: milestone 20)", folding in:
  - the draft's §3 type note;
  - §4 "Interface values" (without type patterns, default methods or `?` conversion);
  - §7, §8 and §9 for interface values (symbols as above);
  - E0425 and the reused codes;
  - the §10 interface-values program;
  - §11 milestone 20 done;
  - the compatibility sentence.

  `lughac spec` prints it (`lib.rs`, `tests/cli.rs`), and v4 is frozen. The v1 "Out of scope" list in §11 is replaced by the draft's v5 list (draft §12).
- **Draft:** a note in §9 that the `lugha_dyn_` wrappers were dropped (this PRP).
- **Docs:**
  - CLAUDE.md: architecture rule 7 (`lugha_vt_`, `lugha_type_`, `lugha_copy_…_box`), anti-pattern 4 (the one named conversion), anti-pattern 5 and STACK (v5 in progress), and the tree;
  - README, TODO, CHANGELOG, MEMORY decision 56, CONTEXT; tag `m20`.

### Must NOT Do
- **No type patterns** (milestone 21), **default methods** or **`?` conversion** (milestone 22).
- **No inline storage of small values** (draft §12 keeps that an open optimisation).
- **No `malloc`:** boxes come from `lugha_rt_alloc` (architecture rule 8).

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **No new user errors:** the checker reports them all.
- **Codegen invariants** are `expect`s stating the invariant: a table for a pair the checker satisfied, a method that exists.

## SECURITY CONSIDERATIONS
- **Boxes** are GC allocations sized by `layout`, written once at conversion and only through typed stores.
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — codegen IR (`codegen/interfaces.rs`):
- [ ] **One table per converted pair:** converting two `Circle`s and a `Square` to `Shape` gives one `lugha_vt_Circle.Shape` and one `lugha_vt_Square.Shape`, each pointing at the methods in declaration order.
- [ ] **Dynamic calls:** `s.area()` is an indirect call through a loaded slot, with the box first.
- [ ] **Boxes:** conversion calls `lugha_rt_alloc` and never `malloc`.
- [ ] **Copies:** copying a `Shape` calls `lugha_copy_5Shape`, which calls the table's copy slot.
- [ ] **No conversion, no tables:** a program without interface values has no `lugha_vt_`, `lugha_type_` or `_box` symbols.

Unit tests — checker (`check/iface_values.rs`):
- [ ] **Reachability:** converting a `Stack<i64>` to `Sized` makes `Stack.size<i64>` an instance; converting a `T: Sized` inside a generic function does so for each instance.

Acceptance — `tests/programs/m20/` (run at `-O0` and `-O2`):
- [ ] `shapes.la`: the §10 program.
- [ ] `copies.la`:
  - a `mut self` call on a copy doesn't change the original;
  - a struct holding an array, converted and then changed through the interface value, leaves the original intact;
  - an interface value in a struct field, an array and an `Option`, copied and changed;
  - move on last use.
- [ ] `generic.la`:
  - a generic struct (`Stack<i64>`) as an interface value;
  - `total_area<T: Shape>` called with a `Shape[]`;
  - a `T: Shape` converted inside a generic function;
  - an interface from another module.
- [ ] `any.la`: an interface with no methods holding an `i64`, a `string` and a struct (boxed primitives).
- [ ] The §10 interface-values program runs from the spec (`tests/spec.rs`).

Driver / CLI:
- [ ] `tests/interface_values.rs` becomes a run test: `run` prints the three lines, and `--emit=ir` contains `@lugha_vt_Circle.Shape`.
- [ ] `lughac spec` starts with `# Lugha v5 Specification` (`tests/cli.rs`).

Regression:
- [ ] Every earlier test passes; programs without interface values give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-047-interface_value_compiling`, merged into `main` after green CI.
- To abandon: delete the branch; the gate stays.

## ACCEPTANCE CRITERIA
- [ ] The §10 interface-values program and the `m20/` programs pass at `-O0` and `-O2`.
- [ ] The gate is gone; the v5 spec begins (in progress: milestone 20); docs updated; tag `m20`.
- [ ] No file over 300 lines (splits proposed first); no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m20/shapes.la
```
