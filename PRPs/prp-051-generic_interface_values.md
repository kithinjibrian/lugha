## FEATURE: Generic interface values — `Source<i64>` as a type; milestone 23 complete.

**Status:** approved 2026-10-13 — session 65
**Milestone:** 23, the second of two PRPs. 050 covered bounds; **051 covers values**, completes the milestone, and adds the tag `m23`.
**Spec:** `Language v6 Draft.md`:
- decision 3;
- §4 "Generic interfaces", in particular "As a value type";
- §9: tables named with the interface's arguments;
- §10 "Generic interfaces";
- §11: milestone 23.

**Decisions:** MEMORY 59 (v6), MEMORY 60 (bounds, `Applied`), MEMORY 55–58 (interface values, tables, type patterns, defaults, `?`).

## OBJECTIVE
A generic interface is a type wherever a plain one is. The draft's §10 program prints `3`, `a-b`, `11`:

```
let mut s: Source<i64> = Count { n: 10, end: 12 };
println(s.next().unwrap_or(0));                 // 11, through the table lugha_vt_Count.Source<i64>

fun total<T>(s: Source<T>, zero: T): T { … }
total(Count { n: 0, end: 3 }, 0)                // T = i64 from Count.next, then Count converts
```

Generic interface values support everything v5 values do, under v5's rules: conversion, dynamic calls, copies, type patterns, default methods and `?`.

## CONTEXT

- **Starting state (after PRP-050):**
  - `Type::Interface(String)` has no arguments.
  - A generic interface in a type position is E0418 ("can't be a value type yet", `errors/v6.rs`).
  - `Checked.interfaces` lists each key's methods in their declared types.
- **Ending state:**
  - `Type::Interface(String, Vec<Type>)`, printed `Source<i64>`.
  - Values of generic interfaces check, compile and run.
  - Milestone 23 is done, and the §10 program runs from the spec.
- **Related existing code:**
  - **Every `Type::Interface` site** (about 35, listed by `grep`):
    - checker: `types.rs`, `iface_values.rs`, `iface_calls.rs`, `iface_tables.rs`, `type_patterns.rs`, `satisfy.rs`, `bound_infer.rs`, `bounds.rs`, `defaults.rs`, `method_calls.rs`, `literal.rs`, `env.rs`, `errors/v5.rs`;
    - codegen: `value.rs`, `structs.rs`, `copy.rs`, `interfaces.rs`, `dispatch.rs`, `methods.rs`, `defaults.rs`.
  - **Substitution and inference:** `generics.rs` (`replace`, `has_param`), `infer.rs` (`unify`, `unbound`, `input`), `Type::substitute`.
- **Open decisions that must be resolved first:** none.

### Discovery (session 65)
- **Inference into interface-typed parameters:** in `fun total<T>(s: Source<T>, zero: T)`, an argument of a concrete type fixes `T` from its methods, and then converts. This is the same rule PRP-050 gave bounds.

### Design
- **The type:** `Type::Interface(key, args)`, where a plain interface has `args: []`.
  - **Printing:** `Source<i64>`; a plain interface prints as before.
  - **Generic treatment:** substitution, `has_param`, `unify` and `unbound` treat its arguments as they treat a struct's.
  - **Resolving it:**
    - `Source<i64>` resolves with E0419 for an argument that misses its bound;
    - E0425 applies as before;
    - a bare `Source` is E0418;
    - `errors::generic_interface_value` is deleted.
- **Checker,** v5's rules, run at the arguments by way of `Applied { key, args }`:
  - **Conversion:**
    - where `Source<i64>` is expected, a value converts when it satisfies `Source<i64>` (E0422 otherwise);
    - a `Source<i64>` value is already one;
    - a `Source<string>` value, or another interface, is E0403.
  - **Calls:** `s.next()` is checked against the substituted signature.
  - **Satisfaction by an interface value,** for `T: Source<i64>` given a `Source<i64>`: its items at its arguments.
  - **Type patterns:** `c: Count` on a `Source<i64>` checks `Count` against `Source<i64>`.
  - **`?`:** `F` may be a generic interface, `Result<T, Error<string>>`.
  - **Reachability:** `conversion_uses` records a default's instance as `[concrete, X…]`, and a generic type's methods as before.
  - **Inference into an interface-typed parameter** (new code in `bound_infer.rs`): when the declared type is `Source<T>` with `T` unbound and the argument's type is concrete, `T` is read off its methods (`read_off`). The argument then converts at the substituted type.
- **`Checked.interfaces`** gains each interface's parameter names. Its type becomes `HashMap<String, InterfaceShape>`, where `InterfaceShape { params, methods }`. Codegen substitutes them with an interface type's arguments, then with the current instance's (`concrete`), so it never re-derives a signature (architecture rule 4).
- **Codegen:**
  - **Tables:** `lugha_vt_<Type>.<Interface<args>>` (`"lugha_vt_Count.Source<i64>"`). Its method slots are the concrete type's methods, or the default's instance `lugha_fn_Source<i64>.skip<Count>`.
  - **Copy helpers:** `lugha_copy_<mangled>`, with the arguments mangled as for generic structs.
  - **Dynamic calls** use the substituted signature.
  - **`box_as`** (`?`) takes the interface's arguments.
  - **Type patterns** are unchanged: the identity slot doesn't depend on the interface.
- **Programs without generic interface values give byte-identical IR.** A plain interface's symbols and types print exactly as before.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Checker:**
  - `Type::Interface(String, Vec<Type>)` and every site above;
  - resolution with arguments (E0418, E0419, E0425);
  - conversion, calls, satisfaction, type patterns, `?` and reachability at the arguments;
  - inference into interface-typed parameters;
  - `InterfaceShape`;
  - delete the not-yet E0418.
- **Codegen:**
  - substituted signatures for tables, dynamic calls and copies;
  - symbols with arguments;
  - default slots of generic interfaces.
- **Spec** (`Language v6 Specification.md`):
  - fold in §4 "As a value type", the §9 table symbol, the §10 generic-interfaces program and its expected output;
  - §11: milestone 23 done;
  - the status becomes "in progress: milestone 24 next".
- **Docs:** CLAUDE.md (STACK; rule 7 table symbols with arguments), README, TODO, CHANGELOG, MEMORY decision 61, CONTEXT. The tag `m23` is added at the merge.

### Must NOT Do
- **No `Iter`, no `Self`, no conversion between interfaces:** those are milestones 24–26. A `Source<i64>` value doesn't convert to `Source<string>` or to any other interface.
- **No `==` or printing** of interface values (v5's rule).

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **No new codes.** Mistakes are reported as:
  - **E0418:** wrong count, or a bare generic interface;
  - **E0419:** an argument that misses its parameter's bound;
  - **E0422:** a value that doesn't satisfy `Source<X>`, with substituted help;
  - **E0403:** `Source<i64>` where `Source<string>` is expected;
  - **E0412:** a parameter that nothing fixes;
  - **E0424:** `?` whose error neither matches nor converts.
- **Codegen invariants** are `expect`s: an interface's shape exists for every converted key.

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — checker (`check/iface_values.rs`, `check/bound_infer.rs`, `check/applied.rs`):
- [ ] **Types:**
  - `Source<i64>` resolves as a type;
  - `Source<P>` with `interface Source<T: Ord>` and a non-`Ord` `P` is E0419;
  - `Source` and `Source<i64, i64>` are E0418;
  - the "not a value type yet" test now checks.
- [ ] **Conversion:**
  - `let s: Source<i64> = Count {…}` converts;
  - `let s: Source<string> = Count {…}` is E0422, with help showing `Option<string>`;
  - a `Source<i64>` where a `Source<string>` is expected is E0403;
  - `Source<i64>[]` holds `Count` and `Repeat<i64>`.
- [ ] **Calls:** `s.next()` has type `Option<i64>`; `mut self` rules as in v5.
- [ ] **Inference:**
  - `total(Count {…}, 0)` gives `T = i64` and converts;
  - `fun f<T>(s: Source<T>)` given a `Source<string>` value gives `T = string`.
- [ ] **Satisfaction:** a `Source<i64>` value satisfies `S: Source<i64>`, and calls through it dispatch.
- [ ] **Type patterns:** `c: Count` on a `Source<i64>` checks, and `w: Words` (which offers `Option<string>`) is E0426.
- [ ] **`?`:** an error converting into `Result<i64, Problem<string>>` checks.

Unit tests — codegen (`codegen/interfaces.rs`, `dispatch.rs`):
- [ ] Converting a `Count` to `Source<i64>` emits `@"lugha_vt_Count.Source<i64>"` with a slot for `lugha_fn_Count.next`.
- [ ] A default slot points at `@"lugha_fn_Source<i64>.skip<Count>"`.
- [ ] A dynamic call on a `Source<string>` returns `%"Option<string>"`.

Acceptance — `tests/programs/m23/`:
- [ ] `values.la`:
  - `Source<i64>` and `Source<string>` values in arrays, struct fields and `Option`;
  - copies are independent;
  - a generic function taking `Source<T>`, with `T` inferred;
  - a default method through a value;
  - type patterns on a `Source<i64>`;
  - `?` into a generic interface error.
- [ ] `e0403.la`: reject golden, a `Source<i64>` where a `Source<string>` is expected.
- [ ] `e0418.la`: becomes a bare `Source` as a type.
- [ ] The §10 generic-interfaces program runs from the spec (`tests/spec.rs`).

Regression:
- [ ] Every earlier test passes, and programs without generic interface values give byte-identical IR (compared with `main`).

## ROLLBACK PLAN

- Branch `prp-051-generic_interface_values`, merged into `main` after CI passes; tag `m23`.
- To abandon: delete the branch. `main` keeps generic interfaces as bounds only.

## ACCEPTANCE CRITERIA
- [ ] The §10 generic-interfaces program and the `m23/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [ ] Milestone 23 is marked done in the spec; docs updated.
- [ ] No file over 300 lines: new code goes in new files, and any split of an existing file is proposed first. No new dependencies, no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m23/values.la
```
