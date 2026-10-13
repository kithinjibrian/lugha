## FEATURE: Functions as values — `fun(A): R` types, named functions as values, calling values.

**Status:** approved 2026-10-13 — session 49
**Milestone:** 14 (one PRP)
**Spec:** `Language v3 Draft.md`:
- decision 3;
- §3 (`base_type` function type);
- §4 "Function types", "Functions as values", "Calls";
- §7 (value layout);
- §8 (no function types in externs);
- §9 (`lugha_ref_*`, calling a function value, reused codes);
- §10 "Functions as values";
- §11 (milestone 14).

**Decisions:** MEMORY 44 (v3 direction), MEMORY 42 (instances, symbols), MEMORY 45 (the v3 spec in progress).

## OBJECTIVE
Functions are values: they can be stored, passed, returned and called. The §10 program prints `6` and `9`.
- **Types:** `fun(i64, i64): bool`, and `fun(i64)` for no result.
- **Named functions as values:** `let f = double;`, `apply(Circle.make, 1.0)`, `m.f`. A generic function is a value when an expected function type fixes its type arguments.
- **Calls:** any expression of function type can be called: `f(3)`, `make()(3)`, `fs[0](3)`, `b.on_click(2)`.
- **No `==`:** function values have no `==`, and neither do types containing them, so `T: Eq` now restricts.

## CONTEXT

- **Starting state:**
  - Functions can only be called by name.
  - A function name used as a value is E0406, and so is calling a local.
  - Every type is `Eq`.
- **Ending state:**
  - `Type::Fun` exists end to end.
  - Function values are `{ code, env }` pairs whose environment is null until closures (milestone 15).
  - Named functions are wrapped by `lugha_ref_<symbol>`.
- **Related existing code:**
  - `src/parser/` (`item.rs` `ty`, `generic.rs`), `src/ast/mod.rs` (`TypeKind`), `sexp/`;
  - `src/check/` (`types.rs`, `env.rs`, `call.rs`, `expr.rs`, `access.rs`, `method_calls.rs`, `bounds.rs`, `infer.rs`, `instances.rs`, `layouts.rs`);
  - `src/codegen/` (`value.rs`, `function.rs`, `declare.rs`, `methods.rs`, `instances.rs`, `structs.rs`, `heap.rs`, `equality.rs`, `copy.rs`).
- **Open decisions that must be resolved first:** none.

### Discovery answers (session 49)
1. **Arrays of functions** need parentheses: `fun(i64): i64[]` returns an array, and `(fun(i64): i64)[]` is an array of functions. Types may be parenthesized anywhere. Messages print types this way too.
2. **Calling a field:** `s.f(x)` calls method `f` if the type has one, otherwise field `f` if it holds a function. E0310 already forbids a method and a field with the same name, so this is never ambiguous. This amends the draft's §3 note that `(s.f)(x)` is required; `(s.f)(x)` still works.

### Design choices for review
- **Types:** `Type::Fun(Vec<Type>, Box<Type>)`, with `Type::Void` for no result. It prints as `fun(i64, i64): bool`, or `fun(i64)`. Inside an array it is parenthesized: `(fun(i64): i64)[]`.
- **Which names are values:**
  - non-generic functions;
  - generic functions with an expected function type (E0412 otherwise);
  - associated functions (`T.f`, `m.T.f`);
  - functions of another module (`m.f`, with visibility E0313).

  Methods with `self`, externs and intrinsics stay E0406, with help: wrap a method as `fun(c) = c.area()` once closures land in milestone 15, or call it directly.
- **Calls:**
  - `f(…)` where `f` is a local of function type calls the value; a local shadows a function of the same name, as today.
  - Any other callee expression of function type is called the same way.
  - A callee that isn't a function is still E0406.
- **Equality:**
  - A function type is not `Eq`, and neither is any struct, enum, array or `Option` that holds one, looking through fields and payloads with a guard for recursive types.
  - `==` on one is E0404, and passing one where `T: Eq` is wanted is E0419.
- **Elsewhere:** function types can't be printed or interpolated (E0403) or cast (E0408). In an extern signature they are E0409.
- **Layout:**
  - A function value is an LLVM `{ ptr, ptr }`: code, then environment. It is 16 bytes with alignment 8 in arrays, fields and payloads, and passed by value.
  - Copying a function value copies the pair.
- **Code:** a named function's value is `{ @lugha_ref_<symbol>, null }`. The `lugha_ref_` wrapper takes the environment pointer first, then the function's parameters, and calls `lugha_fn_<symbol>`. Wrappers are emitted on first use. A generic instance gets its own (`lugha_ref_max<i64>`), and the instance pass lists generic functions used as values.
- **Calling a value:** load the code and environment, then make an indirect call with the environment first. Struct, enum and `Option` arguments go by pointer, as in direct calls. The call takes the function type's signature.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Syntax**
- The type grammar gains `"fun" "(" [ type { "," type } [ "," ] ] ")" [ ":" type ]` and `"(" type ")"`. `TypeKind::Fun(Vec<Type>, Option<Box<Type>>)` is the node; parentheses leave no node.
- S-expressions print `(fun-type (i64 i64) bool)`.

**Checker**
- `Type::Fun`:
  - display with parentheses inside arrays;
  - `fits` by equality;
  - resolution of written function types.
- **Function names as values,** in expression position:
  - `Name` that isn't a local, `m.f`, `T.f` and `m.T.f`;
  - the callee key is recorded for codegen (`Checked.callees`), and type arguments in `Checked.instances`;
  - inference of a generic one from the expected type;
  - E0406 with help for methods, externs and intrinsics.
- **Calls of values:**
  - a local of function type;
  - any callee expression of function type;
  - `s.f(x)` on a struct field of function type when there's no method `f`.

  Arity (E0405) and argument (E0403/E0401) checks follow the function type.
- **Equality:** `eq_ok` is false for function types and for anything that holds one (E0404, E0419), with the recursion guard.
- **Errors elsewhere:** printing, interpolating or `to_string` of a function is E0403; casting one is E0408; one in an extern signature is E0409.
- **Instances:** a generic function used as a value is a use, so the instance pass lists it.
- **Liveness:** a function name used as a value is no local, so nothing changes.

**Codegen**
- `llvm_type(Fun)` is the `{ ptr, ptr }` struct, with element size 16 and alignment 8. The layout and indirect-slot code treat it as a plain value.
- **Wrappers:**
  - `lugha_ref_<symbol>` for each function used as a value, on demand, including instances (`lugha_ref_max<i64>`) and associated functions (`lugha_ref_Circle.make`);
  - the value built from it is `{ wrapper, null }`.
- **Calls of a value:** an indirect call with the environment first; aggregates by pointer.
- **Copies:** a function value is plain data, so the deep-copy helpers skip it (no array inside).

**Spec** — `Language v3 Specification.md`
- **The draft's text, folded in:**
  - §3: function types and parenthesized types;
  - §4: "Function types", "Functions as values", "Calls", including the field-call rule;
  - §7: the function-value layout;
  - §8: function types in externs (E0409);
  - §9: `lugha_ref_` in the symbol table, calling a function value, and the reused codes;
  - §10: the functions-as-values program;
  - §11: milestone 14 done.
- **Changes since v2:** gains the function-values item; the title becomes "in progress: milestone 14".
- **The draft's §3 note on `(s.f)(x)`** is amended to discovery answer 2.
- **CLAUDE.md:** architecture rule 7 gains `lugha_ref_`, plus the tree.
- README, TODO, CHANGELOG, MEMORY decision 46, CONTEXT.

### Must NOT Do
- **No closures** (milestone 15): `fun(x) = …` literals, captures, parameter inference for literals. Also no array helpers (milestone 16).
- **No method values** (`c.area` without a call) and no extern or intrinsic values.
- **No `==`, printing or ordering** of functions.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **Records:** every user error is a `Diagnostic` from the existing codes (E0403, E0404, E0405, E0406, E0408, E0409, E0412, E0419), and checking continues.
- **Codegen invariants:** a function value whose wrapper or instance is missing is a compiler bug (an `expect` stating the invariant, exit 2).

## SECURITY CONSIDERATIONS
- **Calls:** indirect calls only ever target wrappers the compiler emitted. Function values can't be forged, since they have no casts, no `extern` use, and no arithmetic.
- No `unsafe`, no new inputs.

## TESTS TO WRITE

Unit tests — parser:
- [x] `fun(i64, i64): bool`, `fun(i64)`, `fun(): fun(i64): i64` (a function returning a function), `fun(i64): i64[]` (returning an array) and `(fun(i64): i64)[]` (an array of functions) parse and print.

Unit tests — checker (a new `check/fun_values.rs`):
- [x] `let f = double;` is `fun(i64): i64`; `f(3)` is `i64`; `apply(double, 3)` type-checks.
- [x] Values from elsewhere: `Circle.make` is a value; `m.f` from an import is a value; a `private` one from another module is E0313.
- [x] Generic: `let m: fun(i64, i64): i64 = max;` works; `let m = max;` is E0412.
- [x] E0406 with help for a method with `self` (`Circle.area`), an extern, `println`, and for calling an `i64`.
- [x] Calls of values:
  - `fs[0](3)` and `make()(3)` work;
  - `b.on_click(2)` calls a field when no method has that name;
  - wrong arity is E0405 and wrong argument types are E0403.
- [x] A local of function type shadows a function of the same name.
- [x] No `==` (E0404) on functions, on a struct with a function field, or on `(fun(i64): i64)[]`; `contains(fs, double)` with `T: Eq` is E0419.
- [x] `println(f)` and `"{f}"` are E0403; `f as i64` is E0408; `extern fun g(f: fun(i64))` is E0409.
- [x] Messages show `fun(i64): i64` and `(fun(i64): i64)[]`.

Codegen / IR (`tests/codegen.rs` or unit):
- [x] `lugha_ref_double` is defined once, and a value is `{ ptr @lugha_ref_double, ptr null }`.
- [x] A generic instance used as a value gets `lugha_ref_max<i64>`.

Acceptance — `tests/programs/m14/` (run at `-O0` and `-O2`):
- [x] `values.la`: functions in locals, arrays, struct fields (`b.on_click(2)`), `Option` and parameters; returning a function; `make()(3)`.
- [x] `generic.la`: `let m: fun(i64, i64): i64 = max;` and a generic `apply<T>(f: fun(T): T, x: T)` at two types.
- [x] `modules/`: a function of another module and an associated function passed as values.
- [x] `e0406.la`, `e0404.la`: reject goldens.
- [x] The §10 functions-as-values program runs from the spec.

Regression:
- [x] Every earlier test passes; programs without function values give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-039-function_values`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 functions-as-values program and the `m14/` programs pass.
- [x] Every listed error is reported as specified.
- [x] The spec is folded (in progress: milestone 14); CLAUDE.md, README, TODO, CHANGELOG and MEMORY are updated; tag `m14`.
- [x] No file over 300 lines (splits proposed first); no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m14/values.la
```

## AMENDMENTS (during build, session 49)

- **A v2 test changed:** `let h = g;` (a function name as a value) was asserted to be E0406 in `check/call.rs`. It now asserts the new `fun(): …` type, as this PRP intends.
- **E0406 wording:** "`x` can't be used as a value", with a help for each case (method, extern, intrinsic).
- **No new file splits:**
  - `holds_fun` lives in `check/bounds.rs`, beside `eq_ok` (it is new code);
  - `env.rs` gained one line (the extern arm);
  - `copy.rs` is unchanged: a function type's copy-helper name uses its printed form.
- **Signatures:** `Signature` gained `is_extern`. Codegen's `arguments()` takes `(params, is_extern)`, so value calls can use it.
