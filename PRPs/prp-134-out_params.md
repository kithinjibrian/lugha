## FEATURE: `out` parameters on externs — C fills a value through a pointer and Lugha gets it back after the call; milestone 102 (v17).

**Status:** approved 2026-10-17 ("approved", session 249); proposed in session 248, after a discovery of four questions answered by the user. Asked for by lugha-wasmtime (its DECISION-005, need list items 1, 2 and part of 6).

## OBJECTIVE
A Lugha binding can call C functions that return values through pointers:

```lugha
extern type Context;
extern type Module;
extern type WasmError;
extern fun wasmtime_context_get_fuel(c: Context, out fuel: u64): Option<WasmError>;
extern fun wasmtime_module_new(e: Engine, wasm: string, len: u64, out ret: Module): Option<WasmError>;

let mut fuel: u64 = 0;
let err = wasmtime_context_get_fuel(ctx, out fuel);
```

## CONTEXT
- Starting state: externs pass numbers, `bool`, `string`, handles (`extern type`), callbacks and `extern struct` by value (v16, milestones 87–90; spec v17 §8). No parameter is passed by address.
- Ending state: `out` in extern parameter lists and at call sites; checker and codegen as below; spec §3 grammar, §8, §11 row 102; E0443's page.
- Related existing code: `src/parser/{item,primary}.rs` (extern declarations, call arguments), `src/ast` (`Param`, call arguments), `src/check/{callbacks,arguments,assign}.rs` (extern signatures, argument checking, places), `src/codegen/{externs,c_calls,places,function}.rs` (calls into C, `Option` of a handle ↔ NULL, extern struct layout, writing places).
- Not in scope (later milestones, same machinery): `ptr x` for `const T *`, in-out `mut x`, arrays across the boundary, unions, raw C memory, callbacks with pointer parameters.

## DECISIONS (session 248; draft decisions 81–87)
81. **Declaration:** `out name: T` in an `extern fun`'s parameter list marks a pointer parameter C writes through (`T *` in C). `out` is contextual: a keyword only before a parameter of an `extern fun` and before a call argument. `out` on a Lugha function's parameter is E0201.
82. **Call site (Q1):** the argument is written `out place`. A missing `out` for an `out` parameter, or `out` for any other parameter, is E0443 (new), whose help shows the right form.
83. **The place (Q2):** anything assignable — a `let mut` variable, a field, an element — with the same rules and errors as `place = …` (E0501, E0502, E0506, iteration E0507). C receives a pointer to a zeroed slot: the place's old value is not passed. After C returns, the slot's value is stored into the place, as an assignment would (copy rules, `for … of` guards, copy-on-write ownership).
84. **Types (Q3):** integers and `bool` (C's integer and `bool`), `f32`/`f64`, handles (`T **` in C; NULL is a panic `NULL from C` at the call, as for a handle result), `Option` of a handle (NULL → `None`), and `extern struct`s (C fills the struct). Strings, Lugha structs, arrays, enums and functions are E0409 for an `out` parameter, as for other extern positions.
85. **Order:** arguments are evaluated left to right as now; an `out` argument's place is fixed (its indexes evaluated) in its turn; the write-backs happen after the call, left to right.
86. **Scope (Q4):** only `out`; `ptr` (`const T *`) and in-out `mut` are the next milestone.
87. **Success:** milestone 102, tag `m102`; lugha-wasmtime can create modules, read fuel and trap codes.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- Parser: `out` before an extern parameter (AST `Param` gains `out: Option<Span>`) and before a call argument (an argument wrapper, `ExprKind::Out(place)` or an argument flag); s-expressions; formatter keeps `out`.
- Checker: extern signatures record which parameters are `out` (E0409 for a disallowed type); calls match `out` to `out` (E0443); the place is checked as an assignment target of the parameter's type; `out` arguments only in calls of externs (E0443 elsewhere).
- Codegen: per `out` argument, an entry-block slot of the C type, zeroed; its address passed; after the call, the value read (handle NULL → panic or `None`; extern struct by its C layout) and stored into the place through the existing place-writing path.
- Spec v17 §3 (grammar: `param = [ "out" ] IDENT ":" type` in extern declarations; `arg = [ "out" ] expr`), §8 (out parameters), §11 row 102, changes list; draft; `docs/errors/E0443.md`; VS Code grammar keyword `out` (in those positions).
- Tests (below), with a C file through `[link]` as `tests/callbacks.rs` and `tests/extern_structs.rs` do.

### Must NOT Do
- `ptr`, in-out `mut`, arrays, unions, raw pointers, `out` in callback types (later milestones).
- Let C keep the pointer after the call (it is a stack slot; documented, as for other extern arguments).

### New dependencies
- none

## ERROR HANDLING REQUIREMENTS
- E0201 (existing): `out` on a Lugha function's parameter, or in an unexpected place.
- E0409 (existing): a type not allowed as `out`.
- E0443 (new): `out` missing for an `out` parameter, or given for another parameter or in a call of a non-extern.
- E0501/E0502/E0506/E0507 (existing): the place's assignment rules.
- Runtime: `panic: NULL from C` at the call for a non-`Option` handle C left NULL.

## SECURITY CONSIDERATIONS
- C writes into a stack slot sized for the declared C type, during the call only. A wrong declaration is undefined behaviour, as any wrong extern declaration already is (spec §7–8); no new `unsafe` surface in Lugha code. No change to how `cc`/`pkg-config` run.

## TESTS TO WRITE
- [ ] Parser: `out` in extern parameters and call arguments; `out` on a Lugha function's parameter (E0201); round-trip through the formatter.
- [ ] Checker: matching `out` (E0443 both ways and for non-externs); place rules (E0501 on `let`, E0502 on a non-place); types (E0409 for `string`, a Lugha struct, an array); field and element places.
- [ ] `tests/out_params.rs` (lughac + a C file via `[link]`): a `uint64_t *` and a `uint8_t *` filled; a `T **` handle; `Option<T>` NULL → `None`; a non-`Option` NULL panics `NULL from C` (exit 101); an extern struct filled; fields and elements as places; write-back order with two `out` arguments; the old value isn't passed (C sees zero).
- [ ] VS Code grammar: `out` highlighted as a keyword in an extern parameter and an argument.

## ROLLBACK PLAN
- Branch to return to: `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] lugha-wasmtime's `wasmtime_context_get_fuel`, `wasmtime_trap_code` and `wasmtime_module_new` shapes work in tests against a C stand-in.
- [ ] Narrow targeted tests (the parser/checker filters above, `out_params`, `callbacks`, `extern_structs`, the formatter and grammar tests), fmt, clippy pass; CHANGELOG updated.

## VALIDATION
- `cargo test --lib -- out`, `cargo test --test out_params --test callbacks --test extern_structs`, `cd editors/vscode && npm test`, `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`
