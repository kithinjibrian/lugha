## FEATURE: `ptr` and `mut` parameters on externs — C reads a value by address (`const T *`), or reads and changes it (`T *`); milestone 103 (v17).

**Status:** approved 2026-10-17 ("build all in one go", session 251; the user's security sign-off for PRP-137 included); proposed in session 250 (the user: "let's knock them out" — lugha-wasmtime's remaining needs), with PRP-136 and PRP-137. Decisions follow `out` (PRP-134) and the recommendations already discussed.

## OBJECTIVE
```lugha
extern struct Instance { store_id: u64, hidden: u64 }
extern fun wasmtime_instance_export_get(c: Context, ptr i: Instance, name: string, len: u64, out item: Extern): bool;
extern fun bump(mut counter: u64);

wasmtime_instance_export_get(ctx, ptr inst, "run", 3, out item);
bump(mut n);
```

## CONTEXT
- Builds directly on PRP-134: `Param.out` becomes a mode (`out`, `ptr`, `mut`), `ExprKind::Out` becomes `ExprKind::Pass(mode, place)`, `CParam::Out` becomes `CParam::Pointer(mode)`; the slot and write-back code (`codegen/out_params.rs`) gains the initialised-slot case.
- Related: `src/{ast,parser,check,codegen}/out_params.rs`, `tests/out_params.rs`, spec v17 §8.

## DECISIONS (draft decisions 88–92)
88. **`ptr name: T`** (C's `const T *`): the call writes `ptr e`, where `e` is any expression of type `T` (not only a place: C only reads); C gets the address of a slot holding a copy. Nothing is written back.
89. **`mut name: T`** (C's `T *`, read and written): the call writes `mut place`; C gets the address of a slot holding the place's current value; after the call the slot is stored into the place, as for `out` (assignment rules; E0501 etc.).
90. **Types:** as `out` (numbers, `bool`, floats, handles, `Option` of a handle, extern structs); `string` is E0409.
91. **Call site:** the mode is written at the call as at the declaration; a missing, extra or different mode is E0443 (its message names the expected one).
92. **Success:** milestone 103, tag `m103`.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- Parser: `ptr`/`mut`/`out` before an extern parameter and a call argument (contextual before a name or, for `ptr`, any expression start that isn't `)`/`,`); AST `PassMode { Out, Ptr, Mut }`; formatter; s-expressions `(ptr e)`, `(mut x)`.
- Checker: modes in the plan; E0443 per mode; `ptr` checks its expression against `T`; `mut`/`out` check a place.
- Codegen: `ptr`/`mut` slots initialised from the value (C layout: `bool` byte, `Option` handle pointer); `mut` writes back like `out`.
- Spec §3, §8, row 103; E0443's page mentions all three; VS Code grammar `ptr`/`mut` in those positions (bump).
### Must NOT Do
- Arrays (PRP-136), raw memory (PRP-137).
### New dependencies
- none

## ERROR HANDLING REQUIREMENTS
- E0443 (existing, widened), E0409, E0501/E0502 (existing).

## SECURITY CONSIDERATIONS
- As PRP-134: stack slots for the call only; a wrong declaration is undefined behaviour as for any extern.

## TESTS TO WRITE
- [ ] Parser/checker/formatter units for `ptr` and `mut` (E0443 each way, `ptr` on a non-place expression OK, `mut` on a `let` E0501).
- [ ] `tests/out_params.rs` (renamed scope: pointer parameters): C reads a struct through `ptr`, C bumps a counter through `mut`, a `mut` extern struct changed in place, `bool` both ways.
- [ ] VS Code grammar.

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] lugha-wasmtime's `wasmtime_instance_export_get` shape works against a C stand-in; narrow tests, fmt, clippy pass; CHANGELOG.

## VALIDATION
- `cargo test --lib -- out ptr`, `cargo test --test out_params --test callbacks --test extern_structs`, `npm test`, fmt, clippy
