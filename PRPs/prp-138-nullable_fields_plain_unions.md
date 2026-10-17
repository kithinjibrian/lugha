## FEATURE: Nullable handle fields and plain-data unions — `Option<H>` in extern structs and unions, and unions that can't hold pointers; milestone 106 (v17).

**Status:** proposed 2026-10-17 (session 252), after lugha-wasmtime's probe (an extern struct can't start a pointer field as NULL) and the union hole found in session 251 (a union turns a number into a `CPtr`).

## OBJECTIVE
```lugha
extern type Valtype;
extern struct Slot { size: u64, ty: Option<Valtype> }   // C: wasm_valtype_t *ty, may be NULL

let mut s = Slot { size: 0, ty: None };                  // None is C's NULL
fill_slot(out s);
if let Some(t) = s.ty { use_type(t); }

extern union Forge { n: u64, p: CPtr }                    // error: E0437 — a union holds plain data only
```
An extern struct can hold a pointer C may leave NULL, written as an `Option` of a handle, and start it as `None`. A union can no longer make a pointer out of a number: no Lugha program without C code can read memory it doesn't own.

## CONTEXT
- Today: `Option<H>` is allowed for extern parameters, results and `out` (NULL ↔ `None`, `codegen/externs.rs`) but is E0437 as an extern-struct field. Unions (milestone 104) take any extern-struct field type, including handles and `CPtr`, so `Forge { n: 4096 }.p` is a `CPtr` to address 4096 and `c_read` on it crashed a program with no C code (session 251, exit 139).
- `uint8_t *` fields already work as `CPtr` with `CPtr.null` (milestone 105); this PRP is for typed pointers (`wasm_valtype_t *`).
- Related: `src/check/structs.rs` (extern-struct fields, unions), `src/check/handles.rs`, `src/check/errors/v16.rs` (E0437), `src/check/layouts.rs` (`UNION_MARK`), `src/codegen/{structs,externs,c_abi,c_calls}.rs`, spec v17 §8, PRP-120 (milestone 90), PRP-136.
- Open decisions: none.

## DECISIONS (draft decisions 107–113)
107. **`Option<H>` fields:** an extern struct may hold `Option<H>`, `H` a handle (`extern type` or `CPtr`): laid out as one pointer, NULL is `None`. Literals write `None` or `Some(h)`; reading the field gives the `Option`; `out`/`mut`/`ptr`, arrays and by-value passing (classified as a pointer, INTEGER) work as for a handle field.
108. **Plain handle fields stay as they are:** they have no NULL, so a struct with one needs a handle from C to start; declare a pointer C may leave NULL as `Option<H>`. No zeroed-struct syntax: it would put NULL in fields whose type says they can't be.
109. **Unions hold plain data:** a union's fields are numbers, `bool`, `char`, and extern structs and unions made only of those (checked through every level). A handle, `Option<H>` or `CPtr` in a union, directly or nested, is E0437 with its own message ("a union can't hold a pointer: reading it from another field's bytes would make one up").
110. **lugha-wasmtime's `wasmtime_valunion_t`:** its numeric fields are plain; its reference fields (`wasmtime_func_t`, `externref`) declare their private pointer as `u64`, which is safe for Lugha (only C ever follows it) and as trusted as any extern declaration.
111. **Spec §8** says plainly that `c_read`/`c_write` are where memory safety becomes the program's: offset and length are checked only against what C says its buffer's size is.
112. **Breaking:** a union holding a handle or `CPtr` (allowed since milestone 104, same day) is now E0437.
113. **Success:** milestone 106, tag `m106`.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- Checker: `Option<H>` accepted as an extern-struct and union-member-struct field type (not a union's own field: decision 109); the union field rule (recursive) with E0437's union message; E0437's page gains the union case and the `Option<H>` field.
- Codegen: `Option<H>` fields stored as a nullable pointer in C layout (reuse `externs.rs`'s conversion), read back as `Option`; by-value classification as a pointer.
- Spec §8 (fields, unions, the `c_read`/`c_write` sentence), row 106; CHANGELOG; draft decisions.
### Must NOT Do
- A zeroed-struct or "uninitialised" syntax (decision 108); nullable plain handles; any change to `c_read`/`c_write` themselves.
### New dependencies
- none

## ERROR HANDLING REQUIREMENTS
- E0437 (existing): a union field that is or holds a handle, `Option<H>` or `CPtr`; still for `string`, Lugha structs and other disallowed fields.

## SECURITY CONSIDERATIONS
- Closes the union hole: with this, a non-null `CPtr` or handle comes only from C (results, `out`/`mut`, callback parameters, fields C filled). `c_read`/`c_write` remain trusted copies (PRP-137, decision 102).

## TESTS TO WRITE
- [ ] Checker units: `Option<H>` field accepted (extern type, `CPtr`); a union with a handle, `Option<H>`, `CPtr`, and a struct holding one (nested) each E0437; a union of numbers and a plain struct accepted.
- [ ] `tests/extern_structs.rs`: a struct with an `Option<H>` field built with `None` and `Some`, passed by value and through `ptr`; C fills it through `out` with NULL and non-NULL; an array of such structs.
- [ ] `tests/c_arrays.rs`: the session-251 forge program is rejected by `lughac check` (E0437).
- [ ] `tests/explain.rs` (existing): E0437's page examples.

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] lugha-wasmtime declares `wasm_byte_vec_t` with `CPtr` and typed NULL-able pointer fields with `Option<H>`, no `u64` stand-ins except decision 110's private pointers.
- [ ] The forge program doesn't compile.
- [ ] Narrow tests, fmt, clippy; CHANGELOG.

## VALIDATION
- `cargo test --lib -- extern_struct union handles`, `cargo test --test extern_structs --test c_arrays --test explain`, fmt, clippy
