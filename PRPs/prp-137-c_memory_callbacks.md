## FEATURE: C memory and pointer callbacks — a `CPtr` type with copy-only access, and callbacks that receive arrays and `mut` arrays; milestone 105 (v17).

**Status:** approved 2026-10-17 ("build all in one go", session 251; the user's security sign-off for PRP-137 included); proposed in session 250 with PRP-135 and PRP-136 (lugha-wasmtime's needs 4, 5 and 7). **Needs the user's security sign-off** (decision 102): `c_read`/`c_write` trust a pointer and length from C.

## OBJECTIVE
```lugha
extern struct ByteVec { size: u64, data: CPtr }          // wasm_byte_vec_t, wasm_name_t, wasm_message_t
extern fun wasmtime_error_message(e: WasmError, out msg: ByteVec);
extern fun wasmtime_memory_data(c: Context, ptr m: Memory): CPtr;

let mut msg = ByteVec { size: 0, data: CPtr.null };
wasmtime_error_message(err, out msg);
let text = string_from_utf8(c_read(msg.data, 0, msg.size));

c_write(wasmtime_memory_data(ctx, ptr mem), offset, chunk);   // after checking against data_size in Lugha

// A host function: C passes (args, nargs) and (results, nresults).
fun add(env: data, caller: Caller, args: Val[] nargs, mut results: Val[] nresults): Option<Trap> {
    results[0] = Val { kind: 1, of: ValUnion { i64: args[0].of.i64 + args[1].of.i64 } };
    None
}
```

## CONTEXT
- Builds on PRP-134 (pointer modes), PRP-136 (arrays, unions) and v16's callbacks (`data`, trampolines, `callbacks.rs`).

## DECISIONS (draft decisions 100–106)
100. **`CPtr`**: a built-in opaque type for an untyped C pointer (`void *`, `uint8_t *`), like a handle without a name: in extern parameters, results, `out`, and extern-struct/union fields; `CPtr.null`; `p.is_null()`; `==`; prints as `<cptr>`. No arithmetic.
101. **`c_read(p: CPtr, offset: u64, len: u64): u8[]`** copies `len` bytes at `p + offset` into a new array; **`c_write(p: CPtr, offset: u64, bytes: u8[])`** copies the array's bytes there. A null `p` panics (`NULL CPtr`); nothing else is checked: C's pointer and length are trusted.
102. **Security:** `c_read`/`c_write` are the only new memory-unsafe operations; they copy only, never keep or hand out the pointer as an array, and the spec says to bounds-check against C's size first (as lugha-wasmtime does with `data_size`). Requires the user's sign-off before building.
103. **Callback array parameters:** in a callback type, `xs: T[] n` names the C parameter holding the count (`const T *args, size_t nargs`): Lugha's callback receives a copy of the `n` elements as `T[]`. `mut xs: T[] n` (`T *results, size_t nresults`): the callback receives an array of `n` elements copied in, and when it returns, its elements are copied back to C (its length must still be `n`; a different length panics). The count parameter is C's `size_t`/`u64` and is not written separately in Lugha.
104. **Callback results:** callbacks may return `Option` of a handle (already allowed) — the trap.
105. **Not:** pointer arithmetic, keeping `CPtr`-backed arrays, `CPtr` to Lugha memory.
106. **Success:** milestone 105, tag `m105`; lugha-wasmtime's need list is covered.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- `CPtr` type (checker built-in like `Ref`'s predeclaration; codegen `ptr`), `CPtr.null`, `is_null`, `==`, printing; `c_read`/`c_write` intrinsics with runtime helpers (`lugha_rt_c_read`/`_write`: memcpy, the null panic) in a new `runtime/cmem.c` fragment.
- Callback types with `T[] n` and `mut T[] n` parameters: trampolines/static entries build the array from (pointer, count) and copy `mut` arrays back.
- Spec §8 (CPtr, c_read/c_write, callback arrays), row 105; VS Code grammar for `CPtr`.
### Must NOT Do
- Anything in decision 105.
### New dependencies
- none

## BUILT (session 251)
- Deviations: no `p.is_null()` — compare `p == CPtr.null`; `CPtr` prints like any handle. Callback arrays are written `xs: T[]` (C's `T *, size_t` as two C parameters) and `results: Ref<T[]>` for the written array, not `mut xs: T[] n`; the copy back is `min(len, count)` elements, not a panic on a changed length.
- Runtime: `runtime/cmem.c` (`lugha_rt_c_read`, `lugha_rt_c_write`; NULL panics `NULL CPtr given to c_read`/`c_write`).
- Tests: `tests/c_memory.rs` (a C-filled `ByteVec` read with `c_read`, `c_write` into a C buffer, a host callback summing `Val[]` and writing `Ref<Val[]>`, the NULL panic exit 101).

## ERROR HANDLING REQUIREMENTS
- E0409 for `CPtr` outside the C boundary positions; E0435 (existing callback-declaration code) for a malformed `T[] n` (missing count, count not an integer).
- Runtime: `panic: NULL CPtr` for `c_read`/`c_write` on null; a `mut` callback array whose length changed panics.

## SECURITY CONSIDERATIONS
- See decision 102: raw copies from and to C memory the program can't verify; documented, copy-only, user sign-off required. `runtime/` gains `cmem.c` (no process, file or network effects).

## TESTS TO WRITE
- [ ] Checker units: `CPtr` positions, `c_read`/`c_write` types, callback `T[] n` declarations and E0435 cases.
- [ ] `tests/c_memory.rs` (C via `[link]`): a C-filled `ByteVec` read with `c_read` and freed by C; `c_write` into a C buffer C then sums; null panics (exit 101); a host callback summing `args` and writing `results`, called from C; a `mut` callback array whose length changed panics.

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] lugha-wasmtime's message reading, guest memory copies and host functions work against C stand-ins; narrow tests, fmt, clippy; CHANGELOG.

## VALIDATION
- `cargo test --lib -- cptr callback`, `cargo test --test c_memory --test callbacks --test c_arrays`, `npm test`, fmt, clippy
