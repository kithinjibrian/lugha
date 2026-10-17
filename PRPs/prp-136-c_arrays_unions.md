## FEATURE: Arrays and unions at the C boundary — `T[]` as a pointer to its elements for the call, and `extern union`; milestone 104 (v17).

**Status:** proposed in session 250 with PRP-135 and PRP-137 (lugha-wasmtime's needs 3, 6 and 8).

## OBJECTIVE
```lugha
extern union ValUnion { i32: i32, i64: i64, f32: f32, f64: f64 }
extern struct Val { kind: u8, of: ValUnion }
extern fun wasmtime_module_new(e: Engine, wasm: u8[], len: u64, out ret: Option<Module>): Option<WasmError>;
extern fun wasmtime_func_call(c: Context, ptr f: Func, args: Val[], nargs: u64,
                              mut results: Val[], nresults: u64, out trap: Option<Trap>): Option<WasmError>;

let mut results = [Val { kind: 0, of: ValUnion { i64: 0 } }; 1];
wasmtime_func_call(ctx, ptr f, args, args.len as u64, mut results, 1, out trap);
println(results[0].of.i64);
```

## CONTEXT
- Today arrays are E0409 at the C boundary. A Lugha array is a pointer to `{ len, cap, elements… }` (spec §7), elements in element layout (`bool` as a byte, extern structs by value in C layout), so the elements start 16 bytes in.
- Related: `src/check/env.rs` (E0409 checks), `src/codegen/{callbacks,out_params,heap,cow}.rs`, `src/parser/{item,decl}.rs`, `src/check/structs.rs` (extern structs, milestone 90).

## DECISIONS (draft decisions 93–99)
93. **`xs: T[]`** on an extern parameter, `T` a number, `bool`, a handle or an extern struct (not `string`, arrays, Lugha structs: E0409): C gets a pointer to the first element (`T *` / `const T *`), valid during the call; the length goes as an ordinary argument, as C's API spells it. An empty array passes a valid pointer.
94. **`mut xs: T[]`**: C may write the elements in place. The call site writes `mut place`; the array is made unique first (copy-on-write) so no other copy sees C's writes; the length never changes.
95. **Arrays of handles** (`wasm_valtype_t *const data[]`) are `T[]` of a handle: a pointer to the handles.
96. **`extern union Name { field: T, … }`**: C's union, with C's size and alignment; fields are numbers, `bool`, handles, `Option` of a handle and extern structs/unions. A literal writes exactly one field (`ValUnion { i64: 0 }`; another count is E0444, new); reading any field reads the bytes as that field's type, as C does. Unions go anywhere extern structs go (fields, `out`/`ptr`/`mut`, arrays, by value); not `==`, not printable (E0444 for both), since only C knows which field is live.
97. **Not arrays from C:** a C function can't return or `out` an array (it has no Lugha header); C memory is PRP-137's.
98. **Callbacks** don't take arrays yet (PRP-137).
99. **Success:** milestone 104, tag `m104`.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- Checker: arrays allowed for extern parameters per 93; `mut` mode for arrays; `extern union` declarations (parser, checker, layout), literals with one field, field reads, E0444.
- Codegen: element pointer = array + 16 (after `own` for `mut`); union types as `[N x i8]` with C's alignment (an LLVM struct of the most aligned member padded to the size) and field access by bitcast; unions in extern-struct layout and the x86-64 classification (milestone 90's `c_abi.rs`).
- Spec §4 (unions), §8 (arrays, unions), row 104; E0444's page; VS Code grammar `union` after `extern`.
### Must NOT Do
- Pointer arithmetic or keeping the array pointer after the call.
### New dependencies
- none

## ERROR HANDLING REQUIREMENTS
- E0409 (existing) for arrays of disallowed elements; E0444 (new) for union literals with other than one field, and `==`/printing of unions.

## SECURITY CONSIDERATIONS
- C may write past the elements if the length passed is wrong — the same trust as any extern; documented. No new unsafe Lugha surface.

## TESTS TO WRITE
- [ ] Checker/parser units: arrays of each allowed element kind, E0409 for `string[]`; unions: declaration, literal, E0444 cases.
- [ ] `tests/c_arrays.rs` (C via `[link]`): C sums a `u8[]` and an `i64[]`; C fills a `mut` array of extern structs; copy-on-write: a copy made before `mut` keeps its old elements; an array of handles; a union written by Lugha and read by C and back; a struct holding a union by value and by `out`.

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] lugha-wasmtime's `wasmtime_module_new` (bytes) and `wasmtime_func_call` (value arrays) shapes work against a C stand-in; narrow tests, fmt, clippy; CHANGELOG.

## VALIDATION
- `cargo test --lib -- union c_array`, `cargo test --test c_arrays --test out_params --test extern_structs`, `npm test`, fmt, clippy
