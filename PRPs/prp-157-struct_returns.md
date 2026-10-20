## FEATURE: Lugha functions return structs, enums, `Option`s and tuples through a hidden result pointer instead of as LLVM values — step 1 of "aggregates through memory"; milestone 125 (v17).

**Status:** proposed 2026-10-20 (session 297). Cause, from session 296's measurements: at `-O0`, LLVM's fast instruction selector can't handle first-class aggregates, so 538 of 571 functions of kivuli's `examples/backgrounds.la` fall back to the DAG selector, 90% of `llc`'s time; those run at 161 µs per instruction against 4.6 µs on the fast path (estimate: ~7.5 s of instruction selection → ~0.2–1 s once aggregates go through memory). 280 of the 538 fall back *for the whole function* because they return (or take) an aggregate by value. Discovery: (1) staged — this PRP is returns only, measured, before whole-value loads/stores/`insertvalue`/`extractvalue` inside bodies (a later PRP, only if this one shows the gain); (2) done = behaviour unchanged, fallback-for-arguments gone, timings recorded, no fixed threshold; (3) PRP-156 parked on its branch, re-measured after this work.

## RESULT (session 298)
Built as one pass over the lowered module (`codegen/sret.rs`) rather than in each of the ~20 places that define or call functions; the old function is left as an unused, unnamed declaration because deleting a function is `unsafe` in inkwell. Full `cargo test` passes. Measured on kivuli, same machine and load (decision 291):

| | before | after |
| --- | --- | --- |
| `lughac build examples/backgrounds.la` | 12.42 s | 10.01 s |
| `llc -O0` total / DAG instruction selection | 9.55 s / 8.39 s | 7.77 s / 6.68 s |
| `lughac test test/diff.la` | 12.80 s | 10.68 s |
| fast-isel misses: `ret` / `store` / `load` of aggregates | 430 / 829 / 364 | 0 / 1253 / 370 |

The acceptance criterion "no *didn't lower all arguments*" was a misreading (session 298): that remark only moves the entry block's argument lowering to the DAG selector, and `sret`, `i1` and struct arguments all trigger it; dropping `sret` changes nothing measurable (8.18 s vs 8.09 s). Blocks fall back per instruction: each struct `ret` became a struct `store` into the slot, so the same blocks still fall back. The remaining gain is step 2 (whole-value `store`/`load`/`insertvalue`/`extractvalue` through memory).

## OBJECTIVE
No change a program can see. In the IR, `define %P @lugha_fn_f(...)` becomes `define void @lugha_fn_f(ptr sret(%P) %out, ...)`, and every caller passes the address of a slot of its own. On kivuli's `examples/backgrounds.la`, `llc -O0 -fast-isel-report-on-fallback -pass-remarks-missed=sdagisel` reports no "didn't lower all arguments" (280 today).

## CONTEXT
- `codegen/declare.rs` (signatures: `llvm_type(ret).fn_type`, line ~101; externs already through `c_calls.rs` with `sret`/`byval`, unchanged), `value.rs` (`is_aggregate`: struct, enum, `Option`; tuples are `$tuple<n>` structs).
- Parameters: aggregates already go as a pointer to a caller-side copy (`function.rs` `arguments`); only results change.
- Every Lugha-convention function and every caller: `function.rs` (bodies, `return`, tails, direct calls), `methods.rs`, `fun_values.rs` (`lugha_ref_` wrappers, indirect calls of `{code, env}`), `closures.rs`, `dispatch.rs`/`interfaces.rs`/`upcast.rs` (`lugha_vt_` tables, box calls), `defaults.rs`, `operators.rs`, `instances.rs`, `tasks.rs` (`lugha_task_` entries storing a `Task<T>` result), `actors.rs` (`lugha_actor_` handlers, call replies), `callbacks.rs` (`lugha_cb_`/`lugha_cfn_` entries: C-facing side unchanged, their call into Lugha changes), `copy.rs` (`lugha_copy_<type>` helpers returning a struct), `iterators.rs` (`next` returning `Option<T>`), `results.rs` (`?`), `test_main.rs` (tests return `()`, unaffected).
- Spec v17 §9 (codegen and lowering; the symbol table is unchanged). Architecture rule 7 (symbols) unchanged; rule 8 (allocation only through `lugha_rt_alloc`) — result slots are stack `alloca`s in the caller's entry block, no allocation.
- Open decisions: none.

## DECISIONS (draft decisions 285–292)
285. **Convention**: a Lugha function (or method, closure body, wrapper, table entry, default instance, task entry, actor handler, copy helper) whose result type is a struct, enum, `Option` or tuple is declared `void` with a first parameter `ptr sret(<T>) noalias` (before a method's receiver and a closure's environment); it stores its result there and returns. Every other result is unchanged.
286. **Callers** pass the address of an `alloca` in their entry block (one per call site; LLVM reuses nothing at `-O0`, which is fine) and load the result from it, so the rest of codegen still sees a value. Removing those loads is step 2.
287. **Indirect calls** (function values, closures, interface tables, task and actor entries) use the same convention, so one Lugha function type has one LLVM signature.
288. **C stays C**: externs and the entries C calls (`lugha_cb_`, `lugha_cfn_`, `main`) keep the System V lowering of `c_calls.rs`; only their inner calls into Lugha change.
289. **Tail positions**: `return e`, a function's tail and `?`'s early return all store into the result slot, then `ret void`; copies at return (`value_for_return`) are unchanged.
290. **Not**: whole-value `load`/`store`/`insertvalue`/`extractvalue` inside bodies (step 2), `byval` parameters, changes at `-O2` beyond what LLVM does with the new IR, a new calling convention for C.
291. **Measurement** (session 297's Q2): before and after, on kivuli `examples/backgrounds.la` and `test/diff.la`: `lughac build` / `lughac test` wall time, `llc -O0 -time-passes` instruction-selection time on the `--emit=ir` output, and the count of fallback functions (whole-function and total), recorded in CONTEXT.md and the PRP.
292. **Docs**: spec §9's lowering notes (results through a hidden pointer), "Changes since v16" not needed (no language change); Draft; CHANGELOG (Changed: faster builds); CLAUDE.md if a file's role changes. **Success**: milestone 125, tag `m125`.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- One helper deciding "returns through a pointer" for a type, used by declaration, definition and every call kind.
- Every Lugha-to-Lugha call kind listed in CONTEXT converted; the IR passes `module.verify()` for every test program.

### Must NOT Do
- No change to program behaviour, output, symbols or the C ABI.
- No `malloc`; result slots are entry-block `alloca`s.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- None new: codegen failures remain `CodegenError` (exit 2). A call site without a result slot where one is required is a compiler bug (`.expect` naming the invariant).

## SECURITY CONSIDERATIONS
- None: no input handling, no `unsafe`, no change to how `cc`, `git` or programs run.

## TESTS TO WRITE
- [ ] `src/codegen` unit tests (IR text): a function returning a struct is `define void` with `sret`; its caller passes an `alloca`; an enum, an `Option`, a tuple, a generic instance, a method, a closure, an interface method, an iterator's `next` the same; a function returning `i64`/`string`/an array unchanged; an extern returning a struct still uses `c_calls`' lowering.
- [ ] `tests/codegen.rs`: programs returning each aggregate kind through every call kind (direct, method, function value, closure, interface, default method, operator, `spawn`'s `join`, actor call, callback from C) run identically at `-O0` and `-O2`.
- [ ] The whole existing suite unchanged: `programs`, `spec`, `codegen`, `extern_structs`, `callbacks`, `c_arrays`, `c_memory`, `out_params`, `tasks_runtime`, `interface_values`, `ordering`, `test_runner` (full `cargo test`: this reaches across codegen, per the TESTING RULE).
- [ ] Measurement (decision 291) recorded.

## ROLLBACK PLAN
- Branch to return to: `main`; the work lives on `prp-157-struct_returns` until merged.

## ACCEPTANCE CRITERIA
- [x] ~~No fallback "didn't lower all arguments"~~ (a misreading, see RESULT); struct `ret`s gone (430 → 0), timings recorded
- [ ] Full `cargo test` passes; clippy and fmt pass; no file over 500 lines
- [ ] Timings before/after recorded; spec §9, Draft, CHANGELOG updated

## VALIDATION
- `cargo fmt --check`; `cargo clippy --all-targets -- -D warnings`; `cargo test`
- `lughac build --emit=ir examples/backgrounds.la | llc -O0 -fast-isel-report-on-fallback -pass-remarks-missed=sdagisel` in kivuli (read only)
