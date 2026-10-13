## FEATURE: Text and option helpers — string, `string[]`, `Option` and `Result` methods.

**Status:** approved 2026-10-13 — session 54
**Milestone:** 18 (one PRP)
**Spec:** `Language v4 Draft.md`:
- decision 4;
- §4 helper table;
- §9 (prelude, runtime);
- §10 "Text helpers";
- §11 (milestone 18);
- §12 (`lines()` and `\r`).

**Decisions:** MEMORY 49 (v4 direction), MEMORY 48 (prelude helpers), MEMORY 50 (`Result` in the prelude).

## OBJECTIVE
Strings, `Option` and `Result` get the methods real input processing needs. The §10 text-helpers program prints `42`, `a-b-c`, `2`.

| Receiver | Methods |
| --- | --- |
| `string` | `lines()`, `split(sep)`, `trim()`, `contains(sub)`, `starts_with(p)`, `ends_with(p)`, `slice(from, to)` |
| `string[]` | `join(sep)` (besides the array helpers) |
| `Option<T>` | `unwrap_or(d)`, `ok_or(e)`, `map(f)`, `is_some()`, `is_none()` |
| `Result<T, E>` | `unwrap_or(d)`, `ok()`, `map(f)`, `map_err(f)`, `is_ok()`, `is_err()` |

Each takes `self` and never changes it; behaviour is as in the draft's §4 table.

## CONTEXT

- **Starting state:**
  - The prelude holds `Result` and the six array helpers.
  - `helper_call` handles array receivers only.
  - Strings have `.len` and `s[i]`, and no methods.
- **Ending state:**
  - Helper dispatch goes by receiver type to prelude functions.
  - `slice` is a built-in string method lowered to a runtime call.
  - Prelude functions that aren't generic are compiled only when something calls them.
- **Related existing code:**
  - `src/prelude.la`;
  - `src/check/` (`helpers.rs`, `method_calls.rs`, `builtins.rs`);
  - `src/codegen/` (`methods.rs`, `lower.rs`, `declare.rs`, `heap.rs`, `runtime.rs`);
  - `runtime/lugha_rt.c`.
- **Open decisions that must be resolved first:** none.

### Discovery answers (session 54)
1. **Exactly the draft's set.** `find`, `replace` and case changes wait.

### Design choices for review
- **`slice` is built in**, not prelude code.
  - `s.slice(from, to)` lowers to `lugha_rt_str_slice(s, from, to, file, line, col)`, which copies the bytes into a new string.
  - It panics with `slice out of bounds: from F to T of a string of length L` unless `0 <= from <= to <= len`.
  - Its panic location is the caller's line, as for indexing.
  - The prelude's string helpers use `slice` themselves.
- **Every other helper is prelude code**, dispatched by receiver type to internally named prelude functions: `str_lines`, `str_join`, `option_map`, `result_map_err`, …. A program sees only the methods. `map` means the array, `Option` or `Result` helper according to the receiver.
- **Panics inside the prelude:** `split("")` panics with `split with an empty separator`. That panic happens in prelude code, so its location is the prelude's line, shown as `<prelude>:L:C`. This is the one helper panic, and the message says what's wrong.
- **Non-generic prelude functions** (such as the string helpers) are declared and compiled only when some call uses them, as generic helpers already are. A program that uses no helper still gives byte-identical IR.
- **Exact behaviour** (from the draft):
  - **`lines`:** splits at `\n` and removes one trailing `\r` from each line. `"a\nb\n"` gives `["a", "b"]`, `"a\n\nb"` gives `["a", "", "b"]`, and `""` gives `[]`.
  - **`split`:** keeps empty parts (`"a,,b"` gives `["a", "", "b"]`), and `"abc".split(",")` gives `["abc"]`.
  - **`trim`:** removes ASCII space, `\t`, `\n` and `\r` at both ends.
  - **`contains("")`, `starts_with("")` and `ends_with("")`** are true.
  - **`join`** on `[]` gives `""`.
  - **`Option`/`Result` helpers** follow the draft's types. A closure's types are inferred as for the array helpers.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Prelude** — `src/prelude.la`
- The string, `join`, `Option` and `Result` helpers as Lugha functions, commented, using `s[i]`, `s.len`, `s.slice(a, b)` and `+`.

**Checker**
- **Dispatch:** `check/helpers.rs` dispatches by receiver:
  - `string` to the string helpers;
  - `string[]` to `join` and the array helpers;
  - arrays to the array helpers;
  - `Option` and `Result` to their helpers.

  Each goes to the prelude function's key, with the receiver binding the first parameter, as today.
- **`slice`:** a built-in method on `string` (`builtins.rs`), `(i64, i64) -> string`.
- **Errors:** the existing ones. An unknown method on a string is E0415, with arity E0405 and argument types E0403/E0401.

**Codegen**
- **Prelude calls:** helper calls for every receiver type call the instance or function with the receiver first. An aggregate receiver (`Option`, `Result`) is passed by pointer, as for any argument.
- **`slice`:** calls `lugha_rt_str_slice` with the panic location.
- **Unused prelude functions:** non-generic prelude functions are declared and defined only if some recorded call (`Checked.callees`) names them.

**Runtime**
- `lugha_rt_str_slice` in `runtime/lugha_rt.c`, added to `RUNTIME_SYMBOLS`.

**Spec** — `Language v4 Specification.md`
- **Folded in:**
  - §4: the helper table and behaviour, including the `slice` panic and `split("")`;
  - §5: the `slice` panic message;
  - §9: the runtime function;
  - §10: the text-helpers program;
  - §11: milestone 18 done.
- **The draft's §12** `lines()`/`\r` question is resolved (yes, as drafted).
- **The status line** becomes "in progress: milestone 18".

**Docs**
- README, TODO, CHANGELOG, MEMORY decision 51, CONTEXT, and the CLAUDE.md tree.

### Must NOT Do
- **No helpers beyond the draft's set:** no `find`, `replace`, `to_upper` or `unwrap`.
- **No I/O** (milestone 19).
- **No helper changes its receiver.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **User mistakes** are ordinary diagnostics at the call.
- **Runtime failures** are panics:
  - `slice` out of range (at the caller);
  - `split("")` (at the prelude line).
- **Prelude bugs:** a prelude check error is an internal error (exit 2), as today.

## SECURITY CONSIDERATIONS
- **`lugha_rt_str_slice`** checks every bound before copying and allocates through `lugha_rt_alloc`.
- No `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — checker (`check/helpers.rs`):
- [ ] **Types:**
  - each string helper's type, including `s.slice(0, 2)`;
  - `xs.join("-")` on `string[]` is `string`;
  - `[1].join("-")` is E0415 (only `string[]` has `join`).
- [ ] **`Option`:** `o.unwrap_or(0)`, `o.ok_or("none")` (`Result<i64, string>`), `o.map(fun(x) = "{x}")` (`Option<string>`), `o.is_some()`.
- [ ] **`Result`:** `r.unwrap_or(0)`, `r.ok()`, `r.map(fun(x) = x + 1)`, `r.map_err(fun(e) = e.len)` (`Result<i64, i64>`), `r.is_err()`.
- [ ] **`?` composes:** `parse_i64(s).ok_or("bad")?` in a `Result<i64, string>` function.
- [ ] **Errors:** `s.trim(1)` is E0405; `s.split(1)` is E0403; `s.upper()` is E0415.

Acceptance — `tests/programs/m18/` (run at `-O0` and `-O2`):
- [ ] `strings.la`:
  - `lines` (with `\r\n`, an empty middle line, a trailing newline, the empty string);
  - `split` (empty parts, no separator present, a multi-byte separator);
  - `trim`;
  - the predicates, including empty arguments;
  - `slice`;
  - `join` (including `[]`).
- [ ] `options.la`: every `Option` and `Result` helper, chained with `?` and with closures capturing locals.
- [ ] `slice_panic.la`: out-of-range `slice` panics at the caller's line (exit 101, `.stderr`).
- [ ] `split_panic.la`: `split("")` panics (exit 101, the message checked with `.stderr`).
- [ ] `e0415.la`: reject golden (`[1, 2].join("-")`).
- [ ] The §10 text-helpers program runs from the spec.

Driver / IR:
- [ ] A program using no helpers gives no `$prelude` functions in its IR.
- [ ] One using `s.trim()` defines `lugha_fn_$prelude/str_trim` and not `str_lines`.

Regression:
- [ ] Every earlier test passes.

## ROLLBACK PLAN

- Branch `prp-043-text_helpers`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The §10 text-helpers program and the `m18/` programs pass.
- [ ] The spec is folded (in progress: milestone 18); docs updated; tag `m18`.
- [ ] No file over 300 lines (splits proposed first); no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m18/strings.la
```
