## FEATURE: `extern fun` declarations for calling C, and integer overflow and division panics replacing wrap-and-trap — completing milestone 4.

**Status:** merged into `main` 2026-10-09 — session 15
**Milestone:** 4, second of two PRPs; tag `m4` after the merge
**Spec:** §5 (arithmetic panics, panic format), §8 (C interop: allowed types, string pointer adjustment, symbol names, reserved `lugha_` prefix), §9 (lowering: `llvm.*.with.overflow`, division checks, error codes)
**Decisions:** DECISION-009 / MEMORY 15 (runtime ABI: `bool` and `u8` widened to `i32` for runtime calls; extern calls follow C's ABI instead)

## OBJECTIVE
- The spec §10 libc example runs: `puts("hello from libc")` and `println(sqrt(2.0))` print `hello from libc` and `1.4142135623730951`.
- Integer arithmetic never silently wraps or crashes with SIGILL. Overflow and bad division stop with `panic: integer overflow at file:line:col` or `panic: division by zero at …`, exit 101, pointing at the operator.
- Milestone 4 is complete.

## CONTEXT

- Starting state:
  - `extern fun` stops in the checker and codegen with "not implemented yet: extern functions (milestone 4)".
  - Integer `+ - *` wrap; `/ %` call `llvm.trap` (SIGILL, exit 132) — a CLAUDE.md known issue.
  - `ExprKind::Binary` and `StmtKind::Assign` carry no operator span.
- Ending state:
  - The checker and codegen support `extern fun`.
  - Checked arithmetic calls `lugha_rt_panic`; `llvm.trap` is gone.
  - The AST records operator spans.
  - Four existing acceptance programs change expectation from wrap or trap to panic.
  - New `m4/` programs; spec §5 and §9 updated.
- Related existing code: `src/ast/{mod,expr}.rs`, `src/parser/expr.rs`, `src/parser/stmt.rs`, `src/check/{env,call,errors}.rs`, `src/codegen/{arith,function,runtime,expr,stmt}.rs`.
- Open decisions that must be resolved first: none.

### Amendments during implementation (session 15)
- **String pointer adjustment uses inkwell's safe `build_struct_gep`** on `{ i64, [0 x i8] }`, field 1, instead of `build_gep`. `build_gep` is an `unsafe fn`, and CLAUDE.md forbids `unsafe` without approval. Same address, 8 bytes in.
- **`zeroext` is set on the extern declaration only.** For direct calls LLVM reads parameter attributes from the callee (`CallBase::paramHasAttr`).
- **Signal tests moved to `extern fun abort();`.** `tests/cli.rs` still covers `lughac run`'s 128 + N signal mapping (SIGABRT → 134), now that SIGILL traps are gone.
- **Updated earlier tests:**
  - `tests/codegen.rs`: the wrap/trap tests became `integer_overflow_panics` and `bad_divisions_panic` (exit 101).
  - Unit tests now assert `with.overflow` calls and the `division by zero` message. With a constant divisor LLVM folds the zero comparison, so the test checks the message, not an instruction name.
  - An old "extern stops" assertion was removed.
- **Renames and goldens:**
  - `m3/i32_wrap` → `m3/i32_overflow` and `m3/u8_wrap` → `m3/u8_overflow`.
  - All panic `.stderr` goldens (columns computed by hand) matched on the first run. The E0306/E0409 goldens were captured and reviewed.

### Discovery answers (session 15)
1. Codes: **E0306** reserved extern name (`lugha_` prefix) and **E0409** type not allowed in an extern signature (a `string` return). A duplicate or intrinsic-named extern reuses E0302; calls reuse E0403/E0405.
2. Panic location: **the operator**. `Binary` gains the operator's span, and compound assignment the `+=` span. Unary minus uses its expression's start, which is the `-`.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**AST and parser**
- `ExprKind::Binary(BinOp, Span, Box<Expr>, Box<Expr>)` — the `Span` is the operator token.
- `StmtKind::Assign { op, op_span, place, value }`.
- The parser fills both in. The S-expression printer, checker and codegen ignore or use them; printed output is unchanged.

**Checker: `extern fun`** — `check/env.rs` (spec §8)
- Collected in pass 1 alongside functions, in the same namespace:
  - a name starting with `lugha_` → **E0306** "`lugha_…` is reserved for the compiler and runtime";
  - a duplicate or intrinsic name → E0302.
- Parameters may be `i32`, `i64`, `u8`, `f64`, `bool` or `string`. Arrays and structs stop with milestone 5 as now; unknown names are E0305.
- The return type may be any of those except `string` → **E0409** "an extern function can't return `string`", with help "C strings have no length header; return a number and use it from Lugha".
- Calls to externs are checked exactly like calls to functions.
- An extern has no body to check; `extern fun main` doesn't count as `main`.

**Codegen: `extern fun`** — `codegen/function.rs`
- Declared with its name verbatim (§8) and external linkage. Lugha functions keep `lugha_fn_`.
- C ABI:
  - `bool` is `i1` and `u8` is `i8`, each with the `zeroext` parameter attribute (and on the return).
  - `f64` is `double`.
  - `string` is `ptr`.
- At each call, a `string` argument is adjusted to point at its first data byte: `getelementptr i8, ptr %s, i64 8` (§8). C receives an ordinary NUL-terminated string.
- `Signature` records whether a function is extern, so the adjustment applies only to externs.

**Checked arithmetic** — `codegen/arith.rs` (spec §5, §9)
- Integer `+ - *` use `llvm.{s,u}{add,sub,mul}.with.overflow.iN`: signed for `i32`/`i64`, unsigned for `u8`. When the overflow bit is set, branch to `lugha_rt_panic("integer overflow", file, line, col)` and `unreachable`; otherwise continue.
- Unary `-` on an integer is a checked `0 - x`. `-i64::MIN` panics.
- Integer `/ %`:
  - a zero divisor panics with `"division by zero"`;
  - a signed `MIN / -1` or `MIN % -1` panics with `"integer overflow"`;
  - otherwise `sdiv`/`udiv`/`srem`/`urem`.
- Compound assignment (`+= -= *= /=`) is checked the same way, located at the compound operator.
- Locations: binary operators at their operator span, compound assignment at `op_span`, unary minus at its expression start.
- The `for`-loop step `i + 1` stays unchecked — it can't overflow (`i < end`), and a comment says so.
- `f64` arithmetic is unchanged (IEEE, no panics). `as` casts are unchanged (§4: casts wrap, truncate or saturate).
- `llvm.trap` and its guard are removed.
- `codegen/runtime.rs` gains `emit_panic(message: &str, at)`, which calls and terminates without starting a dead block, for use inside arithmetic. `panic_at` keeps its current behaviour for the `panic` intrinsic.

**Spec, docs and tests that change**
- Spec §5: name the messages, `integer overflow` and `division by zero`. Spec §9: add rows for E0306 and E0409.
- CLAUDE.md KNOWN ISSUES: remove "Until milestone 4, integer `+ - *` wrap and `/ %` … trap".
- Acceptance programs whose expectation changes from wrap or trap to panic. Each keeps its file name, its `.exit` becomes 101, and a `.stderr` is added with the exact panic line:
  - `m1/div_zero.la`
  - `m3/i32_wrap.la` (renamed to `m3/i32_overflow.la`)
  - `m3/u8_wrap.la` (renamed to `m3/u8_overflow.la`)
  - `m3/div_min_i32.la`
- `tests/codegen.rs`: the wrap and trap tests become panic tests (exit 101), and the IR unit tests assert `with.overflow` calls and `lugha_rt_panic`, with no `llvm.trap`.

**Acceptance** — `tests/programs/m4/`, cross-checked at `-O0`/`-O2`
- `libc.la`: the spec §10 "Calling C" program → `hello from libc\n1.4142135623730951\n`, exit 0.
- `extern_types.la`: `extern fun abs(x: i32): i32;`, `extern fun llabs(x: i64): i64;`, `extern fun toupper(c: i32): i32;` and `extern fun strlen(s: string): i64;`, used together → a known value printed. This proves `strlen` sees the data bytes, not the header.
- `overflow_add.la`: `i32::MAX + 1`, with stderr `panic: integer overflow at …:L:C`, where C is the `+` column.
- `overflow_neg.la`: `-x` for `x: i64 = -9223372036854775808` → `integer overflow` at the `-`.
- `overflow_compound.la`: `let mut b: u8 = 250; b += 10;` → `integer overflow` at `+=`.
- `div_zero.la`: `a % b` with `b == 0` from a function argument → `division by zero` at the `%`.
- Reject mode in `tests/programs/m3/`: `e0306.la` and `e0409.la`, with goldens captured and reviewed.

### Must NOT Do
- No variadic externs (§8), no structs or arrays across the boundary, no `string` returns, no extern callbacks.
- No wrapping arithmetic functions (`wrapping_add` is §11 "out of scope").
- No changes to `f64` semantics, casts or the runtime ABI for intrinsics.
- No new dependencies, no `unsafe`.

## ERROR HANDLING REQUIREMENTS

- Panic paths always end in `unreachable` after `lugha_rt_panic` (which calls `exit(101)`). The IR verifies on every test program.
- Extern declarations that would violate §8 are compile errors (E0306/E0409/E0302), never run-time surprises.

## SECURITY CONSIDERATIONS

- Calling C is the one route to undefined behaviour in v0 (§7). The checker enforces §8's types; the remaining contract (C mustn't keep or write through the string pointer) is the user's, as the spec says. No new runtime surface.
- The string adjustment points inside a valid Lugha string object, at its NUL-terminated bytes (§7 layout).
- No `unsafe`.

## TESTS TO WRITE

Unit tests:
- [x] Parser: binary operator spans slice to `+`, `==`, …; compound `op_span` slices to `+=`. S-expressions are unchanged.
- [x] Checker: E0306 (`lugha_rt_alloc`); E0409 (`string` return); an extern named `println` or duplicating a function → E0302; extern calls checked like functions (E0405, E0403); a `string` parameter is allowed; unknown types are E0305.
- [x] Codegen IR:
  - an `extern` is declared verbatim, with `zeroext` on `bool`/`u8`;
  - a string argument gets `getelementptr i8, ptr …, i64 8`;
  - `i32` add calls `llvm.sadd.with.overflow.i32` and `u8` add calls `llvm.uadd.with.overflow.i8`;
  - division checks zero, then `MIN`/`-1` for signed types only;
  - no `llvm.trap` anywhere; the panic call passes the operator's line and column.

Integration and acceptance:
- [x] All new `m4/` programs and the two reject programs pass; the four updated programs panic as specified.
- [x] All other tests still pass.

## ROLLBACK PLAN

- Branch `prp-012-extern_and_panics`, merged into `main` on acceptance; then tag `m4`.
- To abandon: delete the branch. The AST span change is mechanical and contained in the branch.

## ACCEPTANCE CRITERIA
- [ ] `lughac run tests/programs/m4/libc.la` prints `hello from libc` and `1.4142135623730951` — **milestone 4 done**.
- [ ] Every test above exists and passes; no `llvm.trap` in `src/`.
- [ ] Spec §5 and §9, CLAUDE.md, CHANGELOG.md, TODO.md and MEMORY.md updated.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `grep -rn "llvm.trap" src/` → nothing
- `cargo run -q -- run tests/programs/m4/libc.la`
