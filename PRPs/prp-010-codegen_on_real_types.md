## FEATURE: Codegen reads the checker's type table and lowers real `i32`, `i64`, `u8`, `f64` and `bool`, with spec §4 casts — removing codegen's interim value kinds and closing milestone 3.

**Status:** approved 2026-10-09 — session 13
**Milestone:** 3, last of three PRPs; tag `m3` after the merge
**Spec:** §4 (types, LLVM lowering table, casts), §5 (arithmetic semantics), §8 (symbol naming), §9 (lowering notes: opaque pointers need the checker's types, `llvm.fptosi.sat`, if-phi rules), §11 (milestone 3)
**Decisions:** CLAUDE.md architecture rule 4 (codegen never infers types; it reads the checker's side table); PRP-004 / spec §11 (overflow and division panics are milestone 4, so wrap and trap stay)

## OBJECTIVE
Programs using every milestone 3 type and cast compile and run with the right widths and semantics:
- `i32` arithmetic wraps at 32 bits.
- `u8` compares and divides unsigned.
- `f64` follows IEEE 754.
- `1.0e20 as i32` saturates, and `NaN as u8` is 0.

Codegen no longer makes up types or second-guesses the checker: every "not implemented yet: … (milestone 3)" stop is gone. `lughac` builds every program the milestone 3 checker accepts. Milestone 3 is complete.

## CONTEXT

- Starting state:
  - Codegen (`src/codegen/`) computes its own `Value::{Int, Bool, Never}` kinds, treats every integer as `i64`, and stops with milestone 3 messages on `f64`, casts, type and name mistakes, missing returns, `break` outside loops and non-place assignments.
  - The checker (`src/check/`) produces `Checked { types }` keyed by `ExprId`, but the driver discards it.
  - `lugha_fn_main` returns `i64`.
- Ending state:
  - `codegen::emit_ir(&Program, &Checked)` and `emit_object(&Program, &Checked, OptLevel, &Path)`.
  - Codegen's `value.rs` is reduced to `Value { Val(BasicValueEnum), Void, Never }` plus a `Type` → LLVM mapping.
  - Casts are lowered in a new `codegen/cast.rs`. Milestone 3 `Unsupported` stops are removed.
  - The driver passes the type table through. New run-mode `tests/programs/m3/` programs.
- Related existing code: `src/codegen/*`, `src/check/{mod,types}.rs`, `src/driver/pipeline.rs`, `tests/codegen.rs`.
- Open decisions that must be resolved first: none.

### Discovery answers (session 13)
1. Codegen takes `&Checked` and reads every expression's type from it. Its own kinds and milestone 3 checks are deleted. A small `Value` enum keeps `Never`, because blocks have no `ExprId` and the no-phi-edge rule for diverging branches (§9) needs it.

Fixed by the spec, not asked:
- `bool` is `i1`; `u8` is `i8`, unsigned.
- `f64` follows IEEE: division by zero gives infinity or NaN; comparisons use ordered predicates except `!=`, which uses `UNE`.
- Casts follow §4.
- `lugha_fn_main` returns a real `i32` (§6).

## IMPLEMENTATION REQUIREMENTS

### Must Do

**API and driver**
- `emit_ir` and `emit_object` take `&Checked` after the program.
- `pipeline::Front` keeps the `Checked` from `check()`, and `ir`/`build`/`run` pass it through.
- `--emit=ast` still uses the parse-only front end.

**Types** — `codegen/value.rs`
- `fn llvm_type(ty: check::Type) -> BasicTypeEnum`: `i32`→`i32`, `i64`→`i64`, `u8`→`i8`, `f64`→`double`, `bool`→`i1`. `void`, `Never` and `Error` are unreachable here, since the checker guarantees value positions have value types.
- `Lowerer::ty(expr)` reads `checked.types[expr.id]`.
- `enum Value<'ctx> { Val(BasicValueEnum<'ctx>), Void, Never }`. `Never` has the same meaning as today.

**Literals and arithmetic** — `codegen/expr.rs` (spec §4, §5)
- Integer literals use the width of their recorded type. `-` applied directly to a literal is folded into the constant (§4 rule 6), so `-2147483648` is an `i32` constant. Float literals become `double` constants.
- Integer `+ - *` and unary `-` wrap at their width, with no `nsw` (milestone 4 adds panics).
- Integer `/ %`:
  - `i32`/`i64`: `sdiv`/`srem`, guarded by the trap for a zero divisor or `MIN / -1` at that width.
  - `u8`: `udiv`/`urem`, guarded for zero only.
- `f64`: `fadd fsub fmul fdiv frem`; unary `-` is `fneg`. No traps.
- Comparisons:
  - `i32`/`i64`: signed `icmp`.
  - `u8`: unsigned `icmp`.
  - `bool`: `icmp eq/ne`.
  - `f64`: `fcmp` with `OLT OLE OGT OGE OEQ`, and `UNE` for `!=`.
- `&&`, `||` and `!` are unchanged (`i1`).

**Casts** — new `codegen/cast.rs` (spec §4)
- Int → int: same width is a no-op; narrowing truncates; widening sign-extends from `i32`/`i64` and zero-extends from `u8`.
- Int → `f64`: `sitofp`, or `uitofp` from `u8`.
- `f64` → int: `llvm.fptosi.sat` for `i32`/`i64` and `llvm.fptoui.sat` for `u8`. These saturate, and NaN becomes 0 (§4, §9).
- `f64` → `f64`: a no-op.

**Locals, functions, control flow**
- Every alloca, parameter and return type comes from the checker's types or the declared annotation. `let` uses its initialiser's recorded type.
- Function signatures map annotation types with `llvm_type`. `lugha_fn_main` returns `i32` (or `void`), and the C `main` returns its result directly.
- `if` phis use the recorded `if` type. Divergence (`Never`) and the no-edge rule are unchanged.
- `for` counters use the range's integer type; the step is `+1` at that width.

**Deleted**
- Codegen's `Int`/`Bool` kinds, `type_error`, `annotation_kind`.
- Every `Unsupported { milestone: 3, .. }` path: type checking, undefined names, duplicate names, calls, `main` rules, missing returns, `break` outside loops, assignment targets.
- Where the checker guarantees an invariant, codegen uses `expect("checked: …")` or `unreachable!` stating it.
- Milestone 4/5 `Unsupported` paths (strings, intrinsics, `extern`, arrays, structs) stay as defensive errors, since `emit_ir` is public.

**Tests that change**
- Codegen unit tests that assert milestone 3 stops are deleted or turned into IR assertions.
- `tests/codegen.rs` builds through `check`.
- The CLAUDE.md known issue "Until PRP-010, codegen keeps its interim …" is removed. The wrap/trap issue stays (milestone 4).

**Acceptance** — `tests/programs/m3/`, run mode, cross-checked at `-O0` and `-O2` by `tests/codegen.rs` (extended to every `m3/` program with an `.exit` file):
- `i32_wrap.la`: `let x: i32 = 2147483647; let y = x + 1; if y < 0 { 9 } else { 0 }` → 9. Under the old `i64` codegen this would be 0.
- `u8_unsigned.la`: `200 > 100` as `u8` is true, and `200 / 3` as `u8` is 66 (signed `i8` would give −18) → exit 66.
- `u8_wrap.la`: `let b: u8 = 250; let c = b + 10; c as i32` → 4.
- `f64_math.la`: area of a circle with `r = 2.5` (`3.14159 * r * r` = 19.63…), `as i32` → 19.
- `casts.la`: each §4 cast rule checked in turn — `return k;` on the first failure, `42` if all pass. It covers:
  - `300 as u8` = 44
  - `-1 as u8` = 255
  - `2.9 as i32` = 2 and `-2.9 as i32` = −2
  - `1.0e20 as i32` = `i32::MAX`
  - `(0.0 / 0.0) as u8` = 0
  - `(200 as u8) as f64` = 200.0
  - `(-7 as i32) as i64` = −7
  - `(1.0 / 0.0) > 1.0e308` is true
- `div_min_i32.la`: `let m: i32 = -2147483648; m / -1` → killed by SIGILL, exit 132. The trap guard works at 32 bits.

### Must NOT Do
- No overflow or division panics, `lugha_rt`, intrinsics, strings, `extern`, arrays or structs (milestones 4/5).
- No checker changes beyond exposing what codegen needs. If a checker bug is found, stop and report it.
- No new dependencies, no `unsafe`.

## ERROR HANDLING REQUIREMENTS

- `CodegenError::Verify` still guards everything: every test program's module is verified.
- Checker invariants that codegen relies on use `expect`/`unreachable!` with the invariant in the message. A violation is a compiler bug (exit 2), never a user-facing panic path for checked programs.

## SECURITY CONSIDERATIONS

- No new input handling. Recursion is unchanged (bounded by the parser). No `unsafe`.

## TESTS TO WRITE

Unit tests (`src/codegen/`):
- [ ] IR widths: an `i32` add is `add i32`; a `u8` division is `udiv i8` with a zero guard and no `MIN` guard; `f64` uses `fadd double`/`fcmp olt` and `!=` is `fcmp une`.
- [ ] Casts: `sext`/`zext`/`trunc` chosen by signedness; `uitofp` from `u8`; `llvm.fptosi.sat.i32.f64` and `llvm.fptoui.sat.i8.f64` are declared and called.
- [ ] `define i32 @lugha_fn_main()` for `fun main(): i32`; the C `main` returns it with no truncation.
- [ ] `-2147483648` as an `i32` is a single `i32` constant.

Integration and acceptance:
- [ ] All six new `m3/` run programs pass through `lughac run`, and agree at `-O0`/`-O2`.
- [ ] Every existing `m1/`, `m2/`, `m3/` and CLI test still passes.

## ROLLBACK PLAN

- Branch `prp-010-codegen_on_real_types`, merged into `main` on acceptance; then tag `m3`.
- To abandon: delete the branch. The checker and driver keep working, with the old codegen.

## ACCEPTANCE CRITERIA
- [ ] Every test above exists and passes — **milestone 3 complete**.
- [ ] No `milestone: 3` remains in `src/codegen/`.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass.
- [ ] CLAUDE.md known issues and file tree, CHANGELOG.md, TODO.md, MEMORY.md updated.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `grep -rn "milestone: 3\|, 3," src/codegen/` → nothing
- `cargo run -q -- run tests/programs/m3/casts.la; echo $?` → `42`
