## FEATURE: Compile functions — parameters, calls (including forward and mutual recursion), `return`, void functions and `=` bodies — completing milestone 2.

**Status:** approved 2026-10-09 — session 10
**Milestone:** 2, second of two PRPs. Done-when: the spec §11 milestone 2 program exits 55.
**Spec:** §6 (functions, item order, names, expression bodies, return checking, entry point), §8 (`lugha_fn_` prefix), §9 (lowering: a definitely-returning branch adds no phi edge)
**Decisions:** CLAUDE.md rule 9 (milestones 1–2: integers are `i64`); PRP-006 policies (`Int`/`Bool` value kinds; stop where forced, compile where possible)

## OBJECTIVE
Programs with several functions compile and run: recursion, calls to functions defined later in the file, mutual recursion, early `return`, and void helpers. The spec §11 milestone 2 program exits 55, and lughac's milestone 2 is complete. Mistakes the checker will report in milestone 3 stop with "not implemented yet: … (milestone 3)" and never miscompile.

## CONTEXT

- Starting state:
  - Codegen lowers a single `main` with locals and control flow (`src/codegen/`).
  - Any other function gives "functions other than main" (2); calls give "function calls" (2); `return` gives "`return`" (2).
  - `lugha_fn_main` returns `i32`.
- Ending state:
  - New `src/codegen/function.rs`: signatures, function bodies, calls, `return`.
  - `Value` gains `Never`; `lower.rs` lowers every function.
  - `tests/programs/m2/` gains the §11 program and four more; CLAUDE.md is updated.
- Related existing code: `src/codegen/{lower,value,control,stmt,expr,scope}.rs`.
- Open decisions that must be resolved first: none.

### Discovery answers (session 10)
1. **Divergence is tracked with a third value kind, `Never`, following §6 exactly:**
   - After `return`, `break` or `continue`, the rest of the block is `Never`.
   - An expression statement whose value is `Never` makes its block `Never`.
   - `if`/`else` is `Never` when both branches are.
   - Loops are never `Never`.
   - A `Never` branch adds no phi edge (§9) and fits any expected kind.
   - A non-void function whose body ends `Void` gives "checking missing returns" (3).
   - A `Never` body end emits `unreachable`, which §6 guarantees is never reached.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Signatures** — `codegen/function.rs`
- Before lowering any body, collect every `fun` into a table: name → LLVM function, parameter kinds, return kind (`None` = void). This is §6 pass 1, and it makes forward and mutual calls work.
- Each function is emitted as `lugha_fn_<name>`.
- Kinds from annotations:
  - `i32`, `i64` and `u8` are `Int` (`i64`).
  - `bool` is `Bool` (`i1`).
  - `f64` and `string` give milestones 3 and 4.
  - Arrays and structs give milestone 5.
  - Move `annotation_kind` from `stmt.rs` to `value.rs` so both modules share it.
- A second `fun` with the same name gives "checking duplicate names" (3) at its name. `extern` still gives 4 and `struct` still gives 5.
- `main` must exist, with no parameters and `: i32` or nothing; the milestone 1 errors are kept.
- `lugha_fn_main` now returns `i64` for `: i32`, like every `Int` function. The C `main` truncates it to `i32`, or returns 0 for a void `main`. This is an internal ABI detail until milestone 3.

**Function bodies**
- Per function: a fresh scope stack and loop stack, and an entry block.
- Each parameter gets an entry-block alloca, holds its argument, and is declared in a scope enclosing the body.
- Parameters are immutable (§6). Assigning to one compiles, like any non-`mut` binding — the existing known issue. `let mut n = n;` shadows as usual.
- `= e;` bodies need no special code; the parser already stores them as `{ e }`.
- End of body, for a function returning kind `K`:
  - an `Int`/`Bool` value of kind `K` returns it
  - `Never` emits `unreachable`
  - `Void` gives "checking missing returns" (3) at the function name
  - a value of the wrong kind gives "type checking" (3) at the tail
- End of body, for a void function: `Never` emits `unreachable`; anything else is discarded and it returns `void`.

**`return`**
- `return e;` requires a non-void function and `e` of the return kind.
- `return;` requires a void function.
- Violations give "type checking" (3) at the statement.
- After the `ret`, lowering continues in a fresh dead block (as for `break`), and the statement makes its block `Never`.

**Calls** (`ExprKind::Call` with a `Name` callee)
- Name resolution (§6): locals shadow functions.
  - A local is found: "checking calls" (3) at the callee — values aren't callable in v0.
  - No local and no function, but the name is an intrinsic (`print`, `println`, `panic`, `to_string`): "intrinsics" (4).
  - Otherwise: "checking undefined names" (3).
- A callee that isn't a plain name gives "checking calls" (3).
- Arguments are evaluated left to right (§5).
- A wrong argument count gives "checking calls" (3) at the call. Each argument's kind must match its parameter's, otherwise "type checking" (3) at the argument.
- The result is `Int` or `Bool` per the return kind, or `Void`.
- A function name used as a value (`let f = fib;`) gives "type checking" (3) — v0 has no function values.

**Divergence** — `value.rs`, `control.rs`, `stmt.rs`
- `Value::Never`.
- `Never` stands in for any kind. `int()`, `bool()` and `typed()` on `Never` return an `undef` of the needed type; the code is unreachable, so the value is never observed.
- `stmt()` reports whether the statement diverges: `return`, `break`, `continue`, or an expression statement whose value is `Never`.
- `block()` returns `Never` once any statement diverges, or when its tail is `Never`. Later statements are still lowered, into dead blocks, to report their errors. Unreachable-code warnings are W0101 in milestone 3.
- `if`:
  - A `Never` branch ends its block with `unreachable` and gives no phi edge.
  - Exactly one `Never` branch: the value is the other branch's, used directly. Its block is then the merge block's only predecessor.
  - Both `Never`: the `if` is `Never`.
- `while`/`for` are always `Void` (§6: loops never definitely return).

**Acceptance programs** — `tests/programs/m2/`, cross-checked at `-O2` by the existing `tests/codegen.rs` test:
- `milestone2.la`: the spec §11 program, verbatim → 55.
- `early_return.la`: `abs` using `if x < 0 { return -x; } else { x }`, then `abs(-5)` → 5.
- `mutual_recursion.la`: `is_even`/`is_odd` defined after `main`; `is_even(10)` → 1.
- `void_helpers.la`: a void function with an early `return;`, called as statements, with `main` returning 3 → 3.
- `param_shadow.la`: `fun count(n: i64): i64 { let mut n = n; … }` counting down from 6 → 6.

**Docs**
- CLAUDE.md KNOWN ISSUES: the milestone 3 stop list now includes calls (unknown functions, wrong argument counts or kinds), duplicate functions, and missing returns.
- CLAUDE.md FILE ORGANIZATION: add `function.rs`.

### Must NOT Do
- No intrinsics or `extern` (milestone 4); no real types; no E-codes; no W0101 warnings (milestone 3).
- No closures, function values, default parameters or overloading (§6, §11).
- No changes outside `src/codegen/` apart from tests, test programs and docs. No new dependencies, no `unsafe`.

## ERROR HANDLING REQUIREMENTS

- Every case above is an `Err(CodegenError::Unsupported { what, milestone, span })`. No panics, no invalid IR: `module.verify()` runs on every compile and every test.
- `undef` values appear only in blocks codegen has already made unreachable.

## SECURITY CONSIDERATIONS

- Codegen recursion follows AST depth, which the parser bounds. Runtime recursion depth is the user's program's business — the OS stack limit applies, as in C.
- No `unsafe`.

## TESTS TO WRITE

Unit tests (`src/codegen/`):
- [ ] Each "checking …" case: duplicate function, unknown function, wrong argument count, argument kind mismatch, calling a local, function used as a value, `println(1)` gives intrinsics (4), missing return, `return 1;` in a void function, `return;` in an `i64` function — each with milestone and span.
- [ ] `fun g(): i64 { while true { return 1; } }` gives missing return (§6: loops never definitely return).
- [ ] IR: an `if` with one `Never` branch has no `phi`, and its body verifies; `fun f(): i64 { return 1; }` ends in `unreachable` after the `ret`'s dead block; calls go to `lugha_fn_<name>`.
- [ ] `define i64 @lugha_fn_main()` for `fun main(): i32`; the C `main` truncates.

Integration and acceptance:
- [ ] All five new `m2/` programs pass through `lughac run` and agree at `-O0`/`-O2`.
- [ ] All existing tests still pass.

## ROLLBACK PLAN

- Branch `prp-007-functions`, merged into `main` on acceptance; then tag `m2`.
- To abandon: delete the branch. Existing `m1/` and `m2/` programs guard against regressions.

## ACCEPTANCE CRITERIA
- [ ] `lughac run tests/programs/m2/milestone2.la` exits 55 — **milestone 2 done**.
- [ ] Every test above exists and passes.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass.
- [ ] CLAUDE.md, CHANGELOG.md, TODO.md updated.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo run -q -- run tests/programs/m2/milestone2.la; echo $?` → `55`
