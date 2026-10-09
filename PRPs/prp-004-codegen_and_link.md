## FEATURE: Lower milestone 1 programs (integer arithmetic in `main`) to LLVM IR, emit a native object file, and link it into an executable with `cc`.

**Status:** implemented 2026-10-09 — session 7 (branch `prp-004-codegen_and_link`)
**Milestone:** 1 (codegen subset: every value is `i64`, CLAUDE.md rule 9)
**Spec:** §6 (entry point), §8 (symbol prefixes), §9 (compilation stages 4–6, lowering notes), §11 (milestone 1)
**Decisions:** DECISION-005 (LLVM 21, inkwell `llvm21-1-prefer-dynamic`), DECISION-002 (`thiserror` for internal errors)

## OBJECTIVE
Library code can take a parsed `Program` and produce a running executable. `fun main(): i32 { 2 + 3 * 4 }` builds and exits with 14. Constructs from later milestones fail cleanly with "not implemented yet: X (milestone N)" — never a panic. `lughac` itself is still a stub; wiring it up is the next PRP.

## CONTEXT

- Starting state: `lugha::lexer::lex` and `lugha::parser::parse` produce a `Program`. inkwell 0.10 is a dependency but unused. `lughac` has an empty `main`. `tests/programs.rs::programs` is `#[ignore]`d.
- Ending state: `src/codegen/{mod,lower,expr}.rs` and `src/link.rs` created, `tests/codegen.rs` created, `src/lib.rs` declares `codegen` and `link`. Spec §11 amended. CLAUDE.md KNOWN ISSUES gains the M1–M3 arithmetic behaviour.
- Related existing code: `src/ast/`, `src/span.rs`.
- Open decisions that must be resolved first: none.

### Amendments during implementation (session 7)
- Three more `Unsupported` cases the table didn't list:
  - A program with no `main`: "programs without a `main` function", milestone 3, the checker's job.
  - A second `main`: "duplicate functions", milestone 3.
  - `fun main(): i32 { }` with no tail: "`main` without a result value", milestone 3.
- Codegen reports the **outermost** unsupported construct first. `[1][0]` reports indexing, not the inner array literal; the test was corrected to match. Recorded in CLAUDE.md KNOWN ISSUES.

### Discovery answers (session 7)
1. Unsupported constructs give a typed internal error, `CodegenError::Unsupported { what, milestone, span }`. The driver will print it and exit 2. No E-code is used.
2. Arithmetic before panics exist: `+ - *` wrap; `/` and `%` trap on a zero divisor or `MIN / -1`. Spec §11 moves overflow and division checks from milestone 3 to milestone 4.
3. Testing: integration tests build and run real executables. Unit tests check the IR shape.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Public API** — `src/codegen/mod.rs`
- `pub enum OptLevel { O0, O2 }`, mapped to the pass pipelines `default<O0>` and `default<O2>` (spec §9 stage 5).
- `pub fn emit_ir(program: &Program) -> Result<String, CodegenError>` — the verified module as LLVM IR text. `--emit=ir` will use it.
- `pub fn emit_object(program: &Program, opt: OptLevel, path: &Path) -> Result<(), CodegenError>`:
  1. Lowers and verifies the module.
  2. Runs the pass pipeline.
  3. Writes a native object file for the host triple, with PIC relocation (Ubuntu's `cc` links PIE by default).
- `pub enum CodegenError` (thiserror):
  - `Unsupported { what: &'static str, milestone: u8, span: Span }` — displays `not implemented yet: {what} (milestone {milestone})`.
  - `Verify(String)` — `module.verify()` failed, which is a compiler bug.
  - `Emit(String)` — target lookup, pass pipeline or object writing failed.

**Lowering** — `src/codegen/lower.rs` (items, functions, C `main`) and `src/codegen/expr.rs` (expressions)

Supported in milestone 1:
- Exactly one function, `main`, with no parameters, returning `i32` or nothing (`void`). Its body is optional statement-free: no statements, plus an optional tail.
- Expressions: integer literals (the `u64` bits reinterpreted as `i64`), parentheses (already gone in the AST), unary `-`, and binary `+ - * / %`. All values are `i64`.

Function emission:
- `main` is emitted as `lugha_fn_main` (spec §8 prefix).
  - With return type `i32`: its `i64` tail value is truncated to `i32` and returned. The process exit code is the low 8 bits (spec §11).
  - With no return type: any tail is evaluated and discarded, then it returns `void`.
- A C entry point `define i32 @main()` calls `lugha_fn_main` and returns its result, or `0` for a `void` main. `GC_INIT()` is added in milestone 5, when the GC is first used. The runtime call in milestone 4.

Arithmetic, until milestone 4:
- `+ - *` and unary `-` (as `0 - x`) use plain `add` / `sub` / `mul` with no `nsw`/`nuw` flags. They wrap, which is defined behaviour.
- `/` and `%` guard first:
  - `divisor == 0` or (`dividend == i64::MIN` and `divisor == -1`) branches to a block that calls `llvm.trap` and ends in `unreachable`.
  - Otherwise `sdiv` / `srem` run.
  - Both cases are undefined behaviour in LLVM, so the guard is required for both operators.

Everything else returns `CodegenError::Unsupported`, with the span of the first unsupported node:

| Construct | `what` | Milestone |
| --- | --- | --- |
| Any function other than `main`, or `main` with parameters | `functions other than main` / `parameters` | 2 |
| `main` returning anything but `i32` or nothing | `this return type for main` | 3 |
| Any statement | `` `let` statements ``, `assignments`, `` `while` loops ``, `` `for` loops ``, `` `return` ``, `` `break` ``, `` `continue` ``, `expression statements` | 2 |
| Names, calls, `if`, block expressions, `true`/`false`, comparisons, `&&`, `\|\|`, `!` | e.g. `variables`, `function calls`, `` `if` expressions `` | 2 |
| Floats, casts | `floats`, `` `as` casts `` | 3 |
| Strings, `extern fun` | `strings`, `extern functions` | 4 |
| Structs, arrays, indexing, fields | `structs`, `arrays`, … | 5 |

**Linking** — `src/link.rs`
- `pub fn link(objects: &[&Path], output: &Path) -> Result<(), LinkError>` runs `cc <objects> -lgc -lm -o <output>`. That is spec §9 stage 6, minus `lugha_rt.o` until milestone 4.
- `pub enum LinkError` (thiserror):
  - `CcNotFound` — "C compiler `cc` not found; install gcc or clang".
  - `Io(std::io::Error)`.
  - `Failed { status: Option<i32>, stderr: String }`, which includes `cc`'s stderr.

**Spec and docs**
- Spec §11 milestone table: remove "overflow checks" from milestone 3; add "integer overflow and division checks (panics)" to milestone 4.
- CLAUDE.md KNOWN ISSUES — DO NOT FIX: "Until milestone 4, integer `+ - *` wrap and `/ %` by zero or `MIN / -1` trap with SIGILL. Specified panics arrive with `lugha_rt_panic`."

### Must NOT Do
- No checker, no `i32`/`u8`/`f64` types, no overflow panics (milestone 4).
- No CLI changes. `src/main.rs` stays a stub, and `tests/programs.rs` stays `#[ignore]` (driver PRP).
- No `lugha_rt.c`, no `GC_INIT`.
- No `unsafe` code. inkwell's safe API only.
- No new dependencies. No changes to the lexer, parser or AST.

## ERROR HANDLING REQUIREMENTS

- Every public function returns `Result`.
- Unsupported input is an `Err`, never a panic.
- inkwell builder methods return `Result<_, BuilderError>`. These fail only when the builder isn't positioned, which is a compiler bug, so use `.expect("builder positioned in a block")` with that invariant stated.
- A verifier failure is `CodegenError::Verify` with LLVM's message. The driver will map it to exit 2.
- `link` reports a missing `cc` distinctly from a failing `cc`.

## SECURITY CONSIDERATIONS

- `cc` is invoked with `Command::args`, never through a shell, so paths are never interpreted by `sh`.
- Output paths come from the caller; this module writes only the object file and the executable it is given.
- No `unsafe`. inkwell's safe API only.

## TESTS TO WRITE

Unit tests (`src/codegen/`):
- [x] IR for `fun main(): i32 { 2 + 3 * 4 }` contains `define i32 @main()` and `@lugha_fn_main`, and passes verification.
- [x] IR for arithmetic has no `nsw`/`nuw`.
- [x] IR for `/` contains a call to `llvm.trap`.
- [x] A `void` main's C `main` returns 0.
- [x] Unsupported: `let`, a second function, `extern fun`, `struct`, a string, a float, `if`, and a call each give `Unsupported` with the milestone from the table and a span slicing to the right source text.

Integration (`tests/codegen.rs`, building into a temp dir removed on drop):
- [x] `2 + 3 * 4` exits 14 (the milestone 1 done-when, through the library).
- [x] `-7 / 2` exits 253 (−3 truncates toward zero); `7 % -3` exits 1 (sign of dividend); `-(3 - 10)` exits 7.
- [x] Wrapping: `(9223372036854775807 + 1) / 4611686018427387904` exits 254 (`MIN / 2^62 = -2`).
- [x] `1 / 0`, `1 % 0` and `(0 - 9223372036854775807 - 1) / -1` are killed by SIGILL.
- [x] `fun main() { }` and `fun main() { 5 }` exit 0.
- [x] Every case gives the same result at `-O0` and `-O2`.
- [x] `link` with a missing object file returns `LinkError::Failed` with non-empty stderr.

## ROLLBACK PLAN

- Branch `prp-004-codegen_and_link`, merged into `main` on acceptance.
- To abandon: delete the branch. No migrations or runtime state.

## ACCEPTANCE CRITERIA
- [ ] Every test above exists and passes.
- [ ] Spec §11 and CLAUDE.md KNOWN ISSUES updated.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass.
- [ ] Every `pub` item documented; functions returning `Result` document `# Errors`.
- [ ] CLAUDE.md FILE ORGANIZATION, CHANGELOG.md, TODO.md updated.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo test -- --ignored` still fails only on `m1/arith.la` (lughac not wired yet; driver PRP).
