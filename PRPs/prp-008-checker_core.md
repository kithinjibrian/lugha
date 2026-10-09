## FEATURE: The type checker core — name resolution, types for every expression with literal inference and operator typing, calls, and `lughac check` running it — reaching the milestone 3 done-when (E0401 in both formats).

**Status:** approved 2026-10-09 — session 11
**Milestone:** 3, first of three PRPs. PRP-009 adds casts and flow checks (E05xx, W0101, "remove this semicolon"); PRP-010 moves codegen onto real types.
**Spec:** §4 (types, inference rules 1–6, operator typing, value semantics of `void`), §5 (`if` typing, blocks and tails), §6 (names, two-pass collection, entry point, definitely-returning blocks), §9 (checker output: AST + ExprId → Type table; diagnostics), §10 (rejected program)
**Decisions:** DECISION-002 (stage results); CLAUDE.md rule 9 (the checker covers the milestone 3 subset only)

## OBJECTIVE
`lughac check` and `lughac build` run a real type checker between parsing and codegen. The §10 rejected program prints exactly the spec's E0401 output, human and JSON, with exit 1. Every expression gets a type (`i32`, `i64`, `u8`, `f64`, `bool`, `void`), recorded in a table keyed by `ExprId` for PRP-010's codegen. One mistake gives one diagnostic, never a cascade.

## CONTEXT

- Starting state:
  - The pipeline is lex → parse → codegen.
  - `lughac check` only lexes and parses.
  - Codegen's interim `Int`/`Bool`/`Never` kinds stop type and name mistakes with "not implemented yet: … (milestone 3)".
- Ending state:
  - New `src/check/{mod,types,env,expr,ops,stmt,errors}.rs`.
  - The driver runs the checker after parsing in `check`, `build` and `run`.
  - New `tests/programs/m3/` reject-mode cases, including the spec §10 program.
  - Spec §9 gains the E03xx/E04xx table.
  - Codegen is unchanged; its milestone 3 stops remain for what PRP-009 and PRP-010 cover.
- Related existing code: `src/ast/`, `src/diagnostic.rs`, `src/driver/pipeline.rs`, `src/codegen/` (unchanged).
- Open decisions that must be resolved first: none.

### Discovery answers (session 11)
1. Milestone 3 is three PRPs: 008 checker core, 009 casts and flow checks, 010 codegen on real types.
2. Recovery: a `Type::Error` that is compatible with everything and never reported. One diagnostic per mistake.
3. Constructs from milestones 4 and 5 (strings, intrinsics, `extern`, arrays, structs): the checker stops with "not implemented yet: X (milestone N)", exit 2, so `check` and `build` agree.
4. Codes: E0301–E0305 and E0401–E0407, below.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**API** — `src/check/mod.rs`
- `pub fn check(program: &Program) -> Result<(Checked, Vec<Diagnostic>), CheckError>`.
- `pub struct Checked { pub types: Vec<Type> }` is indexed by `ExprId`, one entry per expression (`Program::expr_count`).
- `pub enum CheckError`:
  - `Program(Vec<Diagnostic>)` — every E03xx/E04xx found; exit 1.
  - `Unsupported { what, milestone, span }` — a milestone 4 or 5 construct; exit 2. The checker stops at the first.

**Types** — `src/check/types.rs`
- `pub enum Type { I32, I64, U8, F64, Bool, Void, Never, Error }`, with `Display` (`i32`, …, `void`).
- `Never` is the type of a block that definitely diverges (`return`, `break`, `continue`, or both branches of `if`/`else`). It fits any expected type (§6).
- `Error` fits everything and is never reported.

**Names and globals** — `src/check/env.rs` (spec §6)
- Pass 1 collects every `fun` signature. `struct` and `extern` stop with milestone 5 and 4.
- A name defined twice, or a `fun` named like an intrinsic, is E0302, with a label on the first definition.
- `main`:
  - missing → E0303
  - parameters or a return type other than `i32` → E0304
- Scopes resolve block → parameters → globals; locals shadow functions.
- Annotation types:
  - `i32`/`i64`/`u8`/`f64`/`bool` resolve to themselves.
  - `string` → milestone 4.
  - Arrays → milestone 5.
  - An unknown name → E0305 "cannot find type `X`".

**Expressions** — `src/check/expr.rs`, `src/check/ops.rs`. Bidirectional: `check(expr, expected) -> Type`, where `expected` optionally carries a type and a reason label.
- **Literal inference (§4 rules 3–6).** Integer and float literals take the expected type from:
  - a `let` annotation
  - a parameter type at a call
  - the declared return type (tail or `return`)
  - the other operand of a binary operator
  - the other bound of a range
  - an assignment target

  Without one, integers default to `i64` and floats to `f64`.
- **Literal checks:**
  - An integer literal where `f64` or `bool` is expected, or a float literal where an integer or `bool` is expected, is **E0401** "{integer|float} literal where T expected". The primary label is "expected T"; the secondary label is the reason, e.g. "this operand is i32".
  - **E0402**: an integer literal that doesn't fit its type, after folding a directly applied `-` (rule 6). A negated `u8` literal is E0402.
- **Binary operators.** When exactly one operand is a literal, possibly negated, the other operand is checked first and gives the literal its type, with the reason label on that operand. This reproduces the spec §10 output exactly. Otherwise the left operand is checked first, then the right with the left's type expected.
- **Operator typing (§4 table):**
  - `+ - * / %`: two numbers of the same type → that type.
  - `< <= > >=`: same numeric type → `bool`.
  - `== !=`: two values of the same primitive type → `bool`.
  - `&& ||`: `bool` → `bool`.
  - Unary `-` on `i32`/`i64`/`f64`; `!` on `bool`.
  - Anything else is **E0404** "cannot apply `op` to T [and U]".
- **Names:**
  - a local → its type
  - a function name used as a value → **E0406** "`f` is a function, not a value"
  - an intrinsic name → milestone 4
  - unknown → **E0301** "cannot find `x` in this scope"
- **Calls:**
  - The callee must be a function name. A local → **E0406** "`f` is not a function"; another callee expression → E0406; an intrinsic → milestone 4.
  - A wrong argument count is **E0405** "`f` takes N arguments but M were given".
  - Arguments are checked with the parameter type expected.
  - The result is the return type, or `void`.
- **Void as a value:** a `void` result used as a value — in `let` initialisers, operands, arguments, conditions or returns — is **E0407** "this expression has no value".
- **`if` (§5):**
  - The condition expects `bool`.
  - With `else`, both branches get the outer expected type. `Never` unifies with anything; otherwise the types must be equal, or **E0403** "`if` and `else` have different types" with a label on each branch.
  - Without `else`, the type is `void`.
- **Blocks:**
  - Each block gets its own scope.
  - The type is the tail's, `void` without a tail, or `Never` once a statement diverges.
- **Later milestones:** `as` casts stop with "not implemented yet: `as` casts (milestone 3)" until PRP-009 lands. Strings, field access, indexing, struct literals and arrays stop with milestones 4/5.
- Every checked expression's type is written to `types[id]`.

**Statements and functions** — `src/check/stmt.rs`
- **`let`:**
  - The annotation, if any, is expected for the initialiser; a mismatch is **E0403** "expected T, found U".
  - Without an annotation, the type is the initialiser's. `void` → E0407; `Never` or `Error` → the binding is `Error`, so nothing cascades.
  - The binding enters scope after the initialiser (§5).
- **Assignment:**
  - A `Name` place: the value expects the place's type. Compound operators need a numeric place (E0404).
  - Field and index places stop with milestone 5.
  - Other places, and mutability, are PRP-009. Until then they're type-checked only, and codegen still stops on them.
- **Loops and jumps:**
  - `while`: the condition expects `bool`.
  - `for i in a..b`: both bounds share one integer type, inferred between them; both literals → `i64`. A non-integer bound is E0403. `i` gets that type.
  - `for … of` → milestone 5.
  - `return e;` expects the function's return type. `return;` in a non-void function is E0403 "expected T, found void". `return e;` in a void function is E0403 "expected void, found T".
  - `break` and `continue` diverge. Checking that they're inside a loop is PRP-009.
- **Function bodies:** the tail expects the return type. A void function's tail is not required to be `void` until PRP-009's semicolon rule. Missing returns are PRP-009.

**Errors** — `src/check/errors.rs`: one constructor per code, so message wording lives in one place.

**Driver** — `src/driver/pipeline.rs`
- `front()` runs `check` after `parse`.
- `CheckError::Program` → `Failure::Program` (exit 1).
- `CheckError::Unsupported` → `Failure::Internal` with the span (exit 2).
- `--emit=tokens` and `--emit=ast` stay before the checker. `--emit=ir` runs after it.
- `lughac check` now type-checks. Update the CLI help text, and drop the CLAUDE.md known issue "`lughac check` only lexes and parses".

**Spec** — §9: a table of E0301–E0305 and E0401–E0407, with one example each. §11 milestone 3 is unchanged.

**Acceptance** — `tests/programs/m3/`, reject mode:
- `e0401.la`: the spec §10 program. `.stderr` is the spec's output with the test path, written by hand from the spec.
- One program per code: E0301, E0302, E0303, E0304, E0305, E0402, E0403, E0404, E0405, E0406, E0407. Their `.stderr` files are captured from the first passing run and reviewed line by line.
- `multiple_errors.la`: three independent mistakes in one file give exactly three diagnostics, with no cascade from `Error`.
- `tests/cli.rs`: `--diagnostics=json` on `e0401.la` gives exactly the spec §9 JSON line (with `"file"` set to its path).

### Must NOT Do
- No casts, mutability, missing-return, loop-context or semicolon checks, and no W0101 (PRP-009).
- No codegen changes (PRP-010). The type table is produced but not yet consumed.
- No string, intrinsic, `extern`, array or struct typing (milestones 4/5).
- No new dependencies, no `unsafe`, and no lexer, parser or AST changes.

## ERROR HANDLING REQUIREMENTS

- Diagnostics are collected, never thrown. The checker visits every function even after errors.
- `Type::Error` suppresses follow-on diagnostics: no message is produced when any operand or expected type is `Error`.
- The checker never panics on any parsed program (AST depth is bounded by the parser's E0206).

## SECURITY CONSIDERATIONS

- Input is an AST from untrusted source. Recursion follows AST depth, which is bounded.
- No `unsafe`.

## TESTS TO WRITE

Unit tests (`src/check/`), as (code, spanned text) pairs:
- [ ] E0301 variable and function; locals shadow functions; `let x = x + 1` uses the outer `x`.
- [ ] E0302 duplicate `fun` (label on the first); `fun println()`. E0303. E0304 for parameters and an `i64` return. E0305.
- [ ] Inference:
  - `let a = 5` is i64; `let b: i32 = 5` is i32; `b + 1` is i32; `1 + b` is i32 (literal on the left).
  - `let d = 2.5` is f64.
  - Range bounds `0..n` with `n: i32` give `i: i32`.
  - Call arguments and the return type flow into literals.
- [ ] E0401 with the exact spec message, labels and spans; `let e: f64 = 5;`; `if 5 {}` (integer literal where bool expected).
- [ ] E0402: `let b: u8 = 256;`, `let c: u8 = -1;`; `-2147483648` fits i32 but `2147483648` doesn't.
- [ ] E0403: let annotation, argument, return, branches (both labels), assignment.
- [ ] E0404: `true + 1`, `-b` on bool, `!1`, `-x` on u8, `1 == true` (U ≠ T), `x + y` with i32 and i64.
- [ ] E0405, E0406 (both forms), E0407 (`let x = noop();`, `noop() + 1`).
- [ ] `if x < 0 { return 0; } else { x }` is i64 (Never unifies). No error cascades from an undefined name.
- [ ] The type table is filled for every expression of a valid program (no default entries remain).
- [ ] Milestone 4/5 constructs stop with the right milestone (`"s"` → 4, `println(1)` → 4, `p.x` → 5, `[1]` → 5, `extern` → 4, `struct` → 5).

Integration and acceptance:
- [ ] All `tests/programs/m3/` cases pass, including the spec E0401 byte-for-byte.
- [ ] `tests/cli.rs`: the exact spec JSON line; `lughac check` exits 1 on a type error and 0 on all `m1/` and `m2/` programs.
- [ ] All existing tests pass: every `m1/` and `m2/` program type-checks cleanly.

## ROLLBACK PLAN

- Branch `prp-008-checker_core`, merged into `main` on acceptance.
- To abandon: delete the branch. The driver change is a single call site.

## ACCEPTANCE CRITERIA
- [ ] `lughac check tests/programs/m3/e0401.la` prints the spec §10 output (with its path) and exits 1. `--diagnostics=json` prints the spec §9 line — **milestone 3 done-when**. The milestone itself closes after PRP-010.
- [ ] Every test above exists and passes.
- [ ] Spec §9 code table added; CLAUDE.md known issues and file tree, CHANGELOG.md and TODO.md updated.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo run -q -- check tests/programs/m3/e0401.la; echo $?` → the spec output, then `1`
