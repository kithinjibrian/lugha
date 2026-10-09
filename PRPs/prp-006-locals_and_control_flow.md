## FEATURE: Compile locals and control flow inside `main` — `let`/`let mut`, assignment, blocks with tail values, `if` expressions, `while`, `for` over ranges, `break`/`continue`, comparisons and boolean operators.

**Status:** implemented 2026-10-09 — session 9 (branch `prp-006-locals_and_control_flow`)
**Milestone:** 2, first of two PRPs (the second, PRP-007, adds functions and reaches the §11 done-when)
**Spec:** §3 (statements), §5 (evaluation order, blocks and tails, `if`, `while`, `for` desugaring, `break`/`continue`, `&&`/`||`), §7 (stack locals), §9 (lowering notes: short-circuit and `if` with `phi`)
**Decisions:** CLAUDE.md rule 9 (milestones 1–2: every integer is `i64`)

## OBJECTIVE
Programs whose `main` uses variables, loops and branches compile and run. For example, summing the even numbers below 10 with a `for` loop and an `if` exits with 20. Mistakes that only the milestone 3 checker can report properly stop compilation with a "not implemented yet: … (milestone 3)" message pointing at the code. lughac never panics and never emits invalid IR.

## CONTEXT

- Starting state: codegen compiles one `main` whose body is a tail integer expression (`src/codegen/{mod,lower,expr}.rs`). Statements, names, comparisons, `if` and blocks return `Unsupported` (milestone 2).
- Ending state:
  - Codegen supports everything in this PRP inside `main`.
  - Files are split up front to stay under 300 lines: `codegen/{mod,lower,value,scope,expr,control,stmt}.rs`.
  - New `tests/programs/m2/` acceptance programs.
  - CLAUDE.md KNOWN ISSUES is updated.
- Related existing code: `src/codegen/*`, `src/ast/*`, `tests/codegen.rs`, `tests/programs/`.
- Open decisions that must be resolved first: none.

### Amendments during implementation (session 9)
- **More `Unsupported` cases:**
  - An assignment target that isn't a place gives `checking assignment targets` (milestone 3).
  - `fun main(): i32` whose block ends without a tail keeps the milestone 1 "`main` without a result value" (milestone 3).
- **Error span for a `let` with a void initialiser:** the initialiser expression, e.g. `if true { }`.
- **Updated earlier tests:**
  - `tests/cli.rs` now uses `return` as its "not implemented" example, since `let` compiles.
  - The codegen unit tests now expect `y + 1` to give "checking undefined names" instead of "variables".

### Discovery answers (session 9)
1. Milestone 2 is split into PRP-006 (locals and control flow in `main`) and PRP-007 (functions, calls, recursion, `return`; done-when 55).
2. Booleans: codegen tracks two value kinds, `Int` (`i64`) and `Bool` (`i1`). Mixing them is `Unsupported("type checking", 3)`.
3. Program mistakes before the checker: codegen stops where it must (`Unsupported`, milestone 3, with a span), and compiles what it can. Assigning to a non-`mut` variable compiles until milestone 3 — a known issue.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Values** — `codegen/value.rs`
- `enum Value<'ctx> { Int(IntValue /* i64 */), Bool(IntValue /* i1 */), Void }`.
- `Void` is the value of statements, blocks without a tail, `if` without `else`, and loops.
- Helpers: `expect_int(span)` and `expect_bool(span)`, which return `Unsupported("type checking", 3, span)` on the wrong kind.

**Scopes and locals** — `codegen/scope.rs`
- A stack of block scopes mapping names to `Local { ptr, kind: Int | Bool }`.
- Every local gets an `alloca` in the function's entry block (spec §7); `mem2reg` promotes them at `-O2`. Loop variables are allocated once in the entry block, not inside the loop.
- `let x = e;`: `e` is evaluated before `x` comes into scope, so `let x = x + 1;` reads the outer `x` (spec §5). Shadowing is allowed in the same and nested blocks.
- A name that isn't found is `Unsupported("checking undefined names", 3, span)`.
- Annotations:
  - `i32`, `i64` and `u8` are treated as integers (rule 9).
  - `bool` is a boolean; a mismatch with the initialiser's kind is `type checking`.
  - `f64` and `string` give `Unsupported` with milestones 3 and 4.
  - Array types and struct names give `Unsupported` with milestone 5.

**Expressions** — `codegen/expr.rs`, extending milestone 1
- Integers keep the milestone 1 behaviour: wrapping `+ - *`, and the trap guard for `/ %`.
- Comparisons:
  - `< <= > >=` on `Int` produce `Bool` (signed `icmp`).
  - `==` and `!=` work on two `Int`s or two `Bool`s.
  - Any other combination is `type checking`.
- `true`/`false` are `Bool`. `!` takes a `Bool`.
- `&&` and `||` short-circuit: the right operand is in its own block and a `phi` joins the result (spec §9).
- Names load from their local's alloca.

**Control flow** — `codegen/control.rs`
- Block expressions (`{ … }`): push a scope, run the statements, and the value is the tail's (or `Void`); pop the scope.
- `if c { a } else { b }`:
  - The condition must be `Bool`.
  - Each branch gets its own basic block, and a non-`Void` result merges with a `phi`.
  - Both branches must have the same kind, otherwise `type checking`.
  - Without `else`, the value is `Void`.
- `while c { body }`: the condition block is evaluated before each iteration.
- `for i in a..b { body }` follows the spec §5 desugaring:
  - `a` and `b` are evaluated once, before the loop; both must be `Int`.
  - `i` is a fresh immutable binding each iteration.
  - The step `__i += 1` (wrapping) runs on `continue` as well.
- `break` and `continue` jump to the innermost loop's exit or step block. Outside a loop they give `Unsupported("checking `break` outside loops", 3)` (likewise for `continue`).
- After `break` or `continue`, code that follows in the same block is unreachable. It is lowered into a fresh block so the IR stays valid.
- `for x of xs` gives `Unsupported("arrays", 5)`.

**Statements** — `codegen/stmt.rs`
- `let` / `let mut`: the initialiser must not be `Void`, otherwise `type checking`.
- Assignment: the place must be a local name. Field or index places are `Unsupported`, milestone 5.
  - Assigning to a non-`mut` binding compiles — a known issue until milestone 3.
  - The value's kind must match the local's.
  - Compound `+= -= *= /=` are `Int` only and evaluate the place once (spec §5). `/=` uses the trap guard.
- Expression statements are evaluated and the value discarded. The `semicolon` flag doesn't change codegen.
- `return` is `Unsupported("`return`", 2)` — PRP-007.

**`main`**
- Still the only function, as in milestone 1, with the same parameter and return-type rules.
- With `: i32`, the body's tail must be `Int`. It is truncated and returned as before. A `Bool` or `Void` tail is `type checking`.
- A `void` main may have any tail; it is discarded.

**Docs**
- CLAUDE.md KNOWN ISSUES: "Until milestone 3, assigning to a non-`mut` variable compiles, and type and name errors that codegen hits report as 'not implemented yet: … (milestone 3)' with exit 2."

**Acceptance programs** — `tests/programs/m2/`. All must exit as stated at `-O0` (the runner) and are cross-checked at `-O2` in `tests/codegen.rs`.
- `even_sum.la`: `for` + `if` + `+=` → 20.
- `while_count.la`: `let mut n = 10; let mut count = 0; while n > 0 { n -= 1; count += 1; } count` → 10.
- `shadowing.la`: `let x = 1; let y = { let x = x + 10; x }; let x = x + y; x` → 12 (inner `x` is 11, outer `x` untouched, then shadowed in the same block).
- `short_circuit.la`: `false && (1 / 0 == 0)` must not trap; exit 7 from a following expression.
- `nested_loops.la`: `break` and `continue` in nested `for`/`while` → a known sum.
- `if_value.la`: `let v = if c { 3 } else if d { 4 } else { 5 };` → 4.
- `for_bounds_once.la`: the bound is a variable reassigned inside the body; the loop still runs the original number of times.
- Internal errors (exit 2) aren't `tests/programs` cases. They're covered by the unit tests and `tests/cli.rs`.

### Must NOT Do
- No functions other than `main`, no calls, no `return` (PRP-007).
- No floats, strings, arrays or structs. No real types — `i32`/`u8` are just `i64`.
- No E-codes for checker errors (milestone 3).
- No changes to the lexer, parser, AST, driver or link step. No new dependencies, no `unsafe`.

## ERROR HANDLING REQUIREMENTS

- Every unsupported or pre-checker situation is an `Err(CodegenError::Unsupported { what, milestone, span })`, with the span of the offending node. Never a panic, never invalid IR.
- A `module.verify()` failure stays `CodegenError::Verify` (exit 2, compiler bug). Tests run with verification on every program.
- Builder errors stay `.expect(POSITIONED)`. After a terminator (`br`, `unreachable`) the builder must always be repositioned before more instructions; a test covers code after `break`.

## SECURITY CONSIDERATIONS

- Input is a parsed AST from untrusted source. Codegen recursion follows the AST depth, which the parser already bounds (E0206, 256 levels).
- No `unsafe`.

## TESTS TO WRITE

Unit tests (`src/codegen/`):
- [x] Value kinds: `if 5 {}`, `1 + true`, `let b: bool = 1;`, `if c { 1 } else { true }` each give `type checking` (milestone 3) with the right span.
- [x] `y + 1` with no `y` gives `checking undefined names` with the span `y`.
- [x] `break;` outside a loop gives `checking `break` outside loops`.
- [x] `return 1;` gives `` `return` `` (milestone 2). `for x of xs` gives arrays (5). `let s = "a";` gives strings (4). `pts[0] = 1;` gives milestone 5.
- [x] IR: locals are allocas in the entry block; `&&` produces a `phi`; IR verifies after code following `break`.

Integration (`tests/codegen.rs`, at `-O0` and `-O2`):
- [x] Every `tests/programs/m2/` program gives the same exit code at both levels.
- [x] Assigning to a non-`mut` variable compiles and runs (the documented known issue).
- [x] `let x = x + 1;` reads the outer `x`.

Acceptance:
- [x] All `tests/programs/m2/` cases pass through `lughac run`.

## ROLLBACK PLAN

- Branch `prp-006-locals_and_control_flow`, merged into `main` on acceptance.
- To abandon: delete the branch. Milestone 1 behaviour is unchanged, so the `m1/` tests guard against regressions.

## ACCEPTANCE CRITERIA
- [ ] Every test above exists and passes; all `m1/` tests still pass.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass.
- [ ] Every `pub` item documented.
- [ ] CLAUDE.md KNOWN ISSUES and FILE ORGANIZATION, CHANGELOG.md, TODO.md updated.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo run -q -- run tests/programs/m2/even_sum.la; echo $?` → `20`
