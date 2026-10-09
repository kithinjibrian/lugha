## FEATURE: Checker rules for casts, mutability, places, missing returns, loop context, block-like statements and unreachable code — E0408, E0501–E0505, W0101, and "remove this semicolon".

**Status:** merged into `main` 2026-10-09 — session 12
**Milestone:** 3, second of three PRPs (PRP-010 moves codegen onto real types and closes the milestone)
**Spec:** §3 (place expressions), §4 (casts, mutability), §5 (block-like statements, "remove this semicolon", `break`/`continue`), §6 (definitely-returns rules, unreachable code), §9 (codes)
**Decisions:** PRP-008 checker design (`Expect`, `Type::Error`, `Never`)

## OBJECTIVE
The checker enforces the rest of milestone 3's rules, so every mistake in the milestone 3 subset gets a proper coded diagnostic instead of codegen's "not implemented yet: … (milestone 3)":
- reassigning an immutable variable
- assigning to something that isn't a place
- a function that can fall off its end
- a stray `;` after a function's result
- `break` outside a loop
- a discarded `if` value
- an invalid cast

Dead code after `return` gets one warning, without failing the build.

## CONTEXT

- Starting state: `src/check/` (PRP-008) reports E0301–E0305 and E0401–E0407. Casts stop with "not implemented yet: `as` casts (milestone 3)". Assignment to non-`mut` bindings, non-places, `break` outside loops and missing returns pass the checker; codegen stops on the last three. `check/stmt.rs` is 270 lines.
- Ending state:
  - New `check/flow.rs` (returns, semicolons, mid-block values, unreachable code) and `check/assign.rs` (mutability, places).
  - Casts in `check/ops.rs`; `Local` records how a name was bound.
  - `check()` returns warnings.
  - New `m3/` cases; spec §9 table extended; CLAUDE.md known issues updated.
- Related existing code: `src/check/*`, `src/driver/pipeline.rs` (already prints warnings), `src/diagnostic.rs`.
- Open decisions that must be resolved first: none.

### Amendments during implementation (session 12)
- **"Remove this semicolon" fires when the last `expr;` has any value type**, not only one that fits the wanted type. A statement expression is checked with no expected type, so `{ 1 + 1; }` has an `i64` statement even where `i32` is wanted; requiring an exact fit would have hidden the help in exactly the case §5 describes.
- **E0502's span** is the place expression without its parentheses (`a + b`), because the AST doesn't keep them.
- **Goldens:** all eight new `.stderr` goldens and the W0101 run-mode golden were captured on the first passing run and reviewed.

### Discovery answers (session 12)
1. Codes:
   - E0408 invalid cast
   - E0501 assignment to immutable
   - E0502 not a place
   - E0503 missing return
   - E0504 `break`/`continue` outside a loop
   - E0505 non-void block-like statement mid-block
   - W0101 unreachable code

   "Remove this semicolon" is a help note plus a label on the `;`, attached to the error that fires.
2. W0101: once per block, on the first unreachable statement or tail, with a label on the statement that caused it. It's printed by `check`, `build` and `run`, never changes the exit code, and isn't repeated inside code that's already unreachable.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Casts** — `check/ops.rs` (spec §4)
- `e as T`:
  - `T` resolves as an annotation (string → milestone 4, arrays → milestone 5, unknown → E0305).
  - `e` is checked as a value with no expected type, so literals default to `i64`/`f64`.
  - Both must be numeric (`i32`, `i64`, `u8`, `f64`); the result is `T`.
  - A `bool` on either side is **E0408** "cannot cast `bool` to `i32`" (or the reverse), with help "write `if b { 1 } else { 0 }`" when casting from `bool`.
- Casting to the same type is allowed.

**Bindings** — `check/mod.rs`, `env.rs`
- `Local { ty, binding: Binding, span }`, where `Binding` is `Let { mutable: bool }`, `Param` or `LoopVar`, and `span` is the name's declaration span.

**Assignment** — new `check/assign.rs`; moves `assign` out of `stmt.rs`
- A place is a name, a field access or an index (§3). Field and index places still stop with milestone 5. Anything else — `1 = 2`, `f() = 3`, `(a + b) += 1` — is **E0502** "cannot assign to this expression" at the place. Its value is still checked.
- Assigning to a name whose root binding isn't `let mut` is **E0501** "cannot assign to `x`: it is not mutable". A label at the declaration says "declared here". The help depends on the binding:
  - `let`: "make it mutable: `let mut x`"
  - parameter: "parameters are immutable; shadow it: `let mut x = x;`"
  - loop variable: "loop variables are immutable"
- Compound assignment follows the same rules.

**Control flow** — new `check/flow.rs`, called from `block` and `function`
- `Checker` tracks a loop depth (`while`/`for` bodies) and a `dead: bool` flag.
- **E0504**: `break` or `continue` with loop depth 0. The statement still diverges, as before.
- **E0505** (§5): a block-like statement (`StmtKind::Expr` with `semicolon: false`) that isn't the block's tail must have type `void`, `Never` or `Error`. Otherwise: "this `if` has a value of type T that is discarded", with help "add `;` to discard it, or make it the block's last expression".
- **E0503**: the body of a non-void function has type `void`, i.e. neither a tail value nor a definite return (the §6 rules, already encoded by `Never`).
  - Reported at the function name with "`f` may end without returning a value of type T".
  - The help explains §6, and suggests `panic("unreachable")` when the body contains a loop.
  - This replaces PRP-008's silent skip.
- **"Remove this semicolon"** (§5): when the block has no tail and its last statement is an expression statement with `;` whose expression type fits the wanted type, add a label "remove this semicolon" on the `;` (the last byte of the statement's span) and help "remove this semicolon to make it the result". It applies to:
  - E0503, for function bodies
  - E0407, when a block expression is used as a value (`let v: i32 = { x * x; };`)
- **W0101** (§6): in a block, after a statement that diverges, the next statement or the tail gets **W0101** "unreachable code". Its primary span is that statement; a label on the diverging statement says "any code after this never runs".
  - Once per block.
  - While checking code after a divergence, `dead` is set, so nested blocks inside dead code report no W0101.

**Warnings out of `check()`**
- If any diagnostic is an error: `Err(CheckError::Program(all diagnostics, warnings included))`.
- Otherwise: `Ok((Checked, warnings))`.
- The driver already prints warnings on success.

**Spec** — §9: add rows E0408, E0501–E0505 and W0101 (with examples) to the checker table.

**CLAUDE.md** — KNOWN ISSUES: replace the "Until PRP-009, assigning to a non-`mut` variable …" entry with "Until PRP-010, codegen keeps its interim `Int`/`Bool`/`Never` kinds and stops on `f64`, casts and non-`i64` integers with 'not implemented yet: … (milestone 3)'".

**Acceptance** — `tests/programs/m3/`
- Reject mode, one program each: `e0408.la`, `e0501.la`, `e0501_param.la`, `e0502.la`, `e0503.la`, `e0503_semicolon.la`, `e0504.la`, `e0505.la`. Goldens are captured on the first passing run and reviewed line by line.
- `w0101.la` in run mode:
  - `fun main(): i32 { return 3; let x = 1; }` (or similar) with `.exit` 3 and empty `.stdout`.
  - `.stderr` holds the exact warning lughac prints before running.
  - This proves warnings don't change the exit code.

### Must NOT Do
- No codegen changes (PRP-010). Casts still stop in codegen until then.
- No checker rules for strings, arrays, structs or intrinsics; no `for … of` assignment restriction (milestone 5); no `panic` (milestone 4).
- No new dependencies, no `unsafe`, no lexer/parser/AST changes.

## ERROR HANDLING REQUIREMENTS

- Every new rule reports and keeps going.
- `Type::Error` suppresses follow-on reports. E0501 and E0502 still check the assigned value.
- No panics. The `;` span is computed from the statement span, which always ends in `;` for `semicolon: true` statements (parser invariant).

## SECURITY CONSIDERATIONS

- No new input handling; recursion depth is unchanged (bounded by the parser). No `unsafe`.

## TESTS TO WRITE

Unit tests (`src/check/`), as (code, spanned text):
- [x] E0408: `true as i32`, `x as bool`; `2.5 as i32` and `300 as u8` are OK (truncation is defined); `x as i64` where `x: i64` is OK.
- [x] E0501 for `let`, a parameter and a loop variable, each with the right help; `let mut` and shadowing with `let mut x = x;` are OK; compound assignment.
- [x] E0502: `1 = 2;`, `f() = 3;`, `(a + b) += 1;`.
- [x] E0503:
  - missing `else` return
  - `while true { return 1; }` (§6), with help mentioning `panic("unreachable")`
  - `fun sq(x: i32): i32 { x * x; }`, with the "remove this semicolon" label on the `;`
  - OK: tail; `if/else` both return; early return then tail.
- [x] Semicolon help on E0407: `let v: i32 = { 1 + 1; };`.
- [x] E0504: `break;` and `continue;` outside loops; OK inside nested loops and inside an `if` inside a loop.
- [x] E0505: `if c { 1 } else { 2 }` mid-block. OK: with `;`, as the tail, `void` ifs, `{ }` blocks mid-block.
- [x] W0101: after `return`, after `break`, after a diverging `if/else`, on the tail. Once per block. None nested inside dead code. Warnings come back in `Ok` and don't make the result an error.

Acceptance and CLI:
- [x] All new `m3/` programs pass, including `w0101.la` exiting 3 with the warning on stderr.
- [x] All existing tests still pass (no `m1/`/`m2/` program is affected by the new rules).

## ROLLBACK PLAN

- Branch `prp-009-casts_and_flow_checks`, merged into `main` on acceptance.
- To abandon: delete the branch. The rules are additive to the checker.

## ACCEPTANCE CRITERIA
- [ ] Every test above exists and passes.
- [ ] Spec §9 updated; CLAUDE.md, CHANGELOG.md, TODO.md updated.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo run -q -- check tests/programs/m3/e0503_semicolon.la` shows the "remove this semicolon" label on the `;`
