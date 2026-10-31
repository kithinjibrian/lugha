## FEATURE: `defer` takes an assignment as well as an expression (`defer n += 1;`); part of milestone 151, v17.

**Status:** approved 2026-10-31 ("build"); proposed 2026-10-31, at the user's request after milestone 151 ("allow `defer n += 1;` without braces"). No discovery: the semantics are milestone 151's, unchanged.

## OBJECTIVE
```lugha
fun main() {
    let mut n = 1;
    defer println(n);       // runs last: prints 11
    defer n += 10;          // today: E0204 "assignment is a statement"
    let mut log: string[] = [];
    defer log = [];         // plain `=` too
    println(n);             // 1
}
```

## CONTEXT
- Read first: `src/parser/stmt.rs` (`element`: `defer`, and the assignment branch), `src/ast/mod.rs` (`StmtKind::Defer`), `src/check/defer.rs`, `src/check/liveness/walk.rs` (`deferred`), `src/codegen/control.rs` + `defer.rs`, `src/codegen/hoist_scan.rs`, `src/formatter/stmts.rs`, `src/parser/sexp/mod.rs`.
- Spec: §3 grammar line for `defer`, §5's **defer** paragraph (one sentence), "Changes since v16" bullet for milestone 151. Draft line for 151.
- Decisions: 214 (unchanged; this widens what may follow `defer`). None open. Not security-sensitive; no dependency.

## REQUIREMENTS

### Must Do
- **Grammar:** `"defer" ( block_like [ ";" ] | expr [ assign_op expr ] ";" )`: every assignment operator (`=`, `+=`, … `>>=`).
- **AST:** `StmtKind::Defer(Box<Stmt>)`, the inner statement an `Expr` or `Assign` (a block-like body is an `Expr` statement without `;`).
- **Checking:** the assignment is checked exactly as the statement would be (E0501 on an immutable place, E0403 on a type mismatch, …); it has type `()`, so E0456 never applies to it. E0455 is unchanged.
- **Running:** unchanged: at exit, values read then, with the names in scope at the `defer`. An overflowing `defer n += 1;` panics where it runs, located at its `+=`.
- **Liveness, hoisting, formatter, s-expressions:** walk/print the inner statement; the formatter prints `defer n += 1;`.

### Must NOT Do
- No other statements after `defer` (`let`, `while`, `for`, `return`, …): a `let` would bind nothing anyone sees; loops go in braces.

## DESIGN
No new diagnostics. Codegen lowers the inner statement with `stmt` instead of `expr` (`Deferred` holds a `Stmt`).

## TESTS
- [ ] Parser/formatter: `defer n += 1;`, `defer xs[0] = 2;` round-trip (`src/formatter/tests.rs`).
- [ ] Checker (`src/check/defer.rs`): `defer n += 1;` ok on `let mut`; E0501 on an immutable `n`; E0403 for `defer n = "a";` on an `i64`.
- [ ] `tests/programs/m151/assign.la`: the OBJECTIVE program, and a deferred `+=` in a loop with `break`.

## ACCEPTANCE
- [ ] The OBJECTIVE prints `1` then `11`.
- [ ] `cargo test --lib`, `--test programs`; `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` clean.
- [ ] Spec, Draft and CHANGELOG updated.
