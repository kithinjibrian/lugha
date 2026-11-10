## FEATURE: `defer` runs a statement when its block is left, last first, on every exit but a panic; milestone 151, v17.

**Status:** approved 2026-10-31 ("approved"); proposed 2026-10-31, from TODO's `defer` item and the design note `docs/decisions/defer.md`, after a discovery (7 questions; every answer was the recommended option, except that question 7's recommendation changed during the discovery from the note's contextual word to a keyword): (1) scope: the enclosing block; (2) the whole statement runs at exit, reading values as they are then; (3) several run last first, on every exit (end, `return`, `?`, `break`, `continue`), after a tail or `return` value is computed; leaving a deferred body is an error; (4) panics don't run defers (an actor's panic leaks what its defers would close: a known limit); (5) a deferred statement has type `()`; (6) no `errdefer`; (7) `defer` is a reserved keyword.

## OBJECTIVE
```lugha
import std.fs;

fun first_lines(paths: string[]): Result<string[], string> {
    let mut out: string[] = [];
    for path in paths {
        let f = fs.open(path)?;
        defer f.close();               // at the end of this iteration
        out.push(f.read_line()?);      // an Err here still closes f
    }
    Ok(out)
}

fun main() {
    defer println("c");
    defer {
        println("b");
    }
    let mut n = 1;
    defer println("n = {n}");          // read at exit: 2
    n = 2;
    println("a");
}
// prints: a, n = 2, b, c (one per line)
```
A program that opens and fails to read 10 000 files in a loop through `first_lines` keeps its descriptor count flat.

## CONTEXT
- Read first: `docs/decisions/defer.md` (the research and implementation notes), `src/lexer/token.rs` (keywords), `src/parser/stmt.rs`, `src/ast/mod.rs` (`StmtKind`), `src/check/` (statements, `never`, the exits checks behind E0504), `src/check/liveness/` (milestone 149), `src/codegen/control.rs` (`block_inner`, `Loop`, `jump`), `src/codegen/stmt.rs` (`return`), `src/codegen/results.rs` (`try_value`), `src/codegen/function.rs` and `closures.rs` (a function's tail, closures and `spawn` bodies), `src/codegen/hoist_scan.rs`, `src/formatter/`, `src/driver/lsp/` (keyword completion), `editors/vscode/` (the TextMate grammar).
- Creates: `docs/errors/E0455.md`, `docs/errors/E0456.md`, `tests/programs/m151/`, probably `src/check/defer.rs` and `src/codegen/defer.rs` (kept under 500 lines each).
- Spec: §2 (keywords: `defer` added), §3 (grammar: the statement), §4 (its typing: `()`), §6 (statements: when it runs; the exit rule; panics and `exit` don't run it; the actor limit), §9 (lowering), §11 row 151, "Changes since v16" (with **Breaking**). The v17 Draft first.
- Decisions: the OPEN `defer` entry (2026-10-31) is settled by this discovery and moves to RESOLVED with the seven answers; `errdefer` is added under DEFERRED. DECISION-010 (no reference counts) and DECISIONS v3 (one copy semantics) are why it is a statement, not destructors. Nothing else open.
- Not security-sensitive: no `unsafe`, no runtime C change, no dependency, nothing run.

## REQUIREMENTS

### Must Do
- **Grammar:** `defer` is a keyword (an identifier named `defer` is now a syntax error, the usual one for a keyword). A statement `defer <expression>;` or `defer <block>` (no `;` needed after a block, as for a block statement). Only as a statement in a block; anywhere else, the existing syntax errors. `StmtKind::Defer(Expr)`, a block being a block expression.
- **Typing:** the expression is checked like an expression statement with `()` expected; a type other than `()` (or `never`) is **E0456** at the expression, help: handle it inside (a `match`), or discard it on purpose with a `;` inside braces (`defer { fs.remove(tmp); }`), as any statement does. A block `{ …; }` has type `()`, as today.
- **No leaving:** `return`, `?`, `break` and `continue` inside a deferred statement that would leave it are **E0455** at the jump, help: the block is already being left. A `break`/`continue` of a loop that is itself inside the deferred statement is fine; a closure's `return` or `?` inside it belongs to the closure and is fine. `panic(…)` and `exit(…)` are allowed (they end things, not jump).
- **Running:** at every way out of the block that holds it, after the `defer` statement was reached: the block's end (after its tail is computed), `return` (after its value is computed), `?` on the failing path, `break` and `continue` (every block between the jump and its loop, innermost first). Several defers run last first. A defer not yet reached when the block is left doesn't run. A deferred block that itself holds a `defer` runs that one at its own end.
- **Values at exit:** the deferred statement is evaluated when it runs, reading locals as they are then.
- **Not run by:** a panic (the process ends, or an actor stops: §6 says so and names the leak), `exit(…)`, or `main` returning while other green threads still run (their defers don't run: the program ends, §9's rule). Closures and `spawn` bodies are functions of their own: their defers run when they return.
- **Liveness (milestone 149):** a local mentioned in a deferred statement is used at every exit after the `defer`, so it is never handed over (moved) before then. Simplest correct rule: such a local is not handed over anywhere in that block after the `defer`.
- **Hoisting (`hoist_scan.rs`):** a deferred statement inside a loop counts as part of the loop's body for what it writes.
- **Formatter:** `defer` statements print as written (`defer e;`, `defer { … }`), idempotent.
- **Language server:** `defer` in keyword completion at the start of a statement; hover on it shows nothing new (keywords have none).
- **VS Code:** `defer` in the TextMate keyword list; extension version bumped (minor).
- **Bookkeeping:** spec sections above, CHANGELOG (with **Breaking:** `defer` is a keyword), DECISIONS (RESOLVED entry with the why of each answer; `errdefer` DEFERRED), TODO (the item removed), CLAUDE.md unchanged unless a command changes.

### Must NOT Do
- No unwinding and no run-time defer list: panics don't run defers (answer 4).
- No `errdefer` (answer 6).
- No Go-style early evaluation of arguments (answer 2).
- No function-scoped defer (answer 1).
- No closing by the collector, no finalizers on `File`/`Conn`/`Listener` (the note's reasons).
- No change to `close()` signatures or to the runtime.

## DESIGN
- **Diagnostics:**
  - **E0455** `` `return` inside `defer` `` (likewise `?`, `break`, `continue`): "a deferred statement can't leave its block"; label at the jump, a secondary label at the `defer`; help "it runs while the block is already being left; handle the case inside it instead". Page `docs/errors/E0455.md`.
  - **E0456** "a deferred statement must have type `()`, found `Result<(), string>`"; label at the expression; help for a `Result`: "handle the error, or discard it on purpose: `defer { fs.remove(tmp); }`". Page `docs/errors/E0456.md`.
- **Codegen:** each function's codegen state keeps a stack of scopes, each a list of the deferred statements reached so far in that block (pushed when the `defer` statement is lowered, which emits no code then). A `Loop` records the scope depth at its body. Every exit lowers the pending statements inline, innermost scope first and last first within one: `block_inner`'s end (after the tail value), `return` and a failing `?` (every scope of the function), `jump` (scopes down to the loop's depth). Lowering the same statement at several exits duplicates its code, as Swift and Zig do; a deferred statement is checked once, and the checker's side table (`Checked`) is keyed by expression, so lowering it twice reads the same types. A closure or `spawn` body starts its own scope stack (saved and restored around it). Code after a jump is in a dead block (`start_dead_block`) and lowers no defers.
- **Tail before defers:** the tail's value is held in a local (a register or the frame, through the existing paths) while the defers run, so `defer f.close(); f.read_line()` reads then closes; a `return e` likewise.
- **Frames (precise roots):** a deferred statement's mentions are uses at the exits, so frame slots for the locals it reads stay alive until then; liveness above guarantees it.

## TESTS
- [ ] `tests/programs/m151/`: `order.la` (several defers last first; a nested block's before the outer's), `values.la` (values read at exit), `exits.la` (end, `return`, `?`, `break`, `continue`, each printing the cleanup), `tail.la` (tail and `return` value computed before the defers), `nested.la` (a `defer` inside a deferred block; a `defer` in a closure and in a `spawn`ed call runs when they return), `unreached.la` (a defer after an early `return` doesn't run), `loop_inside.la` (a `break` of a loop wholly inside a deferred block is fine), `files.la` (opens and closes 10 000 files through `first_lines` with a failing read; counts `/proc/self/fd` before and after: equal), `panic.la` (a panic after a defer: the defer's output absent, exit 101).
- [ ] Rejected programs (`.stderr`, exit 1): `leave_return.la`, `leave_try.la`, `leave_break.la`, `leave_continue.la` (E0455), `result.la` and `value.la` (E0456), `name.la` (`let defer = 1;`: the keyword syntax error).
- [ ] Liveness: a local read only in a deferred statement isn't handed over before it (a unit test in `src/check/liveness/tests.rs` or a program whose output shows the value intact).
- [ ] Formatter: `tests/fmt.rs` (or its area file) round-trips both forms.
- [ ] LSP: completion offers `defer` (the existing keyword-completion test file).
- [ ] `tests/explain.rs` covers the two new pages (it checks every code).
- [ ] VS Code: `npm test` passes; the grammar highlights `defer`.

## ACCEPTANCE
- [ ] The OBJECTIVE program prints `a`, `n = 2`, `b`, `c` and exits 0; the descriptor count stays flat.
- [ ] `cargo test --lib`, `--test programs`, `--test explain`, and the touched fmt/lsp test files pass; `cargo fmt --check` and `cargo clippy --all-targets -- -D warnings` are clean.
- [ ] Every `pub` item documented; spec, CHANGELOG, DECISIONS, TODO updated; extension version bumped.
