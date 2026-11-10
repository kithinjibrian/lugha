## FEATURE: `loop { }`, `break` with a value, and labelled `break`/`continue` (milestone 152)

## OBJECTIVE

```
fun next_id(taken: Set<i64>): i64 {
    let mut id = 0;
    loop {
        id += 1;
        if !taken.has(id) {
            return id;
        }
    }
}                                   // no panic("unreachable") needed: the loop never ends

fun main() {
    let grid = [[1, 2], [3, 4]];
    let found = search: loop {
        for row of grid {
            for x of row {
                if x == 3 {
                    break search x * 10;
                }
            }
        }
        break 0;
    };
    println(found);                 // 30
    println(next_id(#{1, 2}));      // 3

    outer: for i in 0..3 {
        for j in 0..3 {
            if j > i {
                continue outer;
            }
            print("{i}{j} ");
        }
    }
    println("");                    // 00 10 11 20 21 22
}
```

## DISCOVERY

Settled by Claude's judgement at the user's request (2026-10-31), from the benchmark suite (PRP-184: `fannkuch` needed a `done` flag) and the known issue in CLAUDE.md:

1. **Scope:** `loop`, labels and `break` with a value together. Each is small, and together they remove both the flag pattern and the `while true` workaround. Rejected: `loop` alone, since nested exits would still need flags.
2. **Label syntax:** Go/JS style, `name: loop { … }`, `break name;`, `continue name;`. Rust's `'name` would clash with char literals (`'a'`); a name and `:` at the start of a statement is unambiguous in Lugha, since `let` introduces every binding. Labels live in their own namespace (a label `count` doesn't clash with a variable `count`).
3. **Which loops take labels:** `loop`, `while`, `while let` and `for`. Not plain blocks: Rust's labelled blocks are rare, and `loop { …; break v; }` covers that use.
4. **`break` with a value:** only out of a `loop`, since `while` and `for` can end without a `break` and would have no value then. `break v;` and `break name v;`. A `loop` is an expression like `if` and `match`.
5. **Types and return checking:** a `loop` that no `break` targets has type `never`, so it *definitely returns* (§6) and can stand in for any type. Otherwise its type is its breaks' values' type (`()` for a plain `break;`), typed like a `match`'s arms: the expected type flows into each value, and a value that can't be typed alone takes a typed sibling's type. `while true` is unchanged (still never definitely returns); E0503's help suggests `loop` when the body ends in `while true`.
6. **Interactions:** a `break` or `continue` to a label runs the `defer`s of every block it leaves, innermost first, exactly as a plain `break` does today (PRP-182). Labels don't reach into closures (a closure's body is its own function, §5). Green-thread preemption (Draft item 10) counts on a `loop`'s back edge as on any loop's. Copy-on-write's "unique before the loop starts" (milestone 56) and move-on-last-use liveness treat `loop` as a loop, and a labelled jump's target as its edge.
7. **Tooling:** the formatter writes `name: loop {` on one line; `loop` gets keyword completion and the VS Code grammar; go-to-definition and rename for labels are left for later.

## CONTEXT

- Spec: §2 (keywords), §3 (grammar: `loop_stmt`, labels, `break`/`continue`), §5 ("break and continue", loops, blocks and tail values, `defer`), §6 (return checking, the `while true` paragraph), §9 (lowering), §11 (row 152), the error table, "Changes since v16". The Draft gains a planned entry for this milestone (needs approval with this PRP: CLAUDE.md architecture rule 9).
- Files: `src/lexer/token.rs` (keyword), `src/ast/mod.rs` (`Loop`, labels on loops, `Break { label, value }`, `Continue { label }`), `src/parser/stmt.rs`, `src/parser/describe.rs`, `src/parser/sexp/` (`--emit=ast`), `src/check/flow.rs` and `check/stmt.rs` (loop types, `never`, return checking, E0503's help), `src/check/defer.rs`, `src/check/closures.rs`, `src/check/liveness/walk.rs`, `src/codegen/control.rs`, `codegen/stmt.rs`, `codegen/hoist_scan.rs`, `src/formatter/stmts.rs`, `src/driver/lsp/completion.rs`, `editors/vscode/syntaxes/` (and a minor version bump), `docs/errors/`, `src/explain.rs`.
- Decisions: none open. Follows decision 214 (`defer` on every exit) and the closure rule behind E0504.

## REQUIREMENTS

### Must Do

- `loop { body }`: runs `body` until a `break` targets it (or a `return`, `?`, `panic` or `exit` leaves it).
- An optional label before `loop`, `while`, `while let` and `for`: `IDENT ":"`. `break [label] [value];` and `continue [label];`, the label naming an enclosing loop in the same function or closure.
- `break` with a value only targets a `loop`; the `loop`'s type follows Discovery 5, and every `break` targeting one loop must agree on that type (E0403 otherwise, at the value).
- A `loop` no `break` targets has type `never` and definitely returns.
- Defers run on labelled exits as on plain ones; preemption and copy-on-write's loop rules cover `loop`.
- New diagnostics (next unused codes; pages in `docs/errors/`):
  - **E0512** `unknown label \`name\``: no enclosing loop has it in this function or closure (help, inside a closure: "labels don't reach into closures").
  - **E0513** `label \`name\` is already used by an enclosing loop`: nested loops can't share a label (secondary label at the outer one).
  - **E0514** `\`break\` with a value out of a \`while\` or \`for\` loop`: help "only `loop` gives a value; a `while` or `for` can end without a `break`".
- E0504 (break or continue outside a loop) keeps its meaning. E0503's help mentions `loop` when the body's last statement is `while true`.
- `--emit=ast` prints labels and values; the formatter keeps them; keyword completion offers `loop`; the VS Code grammar highlights `loop` and labels.
- Spec, Draft, CHANGELOG (**Breaking:** `loop` is a keyword) and CLAUDE.md's KNOWN ISSUES (the `while true` entry now says to write `loop`).

### Must NOT Do

- Change `while true`'s typing: it's an expression condition, and special-casing it hides what `loop` makes explicit.
- Labelled blocks, `goto`, or labels on anything but loops.
- Labels across closures, functions or `spawn`ed calls.
- A `do … while` form.

## DESIGN

- **Grammar (§3):** `labelled = [ IDENT ":" ] ( loop_stmt | while_stmt | while_let | for_stmt )`; `loop_stmt = "loop" block`; `break_stmt = "break" [ IDENT ] [ expr ] ";"`, `continue_stmt = "continue" [ IDENT ] ";"`. After `break`, an identifier that names an enclosing label is the label; otherwise the expression begins there. So `break x;` with a variable `x` and no label `x` breaks with `x`'s value. A label and a variable of one name in scope together is resolved for the label, and the checker warns nothing: labels and variables are different namespaces (the spec says so in §5).
- **Where a label may stand:** at the start of any expression whose next tokens are `IDENT ":"` and then `loop`, `while` or `for`, so `let found = search: loop { … };` works. The one exception is a map literal's key, which the map parser reads with labels off: `#{a: loop { … }}` is the entry `a` → a loop, as today. Struct literals read `field:` themselves, so `P { x: outer: loop { … } }` is a field holding a labelled loop.
- **A loop as an expression:** `loop` is accepted wherever an expression is (`let x = loop { … };`), like `if`. As a statement it needs no `;`, and a statement `loop` whose type isn't `()` or `never` is the existing "block-like statement with a value" error, as for `if`.
- **Codegen:** a `loop` with a value gets a result slot (an `alloca` in the entry block, decision 6's frame rules) written before each `break` jumps to the exit block; a `never` loop has no exit block. Labels are resolved by the checker to loop ids, so codegen keeps a stack of (id, continue block, break block, defer depth).
- **No runtime, ABI or security changes.** No new dependencies.

## TESTS

- [ ] `tests/programs/m152/`: `loop_return.la` (the OBJECTIVE's `next_id`: definitely returns), `break_value.la` (values, labelled and not; expected type flowing into `None` and `[]`), `labels.la` (`break`/`continue` to outer `for`, `while`, `while let`), `defer_labels.la` (defers run innermost first on a labelled exit), `loop_unit.la` (plain `break;`, `()`), `spawn_loop.la` (a green thread in a `loop` yields to another).
- [ ] Rejected programs with `.stderr`: E0512 (unknown; and inside a closure), E0513, E0514 (`while` and `for`), E0403 (breaks disagreeing), E0503 with the `loop` help after `while true`, and `loop` as a name (E0201-family parse error).
- [ ] Unit tests: lexer (`loop` keyword), parser (labels at a statement's start and in `let x = l: loop`, `#{a: loop {…}}` staying a map entry, `break label value`, `break x` resolving), formatter (round trip), liveness (a value moved inside a `loop` that a labelled `continue` revisits).
- [ ] `tests/explain.rs` covers the three new pages; `tests/lsp_completion.rs` offers `loop`.
- [ ] `bench/programs/fannkuch/fannkuch.la` rewritten with labels if PRP-184 has merged, and its check still passes.

## ACCEPTANCE

- [ ] The OBJECTIVE program prints `30`, `3` and `00 10 11 20 21 22 ` lines as shown, exit 0.
- [ ] `cargo test --lib`, `--test programs`, `--test explain`, `--test lsp_completion`, `--test fmt` pass; `cargo fmt --check` and `cargo clippy --all-targets -- -D warnings` clean; the extension's `npm test` passes.
- [ ] Spec (§2, §3, §5, §6, §9, §11 row 152, errors, Changes since v16), Draft, CHANGELOG, CLAUDE.md known issue and DECISIONS (a resolved entry) updated; files ≤ 500 lines.
