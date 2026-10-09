# CLAUDE.md — lugha

Behavioral instructions for AI coding assistants. Each rule exists to prevent a specific mistake.

---

## SESSION HANDOFF RULE — NON-NEGOTIABLE

Every session in `CONTEXT.md` must have:
- A **name** — short descriptive title of what the session accomplished
- A **state** — `open` while work is in progress, `closed` once handoff is done
- A **branch** — the git branch this session's work lives on

Format:

    ## SESSION {n} — {YYYY-MM-DD} — {Name} — {state}
    Branch: {branch-name}

Rules:
- **Step 1 of every session, no exceptions:** append a new session entry to `CONTEXT.md` with state `open` and the current branch name. Do this before reading any other file, before planning, before writing any code. Commit it before any other work begins.
- Mark it `closed` only after CONTEXT.md is updated, committed and pushed.
- Never leave a session `open` at the end of a turn.
- Never start a new session without closing the previous one first.
- The NEXT SESSION START POINT block is rewritten at the end of every session.
- Sessions are never deleted — the full history stays in CONTEXT.md.

---

## PRP RULE — NON-NEGOTIABLE

Never write code for a new feature without a PRP file in `/PRPs`.

If a feature request is given without a PRP:
1. Do not write any code.
2. Run a discovery interview (`PRPs/DISCOVERY.md`) — one question at a time.
3. Write the PRP to `/PRPs/prp-{NNN}-{feature_name}.md` from `PRPs/TEMPLATE.md`.
   - `NNN` is the next unused number, zero-padded to three digits. Numbers are never reused or renumbered.
   - `feature_name` is lowercase snake_case: `prp-001-lexer.md`, `prp-002-type_checker.md`.
4. Present it to the user and build only after explicit approval.

A vague prompt is not a starting point. It is the beginning of a discovery.

---

## SCOPE RULE — NON-NEGOTIABLE

One PRP at a time. Never implement more than one feature's scope in a single session.

If mid-implementation the scope turns out larger than the PRP described: stop, document what was discovered, update or create a PRP, and get approval before continuing. The human decides what is "small enough to add", not the model.

---

## TESTING RULE — NON-NEGOTIABLE

Write the test before writing the implementation. No exceptions.

- For every new function, write a failing test first, then the minimum code to make it pass.
- Unit tests live in a `#[cfg(test)] mod tests` block at the bottom of the file they test.
- Integration tests (public API only) live in `tests/`, one file per area: `src/lexer.rs` → `tests/lexer.rs`.
- End-to-end tests are files in `tests/programs/`: `<name>.la` plus `<name>.stdout`, `<name>.exit`, and `<name>.stderr` for rejected programs. Add a test by adding files — never by editing the runner.
- Test behavior visible to callers — inputs, outputs, and every error variant. Do not test private internals or third-party crate behavior.
- Every new `pub` function gets at least one happy-path test and one test per error variant it can return.
- Run the full suite (`cargo test`) after any non-trivial change before calling the task done.

If the PRP does not describe what to test, add the test cases to the PRP before writing code.

---

## SECURITY RULE — NON-NEGOTIABLE

Never implement without explicit human review and approval:
- Authentication, authorization, or session management
- Cryptographic operations (hashing, signing, encrypting)
- Secrets or credential handling
- Any `unsafe` block

For everything else:
- Never hardcode credentials, API keys, tokens, or secrets — not even in comments or examples.
- Never trust external input (files, CLI args, network, env) without validating it first.
- Never log sensitive data.
- Never add a dependency without stating why in the PRP — every crate is attack surface.

If unsure whether something has a security implication, stop and ask.

---

## CODE DOCUMENTATION RULE — NON-NEGOTIABLE

Read `docs/CODE_STYLE.md` before writing any function, type, or module.

Every `pub` item has a `///` doc comment. Every non-obvious decision inside a function has a `//` comment explaining *why*, not what.

---

## PENDING DECISIONS RULE — NON-NEGOTIABLE

Before writing code that depends on an unresolved architectural question, check `DECISIONS.md`.

- `open` → stop. Do not implement. Ask the user to resolve it.
- `resolved` → follow the recorded outcome. Do not re-litigate.
- New unresolved question mid-implementation → add it to `DECISIONS.md` as `open` and stop. Do not guess.

Never make an architectural choice silently.

---

## FILE SIZE RULE

No file exceeds 300 lines (including tests).

When a file reaches the limit: stop, propose a specific split (new file names and what moves where), wait for approval, split, then continue.

---

## PROTECTED FILES — NON-NEGOTIABLE

Never read, modify, or delete:

- `.env` and `.env.*`
- `Cargo.lock` — never hand-edit; it changes only as a side effect of cargo commands
- `target/` — build output
- `LICENSE` — GPL-3.0, changed only by the human

If a task seems to require touching a protected file, stop and ask. See also `.llmignore`.

---

## COMMANDS

```bash
cargo build                                   # build
cargo test                                    # all tests
cargo fmt --check                             # formatting
cargo clippy --all-targets -- -D warnings     # lint

cargo run -q -- build prog.la [-o out] [--emit=tokens|ast|ir] [-O0|-O2] [--diagnostics=human|json]
cargo run -q -- run prog.la                   # exit code = the program's
cargo run -q -- check prog.la                 # lex, parse, type-check
```

Run fmt, clippy and tests after every non-trivial change. A task is not done until all pass.

> End-to-end tests also need `llvm-21-dev`, `cc`, and `libgc-dev` installed.

---

## STACK

- Rust 1.99.0 (pinned in `rust-toolchain.toml`), edition 2024 — package `lugha`, binary `lughac`.
- `inkwell` 0.10, feature `llvm21-1-prefer-dynamic` + LLVM 21 (`llvm-21-dev`) — codegen. Never use another LLVM major; never switch to static LLVM linking (fails on Ubuntu, see DECISION-005).
- C (`lugha_rt.c`) — the runtime linked into every compiled program.
- Boehm GC (`libgc`), `libc`, `libm` — linked into every compiled program via the system `cc`.
- `codespan-reporting` (ASCII chars) — human diagnostics. `clap` (derive) — CLI. `thiserror` — internal errors.
- License: GPL-3.0 — every dependency must be GPL-3.0-compatible.

The language itself is defined by `docs/specs/Language v0 Specification.md`.

---

## ARCHITECTURE RULES

1. **The spec is the source of truth.** Read the relevant section of `docs/specs/Language v0 Specification.md` before implementing any language feature, and cite the section in the PRP. If code and spec disagree, the code is wrong. If the spec looks wrong, stop and propose a spec change — never edit the spec without explicit approval. *Why:* `lughac spec` ships the spec as the language reference; drift makes it lie.
2. **Stages only depend backwards.** lexer → parser → check → codegen → driver. A stage never imports a later one. *Why:* `lughac check` and `--emit=tokens|ast|ir` must run the pipeline partway.
3. **Every token and AST node carries a byte-offset `Span`.** *Why:* every diagnostic needs a location, and spans can't be recovered later.
4. **Codegen never infers types.** It reads the checker's expression → type side table. *Why:* opaque pointers need the element type on every load/store/GEP (spec §9), and two type computations will disagree.
5. **User-program errors are `Diagnostic` records, never panics or strings.** Each has a stable code from its stage's range (E01xx lex … E05xx, W01xx). New codes take the next unused number; a code is never reused or renumbered. Human and JSON output render from the same record. *Why:* spec §9; codes are a public contract.
6. **Exit codes are 0 / 1 / 2.** 1 = the user's program has errors; 2 = bad CLI usage or internal compiler error. A `module.verify()` failure or a Rust panic inside lughac is exit 2, never 1. *Why:* spec §9; tools tell "your bug" from "our bug" by it.
7. **Symbol prefixes are fixed.** Lugha functions → `lugha_fn_<name>`, runtime → `lugha_rt_<name>`, copy helpers → `lugha_copy_<type>`, externs verbatim. *Why:* disjoint prefixes are what prevents symbol collisions (spec §8).
8. **Generated code allocates only through `lugha_rt_alloc`.** Never emit calls to `malloc`. *Why:* Boehm can't see `malloc` memory and will free live objects.
9. **Follow the milestone order (spec §11).** The lexer and parser cover the full §2–§3 syntax. The checker and codegen implement only the current milestone's subset; in milestones 1–2 treat every value as `i64`. *Why:* each milestone must end in a running program, while syntax is cheap to do once and fully specified.
10. **The §10 programs are the acceptance tests.** Their stdout and exit codes must match exactly. *Why:* they define "conforming compiler".

---

## ERROR HANDLING — NON-NEGOTIABLE

Expected failures return `Result<T, E>`. Panics are for bugs only.

```
Expected failure  →  return Err(...)
Truly unexpected  →  panic (invariant violated, programmer error)
```

```rust
// Good — the caller sees every failure mode in the type
pub fn parse_number(src: &str) -> Result<i64, ParseError> {
    src.trim().parse().map_err(|_| ParseError::InvalidNumber(src.to_owned()))
}

// Bad — hides a failure path and crashes on user input
pub fn parse_number(src: &str) -> i64 {
    src.trim().parse().unwrap()
}
```

- Each module defines its own error enum; errors are typed, never `String`, derived with `thiserror`.
- **Never** `.unwrap()` / `.expect()` on anything derived from external input. `.expect("reason")` is allowed only for true invariants, and the message states the invariant.
- **Never** return `Option` to signal an error — `None` cannot say *why*.
- **Never** swallow an error with `let _ =` or `.ok()` without a comment explaining why ignoring it is safe.
- Propagate with `?`; convert between error types with `From` impls, not ad-hoc `map_err` everywhere.

**Two kinds of error in this project — keep them apart:**
- **Errors in the user's `.la` program** (bad token, type mismatch) are expected output of the compiler. They become `Diagnostic` records (architecture rule 5) and the stage keeps going to find more where it can. Every stage returns `Result<(Output, Vec<Diagnostic>), Vec<Diagnostic>>`: `Ok` = output + warnings, `Err` = all errors and warnings.
- **Errors in lughac itself** (I/O failure, linker not found) use `thiserror` enums and `Result`. A violated compiler invariant is a bug → exit 2.

---

## FILE ORGANIZATION

```
.
├── CLAUDE.md        — this file
├── MEMORY.md        — resolved decisions + project state
├── CONTEXT.md       — session handoff log
├── DECISIONS.md     — open/deferred/resolved questions
├── CHANGELOG.md     — what shipped
├── TODO.md          — outstanding tasks
├── .llmignore       — protected paths
├── Cargo.toml       — package `lugha`, binary `lughac`
├── rust-toolchain.toml — pinned Rust 1.99.0
├── src/
│   ├── lib.rs       — compiler library; stage modules added per PRP
│   ├── main.rs      — `lughac` entry point (CLI only)
│   ├── span.rs      — byte-offset Span
│   ├── diagnostic.rs — Diagnostic record (spec §9 fields), no rendering
│   ├── lexer/       — mod.rs (lex, main loop), token.rs, number.rs, string.rs
│   ├── ast/         — mod.rs (items, types, statements), expr.rs (expressions, ExprId)
│   ├── parser/      — mod.rs (parse, cursor, errors), recover.rs, describe.rs,
│   │                  expr.rs (Pratt), primary.rs, stmt.rs, item.rs, sexp.rs, test_util.rs
│   ├── check/       — mod.rs (check, Checked, CheckError), types.rs, env.rs (globals, main, scopes),
│   │                  expr.rs, literal.rs, call.rs, ops.rs (operators, casts), stmt.rs,
│   │                  assign.rs (places, mutability), flow.rs (returns, loops, W0101), errors.rs
│   ├── codegen/     — mod.rs (emit_ir, emit_object, CodegenError), lower.rs (module, C main),
│   │                  value.rs (Value, Type → LLVM), scope.rs (locals), expr.rs, arith.rs (ops at
│   │                  each width), cast.rs (§4 casts), control.rs (blocks, if, loops, jumps), stmt.rs,
│   │                  function.rs (signatures, bodies, return, calls)
│   ├── link.rs      — `cc … -lgc -lm -o out`, LinkError
│   └── driver/      — mod.rs (clap CLI, exit codes), pipeline.rs, source.rs (load, E0110, line_col),
│                      render.rs (codespan, Report), json.rs (JSON lines)
├── tests/
│   ├── programs.rs  — end-to-end acceptance test (runs every tests/programs/ case)
│   ├── lexer.rs     — lexer public-API tests (every spec program lexes)
│   ├── parser.rs    — parser public-API tests (every spec program parses)
│   ├── codegen.rs   — builds and runs real executables at -O0 and -O2
│   ├── cli.rs       — the lughac binary: commands, emit, diagnostics, exit codes
│   ├── common/      — spec_programs.rs: the §10/§11 programs, shared by test crates
│   ├── programs/    — m1/ … m5/: <name>.la + .stdout/.exit/.stderr expectations
│   └── support/     — fixture.rs (temp dirs), runner/ (discover, execute, report)
├── setup.md         — the guide this context system follows
├── PRPs/            — feature briefs prp-{NNN}-{feature_name}.md (+ TEMPLATE.md, DISCOVERY.md)
├── docs/            — CODE_STYLE.md, source/, decisions/, incidents/, status/
│   └── specs/       — Language v0 Specification.md (the language definition)
└── reports/         — EOD reports (YYYY-MM-DD.md)
```

Update this tree when modules or `tests/` are added.

---

## ANTI-PATTERNS

1. **Never add a crate without a PRP line justifying it.** Dependencies are permanent cost and licensing risk.
2. **Never write `unsafe` without human approval.** It voids the guarantees every other rule relies on.
3. **Never silence a clippy lint with `#[allow(...)]` without a comment saying why.** Silent allows hide real bugs.
4. **Never add an implicit conversion or coercion.** Spec §4 forbids them; every one is a type hole the checker can't see.
5. **Never implement a §11 "out of scope" feature** (enums, generics, closures, methods, modules…) even partially. It belongs to v1 and needs a spec change first.
6. **Never stop the checker at the first error.** Spec §9 requires reporting as many as it can find.
7. **Never print diagnostics directly from a stage.** Stages return records; only the driver renders. Otherwise human and JSON output diverge.

---

## KNOWN ISSUES — DO NOT FIX

These look like bugs but are specified behavior:

- **A function whose only exit is a `return` inside `while true` is rejected** as missing a return. Spec §6 rejects it on purpose to keep return checking simple; users add `panic("unreachable");`.
- **Integer overflow always panics**, even at `-O2`. Spec §12 decided against wrapping.
- **Boehm may keep garbage alive** when an integer looks like a pointer. Conservative GC, spec §7.
- **Extern C code keeping a Lugha pointer is undefined behavior.** Spec §7–8 accept this for v0.
- **`as` casts truncate/saturate silently.** The only place values wrap (spec §4).
- **Until milestone 4, integer `+ - *` wrap and `/ %` by zero or `MIN / -1` trap with SIGILL.** The specified panics need `lugha_rt_panic` (spec §11). Do not add overflow checks before then.
- **Codegen reports the outermost unsupported construct first** (`[1][0]` → indexing, not arrays). The message names the milestone that adds it; it goes away by milestone 5.
