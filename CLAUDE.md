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
```

Run fmt, clippy and tests after every non-trivial change. A task is not done until all pass.

> No `Cargo.toml` exists yet — these commands start working once DECISION-001 is resolved and the crate is initialised.

---

## STACK

- Rust (stable). Edition and MSRV: see DECISION-003.
- License: GPL-3.0 — every dependency must be GPL-3.0-compatible.

---

## ARCHITECTURE RULES

[Fill in once DECISION-001 is resolved. Each rule says what to do AND why deviating breaks something.]

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

- Each module defines its own error enum; errors are typed, never `String`. (Crate choice for deriving errors: DECISION-002.)
- **Never** `.unwrap()` / `.expect()` on anything derived from external input. `.expect("reason")` is allowed only for true invariants, and the message states the invariant.
- **Never** return `Option` to signal an error — `None` cannot say *why*.
- **Never** swallow an error with `let _ =` or `.ok()` without a comment explaining why ignoring it is safe.
- Propagate with `?`; convert between error types with `From` impls, not ad-hoc `map_err` everywhere.

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
├── setup.md         — the guide this context system follows
├── PRPs/            — feature briefs prp-{NNN}-{feature_name}.md (+ TEMPLATE.md, DISCOVERY.md)
├── docs/            — CODE_STYLE.md, source/, specs/, decisions/, incidents/, status/
└── reports/         — EOD reports (YYYY-MM-DD.md)
```

Update this tree when `src/` and `tests/` are created.

---

## ANTI-PATTERNS

1. **Never add a crate without a PRP line justifying it.** Dependencies are permanent cost and licensing risk.
2. **Never write `unsafe` without human approval.** It voids the guarantees every other rule relies on.
3. **Never silence a clippy lint with `#[allow(...)]` without a comment saying why.** Silent allows hide real bugs.

---

## KNOWN ISSUES — DO NOT FIX

None yet.
