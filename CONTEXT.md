# CONTEXT.md — lugha

Session handoff file. Updated at the end of every session.
Read at the start of the next session alongside CLAUDE.md, MEMORY.md, and DECISIONS.md.

Every session has a name and a state: open | closed.
A session is closed only after CONTEXT.md is committed and pushed.

---

## SESSION 1 — 2026-10-09 — Context system setup — closed

Branch: main

### WHAT WAS DONE

Scaffolded the AI context system described in `setup.md`, adapted from its TypeScript examples to Rust: CLAUDE.md, MEMORY.md, CONTEXT.md, DECISIONS.md, CHANGELOG.md, TODO.md, .llmignore, PRPs/ templates, docs/CODE_STYLE.md and the docs/ and reports/ folder structure. Project-specific facts that are not yet known (crate layout, error crate, edition/MSRV) were opened as decisions rather than guessed. `docs/DESIGN.md` was skipped — there is no UI.

Then reviewed `docs/specs/Language v0 Specification.md` and fixed its inconsistencies at the user's request: symbol prefixes (`lugha_fn_` / `lugha_rt_`), milestone 2 test program, struct passing, negative literal folding, negative repeat counts, exact acceptance outputs and float formatting, associativity, prefix grammar, `void` as a non-value. Source extension changed from `.lugha` to `.la`. PRP filenames set to `prp-{NNN}-{feature_name}.md`. Finally, updated CLAUDE.md (stack, 10 architecture rules, two kinds of error, anti-patterns, known issues), MEMORY.md (decisions 3–7), DECISIONS.md (001 narrowed to crate layout; new 005–010) and TODO.md (prerequisites and milestones) from the spec.

### FILES CREATED OR MODIFIED

```
CLAUDE.md                 — behavioral rules, Rust-adapted
MEMORY.md                 — initial decisions (Rust/GPL, Result-based errors)
CONTEXT.md                — this log
DECISIONS.md              — DECISION-001..003 open, DESIGN.md deferred
CHANGELOG.md              — Unreleased block
TODO.md                   — outstanding setup and project tasks
.llmignore                — protected paths
PRPs/TEMPLATE.md          — PRP template
PRPs/DISCOVERY.md         — discovery interview protocol
docs/CODE_STYLE.md        — rustdoc and comment rules
docs/**, reports/         — empty folders (.gitkeep)
docs/specs/Language v0 Specification.md — inconsistencies fixed, extension .la
```

### TESTS WRITTEN

- None — no code yet.

### DECISIONS MADE

- Rust conventions replace the guide's TS ones: unit tests in `#[cfg(test)]` modules, integration tests in `tests/`, `///` rustdoc, `Cargo.lock` protected from hand edits.
- File size limit set to 300 lines (the guide's default).
- PRP filenames: `prp-{NNN}-{feature_name}.md`, three-digit number, snake_case name.
- Source file extension is `.la`.

### PENDING DECISIONS OPENED

- DECISION-001 — What lugha is and its crate layout
- DECISION-002 — Error derive crate
- DECISION-003 — Rust edition and MSRV
- DECISION-005..008 — LLVM version, diagnostics crate, CLI parsing, test harness (open)
- DECISION-009, -010 — runtime location, array-copy cost (deferred)

### STILL OPEN AT CLOSE

- Seven decisions block milestone 1: DECISION-001 (crate layout), -002, -003, -005, -006, -007, -008.
- Rust, LLVM dev and libgc dev are not installed on the dev machine.

---

## SESSION 2 — 2026-10-09 — Resolve milestone 1 decisions — closed

Branch: main

### WHAT WAS DONE

The user accepted the suggested option for every decision blocking milestone 1. Resolved DECISION-001, -002, -003, -005, -006, -007 and -008 and recorded them in MEMORY.md (decisions 8–12), CLAUDE.md (stack, testing, error handling) and TODO.md. For DECISION-005 the suggestion was "a version inkwell supports and Ubuntu packages"; checked both: Ubuntu 26.04's default `llvm-dev` is LLVM 21, and inkwell 0.10.0 (latest on crates.io) supports `llvm21-1`, so LLVM 21 was chosen. For DECISION-006 the suggestion was "codespan-reporting or hand-rolled"; chose codespan-reporting, with a milestone 3 check that it reproduces the spec's E0401 output.

### FILES CREATED OR MODIFIED

```
DECISIONS.md — 001, 002, 003, 005, 006, 007, 008 moved to RESOLVED; no open decisions
MEMORY.md    — decisions 8–12, state and next start point
CLAUDE.md    — stack versions, tests/programs/ rule, stage return type
TODO.md      — decisions checked off, install and init steps made concrete
CONTEXT.md   — this entry
```

### TESTS WRITTEN

- None — no code yet.

### DECISIONS MADE

- See above; all recorded in DECISIONS.md RESOLVED.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Toolchain not installed (rustup, `llvm-21-dev`, `libgc-dev`).
- Crate not initialised.

---

## SESSION 3 — 2026-10-09 — Verify toolchain and initialise crate — open

Branch: main

---

## NEXT SESSION START POINT

Install the toolchain (rustup stable, `llvm-21-dev`, `libgc-dev`), initialise the crate per MEMORY.md decisions 8–12, then run a discovery interview for `PRPs/prp-001-lexer.md` (or the `tests/programs/` runner first). See `TODO.md`.
