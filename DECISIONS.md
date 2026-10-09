# DECISIONS.md — lugha

Tracks architectural and design questions that are open, deferred, or resolved.

Rules:
- Every open decision blocks implementation of the code it affects.
- The AI must not implement anything that depends on an open decision.
- When a decision is resolved, move it to RESOLVED and record the outcome.
- Once resolved, copy the outcome to MEMORY.md as an architectural decision.

Questions about the *language* are answered by `docs/specs/Language v0 Specification.md`. This file holds questions about the *compiler implementation* that the spec leaves open.

---

## OPEN — Requires human input before implementation

None.

---

## DEFERRED — Acknowledged, not yet needed

### DECISION-004 — Design system (docs/DESIGN.md)

**Status:** deferred
**Raised:** 2026-10-09 — Session 1
**Revisit when:** Any UI (web playground, editor extension, GUI) is proposed.

**Question:** What design tokens and component rules apply?

**Notes:** lughac is a CLI; `docs/setup.md` Step 9 was skipped.

---

## RESOLVED

### DECISION-001 — Crate layout for lughac

**Status:** resolved
**Raised:** 2026-10-09 — Session 1
**Resolved:** 2026-10-09 — Session 2

**Question:** How is the `lughac` compiler laid out as Cargo crates?

**Outcome:** One package with a library and a thin binary: `src/lib.rs` holds the stages as modules (`lexer`, `parser`, `check`, `codegen`, `driver`); `src/main.rs` only parses the CLI and calls the library.

**Rationale:** Stages are testable from `tests/` without spawning processes, at almost no cost over a single binary. A workspace's enforced boundaries are covered by the "stages only depend backwards" architecture rule instead.

**Copied to MEMORY.md:** yes

---

### DECISION-002 — Error and diagnostic types

**Status:** resolved
**Raised:** 2026-10-09 — Session 1
**Resolved:** 2026-10-09 — Session 2

**Question:** (a) How do stages return user-program errors? (b) How are internal errors defined?

**Outcome:** (a) Each stage returns `Result<(Output, Vec<Diagnostic>), Vec<Diagnostic>>`: `Ok` carries the output plus any warnings, `Err` carries every error and warning found. (b) Internal lughac errors use `thiserror`.

**Rationale:** Matches the spec's "a stage with an error stops the pipeline" while still letting W01xx warnings through on success. `thiserror` removes boilerplate and is MIT/Apache-2.0.

**Copied to MEMORY.md:** yes

---

### DECISION-003 — Rust edition and MSRV

**Status:** resolved
**Raised:** 2026-10-09 — Session 1
**Resolved:** 2026-10-09 — Session 2

**Question:** Which edition and minimum supported Rust version?

**Outcome:** Edition 2024. MSRV is the stable release current when the crate is initialised, pinned exactly in `rust-toolchain.toml` and mirrored in `Cargo.toml` `rust-version`.

**Rationale:** Newest language features; a pinned toolchain keeps every session and CI on the same compiler.

**Copied to MEMORY.md:** yes

---

### DECISION-005 — LLVM version and inkwell feature

**Status:** resolved
**Raised:** 2026-10-09 — Session 1
**Resolved:** 2026-10-09 — Session 2

**Question:** Which LLVM major version and inkwell feature?

**Outcome:** LLVM 21 via `inkwell` 0.10 with the `llvm21-1-prefer-dynamic` feature (links `libLLVM.so`). Install with `apt install llvm-21-dev`.

**Amended 2026-10-09 — Session 3:** the plain `llvm21-1` feature links LLVM statically and fails on Ubuntu with `could not find native static library Polly` (the dev package ships no static Polly). `-prefer-dynamic` links the shared library instead; verified by building and verifying an LLVM module.

**Rationale:** LLVM 21 is Ubuntu 26.04's default `llvm-dev`, so contributors get it with one package, and it is supported by inkwell 0.8–0.10 (checked on crates.io 2026-10-09). It meets the spec's needs: opaque pointers, the new pass manager, `llvm.fptosi.sat`.

**Copied to MEMORY.md:** yes

---

### DECISION-006 — Diagnostic rendering crate

**Status:** resolved
**Raised:** 2026-10-09 — Session 1
**Resolved:** 2026-10-09 — Session 2

**Question:** What renders rustc-style diagnostics?

**Outcome:** `codespan-reporting`, configured with ASCII characters (`-->`, `|`).

**Rationale:** Its layout is closest to rustc's, which the spec's E0401 example uses. **Must verify in the milestone 3 PRP** that the output matches the spec example byte-for-byte; if it can't, open a new decision (custom renderer vs. spec change) — do not silently change the spec.

**Verified 2026-10-09 — Session 8:** it can't match byte-for-byte. See DECISION-011 for the outcome.

**Copied to MEMORY.md:** yes

---

### DECISION-007 — CLI argument parsing

**Status:** resolved
**Raised:** 2026-10-09 — Session 1
**Resolved:** 2026-10-09 — Session 2

**Question:** How is the `lughac` command line parsed?

**Outcome:** `clap` with the derive API.

**Rationale:** Standard, generates help, and its usage-error exit code is already 2, matching spec §9.

**Copied to MEMORY.md:** yes

---

### DECISION-008 — Acceptance test harness

**Status:** resolved
**Raised:** 2026-10-09 — Session 1
**Resolved:** 2026-10-09 — Session 2

**Question:** How are `.la` programs compiled and checked in tests?

**Outcome:** `tests/programs/<name>.la`, each with sibling `<name>.stdout` and `<name>.exit` files, plus `<name>.stderr` for rejected programs. One Rust integration test discovers every `.la` file, runs `lughac` on it, and compares exactly.

**Rationale:** No extra dependency; adding a test is adding files; the §10 programs drop straight in.

**Copied to MEMORY.md:** yes

---

### DECISION-011 — Human diagnostics layout

**Status:** resolved
**Raised:** 2026-10-09 — Session 8 (verifying DECISION-006)
**Resolved:** 2026-10-09 — Session 8

**Question:** codespan-reporting 0.13 renders the spec's E0401 example with three differences: `  --> ` instead of ` --> `, trailing spaces on the connector line, and a blank line after each diagnostic. Change the renderer, patch its output, or change the spec?

**Outcome:** Keep codespan-reporting with ASCII characters, and strip trailing whitespace from every line. The spec §10 example and §9 text now follow codespan's layout: `  --> ` and a blank line after each diagnostic. No other string patching.

**Rationale:** A maintained crate handles multi-line spans, overlapping labels and line-number widths. DECISION-006 allowed adjusting the spec example, and patching codespan's text would break silently on upgrades.

**Copied to MEMORY.md:** yes

---

### DECISION-009 — How lugha_rt.c is built and found at link time

**Status:** resolved
**Raised:** 2026-10-09 — Session 1
**Resolved:** 2026-10-09 — Session 14

**Question:** Is `lugha_rt.c` compiled by `build.rs` and embedded, embedded as source and compiled on use, or installed alongside `lughac`?

**Outcome:** `runtime/lugha_rt.c` is embedded with `include_str!` (`link::RUNTIME_SOURCE`). Each link writes it to a private temp file and passes it to the one `cc` call: `cc prog.o lugha_rt.c -lgc -lm -o prog`.

**Rationale:** No new dependency, no install layout, and it works from any directory. The cost is a few milliseconds of C compilation per build.

**Copied to MEMORY.md:** yes

---

### DECISION-010 — Cost of array copies

**Status:** resolved
**Raised:** 2026-10-09 — Session 1 (from spec §12)
**Resolved:** 2026-10-09 — Session 21

**Question:** Keep eager deep copies, or move to copy-on-write?

**Outcome:** Keep eager deep copies at the spec §4 copy sites; copy-on-write is rejected. Two follow-ups are noted in TODO but not scheduled:
- a possible "move on last use" optimization for dead locals (no spec change);
- a v1 language question about a swap or move operation for buffers.

**Rationale:** Measurements in `docs/decisions/decision-010.md`:
- Copy-on-write would save about 0% on `life`, `sort` and `matmul`, because the copied arrays are written right away and get copied anyway.
- It would save at most ~65% on `particles`, and only with full reference counting: increments, scope-exit decrements, and a check on every element write. Boehm makes that bookkeeping unnecessary otherwise.
- The savings that matter come from dead locals, which a compile-time move-on-last-use check can remove. Rewriting `particles` to update through the place chain is already 10× faster.
- The spec §10 programs make no copies.

**Copied to MEMORY.md:** yes
