# DECISIONS.md — lugha

Tracks architectural and design questions that are open, deferred, or resolved.

Rules:
- Every open decision blocks implementation of the code it affects.
- The AI must not implement anything that depends on an open decision.
- When a decision is resolved, move it to RESOLVED and record the outcome.
- Once resolved, copy the outcome to MEMORY.md as an architectural decision.

---

## OPEN — Requires human input before implementation

### DECISION-001 — What lugha is and how the crate is laid out

**Status:** open
**Raised:** 2026-10-09 — Session 1
**Resolved by:** human
**Blocks:** `cargo init`, CLAUDE.md STACK / ARCHITECTURE RULES, every feature

**Question:** What is lugha (e.g. a programming language, interpreter, compiler, NLP tool), and should the crate be a binary, a library, or a Cargo workspace?

**Options:**
- A) Single binary crate — simplest; harder to reuse internals or test them as a library.
- B) Library + thin binary (`src/lib.rs` + `src/main.rs`) — internals testable via `tests/`; small overhead.
- C) Cargo workspace with several crates (e.g. lexer, parser, runtime, cli) — clean boundaries; more setup and build time.

**Notes:** The name is Swahili for "language", which suggests a language project — but this has not been confirmed.

---

### DECISION-002 — Error type crate

**Status:** open
**Raised:** 2026-10-09 — Session 1
**Resolved by:** human
**Blocks:** First error enum

**Question:** How are error types defined?

**Options:**
- A) `thiserror` for library errors (+ optionally `anyhow` at the binary edge) — idiomatic, little boilerplate.
- B) Hand-written `Display`/`Error` impls — zero dependencies, more boilerplate.

**Notes:** Either satisfies the Result rule in CLAUDE.md. Both crates are MIT/Apache-2.0, GPL-compatible.

---

### DECISION-003 — Rust edition and MSRV

**Status:** open
**Raised:** 2026-10-09 — Session 1
**Resolved by:** human
**Blocks:** `Cargo.toml`, CI

**Question:** Which edition and minimum supported Rust version?

**Options:**
- A) Edition 2024, MSRV = current stable — newest features; users need a recent toolchain.
- B) Pin an older MSRV — wider compatibility; restricts features and dependency versions.

**Notes:** Also decide whether to add a `rust-toolchain.toml`.

---

## DEFERRED — Acknowledged, not yet needed

### DECISION-004 — Design system (docs/DESIGN.md)

**Status:** deferred
**Raised:** 2026-10-09 — Session 1
**Revisit when:** Any UI (web playground, editor extension, GUI) is proposed.

**Question:** What design tokens and component rules apply?

**Notes:** No UI exists, so `setup.md` Step 9 was skipped.

---

## RESOLVED

None yet.
