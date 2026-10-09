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

### DECISION-001 — Crate layout for lughac

**Status:** open
**Raised:** 2026-10-09 — Session 1
**Resolved by:** human
**Blocks:** `cargo init`, every milestone

**Question:** How is the `lughac` compiler laid out as Cargo crates?

**Options:**
- A) Single binary crate, one module per stage (`lexer`, `parser`, `check`, `codegen`, `driver`) — simplest; integration tests can only drive the binary.
- B) Library + thin binary (`src/lib.rs` + `src/main.rs`) — stages testable from `tests/` without spawning processes; minimal overhead. **Suggested.**
- C) Cargo workspace, one crate per stage — enforced stage boundaries and parallel builds; more boilerplate, and inkwell's LLVM build cost lands only on the codegen crate.

**Notes:** What lugha *is* was answered by the spec (2026-10-09): a compiled language whose compiler `lughac` is written in Rust with inkwell. Only the layout remains open. Also decide where `lugha_rt.c` lives (see DECISION-009).

---

### DECISION-002 — Error and diagnostic types

**Status:** open
**Raised:** 2026-10-09 — Session 1
**Resolved by:** human
**Blocks:** Lexer (milestone 1)

**Question:** (a) How do compiler stages return user-program errors, and (b) how are internal Rust error types defined?

**Options for (a):**
- A) Each stage returns `Result<Output, Vec<Diagnostic>>` — simple; a stage with any error produces no output. **Suggested** — matches the spec's "each stage stops the pipeline if it reports an error".
- B) Each stage returns `(Option<Output>, Vec<Diagnostic>)` and pushes into a shared sink — allows warnings alongside success (W01xx), more plumbing.

**Options for (b):**
- A) `thiserror` for internal errors (I/O, linker failure).
- B) Hand-written `Display`/`Error` impls.

**Notes:** Warnings (W01xx) must be reportable on successful compiles, which option (a)-A doesn't cover by itself — a warnings list on the Ok side would. Spec §9: the checker reports as many errors as it can.

---

### DECISION-003 — Rust edition and MSRV

**Status:** open
**Raised:** 2026-10-09 — Session 1
**Resolved by:** human
**Blocks:** `Cargo.toml`

**Question:** Which edition and minimum supported Rust version, and is a `rust-toolchain.toml` pinned?

**Options:**
- A) Edition 2024, MSRV = current stable, pinned in `rust-toolchain.toml`.
- B) Older MSRV — wider compatibility; may conflict with inkwell's requirements.

**Notes:** No Rust toolchain is installed on the dev machine yet (checked 2026-10-09).

---

### DECISION-005 — LLVM version and inkwell feature

**Status:** open
**Raised:** 2026-10-09 — Session 1
**Resolved by:** human | technical constraint check
**Blocks:** Codegen (milestone 1)

**Question:** Which LLVM major version does lughac build against, and which `inkwell` `llvmNN-M` feature matches it?

**Options:**
- A) Newest LLVM that inkwell supports — newest pass manager and intrinsics.
- B) The LLVM version the distro packages as `llvm-NN-dev` — easiest install for contributors.

**Notes:** The spec relies on opaque pointers, the new pass manager (`default<O2>`) and `llvm.fptosi.sat`, so LLVM ≥ 15. `/usr/lib/llvm-21` exists on the dev machine but without `llvm-config` or `libLLVM` (no dev package). Check inkwell's supported versions before choosing.

---

### DECISION-006 — Diagnostic rendering crate

**Status:** open
**Raised:** 2026-10-09 — Session 1
**Resolved by:** human
**Blocks:** Human-readable diagnostics (milestone 1 needs basic output; milestone 3 needs the full format)

**Question:** What renders rustc-style diagnostics?

**Options:**
- A) `ariadne` — spec §9 calls it "a good fit"; pretty output.
- B) `codespan-reporting` — closer to rustc's exact layout, which the spec's E0401 example uses.
- C) Hand-rolled — no dependency; exact control over the golden output.

**Notes:** The section 10 rejected-program output is an acceptance test, so whichever is chosen must reproduce it byte-for-byte — or the spec example is adjusted to match the crate. JSON output is hand-written either way (serde or manual).

---

### DECISION-007 — CLI argument parsing

**Status:** open
**Raised:** 2026-10-09 — Session 1
**Resolved by:** human
**Blocks:** `lughac` driver (milestone 1)

**Question:** How is the `lughac` command line parsed?

**Options:**
- A) `clap` (derive) — standard; generates help; exit code on bad usage must be forced to 2 (spec §9).
- B) Hand-rolled — tiny CLI surface, zero dependencies.

---

### DECISION-008 — Acceptance test harness

**Status:** open
**Raised:** 2026-10-09 — Session 1
**Resolved by:** human
**Blocks:** Milestone 1 "done when" check

**Question:** How are `.la` programs compiled and checked in tests?

**Options:**
- A) `tests/programs/*.la` with sibling `.stdout` / `.exit` (and `.stderr` for rejected programs) files, driven by one Rust integration test that runs `lughac` on each. **Suggested.**
- B) A snapshot crate such as `insta` / `trycmd`.

**Notes:** End-to-end tests need `cc`, LLVM and `libgc` installed; CI must install them.

---

## DEFERRED — Acknowledged, not yet needed

### DECISION-004 — Design system (docs/DESIGN.md)

**Status:** deferred
**Raised:** 2026-10-09 — Session 1
**Revisit when:** Any UI (web playground, editor extension, GUI) is proposed.

**Question:** What design tokens and component rules apply?

**Notes:** lughac is a CLI; `setup.md` Step 9 was skipped.

---

### DECISION-009 — How lugha_rt.c is built and found at link time

**Status:** deferred
**Raised:** 2026-10-09 — Session 1
**Revisit when:** Starting milestone 4.

**Question:** Is `lugha_rt.c` compiled by `build.rs` and embedded in the binary, embedded as source and compiled on first use, or installed alongside `lughac`?

**Notes:** Must work for `lughac run` from any directory. Spec §9 only says it is "compiled once and linked into every program".

---

### DECISION-010 — Cost of array copies

**Status:** deferred
**Raised:** 2026-10-09 — Session 1 (from spec §12)
**Revisit when:** Milestone 5 works and real programs can be measured.

**Question:** Keep eager deep copies, or move to copy-on-write?

**Notes:** Copy-on-write needs a per-array reference count, which Boehm doesn't provide. Spec says measure first.

---

## RESOLVED

None yet.
