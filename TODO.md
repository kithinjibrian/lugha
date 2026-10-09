# TODO — lugha

Outstanding tasks. Check items off as they are done; move shipped work to CHANGELOG.md.
Questions that need a decision go in DECISIONS.md, not here.
Every feature below gets a PRP (`PRPs/prp-{NNN}-{feature_name}.md`) before code.

---

## Machine prerequisites

- [x] Install Rust via rustup (1.99.0)
- [x] Install `llvm-21-dev` (21.1.8)
- [x] Install Boehm GC dev package (`libgc-dev`)
- [x] C compiler (`cc`, GCC 15.2) — present

## Setup

- [x] Resolve DECISION-001 — crate layout
- [x] Resolve DECISION-002 — error and diagnostic types
- [x] Resolve DECISION-003 — edition and MSRV
- [x] Resolve DECISION-005 — LLVM version / inkwell feature
- [x] Resolve DECISION-006 — diagnostic rendering crate
- [x] Resolve DECISION-007 — CLI parsing
- [x] Resolve DECISION-008 — acceptance test harness
- [x] `cargo init --lib` + `src/main.rs`, edition 2024, `rust-toolchain.toml` pinned; commit `Cargo.toml` + `Cargo.lock`
- [x] Fill in CLAUDE.md FILE ORGANIZATION `src/` tree and the pinned Rust version
- [x] Copy resolved decisions into MEMORY.md
- [ ] Replace the one-line README.md with a project description
- [ ] Add CI: fmt, clippy, test — installing LLVM and libgc
- [ ] Mirror `.llmignore` as deny rules in `.claude/settings.json`
- [ ] Decide whether `setup.md` stays at the root or moves to `docs/`

## Milestones (spec §11) — build strictly in order

### Milestone 1 — Expressions to a binary
Done when `fun main(): i32 { 2 + 3 * 4 }` exits with 14.
- [x] PRP-001: test runner for `tests/programs/` (DECISION-008)
- [x] PRP-002: lexer (full §2 token set, E0101–E0109)
- [x] PRP-003: parser (full §3 grammar, E0201–E0206, S-expression printer)
- [ ] PRP: codegen + link (inkwell, object file, `cc`)
- [ ] PRP: `lughac build` driver and exit codes — must remove the `#[ignore]` on `tests/programs.rs::programs`

### Milestone 2 — Variables, control flow, functions
Done when the §11 milestone 2 program exits with 55.
- [ ] PRP: `let` / `let mut`, assignment, blocks with tail values
- [ ] PRP: `if` expressions, `while`, `for` over ranges, optional condition parentheses
- [ ] PRP: functions, calls, recursion, `=` bodies

### Milestone 3 — Type checker and diagnostics
Done when the §10 rejected program reports E0401 in both formats.
- [ ] PRP: type checker (all primitives, literal inference, casts)
- [ ] PRP: mutability and return checking
- [ ] PRP: overflow and division checks in codegen
- [ ] PRP: diagnostics (error codes, human + JSON, `lughac check`, "remove this semicolon")

### Milestone 4 — Runtime and C
Done when hello world, recursion and the libc example run.
- [ ] Resolve DECISION-009 — runtime build/location
- [ ] PRP: `lugha_rt.c` and intrinsics (`print`, `println`, `panic`, `to_string`, float formatting)
- [ ] PRP: `extern fun` and string literals

### Milestone 5 — Heap data
Done when every §10 program passes.
- [ ] PRP: Boehm GC, strings
- [ ] PRP: arrays, bounds checks, repeat literals, `for`-`of`
- [ ] PRP: structs and array copies
- [ ] PRP: `lughac spec`
