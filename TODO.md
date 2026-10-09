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

### Milestone 1 — Expressions to a binary ✅
Done when `fun main(): i32 { 2 + 3 * 4 }` exits with 14.
- [x] PRP-001: test runner for `tests/programs/` (DECISION-008)
- [x] PRP-002: lexer (full §2 token set, E0101–E0109)
- [x] PRP-003: parser (full §3 grammar, E0201–E0206, S-expression printer)
- [x] PRP-004: codegen + link (inkwell, object file, `cc`)
- [x] PRP-005: `lughac` driver and exit codes; `#[ignore]` removed — **milestone 1 done**

### Milestone 2 — Variables, control flow, functions ✅
Done when the §11 milestone 2 program exits with 55.
- [x] PRP-006: locals and control flow in `main` (`let`/`mut`, assignment, blocks, `if`, `while`, `for`, `break`/`continue`, comparisons, `&&`/`||`)
- [x] PRP-007: functions, parameters, calls, recursion, `return`, `=` bodies — §11 program exits 55 — **milestone 2 done**

### Milestone 3 — Type checker and diagnostics ✅
Done when the §10 rejected program reports E0401 in both formats.
- [x] PRP-008: checker core (names, types, literal inference, calls, E0301–E0305, E0401–E0407) — E0401 done-when met
- [x] PRP-009: casts, mutability, places, missing returns, loop context, discarded values, W0101 (E0408, E0501–E0505)
- [x] PRP-010: codegen on real types (i32/i64/u8/f64/bool, §4 casts); overflow and division checks moved to milestone 4 (spec §11)
- [x] Diagnostics: error codes, human + JSON (PRP-005), `lughac check` and "remove this semicolon" (PRP-008/009)

### Milestone 4 — Runtime and C ✅
Done when hello world, recursion and the libc example run.
- [x] Resolve DECISION-009 — runtime embedded as source, compiled in the link step
- [x] PRP-011: `lugha_rt.c` and intrinsics (`print`, `println`, `panic`, `to_string`, float formatting), string literals
- [x] PRP-012: `extern fun` and integer overflow/division panics — libc example runs

### Milestone 5 — Heap data
Done when every §10 program passes.
- [x] PRP-013: string operations (`+`, `==`/`!=`, `.len`, bounds-checked `s[i]`); GC allocation landed in PRP-011
- [x] PRP-014: arrays, bounds checks, repeat literals, `for`-`of`, array copies — primes prints 25
- [x] PRP-015: structs and their copies — centroid prints `centroid: 2.0, 1.0`
- [ ] PRP-016: `lughac spec` and every §10 program — **milestone 5 done**; remove the now-dead `CheckError::Unsupported`/`CodegenError::Unsupported` and the checker's `Stop`
