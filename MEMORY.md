# MEMORY.md — lugha

Records resolved architectural decisions and current project state.
Read this at the start of every session before writing any code.
Open questions live in `DECISIONS.md`, not here.

---

## ARCHITECTURAL DECISIONS

### 1. Language and license

**Decision:** The project is written in Rust and licensed GPL-3.0.

**Why:** Established by the initial commit (`Cargo` `.gitignore`, GPL-3.0 `LICENSE`).

**Rules out:** Dependencies with GPL-incompatible licenses.

---

### 2. Return-based error handling

**Decision:** Expected failures return `Result<T, E>` with typed error enums; panics are reserved for bugs. See CLAUDE.md → ERROR HANDLING.

**Why:** Makes every failure path visible in the type signature and impossible to ignore silently.

**Rules out:** `unwrap()` on external input, `Option` as an error signal, stringly-typed errors.

---

### 3. The spec defines the language

**Decision:** `docs/specs/Language v0 Specification.md` is the source of truth for Lugha v0. The compiler implements it exactly; spec changes come before code changes and need human approval.

**Why:** No model has seen Lugha in training, and the spec doubles as the LLM-ready bundle printed by `lughac spec` — any drift makes both wrong.

**Rules out:** Implementing behavior the spec doesn't describe; "fixing" the spec silently to match code.

---

### 4. Compiler shape

**Decision:** `lughac` is a Rust compiler using inkwell. Pipeline: lex → parse → check → lower to LLVM IR → optimize/emit object → link with `cc` + `lugha_rt.o` + `-lgc -lm`. Each stage stops the pipeline on error. Source files use the `.la` extension.

**Why:** Spec §9, decided 2026-10-09.

**Rules out:** A hand-written backend, an interpreter, a self-hosted compiler in v0.

---

### 5. Milestone order

**Decision:** Build spec §11 milestones 1→5 strictly in order. In milestones 1–2 every value is treated as `i64`; real types arrive in milestone 3.

**Why:** Every milestone ends in a program that runs, which keeps the pipeline end-to-end from day one.

**Rules out:** Building the type checker before codegen works; starting heap data before the runtime exists.

---

### 6. Symbol naming

**Decision:** Lugha functions are emitted as `lugha_fn_<name>`, runtime functions as `lugha_rt_<name>`, generated copy helpers as `lugha_copy_<type>`. Extern names are used verbatim; extern names starting with `lugha_` are rejected.

**Why:** The earlier single `lugha_` prefix let a user function like `alloc` collide with the runtime. Disjoint prefixes make collisions impossible. (Spec fix, 2026-10-09.)

**Rules out:** Unprefixed symbols for Lugha functions; runtime functions outside `lugha_rt_`.

---

### 7. PRP naming

**Decision:** PRP files are `PRPs/prp-{NNN}-{feature_name}.md` — three-digit sequential number, lowercase snake_case name.

**Why:** User direction, 2026-10-09.

**Rules out:** Unnumbered PRPs; renumbering or reusing numbers.

---

### 8. Crate layout (DECISION-001)

**Decision:** One package: library `src/lib.rs` with stage modules `lexer`, `parser`, `check`, `codegen`, `driver`; thin binary `src/main.rs` that parses the CLI and calls the library.

**Why:** Stages testable from `tests/` without spawning processes.

**Rules out:** A Cargo workspace; logic in `main.rs`.

---

### 9. Stage results and errors (DECISION-002)

**Decision:** Each stage returns `Result<(Output, Vec<Diagnostic>), Vec<Diagnostic>>` — `Ok` = output + warnings, `Err` = all errors and warnings. Internal lughac errors use `thiserror`.

**Why:** Errors stop the pipeline (spec §9) while warnings still surface on success.

**Rules out:** Panicking or returning a single error from a stage; `anyhow` in library code.

---

### 10. Toolchain (DECISION-003, DECISION-005)

**Decision:** Rust edition 2024, toolchain pinned to 1.99.0 in `rust-toolchain.toml` and mirrored as `rust-version = "1.99"`. LLVM 21 via `inkwell` 0.10, feature `llvm21-1-prefer-dynamic` (shared `libLLVM.so`; static linking fails on Ubuntu for lack of a static Polly library).

**Why:** LLVM 21 is Ubuntu 26.04's default `llvm-dev` and supported by inkwell 0.8–0.10.

**Rules out:** Unpinned toolchains; any other LLVM major without a new decision.

---

### 11. Diagnostics and CLI crates (DECISION-006, DECISION-007)

**Decision:** `codespan-reporting` (ASCII characters) renders human diagnostics; `clap` derive parses the CLI.

**Why:** Closest to rustc's layout used by the spec; clap's usage-error exit code is already 2.

**Rules out:** `ariadne`. Its output can't match the original spec byte-for-byte; see decision 13.

---

### 12. Acceptance tests (DECISION-008)

**Decision:** `tests/programs/<name>.la` with sibling `.stdout`, `.exit` and (for rejected programs) `.stderr` files; one integration test runs them all and compares exactly.

**Why:** No dependency; adding a test is adding files.

**Rules out:** Snapshot crates (`insta`, `trycmd`).

---

### 13. Diagnostics rendering and records (DECISION-011, PRP-005)

**Decision:**
- Human output comes from codespan-reporting with ASCII characters and trailing whitespace stripped; the spec example follows its layout (`  --> `, blank line after).
- `Diagnostic` has `label` for the primary span's text, and JSON has a matching `"label"` key.
- Internal errors have no code (`"code":null`) and exit 2.

**Why:** A maintained renderer beats string patches, and both formats must come from one record (spec §9).

**Rules out:** Hand-rolled rendering; patching codespan's text beyond trimming; putting primary text in `labels`.

---

### 14. Codegen reads the checker's types (PRP-010)

**Decision:** codegen takes `&Checked` and reads every expression's type from it; the AST plus that table is its only input (CLAUDE.md rule 4). It keeps only `Value { Val, Void, Never }` to track divergence, since blocks have no `ExprId`. Checker invariants are `expect`/`unreachable!` in codegen, not errors.

**Why:** one type computation; two that can disagree is what rule 4 forbids.

**Rules out:** codegen-side type inference or checks; a separate typed IR in v0.

---

### 15. Runtime embedded as source (DECISION-009, PRP-011)

**Decision:**
- `runtime/lugha_rt.c` is embedded in lughac and compiled in the single `cc` link call.
- `bool` and `u8` cross into the runtime zero-extended to `i32`.
- The C `main` calls `lugha_rt_init()`, which runs `GC_INIT()`.

**Why:** No build script, no new dependency, no install layout, and no reliance on C's narrow-parameter extension rules.

**Rules out:** precompiled runtime objects; `build.rs`.

---

### 16. Array value semantics (PRP-014)

**Decision:**
- `check::Type` is recursive (`Array(Box<Type>)`), so it is `Clone`, not `Copy`.
- Codegen copies a value whose type contains arrays when it is read from a place (looking through `if`/block tails) and stored: `let`, assignment, literal elements, repeat fill. On `return`/tail it copies unless the place is rooted in an owned `let` local (which moves). Parameters and `for … of` variables are borrowed (`Local.borrowed`).
- Call arguments and fresh values are never copied. Plain element arrays copy with `llvm.memcpy`; nested ones through internal `lugha_copy_<mangle>` functions generated on demand.
- `for x of xs` assignments to an overlapping place are E0507 (conservative: any two indexes may be equal).

**Why:** spec §4 value semantics without copy-on-write (DECISION-010 deferred).

**Rules out:** copying call arguments; sharing arrays between places.

---

## CURRENT PROJECT STATE

### Fully Working
- **Milestone 4 complete**: C runtime, intrinsics, `extern fun`, overflow/division panics; `m4/` programs pass
- **Milestone 3 complete**: type checker (E03xx–E05xx, W0101), real-typed codegen, §4 casts; `m3/` programs pass
- **Milestone 2 complete**: functions, recursion, locals, control flow; `m2/` acceptance programs pass (§11 program exits 55)
- **Milestone 1 complete**: `lughac build|run|check`, `--emit`, `-O`, human/JSON diagnostics; `m1/` acceptance programs pass
- Toolchain installed and verified: Rust 1.99.0, LLVM 21.1.8, libgc, cc
- Crate initialised: package `lugha`, binary `lughac`; builds, fmt/clippy/test pass
- PRP-014 arrays: `check/array.rs`, `codegen/{array,copy}.rs`; element layout in `heap.rs`; deep copies per spec §4; primes prints 25
- PRP-013 string operations: `check/access.rs`, `codegen/heap.rs` (integer address arithmetic — user chose it over an audited `unsafe` GEP), `lugha_rt_panic_bounds`; AST `Index` bracket span
- PRP-012 extern and panics: externs declared verbatim with C ABI (`zeroext`), string args via a safe struct GEP to field 1; checked arithmetic via `llvm.*.with.overflow`; AST operator spans
- PRP-011 runtime and intrinsics: `runtime/lugha_rt.c`, `link::RUNTIME_SOURCE`, `check::Type::String`, intrinsics in checker and codegen (`codegen/runtime.rs`), `SourceInfo` for panic locations
- PRP-010 codegen on real types: `emit_ir`/`emit_object` take `&Checked`; arith.rs and cast.rs; `lugha_fn_main` returns i32
- PRP-009 casts and flow checks: `check/assign.rs`, `check/flow.rs`, `Binding` on locals, W0101 once per block; warnings returned from `check()`
- PRP-008 checker core: `src/check/`, bidirectional checking with `Expect`, `Type::Error` recovery, `Never` for divergence; E0401 matches spec in both formats
- PRP-007 functions: signatures declared first (`lugha_fn_<name>`), params as locals, calls, `return`, `Value::Never` divergence per §6; `lugha_fn_main` returns i64 until M3 — **milestone 2 complete**
- PRP-006 locals and control flow: codegen `Value` kinds (Int i64 / Bool i1), scopes with entry-block allocas, if/while/for/break/continue, short-circuit; `tests/programs/m2/` (7 programs)
- PRP-005 driver: `src/driver/` (clap CLI, pipeline, codespan render, hand-written JSON); E0110 for invalid UTF-8
- PRP-004 codegen + link: `codegen::emit_ir`/`emit_object` (O0/O2), `link::link`; M1 subset; wrap + trap until M4
- PRP-003 parser: `lugha::parser::parse`, full §3, codes E0201–E0206; AST with dense `ExprId`s; `parser::sexp` printer
- PRP-002 lexer: `lugha::lexer::lex`, full §2, codes E0101–E0109; `Span`, `Diagnostic` types
- PRP-001 test runner: `tests/programs/` cases run by `tests/programs.rs` (ignored until milestone 1); 17 unit tests
- AI context system scaffolded from `setup.md`
- v0 language spec reviewed and fixed

### In Progress
- Milestone 5: PRP-013 and PRP-014 done; structs (PRP-015) and `lughac spec` (PRP-016) remain

### Not Started
- PRP-015 structs, PRP-016 `lughac spec`

---

## NEXT SESSION START POINT

Milestone 5, PRP-015: structs (spec §3, §4, §7). It covers:
- Top-level `struct` declarations; a struct may not contain itself directly (a new E03xx code).
- Struct literals that initialise every field exactly once, in any order; `p.x` reads and `p.x = v` writes, with the `let mut` root rule.
- Layout per §7, and deep copies of structs that contain arrays, reusing `codegen/copy.rs` (copy sites per the §4 table, including struct-literal fields).

Done-when: the §10 centroid program prints `centroid: 2.0, 1.0`.

Discovery questions to expect:
- codes for missing, duplicate and unknown fields in literals, and for recursive structs;
- by-pointer vs by-value struct representation in codegen (§7);
- `==` on structs (probably E0404, like arrays).
