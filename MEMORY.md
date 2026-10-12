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

**Note (PRP-028):** the source is kept as three fragments: `runtime/lugha_rt.c` (core, first), `stack.c` and `print.c`. `link::RUNTIME_SOURCE` joins them with `concat!`, so lughac still writes and compiles one `lugha_rt.c`.

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

### 17. Structs (PRP-015)

**Decision:**
- `Type::Struct(name)`; `Checked.structs` lists fields in declaration order and is codegen's only source of struct layouts.
- Codegen holds structs as SSA aggregates (`%S`). Fields use element storage (`bool` as `i8`). Sizes are hand-computed with natural alignment (`codegen/structs.rs::layout`) and are used for every element size.
- Struct parameters are `ptr` to caller storage, used directly as the callee's slot and marked borrowed. Arguments are spilled to an entry `alloca`. Returns are by value.
- Copy helpers length-prefix struct names (`lugha_copy_5Point_arr`).
- A struct may contain itself through an array (spec §4 clarified).
- Codes: E0307 recursive struct, E0308 duplicate field, E0413 missing fields, E0414 field given twice.

**Why:** Fits the existing `Value` model. No `unsafe` (constant-index struct GEPs). One layout source.

**Rules out:** structs by pointer everywhere; copying struct arguments.

---

### 18. The spec is the acceptance source (PRP-016)

**Decision:** `lugha::SPEC` embeds the spec verbatim (`include_str!`). `lughac spec` prints it, and a broken pipe still exits 0. Tests extract the §10/§11 programs and their stated results from `SPEC` (`tests/common/spec_programs.rs`, `tests/spec.rs`). `check::check` returns the standard stage shape; no milestone stop mechanism remains.

**Why:** One source of truth: the printed spec, the reference and the acceptance tests can't drift.

**Rules out:** a condensed second copy of the spec; hand-maintained expected outputs for §10.

---

### 19. CI and protected-file enforcement (PRP-017)

**Decision:**
- **CI:** `.github/workflows/ci.yml` runs in the `ubuntu:26.04` container. It installs `llvm-21-dev libgc-dev build-essential` from the stock archive, gets Rust via rustup from `rust-toolchain.toml`, and runs fmt, clippy and test.
- **Supply chain:** actions are pinned by SHA (`actions/checkout` v7.0.1, `actions/cache` v6.1.0); the token is `contents: read`.
- **Protected files:** `.claude/settings.json` denies Read/Edit on the `.llmignore` paths. It blocks only Claude's own tools, not a sandbox.

**Why:** CI matches the development machine package for package, with no third-party apt source.

**Rules out:** apt.llvm.org; unpinned or third-party actions.

---

### 20. Eager array copies stay (DECISION-010)

**Decision:** Eager deep copies at the spec §4 copy sites; no copy-on-write. Measurements are in `docs/decisions/decision-010.md`. Spec §12 is ticked, with the tip to write through places.

**Why:** Most copied arrays are written right away, so copy-on-write saves almost nothing. It would also need full reference counting, which Boehm makes unnecessary otherwise. The savings worth having come from dead locals (a static move-on-last-use check) or from writing in place.

**Rules out:** copy-on-write and reference counts on arrays. A move-on-last-use optimization stays possible (unscheduled; TODO).

---

### 21. No design system (DECISION-004)

**Decision:** Not applicable. There is no UI and no `docs/DESIGN.md`. Diagnostics formatting follows spec §9 and DECISION-011. A future UI opens a new decision.

**Why:** Nothing to style.

**Rules out:** creating `docs/DESIGN.md` without a UI proposal.

---

### 22. Move on last use (PRP-018)

**Decision:**
- The checker records the binding each local `Name` resolves to (`mentions`), and `check/liveness.rs` computes `Checked.moves`.
- **What moves:** store-site values (let init, assignment value, list-literal element, struct-literal field) that are place chains rooted in a `let` local, where:
  - no mention of that binding starts after the value ends;
  - every enclosing loop also encloses the declaration.
- **Codegen:** `value_for_store` skips the deep copy for them. Parameters, `for … of` variables and repeat values never move.
- **Spec:** §4 says the compiler may move from a local that is never used again.

**Why:** It removes unobservable copies (`grid = next`, `ps[i] = p`) with no runtime cost; `particles` went from 253 to 130 ms. The rule errs towards copying: no flow-sensitive dataflow.

**Rules out:** moves from parameters or loop variables; flow-sensitive liveness without a new PRP.

---

### 23. v1 direction (session 24)

**Decision:** `docs/specs/Language v1 Draft.md` (a delta against v0) is approved.
- **Scope:** enums with flat exhaustive `match`; a built-in `Option<T>`; recursive enums; methods in `impl` blocks with `self`/`mut self`; growable `T[]` (`push`/`pop`/`clear`).
- **Dot access** (`Shape.Circle(2.0)`), with E0311 (a local named like a type) and E0207 (`::`, with help).
- **Recursive enums:** no `Box`; only the slots that lead back to the enum live on the heap; `Box<…>` is E0305 with help.
- **Milestones:** M6 enums, M7 methods, M8 growable arrays. v0 stays authoritative until each milestone folds its sections in.

**Why:** Each choice plays to Lugha's strengths: everything is a value, the spec is small enough to sit in a model's prompt, there is one way to do each thing, mistakes are caught at compile time, and allocation is invisible.

**Rules out:** user generics, modules and closures in v1; `Box`, `::` paths, `indirect` markers; a separate list type.

---

### 24. Milestone 6 plan and the v1 gate (PRP-019)

**Decision:**
- **Split:** M6 is three PRPs:
  - 019 syntax for all of v1;
  - 020 enums/`match`/`Option` plus the new `Language v1 Specification.md` (v0 frozen);
  - 021 recursive enums and copies.
- **Gate:** v1 constructs parse, but `driver/gate.rs` stops them before checking with "not implemented yet: … (milestone N)" (exit 2). Each PRP deletes its entries; the checker and codegen keep `unreachable!` arms for gated nodes. No `Stop`/`Checking` comeback.
- **AST:** `self` is `ExprKind::Name("self")`; receivers are `FunDecl.receiver`; `Option<T>` is `TypeKind::Option`.

**Why:** It keeps the checker plumbing simple, and `--emit=ast` works on v1 at once.

**Rules out:** reintroducing checker stops.

---

### 25. Enums in the compiler (PRP-020)

**Decision:**
- **Checker:** `Type::Enum`/`Type::Option`, and `Checked.enums`. Construction is recognised in the checker (`E.V` with a type-name base is recorded `void`) and in codegen (same shape).
- **Bindings:** `match` bindings are `Binding::Pattern` (immutable, borrowed). No local may be named like a type (E0311, in `declare`).
- **Layout:** `%E` / `%"Option<T>"` = `{ i32, [k x iA] }`, with `A` the largest payload alignment (1, 4 or 8). Payloads are accessed with constant GEPs through an anonymous variant struct. Enums are passed like structs (`is_aggregate`).
- **Sharing:** enum values *share* payloads, never deep-copied (`contains_array` is false for them), because payloads are immutable. Payloads are still copied from places at construction, and bindings are copied when stored.
- **Spec file:** `Language v1 Specification.md` is the spec `lughac spec` prints; the v0 file is frozen. Each milestone folds its draft sections in.
- **Gate:** it still stops recursive enums, and struct cycles through an enum or `Option`, by name reachability. Methods stay gated too.

**Why:** It keeps value semantics without copy helpers, and the printed spec always matches the compiler.

**Rules out:** deep-copying enums; mutable payload places.

---

### 26. Recursive data (PRP-021)

**Decision:**
- **One rule:** `check::indirect_slot(owner, slot, structs, enums)` decides which enum payload slots lead back to their enum: inline through struct fields, enum payloads and `Option`'s payload, not arrays. Codegen uses it for layout, construction and `match` reads.
- **Layout:** an indirect slot is an 8-byte pointer to a `lugha_rt_alloc` copy, made at construction and never mutated. Everything else stays inline, so `Nil` allocates nothing.
- **Copies:** sharing (MEMORY 25) makes copies O(1).
- **E0307** stays for struct-only cycles.
- **Deep recursion:** panics with `stack overflow` since PRP-025 (MEMORY 30).

**Why:** It is the approved v1 direction (no `Box`), with the heap used only where needed.

**Rules out:** `Box`/`indirect` annotations; reference counting.

---

### 27. Methods (PRP-022)

**Decision:**
- **Collection:** `impl` signatures are collected per type (methods live in the type's namespace) and keyed `Type.name`; the symbols are `lugha_fn_<Type>.<name>`.
- **Receivers:**
  - `self` is a borrowed parameter, passed as a pointer to a caller-side copy.
  - `mut self` is `Binding::SelfMut`, assignable but never moved from. The caller passes its place's own address (`place_address`), so writes land in place.
- **Call checks:** a `mut self` receiver gets the assignment place checks (E0501 with receiver wording, E0502, E0506, E0507). E0508 rejects an argument mentioning the receiver's root.
- **Call order:** `method_call` before `construction` before plain calls (variants keep priority via E0310).
- **The driver gate is deleted:** every v1 construct is checked, and `xs.push` is E0415 until milestone 8.

**Why:** In-place `mut self` keeps value semantics without copies. E0508 closes the only aliasing hole at compile time.

**Rules out:** methods on built-in types (until M8's built-ins), implicit argument copies, the gate.

---

### 28. Growable arrays and v1 complete (PRP-023)

**Decision:**
- **Layout:** arrays are `{ i64 len; i64 cap; T elems[cap] }`, with elements at byte 16 (`heap::ARRAY_HEADER`); strings keep their 8-byte header. Literals and copies are full (`cap == len`), and copies copy only `len` elements.
- **Built-ins:** `push`/`pop`/`clear` are array built-ins checked like `mut self` methods (E0501/E0502/E0506/E0507, with the method named), but not E0508: `xs.push(xs[0])` is fine because the value is read before growth.
- **Growth:** `lugha_rt_array_grow` doubles the capacity (at least 4), and the grown array replaces the old one in its single owning place.
- **`pop`** moves the element out into an `Option`.
- **Moves:** `push`'s argument is a move-on-last-use store site.
- **Spec:** the v1 spec is complete and `Language v1 Draft.md` was deleted.

**Why:** one sequence type; value semantics kept by single ownership; the spec is the only definition.

**Rules out:** other array methods (`insert`, slicing, `.cap`), string methods — v2.

---

### 29. Internal compiler errors (PRP-024)

**Decision:**
- **Hook:** `driver::main` installs a panic hook that records the location and prints nothing.
- **Guard:** it runs `run` inside `driver/guard.rs::guarded`, which catches a panic and writes one internal error in the requested format (human `error: internal compiler error: …` with a help note naming the source location, or a `"code":null` JSON line), then exits 2.
- **Library:** `run` installs no hook, so library users and tests keep Rust's normal panic output.

**Why:** architecture rule 6 and spec §9 (JSON stderr carries nothing else).

**Rules out:** `panic = "abort"`; a crash switch in the binary.

---

### 30. Stack overflow panic (PRP-025)

**Decision:**
- **Setup:** `lugha_rt_init`, after `GC_INIT`, records the main stack's lowest address (`pthread_getattr_np`), installs a 64 KiB static alternate signal stack, and installs a `SA_SIGINFO | SA_ONSTACK` SIGSEGV handler.
- **Stack faults:** a fault within 64 KiB of the stack's end flushes stdout and prints `panic: stack overflow` (the one panic without a location), then `_exit(101)`.
- **Other SIGSEGVs:** they restore the default action and re-raise (exit 139).
- **Failures:** any failed setup step leaves the old behaviour. Linux/glibc only.

**Why:** It costs nothing per call, and only C code can produce other segfaults, which stay visible.

**Rules out:** per-call depth counters; catching other signals.

---

### 31. The `swap` intrinsic, Lugha v1.1 (PRP-026)

**Decision:**
- **Form:** `swap(a, b)` is an intrinsic, and `swap` is a reserved name. It takes two places of the same type (E0405, E0502, E0403), each under the assignment place rules: E0501, E0506 and E0507 with swap wording, via `PlaceUse` in `check/assign.rs`.
- **E0509:** reported when one place path is a strict prefix of the other (`Path::overlaps` with different depths). Equal depths are allowed even when the places might be the same.
- **Codegen:** both addresses first (bounds-checked), then two loads and two crossed stores. No copy and no allocation; the arguments are not store sites for liveness.
- **Version:** the spec is retitled v1.1 (same file), with a changes note. Tag `v1.1`.

**Why:** DECISION-010 showed that a buffer still in use is copied on every `ys = buf`. CoW and move on last use can't avoid that copy; `swap` makes it O(1). `sort_swap`: 95 → 5 copies, 154 → 109 ms.

**Rules out:** a `take`/move-out intrinsic, a swap operator.

---

### 32. E0510: no changing what a statement writes, Lugha v1.2 (PRP-027)

**Decision:**
- **The rule:** an assignment, `swap` or `mut self` call fixes its place first. The rest of the statement (value, arguments, the place's own indexes) may not change a place that strictly contains it: same root, `Path::overlaps`, smaller depth. That is E0510.
- **Allowed:** same-depth, sibling and self changes. Their addresses stay valid; push re-reads the array after its argument.
- **How it works:**
  - `check_place_for` records every changed place in `Checker.changes`, which is reset per body.
  - Each statement takes a `changes_mark()` before checking its parts, then calls `no_change_inside`.
  - Each change is reported once.
  - E0510 is checked before E0508 and suppresses it.
- **Code:** no codegen change. The spec is retitled v1.2. Tag `v1.2`.

**Why:** Only the statement itself can change a local (no closures, no globals), so a compile-time check is exact enough and costs nothing at run time. It matches E0507/E0508.

**Rules out:** re-resolving places at run time; rejecting every change to the root.

---

### 33. v2 direction (session 36)

**Decision:** `docs/specs/Language v2 Draft.md`, a delta against v1.2, is approved.
- **Scope:**
  - M9: nested patterns, guards, deep `==` on every type, string `<` (bytewise).
  - M10: modules. A file is a module, `import a.b;` binds `b`, access is always qualified, cycles are allowed, `pub` is per item, and a `pub struct` exposes all its fields.
  - M11: generics on fun, struct, enum and methods, with built-in `Eq`/`Ord` bounds. Type arguments are always inferred (else E0412), bodies are checked once against their bounds, and codegen monomorphizes. `Option<T>` becomes an ordinary generic enum.
- **Syntax:** `impl Stack<T>` (no `impl<T>`); bounds come from the type's declaration.
- **Symbols:** `lugha_fn_a.b.f` and `lugha_fn_max<i64>`; root-module names unchanged; `lugha_eq_<type>` helpers.
- **Codes:** E0208, E0312–E0316, E0418–E0420 proposed. v1.2 stays authoritative until M11 folds in a full v2 spec.

**Why:** It plays to the same strengths as v1: one way to write each thing (dot everywhere, no turbofish, no selective imports), mistakes caught at compile time (bodies checked once), everything still a value, and a spec small enough for a model's prompt.

**Rules out (v3):** closures, shared references, user interfaces, explicit type arguments, selective imports, per-field visibility, or-patterns.

---

### 34. v2 syntax and the gate (PRP-030)

**Decision:**
- **Milestone 9 split:** PRP-030 syntax plus gate, PRP-031 `==`/`<`, PRP-032 patterns and guards.
- **AST:**
  - `Program.imports`; `public: Option<Span>` on fun, extern, struct and enum; `type_params` (`TypeParam { name, bounds }`) on fun, struct and enum;
  - `ImplDecl { module, params }`;
  - `TypeKind::Applied(Box<AppliedType>)` for `m.T` and `T<args>`; plain names stay `Named` and `Option<T>` stays `Option` until M11;
  - `ExprKind::StructLit(Box<StructLiteral { module, name, fields }>)`;
  - pattern slots are full `Pattern`s (`PatSlot` is gone, and `Pattern::slot_binding` keeps v1's meaning); `Arm.guard`.
- **Boxing:** `Applied` and `StructLit` are boxed so `Expr` stays small. They had grown `ExprKind` from 72 to 128 bytes and overflowed the E0206 depth test. `ExprKind` is now 64 bytes.
- **Gate:** `driver/gate/` (`first` and `Walk`) stops M9, M10 and M11 constructs in `pipeline::front` with exit 2. Type arguments are gated only on the program's own structs and enums, so `Box<L>` keeps its E0305 help.
- **Sexp forms:** `(import a b)`, `(pub fun …)`, `(type-params T (U Eq Ord))`, `(applied m.T args…)`, `(struct-lit m.T …)`, `(guard …)`.

**Why:** It is v1's proven approach (MEMORY 24). Parsing is done once for all of v2, and each later PRP only deletes gate entries.

**Rules out:** checker or codegen support for any v2 construct before its milestone.

---

## CURRENT PROJECT STATE

### Fully Working
- PRP-030: all v2 syntax parses (`--emit=ast`); `driver/gate/` stops v2 constructs until their milestone
- PRP-029: `check/body.rs` (function bodies) and `check/builtins.rs` (array built-ins) split out of `stmt.rs` and `method_calls.rs`
- PRP-028: the C runtime split into three fragments (`runtime/{lugha_rt,stack,print}.c`), joined into one `lugha_rt.c`
- **Lugha v1.2**: PRP-027 E0510 (`check/conflicts.rs`): no lost writes when a statement changes what contains its place
- **Lugha v1.1**: PRP-026 `swap(a, b)` intrinsic (`check/swap.rs`, `codegen/swap.rs`, E0509)
- **Milestone 4 complete**: C runtime, intrinsics, `extern fun`, overflow/division panics; `m4/` programs pass
- **Milestone 3 complete**: type checker (E03xx–E05xx, W0101), real-typed codegen, §4 casts; `m3/` programs pass
- **Milestone 2 complete**: functions, recursion, locals, control flow; `m2/` acceptance programs pass (§11 program exits 55)
- **Milestone 1 complete**: `lughac build|run|check`, `--emit`, `-O`, human/JSON diagnostics; `m1/` acceptance programs pass
- Toolchain installed and verified: Rust 1.99.0, LLVM 21.1.8, libgc, cc
- Crate initialised: package `lugha`, binary `lughac`; builds, fmt/clippy/test pass
- **Milestone 8 complete — Lugha v1 complete**: PRP-023 growable arrays (`codegen/growth.rs`, `lugha_rt_array_grow`); the v1 spec is complete
- **Milestone 7 complete** — PRP-022 methods: `check/{methods,method_calls}.rs`, `codegen/methods.rs`; driver gate removed
- **Milestone 6 complete** — PRP-021 recursive enums: `indirect_slot`, `codegen/construct.rs`; the gate stops only methods
- PRP-020 enums, `match`, `Option` (non-recursive) and `Language v1 Specification.md`
- PRP-019 v1 syntax: lexer/parser/AST for all of v1, E0207, `driver/gate.rs`
- v1 spec draft approved (`docs/specs/Language v1 Draft.md`): milestones 6–8 planned
- PRP-018 move on last use: `check/liveness.rs`, `Checked.moves`; particles benchmark 2× faster
- DECISION-004 closed as not applicable (no UI)
- DECISION-010 resolved: eager copies stay, with measurements in `docs/decisions/decision-010.md`
- PRP-017 housekeeping: README, CI (green), `.claude/settings.json` deny rules, `docs/setup.md`
- **Milestone 5 complete — Lugha v0 complete**: every §10 program passes, extracted from the spec; `lughac spec`
- PRP-016 spec command: `lugha::SPEC`, `tests/spec.rs`, dead `Unsupported` paths removed
- PRP-015 structs: `check/structs.rs`, `check/errors/` split by code range, `codegen/structs.rs` (layout, literals, fields, place addresses); centroid prints `centroid: 2.0, 1.0`
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
- Nothing

### Not Started
- v2 milestones 9–11 (draft approved, session 36)

---

## NEXT SESSION START POINT

The Lugha v2 draft is approved (`docs/specs/Language v2 Draft.md`, MEMORY 33). v1.2 is still the spec `lughac spec` prints. Next is **milestone 9, patterns and equality**: nested patterns, `if` guards, deep `==` on every type (`lugha_eq_<type>` helpers), and string `<` (`lugha_rt_str_cmp`). Start with discovery for its PRP(s), citing the draft's §3–§5, §9 and §10 (patterns program). The draft's §12 lists open questions to settle along the way (an unused-binding warning for nested slots).

See `TODO.md`.
