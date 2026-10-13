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

### 35. Deep equality, string order, and the v2 spec file (PRP-031)

**Decision:**
- **Operators:** `==`/`!=` accept any type. `<`… accept numbers or strings (`lugha_rt_str_cmp`, bytewise, prefix smaller).
- **Operand typing (`check/ops.rs`):** the side that *defers* (a literal, `None`, `[]`, a list or `Some` around one) is checked second and gets the other side's type, any type for comparisons. A number literal never takes a non-numeric type (so `"a" == 1` stays E0404). `None` and `[]` stay quiet on an expected `Error`, so `None == None` gives one E0412.
- **Codegen (`codegen/equality.rs`):** `lugha_eq_<mangle>(T, T) -> i1` per struct, enum, `Option` and array, built like the copy helpers.
  - Structs: field by field, with `bool` fields through `loaded_form`.
  - Arrays: lengths first.
  - Enums: tag compare, then a switch, with indirect payloads loaded through their pointer.

  `mangle` is shared and gains `{len}{Enum}` and `{T}_opt`.
- **Spec:** `Language v2 Specification.md` (v1.2 plus what has landed, "in progress: milestone 9") is `lugha::SPEC`; v1.2's file is frozen. Each PRP folds its draft sections in.

**Why:** Architecture rule 1 requires the printed spec to match the compiler. Equality helpers mirror copy helpers, which already handle recursion.

**Rules out:** a pointer-equality fast path (wrong for NaN); ordering structs or arrays.

---

### 36. Nested patterns, guards, usefulness, W0103 (PRP-032)

**Decision:**
- **Pattern typing (`check/patterns.rs`):** recursive; it returns bindings and a `Pat` (`Wild` or `Ctor(Variant(i, name) | Bool | Int | Str, args)`).
  - **Bare names in slots:** decided by `check::slot_names_variant(variants, name)`, which codegen shares. A bare name is a variant if the slot's enum or `Option` has one by that name, else a binding.
  - **At the top level** a bare name must still be a variant.
- **Usefulness (`check/usefulness.rs`, Maranget):**
  - **W0102:** a pattern is unreachable unless it is `useful` against the earlier unguarded rows. The label names a single covering arm, or else says the earlier arms cover it together.
  - **E0417:** `missing` gives witnesses in declaration order, capped at 100; three are shown, then "and N more". A top-level variant with all-`_` payloads prints bare (the v1 format). Integer and string witnesses at the top level keep the "other values" message.
  - **Bad patterns:** an arm whose pattern had errors is left out, and E0417 is skipped for that match.
- **W0103:** a binding with no mention in its arm (guard or body). It is skipped for a binding named like a type (already E0311).
- **Codegen:** `is_flat` (no guards, slots `_` or bindings) keeps v1's `switch`. Otherwise `match_chain` in `codegen/patterns.rs` goes arm by arm, with a `Subject` that is either a value or a storage-form address (indirect payloads loaded first), and ends in `unreachable`.
- **Removed:** `Pattern::slot_binding`, and the M9 gate entries.
- **Spec:** v2 spec "in progress: milestone 10", with the §10 patterns program. The draft's lowering note now says "arm by arm".

**Why:** These are the discovery answers (session 39). The chain is simple and keeps v1's IR. The usefulness algorithm is the standard one, and E0417 and W0102 both come from it.

**Rules out:** a decision tree; or-patterns; top-level binding patterns.

---

### 37. v2 visibility amendment (session 40)

**Decision:** the owner's visibility note (`docs/decisions/visibility.md`) is adopted for v2. It replaces decision 33's per-item `pub`.
- **Default:** no keyword means visible to every module in the program. There are no packages in v2, so the program is the package.
- **`private`:** limits an item, method, associated function or struct field to its module (E0313).
  - A struct literal outside the module needs every field visible.
  - Variants share their enum's visibility.
  - `private` means private to the module.
- **`public`:** reserved for v3 packages; E0317 in v2.
- **No leaking:** a non-`private` signature (parameters, return, field, payload) can't mention a `private` type (E0318).
- **Keywords:** `private` and `public` replace `pub`; PRP-030's `pub` keyword changes in M10.
- **v3:** interfaces, built-in impls, re-exports, `api-diff`.

**Why:** Per-file `pub` would sit on nearly everything in a program. The package is the real API boundary, and private fields are the only way to protect invariants. The owner chose the keyword pair `private`/`public`.

**Rules out:** `pub`, and private-by-default modules.

---

### 38. Modules front end (PRP-033)

**Decision:**
- **M10 split:** 033 is the front end; 034 covers names, visibility and symbols.
- **Stage:** `src/modules/` (lexer → parser → **modules** → check). `load(main, read)` takes a read closure, so tests run in memory.
  - The root is the main file's directory; `import a.b` reads `<root>/a/b.la`.
  - Each file is read once (BFS), so cycles are free.
  - Errors: E0312 at the import, E0314 for a repeat, a last-segment clash or a clash with an item, and E0319 for importing the main file (a new code).
  - An unreadable existing file is `LoadError::Io` (internal).
- **One offset space:** each file's base is the previous base plus its length plus 1. Tokens are shifted after lexing, so `Span` is unchanged.
- **Driver:** `Source` is a map of `SourceFile { name, base, text }`. The main file is named as given; modules relative to the root.
  - Rendering uses codespan `SimpleFiles` with per-file local ranges.
  - JSON gives local offsets; a label in another file gets a `"file"` key, so single-file output is identical.
- **`--emit=tokens|ast`:** a `; module <name>` header when there is more than one module. `--emit=tokens` re-lexes each module.
- **Visibility syntax:** `Visibility { kind: Private | Public, span }` on fun, extern, struct, enum and methods, plus a real `Field` struct. A visibility on a parameter or before `import` is one E0201. The gate stops visibility as milestone 10.
- **Runner:** a directory `d/` with `d.stdout|exit|stderr` beside it is one case (`runner/dirs.rs`).

**Why:** These are the session 41 discovery answers. The offset space keeps AST nodes small (the E0206 limit) and leaves check and codegen untouched.

**Rules out:** a file id in `Span`; reading outside the root.

---

### 39. Names across modules (PRP-034)

**Decision:**
- **Keys:** an item `X` of module `a.b` is keyed `a.b.X`; a main-file item is keyed `X`, so single-file programs are unchanged. `check::qualify`.
  - Keys are the names in `Type::Struct`/`Enum`, in LLVM type names and in messages (the full module path, a discovery answer).
- **Symbols:** `check::symbol(key)` turns `.` into `/`, giving `lugha_fn_a/b/f` and methods `lugha_fn_a/b/T.m`, so a function can't collide with a method (a discovery answer). Externs keep their C name and are declared once.
- **One id space:** `parser::parse_from(tokens, first)`; the loader chains each module's `expr_count`.
- **Checker:**
  - **Entry:** `check_modules(&[Unit])` (`check()` wraps it), with `check/modules/` (passes over every module, `enter`, `key`, `imported`) and `modules/lookup.rs` (`type_named_by`, `type_key`, `module_call`, `same_extern`, `main` outside the main file).
  - **Side tables:** `Checked.callees` (call → function key; associated calls → method key) and `Checked.annotations` (written-type span → `Type`). Codegen reads these and never resolves names (rule 4).
- **Visibility (`check/visibility.rs`):** a `Privacy` table tracks items (owner module and `private`), methods and fields.
  - E0313 covers foreign private items, methods and fields, and literals of structs with any private field.
  - E0317 is reported at every `public`.
  - E0318 is reported for a non-private signature mentioning a private type (through arrays and `Option`), at the written type.
  - E0304 covers `main` in a module; E0306 one C function with two signatures; E0311 a local named like a module.
- **Codegen:** `emit_ir_modules`/`emit_object_modules` take `&[Unit]` and a `ModuleSource` per file. Panics find their file by offset, with one file-name constant per file.
- **Gate:** generics only.
- **Splits (approved):** `check/modules/{mod,lookup}`, `check/struct_literal.rs`, `codegen/declare.rs`.

**Why:** These follow the session 42 discovery answers. Keys keep one map per kind and leave single-file behaviour identical; the side tables keep codegen from becoming a second resolver.

**Rules out:** packages, `public` semantics, re-exports, selective imports.

---

### 40. v2 interfaces amendment (session 43)

**Decision:** the owner's generics and interfaces note (`docs/decisions/generics.md`) is adopted for v2 as two milestones.
- **M11, generics:** as approved in decision 33: `Eq`/`Ord` bounds, inference only, checked once, monomorphized. Bounds stay on the declaration (`struct Set<T: Eq>`, `impl Set<T>`), not on `impl`.
- **M12, interfaces (new):**
  - `interface Shape { fun area(self): f64; }` is used only as a bound (`T: Shape`), combined with `Eq`/`Ord`.
  - Satisfaction is structural: same name, receiver kind and types, and the method must be visible.
  - Errors: E0421 for a method no bound provides (in a body), E0422 for a type that doesn't satisfy (at a use), E0305 for an interface used as a type.
  - Signatures use concrete types only (no `Self`).
  - The v2 spec becomes complete at M12.
- **Kept:** deep `==` on every type and built-in `Eq`/`Ord`; no operator overloading.
- **v3:** interface values and dynamic dispatch, `Self`, generic interfaces, default methods, explicit type arguments, `.len()`.

**Why:** Interfaces are what make generic code useful beyond comparing. Keeping them as a separate milestone keeps M11's size reasonable.

**Rules out (v2):** `eq`/`lt` methods driving operators; `parse<i64>(s)`.

---

### 41. Generic checking (PRP-035)

**Decision:**
- **M11 split:** PRP-035 checks generics; PRP-036 compiles them (monomorphization, E0420, `Option<T>` as a generic enum, gate removed, spec folded).
- **Types:** `Type::Struct(key, args)`, `Type::Enum(key, args)` and `Type::Param(name)`. A parameter allows only what its bounds give: `==` with `Eq`, `<` with `Ord` (otherwise E0404 with "declare `T: Eq`"). Every concrete type is `Eq`; `Ord` is numbers and `string`.
- **Declarations (`check/generics.rs`):** parameters are in scope through `Checker.type_params` while signatures, fields, payloads and bodies are resolved. E0315 unknown bound, E0316 bad name (duplicate, like a type or module, or a method parameter named like its `impl`'s), E0418 wrong count (including a bare generic name and a wrong `impl` header). `Eq`/`Ord` are built-in names (E0302). An `impl`'s parameters take the declaration's bounds under the `impl`'s names.
- **Bounds (`check/bounds.rs`):** E0419 at the type or use whose argument misses a bound.
- **Inference (`check/infer.rs`):** binds the callee's parameters from the expected type, then the arguments that type alone, then deferring ones (literals, `None`, `[]`). Methods first bind their `impl`'s parameters from the receiver. Undecided is E0412 at the use, except when the context already failed. Fields and variants are substituted where they are read (`field_type`, `variants_of`).
- **Side table:** `Checked.instances: ExprId → type arguments` for every generic call, associated call, struct literal and construction, for PRP-036.
- **Gate:** moved from before checking to before codegen (`ir`, `build`, `run`).
- **Splits (approved):** `check/construct.rs` out of `enums.rs`, `check/bounds.rs` out of `generics.rs`; new `check/errors/generics.rs`.
- **Amendment:** `max(1, "a")` is E0401 at `1`: the string decides `T` before the literal, as the inference order says. The PRP's checklist had said E0403 at `"a"`.

**Why:** This checks generic code once, so PRP-036 only has to instantiate it. Recording instances keeps codegen from inferring anything (rule 4).

**Rules out:** explicit type arguments, bounds on `impl`, interfaces (milestone 12).

---

### 42. Compiling generics (PRP-036)

**Decision:**
- **Instance list in the checker** (`check/instances.rs`): each generic call is recorded with the body it is in. From the non-generic bodies, a worklist substitutes each instance's arguments into its body's calls and gives `Checked.monos` in discovery order. E0420 is reported past depth 64.
- **Concrete types in the checker** (`check/layouts.rs`):
  - each generic body's parameter-mentioning types are recorded, so each instance's types are those, substituted;
  - `Checked.used` lists every concrete type for codegen to declare;
  - `Checked.structs`/`enums` drop generic declarations and add each instance, keyed by the type as it prints (`Stack<i64>`), with fields and payloads substituted;
  - a type that keeps growing (`S<T>` holding `S<T[]>`) is E0420 at its declaration.
- **Codegen** (`codegen/instances.rs`): `Lowerer.subst` maps the current instance's parameters to types. `ty()` and `annotation_type()` apply it, so nothing is inferred. Generic calls find their instance through `Checked.instances`. Every table lookup and helper mangle uses the printed type (`10Stack<i64>`).
- **Symbols:** arguments as they print, the whole name through `check::symbol` (`lugha_fn_a/b/max<a/b/Point>`). A method's go on its type, then its own (`lugha_fn_Stack<string>.tag<f64>`).
- **`Option`:** described in the spec as a predeclared generic enum; the compiler keeps `Type::Option` (a discovery answer).
- **Gate:** deleted (`driver/gate/`). A generic `main` is E0304.

**Why:** Listing instances and concrete types in the checker makes E0420 a user error that `lughac check` finds. Keying tables by the printed type let codegen lay out instances without learning about type parameters.

**Rules out:** interfaces (milestone 12), explicit type arguments.

---

### 43. Interfaces (PRP-037)

**Decision:**
- **Syntax:**
  - `interface` is a keyword;
  - an `Item::Interface` holds items with an optional receiver, parameters and a return type, and no body;
  - a bound is `{ module, name, span }`, so `T: m.Shape` works.
- **Declarations** (`check/interfaces.rs`):
  - interfaces are collected first in pass 1, so struct, enum and function bounds can name them;
  - they share the item namespace (E0302) and follow `private`/E0313/E0318;
  - item signatures resolve in pass 2, with E0310 for a repeated item.
  - `Bounds.interfaces` holds the interface keys.
- **Bound errors:**
  - E0315 for a non-interface bound, with a hint when it names a type;
  - E0423 at the bound list when two interfaces declare one name differently;
  - E0305 with a "use it as a bound" hint for an interface in a type position.
- **Use** (`check/satisfy.rs`):
  - `x.m(…)` and `T.f(…)` on a type parameter check against the bound's signature;
  - E0421 when no bound declares the name;
  - E0415 for calling a method as associated, or the reverse.
- **Satisfaction** (structural; E0422 lists what is missing or mismatched): every item needs a visible method with the same name, receiver kind and types.
  - A generic type's `impl` parameters are substituted; a method with its own type parameters never matches.
  - A type parameter satisfies an interface through its own bounds.
  - Built-in types have no methods.
- **Instances:** a call through a bound is recorded with its parameter. The instance pass resolves it to the concrete method, and adds an instance when that method is generic.
- **Codegen:** a receiver whose type has arguments finds the instance `key<type args>`. `T.make(…)` passes no receiver.
- **Split (approved):** the checker's test helpers moved verbatim to `check/test_util.rs`.
- **v2 complete:** the spec is no longer "in progress"; tags `m12` and `v2`.

**Why:** These follow the session 46 discovery answers: one PRP, and E0423 at the bound. Monomorphization made calls through bounds static calls, so codegen barely changed.

**Rules out (v2):** interface values, `Self`, generic interfaces, default methods, operator overloading.

---

### 44. Lugha v3 direction (session 47)

**Decision:** `docs/specs/Language v3 Draft.md` is approved. It is a delta against the complete v2 spec, built as milestones 13–16.
- **Headline:** closures.
- **Function types:** `fun(A, B): R`.
- **Functions as values:** named and associated functions are values; a generic one needs an expected type; methods with `self`, externs and intrinsics are not.
- **Closure literals:** `fun(x) = e` and `fun(x: T): R { … }`, with parameter types inferred from an expected function type.
- **Capture:** by copy and read-only (E0511 on assignment), so value semantics hold and there are no shared references.
- **Equality:** functions have no `==`, so `T: Eq` now restricts.
- **Practical additions:** `args()`, `parse_i64`/`parse_f64` returning `Option`, and interpolation `"{x}"` in every string literal, with `{{`/`}}` for braces (E0111).
- **Built-in array methods:** `map`, `filter`, `fold`, `sort_by` (stable), `any`, `all`. Each takes `self` and returns a new value.
- **Value layout:** `{ code, env }`, with the environment on the GC heap. Symbols are `lugha_closure_<fn>.<n>` and `lugha_ref_<fn>`.
- **New codes:** E0111, E0209, E0511.
- **Milestones:**
  - M13: input and interpolation;
  - M14: functions as values;
  - M15: closures;
  - M16: array helpers. When M16 is done, the v3 spec is complete.

**Why:** Closures were the largest expressiveness gap: generic code could only compare with `<`, never take a custom order or callback. Copy capture keeps v2's "everything is a value" without a reference type. The small additions make real programs (input, readable output) practical.

**Rules out (v3):** packages, interface values, shared references, closures that change their captures, bitwise operators and more integer types, or-patterns, `mut` match bindings, explicit type arguments.

---

### 45. Input and interpolation (PRP-038)

**Decision:**
- **Lexer** (`lexer/interp.rs`):
  - a string with `{…}` lexes as `StrStart`, the expression's tokens, `StrMid`…, `StrEnd`, with the part texts already decoded;
  - `Lexer.interp` tracks the open interpolation and its brace depth;
  - E0111 covers a lone `}`, an unclosed `{`, an empty `{}`, and a string literal inside `{…}`. A `"` inside an interpolation is taken as a nested literal if the line has another `"`, else as an unclosed `{`.
- **AST:** `ExprKind::Interp(Vec<Part>)`, where `Part` is `Text` or `Expr`, plus `Part::expr`.
- **Checker** (`check/input.rs`):
  - interpolated values must be numbers, `bool` or `string` (E0403 "can't interpolate");
  - the input intrinsics are `INPUT`, and `env::is_intrinsic` covers both lists.
- **Liveness:** interpolated values are mentions. `check/liveness/` was split (approved) into `mod.rs` (the rule) and `walk.rs` (the walk).
- **Codegen** (`codegen/input.rs`):
  - interpolation is a chain of `lugha_rt_str_concat` over `lugha_rt_to_string_*`;
  - `parse_*` writes into an alloca, and the flag selects `Some`/`None`.
- **Runtime** (`runtime/input.c`, the fourth fragment): `lugha_rt_init(argc, argv)`, `lugha_rt_args`, `lugha_rt_parse_i64`/`_f64`. The C `main` is `main(argc, argv)`.
- **Driver:** `lughac run file [opts] [-- args…]`.
- **Tests:** the runner reads `<name>.args` (`runner/args.rs`), which is malformed beside a reject case. The spec extractor reads "Run as `prog a b`.".
- **Spec:**
  - `Language v3 Specification.md` starts (in progress: milestone 13) and is `lughac spec`;
  - v2 is frozen;
  - the draft's §10 sentence became one run.

**Why:** These follow the session 48 discovery answers: start the v3 spec now, and add `.args` support. Interpolation reuses `to_string`, so it prints exactly like `print`.

**Rules out:** format specifiers, stdin or file input, and string literals inside interpolations.

---

## CURRENT PROJECT STATE

### Fully Working
- **Milestone 13 complete**: PRP-038 args, parsing, interpolation; the v3 spec begins
- **Milestone 12 complete — Lugha v2 complete**: PRP-037 interfaces (E0421, E0422, E0423)
- **Milestone 11 complete**: PRP-036 compiles generics (instances, E0420, gate removed, spec folded)
- PRP-035: generics are type-checked (`lughac check`); the gate stops them before codegen
- **Milestone 10 complete**: PRP-034 names across modules, visibility (E0313/E0317/E0318), module symbols
- PRP-033: modules front end — loading, E0312/E0314/E0319, file-aware diagnostics and `--emit`, `private`/`public` syntax
- **Milestone 9 complete**: PRP-032 nested patterns, guards, usefulness-based E0417/W0102, W0103
- PRP-031: deep `==` on every type, string order; `Language v2 Specification.md` (in progress) printed by `lughac spec`
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
- v3 milestones 13–16 (draft approved, session 47, MEMORY 44)

---

## NEXT SESSION START POINT

**Milestone 13 is complete** (tag `m13`; MEMORY 45). `lughac spec` prints `Language v3 Specification.md` (in progress: milestone 13).

Next is **milestone 14, functions as values** (`Language v3 Draft.md`: decision 3, §3 `fun(…)` types, §4 "Function types" and "Functions as values", §7 value layout, §9 `lugha_ref_*`, the §10 functions-as-values program). It covers:
- `fun(A, B): R` types;
- named and associated functions as values;
- calling any expression of function type;
- no `==` on functions (so `T: Eq` restricts);
- E0406/E0409/E0412 extensions;
- the `{ code, env }` layout with a null environment.

Closures (milestone 15) come after. Start with discovery for PRP-039.

See `TODO.md`.
