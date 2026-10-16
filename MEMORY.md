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

### 46. Functions as values (PRP-039)

**Decision:**
- **Syntax:** `TypeKind::Fun(params, Option<ret>)`. Types may be parenthesized. A return type takes any `[]`, so an array of functions is `(fun(i64): i64)[]` (a discovery answer).
- **Checker:**
  - `Type::Fun(params, Box<ret>)`, with `Void` for no result. It prints with parentheses inside arrays.
  - **Values** (`check/fun_values.rs`):
    - `function_value` handles a function name used without a call;
    - `function_field` handles `m.f`, `T.f` and `m.T.f`;
    - the key goes in `Checked.callees`, and a generic one's arguments come from the expected type through `infer` (recorded with `used` for the instance pass).
  - **Not values** (E0406 with help): methods with `self`, externs (`Signature.is_extern`) and intrinsics.
  - **Calls:**
    - `call_expr` and `call_value` handle any callee of function type;
    - `fun_field` handles `s.f(x)` on a field of function type when there's no method `f` (a discovery answer; E0310 keeps the names apart).
  - **Equality:** `holds_fun` (in `bounds.rs`) makes `eq_ok` false for anything holding a function.
  - **Externs:** E0409 for function types in extern signatures.
- **Codegen** (`codegen/fun_values.rs`):
  - a value is an anonymous `{ ptr, ptr }`, 16 bytes with alignment 8;
  - a named function becomes `{ lugha_ref_<symbol>, null }`, with internal wrappers emitted on first use and named from the target's LLVM name;
  - a value is called indirectly, with the environment first;
  - `arguments()` now takes `(params, is_extern)`.

**Why:** These follow the session 49 discovery answers. Wrapping every function the same way gives closures (milestone 15) one calling convention: their code takes the environment first too.

**Rules out (M14):** closure literals, method values, externs as values, `==` on functions.

---

### 47. Closures (PRP-040)

**Decision:**
- **Syntax** (`parser/closure.rs`):
  - `ExprKind::Closure(Box<Closure { params: (Ident, Option<Type>), ret, body }>)`, where `= e` becomes a block with a tail;
  - E0209 for `self`/`mut` parameters.
- **Checker** (`check/closures.rs`):
  - **Typing:** parameter types are written, or come from an expected `Fun` with the same count (E0412 otherwise). The return type is written, or expected, or the body's (a `Never` body counts as `Void`). E0412 covers `return` when the return type is still unknown.
  - **Own function:** the body saves and restores `ret`, `loops`, `dead`, `changes` and `iterating`, so `return` leaves the closure and outer loops are out of reach (a discovery answer).
  - **Captures:** a `closures: Vec<Frame { base, captures, infer_ret }>` stack records them. `note_capture` (in the `Name` arm) adds a name to every frame whose `base` is above the local's scope depth, and records the mention as `Binding::Param`, so a capture never moves. The result is `Checked.captures: ExprId → names` in order.
  - **E0511:** `check_place_for` reports it when the root is a capture of the innermost closure.
  - **Inference:** `defers()` is true for a closure with an unwritten parameter, and `infer` checks closures after every other waiting argument.
- **Codegen** (`codegen/closures.rs`):
  - captures are copied (deep for arrays) into one `lugha_rt_alloc` environment laid out as an LLVM struct, or a null environment for none;
  - the body becomes an internal `lugha_closure_<enclosing>.<n>` (`Lowerer.closure_counts`), lowered with the builder, function, return type, scopes and loops saved and restored;
  - captures are read in place from the environment as borrowed locals.

**Why:** These follow the session 50 discovery answer and the approved design choices (always copy, E0412 for an inferred return with `return`, closures last in inference). Function values already took the environment first, so closures needed only literals and environments.

**Rules out (M15):** changing captures, recursive closures, closures with type parameters, moving captures.

---

### 48. Array helpers in a Lugha prelude (PRP-041)

**Decision:**
- **Prelude** (`src/prelude.la`, `lugha::PRELUDE`): the six helpers are generic Lugha functions (a discovery answer), in module path `$prelude` (`modules::PRELUDE_PATH`), which no program can name. `sort_by` is a bottom-up merge sort between two buffers.
- **Loading:**
  - `modules::with_prelude` appends it after the user's modules, in the same offset and id spaces;
  - the driver adds it only in `front()` (the checking path), so `--emit=tokens|ast` never sees it;
  - `Source.add` names it `<prelude>`;
  - any diagnostic in it is an internal error (exit 2);
  - checker tests load it too (`check/test_util.rs`).
- **Checker** (`check/helpers.rs`):
  - `helper_call` in `on_value`, before the array built-ins, turns `xs.m(args)` into a call of `$prelude.m`;
  - `infer::bind` binds the element type from the already-checked receiver, then the arguments are inferred;
  - the key and arguments are recorded in `callees`/`instances`/`used`.
- **Partly-known closure types:** `infer` hints a closure argument with the expected type, where still-unbound callee parameters become `Error`. A closure reads an `Error` parameter as unknown (E0412) and an `Error` return as "from the body". That is how `xs.map(fun(x) = "{x}")` decides `U`.
- **Codegen:** an array method with a prelude key calls the instance with the receiver first. IR is unchanged when no helper is used.

**Why:** The helpers are ordinary generic code, so there's no special codegen; the merge sort is readable Lugha rather than builder calls.

**Rules out:** other built-in methods (`len()`, `reverse`, `find`), and prelude names visible to programs.

---

### 49. Lugha v4 direction (session 52)

**Decision:** `docs/specs/Language v4 Draft.md` is approved: input and output, as milestones 17–19.
- **Errors:** a predeclared `Result<T, E>` (`Ok`/`Err` without a prefix, like `Some`). I/O returns `Result<T, string>` with the system's message.
- **`?`:** a postfix operator on `Result` (the same `E`, no conversions) and on `Option`. It returns early from the enclosing function or closure, and misuse is E0424.
- **Helpers** (prelude methods):
  - for strings: `lines` (drops a trailing `\r`), `split`, `trim`, `contains`, `starts_with`, `ends_with`, `slice` (bytes), and `join` on `string[]`;
  - for `Option` and `Result`: `unwrap_or`, `ok_or`, `ok`, `map`, `map_err`, `is_*`;
  - no `unwrap()`;
  - the prelude may call compiler-internal intrinsics (`lugha_rt_str_slice`).
- **I/O:**
  - `read_line(): Option<string>`;
  - `read_file`, `write_file` and `append_file`, returning `Result<…, string>`, with the bytes written for writes (Lugha has no unit type);
  - `eprint`/`eprintln`, which flush stdout first;
  - `exit(i32)`;
  - text only.
- **Tests:** `<name>.stdin` files, a scratch directory for file writes, and "Run with … on stdin" in §10.
- **Milestones:** M17 `Result` and `?`; M18 helpers; M19 I/O, after which the v4 spec is complete.

**Why:** Programs could compute but not read data. Errors as values, made short by `?`, fit Lugha's exhaustive `match`.

**Rules out (v4):** interface values, packages, bitwise operators and more integer types, `match` additions, binary or directory I/O, `unwrap()`, error conversions in `?`.

---

### 50. Results and `?` (PRP-042)

**Decision:**
- **`Result`:** `enum Result<T, E> { Ok(T), Err(E) }` lives in `src/prelude.la`, keyed `$prelude.Result` (`check::results::RESULT`). It is an ordinary generic enum (a discovery answer).
- **Checker** (`check/results.rs`):
  - `prelude_type` lets `type_key` resolve `Result` when no module declares it;
  - `result_construction` handles bare `Ok`/`Err` (before `Some` in `construction`) through `construct`;
  - `BUILTIN_NAMES` gains `Result`/`Ok`/`Err`, with `in_prelude` exempt, and `is_type_name` covers `Result`;
  - `Type`'s `Display` strips `$prelude.`, so LLVM and table names are `Result<i64, string>`.
- **`?`:**
  - lexed as `Question`, parsed as postfix `ExprKind::Try`;
  - `try_expr` checks it against `self.ret`, so a closure's `?` returns from the closure;
  - E0412 if a closure's return type isn't known yet;
  - E0424 (`errors/v4.rs`), with help for: not a `Result`/`Option`; `main` (owner `main`, no closure); an `Option` in a `Result` function; different `E`; any other return type.
- **Codegen** (`codegen/results.rs`): the operand goes in a slot and its tag is compared. The failing branch builds the function's own `Err(payload)`/`None` and returns. The passing branch loads the payload. `constructed_variant` accepts `Ok`/`Err`.
- **Spec:** `Language v4 Specification.md` starts (in progress: milestone 17) and is `lughac spec`; v3 is frozen.

**Why:** It follows the session 53 discovery answer. A prelude enum reused patterns, exhaustiveness, `==`, layout and copies, so the compiler gained only names, construction, display and `?`.

**Known draft bug:** the draft's §10 reading-input program writes `Some(n) => total += n,`, which isn't valid. It is fixed in M19's PRP (TODO).

---

### 51. Text and option helpers (PRP-043)

**Decision:**
- **Dispatch:** `check/helpers.rs::helper_function` maps a receiver and method to a prelude function:
  - arrays keep their names;
  - `string[]` `join` goes to `str_join`;
  - `string` goes to `str_*`, `Option` to `option_*`, and `Result` to `result_*`.

  `map` is chosen by the receiver. The draft's set only (a discovery answer).
- **`slice`** is a built-in string method (`builtins.rs`, receiver `Some(false)`, so no place rules apply). It lowers to `lugha_rt_str_slice(s, from, to, file, line, col)`, which panics at the method name. The prelude's string helpers use it. (The draft's "internal intrinsic" was amended.)
- **Reached prelude functions only:**
  - `used()` records prelude calls even without type arguments, and prelude bodies aren't instance roots;
  - so non-generic prelude functions compile only when user code reaches them, appearing in `monos` with no arguments;
  - `instance_name` with no arguments is the bare key, `prelude_target` finds them, and `declare_functions` and `define` skip prelude functions.
- **Codegen dispatch:** a call whose callee key starts with `$prelude.` calls the helper with the receiver lowered through `arguments` (aggregates by pointer).
- **Panics:** `split("")` panics inside the prelude, so its location is `<prelude>:L:C`.
- **Split (approved):** `RUNTIME_SYMBOLS` moved verbatim from `codegen/runtime.rs` to `link.rs`, its only user.

**Why:** These follow the session 54 discovery answer and the approved design. Making `slice` built in kept its panic at the caller. Compiling only reached prelude code kept every helper-free program's IR unchanged.

---

### 52. Input and output (PRP-044)

**Decision:**
- **Checker:**
  - the seven I/O intrinsics join `input.rs`'s `INPUT` (so `is_intrinsic` covers them);
  - `eprint`/`eprintln` reuse `call.rs`'s `print`/`println` rules;
  - `exit(i32)` is `Never`;
  - `read_file` and `write_file`/`append_file` return `Result<string|i64, string>`.
- **Codegen** (`codegen/io.rs`):
  - runtime functions return a flag and write through cleared out-slots, and `select` picks between the `Ok`/`Err` (or `Some`/`None`) values;
  - `eprint` formats with `text_of`;
  - `exit` calls `lugha_rt_exit`, then `unreachable` and a dead block.
- **Runtime** (`runtime/input.c`):
  - `read_line` uses `getline`, flushes stdout, strips `\n` then `\r`, and treats errors as end of input;
  - files are opened in binary mode, with `strerror` messages, and a path with a NUL byte is refused;
  - `eprint`/`exit` flush stdout first;
  - `errno.h` joins the core's includes.
- **Runner:**
  - `args.rs::Extras { args, stdin, scratch }` from `<name>.args`/`.stdin`/`.scratch` (malformed beside a reject case);
  - `execute.rs` pipes stdin from a thread, and `case_command` runs a scratch case in a `Fixture` temp dir with the absolute program path (a discovery answer).
- **Extractor:** "Run with `a`, `b` … on … stdin." becomes stdin lines.
- **The draft's §10 program** is corrected (`{ total += n; }`, `{ break; }`).

**Why:** These follow the session 55 discovery answer. I/O failures as values fit `Result` and `?`, and only scratch cases change the runner's working directory.

### 53. Nested strings in interpolations — Lugha v4.1 (PRP-045)

**Decision:**
- **Lexer:** `Lexer.interp` is a stack (`Vec<Interp>`), innermost last. Inside an interpolation, `"` lexes an ordinary string (`self.string`), whose own `{` pushes; `{`/`}` count the innermost's braces, and its closing `}` pops and resumes the enclosing string's text.
- **Line breaks:** strings stay single-line. A string that runs past its line while an interpolation is open (`string_unclosed`) is E0111 "`{` in a string is never closed" at the innermost open `{`, and clears the stack; otherwise E0102. A line break or EOF in interpolation code pops the top first, so `"{x` + newline is E0102 as before.
- **Removed:** the `quote_in_interpolation` heuristic and its "string literal can't appear inside `{…}`" E0111.
- **No parser, checker or codegen change:** a nested string is just tokens among the interpolation's.
- **Spec:** v4 spec §2 "String interpolation", the E0111 row and the status line (v4.1); tag `v4.1`.

**Why:** E0111 tripped real programs (sessions 48, 54, 55); the session 56 discovery chose a point release over starting v5.

### 54. Lugha v5 direction (session 57)

**Decision:** `docs/specs/Language v5 Draft.md` is approved: interface values, as milestones 20–22. Interfaces only; bits, packages and `match` additions wait.
- **Types:** an interface whose items are all methods is a type (`Shape`, `Shape[]`, `Option<Shape>`); one with an associated function used as a type is E0425.
- **Conversion:** implicit only where the expected type is exactly the interface — the one implicit conversion (spec §4 names it). Not inside other types (`Circle[]` isn't `Shape[]`), not interface to interface; no expected type, no conversion.
- **Values:** an interface value holds its own deep copy; `mut self` calls change that copy. No `==` (E0404/E0419) and no printing. `Shape` satisfies the bound `Shape`.
- **Type patterns:** `c: Circle =>` / `_: Square =>`, nested and guarded; `_` is required (E0417); misuse is E0426.
- **Default methods:** interface methods with bodies, checked once with `self` as a bounded type parameter; callable through values and bounds, not on concrete types (E0415).
- **`?`:** converts `E` into an interface `F` that `E` satisfies.
- **Layout:** two words (dispatch table, GC box); primitives boxed too. Symbols `lugha_vt_<Type>.<Interface>`, `lugha_dyn_<Type>.<method>`; tables only for converted pairs.

**Why:** the session 57 discovery: heterogeneous collections are the largest expressive gap, and interface errors resolve v4's `?` conversion question.

---

### 55. Checking interface values (PRP-046)

**Decision:**
- **Milestone 20 is two PRPs** (session 58 discovery): 046 checks, 047 compiles. Between them a driver gate (`pipeline.rs::stop_gated`) stops `build`, `run` and `--emit=ir` with "not implemented yet: interface values (milestone 20)" (exit 2) when any recorded annotation `mentions_interface()`.
- **`Type::Interface(key)`;** `InterfaceInfo.not_value` (the first associated function) decides E0425 at collection. `interface_type` resolves the name before `type_key` in `resolve_named`/`resolve_applied` (type arguments are E0418).
- **Conversion in one place:** `Checker::expr` ends with `convert(expr, ty, expect)`. Where the expected type is `Interface(I)`, a satisfying type is recorded in `Checked.conversions` (ExprId → `Interface(I)`), the expression keeps its own type in `types`, and `I` is returned; otherwise E0422 via `satisfies`. Branches and tails convert by themselves. Fully bound call arguments already go through `expect_type`, so `infer.rs` needed no change.
- **Calls:** `on_value` sends an `Interface` receiver to `through_interface` (`iface_calls.rs`), which records the callee `<key>.<method>`.
- **Satisfaction:** an `Interface(I)` offers `I`'s declared items. **Equality:** `eq_ok` and `holds_fun` treat interface values like functions. **Copies:** `contains_array` is renamed `is_deep` and is true for interface values.
- **Errors:** `errors/v5.rs` (E0425, E0403 interface-to-interface, and wrappers adding interface help to E0410, E0404 and E0409, leaving `errors/types.rs` at its size).
- **Split (approved):** `arguments`/`unchecked` moved verbatim from `method_calls.rs` to `check/arguments.rs`.

**Why:** a gate keeps every merged state honest while codegen catches up, as milestone 11 did (MEMORY 34).

---

### 56. Compiling interface values (PRP-047)

**Decision:**
- **Layout:** `{ ptr table, ptr box }` like a function value (`llvm_type`, `layout` 16/8), passed by value. The box is `lugha_rt_alloc`ed, primitives included.
- **Tables:** `lugha_vt_<Type>.<Interface>` (internal constant struct of pointers), emitted once per converted pair: slot 0 `lugha_type_<Type>` (an i8 constant whose address is the identity, for milestone 21), slot 1 `lugha_copy_<mangled>_box`, then the methods themselves in declaration order — **no `lugha_dyn_` wrappers** (draft amended): methods take their receiver as a pointer, so a call passes the box. Slots are reached with `build_struct_gep` on a view of `index + 1` pointers (no `unsafe` array GEP).
- **Conversion:** `Lowerer::expr` checks `conversions`; `converted` lowers the value with the conversion entry lifted (so `ty(expr)` is the concrete type inside and the interface outside), deep-copies it if `aliasing` and not moved, boxes it and pairs it with its table. In an instance where the type is already the interface it passes through.
- **Copies:** `lugha_copy_<mangled interface>` calls the table's copy slot; `mangle` length-prefixes interfaces (`length_prefixed`). `value_for_store` skips converted expressions (`aliased`); `value_for_return` doesn't (an extra box copy at worst, kept for `copy.rs`'s size).
- **Checker:** `Checked.interfaces` (methods in order); `convert` records `used`/`bound_used` for each interface method so generic types' methods are instantiated (`conversion_uses`); a numeric literal facing an interface takes its default type and converts (amendment, approved).
- **Spec:** `Language v5 Specification.md` begins (in progress: milestone 20), v4 frozen; §11's stale v1 out-of-scope list replaced by v5's.

- **Files:** `codegen/interfaces.rs` (building) and `codegen/dispatch.rs` (calls, copies); `check/iface_tables.rs` (split from `iface_values.rs`, approved).

**Why:** reusing the function-value pair, the receiver-pointer convention and the copy-helper scheme keeps interface values within the existing ABI; the gate (MEMORY 55) is gone.

---

### 57. Type patterns (PRP-048)

**Decision:** built in session 60 while the user was away (pre-approved; merge held for review).
- **Syntax:** `PatternKind::Type(Option<Ident>, Annotation)`; after `_` or a name, `:` starts a type. In a `match` scrutinee the parser's misplaced-struct-literal heuristic (`Name { field:` → E0205) is off (`Parser.scrutinee`), since `{ c:` begins an arm.
- **Checker** (`check/type_patterns.rs`): only on interface values; `T` concrete (not an interface, not a type parameter — decision taken while away), generic `T` written with arguments (bare `Stack` is E0426, as the draft says, not E0418), and satisfying the interface (`satisfy.rs::unmet`, the non-reporting half of `satisfies`). `Ctor::Type(T as printed)` with no payloads: the usefulness algorithm needs no change (no finite signature, so `_` is required, witness `_`; repeats are W0102).
- **Codegen** (`codegen/dispatch.rs::type_test`): load table slot 0, `icmp eq ptr` with `lugha_type_<T>`, then bind the box's value as a borrowed local; `is_flat` is false for type-pattern arms, so they use the arm chain.

**Why:** the identity slot reserved in PRP-047 makes the test one pointer comparison; reusing `Ctor`/usefulness keeps exhaustiveness uniform.

---

### 58. Default methods and `?` conversion — v5 complete (PRP-049)

**Decision:** built in session 61 while the user was away (pre-approved; merge held; stacked on PRP-048).
- **Syntax:** `InterfaceItem.default: Option<FunDecl>`; `fun_body` (decl.rs) shared by functions and defaults; an associated function with a body is E0201.
- **Checker** (`check/defaults.rs`): `Requirement.default` = the interface key; defaults are registered in `methods[<interface key>]` as generic methods with one hidden parameter `SELF` (`"Self"`, bounded by the interface, `outer` 1, key `<I>.<m>`), and their bodies are checked from `method_bodies`. `resolve_named` ignores a written `Self` (E0305). `unmet` forgives a missing defaulted item. A bound call of a defaulted item records the callee `<I>.<m>`; `bound_target` → `default_target` reaches `<I>.<m><T>` for a `T` without its own method; `conversion_uses` does the same for tables. E0415's help names the interface (`default_help`).
- **`?`:** `results.rs` accepts `Result<_, E>` → `Result<_, F>` when `F` is an interface a concrete `E` satisfies (`iface_tables.rs::try_converts`), recorded in `Checked.try_conversions` (keyed by the operand); codegen boxes the error with `box_as` on the failure branch.
- **Codegen:** `instances.rs` finds default bodies in interfaces (`Source.interface`; subst `Self`; receiver = the type argument; symbol `lugha_fn_<I>.<m><T>`); `interfaces.rs::method_of` and `methods.rs` (any receiver type, via `default_method`) fall back to the default's instance; new `codegen/defaults.rs` (`default_method`, `box_as`).
- **Spec:** `Language v5 Specification.md` complete; §12 gains the boxing and concrete-default questions.

**Why:** treating a default as a generic method of the interface reuses checking-once, monomorphization and instance symbols unchanged.

---

---

### 59. Lugha v6 direction (session 63)

**Decision:** `docs/specs/Language v6 Draft.md` is approved: generic interfaces and `Self`, as milestones 23–26. Bits, packages and `match` additions wait.
- **Generic interfaces:** `interface Source<T>`, bounds with arguments (`S: Source<T>`), bounded parameters checked at use (E0419), a bare `Source` is E0418. A parameter appearing only in a bound's arguments is inferred from the concrete type's methods (E0412 when nothing fixes it). They are value types too (`Source<i64>`, `lugha_vt_Count.Source<i64>`).
- **Iterators:** the prelude's `interface Iter<T> { fun next(mut self): Option<T>; }`; `for x of e` on a non-array satisfying it loops over a hidden copy of `e` (moved on last use), so `e` never changes. A module's own `Iter` hides the prelude's.
- **`Self`:** inside interfaces (signatures and default bodies) only; outside, E0305. An interface mentioning `Self` beyond the receiver is a bound only: used as a type it is E0427.
- **Interface to interface:** `J` converts to `I` when `J` declares every item of `I` (defaults included) the same way; else E0422. The box keeps its concrete type (type patterns still see it). `?` converts the same way. Codegen: each `J` table gains a slot per target interface pointing at the derived table.

**Why:** the session 63 discovery: finish the interface story begun in v5; iterators give generic interfaces an everyday use.

---

### 60. Generic interfaces as bounds (PRP-050)

**Decision:** milestone 23 is two PRPs: bounds (050), then values (051).
- **Syntax:** `InterfaceDecl.type_params`, `Bound.args` (parsed like type arguments).
- **Checker:**
  - `check::applied::Applied { key, args }` replaces the key in `Bounds.interfaces`; `Checker::items(&Applied)` substitutes the interface's parameters; every reader of bound items goes through it (`bound_item`/`bound_entry`, `unmet`/`satisfies`, E0423, defaults).
  - `InterfaceInfo.generics`: names in pass 1, bounds in the new pass `interface_params` (after `collect_enums`); items resolve with them in scope. `Self` as an interface parameter is E0316.
  - Bound arguments resolve quietly in `bounds_of` (`quietly` truncates diagnostics) and are reported once by `check_bounds` → `bound_args` (E0418, E0419). `generics_with` puts the declaration's own and outer parameters in scope; structs and enums recompute their generics in pass 2, once every type is known. `renamed` renames bound arguments for `impl` parameter names.
  - Inference: `bound_infer.rs::through_bounds`, called in `infer` before E0412, unifies each bound item with what the bound parameter's type offers, repeating until nothing new is bound.
  - Defaults of a generic interface: generics `[Self: I<P…>, P…]`, `outer` 1 + n; instances `[concrete, X…]`; bound calls record the interface's arguments in `Checked.default_args`.
  - A generic interface in a type position is E0418 ("can't be a value type yet", `errors/v6.rs`) until PRP-051.
- **Codegen:** `instances.rs` `Source.interface` carries the declaration (subst `Self` then the interface's parameters; symbol `lugha_fn_Source<i64>.skip<Count>`); `default_method` substitutes `default_args`.
- **Spec:** `Language v6 Specification.md` begins (in progress: milestone 23); v5 frozen.

**Why:** a bound with arguments is all generic code needs; calls through bounds were already monomorphized, so values (tables) can follow separately.

---

### 61. Generic interface values — milestone 23 complete (PRP-051)

**Decision:** `Type::Interface(key, args)`; generic interface values follow every v5 rule at their arguments.
- **Checker:** `Applied::of`/`ty` bridge types and applied interfaces; `no_type_args` resolves `Source<i64>` (E0418 count, E0419 bounds); conversion via `satisfies(&ty, &applied)`, same-interface values only when `fits` (different arguments are E0403); `through_interface`, `offer`, type patterns, `?` (`try_converts`) and `conversion_uses` (default instances `[concrete, X…]`) take the `Applied`. Substitution, `has_param`, `unify`, `unbound`, `depth` and `fits` treat interface arguments like a struct's.
- **Inference into `Source<T>` parameters** (discovery): `bound_infer.rs::interface_input`, hooked in `infer::input`, reads unbound arguments off a concrete argument's methods and converts it.
- **`Checked.interfaces`** is `HashMap<String, InterfaceShape { params, methods }>`; codegen substitutes with `methods_at(args)` for dynamic calls; tables `lugha_vt_<T>.<Interface<args>>`, default slots `<I>.<m>` at `[ty, args…]`, copies mangled with arguments (`lugha_copy_11Source<i64>`).
- **Removed:** the temporary E0418 (`errors/v6.rs`).
- **Split (approved):** `applied_bound`, `bound_args` and two tests moved verbatim from `applied.rs` to `bound_args.rs`.

**Why:** carrying arguments on the type, like structs, reuses substitution and monomorphization; `InterfaceShape` keeps signatures out of codegen's hands (architecture rule 4).

---

### 62. Iterators — milestone 24 (PRP-052)

**Decision:** iterator loops are recognised by shape, not by name.
- **Prelude:** `interface Iter<T> { fun next(mut self): Option<T>; }`, appended at the end of `prelude.la` (so prelude panic locations don't move). `Iter` resolves to `$prelude.Iter` in types and bounds unless the module declares something named `Iter` (`or_prelude_interface`, used by `interface_key` and `interface_type`).
- **Checker** (`check/iterators.rs`): `for_of` hands a non-array to `iterator_loop`; `next_of` uses `offered(ty, "next")` and requires `mut self`, no parameters, an `Option<X>` result (else E0411 with the signature in the help). The loop variable is a `LoopVar`; no E0507 guard (the loop works on a copy). Reachability: a generic type's `next` (`used`), a type parameter's through its bound (`bound_used`, defaults included). `Checked.iterations[iter id] = Iteration { element, default }`, where `default` is the bound interface whose default `next` may apply.
- **Codegen** (`codegen/iterators.rs`): `value_for_store` into a hidden slot, `next` called with the slot's address (own method, generic instance, or the bound default's instance) or through the table for an interface value (`dispatch.rs::parts`/`slot` made `pub(super)`); the payload of `Some` is an owned local.
- **Struct literals in `for` headers need parentheses** (existing E0205 rule): `for n of (Count { … })`.

**Why:** structural recognition means a module's own `Iter` never breaks loops, and every iterable shape reuses existing method lookup.

---

### 63. `Self` in interfaces — milestone 25 (PRP-053)

**Decision:** `Self` is the hidden type parameter defaults already had (`check::SELF`), now writable inside interfaces.
- **Resolution:** `resolve_interfaces` pushes `self_type::self_param()` into `type_params` while items resolve; default bodies already have it; `resolve_named` no longer special-cases `SELF`, so elsewhere `Self` is E0305. An interface parameter named `Self` is E0316 (`no_self_param`).
- **Substitution:** `self_type::items_for(applied, self_ty)` = `items` with `Self` replaced; used by `unmet` (the argument), `bound_entry` (the type parameter), `offer` for interface values (the interface type) and `read_off` (the bound parameter's type).
- **E0427** (`errors/v6.rs`): `InterfaceInfo.uses_self`, read from the syntax in pass 1 (`written_self_use`) because struct fields resolve before interface items; reported by `interface_type` after E0425; such interfaces have no `Checked.interfaces` entry.
- **Codegen:** unchanged (bound calls and default instances are per concrete type).

**Why:** reusing the defaults' hidden parameter made `Self` a substitution, with no new machinery in codegen.

---

### 64. Converting between interfaces — v6 complete (PRP-054)

**Decision:** `J` converts to `I` (different keys) when `J` declares every item of `I`, defaults included (`check/upcast.rs::undeclared`, E0422 "`J` doesn't declare everything `I` needs"); same key, other arguments stays E0403. `?` accepts an interface error by the same rule (`try_converts`).
- **Codegen** (`codegen/upcast.rs`), amending draft §9 (approved): conversions call `lugha_up_<J>.<I>(table) -> table`, whose body (built in `finish_upcasts` after all lowering) compares with every `J` table and returns the matching `I` table. Tables are registered as `(concrete, root, interface)` (`interfaces.rs::table` adds direct ones); derived tables `lugha_vt_<C>.<root>.<I>` take `C`'s identity/copy and the root table's functions (`method_of` on the root), so methods behave as through `J`; derived-of-derived keep the root, so the fixpoint is finite and converting back finds the root table. `box_as` routes an interface source to `upcast` (covers generic `T = J` instances and `?`).
- **Why not slots:** tables are emitted lazily; growing a global would need replacing it, and deleting globals needs `unsafe` in inkwell.

**Why:** behaviour exactly as the draft; the lookup keeps codegen safe and adds nothing to programs that don't convert between interfaces.

---

### 65. Lugha v7 direction (session 69)

**Decision:** `docs/specs/Language v7 Draft.md` is approved: numbers and bits, as milestones 27–29. Packages, iterator helpers and `match` additions wait.
- **Types:** `u32`, `u64`, `f32` (literals by expected type, E0402; unsigned negation E0404; every numeric cast under v0 rules; `f32` prints shortest round-trip; extern-safe). Indexes and lengths stay `i64`.
- **Bit operators:** `& | ^ ~ << >>` on integers only, both operands one type (a shift amount has the shifted type); `>>` arithmetic on signed; a shift amount negative or ≥ width panics (`shift out of range`); compound `&= |= ^= <<= >>=`; binary literals `0b…`.
- **Precedence (Rust):** `* / %` > `+ -` > `<< >>` > `&` > `^` > `|` > comparisons > `&&` > `||`; prefix `~`.
- **Integer methods:** `wrapping_add/sub/mul` (same type), `count_ones`, `leading_zeros`, `trailing_zeros` (`i64`); others E0415.
- **No new error codes.**

**Why:** the session 69 discovery: after two interface releases, the largest gap is that hashes, checksums and flags can't be written.

---

### 66. More numbers — milestone 27 (PRP-055)

**Decision:** `u32`, `u64`, `f32` are lexer keywords (`TyU32`…), like the v0 types — at the user's direction, breaking only v6 programs that use them as names (v4's `Result` precedent; draft amended).
- **Checker:** `Type::U32/U64/F32`; `check/numbers.rs` (`is_float`, `is_unsigned`, `Type::primitive` for keyword types); `literal_range` covers them; a float literal takes `f32` when expected; unsigned negation E0404; mixing types stays E0404 (as `i32 + i64` always was).
- **Codegen:** float paths test `is_float()`; `is_signed` stays `i32`/`i64`, so unsigned ops come for free; casts add `fptrunc`/`fpext` and use the source/target float width; `f32` literals are `f64::from(value as f32)`; layout 4/8/4.
- **Runtime:** `lugha_rt_print_u64`, `lugha_rt_print_f32`, `lugha_rt_to_string_u64`, `lugha_rt_to_string_f32`; `format_float(x, single, …)` stops at the fewest digits that read back (`strtof` for `f32`); `u32` is zero-extended to the `u64` functions (`abi` / `abi_value`).
- **Approved splits:** `slot_names_variant`/`indirect_slot` → `check/slots.rs`; AST `Type`/`TypeKind`/`AppliedType` → `ast/types.rs` (verbatim).
- **Spec:** `Language v7 Specification.md` begins; v6 frozen.

**Why:** signedness and float width were already parameters in codegen, so the new types are mostly table entries.

---

### 67. Bits — milestone 28 (PRP-056)

**Decision:** built as part of the user's "do all of them in one go" (milestones 28–29 back to back, each its own PRP and merge).
- **Lexer:** `& &= | |= ^ ^= ~ << <<=`; `>>` and `>>=` are not tokens — the parser (`parser/operators.rs`) reads `>` touching `>` as `Shr`, `>` touching `>=` as `>>=` (and the expression loop stops there), so `Option<Option<i64>>` still closes. `0b` literals share the hex path (`radix`). The E0101 "did you mean `&&`" help is gone.
- **AST/levels:** `BinOp::{BitOr 5, BitXor 6, BitAnd 7, Shl/Shr 8}`, `+ -` 9, `* / %` 10, `as` 11 (`CAST_LEVEL`); `UnOp::BitNot`; `AssignOp` bit forms; `BinOp::is_bitwise`.
- **Checker:** bit ops are "arithmetic" (context type flows to operands) but need `is_integer`; `~` integer; compound bit forms need an integer (`assign.rs`). Mixed types E0404.
- **Codegen** (`codegen/bits.rs`): `and/or/xor/shl`, `lshr`/`ashr` by signedness; shifts first `icmp uge n, width` → `shift out of range` panic at the operator (folded away for constant amounts); `~` is `not`.

**Why:** two-token `>>` avoids splitting tokens when closing nested generics.

---

### 68. Integer methods — v7 complete (PRP-057)

**Decision:** the six methods are built-ins in `check/builtins.rs::builtin` (like `slice`): `wrapping_add/sub/mul(b: T): T`, `count_ones/leading_zeros/trailing_zeros(): i64`, `self` receiver, any integer `T`; other names E0415. Codegen `codegen/int_methods.rs` (hooked in `methods.rs` for integer receivers): plain `add/sub/mul`; `llvm.ctpop`/`ctlz`/`cttz` (`is_zero_poison` false), `zext` to `i64` below 64 bits.

**Why:** the built-in method table already typed receivers and arguments; no prelude code needed.

---

### 69. Lugha v8 direction (session 73)

**Decision:** `docs/specs/Language v8 Draft.md` is approved: packages, as milestone 30 (possibly two PRPs: loading/resolution, then `public` visibility).
- **Manifest:** `lugha.toml` next to the main file (and optionally in each package): blank lines, `#` comments, one `[packages]` table, `name = "path"` lines (identifier = double-quoted string, no escapes, paths relative to the manifest, `/`). Read by lughac (TOML subset, no new crate). Bad manifest E0321; missing folder E0322.
- **Imports:** `import a.b.c;` in package P: if P's manifest lists `a`, load `b/c.la` from that package; else local as before. Package name also a local folder/file → E0320. `import geo;` alone → E0312 with help. One name = one folder across all manifests, else E0323. Not transitive: a package's manifest is seen only inside it.
- **Visibility:** `private` = module; none = package; `public` = other packages. E0313 extended (message names the package); E0318 extended (non-`public` type in a `public` signature); E0317 retired.
- **Symbols:** module paths starting with the package name (`lugha_fn_geo/shapes/circle`); no `main` in packages (E0304); cycles fine.
- **The user chose a manifest over a `packages/` folder + flag** after asking for the difference twice — explain choices in concrete terms (what you type, where it lives).

**Why:** the session 73 discovery: the user picked packages as v8's headline over the recommended `match` conveniences.

---

### 70. Loading packages (PRP-058)

**Decision:** milestone 30 part 1, built under the user's "in one go".
- **Manifest** (`modules/manifest.rs`): hand-written TOML subset parser → `Entry { name, path, span }` + E0321 diagnostics; `#` comments after the last quote.
- **Loader** (`modules/packages.rs`, `Packages`): `folders` (name → normalised folder, lexical `.`/`..`), `listed` (per package, `None` = root). A package's manifest is read after its first module is parsed (so the main file keeps offset 0) and joins the offset space (`Loaded.manifests`, `ManifestFile`; `Source::of` merges and sorts by base). Folder existence is probed through the `read` callback (reading a directory fails with a non-`NotFound` error; tests model folders). E0322 missing folder; E0323 one name two folders; E0320 a name also in the root folder (at the entry; still registered, so imports don't cascade) or in a package's own folder (at the import); `import geo;` E0312.
- **Paths:** listed package → module path = import names, file in the package folder, package = first name; else P's prefix (`[P]` or none) + names in P's folder. `Module.package`, `Module.imports` (resolved paths); `Unit.imports` feeds the checker's `enter` (empty = as written). `load` now takes `&mut Read<'_>`.
- **Visibility unchanged** until PRP-059 (`public` still E0317).

**Why:** resolved paths in the loader keep the checker and codegen unchanged; root-folder clashes are checked globally because root modules have no prefix.

---

### 71. Package visibility — v8 complete (PRP-059)

**Decision:** `check/levels.rs`: `Level { Private, Package, Public }` from the keyword; `Owner { module, package, level }` per item (`Privacy.items`), methods/fields store a `Level` and use their type's owner; prelude items are `Public`. `Checker.package` comes from `Unit.package` (`Module::package`). `denied` decides (Public anywhere; Package same package; Private same module) for `item_visible`, `method_visible`, `method_usable` (bounds), `field_visible`, `literal_visible` (reports the hidden field; skipped when the type itself is hidden). Messages: v2's for private; `errors/v8.rs::package_item` for package. E0318 via `hidden_inside(ty, at)`: shallow structs/enums for v7 levels (unchanged), deep (type args, interface types, bounds) only for `public` declarations, so no v7 program changes. E0317 retired (constructor and `m10/e0317` golden removed; `m30/public` shows `public` builds). Spec extractor: `# lugha.toml` blocks, subfolder files, only `.la` lexed/parsed.

**Why:** levels compare as an ordered enum, so leak checks are "less visible than the declaration".

---

### 72. Lugha v9 direction (session 76)

**Decision:** `docs/specs/Language v9 Draft.md` is approved: `match` and everyday conveniences, as milestones 31–33.
- **M31 patterns:** or-patterns `p | q` anywhere a pattern goes (nested, guard covers the arm); every alternative binds the same names at the same types, else E0428 (also `mut` on a variant); exhaustiveness/W0102 per alternative. `mut name` bindings (every binding position, incl. type patterns) are an owned mutable copy; plain bindings stay read-only (E0501).
- **M32 unwrapping:** built-in `unwrap()` / `expect(msg: string)` on `Option` and `Result`, panicking `unwrap on None` / `unwrap on Err` / the message at the method name (not in the prelude); E0415 elsewhere. `%=` (zero-divisor panic).
- **M33 conditional patterns:** `if let p = cond block [else …]` (expression with `else`; `else if let` chains; irrefutable allowed silently), `while let p = cond block`, `let p = e else block;` whose `else` must not finish (E0429), bindings in scope after; scrutinees in no-struct-literal mode.
- **New codes:** E0428, E0429.

**Why:** the session 76 discovery; the user took every recommendation plus `while let` and `let … else`.

---

### 73. Better patterns — milestone 31 (PRP-060)

**Decision:** built under the user's "in one go" (milestones 31–33).
- **AST/parser:** `PatternKind::Or(Vec<Pattern>)` (from `|` between alternatives, any depth), `PatternKind::Mut(Box<Pattern>)` (`mut name`, `mut name: T`; anything else after `mut` is E0201), `(p)` is `p`. Sexp `(| a b)`, `(mut n)`.
- **Checker:** pattern typing returns the expanded ways a pattern matches (`Vec<Pat>`, products over nested ors), so `usefulness.rs` is unchanged; `arm_pattern` pairs each with its top-level alternative's span for per-alternative W0102 (an alternative is dead when none of its expansions is useful, counting earlier alternatives of the arm). Bindings are `(Ident, Type, bool mut)` (`patterns::Bindings`); `check/or_patterns.rs`: `or_alternatives` (first alternative's bindings kept; E0428 for missing/extra names or other types, `errors/v9.rs::or_bindings`), `mut_pattern` (E0428 if the bare name is a variant of the matched type, even at the top; top-level `mut x` binds the scrutinee). `mut` bindings declared `Binding::Let { mutable: true }`.
- **Codegen** (`codegen/or_patterns.rs`): `is_flat` false for `Or`/`Mut` arms; `test_or` tries alternatives (`or.next`/`or.ok` blocks); `bind_as` reuses a name's local in the arm's innermost scope (`Scopes::in_innermost`), so alternatives share one slot; `test_mut` binds an owned (`borrowed: false`), deep-copied local.

**Why:** expanding or-patterns before the usefulness check keeps that algorithm untouched.

---

### 74. Unwrapping — milestone 32 (PRP-061)

**Decision:** `unwrap()`/`expect(msg: string)` are built-ins in `check/builtins.rs` (on `Option<T>` and `$prelude.Result`, giving the payload type). Codegen `codegen/unwrap.rs` (hooked in `methods.rs` via `unwraps`): store, compare tag with `Some`/`Ok`, on mismatch `call_panic` (now `pub(super)`) at the method name with the literal `unwrap on None`/`unwrap on Err` or `expect`'s (eagerly evaluated) message; else `load_payload`. `%=`: `TokenKind::PercentEq`, `AssignOp::Rem`, numeric targets, `compound_op` → `BinOp::Rem`. `expect(1)` reports E0401 (literal of the wrong kind), not E0403.

**Why:** built in rather than prelude, so the panic location is the caller's.

---

### 75. Conditional patterns — v9 complete (PRP-062)

**Decision:** native AST forms, not a parser rewrite into `match` (which would add a hidden `_` arm and W0102 for irrefutable patterns, and wrap the rest of a block for let-else): `ExprKind::IfLet { pattern: Box<Pattern>, scrutinee, then, else_ }`, `StmtKind::WhileLet { pattern: Box<Pattern>, scrutinee, body: Box<Block> }`, `StmtKind::LetElse { pattern: Box<Pattern>, init, else_: Box<Block> }` (boxed for clippy's large-enum-variant). Parser `parser/conditional.rs` (`if let` after `if`, `while let`, `let` with a pattern ahead or `let x = e else`). Checker `check/conditional.rs`: `arm_pattern` (no exhaustiveness), bindings scoped to then/body, let-else bindings in the current scope, else block must be `never` (E0429, `errors/v9.rs`); `join_branches` extracted from `if_expr`; `unused` now `pub(super)`. Liveness walks the new forms. Codegen `codegen/conditional.rs`: `subject_of`, `test` with `next` = else / loop exit / else block (terminated with `unreachable` if needed).

**Why:** reusing the arm machinery gives or-patterns and `mut` in all three forms for free.

---

### 76. Lugha v10 direction (sessions 80–81)

**Decision:** `docs/specs/Language v10 Draft.md` is approved: three independent milestones, built "in one go".
- **M34 `Self` everywhere:** in `impl T` blocks `Self` is `T` with its parameters, as a type, in struct literals (`Self { … }`), associated calls (`Self.f()`) and variants (`Self.Red`); still E0305 in struct/enum declarations. A default method is callable on a concrete type when exactly one visible interface (calling module, its imports, the prelude) supplies it and the type satisfies it; own method wins; none → E0415, several → E0430.
- **M35 more integers:** keyword types `i8`, `i16`, `u16`; on every integer type `checked_add/sub/mul/div` (`Option`), `saturating_add/sub/mul`, `rotate_left/right(n: i64)` modulo the width, `I.MIN`/`I.MAX` (E0415 for other names and floats); `parse_u64` (`lugha_rt_parse_u64`).
- **M36 iterator helpers:** lazy prelude adapters `map`, `filter`, `take`, `skip`, `enumerate` (`Indexed<T> { i, value }`) and consumers `collect`, `count`, `fold`, `any`, `all`, `find`, `sum` on anything `for … of` iterates (own methods win; arrays keep eager v3 helpers); sources `xs.iter()`, `s.bytes()`, `range(a, b)`, and `for b of s` over bytes. Prelude type names are shadowable like `Iter`.
- **New code:** E0430.

**Why:** the session 80 discovery; the user took every recommendation.

---

### 77. `Self` everywhere — milestone 34 (PRP-063)

**Decision:** built under the user's "in one go" (milestones 34–36).
- **`Self` in `impl` blocks is parser sugar** (`parser/self_type.rs`): while reading `impl T<P…>`, `Parser::impl_self` holds the type; `named_type` turns a bare `Self` into `T` / `T<P…>` (span of `Self`), and `name_or_struct` renames an expression or struct-literal name `Self` to `T`. The checker and codegen see plain programs; `--emit=ast` shows the replaced name. Struct/enum declarations and patterns are untouched.
- **Default methods on concrete types** (`check/concrete_defaults.rs`, hooked in `method_calls.rs::on_value` after the receiver's own methods): candidates are interfaces whose key's module is the current one, an import, or `$prelude`, not `denied`, with a default of that name, that the struct/enum satisfies (`unmet` empty) at arguments `read_off` its methods (now `pub(super)`). One: record `callees` = `I.m`, `default_args`, and `used(call, "I.m", [ty, args…])`, then check against `items_for` with `Self` = the type; codegen's existing `default_method` does the rest. Two+: E0430 (`errors/v10.rs`). Other receivers keep E0415.
- **Changed tests:** `m22/e0415` now has `Sq.area` return `i64` so `Sq` doesn't satisfy `Shape`; two unit tests assert the v10 behaviour.

**Why:** a parser rewrite keeps the checker and codegen unchanged and the IR byte-identical; reusing the default instances means no new lowering.

---

### 78. More integers — milestone 35 (PRP-064)

**Decision:** built under the user's "in one go" (milestones 34–36).
- **Types:** `TyI8/TyI16/TyU16` → `TypeKind`/`Type::{I8, I16, U16}`, wherever the type lists are written out (lexer, describe, parser `ty`, sexp, `is_integer`, `is_unsigned`, `literal_range`, Display, `primitive`, `llvm_type`, `is_signed`, `layout`).
- **Printing:** goes through the `i64`/`u64` runtime functions, sign- or zero-extended (`suffix`, `runtime` ABI, `abi_value`, `text_of`), with no new runtime printers.
- **Externs:** `c_abi` adds `signext` for `i8`/`i16` and `zeroext` for `u16`.
- **`I.MIN`/`I.MAX`:**
  - **Parser:** an integer keyword followed by `.` is `ExprKind::Name("i16")`.
  - **Checker:** `access.rs::field` checks it first (`Type::int_keyword`, `Type::int_limit` in `check/numbers.rs`); base recorded `void`; another name is E0415.
  - **Codegen:** an `expr.rs` arm before the field arms → `int_limit` constant.
- **Methods:** `check/builtins.rs` entries; `codegen/int_checked.rs` (`int_v10_method`, called first from `integer_method`).
  - `checked_*`: with.overflow plus a `select` of Some/None.
  - `checked_div`: divides by `select(bad, 1, b)`.
  - `saturating_add/sub`: `.sat`.
  - `saturating_mul`: overflow → MAX, or MIN when the signs differ.
  - Rotations: `fshl/fshr(a, a, trunc n)`.
- **`parse_u64`:** in `INPUT`, codegen `function.rs`'s input list and `input_intrinsic`; `lugha_rt_parse_u64` in `runtime/input.c`; `RUNTIME_SYMBOLS` has 34 entries.
- **Out-of-range literals are E0402** (not E0401 as the PRP said).

**Why:** following `u32`'s path kept the change mechanical. Routing printing through the 64-bit functions avoids new runtime code.

---

### 79. Iterator helpers — milestone 36, v10 complete (PRP-065)

**Decision:** built under the user's "in one go" (milestones 34–36).
- **Prelude:** `src/prelude_iter.la`, joined after `prelude.la` (`PRELUDE = concat!(…)`) into one `$prelude` module. It holds:
  - sources `ArrayIter<T>`/`array_iter`, `Bytes`/`str_bytes`, `Range`/`range`;
  - adapters `Map`, `Filter`, `Take`, `Skip`, `Enumerate` (each `{ src: Iter<T>, … }`) with `iter_map`, …;
  - `Indexed<T> { public i, public value }`;
  - consumers `iter_collect/count/fold/any/all/find`;
  - `iter_sum_<N>` for the ten number types, written as loops: closures there would add to every program's checked closure list.
- **Checker:** `check/iter_helpers.rs` (`iter_helper_call`, hooked in `on_value` after the array helpers).
  - It applies when the receiver doesn't declare the name (`method` for structs and enums, `offered` otherwise) and `next_of` (now `pub(super)`) gives `T`.
  - It then converts the receiver to `Iter<T>` (`convert`), binds from `Iter<T>`, and infers as `helper_call` does.
  - `helper_function` maps `xs.iter()` → `array_iter` and `s.bytes()` → `str_bytes`.
  - `call.rs`: a bare `range` that no module declares calls `$prelude.range`.
  - `prelude_type` also resolves `ITER_TYPES`.
  - `for_of` gives a string the element `u8`.
- **Prelude structs print with `$prelude.`** (`Type` Display strips the prefix only for enums and interfaces): codegen names LLVM types and symbols by Display, so a user `struct Range` would otherwise clash.
- **Prelude methods compile only when reached:**
  - **Codegen:** `declare_methods`/`define_methods` skip the prelude unit; its methods come from `monos` (`instance_name(k, [])` = `k`), with lookups falling back to `monos` (`methods.rs` found chain, `iterators.rs::next_function`, `interfaces.rs` table slots).
  - **Checker:** records the use:
    - in `on_value` (non-generic method calls);
    - in `iterator_element` (every struct/enum `next`);
    - in `conversion_uses` (already did);
    - in `bound_target` (prelude keys listed even when not generic).

    `used` keeps empty-argument keys only for the prelude.
- **Codegen `for b of s`:** `for_of` treats a string as `u8[]` with `heap.rs::byte_address` (8-byte string header).
- **Panics:** a `sum` overflow panics located in the prelude (`<prelude>:L:C`).

**Why:** writing everything in Lugha reuses generic interface values, closures and the instance machinery. Gating prelude methods keeps every earlier program's IR byte-identical.

---

### 80. Lugha v11 direction (sessions 84–85)

**Decision:** `docs/specs/Language v11 Draft.md` is approved: package versions and re-exports, built "in one go".
- **M37 package versions:**
  - a package's `lugha.toml` may have `[package]` with `version = "MAJOR.MINOR.PATCH"`;
  - a manifest entry may be `name = { path = "…", version = "R" }`, a Cargo-compatible requirement (`"1.2"` = >=1.2.0, <2.0.0; the first non-zero part is kept);
  - a mismatch, or a requirement on an unversioned package, is E0324; malformed text is E0321;
  - packages stay local: no network, no lock file.
- **M38 named imports and re-exports:**
  - `import a.b { X, y };` binds the module and the listed items in the file;
  - `public import a.b { X, y };` also makes them items of the module, `public`, the same items (no copies; symbols unchanged);
  - a re-exported item must be `public` (E0318);
  - errors: E0313 not visible, E0301/E0305 unknown, E0302/E0314 clashes;
  - `public import` without braces is E0201.
- **New code:** E0324.

**Why:** the session 84 discovery; the user took every recommendation.

---

### 81. Package versions — milestone 37 (PRP-066)

**Decision:** built under the user's "in one go" (milestones 37–38).
- **`modules/version.rs`:**
  - `Version` (three numbers, no leading zeros, `u64` overflow is an error);
  - `Requirement { base, parts }`, where `ceiling()` bumps the first non-zero written part (else the last written one) and `matches` = `base <= v < ceiling`;
  - `unmet` builds E0324, naming the range (`>= 1.2.0, < 2.0.0`) and the version found.
- **`modules/inline.rs`:** parses `{ path = "…", version = "…" }` by hand (quoted values, either order, trailing comma, `path` required).
- **`modules/manifest.rs`:**
  - `parse` returns `Manifest { entries, version }`;
  - a `Table` state takes `[packages]` and `[package]`, each at most once and in either order;
  - `[package]` takes only `version = "…"`;
  - `Entry.requirement`.
- **`modules/packages.rs`:**
  - `read_manifest` records `versions[package]`, and pushes `required` for entries that passed E0322/E0323;
  - `check_versions`, called at the end of `load`, reads any required package's manifest not yet read (which may add requirements, hence an index loop) and reports E0324.
- **Tests:** `modules/version_tests.rs` uses `tests.rs`'s `run`/`codes` (now `pub(super)`).
- **Goldens:** E0321's help mentions the new forms (`m30/e0321.stderr` updated).
- **Spec:** `Language v11 Specification.md` begins.

**Why:** checking after loading sees every manifest, including those of packages nothing imports. Keeping the version logic in its own files leaves `manifest.rs` and `packages.rs` small.

---

### 82. Named imports and re-exports — milestone 38, v11 complete (PRP-067)

**Decision:** built under the user's "in one go" (milestones 37–38).
- **AST and parser:**
  - `Import` gains `items: Vec<Ident>` and `public: Option<Span>`;
  - `parser/import.rs` has `import_visibility` (`private import` is E0201; `public` kept) and `import_with` (an optional `{ … }` list; `public` without names is E0201; an empty list is E0201, an unclosed one E0202);
  - `item.rs::program` uses them;
  - the sexp is `(public import a b (items X y))`.
- **Checker (`check/named_imports.rs`):**
  - **Registration:** `Checker::aliases` maps `qualify(module, X)` → `Alias { target, at, public }`. `named_imports` runs before every declaration pass: E0302 for a name the module declares, E0314 for one listed twice or named like an imported module. It registers a privacy record, `public` for re-exports and `private` otherwise.
  - **Following:** `key()` follows aliases (at most 64 steps). `member(path, name)` replaces `qualify(imported, name)` at the `m.X` sites (lookup.rs ×3, construct.rs, iface_values.rs, fun_values.rs), checking the alias's own visibility first. `interfaces.rs::interface_key` uses `follow` (no span there).
  - **Validation:** `check_named_imports`, after the declaration passes, reports E0301 for an unknown item, E0313 for one not visible here, and E0318 (its own wording) for a re-export of a non-`public` item.
- **Codegen is unchanged:** keys are the declaring module's.

**Why:** aliases resolved at the key level reach every name use without touching codegen, and keep symbols and IR byte-identical.

---

### 83. Lugha v12 direction (sessions 87–88)

**Decision:** `docs/specs/Language v12 Draft.md` is approved: every value prints, built "in one go".
- **M39 printing values:**
  - the prelude's `interface Display { fun show(self): string; }`; a struct's or enum's own `fun show(self): string` (any visibility) is used, and a `show` of another signature is ignored;
  - otherwise the built-in form: `Point { x: 1, y: 2 }`, `Circle(2.0)`/`Empty`, `[1, 2]`, `Some(x)`/`None`, `Ok`/`Err`; bare type names, no generic arguments;
  - strings are quoted and escaped only inside built-in forms;
  - generic `T` prints without a bound;
  - lowering: `lugha_show_<type>` helpers, emitted only when used, and the runtime's `lugha_rt_str_quote`.
- **M40 printing anything:**
  - interface values print by their concrete type, through an identity chain at the print site (tables unchanged);
  - function values print as `<fun>`;
  - `unwrap` on `Err` panics with `unwrap on Err: <error>`.
- **New codes:** none (E0403 shrinks).

**Why:** the session 87 discovery; the user took every recommendation.

---

### 84. Printing values — milestone 39 (PRP-068)

**Decision:** built under the user's "in one go" (milestones 39–40).
- **Checker (`check/printing.rs`):**
  - **What prints:** `printable` is every type but `void` and (until M40) interface and function values; `call.rs` (the print family and `to_string`) and `input.rs` (interpolation) use it.
  - **Uses:** each print site records `printed` → a use of key `$print` at `[ty]` (skipped for numbers, `bool`, strings). `monomorphize` expands it, at concrete types, into `show_instances(ty)`, which walks fields, payloads, elements, `Option` and the types converted to an interface, and lists each generic type's own `show` instance.
  - **Which `show`:** `own_show` requires receiver `self`, no params, `string` result, and no own type params.
  - **Prelude:** `Display` is in `prelude_iter.la`; `or_prelude_interface` resolves `Iter` and `Display`.
- **Codegen (`codegen/show.rs`):**
  - `printable(ty, value)` turns a non-plain value into `(String, text)`; it is used for the print, `to_string` and `eprint` arguments, and by interpolation (`text`).
  - `part` quotes strings (`lugha_rt_str_quote`), gives `<fun>` for function values, and otherwise calls `lugha_show_<mangle>(value)`.
  - The helper calls `own_show` (non-generic method, prelude instance, or generic instance from `monos`), or builds `Name { f: … }`, `Variant(…)` (a tag switch with an accumulator slot) or `[…]` (`count_loop` and a `select` for the `, `). Names are bare.
- **Interface values (`codegen/show_iface.rs`):** the interface helper is declared and queued in `Lowerer::shows`. `finish_shows` (after `finish_upcasts`) builds its body: an identity-slot comparison per concrete type in `Upcasts::concretes(interface)`, then `load_element` from the box and that type's text.
- **Runtime:** `lugha_rt_str_quote` in `print.c`; `RUNTIME_SYMBOLS` has 35 entries.
- **Earlier tests changed:** four E0403 checker tests (`bounds`, `call`, `input` ×2) now assert the v12 behaviour; printing `void` stays E0407.

**Why:** helpers per type mirror the copy and equality helpers. Building interface bodies at the end keeps the tables and earlier IR unchanged.

---

### 85. Printing anything — milestone 40, v12 complete (PRP-069)

**Decision:** built under the user's "in one go" (milestones 39–40).
- **Checker:**
  - `printing::printable` is everything but `void` (which is E0407);
  - `unwrap_prints` (called beside the built-ins in `on_value`) records `printed(call, E)` for `unwrap()` on `Result<T, E>`, so `E`'s `show` instances are listed.
- **Codegen (`codegen/unwrap.rs`):** in the failing branch of `unwrap()` on a `Result`, it loads the `Err` payload and panics with `"unwrap on Err: "` + `text(e)`.
- **IR:** programs that `unwrap()` a `Result` (`m32/err`, `more`, `unwrap`) get new IR; every other earlier program is byte-identical.
- **Goldens and tests:**
  - the `m32/err` golden is now `unwrap on Err: no`;
  - the E0403 tests for printing function and interface values were replaced by E0407 `void` cases, with positive tests in `check/printing.rs`.
- **Spec:** a §10 sentence lists only stdout lines in backticks; the panic text is written plain.

**Why:** milestone 39's codegen already printed every type, so milestone 40 was a checker relaxation plus the panic message.

---

### 86. Lugha v13 direction (sessions 90–91)

**Decision:** `docs/specs/Language v13 Draft.md` is approved: four conveniences, built "in one go".
- **M41 `Self` in declarations:** in struct fields and enum payloads, `Self` is the declared type with its parameters (parser sugar, as in `impl`s).
- **M42 `for x of mut e`:**
  - on an iterator place (rooted in `let mut`, E0501), `next` is called on `e` itself, so loops resume;
  - on an array place, `x` aliases each element (assignments and `mut self` calls write through);
  - E0507 for changing `e`/`xs` in the body; E0506 on strings.
- **M43 helpers:** `zip` (prelude `Pair<A, B> { public first, public second }`), `chain`, `take_while`, `skip_while`, `flat_map` (`f` returns an iterator), `min`/`max` (`Ord`, `Option<T>`), `position` (`Option<i64>`).
- **M44 format specifiers:**
  - `"{x:[[fill]align][#][0][width][.precision][kind]}"`, split at the first top-level `:`;
  - precision for floats only, kinds `x X b o` for integers only (two's complement at the type's width), `#` only with a kind, `0` only for numbers, width ≤ 1024 and in bytes;
  - fill, align and width also for any value's text;
  - E0431 for anything else;
  - lowering: `lugha_rt_fmt_int`, `lugha_rt_fmt_float`, `lugha_rt_pad`.
- **New code:** E0431.

**Why:** the session 90 discovery; the user took every recommendation.

---

### 87. `Self` in declarations — milestone 41 (PRP-070)

**Decision:** built under the user's "in one go" (milestones 41–44). `struct_decl` (`parser/item.rs`) and `enum_decl` (`parser/decl.rs`) read their bodies inside `Parser::in_impl(name, type parameter names)`, so `Self` becomes the declared type (`T<P…>` for a generic one) exactly as in `impl` blocks. The checker and codegen are unchanged. Two tests changed: `struct S { me: Self }` is now E0307 (recursive), and `struct N { kids: Self[] }` checks.

**Why:** reusing v10's parser sugar made it a two-line change with byte-identical IR.

---

### 88. `for … of mut` — milestone 42 (PRP-071)

**Decision:** built under the user's "in one go" (milestones 41–44).
- **AST and parser:** `ForIter::MutOf(Expr)`, read when `mut` follows `of`; the sexp is `(of-mut e)`.
- **Checker (`check/for_mut.rs`, `for_of_mut`):**
  - **`loop_place`:** E0506 for a string, E0502 unless the expression is a name, field or element with a place path, E0501 unless its root is `let mut` or `mut self` (loop-worded message, help `let mut`). It also calls `changed`.
  - **Guard:** `e`'s path goes on `iterating` (E0507 for changing it in the body).
  - **Binding:** an array element is declared `Binding::Let { mutable: true }`; an iterator goes through `iterator_loop` as before.
- **Codegen:** `for_of(…, in_place)` and `iterator_loop(…, in_place)`.
  - **Arrays:** the local's `ptr` *is* the element address (no alloca, still `borrowed`). An `i1` load or store on the `i8` element byte is valid for 0/1, so `bool` aliases too.
  - **Iterators:** `next` is called on `place_address(e).0`.
- **Tests:** the parser test lives in `parser/conditional.rs`, since `parser/stmt.rs` is at 296 lines.

**Why:** aliasing the element's address needs no write-back, so `break`, `continue` and `return` keep every write.

---

### 89. More iterator helpers — milestone 43 (PRP-072)

**Decision:** built under the user's "in one go" (milestones 41–44).
- **Prelude:** `src/prelude_more.la`, the third file joined into `PRELUDE`. It holds `Pair`, `Zip`, `Chain`, `TakeWhile`, `SkipWhile` and `FlatMap<T, U, I: Iter<U>>`, the last keeping its current inner iterator in a 0/1-element array so `self.cur[0].next()` steps it in place. `iter_min`/`iter_max` (`T: Ord`, the first extreme, kept in a 0/1 array) and `iter_position`.
- **Checker:**
  - `HELPERS` (20 names) and `ITER_TYPES` (15 names) grow;
  - `struct_literal.rs` falls back to `prelude_iter_type` for a bare struct name no module declares, so `Pair { … }` builds;
  - `chain` of another element type is E0422 (the second iterator doesn't satisfy `Iter<T>`), not E0403.

**Why:** everything is Lugha over generic interface values, and IR stays byte-identical.

---

### 90. Format specifiers — milestone 44, v13 complete (PRP-073)

**Decision:** built under the user's "in one go" (milestones 41–44).
- **Lexer:**
  - `Interp` also counts `(`/`[` (`parens`);
  - at depth 0, a `:` calls `format_spec` (`lexer/format_spec.rs`), which emits `TokenKind::FmtSpec(raw text)` up to the `}`, a newline or a `"`;
  - an empty spec is still a token.
- **AST and parser:** `Part::Expr(Expr, Option<FormatText { text, span }>)` (`FormatText` re-exported from `ast`); the parser takes a `FmtSpec` after the expression; the sexp is `(fmt e "spec")`.
- **`src/format.rs`** (crate root, shared): `FormatSpec`, `parse` (width and precision ≤ 1024, ASCII non-brace fill, kinds `x X b o`) and `check` against `Value::{Int, Float, Other}`. The checker (`check/input.rs`) reports E0431 (`errors/v13.rs::bad_format`) at the spec span.
- **Codegen (`codegen/format.rs::formatted`):**
  - integers → `lugha_rt_fmt_int(i64 (sext/zext), bits, signed, kind, alt, fill, align, width, zero)`;
  - floats → `lugha_rt_fmt_float(f64, is_f32, precision or -1, fill, align, width, zero)`;
  - others → `lugha_rt_pad(text, fill, align, width)`;
  - the default alignment is right for numbers; `zero` only when no alignment is given.
- **Runtime:** `runtime/format.c`, joined fifth (`RUNTIME_PARTS` and `RUNTIME_SOURCE`); `RUNTIME_SYMBOLS` has 38 entries.

**Why:** specifiers parse in one shared module, so the checker and codegen agree, and plain numeric arguments keep the runtime ABI simple.

---

### 91. Lugha v14 direction (sessions 95–96)

**Decision:** `docs/specs/Language v14 Draft.md` is approved: seven features in milestones 45–50. Milestones 45–49 are built "in one go"; M50 (git packages) waits for the user's security review of its PRP.
- **M45 bytes and indexes:** `read_bytes`/`write_bytes`/`append_bytes`/`read_stdin_bytes`, `s.to_bytes()`, `string_from_utf8` (checked, `Err("invalid UTF-8 at byte N")`), any integer type as an index (negative or too large panics).
- **M46 `char`:**
  - `'a'` literals (E0112) and the `char` keyword (`i32`);
  - `s.chars()` (prelude, U+FFFD for bad bytes), `char_count()`;
  - `as` to integers and from integers (U+FFFD for non-scalars), `char_from`;
  - ASCII/Latin-1 classification and case;
  - quoted `'a'` inside built-in forms.
- **M47 tuples:**
  - `(A, B, …)` with at least two elements, `t.0` (the parser splits `0.1`);
  - patterns everywhere, and irrefutable `let`/`for` patterns (E0433);
  - element-wise `==`, lexicographic `<`, printing `(1, "a")`;
  - `zip` yields tuples and `Pair` is removed (a break); `enumerate` keeps `Indexed`.
- **M48 `format`:** `format("{} {:>5}", a, b)`: the template is a string literal (the lexer reads it raw after `format(`), placeholders must equal the arguments (E0432), and specs fit as v13 (E0431). `format` is an intrinsic name.
- **M49 imports:** `import a.b as c;`, `import a.b { X as Y };`, `public import a.b [as v];` giving `m.v.X` (one level).
- **M50 git packages:**
  - `{ git, version }` entries, the system `git` (argument lists, no shell, no prompts or hooks), `https://`/`file://` only;
  - the cache under `$LUGHA_CACHE`/XDG/`~/.cache/lugha`;
  - `lugha.lock` pinning commits, `lughac update`, `--offline`;
  - E0325/E0326.
- **New codes:** E0112, E0325, E0326, E0432, E0433.
- **Approved split:** `pub struct Checked` moves verbatim from `check/mod.rs` to `check/checked.rs`.

**Why:** the session 95 discovery. The user chose the template `format` and `zip` → tuples over the recommendations; everything else as recommended.

---

### 92. Bytes and indexes — milestone 45 (PRP-074)

**Decision:** built under the user's "in one go" (milestones 45–49).
- **Housekeeping:** the approved split came first — `Checked` moved verbatim to `check/checked.rs` (committed directly to `main` before the branch, a slip).
- **Checker:**
  - `INPUT` has 16 entries (`read_bytes`, `write_bytes`, `append_bytes`, `read_stdin_bytes`, `string_from_utf8`);
  - `builtins.rs` gains `(String, "to_bytes") → u8[]`;
  - `access.rs::index` accepts any integer index (`value(index, None)`, E0403 via `mismatch` otherwise). `s[1.5]` is now E0403 rather than E0401.
- **Codegen:**
  - `io.rs` lowers the five like `read_file`/`write_file` (out-slots, `result`), and `function.rs`'s `IO` list has 12 entries;
  - `methods.rs` lowers `to_bytes` to `lugha_rt_str_to_bytes`;
  - `heap.rs::element_place` widens the index with `cast.rs::widen_index` (sext/zext), so `i64` IR is unchanged.
- **Runtime:** `runtime/bytes.c`, joined sixth in both `RUNTIME_PARTS` and `RUNTIME_SOURCE`.
  - Read and write reuse `lugha_rt_read_file`/`lugha_rt_write_file` (`string_of`);
  - `utf8_step` is strict;
  - `RUNTIME_SYMBOLS` has 43 entries.

**Why:** reusing the v4 file paths keeps error messages and path checks identical.

---

### 93. Unicode text — milestone 46 (PRP-075)

**Decision:** built under the user's "in one go" (milestones 45–49).
- **Lexer:** `lexer/char_lit.rs` lexes `'…'` to `TokenKind::Char(u32)`, with escapes `\n \t \r \0 \\ \' \"` and `\u{1-6 hex}`. Errors are E0112, still pushing `Char(0xFFFD)` so parsing goes on. The keyword is `TyChar`.
- **AST:** `ExprKind::Char(u32)` and `TypeKind::Char`; the sexp shows `{:?}` of the char.
- **Checker:**
  - `Type::Char`; `ord_ok` includes it;
  - `ops.rs::cast` allows `char` ↔ any integer;
  - `helper_function` maps `char` methods to prelude `char_<m>` and string `chars`/`char_count` to `str_<m>`;
  - `char_from` joins `range` as a bare prelude name. It is shadowable, not a reserved intrinsic as the draft listed;
  - `ITER_TYPES` gains `Chars`;
  - `printing::plain` includes `char`.
- **Codegen:**
  - `char` is `i32` (layout 4/4), compared unsigned (not `is_signed`);
  - `cast.rs::to_char` widens to `i64` and selects U+FFFD unless < 0xD800 or 0xE000..=0x10FFFF;
  - print, `to_string` and interpolation go through the `char` runtime suffix;
  - `show.rs::part` quotes it with `lugha_rt_char_quote`.
- **Prelude:** `src/prelude_text.la`, the fourth file. `Chars.next` is a strict UTF-8 decoder in Lugha (U+FFFD, one byte on); the Latin-1 tables are written as ranges.
- **Runtime:** `runtime/chars.c` (`utf8_encode`, print, to_string, quote); `RUNTIME_SYMBOLS` has 46 entries.

**Why:** decoding in Lugha needs no new runtime surface. Printing needs UTF-8 encoding, which is cheap in C.

### 94. Tuples — milestone 47 (PRP-076)

**Decision:** built under the user's "in one go" (milestones 45–49).
- **Representation:** `(A, B, …)` is the generic struct `$tuple<n>` (fields `0`, `1`, …, parameters `T0…`), registered by `check/tuples.rs::tuple_type` on first use. Layout, copies, `==`, `t.0`, instances and printing reuse the struct paths. `Type` prints it `(A, B)`, so LLVM names read `%"(i64, string)"`.
- **Lexer:** a number right after `.` lexes as an integer only (`t.0.1`).
- **Parser:** `parser/tuples.rs`. `(a,)` is E0201. `let (…) = e;` is `LetElse { else_: None }`. `for (i, w) of e` becomes a hidden `$item<offset>` loop variable plus a `let` at the top of the body.
- **Checker:** tuple patterns give `Pat::Ctor(Variant(0, "()"), …)`, and `variants_of` gives tuples the one variant `"()"`, so E0417/W0102 work unchanged. A `let` without `else` or a `for` destructuring must be irrefutable (E0433, `errors/v14.rs`). `<` needs every element `Ord`.
- **Codegen:** `codegen/tuples.rs`: `insertvalue` literals; patterns test each field's address (no tag; `is_flat` excludes them); `<` calls `lugha_cmp_<mangle>(a, b) -> i32`.
- **Prelude:** `Zip` yields `(A, B)`; `Pair` is removed (a planned v13 break), `ITER_TYPES` has 15 entries.

**Why:** reusing generic structs gives tuples every struct feature for a few hundred lines; the one-variant trick keeps exhaustiveness in one place.

### 95. `format` — milestone 48 (PRP-077)

**Decision:** built under the user's "in one go" (milestones 45–49).
- **Lexer:** a string right after `format` `(` (not after `.`) is one `Str` token with escapes decoded and braces as written (`lexer/string.rs::template`).
- **Parser:** `format` `(` STR followed by `,` or `)` → `ExprKind::Format(Box<FormatCall { template, template_span, args }>)` (`parser/format.rs`); anything else stays a call.
- **Shared:** `src/template.rs` splits the template (`pieces`, `holes`) and builds interpolation `Part`s with the argument exprs cloned (`parts`), so the checker's `interpolation` and codegen's `interpolation` do the rest.
- **Checker:** `check/templates.rs`; E0432 for a bad template, a count mismatch, or (via `INPUT`) a non-literal template. `format` is reserved (E0302).

**Why:** reusing interpolation gives printing and specifiers for free, with no runtime change and byte-identical IR elsewhere.

### 96. Import aliases and re-exports — milestone 49 (PRP-078)

**Decision:** built under the user's "in one go" (milestones 45–49).
- **AST:** `Import { path, alias, items: Vec<ImportItem { name, alias }>, public }`, with `bound()` on both (the alias, or the last segment / the name). The loader's E0314 checks and the checker's `imports` table use `bound()`.
- **Parser:** `as` after a path or item; `public import a.b;` allowed (the v11 E0201 is gone); `qualified_name` in `generic.rs` reads `m.v.T` in types and bounds as module ident `"m.v"`, and `expr.rs::member` does the same for `m.v.S { … }`.
- **Checker:** `named_imports` records `reexports[qualify(module, v)] = path` for a public import without items. `check/reexports.rs`: `module_path("m" | "m.v")` (one re-export, locals shadow), `module_name(expr)` (`Name` or `Field(Name, v)`), and `member_through`, which replaced the `imported` + `member` pairs and reports E0313 for a non-`public` item the ordinary rules would allow.
- **Codegen:** unchanged; keys are the declaring module's.

**Why:** dotted module idents ("m.v") reuse every `Option<Ident>` module slot in the AST, so three-part names needed no new AST shapes.

### 97. Git packages — milestone 50 (PRP-079)

**Decision:** the user reviewed PRP-079's security design and approved it as written (session 102).
- **Manifest:** `inline.rs` reads `{ key = "value" }` generically (`pairs`, `get`); an entry's `Place` is `Path` or `Git`; `Entry.git: Option<String>`. URLs: `https://host/…` without user info, or `file:///…` (`git::check_url`).
- **`modules/git.rs`:** the `Git` trait (`ls_remote`, `clone_tag`, `head`); `SystemGit` runs `git` from argument lists with `CONFIG` (`-c` hooksPath=/dev/null, fsmonitor=false, symlinks=false, submodule.recurse=false) and `ENV` (`GIT_TERMINAL_PROMPT=0`, `GIT_ALLOW_PROTOCOL=https:file`, low-speed timeout); `--` before every URL and folder; stderr trimmed to 10 lines.
- **`cache.rs`:** root from `LUGHA_CACHE`/`XDG_CACHE_HOME`/`HOME` (absolute only); `<root>/git/<escape(url)>/<commit>`; clones go to `<root>/tmp/clone-…` and are installed by rename after removing `.git`.
- **`lock.rs`, `fetch.rs`:** `Fetcher { git, root, mode, lock, used }`; locked entries are used exactly (E0326 if a fresh clone's HEAD differs); unlocked ones take the highest matching tag; `--offline` runs no `git` (E0325). `Loaded.lock` is the new lock text, written by `driver/update.rs` (temp file + rename).
- **Driver:** `Mode { offline, update }` flows through `pipeline::{loaded, parsed, front}`; `lughac update`; `--offline` on build/run/check.

**Why:** the security rules live in one file (`git.rs`) with tests that check them as data; the loader stays testable without `git`.

### 98. v15 draft — package layout first (session 104, approved session 105)

**Decision:** v15 starts with M51, package layout (`Language v15 Draft.md`):
- `[package] name` is optional and checked against listing keys (E0327);
- `source = "src"` is explicit;
- a program without a manifest beside its main file looks upward for one, stopping at the first `lugha.toml`, a `.git` or `$HOME`, and a found `name` gives an implicit package;
- the file argument stays required;
- a file reached as two modules is E0328;
- `example/..` folds to `.` (bug fix).

The rest of v15 (directories, processes, env, Unicode follow-ups) is in `TODO.md`, with its discovery paused.

**Why:** found setting up the user's `glowworm` library: `glowworm.src.array` was unwanted, and examples needed a manifest of their own.

### 99. Package layout — milestone 51 (PRP-080)

**Decision:** built as approved (with the splits of `modules/mod.rs` into `parse.rs` and `modules/packages.rs` into `paths.rs`).
- **`package_table.rs`:** `[package]` `version`/`name`/`source`; `Manifest.package: PackageTable`.
- **`layout.rs`:**
  - `LoadEnv { fetcher, home, cwd }` is `load`'s third argument;
  - `manifest_folder` is the bounded upward lookup (manifest, `.git`, `$HOME`, top);
  - `Packages::layout` checks E0327, sets `sources` (module roots) and inserts the implicit package as the root manifest's first entry;
  - `two_names` gives E0328.
- **`packages.rs`:** `folder` (the manifest's) vs `module_root` (with `source`); `resolve` reads a package's manifest before building its file path.
- **`mod.rs`:** `seen` maps normalized files to module paths (E0328).
- **`paths::normalized`:** gives `.` instead of an empty path.

**Why:** the user's `glowworm` library wanted `glowworm.array` from `src/` and manifest-free examples.

### 100. Operator overloading — milestone 52 planned (session 106, approved session 107)

**Decision:** the user's goal is that `a + b` / `a * b` on their `Vec2` run (`Language v15 Draft.md` decisions 8–13):
- `+ - * / %` and unary `-` map to `add`/`sub`/`mul`/`div`/`rem`/`neg`. Opt-in is structural, with a shape: plain `self`, one parameter (none for `neg`), and a result; any one right-hand type.
- The prelude has `Add`…`Neg` and `Num` bounds, satisfied built-in by numbers (and `string` for `Add`).
- `impl NdArray<T: Num>` bounds.
- `0`/`1` as `T: Num`.

**Also:** the user raised the file-size limit from 300 to 500 lines (CLAUDE.md, session 107), and asked for M52 "in one go".

### 101. Operator overloading — milestone 52 (PRP-081)

**Decision:** built as approved (MEMORY 100).
- **Checker (`check/operators.rs`):**
  - `binary`/`unary`/`assign` route a struct/enum/interface/`T` left operand to `operator_call`. It checks the method shape (`operator_method`: plain `self`, arity, a result; `T` through `bound_entry`), then calls `on_value(expr, …)` with the operator expression as the call. E0404 otherwise, with shape or bound help.
  - `builtin_operator` lets numbers (and `string` for `Add`) satisfy `$prelude.Add`…`Num` in `unmet`. `Num` is for numbers only (E0419 in `meets_bounds`), and `has_num` gives `==`, `<` and `0`/`1` literals.
- **`check/impl_bounds.rs`:** `impl_generics` merges an `impl`'s bounds into the renamed generics; `ImplDecl.params` is `Vec<TypeParam>`.
- **Codegen (`codegen/operators.rs`):** `operator` (struct/enum → `operator_on`; interface → `method_call` with a synthetic callee) and compound assignment; numbers and strings fall through to the built-ins. An `Int` literal at a float type is a float constant.
- **Prelude:** `src/prelude_ops.la` (fifth file); `is_operator_bound` adds the names to `or_prelude_interface`.

**Why:** reusing method calls keeps instances and dispatch in one place; per-instance lowering makes `add<i64>` a plain checked add.

### 102. The std library and `std.math` — milestone 53 planned (session 110, approved session 111)

**Decision** (`Language v15 Draft.md` decisions 14–18):
- **Std:** `std.<module>` modules in Lugha (`src/std/`), embedded in `lughac`, loaded only when imported, with no opt-out. `std` is reserved (E0321 for a manifest entry, E0320 for a local `std/` or `std.la`; E0312 for an unknown module).
- **`std.math`:** `pi()`/`e()` as functions. The float functions are generic over a new built-in `Float` bound (`f32`, `f64`; implies `Num`'s operators), lowered to LLVM float intrinsics through `$float.<op>` built-ins that only bundled sources can name. `abs<T: Num>`, `min`/`max`/`clamp<T: Ord>`.

**Why `Float`, not `Num`:** an integer `sqrt` would truncate, and a Lugha body can't cast `T` (the user asked; session 110).

### 103. The std library and `std.math` — milestone 53 (PRP-082)

**Decision:** built as approved (MEMORY 102).
- **Sources:** `src/std/math.la`, embedded as `lugha::STD`.
- **Loader:**
  - `packages.rs::std_target` maps `std.<m>` to `<std>/<m>.la`, with the hidden package `std`;
  - `mod.rs` takes the bytes from `STD`;
  - `std` is reserved in `manifest.rs` (entry) and `package_table.rs` (`name`).
- **Lexer:** `lex_bundled`; `$` names only in bundled sources.
- **Checker:** `check/float_ops.rs` (`float_op`, `float_call`, `FLOAT_OPS`); `Float` in `prelude/ops.la`, `builtin_operator`/`has_float`, E0419 help.
- **Codegen:** `codegen/float_ops.rs` lowers to `llvm.<op>` (with `ln` → `log`) at the call's float type.
- **Tests:** `check::modules::test_util::prelude_codes` checks in-memory modules with the prelude.

### 104. `std.random` — milestone 54 planned (session 113, approved session 114)

**Decision:**
- **Generator:** xoshiro256\*\* in Lugha (`src/std/random.la`), seeded by splitmix64.
- **Seeding:** `seeded(u64)` and `from_os()` (runtime `lugha_rt_os_seed` via `getrandom`, falling back to `/dev/urandom`); no global generator.
- **Methods (`mut self`):** `next_u64`, `uniform(): f64`, `below`/`between` (half-open, unbiased by rejection, panic on empty), `normal<T: Float>(mean, std)`, `chance`, `choice` (`Option`), `shuffle` (returns a copy).
- **New built-ins:** `$float.from_f64`, `$os.seed`.
- **Not cryptographically secure.**

**Why `uniform(): f64`:** with no arguments, `uniform<T: Float>()` can't infer `T` (E0412 on every bare call); the user approved the change.

### 105. `std.random` — milestone 54 (PRP-083)

**Decision:** built as approved (MEMORY 104).
- **`src/std/random.la`:** `Rng { s0..s3 }`, `splitmix`, xoshiro256\*\* `next_u64`, the rejection loop in `below`.
- **Built-ins:** `$float.from_f64` (expected float type; fptrunc at f32) and `$os.seed` (`lugha_rt_os_seed`); `os_op` lives next to `float_op`.
- **Runtime:** `runtime/random.c` (`getrandom`, `/dev/urandom`, `die`); `RUNTIME_SYMBOLS` has 47 entries.
- **Follow-up:** std modules compile eagerly (noted in TODO).

### 106. Copy-on-write arrays planned — supersedes MEMORY 20 / DECISION-010 (session 116, approved session 117)

**Decision (DECISION-012):**
- **Sharing:** copy sites share arrays and set a sticky `shared` flag (the top bit of `cap`); the first write to a shared array clones it (`lugha_own_<type>`).
- **Writes:** they reach arrays through their slots, after the right-hand side; `for … of mut` goes by slot and index.
- **C:** externs get unshared arrays.
- **Unchanged:** move on last use stays. Program output never changes.

**Why:** a sticky flag needs no reference counts (DECISION-010's main cost), and firefly's read-then-modify bindings (`let g = self.grads[i];`) can't be removed statically. Copy-then-write programs gain nothing and pay a flag check per write; timings are recorded either way.

### 107. Copy-on-write arrays — milestone 55 (PRP-084)

**Decision:** built as planned (MEMORY 106), with these changes:
- **Inline `own`, no `lugha_own_<type>`:** load `cap`, test the sign bit (a cold branch), clone with the existing `lugha_copy_<type>` helpers (now one level deep; inner arrays are marked) or `clone_flat`, store back. `codegen/cow.rs`.
- **Two-phase places** (`codegen/places.rs`): `place_plan` evaluates and bounds-checks indexes, `place_commit` walks from the root owning each array. Assignment and `push` commit after the value; `swap` plans both places before committing either; `for … of mut` re-reaches its place on every use (arrays through `Lowerer.aliases`, iterators before every `next`). Interface `mut self` calls reach the receiver through its place.
- **No extern work:** arrays can't cross the C boundary (§8), so decision 23 needs nothing.
- **C:** `lugha_rt_array_grow` masks the flag defensively.

**Why the changes:** the `swap` and iterator orderings were real hazards found by probes (a copy made between computing an address and writing it); the eager compiler's output is the oracle for every m55 golden.

**Cost:** a flag test per array on each write's path slows write-only loops (histogram, matmul) by up to ~50%; owning once per loop is in TODO.

### 108. Owning arrays once per loop planned (session 118, approved session 119)

**Decision (draft decisions 25–28, PRP-085):**
- **Hoisting:** before a loop, codegen owns each array the loop writes through, when its root passes a strict syntactic whitelist (written places, and reads holding no array); writes inside skip the flag test.
- **Scope:** every loop kind, every nesting level. A path is fixed when its indexes are literals or variables the loop never assigns or redeclares, with no calls. An out-of-bounds hoist index skips the hoist and never panics.
- **Unchanged:** output, panics, copy sites, `own` outside loops. Milestone 56; timings reported, not gated.

**Why:** M55's per-write test slowed write-only loops (histogram +55%, matmul +30%, no vectorization). A syntactic rule can't be fooled by calls, because any call mentioning the root turns hoisting off.

### 109. Owning arrays once per loop — milestone 56 (PRP-085)

**Decision:** built as planned (MEMORY 108):
- **Files:** `codegen/hoist_scan.rs` scans a loop's heads and body (roots that may share, changed and bound names, written arrays); `codegen/hoist.rs` keeps fixed paths, emits guarded hoists before each loop kind, and `hoisted` matches paths by root slot and shape.
- **Writes:** `places.rs::own_place` skips `own` for hoisted paths (`walk`, `local_address`, `growth.rs`).
- **Refinements:** methods on array-free values are fine; `push` of a deep value disqualifies; aliases map to their array's root.

**Why it is safe:** at hoist time every array on the path is made unique; the per-root whitelist means nothing under the root is marked during the loop, and a clone (which re-marks inner arrays) needs a shared array.

**Result:** write loops are back near eager-copy speed (matmul 131 eager, 173 M55, 133 M56 ms).

### 110. Nested `mut self` calls planned (session 120, approved session 122)

**Decision (draft decisions 29–32, PRP-086):**
- **E0508:** only for an argument that can be the receiver's storage, i.e. a place rooted at the receiver (through `if`/block/`match` tails) of a pointer-passed type.
- **Order:** a `mut self` call runs like an assignment: receiver indexes, arguments, then the receiver is reached.
- **E0510:** for receivers only when an index lies between the change and the receiver.
- **Success:** milestone 57, plus firefly's `layer` rewritten.

**Why:** tape-style model code needed one `let` per op; the plan/commit places from M55 make re-reaching the receiver after the arguments safe.

### 111. Nested `mut self` calls — milestone 57 (PRP-086)

**Decision:** built as planned (MEMORY 110).
- **E0508:** `method_calls.rs::no_alias` walks each argument's value tails (`if`/`if let`/block/`match`) and reports a tail rooted at the receiver whose type is passed by pointer (`by_pointer`: arrays, structs, enums, `Option`, interfaces, type parameters).
- **E0510:** `conflicts.rs` skips a change for a `Receiver` target unless `Path::indexed_below(change depth)`.
- **Codegen:** `methods.rs` and `dispatch.rs` plan the receiver, evaluate the arguments, then commit; `place_address` is removed.

**Why:** the M55 plan/commit split made re-reaching the receiver after its arguments safe; only an index between a change and the receiver can invalidate a planned bounds check.

### 112. Numbers as `T` planned (session 121, approved session 122)

**Decision (draft decisions 33–36, PRP-087):**
- **Casts:** `as` with type parameters bounded by `Num`/`Float`, both ways, by §4's rules per instance.
- **Literals:** any integer literal as `T: Num`, any float literal as `T: Float`; E0402 per instance, naming it.
- **Success:** milestone 58, plus the glowworm/firefly cleanup.

**Why:** generic numeric code counted up by `1` to make numbers; casts reuse the existing per-instance lowering.

### 113. Numbers as `T` — milestone 58 (PRP-087)

**Decision:** built as planned (MEMORY 112).
- **Casts:** `check/ops.rs::numeric` treats a `Num`/`Float`-bounded parameter as numeric; codegen needed no change (casts and literals already lower at each instance's concrete type).
- **Literals:** `literal.rs` types any integer literal as `T: Num` and any float literal as `T: Float`, records the integer ones (`ParamLiteral`), and `instance_literals` reports E0402 per (literal, type) after `monomorphize`.
- **Libraries:** glowworm's `arange`/`linspace`/`mean` simplified (`b0683c3`, local); firefly needed nothing.

### 114. User-defined `Ord` planned (session 121, approved session 122)

**Decision (draft decisions 37–40, PRP-088):**
- **`Ord`:** a type with `fun cmp(self, other: Self): Ordering` (a prelude `enum Ordering { Less, Equal, Greater }`, written `Ordering.Less`) is `Ord`; `<` `<=` `>` `>=` call `cmp`.
- **Unchanged:** `==` stays structural, and `Ord` doesn't imply `Eq`.
- **Also:** tuples compare through `cmp`; a prelude `xs.sort()` for `T: Ord`.
- **Success:** milestone 59, tests only.

**Why:** unlocks `min`/`max`/`clamp` and sorting for user types with one method, mirroring M52's operator methods.

### 115. User-defined `Ord` — milestone 59 (PRP-088)

**Decision:** built as planned (MEMORY 114).
- **Checker:** `check/ordering.rs` (`has_cmp`, `bounds_hold`, `order_uses`, `cmp_targets`, `cmp_help`); `bounds.rs::ord_ok` asks `has_cmp`; `ops.rs` records uses and adds help; `monomorphize` lists `cmp_targets` for `cmp` through bounds.
- **Codegen:** `codegen/ordering.rs::user_order` calls `cmp` and maps the tag to −1/0/1 by variant name; `arith.rs` and `tuples.rs::order_of` route user types there.
- **Prelude:** `Ordering` and `sort` at the end of `ops.la`.
- **Printing:** non-generic enums print by key (`$prelude.Ordering`).

**Why:** mirrors M52's operator methods; keying tables by printed names needed a unique name for the prelude's enum.

### 116. Lazy std modules planned (session 125, approved session 126)

**Decision (draft decisions 41–43, PRP-089):** an imported std module's bodies are all checked, but only the functions and methods a program reaches are compiled, as the prelude's are. Reached means called, through bounds or interface tables, used as values, or from reached bodies. Std only; milestone 60, IR tests.

**Why:** std compiled every non-generic body on import (`from_os` in every `std.random` program); the prelude's mechanism already exists.

### 117. Lazy std modules — milestone 60 (PRP-089)

**Decision:** built as planned (MEMORY 116):
- **Checker:** `instances.rs::lazy_key` (prelude or std keys) decides what is listed when reached and what is never a root; `fun_values.rs` records non-generic function values.
- **Codegen:** `declare.rs::is_lazy` skips unlisted prelude and std bodies, and `fun_values.rs::value_target` falls back to `monos`.

**Result:** a `seeded` + `below` program's IR goes from 423 to 314 lines; outputs unchanged.

### 118. Milestones 61–66 planned (sessions 127–128, approved session 129)

**Decision (draft decisions 44–52, PRP-090 to PRP-095):**
- **M61:** one `iter_sum<T: Num>`.
- **M62:** `parse_i64`/`parse_u64` and one UTF-8 decoder in the prelude (`$string.from_bytes`).
- **M63:** format widths in characters (the tables stay Latin-1, §11).
- **M64:** read-only `std.env`.
- **M65:** `std.fs` with no recursive delete.
- **M66:** `std.process`: `posix_spawnp`, no shell, `PATH` lookup, inherited environment, only fds 0–2, UTF-8 output or `Err`, `128 + signal`. The user's approval was the security review.

**Why:** the user asked to finish the v15 TODO in one approval; each item stays its own PRP (scope rule).

### 119. One `iter_sum` — milestone 61 (PRP-090)

**Decision:** built as planned. The prelude has one `iter_sum<T: Num>`, and `check/iter_helpers.rs` maps numeric `sum` to it. Only the `m36/overflow` prelude line moved.

### 120. C → Lugha — milestone 62 (PRP-091)

**Decision:** built as planned, with a closure-free decoder.
- **Prelude:** `utf8_decode(b0..b3, avail)` is shared by `Chars.next` and `text_from_utf8`; `text_parse_i64`/`u64` are the parsers.
- **Checker:** `check::prelude_helper` maps the intrinsics to their helpers and records uses.
- **Codegen:** `prelude_intrinsic` calls the helpers.
- **Built-in:** `$string.from_bytes` → `lugha_rt_str_from_bytes`.
- **Runtime:** `RUNTIME_SYMBOLS` 45.

### 121. Widths in characters — milestone 63 (PRP-092)

**Decision:** built as planned. `format.rs` makes the fill a `char`, `format.c::pad_bytes` counts non-continuation bytes and writes the fill's UTF-8, and only `m46/more` changed.

### 122. `std.env` — milestone 64 (PRP-093)

**Decision:** built as planned.
- **Module:** `src/std/env.la` over `$os.env`/`$os.cwd`, which return bytes checked by `string_from_utf8`; `runtime/env.c`.
- **Built-ins:** `$os.*` now go through a checker table (`os_signature`) and `codegen/os.rs`.
- **Tests:** the runner has `.envvars`.

### 123. `std.fs` — milestone 65 (PRP-094)

**Decision:** built as planned. `src/std/fs.la` sits over `$os.list_dir`, `make_dir`, `remove_file`, `remove_dir` and `stat_kind` (`runtime/fs.c`); names are bytes checked and sorted in Lugha, and `make_dirs` is Lugha.

### 124. `std.process` — milestone 66 (PRP-095)

**Decision:** built as the user security-reviewed it. `src/std/process.la` sits over `$os.spawn`, and `runtime/process.c` does the work:
- **Starting:** `posix_spawnp`, no shell; NUL is refused.
- **Descriptors:** `pipe2(O_CLOEXEC)`, and the runtime's `fopen`s use `"e"`.
- **I/O:** a `poll` loop; `SIGPIPE` is blocked while writing, and a pending one is consumed.
- **Results:** `fflush(NULL)` before spawning; `128 + signal`; output must be UTF-8.

**Rule:** changes to `runtime/process.c` need the user's security review (CLAUDE.md anti-pattern 5).

### 125. Shared boxes `Ref<T>` planned — v16 opens (session 135, approved session 136)

**Decision (v16 draft decisions 1–5, PRP-096):**
- **The box:** `Ref<T>` is the one shared, changeable object. `Ref.new`, `get` (a copy), `set`, `update` and `is` are all the API, with no in-place access.
- **Equality:** not `Eq`.
- **Printing:** `Ref(…)` cuts cycles.
- **Bindings:** `set`/`update` need no `let mut`, so closures can update captured boxes.
- **Version:** v15 is frozen; milestone 67 opens v16.

**Why:** stateful closures and linked data needed sharing. A narrow explicit box keeps value semantics everywhere else, and keeps copy-on-write and loop ownership sound (a box is one holder).

### 126. Shared boxes `Ref<T>` — milestone 67 (PRP-096), v16 begins

**Decision:** built as planned (MEMORY 125).
- **Checker:** `Type::Ref`; `check/refs.rs` (`Ref.new`); the methods in `builtins.rs`; not `Eq` through `eq_ok`/`holds_fun`.
- **Codegen:** `codegen/refs.rs`; printing with a runtime cycle stack.
- **Version:** v15 is frozen, and `lughac spec` prints the v16 spec.

**Why it stays sound:** boxes are pointer copies (`is_deep` false), `get` is a copy site and `set` follows the store rules, so a box is one holder for copy-on-write and loop ownership.

### 127. Changing the working directory planned (session 137, approved session 138)

**Decision (v16 draft decisions 6–8, PRP-097):** `env.set_current_dir(path): Result<i64, string>` (`chdir`), process-wide. Environment variables stay read-only, and `~`, `cd -` and the like are the program's job. Milestone 68, plus oyster's `cd`.

**Why:** a shell's `cd` must move the whole program (redirects, child processes); a per-call `dir` would have left the shell's own paths behind.

### 128. Changing the working directory — milestone 68 (PRP-097)

**Decision:** built as planned. `std.env.set_current_dir` over `$os.chdir` (`runtime/env.c::lugha_rt_chdir`), with `Err` for a NUL byte; oyster's `cd` uses it.

### 129. A VS Code extension (PRP-099; discovery session 139, built session 140)

**Decision:** `editors/vscode/` in this repo, so the grammar changes with the spec. Highlighting plus `lughac check --offline --diagnostics=json` on open and save; no `lughac` change. A file with `main` checks itself; a module checks the `lugha.entries` setting's main files (there is no default entry point), or itself with E0303 hidden. `lugha.path` is machine-scoped. Tests: `node:test` (pure modules, and the runner against the real `lughac`) and `vscode-tmgrammar-test`; a keyword test reads the newest spec's §2. Dev dependencies only; a local `.vsix`, no Marketplace. A language server is a later PRP.

### 130. `lughac init` planned (session 139, approved session 142)

**Decision:** v16 draft decisions 9–12: `lughac init [folder] [--lib] [--name N]`; a program (`src/main.la`) or a library (`src/<name>.la`, `examples/hello.la` importing `<name>.<name>`), both with `[package]` `name`, `version`, `source = "src"`; never overwrites, never prompts, no `git`.

**Why:** starting a project by hand was error-prone, and a library needs something runnable at once. (A concurrent session first recorded this plan as 129, a number already taken, with the approval under its own session number; folded in here.)

### 131. `lughac init` — milestone 69 (PRP-098)

**Decision:** built as planned in `driver/init.rs`, tests in `tests/init.rs`. Errors are exit 2 (`InitError`). The `.gitignore` lists only the executable `lughac build` writes (`/main` or `/hello`); the PRP's `/target` was dropped, as lughac writes no such folder.

### 132. `lughac lsp` planned (session 144, approved session 145)

**Decision:** v16 draft decisions 13–17: a language server in three PRPs (live diagnostics, hover, go-to-definition); `lsp-server` + `lsp-types` + `serde_json`, no async runtime; the extension on `vscode-languageclient` with no fallback to on-save checks; modules checked through entries and mains already seen (no workspace scan); panics caught per check.

**Why:** the user chose to centre work on the VS Code extension (session 144), and live errors were its biggest gap.

### 133. `lughac lsp` — milestone 70 (PRP-100)

**Decision:** built in `driver/lsp/` over `pipeline::front_with` (a `Read` over open documents, no lock written). Checks are offline, 200 ms after typing stops, whole-program. A malformed message ends the server with exit 2 (`lsp-server`'s reader stops), not an error reply. E0325 from git packages lands in `lugha.toml`, as in `lughac check`. Extension 0.2.0: `engines.vscode` `^1.91.0`, Node16 module resolution, a log output channel; not bundled yet.

### 134. Hover planned (session 146, approved session 147)

**Decision:** v16 draft decisions 18–22: hover on expressions and declared names, from the checker's partial results so it works with errors; a Lugha code block plus `instance:` / `converted to` notes; a hover runs a pending check first.

### 135. Hover — milestone 71 (PRP-101)

**Decision:** built as planned. The checker records names (`check/partial.rs`'s `Recorder`) at `env.rs::declare`, function and method declarations, calls (`call_key`, method calls), function values and field declarations; each use maps to its declaration's name span, which go-to-definition can reuse. A function's hover is its header sliced from the source. `check_modules` shares `modules::run` with `check_partial`. **Difference:** while any module fails to parse, there is no hover anywhere (the pipeline stops before checking), not just in that module. The test harness moved to `tests/common/lsp_client.rs`.

### 136. Go to definition planned (session 148, approved session 149)

**Decision:** v16 draft decisions 23–27: definitions from names, types, fields, variants and modules; none for `std`/prelude (no file, no virtual documents); interface calls to the interface's method; same freshness and error tolerance as hover.

### 137. Go to definition — milestone 72 (PRP-102)

**Decision:** built as planned. `Partial.definitions` (use span → `Target::Decl(span)` or `Target::Module(path)`) is recorded where the checker resolves: `expr.rs` local reads (assignments resolve through them too), hover's function hook (declarations point at themselves), `type_key`, `type_named_by`, `interface_type`, `impl` headers, struct literals, field access, constructions, patterns, import paths and module prefixes; field and variant declaration spans live in the `Recorder`. The server shares hover's `fresh`/`snapshot` path.

### 138. CI once per pull request, never waited on (session 150)

**Decision:** `ci.yml` runs on `pull_request` and by hand (`workflow_dispatch`), not on every push; a newer push to a pull request cancels the older run. Merges don't wait for it: local fmt, clippy and `cargo test` (and the extension's `npm test`) are the gate.

**Why:** the user found waiting on CI too slow for rapid development; it stays as a safety net for what only breaks on a clean machine.

### 139. Completion planned (session 151, approved session 152)

**Decision:** v16 draft decisions 28–32: completion after `.` and for plain names, with details, no snippets; from the last check that parsed, trusted while the texts agree; no parser change.

### 140. Completion — milestone 73 (PRP-103)

**Decision:** built as planned. `check/members.rs` (`TypeMembers` from the checker's struct, enum, interface and method tables; built-ins and helpers by filtering one candidate list through `builtins::builtin` and `helpers::helper_function`, so they can't drift; iterator helpers for types with `next`); `Partial.members`, `Partial.prelude`; `check::intrinsics()`. The server keeps `last_good` beside `snapshots` (shared `Rc`). **Refinement of decision 29:** offsets are trusted up to the longest prefix the check's text and the current text share; a receiver past it is looked up by name among locals declared within that prefix, imports and types — this is what makes `p.` on a new line work.

### 141. Signature help planned (session 154, approved session 155)

**Decision:** v16 draft decisions 33–37: every callable; generic calls filled in when the last good check knows the instance (the user's choice over "as written"), the written header as documentation; intrinsics from a table matching spec §5.

### 142. Signature help — milestone 74 (PRP-104)

**Decision:** built as planned. `check/signatures.rs` (`Sig`, recorded in `env.rs::collect` and `methods.rs`, in `Partial.signatures`); `driver/lsp/signature.rs` scans forward from a line start up to 2,000 bytes back with a bracket stack (strings, chars, comments skipped); the receiver logic moved into `completion.rs`'s shared `Here::receiver`. The intrinsic table is tested against `check::intrinsics()` and the embedded spec — it caught three guessed parameter names during the build.

### 143. Partial syntax trees planned (session 156, approved session 157)

**Decision:** v16 draft decisions 38–41: statement- and item-level partial trees (the parser's existing recovery), for the language server only; while a file has a syntax error only syntax errors are shown; the last good check stays as a fallback for lex errors.

### 144. Partial syntax trees — milestone 75 (PRP-105)

**Decision:** built as planned. `parser::parse_partial_from` (`parse_from` wraps it), `LoadEnv.partial` and `parse_module`'s flag, `pipeline::front_partial` checking when every module lexed, `Snapshot.broken` so `last_good` only takes checks of text that parsed. **Found while building:** `with_prelude` placed the prelude right after the last module, overlapping manifests' offsets; harmless in the CLI (it never adds the prelude beside loader errors) but in the server it sent a manifest's E0325 to `<prelude>`. Fixed for the server with `with_prelude_at` past every loaded file. **Signature help** prefers the last good check's filled-in generic when the current check dropped the call being typed. The parser's recovery skips to the next `;`, so a missing `;` also drops the following statement.

### 145. Block comments planned (session 158, approved session 159)

**Decision:** v16 draft decisions 42–46: `/* … */`, nesting (the user's choice), wherever whitespace may be; E0113 for an unclosed one; first of two steps toward `/** … */` doc comments in the language server (milestone 77).

### 146. Block comments — milestone 76 (PRP-106)

**Decision:** built as planned in `lexer/mod.rs::skip_block_comment` (a depth counter, no recursion; nothing special inside but `/*` and `*/`). **Correction found while building:** a line break inside an interpolation is E0102 (the string ends), not E0111 as the draft first said; inside an interpolation the comment stops at its line, so that existing rule applies. VS Code: `comment.block.lugha` (nested) and `blockComment` in the language configuration.

### 147. Doc comments planned (session 160, approved session 161)

**Decision:** v16 draft decisions 47–51: `/** … */` before any named declaration, Markdown with `@param`/`@returns` (JSDoc style, the user's choice), shown in hover, completion and signature help; hover on type and variant names; std and the prelude documented.

### 148. Doc comments — milestone 77 (PRP-107)

**Decision:** built as planned. `lexer::doc_comments` (the lexer's own scan records `/**` spans), `driver/lsp/docs.rs` (parse, attach through whitespace and `fun`/`struct`/… /`public`, a per-snapshot `DocCache`), `Member.decl` for fields, variants and helpers; helper calls now record their name for hover. Library: every public std function and method and every prelude function has a doc (a unit test fails otherwise); implementation `//` notes kept above the docs, since anything between a doc and its declaration detaches it. **Visible side effect:** panic locations inside bundled sources moved down (four `.stderr` expectations updated, same statements).

### 149. Snippets planned (session 162, approved session 163)

**Decision:** v16 draft decisions 52–55: a static snippets file in the extension (declarations and control flow) plus server snippets (call placeholders, postfix `.match`); no quick fix yet.

### 150. Snippets — milestone 78 (PRP-108)

**Decision:** built as planned. `editors/vscode/snippets/lugha.json` (16 snippets; `tests/snippets.rs` expands each with VS Code's rules and runs `lughac check`); `driver/lsp/snippets.rs` (call placeholders from `Sig` names, a prelude helper's receiver skipped; built-ins and variants from their types; the postfix `.match` with a `textEdit` from the receiver's start); the server records `snippetSupport`; `Here.snippets` is off when a `(` follows the cursor.

### 151. The add-missing-arms quick fix planned (session 166, approved session 167)

**Decision:** v16 draft decisions 56–59: on E0417, "Add missing arms" (every missing pattern in full) and "Add a `_` arm", bodies `panic("not handled")` (the user's choices).

### 152. The add-missing-arms quick fix — milestone 79 (PRP-109)

**Decision:** built as planned: `Partial.missing_arms` recorded beside E0417 in `matching.rs` (`show(_, false)`); `driver/lsp/code_actions.rs` places the edit from the source text (a bracket scanner from the scrutinee's end to the closing `}`, skipping strings and nested comments, since the AST has no general walker); answers only from a current, unbroken check. An interface match's only missing case `_` gives just the `_` fix.

### 153. `lughac fmt` planned (session 168, approved session 169)

**Decision:** v16 draft decisions 60–65: a tree-based formatter (like rustfmt); comments attached above or at the end of items, statements, fields, variants and arms, anything else keeps its statement or item verbatim; 4 spaces, 100 columns, no options; the output re-parsed and compared (tree and comments) before anything is written; the editor integration is a separate milestone 81 (the user's choices).

### 154. `lughac fmt` — milestone 80 (PRP-110)

**Decision:** built as planned in `src/formatter/`: `doc.rs` (a hand-written Wadler-style document printer: groups flat when they fit in 100 columns, else broken; a hard line breaks every enclosing group; a list whose last element spans lines hugs its delimiters, `f(a, fun(x) {` … `})`), `items.rs`, `stmts.rs`, `exprs.rs`, `leaves.rs` (types, patterns, parameters), `comments.rs` (sequences: leading, trailing and dangling comments; an element with a comment no sequence placed falls back to its source text, re-indented), `verify.rs`. Literals, names and interpolated strings are copied from their spans (so `0xFF`, `Self` and escapes survive); parentheses come from precedence, plus `(fun(): T)[]`, `(x as T) < y`, struct literals in conditions, and block-likes the user parenthesised as operands. `for (i, w) of` is recognised from the parser's hidden `$item<offset>` binding, and verification ignores those offsets. Blocks, structs and enums are always multi-line; binary chains never break (a known limit). `lexer::comments` is the lexer's own scan. `lughac fmt [paths…] [--check]` (`driver/fmt.rs`): folders sorted, hidden ones and `target/` skipped, symlinks never followed, unchanged files not rewritten, temp file + rename keeping permissions. The repo's own `.la` files are not reformatted (it would move panic lines).

### 155. Formatting in the editor planned (session 170, approved session 171)

**Decision:** v16 draft decisions 66–70: Format Document only (no range or on-type); format-on-save left to the user, the extension only the default Lugha formatter; syntax errors quiet, a verification failure one error message (the user's choices).

### 156. Formatting in the editor — milestone 81 (PRP-111)

**Decision:** built as planned: `driver/lsp/formatting.rs` (`Formatted`: `Edits` / `Unchanged` / `Failed`; the formatter run under `catch_unwind`, so a panic is a `Failed` too; `reply` builds the response and the `window/showMessage`), answered in `mod.rs` before the other requests from the open document's text (`Server::formatting`, no check needed); one whole-document edit, which VS Code reduces to the changed lines. Extension 0.4.0: `configurationDefaults` `[lugha].editor.defaultFormatter = kithinjibrian.lugha`, README "Formatting" and manual step 14; no TypeScript change.

### 157. More quick fixes planned (session 172, approved session 173)

**Decision:** v16 draft decisions 71–75: `let mut` for E0501, the closest variant for W0103 (only within 2 edits or by case, no tie), `import std.<m>;` for E0301; no general did-you-mean (the user's choices).

### 158. More quick fixes — milestone 82 (PRP-112)

**Decision:** built as planned: `Partial.variants` (by an unused binding's span, from `variants_of` the binding's type, `$tuple` constructors dropped) recorded in `matching.rs::unused`; `driver/lsp/fixes.rs` (`fix` by code; the place's root through `partial.definitions`, `after_let` on the text before the binding; Levenshtein `distance`, `closest`; `after_imports` scanning top-level `import` lines to the next `;`); `code_actions.rs` dispatches and shares an `action` builder. Found while testing: at the top of an enum `match` a bare name is a variant (E0415), so W0103 only arises in payload slots (`Some(Emtpy)`). Extension 0.5.0 (README only).

### 159. Find References and Rename planned (session 174, approved session 175)

**Decision:** v16 draft decisions 76–80: References + Rename together; the checked files only, Rename refusing a non-`private` module-level item of a module no checked main imports; Rename self-checked in memory (the user's choices). Go to Implementations later.

### 160. Find References and Rename — milestone 83 (PRP-113)

**Decision:** built as planned: `driver/lsp/references.rs` (`Key` — a declaration by file and bytes, bundled files by their `<…>` name, or a module path — so places compare across checks' offset spaces; `at`, `find` over every snapshot, deduplicated in a `BTreeSet`); `driver/lsp/rename.rs` (`target` with the refusals, `reachable`: a local by `Name::Local`, `private` on the declaration's line, a main file, or another main's check loaded the file; the renamed texts in a cloned `Documents`; `verify` checks every main that loaded a renamed file before and after, comparing error counts per file and `find` of the moved declaration against the moved places). `Server.alone` remembers which mains were checked alone. Intrinsics (`println`) have no declaration: References `null`, Rename "nothing to rename". Extension 0.6.0 (README only).

### 161. `lughac test` planned (sessions 177–178, approved session 179)

**Decision:** v16 draft decisions 81–86: `@test` (the user chose it over `test fun` and a naming convention, and over Rust's `#[test]`, keeping `@` for Python-style decorators later); tests anywhere, paths like `fmt`; `assert`/`assert_eq`/`assert_ne` in any code; each test in its own process, in parallel, 10 s default limit; `--filter`, `--timeout`, output captured and shown on failure.

### 162. `lughac test` — milestone 84 (PRP-114)

**Decision:** built as planned. `@` is the token `At`; `parser/attributes.rs` keeps `@test` as `FunDecl.test: Option<Span>` (not a list: it is the only attribute), E0210 otherwise; inside an `impl` an `@` is a plain syntax error. Checker: `Signature.test`, `test_fns.rs` (E0434 for the shape and for any call or value use, through `call_key`/`function_value`; the assertions as intrinsics, `eq_ok` + printable), `check_tests` (no E0303, `Checked.tests` = the main file's tests). Codegen: tests skipped unless `Checked.tests` is `Some`; `assertions.rs` (branch + `lugha_rt_panic`, values through `part`), `test_main.rs` (a `main` asking `lugha_rt_test_is(name)` per test, else `lugha_rt_test_unknown` → 2), `runtime/test.c`. Driver: `pipeline::front_tests`, `driver/test.rs` (files via `fmt`'s walk, a lex scan for `@test`, one executable per file in a `TempDir`, `thread::scope` workers over an atomic index, reader threads for the pipes, `try_wait` polling and kill on timeout). The old `@` fixtures for E0101 now use a backtick. Defining a function named `assert`/`assert_eq`/`assert_ne` is now E0302 (noted in the spec's compatibility line). VS Code grammar highlights `@test`; extension 0.7.0.

### 163. Tests in the editor planned (session 180, approved session 181)

**Decision:** v16 draft decisions 87–92: VS Code's Testing view with gutter ▶ and inline failures (over "Run test" links); on click only (plus VS Code's continuous run when turned on); nothing at all in untrusted folders; `lughac test --list`/`--json`/`--exact` for the extension (the user's choices; the security note accepted with "build it").

### 164. Tests in the editor — milestone 85 (PRP-115)

**Decision:** built as planned. CLI: `driver/test_json.rs` (hand-written JSON — `serde_json` stays `lughac lsp`'s — via `json::string`; `panic_of` splitting `panic: <message> at <file>:<line>:<col>` from the right; `listed` by lexing and parsing only), `Request.wants` (repeatable `--filter`, `--exact`), `run_all` streaming results to the main thread over an `mpsc` channel so `--json` prints each as it finishes. Extension 0.8.0: `src/tests.ts` (pure: parse lines, build arguments, `outcome` to 0-based places, `\r\n` output), `src/testing.ts` (a `TestController`: items per file from `--list`, refreshed by a debounced `**/*.la` watcher; a default Run profile with continuous run; `spawn` without a shell from the folder; unreported tests `errored` as "doesn't compile"), registered only when the workspace is trusted (else on `onDidGrantWorkspaceTrust`); `workspaceContains:**/*.la` activation. `testing.ts` is the second file importing `vscode`.

### 165. `lughac explain` planned (session 182, approved session 183)

**Decision:** v16 draft decisions 93–97: every code explained with checked examples; a hint line after human diagnostics (the user chose it, knowing every expected `.stderr` changes); code links in the editor opening a read-only tab from the installed `lughac` (the user's choices).

### 166. `lughac explain` — milestone 86 (PRP-116)

**Decision:** built as planned: `docs/errors/<CODE>.md` (99 pages, written in four parallel batches against a fast checker script, then verified by `tests/explain.rs`), `src/explain.rs` (`EXPLANATIONS`, `explain`, `title`), `driver/explain.rs` (`run`, `hint`), the hint in `Output::diagnostics` (human only); `.stderr` fixtures rewritten by inserting the line after the last diagnostic (the diff only adds lines); `codeDescription` `lugha-explain:<CODE>.md` in `publish.rs`; extension 0.9.0 (`explain.ts`: only `^[EW][0-9]{4}\.md$` reaches lughac's arguments; a content provider, registered whatever the trust; the palette command `lugha.explain` as a fallback in case VS Code won't follow a custom-scheme link). Page examples: ```lugha blocks are compiled (`// error: CODE`, `// warning: CODE`, `// also: CODE` markers), ```text blocks are not (multi-file and package codes). **Rule:** a new diagnostic code needs its page, or `tests/explain.rs` fails.

### 167. Linking C libraries planned (session 184, approved session 185)

**Decision:** v16 draft decisions 98–104. The user asked whether Lugha can build GUI apps (not yet: a fixed link line, and no structs, pointers or callbacks across §8) and chose linking any library as the first step: `[link]` in `lugha.toml` (not an attribute or CLI flag); `pkg-config` included, for GTK; every used package's table applies (binding packages); packages may ship C files; fixable link failures are diagnostics, exit 1. Approved with its security review ("yes build").

### 168. Linking C libraries — milestone 87 (PRP-117)

**Decision:** built as planned: `modules/link_table.rs` (`LinkTable`, `LinkLine`, `LinkKey`, `PackageLink`; one-line string arrays; names `[A-Za-z0-9_+.-]`, never `-` first; paths relative without any `..`, stricter than "not leaving the package"), `Loaded.links` (pushed in `read_manifest` when a table lists something, E0322 for missing folders/files; truncated after `check_versions`, whose packages have no module), `src/link/` (`mod.rs` `link_with`, `args`, `LinkError::{CFile, Libraries}` → `diagnostic()`; `inputs.rs` `Plan`; `pkg_config.rs` `run`, `flags`; `c_files.rs` `compile` to `c<n>.o`), `pipeline::build` runs the plan before codegen. `$PKG_CONFIG` names the program (the usual convention; the tests' fake). Diagnostics carry the tool's output in `help` (`Diagnostic` has no notes). **Fix found on the way:** the CLI placed the prelude right after the last module, overlapping manifests' offsets, so a manifest diagnostic made after checking rendered in `<prelude>`; `check_parsed` now uses `with_prelude_at` past every file, like the language server.

### 169. Callbacks and handles planned (session 187, approved session 188)

**Decision:** v16 draft decisions 105–114. The user's choices: named functions and closures, through `user_data` (option 1, as Rust/Swift/Zig/Go; never run-time thunks); a `data` marker callers don't write; closures pinned until exit unless an extern's `release` frees them; opaque handles in the same milestone ("just do it now"). Decided by me and approved with the PRP: no strings into callbacks; capture-free functions only without `data`; the registering thread only; no `unsafe` keyword (the `extern` declaration is the trust point). Renamed externs added because every GTK signal goes through `g_signal_connect_data`.

### 170. Callbacks and handles — milestone 88 (PRP-118)

**Decision:** built as planned, test-first. `Type::Handle(key)` with a `collect_handles` pass before structs; `named_type`/`type_span`/`type_decl`/`is_type_name` know handles, so imports, visibility and the language server follow. Handles may have `impl` methods (the PRP said "no methods"; built-in methods remain none) — codegen picks `Type::Handle` for an `impl` target with no layout. `CParam` plans per extern (`Checked::c_params`); callers' signatures leave out `data`/`release` and show callbacks as `fun(…)` without `data`, so arity and inference are unchanged. E0436: an argument is static when the checker recorded a callee for it, or it is a closure with no captures. Codegen: per-site `lugha_cfn_<n>` + global `lugha_cbv_<n>` (no `data`); `lugha_cb_<signature>` trampolines load the `{ code, env }` pair straight from the pin (no GEP, so no `unsafe`); renamed externs sharing a symbol call through their own `FunctionType` (`Signature::c_type`) with call-site extensions. Byte-identical IR checked for all 185 compiling `tests/programs`. GTK 4 counter built against the installed `.so.N` files through `vendor/lib` symlinks; running it is the user's.

### 171. Strings from C — milestone 89 (PRP-119)

**Decision:** discovery for milestones 89–90 and lugha-gtk together (session 189), one approval (session 190), built in order. Strings: C's text copied by `lugha_rt_str_from_c(text, nullable)` (a new fragment `cstr.c` whose `utf8_length` mirrors the prelude's `utf8_decode`; a test checks overlong, surrogate, >U+10FFFF, truncated and 4-byte cases against `chars()`); `NULL` → `None`, or a location-less panic for a plain `string`; `Option<string>` arguments via `c_string` on the payload. Callbacks take strings but never return them (E0409, help says why). `tests/programs/m3/e0409` rewritten to a callback returning a string. Byte-identical IR (185 programs) against a fresh build of `main`.

### 172. C structs by value — milestone 90 (PRP-120)

**Decision:** built as planned. `StructDecl.c_layout`; the checker's `c_structs` set, `c_field` (numbers, `bool`, `char`, handles, extern structs — `Option` of a handle was dropped from the PRP's list: Lugha lays an `Option` out as a tagged enum, not a nullable pointer), E0437 for fields, generics, empty structs. Codegen: `c_abi.rs` classifies like clang (pieces sized by their data; with two pieces the low one widened to 8 bytes; >16 bytes or not enough free registers → memory), `c_calls.rs` builds `CSig` (separate LLVM params per piece, `byval`/`sret` with align 8) and moves values piece by piece through struct GEPs (never past the struct's bytes). Used only when a signature carries a struct, so other IR is byte-identical (185 programs). Callback entries lift every C parameter first, then hand the `data` pointer to the trampoline (an `sret` or split piece shifts its LLVM index).

### 173. lugha-gtk 0.1.0 (PRP-121)

**Decision:** built at `/home/kithinji/Documents/kithinji/lugha-gtk`; published (session 194) as the public `github.com/kithinjibrian/lugha-gtk` with tag `v0.1.0`, and verified as a git package from a fresh program (`gtk = { git = …, version = "0.1" }`, locked commit `e693800`). Package `gtk`, `source = "src"`, `[link] pkg-config = ["gtk4"]`; `raw.la` (public handles `GtkApplication`, `GtkWidget`, `Argv`; externs checked against GTK 4's headers; `connect_app`/`connect_widget` renaming `g_signal_connect_data`), `app.la`, `widgets.la`. Deviation from the PRP: instead of `label.widget()`, every widget satisfies a `Widget` interface (`handle()`), so `Box.column([label, button])` converts implicitly; `margin` is a method on each widget, since an interface's default methods are reachable only through the interface. Lessons: `of` is a keyword; a method can't share a field's name (fields are `ptr`); a parameter can't share an imported module's name. **Bug found and fixed in lughac** (PR #27): `m.H` for another module's `extern type` resolved to an error type silently (`resolve_applied` dropped handles). Verified: both examples build, `test/smoke.la` runs windowless and prints text read back from GTK.

### 174. Green threads — milestone 91 (PRP-122), v17 begins

**Decision (v17 draft decisions 1–13, sessions 195–201):** concurrency without async/await or function colouring, actors to follow. One OS thread (M:1); `spawn call` only (closures cover blocks, Go's `go func(){}()` is `spawn fun(){}()`), arguments evaluated by the spawner; `Task<T>`/plain `Task`, `join` copies the result; like Go, a panic ends the program and `main` returning ends it; channels like Go with `recv(): Option<T>`; deadlock panics; `select` with recv/send/`after`/`default` in this milestone (the user's choice); `Task`/`Chan` predeclared, `sleep`/`yield_now` in `std.task`; 1 MiB stacks with guards. Mine, approved: countdown preemption at function entries and loop heads (no signals, deterministic), blocking I/O blocks all, C on green stacks, E0438 for spawning a change to a copy. v16 frozen; `lughac spec` prints v17. `std.process`'s private `spawn`/`$os.spawn` renamed `start`/`$os.start`. Deviations in PRP-122's BUILT notes (no `probe-stack`, 64 KiB guards; fixed-seed `select` pick).

### 175. Actors — milestone 92 (PRP-123)

**Decision (v17 draft decisions 14–23, sessions 207–214):** an `actor` keyword; value methods wait (call), void ones are queued (cast); built-in `stop()`/`join(): Option<string>`/`is`; a panic ends only its actor (callers panic in turn; `join` reports); share nothing (no `Ref`, function or interface values across the boundary); start only with `spawn`; the name is the handle type outside, the state inside; private fields; unbounded mailbox; no generic actors. Built: the parser splits an actor into a struct and an `impl` marked `actor`; `Type::Actor`; handlers `lugha_actor_<Type>.<method>`; `runtime/actors.c`; all runtime panics through `panic_with`/`panic_end`. A stack overflow in an actor still ends the program (signal handler).


### 176. Struct update syntax — milestone 93 (PRP-124)

**Decision:** built as planned; the first of kivuli's two prerequisites (`../KICKOFF.md`; the kivuli repo's DECISION-001). `StructLiteral.base`; the parser takes `..e` as a `comma_list` entry and reports E0201 unless it is the last, once (a trailing comma after it is accepted). The checker checks `base` first with **no** expected type (a hint made `..5` report E0401 instead of E0440); a matching base's type arguments fix a generic struct's directly, so the fields need no inference; E0413 skipped with a base, W0104 when nothing is left to copy; `literal_visible` already required every field visible. Codegen evaluates the written fields, then `base` with `value_for_store`, and inserts the written fields over it. Liveness treats `base` as a stored value (moved on last use). Actor states can't be a base outside their actor (E0439 already). Byte-identical IR (204 programs). Built alongside the actors session (214); merged `main` (actors) into the branch: conflicts only in lists both sides appended to.


### 177. Variants as functions — milestone 94 (PRP-125)

**Decision:** built as planned; kivuli's second prerequisite. `check/variant_values.rs`: `E.V` with payloads (from `variant_value`) and `Some`/`Ok`/`Err` (from `name`, after locals and functions) give `fun(payloads): E`; a generic enum's arguments come from the expected `fun` type's result *or* parameters (`bind`), so `map(Some)` works; E0412 otherwise. `Checked::variant_values` → codegen `variant_function_value`, wrapper `lugha_ref_<Enum>.<Variant>` (deep payloads copied, as a function stores a parameter). Inference gives variant-shaped arguments the closure hint (`may_be_variant_value`). **Found and fixed:** the hint was built in two substitutions, so a caller's own `T` was erased when the helper also had a `T` — closures inside generic functions failed on `main` (`tests/programs/m94/generic_closures.la`). **Process change (the user, session 217):** the full `cargo test` is optional; the default gate is fmt, clippy, `--lib` and the touched areas' tests (CLAUDE.md TESTING RULE).


### 178. Formatter hugging — milestone 95 (PRP-126)

**Decision:** built as planned (the user: "approved all and build in one go. no full tests"). `doc.rs` gained `Hug(normal, hugged)` (Prettier's conditional group: flat if it fits, else the hugged form if its first line fits, else broken), `Broken` (a group forced broken) and `Huggable` (marks arrays and struct literals); `call_args` builds the two forms for a call whose last argument is huggable. **`fits` now measures what follows a doc with its groups as broken** (Prettier's rule), so a header stays whole while its body breaks — this changed nothing in the corpus beyond hugging. Closures already hugged (a last element with a hard break). kivuli's `view` formats as `column([` … `])`.


### 179. Inference from siblings — milestone 96 (PRP-127)

**Decision:** built as planned. `check/siblings.rs`: `waits(e)` = `defers(e)` or a call (`f(…)`, `m.f(…)`) of a generic function with a type parameter only its result mentions (key found silently: `module_path` + `qualify` + `follow`, never reporting). Used in `infer` (such arguments go with the literals, checked against what siblings fixed), `ops::operands` (the swap for `==`/`!=`), and `assertion` (`assert_eq`/`assert_ne` check the waiting side second). Associated functions (`View.text(…)`) aren't covered (they aren't in `functions`). One expectation moved: `assert_eq(1, "a")` is now E0401 at `1`, as `1 == "a"` is.

### 180. Packages and tests — milestone 97 (PRP-128)

**Decision:** built as planned (v17 draft decisions 43–50). `skip`: `lugha_rt_test_skip` writes `lugha-skip: <reason>` to stderr and exits 77; the runner (`test_json::skipped`) needs both. `lughac check` → `pipeline::front_check`: test-mode check when the main module declares no `main`. `test/`/`examples/`: `Packages.own` (the root manifest's package name) is given to every root module after loading, so resolution is unchanged. Package files: `LoadEnv.as_module` (test/check only) + `layout::module_path`; the main module's path and package are rewritten after its manifest is read, and E0319 is off for it. Tests are listed qualified (`qualify(path, name)`); the runner names them `test_json::module_name(file)` + `bare(test)`. Lock: read beside the manifest, else once beside the main file (`Fetcher.moved` forces a write); `Loaded.lock_folder` is where it is written.

**Consequences:** `lughac check src/main.la` with `import N.x` inside its own package is E0312 (as in any package module); E0303's program case moved to `tests/cli.rs` because the runner checks rejected programs with `check`.

### 181. pkg-config's SSE flags (PRP-129)

**Decision:** the user approved (security decision, session 223) accepting exactly `-mfpmath=sse`, `-msse`, `-msse2` from `pkg-config --cflags` (`X86_64_DEFAULTS`, `Flag::X86_64`), rather than dropping unrecognised `-m…` flags. They restate x86-64 System V defaults; every other `-m…` flag, and these from `--libs`, stay refused (E0329). Found by the kivuli/lugha-gtk session: C shims plus GTK failed.

### 182. Module doc comments — milestone 98 (PRP-130)

**Decision:** a file's first `/** */`, with only whitespace and plain comments before it, is the module's doc when a blank line, `import`/`public import` or the end of the file follows (the user, session 224). `lsp/docs.rs`: `module_doc` (its own small scanner over comments, so partial text works), `item_doc` (= `attached` minus the module doc: a top doc + blank line + item now documents the module, not the item), `module_doc_at`. `lsp/hover.rs`: `Found::Module` from `Target::Module`. std headers converted (version/milestone notes dropped), shifting std panic lines by 2.

### 183. kivuli's second report — fixes (session 226)

**Decision:** bug fixes within milestones 96, 97 and 87, no PRP. (1) `Checker::waits` recurses: a generic call waits when every argument for a parameter mentioning a result type parameter waits; lists and `Some` of waiting things wait; `array_literal` with no expected type lets the first non-waiting element lead when the first element is a waiting call (literal-first lists unchanged). (2) `load` reads the package's manifest under its own name after rewriting the main module, so its listed packages resolve; `layout::module_path` compares absolute paths (`env.cwd`). (3) `read_manifest` adds a `[link]` table once per folder; E0331's advice follows the linker text (`multiple definition` / `cannot find -l` / `undefined reference`).

---

## CURRENT PROJECT STATE

### Fully Working
- **Milestone 98 complete**: PRP-130 module doc comments
- **Milestone 97 complete**: PRP-128 packages and tests
- **Milestone 96 complete**: PRP-127 inference from siblings
- **Milestone 95 complete**: PRP-126 formatter hugging
- **Milestone 94 complete**: PRP-125 variants as function values
- **Milestone 93 complete**: PRP-124 struct update syntax
- **lugha-gtk 0.1.0** (PRP-121), published on GitHub: GTK 4 windows and widgets in Lugha
- **Milestone 90 complete**: PRP-120 C structs by value
- **Milestone 89 complete**: PRP-119 strings from C
- **Milestone 88 complete**: PRP-118 callbacks and handles (`extern type`, `data`/`release`, E0435/E0436)
- **Milestone 87 complete**: PRP-117 linking C libraries (`[link]`, E0329–E0331)
- **Milestone 86 complete**: PRP-116 `lughac explain`, VS Code extension 0.9.0
- **Milestone 85 complete**: PRP-115 tests in the editor, VS Code extension 0.8.0
- **Milestone 84 complete**: PRP-114 `lughac test`, VS Code extension 0.7.0
- **Milestone 83 complete**: PRP-113 Find References and Rename, VS Code extension 0.6.0
- **Milestone 82 complete**: PRP-112 more quick fixes, VS Code extension 0.5.0
- **Milestone 81 complete**: PRP-111 formatting in the editor, VS Code extension 0.4.0
- **Milestone 80 complete**: PRP-110 `lughac fmt`
- **Milestone 79 complete**: PRP-109 the add-missing-arms quick fix
- **Milestone 78 complete**: PRP-108 snippets
- **Milestone 77 complete**: PRP-107 doc comments
- **Milestone 76 complete**: PRP-106 block comments
- **Milestone 75 complete**: PRP-105 partial syntax trees
- **Milestone 74 complete**: PRP-104 signature help
- **Milestone 73 complete**: PRP-103 completion
- **Milestone 72 complete**: PRP-102 go-to-definition
- **Milestone 71 complete**: PRP-101 hover
- **Milestone 70 complete**: PRP-100 `lughac lsp`, VS Code extension 0.2.0
- **Milestone 69 complete**: PRP-098 `lughac init`
- **VS Code extension**: PRP-099, `editors/vscode/`
- **Milestone 68 complete**: PRP-097 changing the working directory
- **Milestone 67 complete — Lugha v16 begins**: PRP-096 shared boxes `Ref<T>`; v15 frozen (milestones 51–66)
- **Milestone 66 complete — the v15 TODO is done**: PRP-095 `std.process`
- **Milestone 65 complete**: PRP-094 `std.fs`
- **Milestone 64 complete**: PRP-093 `std.env`
- **Milestone 63 complete**: PRP-092 widths in characters
- **Milestone 62 complete**: PRP-091 C → Lugha
- **Milestone 61 complete**: PRP-090 one `iter_sum`
- **Milestone 60 complete**: PRP-089 lazy std modules
- **Milestone 59 complete**: PRP-088 user-defined `Ord`
- **Milestone 58 complete**: PRP-087 numbers as `T`
- **Milestone 57 complete**: PRP-086 nested `mut self` calls
- **Milestone 56 complete**: PRP-085 owning arrays once per loop
- **Milestone 55 complete**: PRP-084 copy-on-write arrays
- **Milestone 54 complete**: PRP-083 `std.random`
- **Milestone 53 complete**: PRP-082 the std library and `std.math`
- **Milestone 52 complete**: PRP-081 operator overloading
- **Milestone 51 complete**: PRP-080 package layout; v15 spec begins
- **Milestone 50 complete — Lugha v14 complete**: PRP-079 git packages
- **Milestone 49 complete**: PRP-078 import aliases and whole-module re-exports
- **Milestone 48 complete**: PRP-077 `format`
- **Milestone 47 complete**: PRP-076 tuples
- **Milestone 46 complete**: PRP-075 Unicode `char`
- **Milestone 45 complete**: PRP-074 bytes and indexes; v14 spec begins
- **Milestone 44 complete — Lugha v13 complete**: PRP-073 format specifiers
- **Milestone 43 complete**: PRP-072 more iterator helpers
- **Milestone 42 complete**: PRP-071 `for x of mut e`
- **Milestone 41 complete**: PRP-070 `Self` in declarations; v13 spec begins
- **Milestone 40 complete — Lugha v12 complete**: PRP-069 printing anything
- **Milestone 39 complete**: PRP-068 printing values (`Display`/`show`, built-in forms); v12 spec begins
- **Milestone 38 complete — Lugha v11 complete**: PRP-067 named imports and re-exports
- **Milestone 37 complete**: PRP-066 package versions (`[package]`, requirements, E0324); v11 spec begins
- **Milestone 36 complete — Lugha v10 complete**: PRP-065 iterator helpers, `iter`/`bytes`/`range`, `for b of s`
- **Milestone 35 complete**: PRP-064 `i8`/`i16`/`u16`, checked/saturating/rotating methods, `MIN`/`MAX`, `parse_u64`
- **Milestone 34 complete**: PRP-063 `Self` in `impl` blocks, default methods on concrete types (E0430); v10 spec begins
- **Milestone 33 complete — Lugha v9 complete**: PRP-062 `if let`, `while let`, `let … else`
- **Milestone 32 complete**: PRP-061 `unwrap`, `expect`, `%=`
- **Milestone 31 complete**: PRP-060 or-patterns and `mut` bindings; v9 spec begins
- **Milestone 30 complete — Lugha v8 complete**: PRP-058 loading packages, PRP-059 package visibility
- **Milestone 29 complete — Lugha v7 complete**: PRP-057 integer methods
- **Milestone 28 complete**: PRP-056 bit operators
- **Milestone 27 complete**: PRP-055 `u32`, `u64`, `f32`; v7 spec begins
- **Milestone 26 complete — Lugha v6 complete**: PRP-054 converting between interfaces
- **Milestone 25 complete**: PRP-053 `Self` in interfaces
- **Milestone 24 complete**: PRP-052 iterators
- **Milestone 23 complete**: PRP-050 generic interfaces as bounds, PRP-051 generic interface values
- **Milestone 22 complete — Lugha v5 complete (branch, merge held)**: PRP-049 default methods and `?` conversion
- **Milestone 21 complete (branch, merge held)**: PRP-048 type patterns
- **Milestone 20 complete**: PRP-046 checking and PRP-047 compiling interface values; v5 spec begins
- **Lugha v4.1**: PRP-045 string literals inside interpolations
- **Milestone 19 complete — Lugha v4 complete**: PRP-044 input and output
- **Milestone 18 complete**: PRP-043 text and option helpers
- **Milestone 17 complete**: PRP-042 `Result` and `?`; the v4 spec begins
- **Milestone 16 complete — Lugha v3 complete**: PRP-041 array helpers in a Lugha prelude
- **Milestone 15 complete**: PRP-040 closures
- **Milestone 14 complete**: PRP-039 functions as values
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
- Lugha v10 (nothing planned; see the v9 spec's §11 out-of-scope list)

---

## NEXT SESSION START POINT

**v16: milestones 67–69 complete** (tags `m67` shared boxes, `m68` changing directory, `m69` `lughac init`). Nothing is proposed or in progress.

Candidates (TODO's v16 section): streaming pipes and timeouts for `std.process`, a package entry module (`import glowworm;`), editing a box in place, a byte buffer, file streams; for editors, a language server.

**VS Code extension done** (PRP-099, `editors/vscode/`, session 140; manually checked by the user and `GPL-3.0-only` confirmed, session 141). A language server and Marketplace publishing are later TODO items.

Unpushed local commits, for the user to push when they ask: firefly `ce6f38b`, `a891512`; glowworm `b0683c3`. oyster is uncommitted in the user's repo.

See `TODO.md`.
