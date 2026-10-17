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
- [x] Replace the one-line README.md with a project description (PRP-017)
- [x] Add CI: fmt, clippy, test — installing LLVM and libgc (PRP-017)
- [x] Mirror `.llmignore` as deny rules in `.claude/settings.json` (PRP-017)
- [x] Move `setup.md` to `docs/setup.md` (PRP-017)

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

### Milestone 5 — Heap data ✅
Done when every §10 program passes.
- [x] PRP-013: string operations (`+`, `==`/`!=`, `.len`, bounds-checked `s[i]`); GC allocation landed in PRP-011
- [x] PRP-014: arrays, bounds checks, repeat literals, `for`-`of`, array copies — primes prints 25
- [x] PRP-015: structs and their copies — centroid prints `centroid: 2.0, 1.0`
- [x] PRP-016: `lughac spec` and every §10 program; dead `Unsupported` paths removed — **milestone 5 done**

## v1 milestones — done (v1 complete)

### Milestone 6 — Enums ✅
Done when the shapes and linked-list programs pass and the rejected program reports E0417.
- [x] PRP-019: v1 syntax (lexer, parser, AST, E0207) and the driver gate
- [x] PRP-020: enums, `match`, `Option<T>` (non-recursive) + the v1 spec file (E0309, E0311, E0415–E0417, W0102)
- [x] PRP-021: recursive enums with indirect slots — **milestone 6 done**, tag `m6`

### Milestone 7 — Methods ✅
Done when the methods program passes.
- [x] PRP-022: `impl`, `self`/`mut self`, associated functions, method calls (E0310, E0415, E0508); driver gate removed — **milestone 7 done**

### Milestone 8 — Growable arrays ✅
Done when the stack program passes and `lughac spec` prints the full v1 spec.
- [x] PRP-023: `{ len, cap, elems }` layout, `push`/`pop`/`clear`, `lugha_rt_array_grow`; v1 spec complete, draft retired — **milestone 8 done: v1 complete**

### Milestone 9 — Patterns and equality ✅
Done when the v2 draft's patterns program passes.
- [x] PRP-030: all of v2's syntax (`import`, `pub`, type parameters, `m.T` / `T<args>`, nested patterns, guards, E0208) and the driver gate
- [x] PRP-031: deep `==` on every type (`lugha_eq_*`), string `<` (`lugha_rt_str_cmp`); `Language v2 Specification.md` started (printed by `lughac spec`)
- [x] PRP-032: nested patterns and guards, usefulness-based E0417/W0102, W0103 — **milestone 9 done**, tag `m9`

### Milestone 10 — Modules ✅
Done when the two-module program passes.
- [x] PRP-033: front end — `private`/`public` syntax, loading every imported module (E0312, E0314, E0319), one offset space, file-aware diagnostics and `--emit`, directory test cases
- [x] PRP-034: names across modules, `private` (E0313), `public` (E0317), no leaking (E0318), module symbols (`lugha_fn_a/b/f`), gate removed — **milestone 10 done**, tag `m10`

### Milestone 11 — Generics ✅
Done when the generics program passes and the full v2 spec replaces v1 in `lughac spec`.
- [x] PRP-035: checking — type parameters, `Eq`/`Ord` bounds, inference, `Checked.instances` (E0315, E0316, E0418, E0419, E0412); `lughac check` accepts generics, the gate moves before codegen
- [x] PRP-036: compiling — instance list and E0420 in the checker, monomorphization in codegen (`lugha_fn_max<i64>`), `Option<T>` described as a predeclared generic enum, gate removed, spec folded

### Milestone 12 — Interfaces ✅
Done when the interfaces program passes and the v2 spec is complete.
- [x] PRP-037: `interface` declarations, interfaces as bounds, structural satisfaction (E0421, E0422, E0423); the v2 spec is complete

## v3 (draft approved, session 47 — `docs/specs/Language v3 Draft.md`)

### Milestone 13 — Input and interpolation ✅
Done when the arguments program passes.
- [x] PRP-038: `args()`, `parse_i64`, `parse_f64`, string interpolation (E0111), `lughac run … -- args`; `Language v3 Specification.md` begins

### Milestone 14 — Functions as values ✅
Done when the functions-as-values program passes.
- [x] PRP-039: function types (`(fun(i64): i64)[]` for arrays), named and associated functions as values, calling values (`b.on_click(2)` for fields), no `==` on functions, `lugha_ref_*`

### Milestone 15 — Closures ✅
Done when the closures program passes.
- [x] PRP-040: closure literals, capture by copy (E0511), parameter inference, `lugha_closure_*`, closures in generic code (E0209)

### Milestone 16 — Array helpers ✅
Done when the array-helpers program passes and the v3 spec is complete.
- [x] PRP-041: `map`, `filter`, `fold`, `sort_by`, `any`, `all`, written in the Lugha prelude (`src/prelude.la`) — v3 complete

## v4 (draft approved, session 52 — `docs/specs/Language v4 Draft.md`)

### Milestone 17 — Results and `?` ✅
Done when the results program passes.
- [x] PRP-042: `Result<T, E>` in the prelude, `Ok`/`Err`, the `?` operator (E0424); `Language v4 Specification.md` begins

### Milestone 18 — Text and option helpers ✅
Done when the text-helpers program passes.
- [x] PRP-043: string helpers, `join`, `Option`/`Result` helpers (prelude), built-in `slice`; prelude functions compile only when reached

### Milestone 19 — Input and output ✅
Done when the reading-input program passes and the v4 spec is complete.
- [x] PRP-044: `read_line`, `read_file`, `write_file`, `append_file`, `eprint`/`eprintln`, `exit`; `.stdin` and `.scratch` in the runner — v4 complete
- [x] Fix the draft's §10 reading-input program: `Some(n) => total += n,` isn't valid Lugha (assignment is a statement; it needs `{ total += n; }`). Found in session 53.

### v4.1 — Nested strings ✅
- [x] PRP-045: string literals and nested interpolations inside `{…}`; E0111 keeps only real mistakes

## v5 (draft approved, session 57 — `docs/specs/Language v5 Draft.md`)

### Milestone 20 — Interface values ✅
Done when the interface-values program passes.
- [x] PRP-046: checking — interfaces as types, conversion at an expected interface type, calls, equality, E0425; `build`/`run` gated
- [x] PRP-047: compiling — dispatch tables, boxes, dynamic calls, copies; gate removed; `Language v5 Specification.md` begins; tag `m20`

### Milestone 21 — Type patterns ✅
Done when the type-patterns program passes.
- [x] PRP-048: `name: T` and `_: T` in `match`, exhaustiveness with `_` (E0426) — built while the user was away; merge held for review

### Milestone 22 — Default methods and error conversion ✅
Done when the errors-as-interfaces program passes and the v5 spec is complete.
- [x] PRP-049: interface methods with bodies; `?` converting into an interface error — v5 complete; built while the user was away, merge held for review


## v6 (draft approved, session 63 — `docs/specs/Language v6 Draft.md`)

### Milestone 23 — Generic interfaces ✅
Done when the generic-interfaces program passes.
- [x] PRP-050: `interface I<T>`, bounds with arguments, inference through bounds; generic defaults; `Language v6 Specification.md` begins
- [x] PRP-051: generic interface values (`Source<i64>`), tables `lugha_vt_Count.Source<i64>`, inference into `Source<T>` parameters; tagged `m23`

### Milestone 24 — Iterators ✅
Done when the iterators program passes.
- [x] PRP-052: `Iter<T>` in the prelude; `for … of` over iterators; tagged `m24`

### Milestone 25 — `Self` ✅
Done when the `Self` program passes.
- [x] PRP-053: `Self` in interface signatures and default bodies; bounds-only interfaces (E0427); tagged `m25`

### Milestone 26 — Converting between interfaces ✅
Done when the converting-between-interfaces program passes and the v6 spec is complete.
- [x] PRP-054: `J` to `I` where `J` declares `I`'s items; through `?`; v6 complete; tagged `m26` and `v6`

## v7 (draft approved, session 69 — `docs/specs/Language v7 Draft.md`)

### Milestone 27 — More numbers ✅
Done when the more-numbers program passes.
- [x] PRP-055: `u32`, `u64`, `f32`: literals, arithmetic, casts, printing, externs; `Language v7 Specification.md` begins; tagged `m27`

### Milestone 28 — Bits ✅
Done when the bits program passes.
- [x] PRP-056: `& | ^ ~ << >>`, compound forms, `0b` literals, the shift panic; tagged `m28`

### Milestone 29 — Integer methods ✅
Done when the hashing program passes and the v7 spec is complete.
- [x] PRP-057: `wrapping_add`/`sub`/`mul`, `count_ones`, `leading_zeros`, `trailing_zeros`; v7 complete; tagged `m29` and `v7`

## v8 (draft approved, session 73 — `docs/specs/Language v8 Draft.md`)

### Milestone 30 — Packages ✅
Done when the packages programs pass and the v8 spec is complete.
- [x] PRP-058: `lugha.toml`, package imports and per-package manifests (E0320–E0323); `Language v8 Specification.md` begins
- [x] PRP-059: `public` and package visibility (E0313, E0318; E0317 retired); v8 complete; tagged `m30` and `v8`

## v9 (draft approved, session 76 — `docs/specs/Language v9 Draft.md`)

### Milestone 31 — Better patterns ✅
Done when the or-patterns program passes.
- [x] PRP-060: or-patterns (E0428), `mut` bindings; `Language v9 Specification.md` begins; tagged `m31`

### Milestone 32 — Unwrapping ✅
Done when the unwrapping program passes.
- [x] PRP-061: `unwrap`, `expect`, `%=`; tagged `m32`

### Milestone 33 — Conditional patterns ✅
Done when the conditional-patterns program passes and the v9 spec is complete.
- [x] PRP-062: `if let`, `while let`, `let … else` (E0429); v9 complete; tagged `m33` and `v9`

## v10 (draft approved, session 81 — `docs/specs/Language v10 Draft.md`)

### Milestone 34 — `Self` everywhere ✅
Done when the `Self`-in-`impl` program passes.
- [x] PRP-063: `Self` in `impl` blocks, default methods on concrete types (E0430); `Language v10 Specification.md` begins; tagged `m34`

### Milestone 35 — More integers ✅
Done when the more-integers program passes.
- [x] PRP-064: `i8`, `i16`, `u16`; `checked_*`, `saturating_*`, rotations, `MIN`/`MAX`; `parse_u64`; tagged `m35`

### Milestone 36 — Iterator helpers ✅
Done when the iterator-helpers program passes and the v10 spec is complete.
- [x] PRP-065: `iter`, `bytes`, `range`, `for b of s`; the adapters and consumers; v10 complete; tagged `m36` and `v10`

## v11 (draft approved, session 85 — `docs/specs/Language v11 Draft.md`)

### Milestone 37 — Package versions ✅
Done when the package-versions program passes.
- [x] PRP-066: `[package]` versions, inline entries with requirements, E0324; `Language v11 Specification.md` begins; tagged `m37`

### Milestone 38 — Re-exports ✅
Done when the re-exports program passes and the v11 spec is complete.
- [x] PRP-067: `import m { X }`, `public import m { X }`; v11 complete; tagged `m38` and `v11`

## v12 (draft approved, session 88 — `docs/specs/Language v12 Draft.md`)

### Milestone 39 — Printing values ✅
Done when the printing-values program passes.
- [x] PRP-068: `Display` and `show`, built-in forms, quoting, printing `T`; `Language v12 Specification.md` begins; tagged `m39`

### Milestone 40 — Printing anything ✅
Done when the printing-anything program passes and the v12 spec is complete.
- [x] PRP-069: interface and function values printed directly, `unwrap on Err: <error>`; v12 complete; tagged `m40` and `v12`

## v13 (draft approved, session 91 — `docs/specs/Language v13 Draft.md`)

### Milestone 41 — `Self` in declarations ✅
- [x] PRP-070: `Self` in struct fields and enum payloads; `Language v13 Specification.md` begins; tagged `m41`

### Milestone 42 — `for … of mut` ✅
- [x] PRP-071: iterators advanced in place; array elements as mutable aliases; tagged `m42`

### Milestone 43 — More iterator helpers ✅
- [x] PRP-072: `zip` (`Pair`), `chain`, `take_while`, `skip_while`, `flat_map`, `min`, `max`, `position`; tagged `m43`

### Milestone 44 — Format specifiers ✅
- [x] PRP-073: `"{x:spec}"`, E0431; v13 complete; tagged `m44` and `v13`

## v14 (draft approved, session 96 — `docs/specs/Language v14 Draft.md`)

### Milestone 45 — Bytes and indexes ✅
- [x] PRP-074: binary I/O, `to_bytes`, `string_from_utf8`, `read_stdin_bytes`, any-integer indexes; `Language v14 Specification.md` begins; tagged `m45`

### Milestone 46 — Unicode text ✅
- [x] PRP-075: `char`, `'a'`, `chars()`, `char_count()`, casts, `char_from`, classification and case (E0112); tagged `m46`

### Milestone 47 — Tuples ✅
- [x] PRP-076: tuple types and values, `t.0`, patterns, `let`/`for` destructuring, `zip` → tuples, `Pair` removed (E0433); tagged `m47`

### Milestone 48 — `format` ✅
- [x] PRP-077: template `format(…)` (E0432); tagged `m48`

### Milestone 49 — Import aliases and re-exports ✅
- [x] PRP-078: `as` on modules and items, `public import m [as v];`, `m.v.X`; tagged `m49`

### Milestone 50 — Downloaded packages ✅
- [x] PRP-079 (security design reviewed and approved): git entries, cache, `lugha.lock`, `lughac update`, `--offline` (E0325, E0326); v14 complete; tagged `m50` and `v14`

## Found while working — needs a PRP

- [x] (PRP-024) A Rust panic inside `lughac` exits with 101, but architecture rule 6 says an internal compiler error is exit 2. Seen in session 26: a codegen bug, since fixed, crashed with 101. Fix: a panic hook or `catch_unwind` in `driver::main`.
- [x] (PRP-027, E0510) An element write is lost when evaluating the right-hand side grows the same array: `xs[0] = { xs.push(2); xs.push(3); 9 };` leaves `xs[0]` as 1. The place's address is computed first (spec §5), then growth moves the elements to a new array. `swap(xs[0], ys[{ xs.push(1); 0 }])` behaves the same way. Seen in session 32; needs a spec ruling (re-resolve the place, or reject growth inside a place's own statement).

- [x] (PRP-028) `runtime/lugha_rt.c` grew to 358 lines (PRP-025), over the 300-line rule. Split into fragments joined at build.

- [x] (PRP-029) `check/stmt.rs` (294) and `check/method_calls.rs` (285) were close to 300 lines. Bodies and array built-ins moved to `body.rs` and `builtins.rs`.

## v15 (draft approved, session 105 — `docs/specs/Language v15 Draft.md`)

### Milestone 51 — Package layout ✅
- [x] PRP-080: `[package] name` (E0327) and `source`, the program's manifest from a parent folder and its implicit package, E0328, the `..` path fix; tagged `m51`

### Milestone 52 — Operator overloading ✅
- [x] PRP-081: `+ - * / %` and unary `-` through methods, `Add`…`Neg` and `Num` bounds, `0`/`1` as `T: Num`, `impl` bounds; tagged `m52`

### Milestone 53 — The std library and `std.math` ✅
- [x] PRP-082: bundled `std.<module>` modules, `std` reserved, `$` built-ins, the `Float` bound, `std.math`; tagged `m53`

### Milestone 54 — `std.random` ✅
- [x] PRP-083: xoshiro256\*\* `Rng`, `seeded`/`from_os`, `uniform`, `below`, `between`, `normal`, `chance`, `choice`, `shuffle`; tagged `m54`

## v15 — later milestones, discovery paused (session 103)

Found writing glowworm and firefly (session 115); each needs a discovery and a PRP. In order of value for numeric code:
- [x] **Read without copying.** Done in milestone 55 (PRP-084): copy-on-write arrays.
- [x] **Own an array once per loop** (found in session 117). Done in milestone 56 (PRP-085). Copy-on-write tests a flag on every array along each write's path, which slows loops that only write (`counts[c] += 1`, a matrix multiply's `out[i][j] +=`) by up to about 50% and stops vectorization. When a loop body has no copy site for an array's root, its arrays could be made unique once before the loop. Needs a discovery: which loops, and how to prove no copy happens inside.
- [x] **Nested `mut self` calls whose arguments only read the receiver.** Done in milestone 57 (PRP-086).
  - **Today:** `g.tanh(g.add(a, b))` is E0508, so tape-style code needs one `let` per op.
  - **Proposal:** allow it when the inner call finishes before the outer one changes the receiver, i.e. arguments evaluated first, then the outer `mut self` call. Model code would then read like PyTorch.
  - **Must keep:** E0508's protection against real aliasing.
- [x] **Any number as a `T: Num`.** Done in milestone 58 (PRP-087).
  - **Today:** only `0` and `1` are `T` literals, so generic code counts up by `1` to make `n` (glowworm's `arange` and `linspace`, firefly's mean).
  - **Proposal:** a conversion such as `T.from_i64(n)` / `T.from_f64(x)`, or `n as T` under a numeric bound.

- [x] **Lazy std modules** (found in session 114). Done in milestone 60 (PRP-089): a std module's non-generic functions are compiled whenever it is imported (e.g. `from_os` in every `std.random` program), unlike the prelude, which compiles only what is reached. Make std lazy the same way; no visible change except smaller IR.

- [x] **`it.sum()` cleanup** (session 112). Done in milestone 61 (PRP-090): replace the ten `iter_sum_<type>` copies in `src/prelude/iter.la` and the per-type table in `check/iter_helpers.rs` with one `fun iter_sum<T: Num>(it: Iter<T>): T`. `["a"].iter().sum()` stays E0415; only the IR names change (`iter_sum<i64>`).
- [x] **User-defined `Ord`** (session 112). Done in milestone 59 (PRP-088). The recommended answers below were chosen:
  - a type with `fun cmp(self, other: Self): Ordering` (a prelude `enum Ordering { Less, Equal, Greater }`) gets `<`, `<=`, `>` and `>=` and satisfies `Ord`;
  - `==` stays structural;
  - `Ord` doesn't imply `Eq`.

  This unlocks `iter().min()/max()`, `math.min/max/clamp` and generic sorting for structs.
- [x] **Optional C → Lugha** (done in milestone 62, PRP-091): `parse_i64`/`parse_u64`, and sharing the UTF-8 decoder between `string_from_utf8` and `s.chars()`.

- [x] **Std library first** (session 109; built as milestone 53, `std.math`): bundled Lugha modules imported as `std.<module>` (`import std.math;`), embedded in `lughac` like the prelude and loaded only when imported, so they cost nothing otherwise. `std` is a reserved package name, always available, versioned with the compiler. The prelude stays small; new features go in std modules. `std.math` comes first; the directories/processes/env items below become `std.fs`, `std.process` and `std.env`.
  - **Agreed with the user: no opt-out mechanism** (no `no_std`, no `[package] deny` list). Lazy loading already makes std free when unused; a `deny` list for policy can be reconsidered later.
  - Next step: a discovery, then a draft milestone and PRP.

The user chose this scope; the discovery stopped after its first answer, so v15 starts by resuming it (one question at a time), then a draft spec, then PRPs.

- [x] **Directories:** done in milestone 65 (PRP-094) as `std.fs`.
- [x] **Processes:** done in milestone 66 (PRP-095, security-reviewed by the user's approval) as `std.process`.
  - Open: the API's shape. The proposal not yet answered:
    - `run(cmd, args): Result<Output, string>` with a prelude `Output { code, stdout, stderr }` that captures;
    - `call(cmd, args): Result<i32, string>` that passes output through to the terminal;
    - optionally stdin text.
- [x] **Environment variables:** done in milestone 64 (PRP-093) as `std.env`: `get(name)` and `current_dir()`, read-only.
- [ ] **Unicode follow-ups:**
  - [x] format widths counted in characters, not bytes (done in milestone 63, PRP-092);
  - fuller `char` classification and case tables beyond Latin-1: **ruled out of v15 by the user** (session 127), keeping §11's "full Unicode tables" exclusion.
- File streams (open, read in chunks, write incrementally) were offered and **not** chosen: they wait below.

## v16 (draft approved, session 136 — `docs/specs/Language v16 Draft.md`)

### Milestone 67 — Shared boxes ✅
- [x] PRP-096: `Ref<T>`, `Ref.new`, `get`, `set`, `update`, `is`; not `Eq`; cyclic printing; v15 frozen; tagged `m67`

### Milestone 68 — Changing directory ✅
- [x] PRP-097: `env.set_current_dir(path)` (`$os.chdir`); oyster's `cd`; tagged `m68`

### lugha-gtk 0.1.0 (PRP-121) ✅ published
- [x] `/home/kithinji/Documents/kithinji/lugha-gtk`: app, Window, Box, Button, Label, Entry, closures for signals; examples and a windowless smoke test run against GTK 4.22; tagged `v0.1.0` locally
- [x] Created `github.com/kithinjibrian/lugha-gtk` (public) and pushed `main` and `v0.1.0` (session 194); a fresh program used it as a git package
- [ ] More widgets (CheckButton, Scale, DropDown, ListView), layout (Grid), dialogs, CSS

### Milestone 90 — C structs by value ✅
- [x] PRP-120: `extern struct`, by value to and from C as on x86-64 System V, E0437; tagged `m90`
- [ ] Later: pointers to structs (out-parameters), arrays of structs to C, unions, other ABIs (AArch64)

### Milestone 89 — Strings from C ✅
- [x] PRP-119: `string`/`Option<string>` extern results and callback parameters, `Option<string>` arguments, U+FFFD; tagged `m89`

### Milestone 88 — Callbacks and handles ✅
- [x] PRP-118: `extern type`, callbacks with `data`/`release`, pinned closures, `extern "symbol" fun`, E0435/E0436; tagged `m88`
- [x] C structs by value (milestone 90) (raylib's `Color`), callbacks without user data for closures (run-time thunks, deliberately not done), a GTK binding package

### Milestone 87 — Linking C libraries ✅
- [x] PRP-117: `[link]` in `lugha.toml` (`libs`, `paths`, `include`, `c`, `pkg-config`), every used package's table, E0329–E0331; tagged `m87`
- [x] Toward GUI programs: opaque handles and callbacks (milestone 88)

### Milestone 86 — `lughac explain` ✅
- [x] PRP-116: a page per code with checked examples, the hint after diagnostics, editor links; tagged `m86`

### Milestone 85 — Tests in the editor ✅
- [x] PRP-115: `lughac test --list`/`--json`/`--exact`; the VS Code Testing view; tagged `m85`

### Milestone 84 — `lughac test` ✅
- [x] PRP-114: `@test`, `assert`/`assert_eq`/`assert_ne`, each test in its own process, in parallel, `--filter`, `--timeout`; tagged `m84`

### Milestone 83 — Find References and Rename ✅
- [x] PRP-113: references across the checked files; rename refused unless safe, self-checked; tagged `m83`

### Milestone 82 — More quick fixes ✅
- [x] PRP-112: `let mut` (E0501), the closest variant (W0103), a std import (E0301); tagged `m82`

### Milestone 81 — Formatting in the editor ✅
- [x] PRP-111: Format Document in `lughac lsp`; the extension the default Lugha formatter (0.4.0); tagged `m81`

### Milestone 80 — `lughac fmt` ✅
- [x] PRP-110: the tree-based formatter, comments kept, verified output, `--check`; tagged `m80`

### Milestone 79 — Add missing arms ✅
- [x] PRP-109: quick fixes on E0417 (every missing arm, or `_`); tagged `m79`

### Milestone 78 — Snippets ✅
- [x] PRP-108: static snippets (checked by `lughac`), call placeholders, postfix `.match`; tagged `m78`

### Milestone 77 — Doc comments ✅
- [x] PRP-107: `/** */` with `@param`/`@returns` in hover, completion and signature help; std and the prelude documented; tagged `m77`
- [ ] Later: a `lughac doc` tool (HTML from doc comments)

### Milestone 76 — Block comments ✅
- [x] PRP-106: `/* … */`, nested, E0113; highlighted and toggled in VS Code; tagged `m76`
- [x] `/** … */` doc comments in the language server — milestone 77, PRP-107

### Milestone 75 — Partial syntax trees ✅
- [x] PRP-105: the language server checks the parser's recovered program; only syntax errors shown meanwhile; tagged `m75`

### Milestone 74 — Signature help ✅
- [x] PRP-104: the call being typed and its active parameter; generics filled in when known; tagged `m74`

### Milestone 73 — Completion ✅
- [x] PRP-103: members after `.` and names in scope, from the last check that parsed; tagged `m73`

### Milestone 72 — Go to definition ✅
- [x] PRP-102: definitions from names, types, fields, variants and modules; tagged `m72`

### Milestone 71 — Hover ✅
- [x] PRP-101: hover over expressions and declared names, also with errors; `check_partial`; tagged `m71`

### Milestone 70 — `lughac lsp` ✅
- [x] PRP-100: a language server with live diagnostics; the VS Code extension 0.2.0 uses it; tagged `m70`

### Milestone 69 — `lughac init` ✅
- [x] PRP-098: program and library layouts, the name from the folder or `--name`, never overwrites; tagged `m69`

Open (v16 §12): editing a box in place (`r.with`), a bounds-checked byte buffer. Found with oyster: ~~changing the working directory (`cd`)~~ done in milestone 68 (PRP-097), streaming pipes and timeouts for `std.process`, and ~~a package entry module (`import glowworm;`)~~ done in milestone 99 (PRP-131).


## v17 (draft approved, session 201 — `docs/specs/Language v17 Draft.md`): concurrency

### Milestone 91 — Green threads ✅
- [x] (PRP-122) `spawn call` → `Task<T>`/`Task`, `join`; `Chan<T>` (`new`, `buffered`, `send`, `recv`, `try_recv`, `close`, `is`); `select` with `recv`/`send`/`after`/`default`; `std.task` (`sleep`, `yield_now`); one OS thread, preemption by countdown, deadlock panic; E0211, E0438.

### Next
### Milestone 92 — Actors ✅
- [x] (PRP-123) `actor` declarations, handles, call/cast, `stop`/`join`/`is`, panics ended in their actor, share-nothing; E0439; VS Code 0.12.0.

### Milestone 93 — Struct update syntax ✅
- [x] (PRP-124) `S { f: e, ..base }`, Rust's rules; E0440, W0104; for kivuli

### Milestone 101 — Maps and sets ✅
- [x] (PRP-133) `Map<K, V>`, `Set<T>`, `#{ … }` literals, E0441/E0442/W0105, `Mapped`; tagged `m101`
- [x] Diagnostics print prelude types as `$prelude.Map<…>`/`$prelude.ArrayIter<…>`: show them without the prefix (found in milestone 101; done in session 245)

### Milestone 100 — Glob re-exports ✅
- [x] (PRP-132) `public import m { * };`; E0201/E0314; completion of re-exports; tagged `m100`

### Milestone 99 — Package entry modules ✅
- [x] (PRP-131) `import N;` imports the package's `lib.la`; `init --lib` writes one; tagged `m99`

### Milestone 98 — Module doc comments ✅
- [x] (PRP-130) a file's top `/** */` documents its module; hover on module names; std documented; tagged `m98`

### Milestone 97 — Packages and tests ✅
- [x] (PRP-128) package files as modules under `test`/`check`, `test/` inside the package, `check` without `main`, one lock per package, `skip(reason)`; tagged `m97`

### Milestone 96 — Inference from siblings ✅
- [x] (PRP-127) result-only generic calls typed by their siblings; tagged `m96`

### Milestone 95 — Formatter hugging ✅
- [x] (PRP-126) trailing arrays and struct literals hug; headers kept; tagged `m95`

### Milestone 94 — Variants as functions ✅
- [x] (PRP-125) `Msg.Named`, `Some`, `Ok`, `Err` as function values; tagged `m94`. kivuli's prerequisites are done.
- [ ] `None` in a closure's result inside a generic function, with nothing fixing its type, is still E0412 (found while testing milestone 94)

### Later
- [ ] Built-in supervisors (restart policies), links/monitors; generic actors; actors as interface values; bounded mailboxes; actors on several OS threads; a stack overflow inside an actor ending only that actor.
- [x] **VS Code:** highlighting for `spawn`/`select` (and `after`/`default` in arms), snippets `spawn`, `chan`, `select`; extension 0.11.0 (session 205).
- [ ] Non-blocking input and output (a waiting read pauses only its green thread); green threads on several OS threads.
- [ ] `probe-stack` on functions of programs that spawn, so a frame over 64 KiB can't skip a green stack's guard (PRP-122 BUILT notes).

## Editor support

- [x] **VS Code extension** — highlighting and `lughac check` diagnostics on open/save (PRP-099, `editors/vscode/`).
- [x] **A language server, live diagnostics** — `lughac lsp`, the extension on `vscode-languageclient` (milestone 70, PRP-100).
- [x] **Hover types** — milestone 71, PRP-101.
- [x] **Go to Definition** — milestone 72, PRP-102.
- [x] **Completion** — milestone 73, PRP-103.
- [x] **Partial syntax trees** (statements and items) — milestone 75, PRP-105.
- [ ] **Expression error nodes** — keep a broken statement with a hole, so `foo(p).` on the broken line completes too.
- [ ] **Lexer recovery** — tokens up to an unclosed string, so a fresh `"` without auto-closing doesn't lose the tree.
- [x] **Signature help** — milestone 74, PRP-104.
- [x] **Snippets** — milestone 78, PRP-108.
- [x] **Quick fix: add missing match arms** — milestone 79, PRP-109.
- [x] **`lughac fmt`** — milestone 80, PRP-110.
- [x] **Format Document and format on save** — milestone 81, PRP-111.
- [ ] Format Selection (the statements or items a selection touches).
- [ ] Formatter: break long binary chains and method chains; `fmt --check` in CI once the repo's own `.la` files are formatted (moves panic lines).
- [x] More quick fixes — milestone 82, PRP-112 (`let mut`, a misspelled variant, a std import).
- [ ] Later quick fixes: did-you-mean for any undefined name (E0301), an import for a user module.
- [x] **Find References** and **Rename** — milestone 83, PRP-113.
- [ ] **Go to Implementations** — from an interface method to every type's.
- [ ] **A read-only view of bundled sources** (`std`, the prelude) for hover and definitions that land there.
- [x] **Go-to-definition** — milestone 72, PRP-102.
- [ ] **Bundling** the extension (vsce warns about 231 files; needs a bundler such as esbuild as a dev dependency).
- [ ] **Publishing** the extension to the Marketplace and Open VSX (publisher accounts, a token as a CI secret).
- [x] **Run tests from the editor** — milestone 85, PRP-115 (the Testing view, not code lenses).
- [ ] Tests in the editor, later: a Debug profile (needs debug info), test items from unsaved text, UTF-16 columns from `lughac test --list`.
- [ ] **Outline and breadcrumbs** (`textDocument/documentSymbol`) — functions, structs, enums, interfaces, `impl` methods, fields and variants in the Outline panel and breadcrumbs; `Ctrl+Shift+O`.
- [ ] **Go to Symbol in Workspace** (`workspace/symbol`, `Ctrl+T`) — functions and types by name across the checked files.
- [ ] **Highlight uses** (`textDocument/documentHighlight`) — the other uses of the name under the cursor in the file, from References' search.
- [ ] **Inlay hints** (`textDocument/inlayHint`) — a `let`'s inferred type (`: i64`) and parameter names at calls, greyed out.
- [ ] **Semantic highlighting** (`textDocument/semanticTokens`) — colours from the checker: parameters, locals, fields, types, variants, functions, methods.
- [ ] **Run from the editor** — a "▶ Run" code lens above `fun main()` (or a task) running `lughac run`; needs a security review (it starts programs).
- [ ] **Debugging** — breakpoints and stepping through a Debug Adapter; needs debug info (DWARF) from codegen first. Large.
- [ ] **Call hierarchy** (`callHierarchy/*`) — a function's callers and callees.
- [ ] **Go to Type Definition** (`textDocument/typeDefinition`) — from a value to its struct, enum or interface.
- [ ] **Smart expand selection** (`textDocument/selectionRange`, `Shift+Alt+→`) — expression → statement → block → item.
- [ ] **Folding by syntax** (`textDocument/foldingRange`) — `match` arms, `impl` blocks, imports and doc comments, beyond indentation.

## After v14 — noted, not scheduled (session 103)

v15 takes system access and the Unicode follow-ups; everything else left at v14's close waits here. Each needs a discovery and an approved spec change.

- [x] **Shared references and closures that change their captures.** Done in v16 as shared boxes, `Ref<T>` (milestone 67, PRP-096); editing a box in place is a v16 open question. The biggest remaining change to the memory model; it would also let `xs.iter()` borrow instead of copying. A version of its own.
- [ ] **A separate debug form, and parsing values back from text.** Also: module paths in printed type names, so two modules' `Point`s print differently.
- [ ] **One-element tuples.**
- [ ] **Packages, more:**
  - a registry;
  - `ssh` URLs;
  - branches or commits as requirements;
  - two versions of one package in a build;
  - full version ranges and pre-release versions.
- [ ] **Imports, more:** re-export chains (`m.v.w.X`), glob imports, a `--package` flag, and package-level (not `public`) re-exports.
- [ ] **Explicit type arguments and `.len()`.**
- [ ] **File streams:** open a file, read it line by line or in chunks, write incrementally, close (not chosen for v15).
- [ ] **Format specifiers chosen at run time.**
- [ ] **Listing a git package's `extern` functions at `lughac update`.** Needs a security review, like PRP-079.
- [ ] **Checking the program's own `[package]` version,** e.g. against a minimum `lughac` version.
- [ ] **Interfaces implemented once per type argument (`From<T>`).** Needs overloading by type.
- [ ] **Literal patterns on `f64`.**
- [ ] **Warnings:** an irrefutable `if let`, and a `show` of the wrong shape.
- [ ] **Small values stored inline in interface values** (an optimisation only).
- [ ] **Spec housekeeping:** tick §12 entries that later versions settled: `%=` and `mut` bindings (v9), re-exports (v11), iterating a copy / `for … of mut` (v13), Unicode text (v14).

## Ideas — noted, not scheduled

- **Decorators** (after milestone 84): user-written `@name` attributes that wrap a function, as Python's do (`@logged`, `@memo`); `@` is already reserved for them. Needs a way to wrap any signature (variadic generics or compile-time rewriting). Also `@test(skip)`-style arguments.

- [x] Move on last use: store a dead local's array without copying it (`grid = next`, `ps[i] = p`) — PRP-018.
- [x] (PRP-025) A `panic: stack overflow` instead of a raw signal (exit 139) for too-deep recursion: an alternate signal stack and a SIGSEGV handler in the runtime (PRP-021 discovery).
- [x] (PRP-026) v1 language question: a swap or move operation for buffers (`ys = buf` while `buf` stays in use is a full copy). See DECISION-010. Done as the `swap(a, b)` intrinsic, Lugha v1.1.
