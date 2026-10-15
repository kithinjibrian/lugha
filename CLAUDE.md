# CLAUDE.md — lugha

Behavioral instructions for AI coding assistants. Each rule exists to prevent a specific mistake.

---

## SESSION HANDOFF RULE — NON-NEGOTIABLE

Every session in `CONTEXT.md` must have:
- A **name** — short descriptive title of what the session accomplished
- A **state** — `open` while work is in progress, `closed` once handoff is done
- A **branch** — the git branch this session's work lives on

Format:

    ## SESSION {n} — {YYYY-MM-DD} — {Name} — {state}
    Branch: {branch-name}

Rules:
- **Step 1 of every session, no exceptions:** append a new session entry to `CONTEXT.md` with state `open` and the current branch name. Do this before reading any other file, before planning, before writing any code. Commit it before any other work begins.
- Mark it `closed` only after CONTEXT.md is updated, committed and pushed.
- Never leave a session `open` at the end of a turn.
- Never start a new session without closing the previous one first.
- The NEXT SESSION START POINT block is rewritten at the end of every session.
- Sessions are never deleted — the full history stays in CONTEXT.md.

---

## PRP RULE — NON-NEGOTIABLE

Never write code for a new feature without a PRP file in `/PRPs`.

If a feature request is given without a PRP:
1. Do not write any code.
2. Run a discovery interview (`PRPs/DISCOVERY.md`) — one question at a time.
3. Write the PRP to `/PRPs/prp-{NNN}-{feature_name}.md` from `PRPs/TEMPLATE.md`.
   - `NNN` is the next unused number, zero-padded to three digits. Numbers are never reused or renumbered.
   - `feature_name` is lowercase snake_case: `prp-001-lexer.md`, `prp-002-type_checker.md`.
4. Present it to the user and build only after explicit approval.

A vague prompt is not a starting point. It is the beginning of a discovery.

---

## SCOPE RULE — NON-NEGOTIABLE

One PRP at a time. Never implement more than one feature's scope in a single session.

If mid-implementation the scope turns out larger than the PRP described: stop, document what was discovered, update or create a PRP, and get approval before continuing. The human decides what is "small enough to add", not the model.

---

## TESTING RULE — NON-NEGOTIABLE

Write the test before writing the implementation. No exceptions.

- For every new function, write a failing test first, then the minimum code to make it pass.
- Unit tests live in a `#[cfg(test)] mod tests` block at the bottom of the file they test.
- Integration tests (public API only) live in `tests/`, one file per area: `src/lexer.rs` → `tests/lexer.rs`.
- End-to-end tests are files in `tests/programs/`: `<name>.la` plus `<name>.stdout`, `<name>.exit`, and `<name>.stderr` for rejected programs. Add a test by adding files — never by editing the runner.
- Test behavior visible to callers — inputs, outputs, and every error variant. Do not test private internals or third-party crate behavior.
- Every new `pub` function gets at least one happy-path test and one test per error variant it can return.
- Run the full suite (`cargo test`) after any non-trivial change before calling the task done.

If the PRP does not describe what to test, add the test cases to the PRP before writing code.

---

## SECURITY RULE — NON-NEGOTIABLE

Never implement without explicit human review and approval:
- Authentication, authorization, or session management
- Cryptographic operations (hashing, signing, encrypting)
- Secrets or credential handling
- Any `unsafe` block

For everything else:
- Never hardcode credentials, API keys, tokens, or secrets — not even in comments or examples.
- Never trust external input (files, CLI args, network, env) without validating it first.
- Never log sensitive data.
- Never add a dependency without stating why in the PRP — every crate is attack surface.

If unsure whether something has a security implication, stop and ask.

---

## CODE DOCUMENTATION RULE — NON-NEGOTIABLE

Read `docs/CODE_STYLE.md` before writing any function, type, or module.

Every `pub` item has a `///` doc comment. Every non-obvious decision inside a function has a `//` comment explaining *why*, not what.

---

## PENDING DECISIONS RULE — NON-NEGOTIABLE

Before writing code that depends on an unresolved architectural question, check `DECISIONS.md`.

- `open` → stop. Do not implement. Ask the user to resolve it.
- `resolved` → follow the recorded outcome. Do not re-litigate.
- New unresolved question mid-implementation → add it to `DECISIONS.md` as `open` and stop. Do not guess.

Never make an architectural choice silently.

---

## FILE SIZE RULE

No file exceeds 500 lines (including tests). (Raised from 300 by the user in session 107.)

When a file reaches the limit: stop, propose a specific split (new file names and what moves where), wait for approval, split, then continue.

---

## PROTECTED FILES — NON-NEGOTIABLE

Never read, modify, or delete:

- `.env` and `.env.*`
- `Cargo.lock` — never hand-edit; it changes only as a side effect of cargo commands
- `target/` — build output
- `LICENSE` — GPL-3.0, changed only by the human

If a task seems to require touching a protected file, stop and ask. See also `.llmignore`. `.claude/settings.json` denies Claude's Read and Edit tools on the same paths. Keep all three in sync. The deny rules block only Claude's own tools, not a sandbox: cargo still writes `Cargo.lock` and `target/`.

---

## COMMANDS

```bash
cargo build                                   # build
cargo test                                    # all tests
cargo fmt --check                             # formatting
cargo clippy --all-targets -- -D warnings     # lint

cargo run -q -- build prog.la [-o out] [--emit=tokens|ast|ir] [-O0|-O2] [--diagnostics=human|json]
cargo run -q -- run prog.la                   # exit code = the program's
cargo run -q -- check prog.la [--offline]     # lex, parse, type-check
cargo run -q -- update prog.la                # re-resolve git packages, rewrite lugha.lock
cargo run -q -- spec                          # print the bundled language spec
```

Run fmt, clippy and tests after every non-trivial change. A task is not done until all pass.

> End-to-end tests also need `llvm-21-dev`, `cc`, and `libgc-dev` installed.

---

## STACK

- Rust 1.99.0 (pinned in `rust-toolchain.toml`), edition 2024 — package `lugha`, binary `lughac`.
- `inkwell` 0.10, feature `llvm21-1-prefer-dynamic` + LLVM 21 (`llvm-21-dev`) — codegen. Never use another LLVM major; never switch to static LLVM linking (fails on Ubuntu, see DECISION-005).
- C (`lugha_rt.c`, kept as three fragments in `runtime/`) — the runtime linked into every compiled program.
- Boehm GC (`libgc`), `libc`, `libm` — linked into every compiled program via the system `cc`.
- `codespan-reporting` (ASCII chars) — human diagnostics. `clap` (derive) — CLI. `thiserror` — internal errors.
- License: GPL-3.0 — every dependency must be GPL-3.0-compatible.

The language itself is defined by `docs/specs/Language v16 Specification.md` (`lughac spec` prints it): Lugha v16, in progress (milestones 67–68 done: `env.set_current_dir`; shared boxes, `Ref<T>` — `Ref.new`, `get`, `set`, `update`, `is`; not `Eq`; cyclic printing). `Language v16 Draft.md` is the approved plan. v15 is complete (milestones 51–66: package layout, operator overloading, the std library `std.math`/`random`/`env`/`fs`/`process`, copy-on-write arrays and loop ownership, nested `mut self` calls, numbers as `T`, user-defined `Ord`, widths in characters, C → Lugha parsers and UTF-8) and frozen, like v0–v14. `Result`, `Iter`, `Display`, `Ordering`, the iterator types and the array helpers are written in Lugha in `src/prelude/` (`core.la`, `iter.la`, `more.la`, `text.la`, `ops.la`), loaded with every program. Git packages need the system `git` at build time (PRP-079); it runs only through `modules/git.rs`.

---

## ARCHITECTURE RULES

1. **The spec is the source of truth.** Read the relevant section of `docs/specs/Language v2 Specification.md` (and, for features not yet landed, `Language v2 Draft.md`) before implementing any language feature, and cite the section in the PRP. If code and spec disagree, the code is wrong. If the spec looks wrong, stop and propose a spec change — never edit the spec without explicit approval. *Why:* `lughac spec` ships the spec as the language reference; drift makes it lie.
2. **Stages only depend backwards.** lexer → parser → modules → check → codegen → driver. A stage never imports a later one. *Why:* `lughac check` and `--emit=tokens|ast|ir` must run the pipeline partway.
3. **Every token and AST node carries a byte-offset `Span`.** *Why:* every diagnostic needs a location, and spans can't be recovered later.
4. **Codegen never infers types.** It reads the checker's expression → type side table. *Why:* opaque pointers need the element type on every load/store/GEP (spec §9), and two type computations will disagree.
5. **User-program errors are `Diagnostic` records, never panics or strings.** Each has a stable code from its stage's range (E01xx lex … E05xx, W01xx). New codes take the next unused number; a code is never reused or renumbered. Human and JSON output render from the same record. *Why:* spec §9; codes are a public contract.
6. **Exit codes are 0 / 1 / 2.** 1 = the user's program has errors; 2 = bad CLI usage or internal compiler error. A `module.verify()` failure or a Rust panic inside lughac is exit 2, never 1. *Why:* spec §9; tools tell "your bug" from "our bug" by it.
7. **Symbol prefixes are fixed.** Lugha functions → `lugha_fn_<name>`, methods → `lugha_fn_<Type>.<name>`, items of a module other than the main file → the module path joined with `/` first (`lugha_fn_shapes/circle/area`, `lugha_fn_shapes/circle/Circle.area`), generic instances → the type arguments as they print, module paths with `/` (`lugha_fn_max<i64>`, `lugha_fn_Stack<string>.push`, `lugha_fn_a/b/max<a/b/Point>`), functions used as values → wrappers `lugha_ref_<symbol>` (v3), closure bodies → `lugha_closure_<enclosing>.<n>` (v3), interface dispatch tables → `lugha_vt_<Type>.<Interface>` (a generic interface with its arguments, `lugha_vt_Count.Source<i64>`, v6) with identities `lugha_type_<Type>` and box copies `lugha_copy_<type>_box` (v5), conversions between interfaces → `lugha_up_<J>.<I>` and derived tables `lugha_vt_<Type>.<J>.<I>` (v6), default methods → `lugha_fn_<Interface>.<method><Type>` (v5; a generic interface's arguments go on the interface, `lugha_fn_Source<i64>.skip<Count>`, v6), runtime → `lugha_rt_<name>`, copy helpers → `lugha_copy_<type>`, equality helpers → `lugha_eq_<type>`, externs verbatim. *Why:* disjoint prefixes are what prevents symbol collisions (spec §8).
8. **Generated code allocates only through `lugha_rt_alloc`.** Never emit calls to `malloc`. *Why:* Boehm can't see `malloc` memory and will free live objects.
9. **Follow the milestone order (spec §11).** The lexer and parser cover the full §2–§3 syntax. The checker and codegen implement only the current milestone's subset; in milestones 1–2 treat every value as `i64`. *Why:* each milestone must end in a running program, while syntax is cheap to do once and fully specified.
10. **The §10 programs are the acceptance tests.** Their stdout and exit codes must match exactly. *Why:* they define "conforming compiler".

---

## ERROR HANDLING — NON-NEGOTIABLE

Expected failures return `Result<T, E>`. Panics are for bugs only.

```
Expected failure  →  return Err(...)
Truly unexpected  →  panic (invariant violated, programmer error)
```

```rust
// Good — the caller sees every failure mode in the type
pub fn parse_number(src: &str) -> Result<i64, ParseError> {
    src.trim().parse().map_err(|_| ParseError::InvalidNumber(src.to_owned()))
}

// Bad — hides a failure path and crashes on user input
pub fn parse_number(src: &str) -> i64 {
    src.trim().parse().unwrap()
}
```

- Each module defines its own error enum; errors are typed, never `String`, derived with `thiserror`.
- **Never** `.unwrap()` / `.expect()` on anything derived from external input. `.expect("reason")` is allowed only for true invariants, and the message states the invariant.
- **Never** return `Option` to signal an error — `None` cannot say *why*.
- **Never** swallow an error with `let _ =` or `.ok()` without a comment explaining why ignoring it is safe.
- Propagate with `?`; convert between error types with `From` impls, not ad-hoc `map_err` everywhere.

**Two kinds of error in this project — keep them apart:**
- **Errors in the user's `.la` program** (bad token, type mismatch) are expected output of the compiler. They become `Diagnostic` records (architecture rule 5) and the stage keeps going to find more where it can. Every stage returns `Result<(Output, Vec<Diagnostic>), Vec<Diagnostic>>`: `Ok` = output + warnings, `Err` = all errors and warnings.
- **Errors in lughac itself** (I/O failure, linker not found) use `thiserror` enums and `Result`. A violated compiler invariant is a bug → exit 2.

---

## FILE ORGANIZATION

```
.
├── CLAUDE.md        — this file
├── MEMORY.md        — resolved decisions + project state
├── CONTEXT.md       — session handoff log
├── DECISIONS.md     — open/deferred/resolved questions
├── CHANGELOG.md     — what shipped
├── TODO.md          — outstanding tasks
├── .llmignore       — protected paths
├── .claude/         — settings.json: harness deny rules mirroring .llmignore
├── .github/         — workflows/ci.yml: fmt, clippy, test in ubuntu:26.04 (LLVM 21, libgc)
├── README.md        — overview, prerequisites, usage, tests
├── Cargo.toml       — package `lugha`, binary `lughac`
├── runtime/         — the C runtime, embedded in lughac (DECISION-009): lugha_rt.c (core: includes, types,
│                      init, alloc, panics, strings), stack.c (overflow handler), print.c (formatting),
│                      input.c (args, parse_i64/f64, stdin lines, files, stderr, exit), format.c (format
│                      specifiers, v13), bytes.c (binary I/O, UTF-8 check, v14), chars.c (`char` printing and quoting, v14), random.c (`lugha_rt_os_seed`, v15), env.c (`lugha_rt_env`, `lugha_rt_cwd`, v15; `lugha_rt_chdir`, v16), fs.c (folders and `stat`, v15), process.c (`lugha_rt_spawn`: `posix_spawnp`, no shell, v15); fragments joined in that order
│                      into one lugha_rt.c (PRP-028)
├── rust-toolchain.toml — pinned Rust 1.99.0
├── src/
│   ├── lib.rs       — compiler library; SPEC (the embedded spec `lughac spec` prints), PRELUDE, STD (v15)
│   ├── std/         — the standard library in Lugha (v15), embedded as `lugha::STD` and loaded only when imported:
│   │                  math.la (`std.math`: float functions over `T: Float` via `$float.*` built-ins, abs/min/max/clamp, pi/e),
│   │                  random.la (`std.random`: xoshiro256** `Rng`, `seeded`/`from_os` via `$os.seed`),
│   │                  env.la (`std.env`: `get`, `current_dir`, `set_current_dir` via `$os.env`/`$os.cwd`/`$os.chdir`),
│   │                  fs.la (`std.fs`: `list_dir`, `make_dir(s)`, `remove_file`, `remove_dir`, `exists`, `is_dir`, `is_file`),
│   │                  process.la (`std.process`: `run`, `run_with`, `call` via `$os.spawn`)
│   ├── prelude/     — the prelude in Lugha, joined in this order into the one module `$prelude` (`lugha::PRELUDE`):
│   │                  core.la (Result<T, E>, Iter<T> (v6), the array helpers, the string/Option/Result helpers),
│   │                  iter.la (iterator sources, adapters and consumers, v10; `Display`, v12), more.la (`zip` yielding
│   │                  tuples, `chain`, `take_while`, `skip_while`, `flat_map`, `min`, `max`, `position`, v13), text.la
│   │                  (`Chars`, `char_count`, `char_from`, Latin-1 classification and case, v14; `utf8_decode`, `text_from_utf8`, `text_parse_i64`/`u64`, v15), ops.la (the
│   │                  operator bounds `Add`…`Neg` and `Num`, v15; `enum Ordering` and `sort`, v15)
│   ├── main.rs      — `lughac` entry point (CLI only)
│   ├── span.rs      — byte-offset Span
│   ├── format.rs    — format specifiers (v13): `parse`, `check`, shared by the checker and codegen
│   ├── template.rs  — `format` templates (v14): `pieces`, `holes`, `parts`, shared by the checker and codegen
│   ├── diagnostic.rs — Diagnostic record (spec §9 fields), no rendering
│   ├── lexer/       — mod.rs (lex, main loop), token.rs, punct.rs (operators), number.rs, string.rs,
│   │                  interp.rs (string interpolation, E0111), format_spec.rs (the `FmtSpec` token, v13), char_lit.rs (`char` literals, E0112, v14); string.rs also reads `format` templates raw (v14); `lex_bundled` lets `$` start a name in bundled sources (v15)
│   ├── ast/         — mod.rs (items, statements), types.rs (written types), expr.rs (expressions, ExprId, StructLiteral, Part),
│   │                  pattern.rs (arms, nested patterns, guards), header.rs (imports, type parameters, bounds,
│   │                  interfaces)
│   ├── parser/      — mod.rs (parse, cursor, errors), recover.rs, describe.rs, operators.rs (operator tables, `>>` as two `>`), conditional.rs (`if let`, `while let`, `let … else`), self_type.rs (`Self` in `impl` blocks, v10, and in struct and enum declarations, v13),
│   │                  expr.rs (Pratt), primary.rs, stmt.rs, item.rs, decl.rs (enum, impl, receivers),
│   │                  pattern.rs (match, patterns, guards), generic.rs (type params, m.T / T<args>),
│   │                  import.rs (import, pub), interface.rs (interfaces, bounds), closure.rs (closure
│   │                  literals, E0209), sexp/ (mod.rs exprs,
│   │                  item.rs items and types), test_util.rs, tuples.rs (tuple types, literals, patterns, `t.0`, `for (i, w)`, v14), format.rs (`format("…", …)` with a literal template, v14)
│   ├── check/       — mod.rs (check), checked.rs (the `Checked` output), test_util.rs, types.rs, env.rs (globals, main, scopes),
│   │                  expr.rs, literal.rs (literals; any literal as `T: Num`/`Float` and its range per instance, v15), call.rs, ops.rs (operators, casts), body.rs (function bodies), stmt.rs,
│   │                  assign.rs (places, mutability), flow.rs (returns, loops, W0101), access.rs
│   │                  (fields, indexing), array.rs (literals, for … of, E0507 guard), structs.rs
│   │                  (declarations, E0307/E0308, literals), liveness/ (mod.rs move on last use; walk.rs the walk), enums.rs
│   │                  (declarations), construct.rs (E.V, Some, None), matching.rs (match, guards, W0103), patterns.rs (pattern typing),
│   │                  usefulness.rs (E0417 witnesses, W0102), methods.rs (impl,
│   │                  E0310), method_calls.rs (calls, receivers, E0415, E0508), arguments.rs (call arguments, arity), builtins.rs (push, pop, clear), swap.rs (swap, E0509), conflicts.rs (E0510), modules/ (mod.rs check_modules,
│   │                  keys, passes; lookup.rs m.f / m.T, externs, main), visibility.rs (E0313/E0318), levels.rs (v8 visibility levels: private, package, public),
│   │                  struct_literal.rs (S { … }, m.S { … }), generics.rs (type parameters, E0315/E0316,
│   │                  E0418, substitution), bounds.rs (Eq/Ord, E0419), infer.rs (type arguments, E0412,
│   │                  Checked.instances), instances.rs (instance list, E0420, Checked.monos), layouts.rs
│   │                  (concrete generic types, substituted tables, Checked.used), interfaces.rs (declarations,
│   │                  interface bounds, E0315, E0423), satisfy.rs (calls through bounds E0421, satisfaction
│   │                  E0422), input.rs (args, parse_i64/f64, interpolation), fun_values.rs (function types
│   │                  and values, calls of values), closures.rs (closure types, captures, E0511), helpers.rs
│   │                  (xs.map(f) etc. as prelude calls), results.rs (Ok/Err, `?`, E0424), errors/ (mod.rs, names.rs E03xx, types.rs E04xx, places.rs E05xx/W01xx,
│   │                  generics.rs milestone 11–12 codes, v4.rs v4 codes, v5.rs v5 codes, v6.rs v6 codes, v8.rs v8 visibility, v9.rs v9 codes, v10.rs v10 codes, v14.rs v14 codes),
│   │                  iface_values.rs (interface types, E0425, conversion, Checked.conversions), iface_calls.rs
│   │                  (calls on interface values), iface_tables.rs (Checked.interfaces, conversion reachability), type_patterns.rs
│   │                  (`c: Circle` patterns, E0426), defaults.rs (default methods: `Self`, bodies, instances),
│   │                  applied.rs (generic interfaces: `Applied`, items at arguments), bound_args.rs (bounds with
│   │                  arguments, E0418/E0419), bound_infer.rs (inferring type arguments through bounds and
│   │                  interface-typed parameters), iterators.rs (`for … of` over `next`, `Iter`, Checked.iterations),
│   │                  self_type.rs (`Self` in interfaces: items for a type, E0427),
│   │                  upcast.rs (converting between interfaces: `undeclared`, E0422),
│   │                  numbers.rs (v7/v10 numeric types: `is_float`, `is_unsigned`, keyword types, `I.MIN`/`I.MAX`), slots.rs (enum payload slots),
│   │                  or_patterns.rs (or-pattern alternatives and bindings E0428, `mut` bindings), conditional.rs
│   │                  (`if let`, `while let`, `let … else`, E0429), concrete_defaults.rs (default methods on
│   │                  concrete types, E0430), named_imports.rs (`import m { X }`, re-exports: aliases, `member`, E0318), printing.rs (what prints, `show` instances, `$print` uses), for_mut.rs (`for x of mut e`: place checks, E0501/E0502/E0506), iter_helpers.rs (iterator helpers as prelude calls, prelude iterator type names), tuples.rs (tuples as `$tuple<n>` structs, patterns, E0433, v14), templates.rs (`format`: holes against arguments, E0432, v14), operators.rs (v15: operators as method calls, built-in satisfaction of the operator bounds, `Num`; operators_tests.rs), impl_bounds.rs (v15: bounds on `impl` blocks), float_ops.rs (v15: `$float.<op>` and `$os.seed` built-ins, `float_op`, `os_op`), ordering.rs (v15: `cmp` makes a type `Ord`, `has_cmp`, `order_uses`, `cmp_targets`, `ORDERING`), reexports.rs (`as`, whole-module re-exports: `module_path`, `member_through`, `m.v.X`, v14), refs.rs (v16: `Ref<T>`: `Ref.new`, `predeclared_ref`, `REF_NEW`; tests)
│   ├── codegen/     — mod.rs (emit_ir, emit_object, CodegenError), lower.rs (module, C main),
│   │                  value.rs (Value, Type → LLVM), scope.rs (locals), expr.rs, arith.rs (ops at
│   │                  each width), cast.rs (§4 casts), control.rs (blocks, if, loops, jumps), stmt.rs,
│   │                  function.rs (bodies, return, calls), declare.rs (signatures, symbols, C ABI), runtime.rs (intrinsics, string
│   │                  literals, panics), heap.rs (headers, element layout, bounds checks, string ops),
│   │                  array.rs (allocation, list/repeat literals, for … of), copy.rs (copy sites, clone helpers, mangle), cow.rs (copy-on-write: the shared flag, `own`), places.rs (write places: plan, commit, `for … of mut` aliases, `own_place`; `mut self` receivers planned before their arguments, v15), hoist.rs (owning arrays once per loop: `hoist_loop`, fixed paths, guarded hoists, `hoisted`, v15), hoist_scan.rs (the loop scan: roots that may share, changed and bound names, written arrays), equality.rs (deep ==, lugha_eq_*),
│   │                  structs.rs (named types, layout, literals, fields, place addresses), enums.rs
│   │                  (enum layout, Option types, payload addresses), growth.rs (push, pop, clear), construct.rs (building enum values,
│   │                  indirect slots), matching.rs (match lowering, switch), patterns.rs (nested/guarded arms),
│   │                  methods.rs (method symbols, receivers, calls), input.rs (interpolation, args, parsing),
│   │                  io.rs (read_line, files, eprint, exit), fun_values.rs ({ code, env } values, lugha_ref_
│   │                  wrappers, indirect calls), closures.rs
│   │                  (environments, lugha_closure_ bodies), results.rs (`?`: branch and early return),
│   │                  interfaces.rs (interface values: conversion, boxes, lugha_vt_ tables, box copies), dispatch.rs
│   │                  (dynamic calls, interface copies, type-pattern tests), defaults.rs (default instances, `?` boxing),
│   │                  instances.rs (generic instances:
│   │                  substitution, instance symbols),
│   │                  swap.rs (swap: loads and crossed stores), bits.rs (bit operators, the shift range check), int_methods.rs (wrapping and counting methods), int_checked.rs (v10 checked/saturating/rotating methods, `I.MIN`/`I.MAX`), or_patterns.rs (alternatives in the arm chain, `mut` copies), format.rs (format specifiers → `lugha_rt_fmt_*`), show.rs (printing: `text`, `lugha_show_<type>` helpers), show_iface.rs (printing interface values by identity), unwrap.rs (`unwrap`/`expect`), conditional.rs (`if let`, `while let`, `let … else`), iterators.rs (iterator loops: a hidden copy, `next` until `None`), tuples.rs (tuple literals, patterns, printing, `lugha_cmp_` order), operators.rs (v15: operator method calls; numbers keep the built-in operators), float_ops.rs (v15: `$float.<op>` → `llvm.<op>.f32/f64`), os.rs (v15: `$os.<op>` → runtime calls with flags and out-slots), ordering.rs (v15: `<` through `cmp`, `user_order`), refs.rs (v16: `Ref.new`, `get`/`set`/`update`/`is`, `show_box` with the cycle stack),
│   │                  upcast.rs (between interfaces: table registry, derived tables, `lugha_up_` lookups)
│   ├── modules/     — mod.rs (load: the main file and every import, one offset space; with_prelude), resolve.rs
│   │                  (import checks, E0312/E0314/E0319), manifest.rs (`lugha.toml`, E0321), inline.rs (inline
│   │                  entries `{ path, version }`, v11), version.rs (versions, requirements, E0324), packages.rs
│   │                  (manifests of a build, package imports, E0320/E0322/E0323, version checks), tests.rs
│   │                  (in-memory loading), version_tests.rs (loading with versions), git.rs (v14: URL checks, `ls-remote`
│   │                  tags, the `Git` trait, `SystemGit` with fixed `-c` flags and env), cache.rs (cache root, escaped
│   │                  folder names, scratch clones, install), lock.rs (`lugha.lock`, E0321), fetch.rs (`Fetcher`, `Mode`:
│   │                  lock, cache, clone, E0325/E0326; fetch_tests.rs with a fake `Git`), parse.rs (one module: decode, lex,
│   │                  parse), paths.rs (`exists`, `normalized`, E0320), package_table.rs (v15: `[package]` version, name,
│   │                  source), layout.rs (v15: `LoadEnv`, the bounded manifest lookup, `name`/`source`, the implicit
│   │                  package, E0327/E0328; layout_tests.rs), std_tests.rs (v15: `std` modules from `lugha::STD`, reserved `std`)
│   ├── link.rs      — `cc … lugha_rt.c -lgc -lm -o out`, RUNTIME_PARTS, RUNTIME_SOURCE, RUNTIME_SYMBOLS, LinkError
│   └── driver/      — mod.rs (clap CLI, exit codes, Output), build.rs (build, --emit per module), guard.rs
│                      (panics → internal error, exit 2), pipeline.rs (load all modules + the prelude, check, codegen),
│                      source.rs (source map of all files, line_col),
│                      render.rs (codespan, Report), json.rs (JSON lines), update.rs (v14: the fetcher, writing
│                      `lugha.lock`, `lughac update`)
├── tests/
│   ├── programs.rs  — end-to-end acceptance test (runs every tests/programs/ case)
│   ├── spec.rs      — spec §10 acceptance: programs and expected results extracted from lugha::SPEC
│   ├── lexer.rs     — lexer public-API tests (every spec program lexes)
│   ├── parser.rs    — parser public-API tests (every spec program parses)
│   ├── codegen.rs   — builds and runs real executables at -O0 and -O2
│   ├── cli.rs       — the lughac binary: commands, emit, diagnostics, exit codes
│   ├── modules.rs   — programs of several files through lughac (emit, per-file diagnostics, symbols)
│   ├── interface_values.rs — interface values through lughac (check, run, IR tables)
│   ├── git_packages.rs — git packages through lughac and a real `file://` repository (lock, offline, E0325/E0326)
│   ├── lazy_std.rs  — lazy std modules through lughac: only reached std functions in the IR (v15)
│   ├── ordering.rs  — user-defined order through lughac: `<` and tuple helpers call `cmp` (v15)
│   ├── package_layout.rs — package layout through lughac: `source`, `examples/` by name, the `$HOME` bound, E0327/E0328
│   ├── common/      — spec_programs.rs: extracts the §10/§11 programs from the embedded spec
│   ├── programs/    — m1/ … m68/, v1_1/, v1_2/, v4_1/: <name>.la + .stdout/.exit/.stderr expectations (+ .args, .stdin, .scratch);
│   │                  a directory <name>/ with main.la (+ modules) and <name>.* beside it is one case
│   └── support/     — fixture.rs (temp dirs), runner/ (discover, dirs (directory cases), args (.args/.stdin/.scratch/.envvars),
│                      execute, report)
├── PRPs/            — feature briefs prp-{NNN}-{feature_name}.md (+ TEMPLATE.md, DISCOVERY.md)
├── docs/            — CODE_STYLE.md, setup.md (the context-system guide), source/, decisions/, incidents/, status/
│   └── specs/       — Language v16 Specification.md (the language definition, in progress, printed by `lughac spec`);
│                      Language v16 Draft.md (approved v16 plan, milestone 67); Language v15 Draft.md (approved v15 plan, milestones 51–66);
│                      Language v14 Draft.md (approved v14 plan, milestones 45–50);
│                      Language v13 Draft.md (approved v13 plan, milestones 41–44);
│                      Language v12 Draft.md (approved v12 plan, milestones 39–40);
│                      Language v11 Draft.md (approved v11 plan, milestones 37–38);
│                      Language v10 Draft.md (approved v10 plan, milestones 34–36); Language v9 Draft.md (approved v9 plan, milestones 31–33); Language v8 Draft.md (approved v8 plan, milestone 30); Language v7 Draft.md (approved v7 plan, milestones 27–29); Language v6 Draft.md (approved v6 plan, milestones 23–26); Language v5 Draft.md (approved v5 plan, milestones 20–22); Language v4 Draft.md (approved v4 plan, milestones 17–19); v3 and v2 Drafts (past plans);
│                      Language v15, v14, v13, v12, v11, v10, v9, v8, v7, v6, v5, v4, v3, v2, v1 (v1.2) and v0 Specification.md (frozen)
└── reports/         — EOD reports (YYYY-MM-DD.md)
```

Update this tree when modules or `tests/` are added.

---

## ANTI-PATTERNS

1. **Never add a crate without a PRP line justifying it.** Dependencies are permanent cost and licensing risk.
2. **Never write `unsafe` without human approval.** It voids the guarantees every other rule relies on.
3. **Never silence a clippy lint with `#[allow(...)]` without a comment saying why.** Silent allows hide real bugs.
4. **Never add an implicit conversion or coercion.** Spec §4 forbids them, except the one it names: a value to an interface where one is expected (v5, recorded in `Checked.conversions`). Every other one is a type hole the checker can't see.
5. **Never implement a §11 "out of scope" feature** (a package registry, `ssh` URLs, a default entry point, full Unicode tables, streams…) even partially, nor a v16 feature that `Language v16 Draft.md` doesn't plan yet. Changes to how `git` runs (`modules/git.rs`) need the user's security review, as PRP-079 had, and so do changes to how programs are started (`runtime/process.c`, PRP-095).
6. **Never stop the checker at the first error.** Spec §9 requires reporting as many as it can find.
7. **Never print diagnostics directly from a stage.** Stages return records; only the driver renders. Otherwise human and JSON output diverge.

---

## KNOWN ISSUES — DO NOT FIX

These look like bugs but are specified behavior:

- **A function whose only exit is a `return` inside `while true` is rejected** as missing a return. Spec §6 rejects it on purpose to keep return checking simple; users add `panic("unreachable");`.
- **Integer overflow always panics**, even at `-O2`. Spec §12 decided against wrapping.
- **Boehm may keep garbage alive** when an integer looks like a pointer. Conservative GC, spec §7.
- **Extern C code keeping a Lugha pointer is undefined behavior.** Spec §7–8 accept this for v0.
- **`as` casts truncate/saturate silently.** The only place values wrap (spec §4).
