# CONTEXT.md — lugha

Session handoff file. Updated at the end of every session.
Read at the start of the next session alongside CLAUDE.md, MEMORY.md, and DECISIONS.md.

Every session has a name and a state: open | closed.
A session is closed only after CONTEXT.md is committed and pushed.

---

## SESSION 1 — 2026-10-09 — Context system setup — closed

Branch: main

### WHAT WAS DONE

Scaffolded the AI context system described in `setup.md`, adapted from its TypeScript examples to Rust: CLAUDE.md, MEMORY.md, CONTEXT.md, DECISIONS.md, CHANGELOG.md, TODO.md, .llmignore, PRPs/ templates, docs/CODE_STYLE.md and the docs/ and reports/ folder structure. Project-specific facts that are not yet known (crate layout, error crate, edition/MSRV) were opened as decisions rather than guessed. `docs/DESIGN.md` was skipped — there is no UI.

Then reviewed `docs/specs/Language v0 Specification.md` and fixed its inconsistencies at the user's request: symbol prefixes (`lugha_fn_` / `lugha_rt_`), milestone 2 test program, struct passing, negative literal folding, negative repeat counts, exact acceptance outputs and float formatting, associativity, prefix grammar, `void` as a non-value. Source extension changed from `.lugha` to `.la`. PRP filenames set to `prp-{NNN}-{feature_name}.md`. Finally, updated CLAUDE.md (stack, 10 architecture rules, two kinds of error, anti-patterns, known issues), MEMORY.md (decisions 3–7), DECISIONS.md (001 narrowed to crate layout; new 005–010) and TODO.md (prerequisites and milestones) from the spec.

### FILES CREATED OR MODIFIED

```
CLAUDE.md                 — behavioral rules, Rust-adapted
MEMORY.md                 — initial decisions (Rust/GPL, Result-based errors)
CONTEXT.md                — this log
DECISIONS.md              — DECISION-001..003 open, DESIGN.md deferred
CHANGELOG.md              — Unreleased block
TODO.md                   — outstanding setup and project tasks
.llmignore                — protected paths
PRPs/TEMPLATE.md          — PRP template
PRPs/DISCOVERY.md         — discovery interview protocol
docs/CODE_STYLE.md        — rustdoc and comment rules
docs/**, reports/         — empty folders (.gitkeep)
docs/specs/Language v0 Specification.md — inconsistencies fixed, extension .la
```

### TESTS WRITTEN

- None — no code yet.

### DECISIONS MADE

- Rust conventions replace the guide's TS ones: unit tests in `#[cfg(test)]` modules, integration tests in `tests/`, `///` rustdoc, `Cargo.lock` protected from hand edits.
- File size limit set to 300 lines (the guide's default).
- PRP filenames: `prp-{NNN}-{feature_name}.md`, three-digit number, snake_case name.
- Source file extension is `.la`.

### PENDING DECISIONS OPENED

- DECISION-001 — What lugha is and its crate layout
- DECISION-002 — Error derive crate
- DECISION-003 — Rust edition and MSRV
- DECISION-005..008 — LLVM version, diagnostics crate, CLI parsing, test harness (open)
- DECISION-009, -010 — runtime location, array-copy cost (deferred)

### STILL OPEN AT CLOSE

- Seven decisions block milestone 1: DECISION-001 (crate layout), -002, -003, -005, -006, -007, -008.
- Rust, LLVM dev and libgc dev are not installed on the dev machine.

---

## SESSION 2 — 2026-10-09 — Resolve milestone 1 decisions — closed

Branch: main

### WHAT WAS DONE

The user accepted the suggested option for every decision blocking milestone 1. Resolved DECISION-001, -002, -003, -005, -006, -007 and -008 and recorded them in MEMORY.md (decisions 8–12), CLAUDE.md (stack, testing, error handling) and TODO.md. For DECISION-005 the suggestion was "a version inkwell supports and Ubuntu packages"; checked both: Ubuntu 26.04's default `llvm-dev` is LLVM 21, and inkwell 0.10.0 (latest on crates.io) supports `llvm21-1`, so LLVM 21 was chosen. For DECISION-006 the suggestion was "codespan-reporting or hand-rolled"; chose codespan-reporting, with a milestone 3 check that it reproduces the spec's E0401 output.

### FILES CREATED OR MODIFIED

```
DECISIONS.md — 001, 002, 003, 005, 006, 007, 008 moved to RESOLVED; no open decisions
MEMORY.md    — decisions 8–12, state and next start point
CLAUDE.md    — stack versions, tests/programs/ rule, stage return type
TODO.md      — decisions checked off, install and init steps made concrete
CONTEXT.md   — this entry
```

### TESTS WRITTEN

- None — no code yet.

### DECISIONS MADE

- See above; all recorded in DECISIONS.md RESOLVED.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Toolchain not installed (rustup, `llvm-21-dev`, `libgc-dev`).
- Crate not initialised.

---

## SESSION 3 — 2026-10-09 — Verify toolchain and initialise crate — closed

Branch: main

### WHAT WAS DONE

Verified the user's install: Rust 1.99.0, LLVM 21.1.8 (`llvm-config-21`, shared mode), libgc headers and library, cc 15.2. Initialised the crate: package `lugha` (library) with binary `lughac`, edition 2024, toolchain pinned to 1.99.0, dependencies inkwell 0.10, clap 4.6 (derive), codespan-reporting 0.13, thiserror 2.0. The first build failed: inkwell's `llvm21-1` feature links LLVM statically and Ubuntu ships no static Polly library. Switched to `llvm21-1-prefer-dynamic` and amended DECISION-005. Confirmed with a throwaway (deleted) example that inkwell builds and verifies a module, and that a C program links `-lgc`. fmt, clippy and test all pass.

### FILES CREATED OR MODIFIED

```
Cargo.toml          — package metadata, [[bin]] lughac, dependencies
Cargo.lock          — generated by cargo
rust-toolchain.toml — pins 1.99.0 + rustfmt, clippy
src/lib.rs          — crate doc only
src/main.rs         — crate doc + empty main
DECISIONS.md        — DECISION-005 amended (prefer-dynamic)
MEMORY.md           — decision 10 details, project state, next start point
CLAUDE.md           — stack versions, file tree
TODO.md             — prerequisites and init checked off
```

### TESTS WRITTEN

- None — no behaviour yet.

### DECISIONS MADE

- Static LLVM linking abandoned for dynamic (DECISION-005 amendment).
- `license-file = "LICENSE"` in Cargo.toml rather than an SPDX id, since whether the project is GPL-3.0-only or -or-later is not recorded.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- No PRPs written yet.

---

## SESSION 4 — 2026-10-09 — PRP-001 test runner — closed

Branch: main → prp-001-test_runner

### WHAT WAS DONE

Ran the discovery interview for PRP-001; the user asked for a recommended answer with every question, now a rule in `PRPs/DISCOVERY.md`. Answers: mode chosen by `.exit` (run vs reject, optional `.stderr` for panic tests); per-milestone subfolders; one end-to-end test reporting every failure; strict malformed-case handling with a 10 s timeout; runner logic unit-tested now, end-to-end test `#[ignore]`d until milestone 1. PRP approved and implemented test-first on branch `prp-001-test_runner`. After `cargo fmt` the planned two files exceeded 300 lines; the user approved splitting by job (discover / execute / report, tests at the bottom of each), and the PRP's Structure section records the amendment.

### FILES CREATED OR MODIFIED

```
PRPs/prp-001-test_runner.md        — new PRP; status implemented; structure amended
PRPs/DISCOVERY.md                  — every question carries a recommended answer
tests/programs.rs                  — end-to-end acceptance test (ignored until M1)
tests/support/mod.rs               — module root
tests/support/fixture.rs           — temp-dir Fixture, exited() helper
tests/support/runner/mod.rs        — shared types, DEFAULT_TIMEOUT
tests/support/runner/discover.rs   — case discovery + 9 tests
tests/support/runner/execute.rs    — process running with timeout + 2 tests
tests/support/runner/report.rs     — compare and report formatting + 6 tests
tests/programs/m1/arith.{la,stdout,exit} — §11 milestone 1 program, expects exit 14
CLAUDE.md, CHANGELOG.md, TODO.md, MEMORY.md — tests/ tree, shipped item, progress
```

### TESTS WRITTEN

- discover.rs — recursive sorted discovery; run vs reject mode; 6 malformed layouts; empty root.
- execute.rs — timeout kills the child (200 ms limit); stdout/stderr/exit captured.
- report.rs — match, newline-sensitive mismatch, multiple mismatches, reject exit 1, timeout, escaping and truncation.
- programs.rs — end-to-end (ignored); currently fails with exactly `m1/arith.la: exit code, expected 14, actual 0`.

### DECISIONS MADE

- Recommended answers accompany every discovery question (user request).
- Runner split into discover / execute / report modules (user-approved file split).
- Timeouts kill the whole process group via `kill -KILL -<pgid>`, since `lughac run` spawns the compiled program.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-001-test_runner` fast-forward merged into `main` and deleted.

---

## SESSION 5 — 2026-10-09 — PRP-002 lexer — closed

Branch: main → prp-002-lexer

### WHAT WAS DONE

Discovery for PRP-002 (recommended answer accepted on every question): full §2 lexer now, with CLAUDE.md rule 9 amended so the lexer and parser cover the full syntax and only the checker and codegen follow milestone subsets; the full `Diagnostic` record without rendering; strict number rules; no raw newlines in strings; recovery after errors; codes E0101–E0108 plus E0109 (float out of range), which I added and the user confirmed. Implemented test-first: suites written against a `todo!()` stub (19 failing), then the lexer. One clippy finding (`3.14` read as approximate π in a test) fixed by changing the sample to `3.25`. Spec §2 now records the number and string rules and the error-code table.

### FILES CREATED OR MODIFIED

```
src/span.rs                — Span (byte range) + doctest
src/diagnostic.rs          — Severity, Label, Diagnostic, builders + test
src/lexer/mod.rs           — lex(), main loop, identifiers, punctuation, comments, E0101 + 6 tests
src/lexer/token.rs         — Token, TokenKind, keyword table
src/lexer/number.rs        — integers and floats, E0104–E0109 + 9 tests
src/lexer/string.rs        — strings and escapes, E0102–E0103 + 4 tests
src/lib.rs                 — declares span, diagnostic, lexer
tests/lexer.rs             — every §10/§11 program lexes; M1 token list; no panic on any prefix
docs/specs/…Specification.md — §2 number/string details + lexical error table
PRPs/prp-002-lexer.md      — new PRP; implemented; Ty* naming note
CLAUDE.md, CHANGELOG.md, TODO.md, MEMORY.md — rule 9, file tree, progress
```

### TESTS WRITTEN

- Unit: keywords and case-sensitivity; longest-match operators; comments; E0101 incl. multi-byte; error ordering; spans; integers, floats, `1..10`; E0104–E0109; strings, escapes, UTF-8; E0102 at EOF and end of line with resumption; E0103 continuing.
- Integration: all seven spec programs lex cleanly; exact milestone 1 tokens; every prefix of a mixed sample lexes without panicking.

### DECISIONS MADE

- Type-name keyword variants are `TyI32` … `TyString` (noted in the PRP).
- Lone `&` / `|` get E0101 with a "did you mean `&&`/`||`?" help.
- CRLF: `\r\n` ends a string's line like `\n`.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-002-lexer` fast-forward merged into `main` and deleted.

---

## SESSION 6 — 2026-10-09 — PRP-003 parser — closed

Branch: main → prp-003-parser

### WHAT WAS DONE

Discovery for PRP-003, with the recommended answer accepted every time:
- An owned AST tree with dense `ExprId`s.
- Recovery at statement and item boundaries.
- Codes E0201–E0205, plus E0206 (nesting limit 256), which I proposed and the user confirmed.
- An optional `;` after block-like statements; the struct-literal restriction lifts inside blocks; a lone `;` is an error.
- An S-expression printer.

Implemented test-first: 25 failing tests against stubs, then the parser. All passed on the first full run.

After `cargo fmt`, four files exceeded 300 lines. The user approved a split by concern: `ast/` folder, `parser/recover.rs`, `parser/primary.rs`, `parser/test_util.rs`. The PRP records it, along with `describe.rs`, the E0203 help wording and the shared `tests/common/spec_programs.rs`. Spec §3 now records the grammar clarifications and the E0201–E0206 table.

### FILES CREATED OR MODIFIED

```
src/ast/mod.rs, src/ast/expr.rs         — AST (items, types, statements; expressions, ExprId, ops)
src/parser/mod.rs                       — parse(), token cursor, mk, error reporting, close()
src/parser/recover.rs                   — comma lists, nesting limit, struct-literal mode, sync
src/parser/describe.rs                  — token names for messages
src/parser/expr.rs                      — Pratt loop, prefix, postfix + 7 tests
src/parser/primary.rs                   — literals, names, struct literals, arrays, if/block + 4 tests
src/parser/stmt.rs                      — blocks and statements + 7 tests
src/parser/item.rs                      — items and types + 5 tests
src/parser/sexp.rs                      — S-expression printer + 2 tests + doctest
src/parser/test_util.rs                 — test helpers
src/lib.rs                              — declares ast, parser
tests/parser.rs                         — spec programs parse; M1 tree; no panic on token prefixes
tests/common/spec_programs.rs           — shared spec programs (moved from tests/lexer.rs)
tests/lexer.rs                          — uses the shared programs
docs/specs/…Specification.md            — §3 clarifications + parse error table
PRPs/prp-003-parser.md                  — new PRP; implemented; amendments recorded
CLAUDE.md, CHANGELOG.md, TODO.md, MEMORY.md — file tree, shipped item, progress
```

### TESTS WRITTEN

- Precedence, associativity, postfix chains, every literal form, if/else chains, tail vs statement, let and all assignment operators, loops and jumps, optional parentheses, items, expression bodies, externs, structs, array types.
- Each of E0201–E0206, including no stack overflow on 300 nested parens or 5,000 prefix minuses. Recovery at statements (3 errors) and items (2 errors). Dense ids and spans.
- Integration: all spec programs parse; the exact milestone 1 tree; no panic on any token prefix.

### DECISIONS MADE

- File split by concern (user-approved).
- `Type` derives `Eq` (needed because `TypeKind` holds `Box<Type>`).

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-003-parser` fast-forward merged into `main` and deleted.

---

## SESSION 7 — 2026-10-09 — PRP-004 codegen and link — closed

Branch: main → prp-004-codegen_and_link

### WHAT WAS DONE

Discovery for PRP-004, with the recommended answer accepted every time:
- Unsupported constructs give a typed `CodegenError::Unsupported { what, milestone, span }`; the driver will print it with exit 2.
- Arithmetic: `+ - *` wrap and `/ %` trap until milestone 4. Spec §11 moves overflow and division checks from milestone 3 to milestone 4, where `lugha_rt_panic` exists.
- Testing: build and run real binaries.

Implemented test-first against `todo!()` stubs. One test expectation was wrong: codegen reports the outermost unsupported construct, so `[1][0]` is "indexing". The test was corrected and a separate array case added. All checks pass; no `unsafe`; the largest file is 217 lines.

### FILES CREATED OR MODIFIED

```
src/codegen/mod.rs     — OptLevel, CodegenError, emit_ir, emit_object, host target machine (PIC)
src/codegen/lower.rs   — find_main, lugha_fn_main, C main wrapper, statement errors + 4 tests
src/codegen/expr.rs    — i64 literals, neg, + - * / %, trap guard, expression errors + 3 tests
src/link.rs            — link() via `cc … -lgc -lm -o`, LinkError
src/lib.rs             — declares codegen, link
tests/codegen.rs       — 6 build-and-run tests at -O0 and -O2
docs/specs/…Specification.md — §11: overflow checks moved to M4; interim wrap/trap note
PRPs/prp-004-codegen_and_link.md — new PRP; implemented; amendments
CLAUDE.md, CHANGELOG.md, TODO.md, MEMORY.md — known issues, file tree, progress
```

### TESTS WRITTEN

- Unit: the C `main` wrapper and `lugha_fn_main` naming; a void main returns 0; no `nsw`/`nuw`; `/` and `%` guarded by `llvm.trap`; Unsupported for items, statements and expressions with milestone and span.
- Integration:
  - `2 + 3 * 4` exits 14.
  - `-7 / 2` exits 253, `7 % -3` exits 1, `-(3 - 10)` exits 7.
  - The wrapping case exits 254.
  - `1 / 0`, `1 % 0` and `MIN / -1` give SIGILL.
  - A void main exits 0.
  - `link` failure carries `cc`'s stderr.
  - Every case matches at -O0 and -O2.

### DECISIONS MADE

- Overflow and division checks belong to milestone 4 (spec §11 amended).
- Outermost-first reporting of unsupported constructs.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-004-codegen_and_link` fast-forward merged into `main` and deleted.

---

## SESSION 8 — 2026-10-09 — PRP-005 lughac driver — closed

Branch: main → prp-005-driver

### WHAT WAS DONE

Before discovery, I ran the DECISION-006 check in a scratch crate: codespan-reporting can't reproduce the spec's E0401 output byte-for-byte. The differences are `  --> `, trailing spaces, and a blank line after each diagnostic. The check also exposed a missing primary-label field.

Discovery outcomes, with the recommended answer accepted every time:
- Keep codespan, strip trailing whitespace, and change the spec layout (DECISION-011).
- Add `Diagnostic::label` and a JSON `"label"` key.
- `run` passes exit codes through (128+N for a signal); `--emit` prints tokens, S-expression or IR.
- Missing file exits 2; invalid UTF-8 is E0110, exit 1; `build` writes `./<stem>`; internal errors have `"code":null`.

Implemented test-first against stubs. All 104 tests passed on the first run, including the un-ignored `programs()` suite and two hand-written golden `.stderr` files. Then three clippy fixes: boxed error payloads twice, and a struct-update `Config`.

**Milestone 1 is complete:** `lughac run tests/programs/m1/arith.la` exits 14.

### FILES CREATED OR MODIFIED

```
src/driver/mod.rs        — clap CLI (build/run/check, --emit, -O, --diagnostics), exit codes, Output
src/driver/pipeline.rs   — load/front/ir/build/run, Failure, TempDir, default_output + 1 test
src/driver/source.rs     — load (E0110), line_col + 2 tests
src/driver/render.rs     — Report, human rendering via codespan + 2 tests (spec E0401 exact)
src/driver/json.rs       — JSON lines, escaping + 3 tests (spec example exact)
src/diagnostic.rs        — label field + with_primary_label
src/main.rs, src/lib.rs  — main calls lugha::driver::main; declares driver
tests/cli.rs             — 9 tests driving the real binary
tests/programs.rs        — #[ignore] removed
tests/programs/m1/       — void_main, div_zero (132), bad_char (E0101), unclosed (E0201) cases
docs/specs/…Specification.md — §2 E0110; §9 codespan layout, label, code null; §10 example layout
DECISIONS.md, MEMORY.md  — DECISION-011; decision 13; milestone 1 done
PRPs/prp-005-driver.md   — new PRP; implemented; amendments
CLAUDE.md, CHANGELOG.md, TODO.md — commands, file tree, known issues, progress
```

### TESTS WRITTEN

- Unit:
  - The spec E0401 renders exactly in both human and JSON form.
  - Help notes; internal errors without a code; JSON escaping; null code and span.
  - Byte columns; E0110 position; default output never overwrites the source.
- CLI:
  - `build` default and `-o`; `run` exit pass-through (14, 132); `check` exit 0 and 1.
  - All three `--emit` modes write no files.
  - JSON gives one line per diagnostic.
  - A missing file, no arguments, or an unknown flag exits 2; `--help` exits 0.
  - E0110; Unsupported gives exit 2 in human and JSON form.
- Acceptance: five `m1/` programs.

### DECISIONS MADE

- DECISION-011 (renderer layout), plus the amendments listed in the PRP.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-005-driver` fast-forward merged into `main` and deleted. Milestone 1 complete.

---

## SESSION 9 — 2026-10-09 — Milestone 2 discovery and PRP-006 — closed

Branch: main → prp-006-locals_and_control_flow

### WHAT WAS DONE

Milestone 2 discovery, with the recommended answer accepted every time:
- Split milestone 2 into two PRPs: PRP-006 locals and control flow, PRP-007 functions.
- Codegen tracks `Int`/`Bool` value kinds until the checker; mixing them gives "type checking (milestone 3)".
- Pre-checker mistakes stop compilation where codegen must, and compile where it can. Non-`mut` assignment compiles — a known issue.

Implemented PRP-006: seven `m2/` acceptance programs first (all red), then the codegen restructure into `value`/`scope`/`control`/`stmt` modules, rewritten `expr` and `lower`. Everything passed on the first full run. I added `-O0`/`-O2` cross-checks for all `m2/` programs.

### FILES CREATED OR MODIFIED

```
src/codegen/value.rs     — Kind (Int/Bool), Value, type_error
src/codegen/scope.rs     — Scopes, Local, entry_alloca, load
src/codegen/control.rs   — block, if (phi), while, for-range desugaring, break/continue + 4 tests
src/codegen/stmt.rs      — let (annotations), assignment and compound ops + 4 tests
src/codegen/expr.rs      — names, bools, comparisons, !, short-circuit &&/|| + 5 tests
src/codegen/lower.rs     — Lowerer gains scopes/loops; main lowered as a block + 4 tests
src/codegen/mod.rs       — module list, doc
tests/programs/m2/       — 7 programs: even_sum 20, while_count 10, shadowing 12, short_circuit 7,
                           nested_loops 7, if_value 4, for_bounds_once 3
tests/codegen.rs         — m2 at -O0/-O2, immutable assignment known issue, let reads outer binding
tests/cli.rs             — unsupported example switched to `return`
PRPs/prp-006-…md         — new PRP; implemented; amendments
CLAUDE.md, CHANGELOG.md, TODO.md, MEMORY.md — known issues, file tree, progress
```

### TESTS WRITTEN

- Unit:
  - Booleans required for conditions; branch kinds must agree; break/continue outside loops.
  - IR stays valid after jumps; locals live in the entry block; `&&`/`||` produce a `phi`.
  - Kind mismatches; undefined names; `let` annotations; assignment targets; `return`/arrays/strings unsupported.
- Integration: every `m2/` program at -O0 and -O2; immutable assignment compiles; `let x = x + 1` reads the outer `x`.
- Acceptance: 7 `m2/` programs via `lughac run`.

### DECISIONS MADE

- Milestone 2 split; `Int`/`Bool` value kinds; stop-if-forced policy (all recorded in PRP-006).

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-006-locals_and_control_flow` fast-forward merged into `main` and deleted.

---

## SESSION 10 — 2026-10-09 — PRP-007 functions — closed

Branch: main → prp-007-functions

### WHAT WAS DONE

Discovery for PRP-007: the user chose divergence tracking with a third value kind, `Never`, following §6. Everything else followed the spec or the PRP-006 policies.

Implemented test-first: five new `m2/` programs (all red), then:
- `codegen/function.rs`: signatures declared first, bodies, `return`, calls.
- `Value::Never` in `value.rs`.
- Divergence in `block`/`if`.
- `stmt()` reports divergence.
- `lower.rs` lowers every function; the C `main` truncates the `i64` result.

Two earlier tests broke as expected (`return` and the `i32` `lugha_fn_main` signature are no longer accurate) and were updated; see the PRP amendments.

**Milestone 2 is complete:** `lughac run tests/programs/m2/milestone2.la` exits 55. 121 tests pass.

### FILES CREATED OR MODIFIED

```
src/codegen/function.rs  — Signature, declare_functions, define, return_stmt, call + 4 tests
src/codegen/value.rs     — Value::Never (undef placeholders), annotation_kind (moved), never()
src/codegen/control.rs   — block divergence, if: Never branches add no phi edge (finish_branch)
src/codegen/stmt.rs      — stmt() returns whether it diverges; return wired in
src/codegen/expr.rs      — calls; a function name used as a value is a type error
src/codegen/lower.rs     — functions table, ret kind, check_main, every function defined, C main truncates
tests/programs/m2/       — milestone2 (55), early_return (5), mutual_recursion (1), void_helpers (3), param_shadow (6)
tests/cli.rs             — unsupported example is now println (milestone 4)
PRPs/prp-007-functions.md — new PRP; implemented; amendments
CLAUDE.md, CHANGELOG.md, TODO.md, MEMORY.md — known issues, file tree, milestone 2 done
```

### TESTS WRITTEN

- Unit:
  - Call mistakes: duplicate, undefined, argument count, argument kind, calling a local, a function used as a value, intrinsics.
  - `return` vs function kind; missing returns, including `while true`.
  - No `phi` for a returning branch; `unreachable` after a diverging body; `lugha_fn_` naming; `i64` main.
- Acceptance: five new `m2/` programs; all twelve `m2/` programs cross-checked at -O0/-O2.

### DECISIONS MADE

- Divergence tracking with `Value::Never` (PRP-007), plus the amendments recorded there.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-007-functions` fast-forward merged into `main` and deleted; tagged `m2`.

---

## SESSION 11 — 2026-10-09 — Milestone 3 discovery — open

Branch: main

---

## NEXT SESSION START POINT

Milestone 2 is complete (tag `m2`). Next is milestone 3: the type checker and diagnostics (spec §11). It covers:
- All primitive types (`i32`, `i64`, `u8`, `f64`, `bool`), literal inference, casts.
- Mutability checks, return checking, error codes E03xx–E05xx, W0101.
- `lughac check` running the checker, and "remove this semicolon".

Its done-when: the §10 rejected program reports E0401 in both formats (the expected output is already in the spec and the renderer tests).

Milestone 3 replaces codegen's interim `Int`/`Bool`/`Never` value kinds and every "not implemented yet: … (milestone 3)" stop. Plan it in discovery:
- how to split it into PRPs (likely: name resolution and types; inference and casts; mutability and return checking; codegen on real types);
- the checker's module layout (the side table keyed by `ExprId`, spec §9);
- error-code allocation.

`codegen/control.rs` is exactly 300 lines. See `TODO.md`.
