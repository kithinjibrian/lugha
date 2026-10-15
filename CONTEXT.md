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

- Nothing. Branch `prp-035-generic_checking` fast-forward merged into `main` and deleted.

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

## SESSION 11 — 2026-10-09 — Milestone 3 discovery and PRP-008 — closed

Branch: main → prp-008-checker_core

### WHAT WAS DONE

Milestone 3 discovery, with the recommended answer accepted every time:
- Three PRPs: 008 checker core, 009 casts and flow checks, 010 codegen on real types.
- `Type::Error` recovery.
- The checker stops with "not implemented" for milestone 4/5 constructs.
- 12 codes: E0301–E0305 and E0401–E0407.

Implemented PRP-008 test-first: 23 unit tests against a stub, then `src/check/`. One unit-test program was itself ill-typed; the checker was right.

Wiring the checker into the driver exposed that eight `m2/` programs were ill-typed under spec §4: untyped `i64` results returned from `main(): i32`. They were annotated `i32`, with exit codes unchanged.

Spec §10 E0401 matches byte-for-byte in human and JSON form — **milestone 3 done-when met**. The milestone closes after PRP-010.

### FILES CREATED OR MODIFIED

```
src/check/mod.rs, types.rs, env.rs, expr.rs, literal.rs, call.rs, ops.rs, stmt.rs, errors.rs — the checker + 23 tests
src/driver/pipeline.rs   — front() type-checks; parsed() for --emit=ast; CheckError mapping
src/driver/mod.rs        — check help text; --emit=ast uses parsed()
src/lib.rs               — declares check
tests/programs/m2/       — 8 programs annotated with i32 (same exit codes)
tests/programs/m3/       — 13 reject-mode programs: e0301–e0305, e0401–e0407, multiple_errors
tests/cli.rs             — exact spec JSON for E0401; check exits 0 on every valid m1/m2 program
docs/specs/…Specification.md — §9 table of E0301–E0305, E0401–E0407
PRPs/prp-008-checker_core.md — new PRP; implemented; amendments
CLAUDE.md, CHANGELOG.md, TODO.md, MEMORY.md — file tree, known issues, progress
```

### TESTS WRITTEN

- Unit:
  - Every code with spans.
  - Literal defaults and inference from annotations, operands, ranges, arguments and returns.
  - Negated-literal folding; `Never` unification.
  - No cascades; a full type table for valid programs; milestone 4/5 stops.
- Acceptance and CLI:
  - 13 `m3/` reject programs; E0401 is the spec golden, the other 12 were captured and reviewed.
  - The exact spec JSON line; `check` exits 0 on all valid programs.

### DECISIONS MADE

- Milestone 3 split, Error recovery, checker stops for later milestones, code list (in PRP-008), plus the amendments recorded there.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-008-checker_core` fast-forward merged into `main` and deleted.

---

## SESSION 12 — 2026-10-09 — PRP-009 casts and flow checks — closed

Branch: main → prp-009-casts_and_flow_checks

### WHAT WAS DONE

Discovery, with the recommended answer accepted every time:
- Codes E0408 and E0501–E0505, plus W0101. "Remove this semicolon" is a label and help on E0503/E0407.
- W0101 once per block, never failing the build, and not repeated inside dead code.

Implemented test-first: 10 failing unit tests, then:
- `check/assign.rs`: places and mutability, using a `Binding` on each `Local`.
- `check/flow.rs`: missing returns, the semicolon label, loop context, discarded values, unreachable code.
- Casts in `ops.rs`.
- `check()` now returns warnings.

Everything passed on the first full run. Nine new `m3/` programs; goldens captured and reviewed. `w0101.la` exits 3 with the warning on stderr.

### FILES CREATED OR MODIFIED

```
src/check/assign.rs      — places (E0502), mutability (E0501) + 3 tests
src/check/flow.rs        — Flow, reachable/after_statement (W0101, E0505), jump (E0504), missing_return (E0503), stray_semicolon + 6 tests
src/check/ops.rs         — cast (E0408) + 1 test
src/check/stmt.rs        — block uses Flow; loop depth; let/param/loop bindings; assign moved out
src/check/expr.rs        — casts checked; E0407 gets the semicolon label for blocks
src/check/mod.rs, env.rs — Binding, Local span, loops/dead state, warnings returned; test helpers
src/check/errors.rs      — constructors for E0408, E0501–E0505, W0101
tests/programs/m3/       — e0408, e0501, e0501_param, e0502, e0503, e0503_semicolon, e0504, e0505 (reject); w0101 (run, exit 3)
docs/specs/…Specification.md — §9 rows E0408, E0501–E0505, W0101
PRPs/prp-009-…md         — new PRP; implemented; amendments
CLAUDE.md, CHANGELOG.md, TODO.md, MEMORY.md — known issues, file tree, progress
```

### TESTS WRITTEN

- Unit:
  - Casts allowed and E0408.
  - E0501 for let, parameter and loop variable, with help by binding; E0502 for three non-places.
  - E0503 including `while true`, with the loop help; the semicolon label on E0503 and E0407.
  - E0504 inside and outside loops; E0505 and the accepted forms.
  - W0101: once per block, the tail, not nested in dead code, returned in `Ok`.
- Acceptance: nine `m3/` programs.

### DECISIONS MADE

- Codes and W0101 behaviour (PRP-009), plus the amendments recorded there.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-009-casts_and_flow_checks` fast-forward merged into `main` and deleted.

---

## SESSION 13 — 2026-10-09 — PRP-010 codegen on real types — closed

Branch: main → prp-010-codegen_on_real_types

### WHAT WAS DONE

Discovery: the user chose "types from the table" — codegen takes `&Checked`, and its own value kinds and milestone 3 checks are deleted.

Six run-mode `m3/` programs were written first; all failed against the old all-`i64` codegen. Codegen was then rewritten:
- `value.rs`: `Value { Val, Void, Never }` and the `Type` → LLVM mapping.
- `scope.rs`: typed slots.
- `expr.rs`, plus new `arith.rs` (widths, signedness, IEEE) and new `cast.rs` (§4, saturating).
- `control.rs`, `stmt.rs`, `function.rs`, `lower.rs` reworked.
- The driver passes the type table through.

One unit test had a wrong expectation. Three older integration tests were ill-typed and were rewritten with explicit `i64` locals. 150 tests pass.

**Milestone 3 is complete.**

### FILES CREATED OR MODIFIED

```
src/codegen/*            — rewritten on the checker's types; new arith.rs, cast.rs; no milestone 3 stops left
src/driver/pipeline.rs   — Front.checked; emit_ir/emit_object get the table
tests/codegen.rs         — builds through check; three tests made well-typed; cross-check covers m2+m3
tests/programs/m3/       — i32_wrap (9), u8_unsigned (66), u8_wrap (4), f64_math (19), casts (42), div_min_i32 (132)
PRPs/prp-010-…md         — new PRP; implemented; amendments
CLAUDE.md, CHANGELOG.md, TODO.md, MEMORY.md — known issue removed, file tree, decision 14, milestone 3 done
```

### TESTS WRITTEN

- Unit:
  - IR widths: `add i32`, `udiv i8` with only a zero guard, the `MIN` guard at 32 bits.
  - IEEE `f64`: `fdiv`, `fcmp olt`/`une`, `frem`, `fneg`.
  - Casts: `sext`/`zext`/`trunc`, `uitofp`, `fptosi`/`fptoui.sat`.
  - Signatures (`double @lugha_fn_half(i8`); `i32 main` without truncation; negated literals fold to one constant.
  - `u8` ranges compare unsigned; compound assignment at the local's type.
- Acceptance: six `m3/` run programs, cross-checked at -O0/-O2.

### DECISIONS MADE

- Codegen reads the checker's types (MEMORY decision 14).

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-010-codegen_on_real_types` fast-forward merged into `main` and deleted; tagged `m3`.

---

## SESSION 14 — 2026-10-09 — Milestone 4 discovery and PRP-011 — closed

Branch: main → prp-011-runtime_and_intrinsics

### WHAT WAS DONE

Milestone 4 discovery, with the recommended answer accepted every time:
- Two PRPs: 011 runtime and intrinsics, 012 `extern` and panics.
- DECISION-009: embed `lugha_rt.c` as source and compile it in the link call.

Implemented PRP-011:
- Six `m4/` programs written first, with hand-written expected output (red).
- `runtime/lugha_rt.c`: GC allocation, panic, printers, `to_string`, §5 float formatting. Compiles with `-Wall -Wextra -Werror`.
- `link.rs` embeds and compiles it.
- Checker: the `string` type and intrinsic typing.
- Codegen: string-literal globals and intrinsic calls in `codegen/runtime.rs`; `SourceInfo` for panic locations; `lugha_rt_init` in the C `main`.

A real checker bug turned up and was fixed: integer literals were accepted where `string` was expected. **The milestone 4 first half is done:** hello world and `fib(30)` → `832040` print. 157 tests pass.

### FILES CREATED OR MODIFIED

```
runtime/lugha_rt.c       — the C runtime (230 lines)
src/link.rs              — RUNTIME_SOURCE, private runtime temp file, cc … lugha_rt.c
src/codegen/runtime.rs   — RUNTIME_SYMBOLS, runtime declarations, string literals, intrinsics, panic_at + 3 tests
src/codegen/{mod,lower,value,expr,function}.rs — SourceInfo, constants, string type, init call, intrinsic dispatch
src/check/{types,env,expr,call,ops,errors,literal}.rs — Type::String, intrinsics, string-op stop, literal bug fix + tests
src/driver/pipeline.rs   — passes SourceInfo
tests/programs/m4/       — hello, recursion, float_format, print_values, strings, panic (101, stderr)
tests/codegen.rs         — SourceInfo; m4 in the -O0/-O2 cross-check; runtime compiles with -Werror
tests/cli.rs             — unsupported example is now arrays (milestone 5)
docs/specs/…Specification.md — §9 runtime table and link command
DECISIONS.md, MEMORY.md  — DECISION-009 resolved; decision 15
PRPs/prp-011-…md         — new PRP; implemented; amendments
CLAUDE.md, CHANGELOG.md, TODO.md — file tree, progress
```

### TESTS WRITTEN

- Unit:
  - Checker: intrinsic arity and arguments; `panic` diverges; `println` as a value is E0406; string `+` stops; `let s: string = 1` is E0401.
  - Codegen IR: string globals shared per text; `print_i64` before `print_newline`; `u8` widened; panic location; `lugha_rt_init` first.
  - Link: every runtime symbol is defined.
- Integration: the runtime compiles with -Werror; `m4/` at -O0/-O2.
- Acceptance: six `m4/` programs with exact stdout and stderr.

### DECISIONS MADE

- DECISION-009 resolved (MEMORY decision 15), plus the amendments recorded in the PRP.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-011-runtime_and_intrinsics` fast-forward merged into `main` and deleted.

---

## SESSION 15 — 2026-10-09 — PRP-012 extern and panics — closed

Branch: main → prp-012-extern_and_panics

### WHAT WAS DONE

Discovery, with the recommended answer accepted every time: codes E0306 (reserved `lugha_` extern name) and E0409 (`string` return from an extern); panic location is the operator.

Implemented:
- Acceptance programs first, including four existing wrap/trap programs moved to panic expectations.
- The AST gains operator spans (`Binary` and compound `Assign`).
- Checked arithmetic via `llvm.*.with.overflow`, plus divisor checks calling `lugha_rt_panic`; `llvm.trap` removed.
- `extern fun` in the checker (E0306/E0409, shared namespace) and codegen: verbatim names, `zeroext`, and string arguments via a safe struct GEP. inkwell's `build_gep` is `unsafe`, so it was avoided.

Several pre-PRP tests were updated. 162 tests pass. **Milestone 4 complete:** the libc example prints `hello from libc` / `1.4142135623730951`.

### FILES CREATED OR MODIFIED

```
src/ast/{mod,expr}.rs    — operator spans on Binary and Assign
src/parser/{expr,stmt,sexp}.rs — record and ignore operator spans + 2 tests
src/check/{env,errors,expr,stmt}.rs — extern collection, E0306, E0409 + test
src/codegen/arith.rs     — checked add/sub/mul, divisor checks, panic_if + 5 tests
src/codegen/runtime.rs   — emit_panic / call_panic split
src/codegen/function.rs  — extern declarations, C ABI zeroext, c_string GEP, is_extern + test
src/codegen/{expr,stmt}.rs — checked negation; compound assignment located at op_span
tests/programs/m4/       — libc, extern_types, overflow_add, overflow_neg, overflow_compound, div_zero
tests/programs/m3/       — e0306, e0409; i32_overflow, u8_overflow, div_min_i32 now panic
tests/programs/m1/div_zero — now a division-by-zero panic
tests/cli.rs, tests/codegen.rs — signal tests via abort(); panic tests replace wrap/trap tests
docs/specs/…Specification.md — §5 panic messages and locations; §9 E0306, E0409
PRPs/prp-012-…md         — new PRP; implemented; amendments
CLAUDE.md, CHANGELOG.md, TODO.md, MEMORY.md — known issue removed, milestone 4 done
```

### TESTS WRITTEN

- Unit:
  - Operator spans; extern rules (E0306, E0409, E0302, E0305, call checks).
  - Overflow intrinsics at each width; unsigned division checks only zero; signed checks `MIN`; panics point at the operator; `f64` has no panics.
  - Extern C ABI and the string GEP.
- Acceptance: six `m4/` programs, two reject programs, four updated programs.

### DECISIONS MADE

- Codes E0306/E0409; operator-located panics; safe struct GEP for C strings (PRP-012 amendments).

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-012-extern_and_panics` fast-forward merged into `main` and deleted; tagged `m4`.

---

## SESSION 16 — 2026-10-09 — Milestone 5 discovery and PRP-013 — closed

Branch: main → prp-013-string_operations

### WHAT WAS DONE

Milestone 5 discovery, with the recommended answer accepted every time:
- Four PRPs: 013 strings, 014 arrays with copies, 015 structs with copies, 016 `lughac spec` and §10.
- Codes E0410/E0411/E0506.
- The bounds panic reports length and index, formatted by a new runtime function and located at the `[`.
- Run-time-indexed addresses use integer arithmetic. The user declined an audited `unsafe` GEP, so the codebase stays free of `unsafe`.

Implemented PRP-013:
- `m5/` programs first.
- The AST `Index` gains the bracket span.
- Runtime: `str_concat`, `str_eq`, `panic_bounds`.
- Checker: string operators, `.len`, `s[i]` and E0410/E0411/E0506 in the new `check/access.rs`.
- Codegen: the new `heap.rs` (length, element address, bounds check, string operators, field and index).

One outdated stop test was updated. 168 tests pass.

### FILES CREATED OR MODIFIED

```
runtime/lugha_rt.c       — lugha_rt_panic_bounds, lugha_rt_str_concat, lugha_rt_str_eq
src/ast/expr.rs, src/parser/{expr,sexp}.rs — Index bracket span + test
src/check/access.rs      — field and index typing + 3 tests
src/check/{ops,expr,assign,errors,mod}.rs — string operators, E0410/E0411/E0506
src/codegen/heap.rs      — length, element_address, bounds_check, string_binary, field, index + 2 tests
src/codegen/{arith,expr,runtime,mod}.rs — routing, file_name(), RUNTIME_SYMBOLS
tests/programs/m5/       — string_ops, index_oob, index_negative (run); e0410, e0411, e0506 (reject)
tests/codegen.rs         — m5 in the -O0/-O2 cross-check
docs/specs/…Specification.md — §5 bounds message; §9 runtime rows and codes
PRPs/prp-013-…md         — new PRP; implemented; amendments
CLAUDE.md, CHANGELOG.md, TODO.md, MEMORY.md — file tree, progress
```

### TESTS WRITTEN

- Unit:
  - The parser bracket span.
  - Checker: `.len`/`s[i]` types, E0410/E0411, index type errors, E0506, string operator typing.
  - Codegen IR: runtime calls for `+`/`==`; the bounds check with `icmp ult`, the panic location, `ptrtoint`/`inttoptr`, and no run-time-indexed GEP.
- Acceptance: three run and three reject `m5/` programs.

### DECISIONS MADE

- Milestone 5 split, codes, bounds message and integer addressing (PRP-013).

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-013-string_operations` fast-forward merged into `main` and deleted.

---

## SESSION 17 — 2026-10-09 — PRP-014 arrays — closed

Branch: main → prp-014-arrays

### WHAT WAS DONE

Discovery for PRP-014, with the recommended answer accepted every time:
- Codes E0412 and E0507.
- `for … of` variables are borrowed, like parameters.
- `Type::Array(Box<Type>)`.

Implemented PRP-014:
- `m5/` programs first.
- `check::Type` made recursive (its own commit).
- Checker: `check/array.rs` (literals, repeat, `for … of`, the E0507 guard), element places in `assign.rs`, E0409 for arrays in externs.
- Codegen: `array.rs` (allocation, literals, repeat with the negative-length panic, `for … of`), `copy.rs` (memcpy and generated `lugha_copy_*` deep copies, copy-site rules), element layout and element places in `heap.rs`, `Local.borrowed`.

The primes program prints 25. 179 tests pass.

### FILES CREATED OR MODIFIED

```
src/check/types.rs       — recursive Type (commit 7ba1c8d)
src/check/array.rs       — new: literals, repeat, for … of, Path overlap + tests
src/check/{access,assign,env,errors,expr,mod,stmt}.rs — array access, element places, E0409/E0411/E0412/E0507
src/codegen/array.rs     — new: new_array, count_loop, literals, repeat, for_of + 3 tests
src/codegen/copy.rs      — new: value_for_store/return, deep_copy, lugha_copy_* + 3 tests
src/codegen/{heap,stmt,expr,function,control,scope,arith,runtime,mod}.rs — element layout/places, wiring
tests/programs/m5/       — primes, array_basics, value_semantics, array_oob, negative_length (run); e0412, e0507, e0501_element (reject)
tests/cli.rs             — internal-error example is now a struct
docs/specs/…Specification.md — §4 copy table row and "from a place"; §9 codes
PRPs/prp-014-arrays.md   — implemented; amendments
CLAUDE.md, CHANGELOG.md, TODO.md, MEMORY.md — file tree, progress, decision 16
```

### TESTS WRITTEN

- Unit:
  - Checker: literal typing and E0412, repeat, `.len`/indexing, E0501/E0506 on element places, E0507 guards (and shadowing), E0409 for array externs.
  - Codegen IR: allocation size and header, `bool` as `i8`, the negative-length panic, the length read once by `for … of`, memcpy and `lugha_copy_i64_arr_arr`, parameter copy vs local move, no copies for arguments.
- Acceptance: five run and three reject `m5/` programs, cross-checked at `-O2`.

### DECISIONS MADE

- PRP-014 discovery answers; MEMORY 16 (array value semantics).

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-014-arrays` fast-forward merged into `main` and deleted.

---

## SESSION 18 — 2026-10-09 — PRP-015 structs — closed

Branch: main → prp-015-structs

### WHAT WAS DONE

Discovery for PRP-015, with the recommended answer accepted every time:
- Codes E0307/E0308/E0413/E0414.
- Length-prefixed struct names in copy helpers.
- SSA aggregates in codegen.

The user also approved the spec clarification that a struct may contain itself through an array, and the split of `check/errors.rs` by code range.

Implemented PRP-015, with the `m5/` programs written first:
- **Checker:** `check/structs.rs` (pass-1 names, pass-2 field types, cycle detection, literals), struct field access, E0409 for structs in externs.
- **Codegen:** `codegen/structs.rs` (named types, layout, literals, fields, place addresses), struct parameters by pointer, struct copy helpers in `copy.rs`.
- **Stop paths:** the checker's and codegen's milestone stops are dead now; see the PRP amendments.

The centroid program prints `centroid: 2.0, 1.0`. 186 tests pass.

### FILES CREATED OR MODIFIED

```
src/check/errors/        — new: split of errors.rs (mod, names, types, places) + E0307/E0308/E0409 struct/E0413/E0414
src/check/structs.rs     — new: StructInfo, collect/resolve, cycles, literals + 4 tests
src/check/{types,mod,env,access,expr}.rs — Type::Struct, Structs, contains_array(&structs), Checked.structs, uninhabited Stop
src/codegen/structs.rs   — new: layout, declare_structs, literals, struct_field, place_address + 3 tests
src/codegen/{copy,heap,array,function,stmt,expr,value,lower,mod}.rs — struct copies, element sizes, params/args, places
tests/programs/m5/       — centroid, struct_basics, struct_copies (run); e0307, e0308, e0413, e0414, e0501_field (reject)
tests/cli.rs             — internal-error test now uses an unreadable file
docs/specs/…Specification.md — §4 recursion through arrays; §9 codes and copy-helper mangling
PRPs/prp-015-structs.md  — new PRP; implemented; amendments
CLAUDE.md, CHANGELOG.md, TODO.md, MEMORY.md — file tree, known issue removed, progress, decision 17
```

### TESTS WRITTEN

- Unit:
  - **Checker:** any-order resolution, E0302/E0305/E0308/E0307 (with the path, once per cycle; `Node[]` allowed), E0409, literal rules (E0413 listing, E0414, E0410, E0401), field E0410/E0404/E0501/E0507.
  - **Codegen IR:** `%P` and `%F = { i8 }`, struct params as `ptr` with `alloca` arguments, constant field GEPs, `lugha_copy_4Wrap` only for array-holding structs, `Node`/`Node[]` helpers generated once.
  - **Layout:** sizes 16/8/12.
- Acceptance: three run and five reject `m5/` programs, cross-checked at `-O2`.

### DECISIONS MADE

- PRP-015 discovery answers; MEMORY 17 (structs).

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-015-structs` fast-forward merged into `main` and deleted.

---

## SESSION 19 — 2026-10-09 — PRP-016 lughac spec and §10 acceptance — closed

Branch: main → prp-016-spec_command

### WHAT WAS DONE

Discovery for PRP-016, with the recommended answer accepted every time:
- `lughac spec` prints the spec verbatim.
- The tests extract §10 from the spec.

Implemented PRP-016:
- Tests first: the extraction helper, `tests/spec.rs`, and the CLI spec test.
- `lugha::SPEC` and the `spec` subcommand (a broken pipe exits 0).
- The lexer and parser tests switched to the extracted programs.
- Then, as a separate commit, `CheckError`, `Stop`/`Checking` and `CodegenError::Unsupported` were removed; `check::check` now returns the standard stage shape.

Every §10 program passes as extracted from the spec. 191 tests pass. **Milestone 5 complete — v0 complete.**

### FILES CREATED OR MODIFIED

```
src/lib.rs               — SPEC
src/driver/{mod,pipeline}.rs — spec subcommand; Unsupported handling removed
src/check/*.rs           — Checking<T>/Ok/? removed; check() returns Result<(Checked, warnings), diagnostics>
src/codegen/mod.rs       — CodegenError::Unsupported removed
tests/common/spec_programs.rs — rewritten: extracts §10/§11 programs, results and the §9 JSON line from SPEC
tests/spec.rs            — new: §10 acceptance at -O0/-O2, rejected program in both formats, milestone 2
tests/{lexer,parser,cli}.rs — extracted programs; spec command test
PRPs/prp-016-spec_command.md — new PRP; implemented; amendments
CLAUDE.md, CHANGELOG.md, TODO.md, MEMORY.md — tree, commands, v0 complete, decision 18
```

### TESTS WRITTEN

- `tests/cli.rs`: `lughac spec` prints `SPEC` exactly and exits 0; extra arguments exit 2.
- `tests/spec.rs`: extractor coverage with spot checks; every §10 program's stdout and exit at `-O0`/`-O2`; the rejected program's human and JSON diagnostics; milestone 2 exits 55.

### DECISIONS MADE

- PRP-016 discovery answers; MEMORY 18.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-016-spec_command` fast-forward merged into `main` and deleted; `m5` tagged on the merged work.

---

## SESSION 20 — 2026-10-09 — Housekeeping — closed

Branch: main → prp-017-housekeeping

### WHAT WAS DONE

Discovery for PRP-017, with the recommended answer accepted every time:
- All four TODO setup items.
- CI in the `ubuntu:26.04` container.

Implemented PRP-017:
- CI workflow first, as the test. It was pushed and green on the first run.
- README; every command in it was run locally as written.
- `.claude/settings.json` deny rules.
- `setup.md` → `docs/setup.md`, with the live references updated.

### FILES CREATED OR MODIFIED

```
.github/workflows/ci.yml — new: fmt, clippy, test in ubuntu:26.04 (LLVM 21, libgc), SHA-pinned actions
.claude/settings.json    — new: Read/Edit deny rules mirroring .llmignore
README.md                — rewritten
docs/setup.md            — moved from setup.md
.llmignore, CLAUDE.md, TODO.md, DECISIONS.md — sync notes, tree, ticks, reference
PRPs/prp-017-housekeeping.md — new PRP; implemented; amendments
CHANGELOG.md, MEMORY.md  — entry, decision 19
```

### TESTS WRITTEN

- CI run 37964219151: every step green, and every test binary ran in the container (191 tests).
- README commands run locally: `apt-get -s` install, release build, the example via `lughac run` (832040), `lughac spec | head -1`.
- `python3 -m json.tool` on the settings file.

### DECISIONS MADE

- PRP-017 discovery answers; MEMORY 19.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-017-housekeeping` fast-forward merged into `main` and deleted.

---

## SESSION 21 — 2026-10-09 — DECISION-010 copy-on-write — closed

Branch: main → decision-010-copy_cost

### WHAT WAS DONE

The user chose to measure by copy-vs-no-copy timing.
- **Programs:** six benchmark programs: life, particles, particles_inplace, sort, matmul, histogram.
- **Instrumented compiler:** a throwaway copy of the source in the scratchpad, never committed, could count copies and bytes or skip copies.
- **Results:**
  - Copy-on-write would save about 0% on life, sort and matmul (the copies are written right away), and at most ~65% on particles, only with full reference counting.
  - The in-place rewrite of particles is 10× faster.
  - The §10 programs make no copies.
- **Resolution:** after an explanation, the user resolved DECISION-010 as **keep eager copies**. They also approved the spec §12 edit.

### FILES CREATED OR MODIFIED

```
docs/decisions/decision-010.md — method, results, analysis, recommendation
docs/decisions/decision-010/*.la — the six benchmark programs
docs/specs/…Specification.md — §12 cost-of-copies item decided
DECISIONS.md, MEMORY.md (decision 20), TODO.md (unscheduled ideas), CHANGELOG.md
```

### TESTS WRITTEN

- None (no code changes); the benchmarks build and run with the released compiler.

### DECISIONS MADE

- DECISION-010 resolved: eager copies stay; no copy-on-write.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `decision-010-copy_cost` fast-forward merged into `main` and deleted.

---

## SESSION 22 — 2026-10-09 — Close DECISION-004 — closed

Branch: main

### WHAT WAS DONE

At the user's request, closed DECISION-004 (design system) as not applicable. Lugha has no UI, and diagnostics formatting is already governed by spec §9 and DECISION-011. A future UI would open a new decision. This was a docs-only change, committed directly to `main`.

### FILES CREATED OR MODIFIED

```
DECISIONS.md   — DECISION-004 moved to RESOLVED; DEFERRED now empty
MEMORY.md      — decision 21, state, next start point
CHANGELOG.md   — entry
CONTEXT.md     — this session
```

### TESTS WRITTEN

- None (no code changes).

### DECISIONS MADE

- DECISION-004 resolved as not applicable.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. No open or deferred decisions remain.

---

## SESSION 23 — 2026-10-09 — PRP-018 move on last use — closed

Branch: main → prp-018-move_on_last_use

### WHAT WAS DONE

Discovery for PRP-018, with the recommended answer accepted every time:
- The rule is "dead owned local".
- The checker records it.
- One spec §4 sentence.

Implemented PRP-018:
- `check/liveness.rs` with 5 unit tests, written with the pass.
- An IR test (failing first), and the `moves.la` acceptance program, which passed with eager copies before the change.
- Then the codegen change.
- Remeasured with a throwaway counting build: particles 2× faster; life copies halved; sort and matmul unchanged; outputs identical.

197 tests pass.

### FILES CREATED OR MODIFIED

```
src/check/liveness.rs    — new: Mention, moves(), the walk + 5 tests
src/check/{mod,expr}.rs  — mentions recorded; Checked.moves
src/codegen/{copy,lower}.rs — skip copies for moves; IR test; two tests keep sources alive
tests/programs/m5/moves.la (+ .stdout/.exit) — semantics unchanged under moves
docs/specs/…Specification.md — §4 move sentence
docs/decisions/decision-010.md — "After PRP-018" measurements
PRPs/prp-018-move_on_last_use.md — new PRP; implemented; amendments
CLAUDE.md, MEMORY.md (decision 22), TODO.md, CHANGELOG.md
```

### TESTS WRITTEN

- Liveness: dead vs live (read and write), the loop rule, shadowing, a record round trip, parameters and for-of, field places, `[a, a]` (only the last moves), struct fields.
- IR: a dead source has no memcpy; a live one has.
- Acceptance: `moves.la`, at -O0 and -O2.

### DECISIONS MADE

- PRP-018 discovery answers; MEMORY 22.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-018-move_on_last_use` fast-forward merged into `main` and deleted.

---

## SESSION 24 — 2026-10-09 — v1 spec draft — closed

Branch: main → v1-spec-draft

### WHAT WAS DONE

Discovery for the v1 spec, with the recommended answer accepted every time:
- Scope: enums with `match`, a growable list, methods.
- Form: a delta draft.
- Positional enums with dot access.
- Flat `match`.
- Implicit recursion.
- A built-in `Option<T>`.
- Growable `T[]`.
- `impl` blocks.

Wrote `docs/specs/Language v1 Draft.md`. Then the user asked for each choice to be justified by its advantages:
- **Dot access** was kept, with two added rules: E0311 (a local named like a type) and E0207 (`::`, with help).
- **Implicit boxing vs `Box<T>`:** kept implicit, but only the recursive payload slots go on the heap. There is no `Box` and no `indirect` marker; `Box<…>` is E0305 with help.
- **The rationale** is Lugha's strengths: everything is a value, the spec is small enough for a model's prompt, there is one way to do each thing, mistakes are caught at compile time, and allocation is invisible.

The user approved the draft on 2026-10-12.

### FILES CREATED OR MODIFIED

```
docs/specs/Language v1 Draft.md — new: the approved v1 delta (§1–§12, examples, milestones 6–8)
CLAUDE.md      — tree; anti-pattern 5 points v1 features to milestone PRPs
MEMORY.md      — decision 23, state, next start point
TODO.md        — v1 milestones 6–8
CHANGELOG.md   — entry
```

### TESTS WRITTEN

- None (spec work); the examples' expected outputs were checked by hand.

### DECISIONS MADE

- The v1 direction (MEMORY 23).

### PENDING DECISIONS OPENED

- None (the open questions are listed in the draft's §12).

### STILL OPEN AT CLOSE

- Nothing. Branch `v1-spec-draft` fast-forward merged into `main` and deleted.

---

## SESSION 25 — 2026-10-12 — Milestone 6 discovery and PRP-019 v1 syntax — closed

Branch: main → prp-019-v1_syntax

### WHAT WAS DONE

Milestone 6 discovery, with the recommended answer accepted every time:
- a new v1 spec file, with v0 frozen;
- three PRPs (019 syntax, 020 enums/`match`/`Option`, 021 recursion and copies);
- a driver feature gate for constructs not yet implemented.

Implemented PRP-019:
- **Lexer:** the keywords `enum`, `match`, `impl`, `self`; the tokens `=>` and `::`; operator scanning split into `lexer/punct.rs`.
- **AST:** enums, impls, receivers, `Option<T>`, `match`, patterns.
- **Parser:** `decl.rs` and `pattern.rs`, E0207, recovery per arm and at `enum`/`impl`.
- **S-expressions** for every new node.
- **`driver/gate.rs`.**
- **Tests:** the v1 draft programs as parser fixtures; CLI tests for the gate.

212 tests pass.

### FILES CREATED OR MODIFIED

```
src/lexer/{mod,token,punct}.rs — keywords, => and ::, punct split + tests
src/ast/{mod,expr,pattern}.rs  — EnumDecl, ImplDecl, Receiver, Variant, TypeKind::Option, ExprKind::Match, patterns
src/parser/{decl,pattern}.rs   — new: enum/impl/receivers; match/patterns/sync_arm + tests
src/parser/{mod,item,primary,stmt,expr,sexp,describe,recover}.rs — wiring, E0207, Option<T>, sexp, recovery
src/driver/{gate,pipeline,mod}.rs — new gate + tests; called before checking
src/check/{env,expr,liveness}.rs, src/codegen/{function,value,expr}.rs — unreachable arms for gated nodes
tests/common/v1_programs.rs, tests/{parser,cli}.rs — v1 fixtures, parse test, gate CLI test
PRPs/prp-019-v1_syntax.md — new PRP; implemented; amendments
CLAUDE.md, CHANGELOG.md, TODO.md, MEMORY.md (decision 24)
```

### TESTS WRITTEN

- **Lexer:** the new keywords, `=>` and `::`, and that `Option`/`Some`/`None` stay identifiers.
- **Parser:** enums, impls with receivers, `self` outside `impl` (E0201), `Option` types, `self` expressions, E0207 with help and an AST equal to the `.` form, `match` forms and patterns, block arms, per-arm recovery, top-level recovery.
- **Gate:** milestones, source order, v0 untouched.
- **Integration:** the v1 draft programs parse; the CLI gate message in human and JSON form; `--emit=ast` works.

### DECISIONS MADE

- M6 plan and the gate (MEMORY 24).

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-019-v1_syntax` fast-forward merged into `main` and deleted.

---

## SESSION 26 — 2026-10-12 — PRP-020 enums, match and Option — closed

Branch: main → prp-020-enums_and_match

### WHAT WAS DONE

Discovery:
- Recursive enums are gated by name reachability.
- Enums holding arrays: the user chose deep copies; while drafting, this was revised to *sharing*, since payloads are immutable, and approved with the PRP.

Implemented PRP-020:
- **Tests first:** the `m6/` programs.
- **Checker:** `check/enums.rs`, `check/matching.rs`, E0311 in `declare`.
- **Codegen:** `codegen/enums.rs`, `codegen/matching.rs`, `is_aggregate` for enum parameters.
- **Gate:** recursive enums and struct cycles through enums.
- **Spec:** `Language v1 Specification.md` (v0 plus the M6 sections, §10 shapes and rejected enum programs), and `lugha::SPEC` points at it.

Two bugs were found and fixed along the way: a void-arm typing bug, and the tail `None` lookup.

227 tests pass.

### FILES CREATED OR MODIFIED

```
src/check/{enums,matching}.rs — new + tests
src/check/{types,mod,env,expr,structs,liveness,flow}.rs, src/check/errors/* — Enum/Option types, Checked.enums, Binding::Pattern, E0309/E0311/E0415–E0417/W0102, E0305 Box help
src/codegen/{enums,matching}.rs — new + tests
src/codegen/{structs,value,function,lower,expr,stmt,scope,control,copy,mod}.rs — layout with enums, annotation_type method, is_aggregate, routing, Scopes::find
src/parser/item.rs — Name<T> types parse
src/driver/gate.rs — recursive enum / struct-through-enum gate + tests
src/lib.rs — SPEC = v1 specification
docs/specs/Language v1 Specification.md — new; Language v1 Draft.md — folding progress
tests/programs/m6/ — enum_basics, enum_values, w0102, recursive_gate (run); e0309, e0311, e0415, e0416, e0417_literal (reject)
tests/{spec,codegen,cli}.rs, tests/common/v1_programs.rs — rejected programs, m6 cross-check, gate test, fixtures
PRPs/prp-020-enums_and_match.md — new PRP; implemented; amendments
CLAUDE.md, CHANGELOG.md, TODO.md (+ the exit-code finding), MEMORY.md (decision 25)
```

### TESTS WRITTEN

- **Checker:** declarations, construction, `Option` inference, E0311, `match` typing and bindings, exhaustiveness, W0102, pattern fit, discarded and diverging `match`es.
- **Codegen:** layout sizes, named types, `switch` on tags and integers, string chains, enums passed as `ptr`, payload sharing vs construction copies.
- **Gate:** recursion direct, via struct, via `Option`; arrays break cycles; plain struct cycles are left to E0307.
- **Acceptance:** the `m6/` programs at -O0/-O2; spec §10 shapes and the rejected enum program; the CLI gate test.

### DECISIONS MADE

- MEMORY 25 (enums in the compiler).

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-020-enums_and_match` fast-forward merged into `main` and deleted.

---

## SESSION 27 — 2026-10-12 — PRP-021 recursive enums — closed

Branch: main → prp-021-recursive_enums

### WHAT WAS DONE

Discovery: deep recursion is documented, not handled (TODO idea for a stack-overflow panic).

Implemented PRP-021:
- **Tests first:** `m6/recursive.la` (a million-node list walked with loops, a BST, an `Option` chain, copy then extend). The old gate test was removed.
- **`check::indirect_slot`** with tests.
- **Codegen:** indirect slots in layout, construction (heap copy) and `match` binding reads.
- **Gate:** reduced to methods.
- **Spec:** the v1 spec gains recursive data, indirect slots, recursion depth and the linked-list program (prints 6). The draft's folding progress is updated.
- **Approved split:** `codegen/construct.rs`.

229 tests pass. **Milestone 6 complete.**

### FILES CREATED OR MODIFIED

```
src/check/{types,mod}.rs — indirect_slot + test; re-export
src/codegen/{enums,matching,mod}.rs, src/codegen/construct.rs (new) — indirect layout, heap slots, binding reads, split
src/driver/gate.rs — methods only
docs/specs/Language v1 Specification.md, Language v1 Draft.md — recursion folded in
tests/programs/m6/recursive.* (new), recursive_gate.* (removed); tests/{cli,spec}.rs; tests/common/v1_programs.rs
PRPs/prp-021-recursive_enums.md — new PRP; implemented; amendments
CLAUDE.md, CHANGELOG.md, TODO.md, MEMORY.md (decision 26)
```

### TESTS WRITTEN

- `indirect_slot`: lists, trees, an `Option` chain through a struct, mutual recursion; arrays break cycles; plain enums.
- IR: the `%List` layout; `List.Cons` allocates 24 bytes; `List.Nil` doesn't.
- Gate: methods are still stopped; recursion passes.
- Acceptance: `m6/recursive.la` at -O0/-O2; spec §10 linked list prints 6.

### DECISIONS MADE

- MEMORY 26 (recursive data).

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-021-recursive_enums` fast-forward merged into `main` and deleted; `m6` tagged on the merged work.

---

## SESSION 28 — 2026-10-12 — Milestone 7 discovery and PRP-022 methods — closed

Branch: main → prp-022-methods

### WHAT WAS DONE

Discovery:
- One PRP for the milestone.
- A `mut self` argument that aliases the receiver is rejected (E0508), not copied.

Implemented PRP-022:
- **Tests first:** the `m7/` programs.
- **Checker:** `check/methods.rs` and `check/method_calls.rs` (the approved split), `Binding::SelfMut`, `body()` shared by functions and methods.
- **Codegen:** `codegen/methods.rs`, `define_with` and shared `arguments`.
- **Driver:** `driver/gate.rs` deleted.
- **Spec:** methods folded into the v1 spec (§2–§10), with the methods program in §10.

233 tests pass. **Milestone 7 complete.**

### FILES CREATED OR MODIFIED

```
src/check/{methods,method_calls}.rs — new + tests
src/check/{stmt,mod,env,expr,assign}.rs, src/check/errors/* — body(), SelfMut, wiring, check_place_for, E0310/E0415/E0508
src/codegen/methods.rs — new + tests; src/codegen/{function,lower,expr,mod}.rs — define_with, arguments, routing
src/driver/gate.rs — deleted; src/driver/{mod,pipeline}.rs
docs/specs/Language v1 Specification.md, Language v1 Draft.md — methods folded in
tests/programs/m7/ — methods (run); e0310, e0415, e0508, e0501_receiver (reject)
tests/{cli,codegen,spec}.rs, tests/common/v1_programs.rs
PRPs/prp-022-methods.md — new PRP; implemented; amendments
CLAUDE.md (tree, rule 7, gate issue removed), CHANGELOG.md, TODO.md, MEMORY.md (decision 27)
```

### TESTS WRITTEN

- **Checker:**
  - methods across `impl`s, on enums; E0305 for unknown or built-in targets; E0310 (duplicate, field, variant); E0415 (unknown, wrong kind, `push`); arity;
  - `self` immutable and `mut self` assignable; receiver E0501/E0502; E0508 (direct and via a field); E0507; `self` outside a method.
- **Codegen:** receiver-first symbols, `mut self` passing the caller's own `alloca`, element receivers bounds-checked.
- **Acceptance:** `m7/methods.la` at -O0/-O2; the spec §10 methods program; four reject goldens; the CLI `impl` run.

### DECISIONS MADE

- MEMORY 27 (methods).

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-022-methods` fast-forward merged into `main` and deleted; `m7` tagged on the merged work.

---

## SESSION 29 — 2026-10-12 — Milestone 8 discovery and PRP-023 growable arrays — closed

Branch: main → prp-023-growable_arrays

### WHAT WAS DONE

Discovery:
- `xs.push(xs[0])` is allowed (no E0508 for built-ins).
- The draft is retired once v1 is complete.

Implemented PRP-023:
- **Tests first:** the `m8/` programs.
- **Runtime:** `lugha_rt_array_grow`.
- **Layout:** the 16-byte array header (`ARRAY_HEADER`, `capacity_address`; `new_array` stores the capacity, `copy_flat` copies elements only).
- **Checker:** built-ins in `method_calls.rs`, with the E0501/E0507 receiver wording.
- **Codegen:** `codegen/growth.rs`, `construct_values`, and `push` as a liveness store site.
- **Spec:** the v1 spec is complete (title, layout, runtime table, §10 stack program, §11, §12 growth factor); the draft and `v1_programs.rs` were deleted.

236 tests pass, and the DECISION-010 benchmarks are unchanged. **Milestone 8 complete — v1 complete.**

### FILES CREATED OR MODIFIED

```
runtime/lugha_rt.c, src/codegen/runtime.rs — LughaArray, lugha_rt_array_grow, RUNTIME_SYMBOLS
src/codegen/{heap,array,copy}.rs — 16-byte header, cap, element-only copies
src/codegen/growth.rs (new) + tests; src/codegen/{construct,methods,mod}.rs
src/check/{method_calls,assign,liveness}.rs — built-ins, receiver wording, push store site + tests
docs/specs/Language v1 Specification.md — complete; Language v1 Draft.md deleted
tests/programs/m8/ — growth (run); e0501_push, e0507_push, e0415_string_push (reject)
tests/{parser,spec,codegen}.rs; tests/common/v1_programs.rs deleted
PRPs/prp-023-growable_arrays.md — new PRP; implemented; amendments
CLAUDE.md, README.md, CHANGELOG.md, TODO.md, MEMORY.md (decision 28)
```

### TESTS WRITTEN

- **Checker:** `push`/`pop`/`clear` types, `xs.push(xs[0])`, E0401/E0405/E0415 (including strings), the receiver E0501 message, E0507.
- **Liveness:** `push` moves a dead local.
- **Codegen:** literals store `cap`; growth only on the full path; `pop` builds an `Option`.
- **Acceptance:** `m8/growth.la` at -O0/-O2; the spec §10 stack program; three reject goldens; the DECISION-010 programs rerun.

### DECISIONS MADE

- MEMORY 28 (growable arrays; v1 complete).

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-023-growable_arrays` fast-forward merged into `main` and deleted; `m8` tagged on the merged work (v1 complete).

---

## SESSION 30 — 2026-10-12 — PRP-024 internal errors exit 2 — closed

Branch: main → prp-024-internal_error_exit

### WHAT WAS DONE

Discovery:
- One internal error in the chosen format.
- Tested through the guard, with no crash switch.

Implemented PRP-024:
- `driver/guard.rs` (hook, `guarded`, payload messages) with tests; `driver::main` routes through it.
- Spec §9 gains one sentence.
- Verified end to end with a throwaway panicking build.

### FILES CREATED OR MODIFIED

```
src/driver/guard.rs (new) + tests; src/driver/mod.rs — main uses the hook and guard; Command::file
docs/specs/Language v1 Specification.md — §9 sentence
PRPs/prp-024-internal_error_exit.md — new PRP; implemented; amendments
CLAUDE.md, TODO.md, CHANGELOG.md, MEMORY.md (decision 29)
```

### TESTS WRITTEN

- Guard: `&str`, `String` and non-string payloads → exit 2 and one human error; JSON → one null-code line; normal codes 0/1/2 pass through silently.

### DECISIONS MADE

- MEMORY 29.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-024-internal_error_exit` fast-forward merged into `main` and deleted.

---

## SESSION 31 — 2026-10-12 — PRP-025 stack overflow panic — closed

Branch: main → prp-025-stack_overflow_panic

### WHAT WAS DONE

Discovery: a guard-page signal handler, with no per-call cost. The spec change (a panic without a location) was approved with the PRP.

Implemented PRP-025:
- The failing test first (`m8/stack_overflow.la` exited 139 and lost its output).
- The runtime handler; it now panics with exit 101 and keeps earlier output.
- An edge case found and fixed: non-overflow SIGSEGVs are re-raised, pinned by `m8/segv_from_c.la`.
- Spec §5/§7/§9 updated; the CLAUDE.md known issue removed.

239 tests pass.

### FILES CREATED OR MODIFIED

```
runtime/lugha_rt.c — _GNU_SOURCE, stack bounds, alternate stack, SIGSEGV handler
tests/programs/m8/stack_overflow.*, segv_from_c.* — new
docs/specs/Language v1 Specification.md — §5 panic list, §7 recursion depth, §9 lugha_rt_init
PRPs/prp-025-stack_overflow_panic.md — new PRP; implemented; amendments
CLAUDE.md (known issue removed), TODO.md, CHANGELOG.md, MEMORY.md (decisions 26, 30)
```

### TESTS WRITTEN

- `stack_overflow.la`: stdout `before`, stderr `panic: stack overflow`, exit 101, at -O0/-O2.
- `segv_from_c.la`: a C-raised SIGSEGV still exits 139.
- The runtime still compiles with `-Werror` (existing test).

### DECISIONS MADE

- MEMORY 30.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-025-stack_overflow_panic` fast-forward merged into `main` and deleted.

---

## SESSION 32 — 2026-10-12 — The swap intrinsic, Lugha v1.1 (PRP-026) — closed

Branch: main → prp-026-swap

### WHAT WAS DONE

Discovery (3 questions, each answered with the recommended option):
- a `swap(a, b)` intrinsic;
- E0509 only for "a part of itself";
- Lugha v1.1.

PRP-026 was approved with its spec changes.

Implemented PRP-026:
- The failing checker tests came first.
- `check/swap.rs`, with `PlaceUse` generalizing the place checks' wording.
- `codegen/swap.rs` (two loads, crossed stores).
- `tests/programs/v1_1/` (also cross-checked at -O2).
- Spec retitled v1.1 (§4 copy table, §5 intrinsic and rules, §9 E0509, §12).
- `sort_swap.la` measured: 95 → 5 copies, 154 → 109 ms.

246 tests pass.

Found, not fixed (TODO "Found while working"): an element write is lost when the same statement grows the array (`xs[0] = { xs.push(2); 9 };`). This predates swap.

### FILES CREATED OR MODIFIED

```
src/check/swap.rs — new: swap checking, E0509, tests
src/check/{assign,method_calls}.rs — PlaceUse replaces Option<&str>
src/check/errors/{places,types}.rs — for_use (replaces for_receiver), swap_with_part, swap_types
src/check/{array,call,env,mod}.rs — Path::depth/describe, swap dispatch, "swap" reserved
src/codegen/swap.rs — new: lowering, IR tests
src/codegen/{mod,runtime}.rs — module, dispatch
tests/programs/v1_1/ — swap.la, e0509.la, e0501_swap.la (+ expectations)
tests/codegen.rs — v1_1 in the -O0/-O2 cross-check
docs/specs/Language v1 Specification.md — v1.1
docs/decisions/decision-010.md, decision-010/sort_swap.la — "After PRP-026"
PRPs/prp-026-swap.md — new PRP; implemented; amendments
README.md, CLAUDE.md (tree), CHANGELOG.md, TODO.md, MEMORY.md (decision 31)
```

### TESTS WRITTEN

- **Checker (5):**
  - accepted places;
  - E0405, E0502, E0403, E0406;
  - E0501, E0506, E0507 with swap wording;
  - E0509 (and none for `g[0]`/`g[1]`);
  - E0302 for `fun swap`.
- **IR (2):** array swaps are two `ptr` loads and stores with no call; element swaps check both bounds before any store.
- **Acceptance:** `v1_1/swap.la` (expected output written by hand; arrays, elements, fields through `mut self`, bool/u8, enums, self-swap, merge sort); goldens for `e0509` and `e0501_swap`.

### DECISIONS MADE

- MEMORY 31.

### PENDING DECISIONS OPENED

- None. The lost-write item needs a spec ruling when it is picked up.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-026-swap` fast-forward merged into `main` and deleted; `main` tagged `v1.1`.
- Not from this session: the lost-write TODO and the oversized `runtime/lugha_rt.c` (see NEXT SESSION START POINT).

---

## SESSION 33 — 2026-10-12 — E0510: no changing what a statement writes, Lugha v1.2 (PRP-027) — closed

Branch: main → prp-027-place_change_check

### WHAT WAS DONE

Discovery (3 questions, each answered with the recommended option):
- reject at compile time;
- only changes to a strict container;
- Lugha v1.2.

PRP-027 was approved with its spec changes.

Implemented PRP-027:
- The failing checker tests came first.
- `check/conflicts.rs`: changes are recorded in `check_place_for`, and assignments, `swap` and `mut self` calls check them (E0510, which suppresses E0508).
- `tests/programs/v1_2/` (also cross-checked at -O2).
- Spec retitled v1.2 (changes note, a §5 paragraph, §9 E0510).

250 tests pass.

### FILES CREATED OR MODIFIED

```
src/check/conflicts.rs — new: Change, changed, changes_mark, no_change_inside, tests
src/check/{assign,swap,method_calls}.rs — marks and E0510 checks; check_place_for records changes
src/check/{mod,stmt}.rs — Checker.changes, reset per body
src/check/errors/places.rs — changed_while_writing
tests/programs/v1_2/ — places.la (+ .stdout/.exit), e0510.la, e0510_swap.la (+ .stderr)
tests/codegen.rs — v1_2 in the -O0/-O2 cross-check
docs/specs/Language v1 Specification.md — v1.2
PRPs/prp-027-place_change_check.md — new PRP; implemented; amendments
README.md, CLAUDE.md (tree), CHANGELOG.md, TODO.md, MEMORY.md (decision 32)
```

### TESTS WRITTEN

- **Checker (4):**
  - rejected assignments: push, clear in an index, an indexed container, replacement, pop in a compound assignment;
  - allowed: same depth, read, the variable itself, a shadowing variable, a sibling;
  - swap and receivers, with E0510 and not E0508;
  - the exact messages per verb and one report for a nested change.
- **Acceptance:** `v1_2/places.la` (expected output written by hand) and goldens for `e0510` and `e0510_swap`.

### DECISIONS MADE

- MEMORY 32.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-027-place_change_check` fast-forward merged into `main` and deleted; `main` tagged `v1.2`.
- Not from this session: `runtime/lugha_rt.c` is over 300 lines (see NEXT SESSION START POINT).

---

## SESSION 34 — 2026-10-12 — Split the C runtime into fragments (PRP-028) — closed

Branch: main → prp-028-runtime_split

### WHAT WAS DONE

Discovery (1 question, answered with the recommended option): fragments joined at build, not separate C modules and not an exemption. PRP-028 approved.

Implemented PRP-028:
- The failing `link.rs` tests came first.
- `runtime/lugha_rt.c` split verbatim into the core (143 lines), `stack.c` (72) and `print.c` (163). A diff confirms only comments were added.
- `link::RUNTIME_PARTS`; `RUNTIME_SOURCE` is `concat!` of the parts.

252 tests pass.

### FILES CREATED OR MODIFIED

```
runtime/lugha_rt.c — now the core only; layout comment
runtime/stack.c, runtime/print.c — new fragments (moved code)
src/link.rs — RUNTIME_PARTS, concat! RUNTIME_SOURCE, two tests
PRPs/prp-028-runtime_split.md — new PRP; implemented; amendments
CLAUDE.md (tree, stack, link line), CHANGELOG.md, TODO.md, MEMORY.md (decision 15 note)
```

### TESTS WRITTEN

- **The parts join in order:** they join to exactly `RUNTIME_SOURCE`, with the core first and `_GNU_SOURCE` before the first `#include`.
- **Fragment rules:** only the core has `#include`s, and each part is non-empty and at most 300 lines.
- **Existing safety net:** the `-Werror` runtime compile and every end-to-end program at `-O0` and `-O2`.

### DECISIONS MADE

- MEMORY 15 note.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-028-runtime_split` fast-forward merged into `main` and deleted.

---

## SESSION 35 — 2026-10-12 — Split check/stmt.rs and check/method_calls.rs (PRP-029) — closed

Branch: main → prp-029-checker_split

### WHAT WAS DONE

Discovery (1 question, answered with the recommended option): two new files rather than moving code into `flow.rs`/`array.rs`. PRP-029 approved.

Implemented PRP-029 as pure moves:
- `function`/`body` and one test to `check/body.rs`;
- `builtin` and its test to `check/builtins.rs`.

The moved blocks were checked to be verbatim. 252 tests pass, the same count as before.

### FILES CREATED OR MODIFIED

```
src/check/body.rs, src/check/builtins.rs — new (moved code and tests)
src/check/stmt.rs, src/check/method_calls.rs, src/check/mod.rs — code removed, imports, modules
PRPs/prp-029-checker_split.md — new PRP; implemented; amendments
CLAUDE.md (tree), CHANGELOG.md, TODO.md, MEMORY.md (state)
```

### TESTS WRITTEN

- None new (pure moves). The two moved tests pass in their new files.

### DECISIONS MADE

- None.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-029-checker_split` fast-forward merged into `main` and deleted.

---

## SESSION 36 — 2026-10-12 — v2 direction and spec draft — closed

Branch: main → v2-spec-draft

### WHAT WAS DONE

Discovery for v2 (8 questions, each answered with the recommended option):
- scope: generics, modules, richer patterns;
- form: a delta draft;
- two built-in bounds, `Eq`/`Ord`;
- type arguments inferred only;
- generic bodies checked once against their bounds;
- `Eq` for every type, `Ord` for numbers and strings;
- modules: file = module with qualified access, cycles allowed, per-item `pub`;
- milestones: M9 patterns → M10 modules → M11 generics.

Wrote `docs/specs/Language v2 Draft.md`, a delta against v1.2 with §1–§12, three acceptance programs and new codes. The parts of the examples that today's compiler accepts were run: the merged module example prints `3.5`, and a non-generic stack prints `b`. The user approved the draft.

### FILES CREATED OR MODIFIED

```
docs/specs/Language v2 Draft.md — new: the approved v2 delta
CLAUDE.md — stack note, tree, anti-pattern 5 points v2 features to milestone PRPs 9–11
TODO.md — milestones 9–11
MEMORY.md — decision 33, state
CHANGELOG.md — entry
```

### TESTS WRITTEN

- None (spec work); example outputs checked by hand.

### DECISIONS MADE

- The v2 direction (MEMORY 33).

### PENDING DECISIONS OPENED

- None (open questions are in the draft's §12).

### STILL OPEN AT CLOSE

- Nothing. Branch `v2-spec-draft` fast-forward merged into `main` and deleted.

---

## SESSION 37 — 2026-10-12 — v2 syntax and the gate (PRP-030) — closed

Branch: main → prp-030-v2_syntax

### WHAT WAS DONE

Discovery (1 question, answered with the recommended option): split M9 into syntax plus gate (030), `==`/`<` (031), and patterns (032). PRP-030 approved.

Implemented PRP-030:
- **Lexer:** `import` and `pub` keywords.
- **AST:** imports, `pub`, type parameters, `Applied` types, qualified struct literals, nested patterns, guards.
- **Parser:** a new `generic.rs` and `import.rs`; E0208.
- **Sexp:** forms for all of the above.
- **Gate:** `driver/gate/`, run in `pipeline::front`, exit 2.
- **Stack depth:** two boxings keep `Expr` small, after the E0206 depth test overflowed.
- **Splits:** two, each approved (`sexp/`, `gate/`).

All 4 code blocks in the draft's §10 parse. The real gate output on the draft programs was checked. 266 tests pass.

### FILES CREATED OR MODIFIED

```
src/lexer/{token,mod}.rs — import, pub
src/ast/{mod,expr,pattern}.rs, src/ast/header.rs (new) — v2 nodes
src/parser/{mod,item,decl,pattern,primary,expr,recover,describe}.rs — v2 parsing
src/parser/generic.rs, src/parser/import.rs (new); src/parser/sexp/{mod,item}.rs (split from sexp.rs)
src/driver/gate/{mod,walk}.rs (new), src/driver/{mod,pipeline}.rs — the gate
src/check/{env,expr,liveness,matching}.rs, src/codegen/{expr,matching,value}.rs — adapted to the new AST shapes
tests/parser.rs, tests/cli.rs — draft programs parse; gate exit 2
PRPs/prp-030-v2_syntax.md — new PRP; implemented; amendments
CLAUDE.md (tree, known issue), TODO.md, CHANGELOG.md, MEMORY.md (decision 34)
```

### TESTS WRITTEN

- **Lexer:** keywords.
- **Parser:**
  - imports, E0208, `pub`;
  - type parameters, `impl` headers, applied and qualified types;
  - qualified literals (including E0205);
  - nested patterns and guards;
  - E0206 for deep types and patterns.
- **Gate:** 5 tests covering every row, first-in-source-order, and v1 passing through.
- **Integration:** the v2 draft's §10 programs parse; `--emit=ast` exits 0 while `check` exits 2 (human and JSON).

### DECISIONS MADE

- MEMORY 34.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-030-v2_syntax` fast-forward merged into `main` and deleted.

---

## SESSION 38 — 2026-10-12 — Deep == and string < (PRP-031) — closed

Branch: main → prp-031-deep_equality

### WHAT WAS DONE

Discovery (2 questions, each answered with the recommended option):
- start the v2 spec file now;
- either operand types the other.

PRP-031 approved.

Implemented PRP-031:
- **Checker:** the operator rules and two-way operand typing, with failing tests first.
- **Runtime:** `lugha_rt_str_cmp`.
- **Codegen:** `codegen/equality.rs` with the `lugha_eq_*` helpers (IR tests first).
- **Acceptance:** `m9/equality.la` (expected output written by hand; it matches at -O0 and -O2) and `e0404_struct_order`.
- **Spec:** the new `Language v2 Specification.md` is `lugha::SPEC`.

272 tests pass.

### FILES CREATED OR MODIFIED

```
src/check/ops.rs (operators, defers, two-way typing), src/check/{enums,array}.rs (quiet on Error), src/check/stmt.rs
src/check/{access,array,enums,structs}.rs — tests updated for v2 equality
src/codegen/equality.rs (new), src/codegen/{arith,heap,copy,mod,runtime}.rs
runtime/lugha_rt.c — lugha_rt_str_cmp
docs/specs/Language v2 Specification.md (new), src/lib.rs (SPEC)
tests/programs/m9/ (equality, e0404_struct_order), tests/codegen.rs (m9), tests/cli.rs (spec title)
PRPs/prp-031-deep_equality.md — new PRP; implemented; amendments
CLAUDE.md (stack, rules 1 and 7, tree), README.md, TODO.md, CHANGELOG.md, MEMORY.md (decision 35)
```

### TESTS WRITTEN

- **Checker:** deep `==` on every type; string order; E0404 cases; two-way typing (6 forms); a single E0412.
- **IR:** one helper per struct with `!=` negated; array lengths before elements; recursive and `Option` helpers; `lugha_rt_str_cmp`.
- **Acceptance:** `m9/equality.la` (25 lines of expected output) and the `e0404_struct_order` golden; the spec title test.

### DECISIONS MADE

- MEMORY 35.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-031-deep_equality` fast-forward merged into `main` and deleted.

---

## SESSION 39 — 2026-10-13 — Nested patterns and guards (PRP-032) — closed

Branch: main → prp-032-nested_patterns

### WHAT WAS DONE

Discovery (3 questions, each answered with the recommended option):
- W0103 for unused bindings;
- keep the `switch` for flat matches and go arm by arm for nested or guarded ones;
- E0417 lists up to three missing cases, then "and N more".

PRP-032 approved.

Implemented PRP-032:
- **Checker:** `check/patterns.rs` and `check/usefulness.rs` (tests first); `check/matching.rs` rewritten for guards, W0103 and usefulness.
- **Codegen:** `codegen/patterns.rs` (IR tests first).
- **Gate:** the M9 entries are removed.
- **Spec:** the v2 spec folds in the pattern sections plus a §10 patterns program; the draft is updated.
- **Acceptance:** `m9/patterns.la` (expected output written by hand; matches at -O0 and -O2) and three goldens.

**Milestone 9 complete.** 281 tests pass.

### FILES CREATED OR MODIFIED

```
src/check/patterns.rs, src/check/usefulness.rs (new); src/check/{matching,types,mod}.rs; src/check/errors/{types,places}.rs
src/codegen/patterns.rs (new); src/codegen/{matching,mod}.rs; src/ast/pattern.rs (slot_binding removed)
src/driver/gate/{mod,walk}.rs — M9 entries removed
tests/programs/m9/ (patterns, e0417_nested, e0403_guard, w0103); tests/spec.rs (title list)
docs/specs/Language v2 Specification.md (patterns, W0103, §10 program, M9 done); Language v2 Draft.md (lowering note, §12)
PRPs/prp-032-nested_patterns.md — new PRP; implemented; amendments
CLAUDE.md (tree), README.md, TODO.md, CHANGELOG.md, MEMORY.md (decision 36)
```

### TESTS WRITTEN

- **Pattern typing:** nested bindings, bare names, E0403/E0416/E0415 at depth.
- **Usefulness:** complete nested matches, witnesses, "and N more", guards not counting, literals inside variants, combined coverage W0102.
- **`match`:** guards are `bool` and see bindings; W0103 for a typo and not for a guard-only use.
- **IR:** nested tests run outside in and a false guard falls through; indirect payloads load before the inner test; flat matches keep the `switch`.
- **Acceptance:** `m9/patterns.la` (19 lines), the `e0417_nested`, `e0403_guard` and `w0103` goldens, and the §10 program run from the spec.

### DECISIONS MADE

- MEMORY 36.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-032-nested_patterns` fast-forward merged into `main` and deleted; tagged `m9`.

---

## SESSION 40 — 2026-10-13 — v2 visibility amendment — closed

Branch: main → v2-visibility

### WHAT WAS DONE

The owner shared a visibility design note. I recommended adopting it for v2 with a package default, `private`, and `public` reserved until packages. When the first explanation was too dense, the owner asked for a plainer one. The owner chose the keywords `private`/`public` and approved the draft amendment.

### FILES CREATED OR MODIFIED

```
docs/specs/Language v2 Draft.md — decision 9; §2 keywords; §3 grammar (visibility, fields, methods); §5 qualified names;
                                  §6 visibility and no-leak rules; §8 externs; §9 E0313/E0317/E0318; §10 example (no pub);
                                  §11 milestone 10 row and v3 list; §12 decisions
docs/decisions/visibility.md — new: the owner's note plus its v2 adaptations
TODO.md (milestone 10), CHANGELOG.md, MEMORY.md (decision 37)
```

### TESTS WRITTEN

- None (spec work). The draft's §10 programs still parse (`tests/parser.rs`).

### DECISIONS MADE

- MEMORY 37 (replaces decision 33's visibility answer).

### PENDING DECISIONS OPENED

- None (the v3 questions are in the draft's §12).

### STILL OPEN AT CLOSE

- Nothing. Branch `v2-visibility` fast-forward merged into `main` and deleted.

---

## SESSION 41 — 2026-10-13 — Modules front end (PRP-033) — closed

Branch: main → prp-033-module_loading

### WHAT WAS DONE

Discovery (3 questions, each answered with the recommended option):
- two PRPs for milestone 10;
- one offset space;
- directory test cases.

PRP-033 approved, including the new code E0319.

Implemented PRP-033:
- **Syntax:** `private`/`public` (replacing `pub`), with field and method visibility.
- **Loading:** the `src/modules/` loader (E0312, E0314, E0319, cycles, errors per file, one offset space).
- **Driver:** a source map with file-aware human and JSON output, `--emit` per module, and the gate over every module.
- **Runner:** directory cases.
- **Tests and spec:** `tests/modules.rs`, the `m10/` goldens, and spec updates.
- **Approved split:** `driver/build.rs`.

297 tests pass.

### FILES CREATED OR MODIFIED

```
src/modules/{mod,resolve,tests}.rs (new), src/lib.rs
src/lexer/{token,mod}.rs; src/ast/{mod,header}.rs; src/parser/{import,item,decl,describe,recover}.rs; src/parser/sexp/item.rs
src/driver/{mod,pipeline,source,render,json,guard}.rs, src/driver/build.rs (new, split); src/driver/gate/{mod,walk}.rs
tests/support/runner/{discover,dirs (new),mod}.rs; tests/modules.rs (new); tests/programs/m10/
docs/specs/Language v2 Specification.md, Language v2 Draft.md (E0319)
PRPs/prp-033-module_loading.md — new PRP; implemented; amendments
CLAUDE.md (stages, tree, known issue), TODO.md, CHANGELOG.md, MEMORY.md (decision 38)
```

### TESTS WRITTEN

- **Loader (7):** order and bases, cycles and repeats, E0312, E0314 ×3, E0319, errors in imported files, internal errors.
- **Syntax:** lexer keywords; parser visibility forms and E0201s; gate visibility.
- **Driver:** offset → file, labels in other files (human and JSON).
- **Runner:** directory cases and a directory missing `main.la`.
- **CLI (`tests/modules.rs`):** `--emit` per module, the gate, errors naming the module's file.
- **Acceptance:** `m10` goldens `e0312`, `e0314/`, `parse_error/`.

### DECISIONS MADE

- MEMORY 38.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-033-module_loading` fast-forward merged into `main` and deleted.

---

## SESSION 42 — 2026-10-13 — Names across modules (PRP-034) — closed

Branch: main → prp-034-module_names

### WHAT WAS DONE

Discovery (2 questions, each answered with the recommended option):
- `/` after the module path in symbols;
- full module paths in messages.

PRP-034 approved.

Implemented PRP-034:
- **Ids:** one id space across modules.
- **Checker:** keys, every pass over every module, imports, `m.f`/`m.T`/`m.T { … }`/`m.T.f()`/`m.T.V`, the callee and annotation side tables, and visibility (E0313, E0317, E0318).
- **Codegen:** all modules, `/` symbols, panic locations per file.
- **Gate:** reduced to generics.
- **Spec:** module sections and the two-module §10 program, with the spec extraction now handling multi-file examples.
- **Approved splits:** three.

**Milestone 10 complete.** 305 tests pass.

### FILES CREATED OR MODIFIED

```
src/check/modules/{mod,lookup}.rs, src/check/visibility.rs, src/check/struct_literal.rs (new)
src/check/{mod,env,structs,enums,methods,method_calls,call,access,expr,body}.rs, src/check/errors/names.rs
src/codegen/declare.rs (new); src/codegen/{mod,lower,function,methods,expr,value,enums,runtime,heap}.rs
src/parser/mod.rs (parse_from), src/modules/{mod,tests}.rs; src/driver/{pipeline}.rs, src/driver/gate/{mod,walk}.rs
tests/modules.rs (run + IR symbols), tests/common/spec_programs.rs, tests/spec.rs
tests/programs/m10/ (two_modules/, modules/, e0313_field/, e0318/, e0317.la + expectations)
docs/specs/Language v2 Specification.md (milestone 10), Language v2 Draft.md (symbols)
PRPs/prp-034-module_names.md — new PRP; implemented; amendments
CLAUDE.md (rule 7, tree, known issue), README.md, TODO.md, CHANGELOG.md, MEMORY.md (decision 39)
```

### TESTS WRITTEN

- **Checker (`check/modules`):**
  - keys;
  - every cross-module form;
  - separate namespaces;
  - E0301/E0305/E0311/E0304/E0306;
  - full-path messages.
- **Visibility:** E0313 (functions, associated functions, methods, field read, assign and `swap`, literals), E0317 ×3, E0318 ×3, and private things allowed in their own module.
- **CLI and IR:** programs that import run; module symbols, separate `f`s, one shared extern.
- **Acceptance:**
  - `m10/two_modules` (`3.5`);
  - `m10/modules` (7 lines plus a panic in `range.la:12:7`, exit 101);
  - goldens `e0313_field`, `e0318`, `e0317`;
  - the spec's §10 two-module program at -O0/-O2.

### DECISIONS MADE

- MEMORY 39.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-034-module_names` fast-forward merged into `main` and deleted; tagged `m10`.

---

## SESSION 43 — 2026-10-13 — v2 interfaces amendment (milestone 12) — closed

Branch: main → v2-interfaces

### WHAT WAS DONE

The owner shared a generics and interfaces design note. I recommended keeping generics as M11 and adding structural interfaces as M12, keeping deep `==` (no overloading), and leaving explicit type arguments, `.len()` and `Self` out of v2. The owner chose that and approved the draft amendment.

### FILES CREATED OR MODIFIED

```
docs/specs/Language v2 Draft.md — decision 10; §1, §2 (interface keyword), §3 (interface_decl, bounds);
                                  §4 interfaces; §9 E0421/E0422; §10 interfaces program; §11 M12, v3 list; §12
docs/decisions/generics.md — new: the owner's note plus its v2 adaptations
tests/parser.rs — draft examples parse, except the M12 program (until M12)
TODO.md (milestone 12), CHANGELOG.md, MEMORY.md (decision 40)
```

### TESTS WRITTEN

- None new. The draft-examples parse test now expects 5 programs, with only the interfaces one waiting for M12.

### DECISIONS MADE

- MEMORY 40.

### PENDING DECISIONS OPENED

- None (`Self` and printing generic values are open questions in the draft's §12).

### STILL OPEN AT CLOSE

- Nothing. Branch `v2-interfaces` fast-forward merged into `main` and deleted.

---

## SESSION 44 — 2026-10-13 — Checking generics (PRP-035) — closed

Branch: prp-035-generic_checking

### WHAT WAS DONE

- Discovery: milestone 11 splits into PRP-035 (checking) and PRP-036 (compiling). PRP-035 written and approved.
- `Type::Struct`/`Type::Enum` carry type arguments; `Type::Param` added. Non-generic programs and their messages are unchanged.
- Generic declarations (`check/generics.rs`): parameter scopes, E0315, E0316, E0418, `Eq`/`Ord` reserved (E0302), and substitution of fields and variants.
- Bounds (`check/bounds.rs`): E0419, plus E0404 with "declare `T: Eq`" help for `==` or `<` without the bound.
- Inference (`check/infer.rs`): from the expected type, then arguments that type alone, then deferring ones. Methods bind their `impl`'s parameters from the receiver. E0412 when undecided. `Checked.instances` records every use's type arguments.
- The gate moved from before checking to before codegen: `lughac check` accepts generics, while `build`, `run` and `--emit=ir` exit 2.
- Spec: the "Changes since v1.2" sentence; E0315, E0316, E0418 and E0419 added to §9.
- Splits approved: `check/construct.rs` out of `enums.rs`, `check/bounds.rs` out of `generics.rs`; new `check/errors/generics.rs`.

### FILES CREATED OR MODIFIED

- New: `src/check/{generics,bounds,infer,construct}.rs`, `src/check/errors/generics.rs`, `tests/programs/m11/{e0418,e0419,e0404_eq}.{la,stderr}`, `PRPs/prp-035-generic_checking.md`.
- Modified:
  - the checker: `mod.rs`, `types.rs`, `env.rs`, `structs.rs`, `enums.rs`, `methods.rs`, `method_calls.rs`, `call.rs`, `expr.rs`, `struct_literal.rs`, `access.rs`, `ops.rs`, `body.rs`, `builtins.rs`, `modules/`, `errors/mod.rs`, plus mechanical `Type` pattern updates;
  - codegen: mechanical `Type` pattern updates only;
  - the driver: `pipeline.rs` (gate before codegen);
  - tests and docs: `tests/cli.rs`, the spec, CLAUDE.md, CHANGELOG, TODO, MEMORY (decision 41).

### TESTS WRITTEN

- `check/generics.rs`: declarations, E0315/E0316/E0302, E0418.
- `check/bounds.rs`: E0419; bare parameters allow only their bounds.
- `check/infer.rs`: from arguments, from the expected type, from methods' receivers; E0412/E0419/clashes; no cascade; `instances`; the draft's generics program checks.
- `tests/cli.rs`: generics check, but `run` and `--emit=ir` stop at the gate.
- The `m11/` reject goldens.
- 316 tests pass.

### DECISIONS MADE

- MEMORY 41. The PRP amendment: `max(1, "a")` is E0401 at `1`, following the inference order.

### PENDING DECISIONS OPENED

- None.

---

## SESSION 45 — 2026-10-13 — Compiling generics (PRP-036) — closed

Branch: prp-036-generic_compiling

### WHAT WAS DONE

- Discovery:
  - instances and E0420 are worked out in the checker;
  - `Option<T>` changes in the spec only;
  - one PRP finishes milestone 11.
- PRP-036 written and approved.
- Checker:
  - `check/instances.rs` lists instances from the non-generic bodies (E0420 past 64; a generic `main` is E0304);
  - `check/layouts.rs` lists concrete generic types and substituted tables (`Checked.used`; E0420 for growing types).
- Codegen: `codegen/instances.rs` declares and defines each instance under a substitution. Calls find their instance, and tables and helpers are keyed by the printed type.
- Driver: the gate is deleted (`driver/gate/`).
- Spec: the generics rules folded in (§3 grammar, §4, §6, §7, §8, §9 symbols, monomorphization, E0420; §10 generics program; §11 row 11). The title is "in progress: milestone 12".

### FILES CREATED OR MODIFIED

- New: `src/check/{instances,layouts}.rs`, `src/codegen/instances.rs`, `tests/programs/m11/{stack,list,copies,equality,e0420}.*`, `tests/programs/m11/modules/`, `PRPs/prp-036-generic_compiling.md`.
- Deleted: `src/driver/gate/`.
- Modified:
  - the checker: `mod.rs`, `types.rs`, `call.rs`, `method_calls.rs`, `methods.rs`, `builtins.rs`, `body.rs`, `env.rs`, `generics.rs`, `modules/mod.rs`, `errors/generics.rs`;
  - codegen: `lower.rs`, `value.rs`, `declare.rs`, `function.rs`, `methods.rs`, `structs.rs`, `enums.rs`, `copy.rs`, `equality.rs`;
  - the driver: `pipeline.rs`, `mod.rs`;
  - tests and docs: `tests/cli.rs`, `tests/modules.rs`, the spec and the draft, CLAUDE.md, README, CHANGELOG, TODO, MEMORY (decision 42).

### TESTS WRITTEN

- `check/instances.rs`: instance lists, recursion, E0420, generic `main`.
- `check/layouts.rs`: substituted tables, growing types.
- `tests/modules.rs`: instance symbols.
- `tests/cli.rs`: generics run.
- `m11/` run programs: stack, list, copies, equality, modules; the `e0420` golden.
- The §10 generics program, from the spec.

### DECISIONS MADE

- MEMORY 42. PRP amendments: concrete types in the checker, E0420 for growing types, helper names keep the length prefix.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-036-generic_compiling` fast-forward merged into `main` and deleted; tag `m11`.

---

## SESSION 46 — 2026-10-13 — Interfaces (milestone 12) — closed

Branch: prp-037-interfaces

### WHAT WAS DONE

- Discovery: one PRP for milestone 12; clashing bounds are E0423 at the bound list. PRP-037 written and approved.
- Syntax:
  - `interface` keyword;
  - `Item::Interface`;
  - qualified bounds `T: m.Shape`;
  - `parser/interface.rs`;
  - S-expressions.
- Checker:
  - `check/interfaces.rs`: declarations, bounds, E0315 help, E0423, interface as a type E0305;
  - `check/satisfy.rs`: calls through bounds E0421, satisfaction E0422;
  - calls through bounds feed the instance pass.
- Codegen: calls through bounds reach the concrete method or instance; `T.f(…)` has no receiver.
- Spec complete: §2 keyword, §3 grammar, §4 "Interfaces", §6 names, §9 codes E0421–E0423, §10 interfaces program, §11 row 12; the title is no longer "in progress".
- Approved split: `check/test_util.rs` out of `check/mod.rs`, moved verbatim.

### FILES CREATED OR MODIFIED

- New: `src/parser/interface.rs`, `src/check/{interfaces,satisfy,test_util}.rs`, `tests/programs/m12/` (shapes, generic_impl, bounds, modules/, e0421–e0423).
- Modified:
  - lexer, AST and parser (`token.rs`, `header.rs`, `mod.rs`, `generic.rs`, `item.rs`, `import.rs`, `decl.rs`, `recover.rs`, `describe.rs`, `sexp/item.rs`);
  - the checker (`mod.rs`, `generics.rs`, `bounds.rs`, `instances.rs`, `method_calls.rs`, `env.rs`, `structs.rs`, `enums.rs`, `methods.rs`, `visibility.rs`, `modules/`, `errors/generics.rs`);
  - codegen (`methods.rs`, `declare.rs`);
  - docs: the spec and the draft, CLAUDE.md, README, CHANGELOG, TODO, MEMORY (decision 43).

### TESTS WRITTEN

- Parser: interfaces and bounds.
- `check/interfaces.rs`: declarations, bounds, E0423, interface as a type, the draft's program.
- `check/satisfy.rs`: calls, E0421, E0422 (missing, mismatched, receiver kind, another module's private method), generic types and parameters satisfying.
- `check/instances.rs`: calls through bounds reach generic methods.
- `m12/`: four run programs and three reject goldens.
- The §10 interfaces program, run from the spec.

### DECISIONS MADE

- MEMORY 43; PRP amendments (E0201 for a body, E0415 for the wrong kind of call, E0422 wording, no E0318 for bounds).

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-037-interfaces` fast-forward merged into `main` and deleted; tags `m12` and `v2`.

---

## SESSION 47 — 2026-10-13 — v3 direction — closed

Branch: main

### WHAT WAS DONE

- v3 discovery, five answers:
  1. headline: closures;
  2. capture: by copy, read-only;
  3. syntax: `fun` everywhere;
  4. additions: args, parsing and interpolation;
  5. helpers: built-in array methods, with interpolation in every string literal.
- Wrote `docs/specs/Language v3 Draft.md`, approved: milestones 13–16, codes E0111, E0209, E0511, symbols `lugha_closure_*`/`lugha_ref_*`, and four §10 programs.

### FILES CREATED OR MODIFIED

- New: `docs/specs/Language v3 Draft.md`.
- Modified: MEMORY (decision 44), TODO (v3 milestones), CHANGELOG, CONTEXT.

### TESTS WRITTEN

- None (a design session).

### DECISIONS MADE

- MEMORY 44.

### PENDING DECISIONS OPENED

- None. The draft's §12 lists four minor open questions to settle in their milestone's PRP.

### STILL OPEN AT CLOSE

- Nothing. Work is on `main`.

---

## SESSION 48 — 2026-10-13 — Input and interpolation (milestone 13) — closed

Branch: prp-038-input_and_interpolation

### WHAT WAS DONE

- Discovery: the v3 spec starts now; tests pass arguments through `.args`. PRP-038 written and approved.
- Lexer:
  - interpolation (`lexer/interp.rs`): `StrStart`/`StrMid`/`StrEnd` and E0111;
  - parser: `ExprKind::Interp`.
- Checker: `check/input.rs` (interpolation, `args`, `parse_i64`/`parse_f64`), `env::is_intrinsic`; liveness split into `check/liveness/{mod,walk}.rs` (approved).
- Codegen: `codegen/input.rs`; runtime `runtime/input.c`; the C `main(argc, argv)`.
- Driver: `lughac run … -- args`.
- Tests: the runner reads `.args`; the spec extractor reads "Run as `prog …`.".
- Spec: `Language v3 Specification.md` created (in progress: milestone 13), printed by `lughac spec`; v2 frozen; the draft's §10 sentence amended.

### FILES CREATED OR MODIFIED

- New:
  - `src/lexer/interp.rs`, `src/check/input.rs`, `src/check/liveness/{mod,walk}.rs` (from `liveness.rs`), `src/codegen/input.rs`, `runtime/input.c`;
  - `tests/support/runner/args.rs`, `tests/programs/m13/`;
  - `docs/specs/Language v3 Specification.md`, the PRP.
- Modified:
  - lexer, parser, AST, sexp;
  - the checker (`call.rs`, `env.rs`, `expr.rs`, `mod.rs`, `structs.rs`, `enums.rs`, `interfaces.rs`, `errors/types.rs`);
  - codegen (`expr.rs`, `function.rs`, `lower.rs`, `runtime.rs`, `mod.rs`);
  - `link.rs`, the driver;
  - tests (`cli.rs`, `spec.rs`, `common/spec_programs.rs`, the runner);
  - the specs, CLAUDE.md, README, TODO, CHANGELOG, MEMORY (decision 45).

### TESTS WRITTEN

- Lexer interpolation (5), parser (1), checker input (3), runner `.args` (2).
- `m13/`: double (+ `.args`), usage, args (an argument containing a space), parse (21 cases), interp, e0111.
- The §10 arguments program, from the spec, with `21`.
- 347 tests pass.

### DECISIONS MADE

- MEMORY 45; PRP amendments.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-038-input_and_interpolation` fast-forward merged into `main` and deleted; tag `m13`.

---

## SESSION 49 — 2026-10-13 — Functions as values (milestone 14) — closed

Branch: prp-039-function_values

### WHAT WAS DONE

- Discovery:
  - arrays of functions need parentheses, `(fun(i64): i64)[]`;
  - `s.f(x)` calls a field when there's no method `f`.

  PRP-039 written and approved.
- Syntax: function types and parenthesized types.
- Checker:
  - `Type::Fun`;
  - `check/fun_values.rs`: function values, calls of values, field functions;
  - `holds_fun` makes functions not `Eq`;
  - E0409 for externs;
  - generic function values feed the instance pass.
- Codegen: `codegen/fun_values.rs` (`{ code, env }` pairs, `lugha_ref_` wrappers, indirect calls).
- Spec: v3 spec folded to milestone 14; the draft's §3 note amended.

### FILES CREATED OR MODIFIED

- New: `src/check/fun_values.rs`, `src/codegen/fun_values.rs`, `tests/programs/m14/`, the PRP.
- Modified:
  - the parser (`item.rs`, `generic.rs`, `sexp/item.rs`) and the AST;
  - the checker (`types.rs`, `call.rs`, `expr.rs`, `env.rs`, `mod.rs`, `method_calls.rs`, `bounds.rs`, `generics.rs`, `infer.rs`, `layouts.rs`, `visibility.rs`, `errors/types.rs`);
  - codegen (`value.rs`, `expr.rs`, `function.rs`, `methods.rs`, `structs.rs`, `input.rs`, `mod.rs`);
  - tests (`tests/spec.rs`);
  - docs: the specs, CLAUDE.md, README, TODO, CHANGELOG, MEMORY (decision 46).

### TESTS WRITTEN

- Parser: function types.
- Checker (7 tests): values, non-values, calls, equality, printing, modules.
- Codegen: wrappers in IR.
- `m14/`: values, generic, modules, and the e0406/e0404 goldens.
- The §10 program.

### DECISIONS MADE

- MEMORY 46; PRP amendments.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-039-function_values` fast-forward merged into `main` and deleted; tag `m14`.

---

## SESSION 50 — 2026-10-13 — Closures (milestone 15) — closed

Branch: prp-040-closures

### WHAT WAS DONE

- Discovery: a closure body is its own function (`return` leaves it; E0504 for outer loops). PRP-040 written and approved.
- Parser: `parser/closure.rs` (closure literals, E0209).
- Checker:
  - `check/closures.rs`: types, parameter inference, captures recorded through nested closures, E0511, E0412 for an unknown return type with `return`;
  - closures last in generic inference;
  - liveness enters closure bodies; captures never move.
- Codegen: `codegen/closures.rs` (environments, `lugha_closure_*` bodies, state saved and restored).
- Spec: v3 spec folded to milestone 15; draft amended (always copy, empty captures resolved).

### FILES CREATED OR MODIFIED

- New: `src/parser/closure.rs`, `src/check/closures.rs`, `src/codegen/closures.rs`, `tests/programs/m15/`, the PRP.
- Modified:
  - the parser (`primary.rs`, `mod.rs`, `sexp/mod.rs`) and the AST;
  - the checker (`mod.rs`, `expr.rs`, `assign.rs`, `stmt.rs`, `ops.rs`, `infer.rs`, `liveness/walk.rs`, `modules/mod.rs`, `errors/`);
  - codegen (`expr.rs`, `lower.rs`, `mod.rs`);
  - tests (`tests/spec.rs`);
  - docs: the specs, CLAUDE.md, README, TODO, CHANGELOG, MEMORY (decision 47).

### TESTS WRITTEN

- Parser (3).
- Checker (6): types, E0412, own function, E0511, captures, generics.
- Codegen IR (1).
- `m15/`: adder, capture, nested, generic, and the e0511/e0209 goldens.
- The §10 closures program.

### DECISIONS MADE

- MEMORY 47; PRP amendments.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-040-closures` fast-forward merged into `main` and deleted; tag `m15`.

---

## SESSION 51 — 2026-10-13 — Array helpers (milestone 16) — closed

Branch: prp-041-array_helpers

### WHAT WAS DONE

- Discovery: the helpers are written in Lugha in a bundled prelude (amends the draft's "inline" note). PRP-041 written and approved.
- Prelude: `src/prelude.la` (`map`, `filter`, `fold`, `sort_by` stable merge sort, `any`, `all`) as module `$prelude`, appended on the checking path (`modules::with_prelude`, `pipeline::front`); errors in it are internal.
- Checker:
  - `check/helpers.rs`: array helper calls;
  - `infer::bind`;
  - partial closure hints, so `U` comes from a closure body;
  - test helpers load the prelude.
- Codegen: helper calls go to prelude instances with the receiver first.
- Spec: v3 complete (title, status, §4 helpers, §9 prelude, §10 program, §11 row 16); draft notes amended or resolved.

### FILES CREATED OR MODIFIED

- New: `src/prelude.la`, `src/check/helpers.rs`, `tests/programs/m16/`, the PRP.
- Modified:
  - `src/lib.rs` (`PRELUDE`), `src/modules/mod.rs`, the driver (`pipeline.rs`, `source.rs`);
  - the checker (`mod.rs`, `method_calls.rs`, `infer.rs`, `closures.rs`, `test_util.rs`);
  - codegen (`methods.rs`);
  - tests (`cli.rs`, `spec.rs`, `modules.rs`);
  - docs: the specs, CLAUDE.md, README, TODO, CHANGELOG, MEMORY (decision 48).

### TESTS WRITTEN

- Checker (4): types, chaining, errors, names.
- Driver: the prelude in emit output.
- `m16/`:
  - helpers;
  - sort (stability, unchanged receiver, 1000 elements);
  - shortcircuit;
  - structs;
  - the e0403 golden.
- The §10 array-helpers program.

### DECISIONS MADE

- MEMORY 48; PRP amendments.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-041-array_helpers` fast-forward merged into `main` and deleted; tags `m16` and `v3`.

---

## SESSION 52 — 2026-10-13 — v4 direction — closed

Branch: main

### WHAT WAS DONE

- v4 discovery, five answers:
  1. headline: input and output;
  2. errors as `Result<T, E>`;
  3. the `?` operator for `Result` and `Option`;
  4. string and `Option`/`Result` helpers;
  5. the I/O surface `read_line`, `read_file`, `write_file`, `append_file`, `eprint`/`eprintln`, `exit`.
- Wrote `docs/specs/Language v4 Draft.md`, approved: milestones 17–19, code E0424, and three §10 programs.

### FILES CREATED OR MODIFIED

- New: `docs/specs/Language v4 Draft.md`.
- Modified: MEMORY (decision 49), TODO (v4 milestones), CHANGELOG, CONTEXT.

### TESTS WRITTEN

- None (a design session).

### DECISIONS MADE

- MEMORY 49.

### PENDING DECISIONS OPENED

- None. The draft's §12 lists four questions to settle in their milestone's PRP.

### STILL OPEN AT CLOSE

- Nothing. Work is on `main`.

---

## SESSION 53 — 2026-10-13 — Results and ? (milestone 17) — closed

Branch: prp-042-results

### WHAT WAS DONE

- Discovery: `Result` is a prelude enum. PRP-042 written and approved.
- Lexer and parser: `?` (`Question`), postfix `ExprKind::Try`.
- Prelude: `enum Result<T, E> { Ok(T), Err(E) }`.
- Checker:
  - `check/results.rs`: names, bare `Ok`/`Err`, `?` typing, E0424 (in `errors/v4.rs`);
  - `Display` strips `$prelude.`;
  - built-in names reserved.
- Codegen: `codegen/results.rs` (`?` branch and early return); `Ok`/`Err` construction.
- Spec: `Language v4 Specification.md` created (in progress: milestone 17) and printed by `lughac spec`; v3 frozen; the draft's §12 conversion question resolved.

### FILES CREATED OR MODIFIED

- New:
  - `src/check/results.rs`, `src/check/errors/v4.rs`, `src/codegen/results.rs`, `tests/programs/m17/`;
  - `docs/specs/Language v4 Specification.md`, the PRP.
- Modified:
  - `src/prelude.la`, the lexer, parser and AST;
  - the checker (`mod.rs`, `expr.rs`, `construct.rs`, `enums.rs`, `env.rs`, `types.rs`, `modules/lookup.rs`, `liveness/walk.rs`, `errors/mod.rs`);
  - codegen (`expr.rs`, `construct.rs`, `equality.rs`, `mod.rs`);
  - `src/lib.rs`, tests (`cli.rs`, `spec.rs`);
  - docs: the specs, CLAUDE.md, README, TODO, CHANGELOG, MEMORY (decision 50).

### TESTS WRITTEN

- Parser (4 `?` cases).
- Checker (6): construction, patterns, names, `?` types, E0424, closures.
- `m17/`: results, try, the e0424 golden.
- The §10 results program.

### DECISIONS MADE

- MEMORY 50; PRP amendments.

### PENDING DECISIONS OPENED

- None. A bug in the v4 draft's M19 example is recorded in TODO.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-042-results` fast-forward merged into `main` and deleted; tag `m17`.

---

## SESSION 54 — 2026-10-13 — Text and option helpers (milestone 18) — closed

Branch: prp-043-text_helpers

### WHAT WAS DONE

- Discovery: exactly the draft's set. PRP-043 written and approved.
- Prelude: the string helpers (`str_*`), `str_join`, and the `option_*` and `result_*` helpers.
- Checker:
  - dispatch by receiver (`helper_function`);
  - `slice` built in;
  - prelude calls recorded for the instance pass, with prelude bodies not roots.
- Codegen:
  - helper calls with the receiver first, for every receiver;
  - `lugha_rt_str_slice`;
  - prelude functions compile only when reached.
- Runtime: `lugha_rt_str_slice`. `RUNTIME_SYMBOLS` moved to `link.rs` (approved).
- Spec: folded to milestone 18; the draft's §9 note amended and §12 `\r` question resolved.

### FILES CREATED OR MODIFIED

- New: `tests/programs/m18/`, the PRP.
- Modified:
  - `src/prelude.la`;
  - the checker (`helpers.rs`, `builtins.rs`, `method_calls.rs`, `instances.rs`);
  - codegen (`methods.rs`, `function.rs`, `instances.rs`, `input.rs`, `declare.rs`, `lower.rs`, `runtime.rs`, `mod.rs`);
  - `src/link.rs`, `runtime/lugha_rt.c`;
  - tests (`tests/modules.rs`, `tests/spec.rs`);
  - docs: the specs, CLAUDE.md, README, TODO, CHANGELOG, MEMORY (decision 51).

### TESTS WRITTEN

- Checker (2 new): string types and errors; `Option`/`Result` types and `?` with `ok_or`.
- Modules: only reached prelude functions compile.
- `m18/`: strings, options, `slice_panic`, `split_panic`, the e0415 golden.
- The §10 text-helpers program.

### DECISIONS MADE

- MEMORY 51; PRP amendments.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-043-text_helpers` fast-forward merged into `main` and deleted; tag `m18`.

---

## SESSION 55 — 2026-10-13 — Input and output (milestone 19) — closed

Branch: prp-044-input_output

### WHAT WAS DONE

- Discovery: scratch directories are opt-in through `.scratch`. PRP-044 written and approved.
- Checker: the I/O intrinsics' types (`input.rs`, `call.rs` for `eprint`).
- Codegen: `codegen/io.rs`.
- Runtime: read_line, read_file, write_file (with an append flag), eprint, exit in `input.c`.
- Runner: `Extras` (`.args`, `.stdin`, `.scratch`), stdin piping, scratch directories.
- Extractor: stdin from the §10 sentence.
- Spec: v4 complete; the draft's §10 program corrected (two invalid arms) and its §12 questions resolved.

### FILES CREATED OR MODIFIED

- New: `src/codegen/io.rs`, `tests/programs/m19/`, the PRP.
- Modified:
  - the checker (`input.rs`, `call.rs`, `errors/types.rs`);
  - codegen (`function.rs`, `input.rs`, `mod.rs`);
  - `runtime/input.c`, `runtime/lugha_rt.c`, `src/link.rs`;
  - the runner (`args.rs`, `discover.rs`, `execute.rs`, `mod.rs`, `report.rs`), `tests/common/spec_programs.rs`, `tests/spec.rs`, `tests/cli.rs`;
  - docs: the specs, CLAUDE.md, README, TODO, CHANGELOG, MEMORY (decision 52).

### TESTS WRITTEN

- Checker (2): I/O types and errors.
- Runner (3): stdin and scratch discovery, stdin piping, scratch directory.
- `m19/`: sum (+ `.stdin`), empty, files (+ `.scratch`), stderr, exit, and the e0403 golden.
- The §10 reading-input program, with its stdin.

### DECISIONS MADE

- MEMORY 52; PRP amendments.

### PENDING DECISIONS OPENED

- None.

### STILL OPEN AT CLOSE

- Nothing. Branch `prp-044-input_output` fast-forward merged into `main` and deleted; tags `m19` and `v4`.

---

## SESSION 56 — 2026-10-13 — String literals inside interpolations — closed

Branch: prp-045-nested_strings

### WHAT WAS DONE
- Discovery: v4.1 point release (string literals inside `{…}`) chosen over starting v5. PRP-045 written and approved.
- Lexer: open interpolations form a stack; `"` inside `{…}` lexes a nested string; the `quote_in_interpolation` heuristic is gone. A string running past its line inside an interpolation is E0111 at that `{`.
- Spec: v4 §2 "String interpolation", the E0111 row, the status line (v4.1).
- Docs: CHANGELOG, MEMORY 53, TODO, README, CLAUDE.md (STACK, tree).

### FILES CREATED OR MODIFIED
- `src/lexer/interp.rs`, `src/lexer/mod.rs`, `src/parser/primary.rs` (test)
- `tests/programs/v4_1/` (`nested.la`/`.stdout`/`.exit`, `e0111.la`/`.stderr`)
- `docs/specs/Language v4 Specification.md`, `PRPs/prp-045-nested_strings.md`, CHANGELOG.md, MEMORY.md, TODO.md, README.md, CLAUDE.md

### TESTS WRITTEN
- Lexer: `string_literals_and_interpolations_nest`; `mistakes_are_e0111` rewritten with spans (lone `}`, `"{x"`, `{}`, `{ }`, `"{ "a" "`, a line break in a nested string).
- Parser: `"<{xs.join("-")}>"` parses to `(interp "<" (call (. xs join) "-") ">")`.
- Acceptance: `v4_1/nested.la` (run), `v4_1/e0111.la` (reject golden). `m13/e0111` unchanged.

### DECISIONS MADE
- MEMORY 53: nested strings in interpolations (v4.1).

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-045 merged to `main` (fast-forward), tagged `v4.1`, branch deleted.

---

## SESSION 57 — 2026-10-13 — v5 direction — closed

Branch: main

### WHAT WAS DONE

- PRP-045 (v4.1) merged and tagged in session 56; this session opened afterwards.
- v5 discovery, six answers:
  1. headline: interface values (interfaces only);
  2. conversion implicit only at a written interface type;
  3. interface values are values, copied like structs;
  4. type patterns in `match` (`c: Circle =>`);
  5. extras: `?` converting into an interface error, and default methods (`Self` and generic interfaces wait);
  6. no `==` on interface values.
- Wrote `docs/specs/Language v5 Draft.md`, approved: milestones 20–22, codes E0425 and E0426, three §10 programs.

### FILES CREATED OR MODIFIED

- New: `docs/specs/Language v5 Draft.md`.
- Modified: MEMORY (decision 54), TODO (v5 milestones), CHANGELOG, CONTEXT.

### TESTS WRITTEN

- None (a design session).

### DECISIONS MADE

- MEMORY 54.

### PENDING DECISIONS OPENED

- None. The draft's §12 lists three questions to settle in their milestone's PRP.

### STILL OPEN AT CLOSE

- Nothing. Work is on `main`.

---

## SESSION 58 — 2026-10-13 — Checking interface values (milestone 20, part 1) — closed

Branch: prp-046-interface_value_checking

### WHAT WAS DONE
- Discovery: milestone 20 is two PRPs (checking, then compiling). PRP-046 written and approved.
- Approved split: `arguments`/`unchecked` moved verbatim from `check/method_calls.rs` to `check/arguments.rs`.
- Checker: `Type::Interface`, E0425, conversion in `Checker::expr` (`Checked.conversions`), calls through interface values, satisfaction by interface values, no `==`, `is_deep` (renamed from `contains_array`).
- Driver: the milestone 20 gate before codegen.

### FILES CREATED OR MODIFIED
- New: `src/check/{arguments,iface_values,iface_calls}.rs`, `src/check/errors/v5.rs`, `tests/interface_values.rs`, `tests/programs/m20/` (`e0425`, `e0422`).
- Modified: `src/check/{types,mod,expr,interfaces,generics,satisfy,bounds,method_calls,access,ops,env,test_util}.rs`, `src/check/errors/mod.rs`, `src/check/modules/mod.rs`, `src/codegen/value.rs` (one `unreachable!` arm), `src/driver/pipeline.rs`; `is_deep` rename across codegen.
- Docs: PRP-046, CLAUDE.md tree, CHANGELOG, TODO, MEMORY 55, CONTEXT.

### TESTS WRITTEN
- `check/iface_values.rs`: types, another module's interface, every conversion site, the recorded conversion, no conversion without an interface expected, E0422 (and another module's private method), type parameters converting.
- `check/iface_calls.rs`: calls and callee keys, E0415/E0405/E0403, `mut self` places (E0501/E0502), fields (E0410 + help), equality (E0404/E0419), printing (E0403), extern (E0409), `is_deep`.
- `check/interfaces.rs`: E0425 replaces the old E0305 test.
- `tests/interface_values.rs`: `check` accepts the §10 program; `build`, `run`, `--emit=ir` stop at the gate.
- `tests/programs/m20/e0425`, `e0422` reject goldens.

### DECISIONS MADE
- MEMORY 55.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-046 merged to `main` (fast-forward), branch deleted. No tag: `m20` comes with PRP-047.

---

## SESSION 59 — 2026-10-13 — Compiling interface values (milestone 20, part 2) — closed

Branch: prp-047-interface_value_compiling

### WHAT WAS DONE
- PRP-047 written and approved; no new discovery questions (semantics settled in sessions 57–58).
- Checker: `Checked.interfaces`; conversions record their methods as used (generic types' methods instantiated); number literals convert (approved mid-build).
- Codegen: `codegen/interfaces.rs` — `{ table, box }` values, `lugha_vt_` tables (identity, box copy, methods), conversion, dynamic calls, interface copies. Gate removed.
- Spec: `Language v5 Specification.md` begins (in progress: milestone 20) and `lughac spec` prints it; v4 frozen; draft §9 amended (no `lugha_dyn_` wrappers).

### FILES CREATED OR MODIFIED
- New: `src/codegen/{interfaces,dispatch}.rs`, `src/check/iface_tables.rs` (approved split of `iface_values.rs`), `docs/specs/Language v5 Specification.md`, `tests/programs/m20/{shapes,copies,any}.*`, `tests/programs/m20/generic/` + `generic.*`.
- Modified: `src/check/{iface_values,literal,mod}.rs`, `src/check/modules/mod.rs`, `src/codegen/{mod,lower,value,structs,expr,methods,copy}.rs`, `src/driver/pipeline.rs` (gate removed), `src/lib.rs`, `tests/{interface_values,cli,spec}.rs`.
- Docs: PRP-047, the v5 draft, CLAUDE.md (STACK, rule 7, anti-patterns 4–5, tree), README, CHANGELOG, TODO, MEMORY 56, CONTEXT.

### TESTS WRITTEN
- `codegen/interfaces.rs`: one table per pair, methods in order, boxes through `lugha_rt_alloc`, indirect calls, copies through the copy slot, no tables without interface values.
- `check/iface_values.rs`: literals convert; converted generic types' methods are instances; `Checked.interfaces` order.
- `tests/programs/m20/`: `shapes`, `copies`, `any`, `generic/` (run cases).
- `tests/interface_values.rs` runs the §10 program; `tests/spec.rs` requires it; `tests/cli.rs` checks the v5 title.

### DECISIONS MADE
- MEMORY 56.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-047 merged to `main` (fast-forward), tagged `m20`, branch deleted.

---

## SESSION 60 — 2026-10-13 — Type patterns (milestone 21) — closed

Branch: prp-048-type_patterns

### WHAT WAS DONE
- The user went out and chose "build m21 and m22, hold merges": PRP-048 written as pre-approved, with the decisions taken while away listed in it.
- Type patterns end to end: parser (`c: Circle`, `_: T`, scrutinee fix), checker (`check/type_patterns.rs`, E0426, `Ctor::Type`), codegen (`dispatch.rs::type_test`).
- Spec: v5 at "in progress: milestone 21" (§3 grammar, §4 type patterns, E0426, §10 program, §11).

### FILES CREATED OR MODIFIED
- New: `src/check/type_patterns.rs`, `tests/programs/m21/` (`describe`, `nested/` + `geo.la`, `e0426`, `e0417`).
- Modified: `src/ast/pattern.rs`, `src/parser/{mod,pattern,primary}.rs`, `src/parser/sexp/mod.rs`, `src/check/{mod,patterns,usefulness,satisfy}.rs`, `src/check/errors/v5.rs`, `src/codegen/{patterns,matching,dispatch,interfaces}.rs`, `tests/spec.rs`, the v5 spec.
- Docs: PRP-048, CLAUDE.md, README, CHANGELOG, TODO, MEMORY 57, CONTEXT.

### TESTS WRITTEN
- Parser: type patterns in an arm and nested, `c:` with no type is E0201.
- Checker: bindings, E0426 (five ways), E0417 needing `_`, W0102 unless guarded, immutable bindings.
- Codegen: identity-slot comparison, the arm chain.
- `tests/programs/m21/`; the §10 type-patterns program from the spec.

### DECISIONS MADE
- MEMORY 57 (pre-approved decisions 1–5 in PRP-048, for the user's review).

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- **PRP-048 is not merged:** the branch `prp-048-type_patterns` waits for the user's review and merge; tag `m21` then.

---

## SESSION 61 — 2026-10-13 — Default methods and error conversion (milestone 22) — closed

Branch: prp-049-default_methods

### WHAT WAS DONE
- PRP-049 written as pre-approved (the user away, "build m21 and m22, hold merges"), on a branch stacked on `prp-048-type_patterns`.
- Default methods: parsed (`InterfaceItem.default`), checked once as generic methods of the interface with a hidden `Self`, compiled per type that uses them (`lugha_fn_<I>.<m><T>`), through tables and bounds.
- `?` converts an error into an interface error (`Checked.try_conversions`; boxed on the failure branch).
- Spec: `Language v5 Specification.md` complete.

### FILES CREATED OR MODIFIED
- New: `src/check/defaults.rs`, `src/codegen/defaults.rs`, `tests/programs/m22/` (`errors`, `defaults`, `e0415`).
- Modified: `src/ast/header.rs`, `src/parser/{interface,decl}.rs`, `src/parser/sexp/item.rs`, `src/check/{interfaces,satisfy,instances,methods,method_calls,generics,iface_tables,results,mod}.rs`, `src/check/modules/mod.rs`, `src/codegen/{instances,interfaces,methods,results,lower,mod}.rs`, `tests/{interface_values,spec}.rs`, the v5 spec.
- Docs: PRP-049, CLAUDE.md, README, CHANGELOG, TODO, MEMORY 58, CONTEXT.

### TESTS WRITTEN
- Parser: default bodies (`=` and block); a default without `self` is E0201.
- Checker (`defaults.rs`): satisfaction with a missing defaulted item, E0422 for a clash, the body's limits (E0421, E0410, E0305, E0403), E0415 with help, only used defaults instantiated; `results.rs`: `?` conversion and two E0424 cases.
- Codegen: a table slot pointing at the default's instance; `?` boxing on the failure path (binary test).
- `tests/programs/m22/`; the §10 errors-as-interfaces program from the spec.

### DECISIONS MADE
- MEMORY 58 (pre-approved decisions 1–5 in PRP-049, for the user's review).

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- **PRP-049 is not merged:** the branch `prp-049-default_methods` (on top of `prp-048-type_patterns`) waits for the user's review; then tags `m22` and `v5`.

---

## SESSION 62 — 2026-10-13 — Merge milestones 21 and 22; v5 — closed
Branch: main

### WHAT WAS DONE
- The user reviewed sessions 60–61 (built while away) and said "merge".
- `main` fast-forwarded to `prp-049-default_methods` (which contains `prp-048-type_patterns`).
- Tags: `m21` on the PRP-048 commit, `m22` and `v5` on this close commit.
- PRP-048 and PRP-049 marked merged; both branches deleted locally and remotely.

### FILES CREATED OR MODIFIED
- PRP-048, PRP-049 (status), MEMORY.md (state, start point), CONTEXT.md.

### TESTS WRITTEN
- None (merge only; CI checked on `main`).

### DECISIONS MADE
- The "decisions taken while away" in PRP-048 and PRP-049 (MEMORY 57–58) stand as approved.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing.

---

## SESSION 63 — 2026-10-13 — v6 direction — closed
Branch: main

### WHAT WAS DONE
- v6 discovery, four answers:
  1. headline: generic interfaces and `Self`;
  2. an interface using `Self` is a bound only (E0427 as a type);
  3. generic interfaces as bounds and values, with type arguments inferred from methods;
  4. interface-to-interface conversion when the source declares the target's items; extra: `Iter<T>` in the prelude with `for … of`.
- Wrote `docs/specs/Language v6 Draft.md`, approved: milestones 23–26, code E0427, four §10 programs. Five gap choices listed in the draft (copy iteration, own `Iter` hides the prelude's, bounded interface parameters, `Self` always the implementing type, conversion needs every item).

### FILES CREATED OR MODIFIED
- New: `docs/specs/Language v6 Draft.md`.
- Modified: MEMORY (decision 59, state), TODO (v6 milestones), CHANGELOG, CLAUDE.md (tree), CONTEXT.

### TESTS WRITTEN
- None (a design session).

### DECISIONS MADE
- MEMORY 59.

### PENDING DECISIONS OPENED
- None. The draft's §12 lists two questions (iterating a copy; one `X` per type).

### STILL OPEN AT CLOSE
- Nothing. Work is on `main`.

---

## SESSION 64 — 2026-10-13 — Generic interfaces as bounds (milestone 23, part 1) — closed
Branch: prp-050-generic_interface_bounds (merged into main)

### WHAT WAS DONE
- Discovery: milestone 23 is two PRPs, bounds then values. PRP-050 written and approved.
- Parser: interface type parameters, bound arguments.
- Checker: `Applied` interfaces in bounds, items at arguments, E0418/E0419 on bound arguments, the `interface_params` pass, inference through bounds, generic defaults (`Checked.default_args`), E0418 for a generic interface as a type (until PRP-051).
- Codegen: default instances of generic interfaces (`lugha_fn_Source<i64>.skip<Count>`).
- `Language v6 Specification.md` begins (in progress: milestone 23); v5 frozen.

### FILES CREATED OR MODIFIED
- New: `src/check/applied.rs`, `src/check/bound_infer.rs`, `src/check/errors/v6.rs`, `tests/programs/m23/` (`bounds`, `modules/`, `e0422`, `e0418`), `docs/specs/Language v6 Specification.md`.
- Modified: `src/ast/header.rs`, `src/parser/{interface.rs,sexp/item.rs}`, `src/check/{interfaces,generics,methods,structs,enums,satisfy,bounds,defaults,instances,infer,iface_values,iface_tables,type_patterns,mod}.rs`, `src/check/{errors,modules}/mod.rs`, `src/codegen/{instances,defaults,lower}.rs`, `src/lib.rs`, `tests/cli.rs`.
- Docs: PRP-050, CLAUDE.md, README, CHANGELOG, TODO, MEMORY 60, CONTEXT.

### TESTS WRITTEN
- Parser: interface type parameters and bound arguments.
- Checker (`applied.rs`): declarations, bounds (E0418, E0305, E0419, E0423), calls through bounds, satisfaction with substituted help, not-yet-a-type; (`bound_infer.rs`): inference from methods, chains, E0412, E0422, a caller's bound; (`defaults.rs`): generic defaults' instances and body typing.
- Codegen (`defaults.rs`): `lugha_fn_Source<i64>.skip<Count>` defined and called.
- `tests/programs/m23/`; `lughac spec` title. Earlier programs' IR compared with `main`: byte-identical. `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 60.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-050 merged into `main`.

---

## SESSION 65 — 2026-10-13 — Generic interface values (milestone 23, part 2) — closed
Branch: prp-051-generic_interface_values (merged into main)

### WHAT WAS DONE
- Discovery: an interface-typed parameter (`Source<T>`) reads `T` off a concrete argument's methods. PRP-051 written and approved.
- `Type::Interface(key, args)`; generic interface values check, compile and run (conversion, calls, copies, type patterns, defaults, `?`).
- `Checked.interfaces` → `InterfaceShape`; tables and copies named with arguments.
- Approved split: `applied.rs` → `bound_args.rs` (verbatim).
- Spec: §4 values, §9 symbols, the §10 generic-interfaces program; milestone 23 done.

### FILES CREATED OR MODIFIED
- New: `src/check/bound_args.rs`, `tests/programs/m23/` (`values`, `e0403`), `e0418` changed.
- Deleted: `src/check/errors/v6.rs`.
- Modified: `src/check/{types,applied,iface_values,iface_calls,iface_tables,method_calls,satisfy,bound_infer,type_patterns,infer,generics,layouts,bounds,defaults,env,literal,mod}.rs`, `src/check/errors/{mod,v5}.rs`, `src/codegen/{interfaces,dispatch,methods,defaults,lower,copy,structs,value}.rs`, `tests/spec.rs`, the v6 spec.
- Docs: PRP-051, CLAUDE.md, README, CHANGELOG, TODO, MEMORY 61, CONTEXT.

### TESTS WRITTEN
- Checker: generic interfaces as value types (E0418, E0419), conversion (E0422 help, E0403), calls (E0403, E0501), bounds and type patterns (E0426); inference into `Source<T>` parameters.
- Codegen: `lugha_vt_Count.Source<i64>` slots, the default's slot, a dynamic call returning `Option<string>`.
- `tests/programs/m23/values.la`, `e0403`, `e0418`; the §10 program from the spec. Earlier programs' IR byte-identical with `main`; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 61.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-051 merged into `main`, tagged `m23`.

---

## SESSION 66 — 2026-10-13 — Iterators (milestone 24) — closed
Branch: prp-052-iterators (merged into main)

### WHAT WAS DONE
- No interview: the v6 draft settles milestone 24. PRP-052 written and approved.
- Prelude `Iter<T>` (at the end of the file), resolved unless a module declares its own `Iter`.
- Checker `check/iterators.rs`: `for … of` over any `next(mut self): Option<T>`, E0411 help, `Checked.iterations`, reachability.
- Codegen `codegen/iterators.rs`: hidden copy, direct or table call of `next`, `Some` payload bound per iteration.
- Spec: §4 iterators, §5 loop and desugaring, §9, E0411 row, the §10 iterators program; milestone 24 done.

### FILES CREATED OR MODIFIED
- New: `src/check/iterators.rs`, `src/codegen/iterators.rs`, `tests/programs/m24/` (`iterators`, `loops`, `own/`, `e0411`).
- Modified: `src/prelude.la`, `src/check/{array,interfaces,iface_values,bound_infer,satisfy,mod}.rs`, `src/check/modules/mod.rs`, `src/check/errors/types.rs`, `src/codegen/{array,dispatch,lower,mod}.rs`, `tests/spec.rs`, the v6 spec.
- Docs: PRP-052, CLAUDE.md, README, CHANGELOG, TODO, MEMORY 62, CONTEXT.

### TESTS WRITTEN
- Checker: what iterates (struct, generic, `Iter<i64>`, own `Source<string>`, `I: Iter<T>`), E0411 with help (four shapes), immutable loop variable and an unguarded iterator, a module's own `Iter`.
- Codegen: direct `next` call on the hidden slot; a table call for an interface value.
- `tests/programs/m24/`; the §10 program from the spec. A default `next` through a bound checked by hand. Earlier programs' IR byte-identical with `main`; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 62.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-052 merged into `main`, tagged `m24`.

---

## SESSION 67 — 2026-10-13 — Self in interfaces (milestone 25) — closed
Branch: prp-053-self_in_interfaces (merged into main)

### WHAT WAS DONE
- No interview: the v6 draft settles milestone 25. PRP-053 written and approved.
- `Self` resolves in interface items and default bodies; `items_for` replaces it in satisfaction, bound calls, interface values and inference.
- E0427 for an interface that uses `Self` written as a type, detected from syntax in pass 1 (a struct field naming it resolves before interface items).
- Spec: §4 `Self`, the default-method sentence, E0427, the §10 `Self` program; milestone 25 done.

### FILES CREATED OR MODIFIED
- New: `src/check/self_type.rs`, `src/check/errors/v6.rs`, `tests/programs/m25/` (`self`, `more`, `e0427`).
- Modified: `src/check/{interfaces,generics,satisfy,bound_infer,iface_values,iface_tables,defaults,mod}.rs`, `src/check/errors/mod.rs`, `tests/spec.rs`, the v6 spec.
- Docs: PRP-053, CLAUDE.md, README, CHANGELOG, TODO, MEMORY 63, CONTEXT.

### TESTS WRITTEN
- Checker (`self_type.rs`): where `Self` resolves (E0305 elsewhere), satisfaction with `Self` replaced (E0422 help), bound calls (`a.max(b): T`, `T.zero(): T`, E0401), E0427 (types, arrays, `Option`, fields, payloads; E0425 first), inference through `Self`. The v5 default test now allows a written `Self`.
- `tests/programs/m25/`; the §10 program from the spec. Earlier programs' IR byte-identical with `main`; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 63.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-053 merged into `main`, tagged `m25`.

---

## SESSION 68 — 2026-10-13 — Converting between interfaces (milestone 26) — closed
Branch: prp-054-interface_conversion (merged into main)

### WHAT WAS DONE
- No interview: the draft settles the behaviour. PRP-054 approved with one amendment to draft §9: lookup functions instead of per-table slots.
- Checker: `J` → `I` when `J` declares every item of `I` (E0422 otherwise); `?` converts interface errors the same way.
- Codegen: table registry, derived tables, `lugha_up_<J>.<I>` built at the end of lowering (fixpoint for chains); `box_as` routes interface sources.
- Spec complete: §4, §7, §9, E0422 row, the §10 program, §11 milestone 26, §12 v6 questions; draft §9 note.

### FILES CREATED OR MODIFIED
- New: `src/check/upcast.rs`, `src/codegen/upcast.rs`, `tests/programs/m26/` (`pets`, `chains`, `e0422`).
- Modified: `src/check/{iface_values,iface_tables,results,mod}.rs`, `src/check/errors/v6.rs`, `src/codegen/{interfaces,defaults,lower,mod}.rs`, `tests/spec.rs`, the v6 spec and draft.
- Docs: PRP-054, CLAUDE.md, README, CHANGELOG, TODO, MEMORY 64, CONTEXT.

### TESTS WRITTEN
- Checker (`upcast.rs`): conversions in every position, E0422 (missing, mismatched, defaulted), E0403 kept (other arguments, arrays), `?` with interface errors (E0424 when not). Two v5 tests updated to the v6 rule.
- Codegen (`upcast.rs`): the up-function and derived table; none without such conversions.
- `tests/programs/m26/`; the §10 program from the spec. Earlier programs' IR byte-identical with `main` (no exceptions); `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 64.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-054 merged into `main`, tagged `m26` and `v6`.

---

## SESSION 69 — 2026-10-13 — v7 direction — closed
Branch: main

### WHAT WAS DONE
- v7 discovery, five answers:
  1. headline: numbers and bits;
  2. wrapping arithmetic as methods (`a.wrapping_add(b)`);
  3. an out-of-range shift panics;
  4. extras: binary literals and bit counting (`count_ones`, `leading_zeros`, `trailing_zeros`);
  5. Rust's precedence, prefix `~`.
- Wrote `docs/specs/Language v7 Draft.md`, approved: milestones 27–29, no new error codes, three §10 programs; six gap choices listed in the draft.

### FILES CREATED OR MODIFIED
- New: `docs/specs/Language v7 Draft.md`.
- Modified: MEMORY (decision 65, state), TODO (v7 milestones), CHANGELOG, CLAUDE.md (tree), CONTEXT.

### TESTS WRITTEN
- None (a design session).

### DECISIONS MADE
- MEMORY 65.

### PENDING DECISIONS OPENED
- None. The draft's §12 lists two questions (`%=`, unsigned indexes).

### STILL OPEN AT CLOSE
- Nothing. Work is on `main`.

---

## SESSION 70 — 2026-10-13 — More numbers (milestone 27) — closed
Branch: prp-055-more_numbers (merged into main)

### WHAT WAS DONE
- No interview: the v7 draft settles milestone 27. PRP-055 written and approved.
- Mid-build, the user chose lexer keywords for `u32`/`u64`/`f32` (like the v0 types) over names recognised only in types; the draft's compatibility sentence was amended (v4 precedent).
- Checker, codegen and runtime support for the three types; `lughac spec` prints the new `Language v7 Specification.md`.
- Approved splits: `check/types.rs` → `check/slots.rs`; `ast/mod.rs` → `ast/types.rs` (verbatim).

### FILES CREATED OR MODIFIED
- New: `src/check/numbers.rs`, `src/check/slots.rs`, `src/ast/types.rs`, `tests/programs/m27/` (`numbers`, `widths`, `overflow`, `e0402`), `docs/specs/Language v7 Specification.md`.
- Modified: `src/lexer/{token,mod}.rs`, `src/parser/{item,describe}.rs`, `src/parser/sexp/item.rs`, `src/ast/mod.rs`, `src/check/{types,env,literal,expr,mod}.rs`, `src/codegen/{value,structs,arith,cast,expr,runtime,input}.rs`, `runtime/print.c`, `src/link.rs`, `src/lib.rs`, `tests/{cli,spec}.rs`, the v7 draft.
- Docs: PRP-055, CLAUDE.md, README, CHANGELOG, TODO, MEMORY 66, CONTEXT.

### TESTS WRITTEN
- Lexer: the new keywords. Checker (`numbers.rs`): literals and ranges (E0402, E0401), unsigned negation (E0404), every cast pair, mixing (E0404), externs. Codegen (`cast.rs`): `uadd` at i64, `udiv i32`, `icmp ult`, `fadd float`, `fpext`, `fptrunc`.
- `tests/programs/m27/`; the §10 program from the spec. Earlier programs' IR byte-identical with `main`; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 66.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-055 merged into `main`, tagged `m27`.

---

## SESSION 71 — 2026-10-13 — Bits (milestone 28) — closed
Branch: prp-056-bits (merged into main)

### WHAT WAS DONE
- The user asked for milestones 28 and 29 "in one go": PRP-056 written as approved within the v7 draft, built, merged on green CI.
- Lexer: bit-operator tokens, `0b` literals; `>>`/`>>=` read by the parser from touching `>` tokens (`parser/operators.rs`).
- AST levels per the draft (Rust order), `~`, bit compound forms; checker rules; codegen `codegen/bits.rs` with the shift range panic.
- Spec: §2 tokens and literals, §3 precedence, §4 operator table, §5 shifts and compound forms, panics list, the §10 bits program; milestone 28 done.

### FILES CREATED OR MODIFIED
- New: `src/parser/operators.rs`, `src/codegen/bits.rs`, `tests/programs/m28/` (`bits`, `more`, `shift_panic`, `e0404`).
- Modified: `src/lexer/{punct,number,token,mod}.rs`, `src/parser/{expr,stmt,describe,mod}.rs`, `src/parser/sexp/mod.rs`, `src/ast/{expr,mod}.rs`, `src/check/{ops,expr,assign,numbers}.rs`, `src/codegen/{arith,expr,stmt,mod}.rs`, `tests/spec.rs`, the v7 spec.
- Docs: PRP-056, CLAUDE.md, README, CHANGELOG, TODO, MEMORY 67, CONTEXT.

### TESTS WRITTEN
- Lexer: operators by longest match, `>>` as two tokens, binary literals and their errors (E0105/E0106/E0108); the E0101 test no longer uses `&`/`|`.
- Parser (`operators.rs`): precedence, `>>` pairing (and `a > > b` an error, `Option<Option<i64>>` fine), compound forms.
- Checker (`numbers.rs`): bit operators' types and E0404 cases. Codegen (`bits.rs`): each instruction and `icmp uge`.
- `tests/programs/m28/`; the §10 program from the spec. Earlier programs' IR byte-identical with `main`; `-O2` checked by hand (including the panic).

### DECISIONS MADE
- MEMORY 67.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-056 merged into `main`, tagged `m28`.

---

## SESSION 72 — 2026-10-13 — Integer methods (milestone 29) — closed
Branch: prp-057-integer_methods (merged into main)

### WHAT WAS DONE
- Second half of the user's "in one go": PRP-057 written as approved within the v7 draft, built, merged on green CI.
- Checker: six built-in integer methods (`builtins.rs`); codegen `codegen/int_methods.rs`.
- Spec complete: §4 methods on integers, §9 lowering of the v7 operators and methods, the §10 hashing program, §11 milestone 29, §12 v7 questions; title and status.

### FILES CREATED OR MODIFIED
- New: `src/codegen/int_methods.rs`, `tests/programs/m29/` (`hashing`, `methods`, `e0415`).
- Modified: `src/check/builtins.rs`, `src/codegen/{methods,mod}.rs`, `tests/spec.rs`, the v7 spec.
- Docs: PRP-057, CLAUDE.md, README, CHANGELOG, TODO, MEMORY 68, CONTEXT.

### TESTS WRITTEN
- Checker (`builtins.rs`): types at several widths, `i64` counts, literal receiver; E0415 (unknown name, float, `bool`), E0405, E0403.
- Codegen (`int_methods.rs`): plain `mul`, the three intrinsics and the widening.
- `tests/programs/m29/`; the §10 program from the spec. Earlier programs' IR byte-identical with `main`; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 68.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-057 merged into `main`, tagged `m29` and `v7`.

---

## SESSION 73 — 2026-10-13 — v8 direction — closed
Branch: main

### WHAT WAS DONE
- v8 discovery, five answers:
  1. headline: packages (over the recommended `match` conveniences);
  2. a manifest (`lugha.toml`) rather than a `packages/` folder + flag — after two rounds of explaining the difference;
  3. format: a TOML subset read by lughac itself;
  4. visibility: no keyword = package, `public` = other packages, `private` = module;
  5. imports keep their syntax, a package/local-folder clash is an error; packages may have their own manifests, not transitive, one name = one folder.
- Wrote `docs/specs/Language v8 Draft.md`, approved: milestone 30, codes E0320–E0323, E0317 retired, a §10 packages program and a rejected one.

### FILES CREATED OR MODIFIED
- New: `docs/specs/Language v8 Draft.md`.
- Modified: MEMORY (decision 69, state), TODO (v8 milestone), CHANGELOG, CLAUDE.md (tree), CONTEXT.

### TESTS WRITTEN
- None (a design session).

### DECISIONS MADE
- MEMORY 69.

### PENDING DECISIONS OPENED
- None. The draft's §12 lists re-exports and a `--package` override.

### STILL OPEN AT CLOSE
- Nothing. Work is on `main`.

---

## SESSION 74 — 2026-10-14 — Loading packages (milestone 30, part 1) — closed
Branch: prp-058-package_loading (merged into main)

### WHAT WAS DONE
- The user asked for milestone 30 "in one go": PRP-058 written as approved within the v8 draft, built, merged on green CI (no tag until PRP-059).
- `modules/manifest.rs` (TOML subset, E0321) and `modules/packages.rs` (manifests of a build, package imports, E0320/E0322/E0323, `import geo;` E0312).
- `Module.package`/`imports`, `Loaded.manifests`, `Unit.imports`; manifests in the source map.
- `Language v8 Specification.md` begins (packages section, symbols, E0320–E0323).
- A first commit went out unformatted with half-updated docs (a docs script stopped on a stale `CLAUDE.md` line while the commit ran anyway); CI failed on `cargo fmt --check`, and a follow-up commit fixed both.

### FILES CREATED OR MODIFIED
- New: `src/modules/manifest.rs`, `src/modules/packages.rs`, `tests/programs/m30/` (`packages/`, `e0320/`, `e0321/`), `docs/specs/Language v8 Specification.md`.
- Modified: `src/modules/{mod,resolve,tests}.rs`, `src/check/modules/mod.rs`, `src/check/{mod,test_util}.rs`, `src/codegen/mod.rs`, `src/driver/{pipeline,source}.rs`, `src/lib.rs`, `tests/{modules,cli}.rs`.
- Docs: PRP-058, CLAUDE.md, README, CHANGELOG, TODO, MEMORY 70, CONTEXT.

### TESTS WRITTEN
- `manifest.rs`: entries with comments and spans; E0321 cases; messages.
- `modules/tests.rs`: package imports and paths, a package's own modules, non-transitive manifests, E0320/E0322/E0323/E0312/E0321, packages importing each other.
- `tests/modules.rs`: a package builds and runs with `lugha_fn_geo/shapes/…`; E0321 at `lugha.toml:2:1` in human and JSON output.
- `tests/programs/m30/`. Earlier programs' IR byte-identical with `main`.

### DECISIONS MADE
- MEMORY 70.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-058 merged into `main` (no tag until PRP-059).

---

## SESSION 75 — 2026-10-14 — Package visibility (milestone 30, part 2) — closed
Branch: prp-059-package_visibility (merged into main)

### WHAT WAS DONE
- Second half of the user's "in one go" for milestone 30. The code was written before the PRP file and branch (on `main`, uncommitted); caught before any commit, moved to `prp-059-package_visibility`, and PRP-059 written to match.
- `check/levels.rs` (levels, owners, `denied`, `hidden_inside`), `visibility.rs` reworked, `errors/v8.rs`; `Unit.package`; E0317 retired.
- Spec extractor handles manifests and subfolders; the §10 packages program runs from the spec. Spec complete.

### FILES CREATED OR MODIFIED
- New: `src/check/levels.rs`, `src/check/errors/v8.rs`, `tests/programs/m30/` (`public`, `e0313/`).
- Deleted: `tests/programs/m10/e0317.{la,stderr}`.
- Modified: `src/check/{visibility,interfaces,mod,test_util}.rs`, `src/check/errors/{mod,names}.rs`, `src/check/modules/mod.rs`, `src/codegen/mod.rs`, `src/driver/pipeline.rs`, `tests/{modules,spec}.rs`, `tests/common/spec_programs.rs`, `m30/packages` sources (`public`), the v8 spec.
- Docs: PRP-059, CLAUDE.md, README, CHANGELOG, TODO, MEMORY 71, CONTEXT.

### TESTS WRITTEN
- `check/levels.rs`: public crosses packages (incl. a bound); E0313 for function, method, field, literal, hidden struct (once); hidden method fails a bound (E0422); E0318 for public return, type argument, bound; v2 tests unchanged; `public` allowed.
- `tests/programs/m30/` `public` and `e0313/`; the §10 packages program from the spec. 215 earlier programs' IR byte-identical with `main`; `-O2` checked by hand for `packages`.

### DECISIONS MADE
- MEMORY 71.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-059 merged into `main`, tagged `m30` and `v8`.

---

## SESSION 76 — 2026-10-14 — v9 direction — closed
Branch: main

### WHAT WAS DONE
- v9 discovery, five answers:
  1. headline: `match` and everyday conveniences;
  2. or-patterns bind names when every alternative binds the same names and types;
  3. `mut` bindings are the arm's own copy;
  4. `unwrap()`/`expect(msg)` panic with fixed messages;
  5. extras: `%=`, `if let`, `while let`, `let … else` (all four).
- Wrote `docs/specs/Language v9 Draft.md`, approved: milestones 31–33, codes E0428 and E0429, three §10 programs.

### FILES CREATED OR MODIFIED
- New: `docs/specs/Language v9 Draft.md`.
- Modified: MEMORY (decision 72, state), TODO (v9 milestones), CHANGELOG, CLAUDE.md (tree), CONTEXT.

### TESTS WRITTEN
- None (a design session).

### DECISIONS MADE
- MEMORY 72.

### PENDING DECISIONS OPENED
- None. The draft's §12 lists printing errors in `unwrap` and irrefutable `if let`.

### STILL OPEN AT CLOSE
- Nothing. Work is on `main`.

---

## SESSION 77 — 2026-10-14 — Better patterns (milestone 31) — closed
Branch: prp-060-better_patterns (merged into main)

### WHAT WAS DONE
- The user asked for milestones 31–33 "in one go": PRP-060 written as approved within the v9 draft, built, merged on green CI.
- Parser: `|` alternatives, `mut` bindings, parenthesised patterns. Checker: expanded alternatives, E0428, per-alternative W0102, mutable bindings. Codegen: alternatives in the arm chain sharing one slot per name, owned `mut` copies.
- `Language v9 Specification.md` begins (§3 grammar, §4 or-patterns and `mut`, E0428, §9, the §10 program).

### FILES CREATED OR MODIFIED
- New: `src/check/or_patterns.rs`, `src/check/errors/v9.rs`, `src/codegen/or_patterns.rs`, `tests/programs/m31/` (`or`, `more`, `e0428`), `docs/specs/Language v9 Specification.md`.
- Modified: `src/ast/pattern.rs`, `src/parser/pattern.rs`, `src/parser/sexp/mod.rs`, `src/check/{patterns,matching,type_patterns,mod}.rs`, `src/check/errors/mod.rs`, `src/codegen/{patterns,matching,scope,mod}.rs`, `src/lib.rs`, `tests/{cli,spec}.rs`.
- Docs: PRP-060, CLAUDE.md, README, CHANGELOG, TODO, MEMORY 73, CONTEXT.

### TESTS WRITTEN
- Parser: or-patterns, nesting, parentheses, `mut`, E0201 cases. Checker (`or_patterns.rs`): shared bindings, E0428 (missing, extra, type, `mut None`), coverage (E0417, W0102 per alternative), `mut` vs E0501, top-level `mut x`. Codegen: arm chain with one slot per name.
- `tests/programs/m31/`; the §10 program from the spec. 220 earlier programs' IR byte-identical with `main`; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 73.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-060 merged into `main`, tagged `m31`.

---

## SESSION 78 — 2026-10-14 — Unwrapping (milestone 32) — closed
Branch: prp-061-unwrapping (merged into main)

### WHAT WAS DONE
- Part of the user's "in one go": PRP-061 written as approved within the v9 draft, built, merged on green CI.
- `unwrap`/`expect` built in (`check/builtins.rs`, `codegen/unwrap.rs`); `%=` through lexer, AST, parser, checker, codegen.
- Spec: §2/§3 `%=`, helper table rows, panics list, §9, the §10 program; milestone 32 done.

### FILES CREATED OR MODIFIED
- New: `src/codegen/unwrap.rs`, `tests/programs/m32/` (`unwrap`, `more`, `err`).
- Modified: `src/lexer/{token,punct}.rs`, `src/parser/{describe,operators}.rs`, `src/ast/mod.rs`, `src/check/{builtins,assign}.rs`, `src/codegen/{methods,runtime,stmt,mod}.rs`, `tests/spec.rs`, the v9 spec.
- Docs: PRP-061, CLAUDE.md, README, CHANGELOG, TODO, MEMORY 74, CONTEXT.

### TESTS WRITTEN
- Checker (`builtins.rs`): types on `Option`/`Result`, E0415/E0401/E0405, `%=` and E0404. Codegen (`unwrap.rs`): tag test and panic. Lexer: `%=`.
- `tests/programs/m32/` (two panicking goldens, exit 101); the §10 program from the spec. 223 earlier programs' IR byte-identical; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 74.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-061 merged into `main`, tagged `m32`.

---

## SESSION 79 — 2026-10-14 — Conditional patterns (milestone 33) — closed
Branch: prp-062-conditional_patterns (merged into main)

### WHAT WAS DONE
- Last part of the user's "in one go" for v9: PRP-062 written as approved within the v9 draft, built, merged on green CI.
- The first CI run overflowed the 2 MB test stack in the parser's deep-nesting test: inline `IfLet` fields grew `Expr` from 88 to 96 bytes. Fixed by `ExprKind::IfLet(Box<IfLet>)`; CI green.
- Native `IfLet`/`WhileLet`/`LetElse` (boxed), parsed in `parser/conditional.rs`, checked in `check/conditional.rs` (E0429), lowered in `codegen/conditional.rs`; liveness and sexp updated.
- Spec complete: §3 grammar, §5 conditional patterns, §9, E0429, the §10 program, §11, §12.

### FILES CREATED OR MODIFIED
- New: `src/parser/conditional.rs`, `src/check/conditional.rs`, `src/codegen/conditional.rs`, `tests/programs/m33/` (`conditional`, `more`, `e0429`).
- Modified: `src/ast/{expr,mod}.rs`, `src/parser/{primary,stmt,mod}.rs`, `src/parser/sexp/mod.rs`, `src/check/{expr,stmt,matching,mod}.rs`, `src/check/errors/v9.rs`, `src/check/liveness/walk.rs`, `src/codegen/{expr,stmt,mod}.rs`, `tests/spec.rs`, the v9 spec.
- Docs: PRP-062, CLAUDE.md, README, CHANGELOG, TODO, MEMORY 75, CONTEXT.

### TESTS WRITTEN
- Parser: the three forms, `else if let`, or-pattern with `mut`, a missing `;`. Checker: binding scopes (E0301 in `else`), `if let` as a value, or-pattern + `mut`, E0403 pattern, E0429 vs `panic`/`return`/`continue`. Codegen: tag test and else branch.
- `tests/programs/m33/`; the §10 program from the spec. 226 earlier programs' IR byte-identical; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 75.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-062 merged into `main`, tagged `m33` and `v9`.

---

## SESSION 80 — 2026-10-14 — v10 direction — closed
Branch: main

### WHAT WAS DONE
- v10 discovery, five questions, each answered with the recommended option:
  - lazy iterator helpers;
  - the focused helper set;
  - the sources `iter`, `bytes` and `range`;
  - full `Self` in `impl` blocks, plus default methods on concrete types;
  - integer types with methods and `MIN`/`MAX`.
- Wrote `docs/specs/Language v10 Draft.md` (proposed): milestones 34–36, E0430, three §10 programs.

### FILES CREATED OR MODIFIED
- New: `docs/specs/Language v10 Draft.md`.
- CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (design only).

### DECISIONS MADE
- None recorded yet; the v10 MEMORY decision follows approval of the draft.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- The v10 draft awaits the user's approval (approved at the start of session 81).

---

## SESSION 81 — 2026-10-14 — `Self` everywhere (milestone 34) — closed
Branch: prp-063-self_everywhere (merged into main)

### WHAT WAS DONE
- The user approved the v10 draft and asked for milestones 34–36 "in one go". The draft is marked approved; MEMORY 76.
- PRP-063 written as approved under that instruction, built and merged on green CI:
  - `Self` in `impl` blocks, as parser sugar (`parser/self_type.rs`);
  - default methods on concrete types (`check/concrete_defaults.rs`, E0430 in `errors/v10.rs`).
- `Language v10 Specification.md` begins: the changes summary, §4 (`Self` in `impl` blocks, defaults on concrete types), §9, E0430, the §10 program, §11 row 34, the v10 out-of-scope list, and the §12 v5 question decided.

### FILES CREATED OR MODIFIED
- New: `src/parser/self_type.rs`, `src/check/concrete_defaults.rs`, `src/check/errors/v10.rs`, `tests/programs/m34/` (`self`, `more`, `e0430`), `docs/specs/Language v10 Specification.md`, PRP-063.
- Modified:
  - `src/parser/{mod,decl,primary,generic}.rs`;
  - `src/check/{mod,method_calls,bound_infer,defaults,self_type}.rs`, `src/check/errors/mod.rs`;
  - `src/lib.rs`;
  - `tests/{cli,spec}.rs`, `tests/programs/m22/e0415.la`;
  - the v10 draft (status).
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 76–77, CONTEXT.

### TESTS WRITTEN
- **Parser:**
  - `Self` in types, literals, calls and variants;
  - a generic `impl`'s `Self<T>`;
  - `Self` untouched outside an `impl`.
- **Checker:**
  - defaults on structs and enums;
  - the type's own method wins;
  - a generic interface's default;
  - E0430;
  - E0415 when the type doesn't satisfy the interface or isn't a struct or enum;
  - `Self` checks in an `impl`, and is E0305 in a struct declaration.
- **Programs:** `tests/programs/m34/`, and the §10 program from the spec.
- **Regression:** 133 earlier programs give byte-identical IR; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 76 (v10 direction), 77 (milestone 34).

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-063 merged into `main`, tagged `m34`.

---

## SESSION 82 — 2026-10-14 — More integers (milestone 35) — closed
Branch: prp-064-more_integers (merged into main)

### WHAT WAS DONE
- PRP-064 written as approved under the "in one go" instruction, built and merged on green CI:
  - `i8`, `i16` and `u16`;
  - `checked_*`, `saturating_*` and rotations on every integer type;
  - `I.MIN`/`I.MAX`;
  - `parse_u64`.
- v10 spec at milestone 35:
  - the changes summary, including the compatibility note on the new keywords and `parse_u64`;
  - §2 keywords, §3 grammar;
  - §4 types table, methods table and integer limits;
  - `parse_u64` in the intrinsics table;
  - §8 externs;
  - §9 runtime table and lowering;
  - the E0404 row;
  - the §10 program, §11 row 35.

### FILES CREATED OR MODIFIED
- New: `src/codegen/int_checked.rs`, `tests/programs/m35/` (`integers`, `more`, `overflow`, `e0402`), PRP-064.
- Modified:
  - `src/lexer/{token,mod}.rs`, `src/ast/types.rs`;
  - `src/parser/{item,describe,primary}.rs`, `src/parser/sexp/item.rs`;
  - `src/check/{types,numbers,access,builtins,input}.rs`;
  - `src/codegen/{mod,value,structs,runtime,input,declare,int_methods,expr,function}.rs`;
  - `runtime/input.c`, `src/link.rs`;
  - `tests/spec.rs`, `tests/programs/m29/e0415.{la,stderr}`;
  - the v10 spec.
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 78, CONTEXT.

### TESTS WRITTEN
- **Lexer:** the new keywords.
- **Parser:** the types, casts and `i16.MAX`.
- **Checker:**
  - literal ranges (E0402);
  - limits (E0415 for `i64.TOP`);
  - `parse_u64`, and a user `parse_u64` (E0302);
  - method types, E0403 and E0405.
- **Codegen:** the intrinsics and constants; `checked_div` never divides by zero.
- **Programs:** `tests/programs/m35/`, and the §10 program from the spec.
- **Regression:** 135 earlier programs give byte-identical IR; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 78.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-064 merged into `main`, tagged `m35`.

---

## SESSION 83 — 2026-10-14 — Iterator helpers (milestone 36) — closed
Branch: prp-065-iterator_helpers (merged into main)

### WHAT WAS DONE
- PRP-065 written as approved under the "in one go" instruction, built and merged on green CI:
  - iterator sources `xs.iter()`, `s.bytes()` and `range(a, b)`;
  - lazy adapters `map`, `filter`, `take`, `skip` and `enumerate`;
  - consumers `collect`, `count`, `fold`, `any`, `all`, `find` and `sum`;
  - `for b of s`.
- **v10 spec complete:**
  - status and changes summary;
  - §4 sources and helpers;
  - §5 for over a string;
  - the E0411 example;
  - §9;
  - the §10 program, §11 row 36;
  - §12 gains two questions.
- **Prelude types and methods compile only when reached** (amendment).

### FILES CREATED OR MODIFIED
- New: `src/prelude_iter.la`, `src/check/iter_helpers.rs`, `tests/programs/m36/` (`helpers`, `more`, `sources`, `overflow`, `e0415`), PRP-065.
- Modified:
  - `src/lib.rs`;
  - `src/check/{mod,helpers,method_calls,iterators,instances,call,array,results,types}.rs`;
  - `src/codegen/{array,heap,methods,iterators,interfaces}.rs`;
  - `tests/spec.rs`;
  - the v10 spec.
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 79, CONTEXT.

### TESTS WRITTEN
- **Checker:**
  - helper and source types;
  - user iterators, `Iter<i64>` values, bound parameters;
  - the type's own method wins;
  - a user `range` wins;
  - strings loop over `u8`;
  - E0415, E0405, E0403.
- **Codegen:** a string loop reads 8 bytes past the pointer.
- **Programs:** `tests/programs/m36/`, and the §10 program from the spec.
- **Regression:** 138 earlier programs give byte-identical IR; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 79.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-065 merged into `main`, tagged `m36` and `v10`.

---

## SESSION 84 — 2026-10-14 — v11 direction — closed
Branch: main

### WHAT WAS DONE
- The user chose v11 = package versions and re-exports.
- Discovery, three questions, each answered with the recommended option:
  - checked local versions;
  - Cargo-compatible requirements only;
  - named re-exports (`public import m { X }`, braces also without `public`).
- Wrote `docs/specs/Language v11 Draft.md` (proposed): milestones 37 (versions, E0324) and 38 (named imports and re-exports).

### FILES CREATED OR MODIFIED
- New: `docs/specs/Language v11 Draft.md`.
- CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (design only).

### DECISIONS MADE
- None recorded yet; the v11 MEMORY decision follows approval of the draft.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- The v11 draft awaits the user's approval (approved at the start of session 85).

---

## SESSION 85 — 2026-10-14 — Package versions (milestone 37) — closed
Branch: prp-066-package_versions (merged into main)

### WHAT WAS DONE
- The user approved the v11 draft and asked for it "in one go". The draft is marked approved; MEMORY 80.
- PRP-066 written as approved under that instruction, built and merged on green CI:
  - `[package]` versions;
  - inline entries with compatible requirements;
  - E0324, checked after loading.
- `Language v11 Specification.md` begins: the changes summary, §6 manifest and versions, the E0321 and E0324 rows, the §10 program (multi-file, with a nested `lugha.toml`), §11 row 37, the v11 out-of-scope list, and a §12 question.

### FILES CREATED OR MODIFIED
- New: `src/modules/{version,inline,version_tests}.rs`, `tests/programs/m37/` (`versions/`, `e0324/`), `docs/specs/Language v11 Specification.md`, PRP-066.
- Modified:
  - `src/modules/{mod,manifest,packages,tests}.rs`;
  - `src/lib.rs`;
  - `tests/{cli,spec}.rs`, `tests/programs/m30/e0321.stderr`;
  - the v11 draft (status).
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 80–81, CONTEXT.

### TESTS WRITTEN
- **Versions and requirements:** parsing, and the compatible rule at every edge.
- **Inline entries:** both key orders, and every E0321 shape.
- **Manifests:** `[package]`, a second table, a bad version.
- **Loading:**
  - a compatible version loads;
  - a mismatch, a missing version, and an unimported required package are E0324;
  - a missing folder is only E0322.
- **Programs:** `tests/programs/m37/`, and the §10 program from the spec.
- **Regression:** 142 earlier programs give byte-identical IR.

### DECISIONS MADE
- MEMORY 80 (v11 direction), 81 (milestone 37).

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-066 merged into `main`, tagged `m37`.

---

## SESSION 86 — 2026-10-14 — Re-exports (milestone 38) — closed
Branch: prp-067-reexports (merged into main)

### WHAT WAS DONE
- PRP-067 written as approved under the "in one go" instruction, built and merged on green CI:
  - `import a.b { X, y };`;
  - `public import a.b { X, y };` re-exports (aliases in the checker; codegen unchanged);
  - E0318 for re-exporting a non-`public` item.
- **v11 spec complete:**
  - status and changes summary;
  - §3 grammar;
  - §6 named imports and re-exports, and name resolution order;
  - the E0201, E0302, E0313, E0314 and E0318 rows;
  - §9;
  - the §10 multi-file program, §11 row 38;
  - §12.

### FILES CREATED OR MODIFIED
- New: `src/check/named_imports.rs`, `tests/programs/m38/` (`reexports/`, `named/`, `e0318/`), PRP-067.
- Modified:
  - `src/ast/header.rs`;
  - `src/parser/{import,item}.rs`, `src/parser/sexp/item.rs`;
  - `src/check/{mod,construct,fun_values,iface_values,interfaces}.rs`, `src/check/modules/{mod,lookup}.rs`;
  - `src/lib.rs`, `tests/spec.rs`;
  - the v11 spec.
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 82, CONTEXT.

### TESTS WRITTEN
- **Parser:** named and public imports; E0201 and E0202.
- **Checker:**
  - unqualified struct, function, enum and interface;
  - a re-export chain and the same type;
  - E0301, E0302, E0314, E0313 (private item; a file-only name from elsewhere), E0318.
- **Programs:** `tests/programs/m38/`, and the §10 program from the spec.
- **Regression:** 143 earlier programs give byte-identical IR.

### DECISIONS MADE
- MEMORY 82.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-067 merged into `main`, tagged `m38` and `v11`.

---

## SESSION 87 — 2026-10-14 — v12 direction — closed
Branch: main

### WHAT WAS DONE
- The user chose v12 = a `Display`-style interface.
- Discovery, three questions, each answered with the recommended option:
  - `show` plus a built-in form for everything else;
  - strings quoted only inside built-in forms;
  - everything prints (generic `T`, interface values, `<fun>`, `unwrap on Err: <error>`).
- Wrote `docs/specs/Language v12 Draft.md` (proposed): milestones 39 (printing values) and 40 (printing anything). No new error codes.

### FILES CREATED OR MODIFIED
- New: `docs/specs/Language v12 Draft.md`.
- CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (design only).

### DECISIONS MADE
- None recorded yet; the v12 MEMORY decision follows approval of the draft.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- The v12 draft awaits the user's approval (approved at the start of session 88).

---

## SESSION 88 — 2026-10-14 — Printing values (milestone 39) — closed
Branch: prp-068-printing_values (merged into main)

### WHAT WAS DONE
- The user approved the v12 draft and asked for it "in one go". The draft is marked approved; MEMORY 83.
- PRP-068 written as approved under that instruction, built and merged on green CI:
  - every struct, enum, array, `Option`, `Result` and `T` prints, through its own `show` or a built-in form, with strings quoted inside it;
  - codegen also prints interface values (for a `T` that is one), by identity.
- `Language v12 Specification.md` begins:
  - the changes summary;
  - §4 on `T`;
  - §5 "How values print", and the intrinsics and interpolation rules;
  - §9 helpers and the runtime table;
  - the E0403 row;
  - the §10 program, §11 row 39;
  - the v12 out-of-scope list and §12.

### FILES CREATED OR MODIFIED
- New: `src/check/printing.rs`, `src/codegen/{show,show_iface}.rs`, `tests/programs/m39/` (`values`, `more_mod/`), `docs/specs/Language v12 Specification.md`, PRP-068.
- Modified:
  - `src/check/{mod,call,input,iterators,instances,bounds}.rs`;
  - `src/codegen/{mod,lower,runtime,io,input,upcast}.rs`;
  - `src/prelude_iter.la`, `runtime/print.c`, `src/link.rs`, `src/lib.rs`;
  - `tests/{cli,spec}.rs`;
  - the v12 draft (status).
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 83–84, CONTEXT.

### TESTS WRITTEN
- **Checker:**
  - everything but `void` and interface or function values prints;
  - a generic `show` instance is listed directly, inside an array, and through `T`.
- **Codegen:** helpers and quoting; a type's own `show`; interface identity; plain values unchanged.
- **Programs:** `tests/programs/m39/`, and the §10 program from the spec.
- **Regression:** 145 earlier programs give byte-identical IR; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 83 (v12 direction), 84 (milestone 39).

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-068 merged into `main`, tagged `m39`.

---

## SESSION 89 — 2026-10-14 — Printing anything (milestone 40) — closed
Branch: prp-069-printing_anything (merged into main)

### WHAT WAS DONE
- PRP-069 written as approved under the "in one go" instruction, built and merged on green CI:
  - interface and function values print directly;
  - `unwrap()` on `Err` panics with `unwrap on Err: <error>`.
- **v12 spec complete:**
  - status and changes summary;
  - the print table rows;
  - the intrinsics, interpolation, interface and function value rules;
  - the `unwrap` row and the panics list;
  - E0403;
  - §9;
  - the §10 program, §11 row 40;
  - §12 printing errors decided.

### FILES CREATED OR MODIFIED
- New: `tests/programs/m40/` (`anything`, `more`), PRP-069.
- Modified:
  - `src/check/{printing,method_calls,call,input,fun_values,iface_calls}.rs`, `src/codegen/unwrap.rs`;
  - `src/lib.rs`, `tests/spec.rs`, `tests/programs/m32/err.stderr`;
  - the v12 spec.
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 85, CONTEXT.

### TESTS WRITTEN
- **Checker:**
  - interface and function values print and interpolate;
  - `unwrap` on a `Result` lists the error's `show` instance;
  - `void` is E0407.
- **Programs:** `tests/programs/m40/`, and the §10 program from the spec.
- **Regression:** 144 earlier programs give byte-identical IR (the three `Result.unwrap` programs differ by design).

### DECISIONS MADE
- MEMORY 85.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-069 merged into `main`, tagged `m40` and `v12`.

---

## SESSION 90 — 2026-10-14 — v13 direction — closed
Branch: main

### WHAT WAS DONE
- The user chose v13 = format specifiers, `zip`/`chain` iterator helpers, `for n of mut c`, and `Self` in declarations.
- Discovery, three questions, each answered with the recommended option:
  - Rust-style core format specifiers;
  - the full helper set with `Pair`;
  - `for … of mut` on iterators and arrays.

  `Self` in declarations needed no question.
- Wrote `docs/specs/Language v13 Draft.md` (proposed): milestones 41–44, E0431.

### FILES CREATED OR MODIFIED
- New: `docs/specs/Language v13 Draft.md`.
- CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (design only).

### DECISIONS MADE
- None recorded yet; the v13 MEMORY decision follows approval of the draft.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- The v13 draft awaits the user's approval (approved at the start of session 91).

---

## SESSION 91 — 2026-10-14 — `Self` in declarations (milestone 41) — closed
Branch: prp-070-self_in_declarations (merged into main)

### WHAT WAS DONE
- The user approved the v13 draft and asked for it "in one go". The draft is marked approved; MEMORY 86.
- PRP-070 written as approved under that instruction, built and merged on green CI: `Self` in struct fields and enum payloads, as parser sugar.
- `Language v13 Specification.md` begins: the changes summary, §4 `Self`, §9, the §10 program, §11 row 41, and the v13 out-of-scope list.

### FILES CREATED OR MODIFIED
- New: `tests/programs/m41/` (`self`, `more/`), `docs/specs/Language v13 Specification.md`, PRP-070.
- Modified:
  - `src/parser/{item,decl,self_type}.rs`, `src/check/{self_type,concrete_defaults}.rs`;
  - `src/lib.rs`, `tests/{cli,spec}.rs`;
  - the v13 draft.
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 86–87, CONTEXT.

### TESTS WRITTEN
- **Parser:** `Self` in structs, enums and generic structs.
- **Checker:** E0307 for a struct holding `Self`; E0305 outside declarations.
- **Programs:** `tests/programs/m41/`, and the §10 program from the spec.
- **Regression:** 149 earlier programs give byte-identical IR.

### DECISIONS MADE
- MEMORY 86 (v13 direction), 87 (milestone 41).

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-070 merged into `main`, tagged `m41`.

---

## SESSION 92 — 2026-10-14 — `for … of mut` (milestone 42) — closed
Branch: prp-071-for_of_mut (merged into main)

### WHAT WAS DONE
- PRP-071 written as approved under the "in one go" instruction, built and merged on green CI: `for x of mut e` over arrays (elements aliased) and iterators (stepped in place).
- **Spec:**
  - §3 grammar, §5 "for over `mut`";
  - the E0501, E0502, E0506 and E0507 rows;
  - §9;
  - the §10 program, §11 row 42.

### FILES CREATED OR MODIFIED
- New: `src/check/for_mut.rs`, `tests/programs/m42/` (`mut`, `more`, `e0501`), PRP-071.
- Modified:
  - `src/ast/mod.rs`;
  - `src/parser/{stmt,conditional}.rs`, `src/parser/sexp/mod.rs`;
  - `src/check/{mod,stmt}.rs`, `src/check/liveness/walk.rs`;
  - `src/codegen/{stmt,array,iterators}.rs`;
  - `tests/spec.rs`;
  - the v13 spec.
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 88, CONTEXT.

### TESTS WRITTEN
- **Parser:** `(of-mut xs)`.
- **Checker:** elements, fields, `mut self`, iterators; E0501, E0502, E0506, E0507 ×2.
- **Codegen:** no `alloca` for an aliased element.
- **Programs:** `tests/programs/m42/`, and the §10 program from the spec.
- **Regression:** 151 earlier programs give byte-identical IR; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 88.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-071 merged into `main`, tagged `m42`.

---

## SESSION 93 — 2026-10-14 — More iterator helpers (milestone 43) — closed
Branch: prp-072-more_iterator_helpers (merged into main)

### WHAT WAS DONE
- PRP-072 written as approved under the "in one go" instruction, built and merged on green CI: `zip` (`Pair`), `chain`, `take_while`, `skip_while`, `flat_map`, `min`, `max`, `position`, in `src/prelude_more.la`.
- **Spec:** `Pair`, the helper table rows, the names list, §9, the §10 program, §11 row 43.

### FILES CREATED OR MODIFIED
- New: `src/prelude_more.la`, `tests/programs/m43/` (`helpers`, `more`), PRP-072.
- Modified: `src/lib.rs`, `src/check/{iter_helpers,struct_literal}.rs`, `tests/spec.rs`, the v13 spec.
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 89, CONTEXT.

### TESTS WRITTEN
- **Checker:** the v13 helper types; E0419 for `min` on a struct; E0422 for `chain` of different types.
- **Programs:** `tests/programs/m43/`, and the §10 program from the spec.
- **Regression:** 153 earlier programs give byte-identical IR; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 89.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-072 merged into `main`, tagged `m43`.

---

## SESSION 94 — 2026-10-14 — Format specifiers (milestone 44) — closed
Branch: prp-073-format_specifiers (merged into main)

### WHAT WAS DONE
- PRP-073 written as approved under the "in one go" instruction, built and merged on green CI: format specifiers in interpolations, E0431, and the runtime fragment `format.c`.
- **v13 spec complete:**
  - status and changes summary;
  - §3 grammar;
  - §5 "Format specifiers";
  - the E0431 row and the runtime table;
  - §9;
  - the §10 program, §11 row 44;
  - §12.

### FILES CREATED OR MODIFIED
- New: `src/format.rs`, `src/lexer/format_spec.rs`, `src/codegen/format.rs`, `src/check/errors/v13.rs`, `runtime/format.c`, `tests/programs/m44/` (`format`, `more`, `e0431`), PRP-073.
- Modified:
  - `src/lib.rs`, `src/link.rs`;
  - `src/lexer/{mod,token,interp}.rs`, `src/ast/{expr,mod}.rs`;
  - `src/parser/{primary,describe,conditional}.rs`, `src/parser/sexp/mod.rs`;
  - `src/check/{input}.rs`, `src/check/errors/mod.rs`;
  - `src/codegen/{mod,input}.rs`;
  - `tests/spec.rs`;
  - the v13 spec.
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 90, CONTEXT.

### TESTS WRITTEN
- **`format.rs`:** parsing, errors, and fitting.
- **Lexer:** the `FmtSpec` token; colons inside parentheses.
- **Parser:** `(fmt x ">5")`.
- **Checker:** E0431 ×6, and valid specs on a struct, `u8` and `f64`.
- **Programs:** `tests/programs/m44/`, and the §10 program from the spec.
- **Regression:** 155 earlier programs give byte-identical IR; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 90.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-073 merged into `main`, tagged `m44` and `v13`.

---

## SESSION 95 — 2026-10-14 — v14 direction — closed
Branch: main

### WHAT WAS DONE
- The user chose v14 = `format()`, tuples, whole-module re-exports and import aliases, downloading packages with a lock file, unsigned indexes, Unicode text, and binary I/O.
- **Discovery, six questions:**
  - `format` is a **template** (the user's choice over the recommended runtime spec);
  - full tuples with **`zip` yielding tuples and `Pair` removed** (the user's choice over keeping `Pair`);
  - aliases plus whole-module re-exports;
  - git plus a lock file;
  - a `char` type;
  - checked bytes and any-integer indexes.
- Wrote `docs/specs/Language v14 Draft.md` (proposed):
  - milestones 45–50;
  - E0112, E0325, E0326, E0432, E0433;
  - the compatibility breaks listed.
- Proposed with the draft: moving `pub struct Checked` from `check/mod.rs` (300 lines) verbatim into a new `check/checked.rs`.

### FILES CREATED OR MODIFIED
- New: `docs/specs/Language v14 Draft.md`.
- CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (design only).

### DECISIONS MADE
- None recorded yet; the v14 MEMORY decision follows approval of the draft.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- The v14 draft and the `check/mod.rs` split await the user's approval (both approved at the start of session 96). M50 (downloads) needs a security review of its PRP.

---

## SESSION 96 — 2026-10-14 — Bytes and indexes (milestone 45) — closed
Branch: prp-074-bytes_and_indexes (merged into main)

### WHAT WAS DONE
- The user approved the v14 draft and the `check/mod.rs` split, building milestones 45–49 "in one go" and pausing before M50 for a security review. MEMORY 91.
- **The split:** `Checked` moved into `check/checked.rs`, committed straight to `main` (a slip, recorded).
- PRP-074 written as approved under that instruction, built and merged on green CI:
  - binary I/O;
  - `to_bytes`;
  - `string_from_utf8` (strict);
  - `read_stdin_bytes`;
  - any-integer indexes.
- `Language v14 Specification.md` begins.

### FILES CREATED OR MODIFIED
- New: `src/check/checked.rs`, `runtime/bytes.c`, `tests/programs/m45/` (`bytes`, `more`, `stdin`, `index_panic`), `docs/specs/Language v14 Specification.md`, PRP-074.
- Modified:
  - `src/check/{mod,input,builtins,access}.rs`;
  - `src/codegen/{io,function,heap,cast,methods}.rs`;
  - `src/link.rs`, `src/lib.rs`;
  - `tests/{cli,spec}.rs`;
  - the v14 draft.
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 91–92, CONTEXT.

### TESTS WRITTEN
- **Checker:** integer indexes, E0403, the byte intrinsics' types, E0302.
- **Codegen:** `zext i8` / `sext i32` indexes.
- **Programs:** `tests/programs/m45/`, and the §10 program from the spec.
- **Regression:** 157 earlier programs give byte-identical IR; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 91 (v14 direction), 92 (milestone 45).

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-074 merged into `main`, tagged `m45`.

---

## SESSION 97 — 2026-10-14 — Unicode text (milestone 46) — closed
Branch: prp-075-unicode_text (merged into main)

### WHAT WAS DONE
- PRP-075 written as approved under the "in one go" instruction, built and merged on green CI:
  - the `char` type and literals (E0112);
  - `chars()`, `char_count()`;
  - casts, `char_from`;
  - Latin-1 classification and case;
  - printing and quoting.
- **Spec:** keywords, E0112, grammar, types table, "Characters", string methods, the print rows, runtime, §9, the §10 program, §11 row 46.

### FILES CREATED OR MODIFIED
- New: `src/lexer/char_lit.rs`, `src/prelude_text.la`, `runtime/chars.c`, `tests/programs/m46/` (`text`, `more` with `.stdin`, `e0112`), PRP-075.
- Modified:
  - `src/lexer/{mod,token}.rs`, `src/ast/{expr,types}.rs`;
  - `src/parser/{primary,item,describe}.rs`, `src/parser/sexp/{mod,item}.rs`;
  - `src/check/{types,numbers,expr,ops,bounds,helpers,call,iter_helpers,printing}.rs`, `src/check/liveness/walk.rs`;
  - `src/codegen/{value,structs,expr,cast,runtime,input,show}.rs`;
  - `src/link.rs`, `src/lib.rs`, `tests/spec.rs`;
  - the v14 spec.
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 93, CONTEXT.

### TESTS WRITTEN
- **Lexer:** escapes and E0112.
- **Checker:** `char` types, comparisons, casts, `Ord`, helpers, E0404, E0408.
- **Codegen:** U+FFFD select, `lugha_rt_print_char`.
- **Programs:** `tests/programs/m46/`, and the §10 program from the spec.
- **Regression:** 161 earlier programs give byte-identical IR; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 93.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-075 merged into `main`, tagged `m46`.

---

## SESSION 98 — 2026-10-14 — Tuples (milestone 47) — closed
Branch: prp-076-tuples (merged into main)

### WHAT WAS DONE
- PRP-076 written as approved under the "in one go" instruction, built and merged on green CI:
  - tuple types `(A, B)` and values, `t.0` reads and writes;
  - tuple patterns in `match`, `if let`, `let` and `for (i, w) of …` (E0433 for refutable `let`/`for` patterns);
  - `==`, `<` (`lugha_cmp_<type>`), printing `(1, "a")`;
  - `zip` yields tuples; `Pair` removed.
- **Spec:** grammar, "Tuples", the zip row, E0433, §9, the §10 "Tuples" program, §11 row 47, the changes summary and compatibility.

### FILES CREATED OR MODIFIED
- New: `src/parser/tuples.rs`, `src/check/tuples.rs`, `src/check/errors/v14.rs`, `src/codegen/tuples.rs`, `tests/programs/m47/` (`tuples`, `more`, `e0433`), PRP-076.
- Modified:
  - `src/lexer/number.rs`, `src/ast/{expr,types,pattern,mod}.rs`;
  - `src/parser/{mod,primary,expr,generic,pattern,conditional,stmt}.rs`, `src/parser/sexp/{mod,item}.rs`;
  - `src/check/{mod,types,env,expr,stmt,patterns,enums,conditional,bounds,iter_helpers}.rs`, `src/check/errors/mod.rs`, `src/check/liveness/walk.rs`;
  - `src/codegen/{mod,expr,arith,conditional,matching,patterns,show,stmt}.rs`;
  - `src/prelude_more.la`, `tests/programs/m43/`, `tests/spec.rs`;
  - the v14 spec.
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 94, CONTEXT.

### TESTS WRITTEN
- **Parser:** tuple types, literals, `t.0.1`, patterns, destructuring, E0201.
- **Checker:** tuple types, generic `max` over tuples, E0433, E0417, E0404, E0410.
- **Codegen:** `%"(i64, string)"`, `lugha_cmp_`.
- **Programs:** `tests/programs/m47/`, updated `m43/helpers` and `m43/more`, and the §10 program from the spec.
- **Regression:** earlier programs give byte-identical IR except `zip` users and panic line numbers in `prelude_text.la` (PRP amendments); `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 94.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-076 merged into `main`, tagged `m47`.

---

## SESSION 99 — 2026-10-14 — format (milestone 48) — closed
Branch: prp-077-format (merged into main)

### WHAT WAS DONE
- PRP-077 written as approved under the "in one go" instruction, built and merged on green CI: `format("…", args…)` with a literal template, holes `{}`/`{:spec}`, `{{`/`}}`, E0432, `format` reserved.
- **Spec:** changes and compatibility, §2 templates, §5 `format`, §9, the E0432 row, the §10 "Format" program, §11 row 48, §12 note.

### FILES CREATED OR MODIFIED
- New: `src/template.rs`, `src/parser/format.rs`, `src/check/templates.rs`, `tests/programs/m48/` (`format`, `more`, `e0432`), PRP-077.
- Modified: `src/lib.rs`, `src/lexer/string.rs`, `src/ast/{expr,mod}.rs`, `src/parser/{mod,primary}.rs`, `src/parser/sexp/mod.rs`, `src/check/{mod,expr,input}.rs`, `src/check/errors/v14.rs`, `src/check/liveness/walk.rs`, `src/codegen/expr.rs`, `tests/spec.rs`, the v14 spec.
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 95, CONTEXT.

### TESTS WRITTEN
- **Lexer:** raw templates, `x.format(…)` still interpolating, templates inside interpolations, E0102.
- **Template:** pieces, holes, errors. **Parser:** the `format` form and ordinary calls.
- **Checker:** the `string` type, each E0432 case, E0431, E0407, E0302.
- **Programs:** `tests/programs/m48/`, and the §10 program from the spec.
- **Regression:** 165 earlier programs give byte-identical IR; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 95.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-077 merged into `main`, tagged `m48`.

---

## SESSION 100 — 2026-10-14 — Import aliases and re-exports (milestone 49) — closed
Branch: prp-078-import_aliases (merged into main)

### WHAT WAS DONE
- PRP-078 written as approved under the "in one go" instruction, built and merged on green CI:
  - `import a.b as c;` and `import a.b { X as Y };`;
  - `public import a.b [as v];` re-exporting a whole module;
  - `m.v.X` for calls, types, literals, associated calls, variants, bounds and function values; E0313 for a non-`public` item through a re-export.
- **Spec:** changes, grammar (`import`, `import_item`, `bound`, `base_type`), §6 "Aliases and whole-module re-exports", the E0201/E0313/E0314 rows, the §10 "Import aliases" program, §11 row 49, the out-of-scope list.

### FILES CREATED OR MODIFIED
- New: `src/check/reexports.rs`, `tests/programs/m49/` (`imports/`, `e0313/` directory cases), PRP-078.
- Modified:
  - `src/ast/{header,mod}.rs`;
  - `src/parser/{import,generic,interface,expr}.rs`, `src/parser/sexp/item.rs`;
  - `src/modules/{resolve,tests}.rs`;
  - `src/check/{mod,named_imports,construct,interfaces,iface_values,fun_values}.rs`, `src/check/modules/{mod,lookup}.rs`, `src/check/errors/v14.rs`;
  - `tests/spec.rs`, the v14 spec.
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 96, CONTEXT.

### TESTS WRITTEN
- **Parser:** aliases, item aliases, whole-module re-exports, `m.v.T` in types, bounds and literals.
- **Loader:** E0314 with aliases.
- **Checker:** every `m.v.X` form, item aliases (the original name unbound), E0313, E0301, E0305.
- **Programs:** `tests/programs/m49/`, and the §10 program from the spec.
- **Regression:** 167 earlier programs give byte-identical IR; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 96.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-078 merged into `main`, tagged `m49`.

---

## SESSION 101 — 2026-10-14 — Git packages PRP for security review (milestone 50) — closed
Branch: main

### WHAT WAS DONE
- Milestones 45–49 are all merged and tagged (`m45` … `m49`); the user's "in one go" is complete.
- PRP-079 (git packages, milestone 50) written as **proposed**, with its security design spelled out, and presented to the user for the security review the v14 draft requires. No code was written.

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-079-git_packages.md`.
- Modified: CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None; PRP-079's security design awaits the user's review.

### PENDING DECISIONS OPENED
- PRP-079's security design (user review).

### STILL OPEN AT CLOSE
- PRP-079 awaits approval; build it on branch `prp-079-git_packages` once approved.

---

## SESSION 102 — 2026-10-14 — Git packages (milestone 50) — closed
Branch: prp-079-git_packages (merged into main)

### WHAT WAS DONE
- The user approved PRP-079's security design as written; built and merged on green CI:
  - git manifest entries (`https://` and `file://` only, no credentials);
  - the per-user cache, `lugha.lock`, `lughac update`, `--offline`;
  - E0325, E0326.
- **Spec (now complete):** status, changes, §6 "Git packages" with the security rules, the CLI table, the E0321/E0325/E0326 rows, §11 row 50, the out-of-scope list, §12.

### FILES CREATED OR MODIFIED
- New: `src/modules/{git,cache,lock,fetch,fetch_tests}.rs`, `src/driver/update.rs`, `tests/git_packages.rs`.
- Modified:
  - `src/modules/{mod,inline,manifest,packages,tests}.rs`;
  - `src/driver/{mod,pipeline,build}.rs`, `src/check/modules/mod.rs`;
  - the v14 spec.
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 97, PRP-079 amendments, CONTEXT.

### TESTS WRITTEN
- **Unit:** URL rules, tag parsing, the `-c` flags and environment, cache escaping and roots, lock round-trip and E0321, inline git entries.
- **Loader (fake git):** highest tag, lock pinning, E0326 and `update`, E0325 (no tag, offline, missing repository), transitive git packages.
- **Integration:** `tests/git_packages.rs` with a real `file://` repository: fetch, lock, `--offline`, the symlink checked out as a plain file, a moved tag (E0326) and `lughac update`, E0325.
- **Regression:** 168 earlier programs give byte-identical IR.

### DECISIONS MADE
- MEMORY 97.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-079 merged into `main`, tagged `m50` and `v14`.

---

## SESSION 103 — 2026-10-14 — v15 plan noted in TODO (discovery paused) — closed
Branch: main

### WHAT WAS DONE
- Listed what remains after v14. The user chose system access and the Unicode follow-ups for v15, and asked for the rest to go to `TODO.md`.
- v15 discovery, question 1: the user chose directories, processes and environment variables (not file streams). At question 2 (process API shape) the user stopped the discovery and asked for v15 to be noted in `TODO.md` instead.
- `TODO.md` gains "v15 — planned, discovery paused" and "After v14 — noted, not scheduled".

### FILES CREATED OR MODIFIED
- `TODO.md`, CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- v15 scope (partial): directories, processes, environment variables, and Unicode follow-ups; no file streams.

### PENDING DECISIONS OPENED
- The rest of the v15 discovery: the process API shape first, then the remaining questions.

### STILL OPEN AT CLOSE
- Nothing; v15 waits for the user to resume its discovery.

---

## SESSION 104 — 2026-10-14 — Discovery: package name and source folder — closed
Branch: main

### WHAT WAS DONE
- Discovered while setting up the user's `glowworm` repository: `import glowworm.src.array` is unwanted, and `lughac run example/main.la` hits a `..` manifest path bug.
- Discussed how libraries and programs should be laid out. Discovery answers:
  - v15's first milestone;
  - `[package] name` checked (E0327);
  - `source` explicit;
  - programs inside a package use it by name through a bounded upward lookup;
  - the file argument stays required;
  - one file one module (E0328);
  - the lookup stops at `lugha.toml`, `.git` or `$HOME`.
- Wrote `docs/specs/Language v15 Draft.md` (M51 only) and PRP-080 (proposed, with two file splits for approval).

### FILES CREATED OR MODIFIED
- New: `docs/specs/Language v15 Draft.md`, `PRPs/prp-080-package_layout.md`.
- Modified: CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- The discovery answers above (to become MEMORY 98 on approval).

### PENDING DECISIONS OPENED
- Approval of the v15 draft, PRP-080, and the splits of `modules/mod.rs` and `modules/packages.rs`.

### STILL OPEN AT CLOSE
- Waiting for the user's approval; then branch `prp-080-package_layout`.

---

## SESSION 105 — 2026-10-14 — Package layout (milestone 51) — closed
Branch: prp-080-package_layout (merged into main)

### WHAT WAS DONE
- The user approved the v15 draft, PRP-080 and the two splits; built and merged on green CI:
  - `[package] name` (E0327) and `source`;
  - the bounded upward manifest lookup and the implicit package;
  - E0328;
  - the `..` path fix.
- Split `modules/mod.rs` (→ `parse.rs`) and `modules/packages.rs` (→ `paths.rs`), verbatim, as approved.
- **Spec:** `Language v15 Specification.md` begins from v14's (v14 frozen); `lughac spec` prints it. Status, changes, the `[package]` lines, §6 "Package layout", the E0321/E0327/E0328 rows, §11 row 51, out of scope, §12.

### FILES CREATED OR MODIFIED
- New: `src/modules/{parse,paths,package_table,layout,layout_tests}.rs`, `tests/package_layout.rs`, `docs/specs/Language v15 Specification.md`.
- Modified:
  - `src/modules/{mod,packages,manifest,tests,fetch_tests}.rs`;
  - `src/driver/{pipeline,update}.rs`, `src/check/modules/mod.rs`;
  - `src/lib.rs`, `tests/cli.rs`.
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 98–99, PRP-080 amendments, CONTEXT.

### TESTS WRITTEN
- **Unit:** the `[package]` lines and each E0321; the lookup's stops.
- **Loader:** the implicit package, `source` and E0327 through a listing, E0322, E0328, the `..` fix, E0320.
- **Integration:** `tests/package_layout.rs`: run from the parent and from `examples/`, a user listing by path, E0327, the `$HOME` bound, E0328.
- **Regression:** 168 earlier programs give byte-identical IR.

### DECISIONS MADE
- MEMORY 98 (the v15 draft), MEMORY 99 (M51).

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-080 merged into `main`, tagged `m51`.

---

## SESSION 106 — 2026-10-14 — Discovery: operator overloading — closed
Branch: main

### WHAT WAS DONE
- The user set the goal: their Vec2 program with `a + b` and `a * b` runs in v15.
- Discovery answers:
  - arithmetic operators plus unary `-`, via `add`/`sub`/`mul`/`div`/`rem`/`neg`;
  - any one right-hand type;
  - prelude `Add`…`Neg` and `Num` bounds in the same milestone;
  - `impl` bounds in the same milestone, written `impl NdArray<T: Num>`;
  - structural opt-in with shape rules;
  - `0`/`1` as `T: Num`, and `Num` for built-in numbers only.
- Added milestone 52 to `Language v15 Draft.md` (decisions 8–13 and choices) and wrote PRP-081 (proposed).

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-081-operator_overloading.md`.
- Modified: `docs/specs/Language v15 Draft.md`, CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- The discovery answers (MEMORY 100 on approval).

### PENDING DECISIONS OPENED
- Approval of the milestone 52 draft additions and PRP-081.

### STILL OPEN AT CLOSE
- Waiting for the user's approval; then branch `prp-081-operator_overloading`.

---

## SESSION 107 — 2026-10-14 — Operator overloading (milestone 52) — closed
Branch: prp-081-operator_overloading (merged into main)

### WHAT WAS DONE
- The user approved PRP-081 "in one go" and raised the file-size limit to 500 lines (CLAUDE.md). Built and merged on green CI:
  - operators on structs, enums and interface values through `add`/`sub`/`mul`/`div`/`rem`/`neg`;
  - compound assignment;
  - the prelude's operator bounds and `Num`;
  - `0`/`1` as `T: Num`;
  - bounds on `impl` blocks.
- The user's goal program runs: `Vec2 { x: 4.0, y: 6.0 }`, `Vec2 { x: 3.0, y: 8.0 }`.
- **Spec:** status, changes, compatibility, the `impl_decl` grammar, the operator bounds and `impl` bounds in §4, "Operator overloading" in §5, §9, the E0404/E0419 rows, the §10 program, §11 row 52.

### FILES CREATED OR MODIFIED
- New:
  - `src/check/{operators,operators_tests,impl_bounds}.rs`, `src/codegen/operators.rs`, `src/prelude_ops.la`;
  - `tests/programs/m52/` (`operators`, `more`, `overflow`, `e0404`, `e0419`).
- Modified:
  - `src/ast/mod.rs`, `src/parser/{generic,decl}.rs`, `src/parser/sexp/item.rs`;
  - `src/check/{mod,ops,expr,assign,literal,bounds,satisfy,method_calls,methods,iterators}.rs`, `src/check/errors/generics.rs`;
  - `src/codegen/{mod,expr,stmt,arith,instances}.rs`;
  - `src/lib.rs`, `tests/cli.rs`, `tests/spec.rs`, the v15 spec and draft.
- Docs: CLAUDE.md (limit, tree, STACK), README, CHANGELOG, TODO, MEMORY 100–101, PRP-081 amendments, CONTEXT.

### TESTS WRITTEN
- **Checker:** operator types, `v * 2.0`, `-v`, `+=`, `add<T: Add>` over numbers, strings and `Vec2`, `sum<T: Num>`, every error; `impl` bounds and E0419.
- **Codegen:** method calls for `+`, `-`, `+=`; `add<i64>` stays a built-in checked add (`tests/cli.rs`).
- **Programs:** `tests/programs/m52/`, and the §10 program from the spec.
- **Regression:** 168 earlier programs give byte-identical IR; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 100, 101.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-081 merged into `main`, tagged `m52`.

---

## SESSION 108 — 2026-10-14 — Move the prelude into src/prelude/ — closed
Branch: prelude-folder (merged into main)

### WHAT WAS DONE
- At the user's request, moved the five prelude files into `src/prelude/` (`core.la`, `iter.la`, `more.la`, `text.la`, `ops.la`), with `git mv`, so history follows them.
- Updated `src/lib.rs` (`PRELUDE`), the files' own "joined after" comments (each kept on its line, so prelude line numbers and panic locations don't move), and the CLAUDE.md tree and STACK line. Merged on green CI.

### FILES CREATED OR MODIFIED
- Moved: `src/prelude*.la` → `src/prelude/*.la`.
- Modified: `src/lib.rs`, CLAUDE.md, CHANGELOG, CONTEXT.

### TESTS WRITTEN
- None needed: a move. The full suite passes, and 171 programs give byte-identical IR.

### DECISIONS MADE
- None (housekeeping; the spec's "the prelude's third file" wording stays true).

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. Merged into `main`.

---

## SESSION 109 — 2026-10-14 — Std library direction noted — closed
Branch: main

### WHAT WAS DONE
- Discussed a standard library with the user. Direction:
  - std is bundled Lugha modules imported as `std.<module>`;
  - it is lazy, so it costs nothing unless imported;
  - the prelude stays small;
  - v15's directories, processes and env become `std.fs`, `std.process` and `std.env`;
  - `std.math` comes first.
- The user agreed: **no opt-out mechanism** (no `no_std`, no `deny` list for now).
- Recorded in `TODO.md` (v15 later milestones).

### FILES CREATED OR MODIFIED
- `TODO.md`, CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- Std direction and "no opt-out" (to be confirmed in its discovery and draft).

### PENDING DECISIONS OPENED
- The std discovery (modules, layout, how intrinsics relate).

### STILL OPEN AT CLOSE
- Nothing.

---

## SESSION 110 — 2026-10-14 — Discovery: std library — closed
Branch: main

### WHAT WAS DONE
- Discovery answers:
  - first milestone = std machinery plus `std.math`;
  - `math.pi()`/`math.e()` as functions;
  - `std` reserved, with normal import rules;
  - float functions generic over a new built-in `Float` bound, after the user asked "why not generics with Num": `sqrt` on integers would truncate, and a Lugha body can't cast `T`, so `Float` plus LLVM float intrinsics.
- Added milestone 53 to `Language v15 Draft.md` (decisions 14–18 and choices) and wrote PRP-082 (proposed).

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-082-std_math.md`.
- Modified: `docs/specs/Language v15 Draft.md`, CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- The discovery answers (MEMORY 102 on approval).

### PENDING DECISIONS OPENED
- Approval of the milestone 53 draft additions and PRP-082.

### STILL OPEN AT CLOSE
- Waiting for the user's approval; then branch `prp-082-std_math`.

---

## SESSION 111 — 2026-10-14 — The std library and std.math (milestone 53) — closed
Branch: prp-082-std_math (merged into main)

### WHAT WAS DONE
- The user approved PRP-082 "in one go"; built and merged on green CI:
  - bundled `std.<module>` modules loaded only when imported;
  - `std` reserved;
  - `$` built-ins lexed only in bundled sources;
  - the `Float` bound;
  - `std.math` (float functions via LLVM intrinsics, `abs`/`min`/`max`/`clamp`, `pi`/`e`).
- **Spec:** status, changes, identifiers (`$`), `Float` in §4, the std library in §6 with a "The standard library" table, §9, the E0312/E0320/E0321/E0419 rows, the §10 program, §11 row 53, out of scope.

### FILES CREATED OR MODIFIED
- New:
  - `src/std/math.la`, `src/check/float_ops.rs`, `src/codegen/float_ops.rs`, `src/modules/std_tests.rs`;
  - `tests/programs/m53/` (`math`, `more`, `clamp_panic`, `e0419`, `e0312`).
- Modified:
  - `src/lib.rs` (`STD`), `src/lexer/mod.rs`;
  - `src/modules/{mod,packages,parse,manifest,package_table}.rs`;
  - `src/check/{mod,expr,operators,bounds,iterators}.rs`, `src/check/errors/generics.rs`, `src/check/modules/mod.rs`;
  - `src/codegen/{mod,expr}.rs`, `src/driver/build.rs`, `src/prelude/ops.la`;
  - `tests/cli.rs`, `tests/spec.rs`, the v15 spec and draft.
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 102–103, PRP-082 amendments, CONTEXT.

### TESTS WRITTEN
- **Lexer:** `$` names. **Loader:** std modules, E0312, E0320, E0321.
- **Checker:** std import forms, `T: Float`, E0419.
- **CLI:** `llvm.sqrt.f64`/`.f32`, no libm.
- **Programs:** `tests/programs/m53/`, and the §10 program from the spec.
- **Regression:** 171 earlier programs give byte-identical IR; `-O2` checked by hand.

### DECISIONS MADE
- MEMORY 102, 103.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing. PRP-082 merged into `main`, tagged `m53`.

---

## SESSION 112 — 2026-10-14 — Noted the sum() cleanup and user-defined Ord — closed
Branch: main

### WHAT WAS DONE
- Reviewed what could move into the prelude. The user liked two items but chose to keep building glowworm first, so they are noted in `TODO.md`:
  - `it.sum()` as one `iter_sum<T: Num>` instead of ten per-type copies;
  - user-defined `Ord` through `fun cmp(self, other: Self): Ordering`, with recommended answers.

### FILES CREATED OR MODIFIED
- `TODO.md`, CONTEXT.md.

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None yet (recommendations recorded for a future discovery).

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing.

---

## SESSION 113 — 2026-10-14 — Discovery: std.random — closed
Branch: main

### WHAT WAS DONE
- The user asked for random numbers from std (firefly carries its own xorshift).
- Discovery answers:
  - xoshiro256\*\* with splitmix64 seeding;
  - `seeded(n)` plus `from_os()`, with no global generator;
  - the API `next_u64`, `uniform`, `below`, `between`, `normal`, `chance`, `choice`, `shuffle`.
- Added milestone 54 to `Language v15 Draft.md` and wrote PRP-083 (proposed). The PRP flags one change for sign-off: `uniform(): f64` instead of `uniform<T: Float>()`, because `T` can't be inferred with no arguments.

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-083-std_random.md`.
- Modified: `docs/specs/Language v15 Draft.md`, CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- The discovery answers (MEMORY 104 on approval).

### PENDING DECISIONS OPENED
- Approval of milestone 54, including `uniform(): f64`.

### STILL OPEN AT CLOSE
- Waiting for the user's approval; then branch `prp-083-std_random`.

---

## SESSION 114 — 2026-10-14 — std.random (milestone 54) — closed
Branch: prp-083-std_random (merged into main)

### WHAT WAS DONE
- The user approved PRP-083 with `uniform(): f64`. Built and merged on green CI:
  - `std.random`, a xoshiro256\*\* `Rng` with `seeded`/`from_os`, `next_u64`, `uniform`, `below`, `between`, `normal`, `chance`, `choice` and `shuffle`;
  - the `$float.from_f64` and `$os.seed` built-ins;
  - `runtime/random.c`.
- firefly now uses `std.random` (a thin wrapper keeps `rng.normals(shape, scale)`); XOR and the gradient check pass.
- **Spec:** status, changes, a `std.random` section and table, §9 and the runtime table, the §10 program, §11 row 54.

### FILES CREATED OR MODIFIED
- New:
  - `src/std/random.la`, `runtime/random.c`;
  - `tests/programs/m54/` (`random`, `more`, `below_panic`).
- Modified:
  - `src/lib.rs`, `src/link.rs`, `runtime/lugha_rt.c`;
  - `src/check/{float_ops,expr,mod}.rs`, `src/codegen/{float_ops,expr}.rs`;
  - `tests/cli.rs`, `tests/spec.rs`, `tests/programs/m53/e0312.stderr`, the v15 spec and draft.
- In firefly: `src/random.la`, `README.md`.
- Docs: CLAUDE.md, README, CHANGELOG, TODO, MEMORY 104–105, PRP-083 amendments, CONTEXT.

### TESTS WRITTEN
- **Checker:** `std.random` typing, E0419 for `normal(0, 1)`; `$os` not lexable in programs.
- **CLI:** `from_os` calls `lugha_rt_os_seed`, and seeded `main` doesn't.
- **Programs:** `tests/programs/m54/`, checked against a Python reference; the §10 program from the spec.
- **Regression:** 174 earlier programs give byte-identical IR; `-O2` output identical.

### DECISIONS MADE
- MEMORY 104, 105.

### PENDING DECISIONS OPENED
- None (lazy std modules noted in TODO).

### STILL OPEN AT CLOSE
- Nothing. PRP-083 merged into `main`, tagged `m54`.

---

## SESSION 115 — 2026-10-15 — Noted three numeric-code improvements — closed
Branch: main

### WHAT WAS DONE
- After writing glowworm and firefly, the user asked how Lugha feels to program in; at their request, the three changes that would help numeric code most went into `TODO.md`:
  - reading without copying;
  - nested `mut self` calls whose arguments only read the receiver;
  - converting any number to a `T: Num`.

### FILES CREATED OR MODIFIED
- `TODO.md`, CONTEXT.md.

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None (each needs its own discovery).

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing.

---

## SESSION 116 — 2026-10-15 — Discovery: read without copying (copy-on-write) — closed
Branch: main

### WHAT WAS DONE
- Explained "read without copying". Corrected an earlier claim: arguments and receivers are already never copied (§4).
- Measured firefly's IR: the costly copies are read-only `let` bindings from a container that is then modified (`Graph.back`: 18 copy calls). Static analysis can't remove these.
- Discovery answers:
  - copy-on-write (the user chose it over the recommended E0508 relaxation);
  - unshare before extern C;
  - success = tests plus timings, not gated.
- Found that DECISION-010 rejected copy-on-write. Opened DECISION-012 to supersede it, with the new facts (a sticky flag needs no reference counts; firefly's read-then-modify shape) and what still holds (no gain for copy-then-write programs; a flag check on every write).
- Added milestone 55 to `Language v15 Draft.md` (decisions 22–24, including the four hazards) and wrote PRP-084 (proposed).

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-084-copy_on_write.md`.
- Modified: `docs/specs/Language v15 Draft.md`, DECISIONS.md (DECISION-012, open), CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: DECISION-012 is open until the user approves PRP-084.

### PENDING DECISIONS OPENED
- DECISION-012 (copy-on-write, superseding DECISION-010).

### STILL OPEN AT CLOSE
- Waiting for the user's approval of PRP-084 and DECISION-012.

---

## SESSION 117 — 2026-10-15 — Copy-on-write arrays (milestone 55) — closed
Branch: prp-084-copy_on_write

### WHAT WAS DONE
- The user approved PRP-084 and DECISION-012 ("approved. build in one go"); DECISION-010 marked superseded; MEMORY 106.
- Built copy-on-write: copy sites mark (`cap` top bit), writes own each array on their path through two-phase places (`codegen/cow.rs`, `codegen/places.rs`); `for … of mut` via aliases; interface `mut self` receivers via their place; C grow masks the flag.
- Probes against the eager v14 compiler found two hazards beyond the plan (`swap` with a copying index; an `of mut` iterator whose body copies its array); both fixed and added to `hazards.la`.
- Spec v15 at milestone 55 (§4, §5 assignment order, §7, §8, §9, §10 "Copy-on-write", §11, §12 reversed); draft notes; docs.
- Timings: firefly mlp 5.25 → 4.06 s, gpt 1000 steps 20.06 → 13.82 s, 6000 steps 1 m 43 s → 1 m 30 s; histogram and matmul slower (140 → 217 ms, 143 → 188 ms); particles faster (140 → 86 ms).
- The user said "carry on, merge when CI is green".

### FILES CREATED OR MODIFIED
- New: `src/codegen/cow.rs`, `src/codegen/places.rs`, `tests/programs/m55/` (writes, hazards).
- Modified: codegen `array.rs`, `copy.rs`, `dispatch.rs`, `growth.rs`, `iterators.rs`, `lower.rs`, `matching.rs`, `mod.rs`, `scope.rs`, `stmt.rs`, `structs.rs`, `swap.rs`; `runtime/lugha_rt.c`; `tests/spec.rs`; the v15 spec and draft; CLAUDE.md, README, TODO, CHANGELOG, MEMORY (107), PRP-084.

### TESTS WRITTEN
- Unit: `a_copy_marks_instead_of_copying`, `a_write_owns_its_array_first`; copy/growth/iterator/matching tests updated to copy-on-write IR.
- Programs: `m55/writes.la`, `m55/hazards.la`; the §10 "Copy-on-write" program.

### DECISIONS MADE
- DECISION-012 resolved (copy-on-write); MEMORY 107 records the built design.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing; merged and tagged `m55`.

---

## SESSION 118 — 2026-10-15 — Discovery: owning arrays once per loop — closed
Branch: main

### WHAT WAS DONE
- Rebuilt firefly's examples with the M55 compiler; updated its README timings (pushed `e61c888`, at the user's request).
- Discovery for the TODO item "Own an array once per loop". The user chose every recommended answer:
  - compiler hoisting;
  - a strict syntactic whitelist per root;
  - every loop kind at every nesting level, with guarded hoists for indexed paths;
  - milestone 56, timings reported but not gated.
- Added decisions 25–28 to `Language v15 Draft.md` and wrote PRP-085 (proposed).

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-085-loop_ownership.md`.
- Modified: `docs/specs/Language v15 Draft.md`, CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: they wait for the user's approval of PRP-085.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Waiting for the user's approval of PRP-085.

---

## SESSION 119 — 2026-10-15 — Owning arrays once per loop (milestone 56) — closed
Branch: prp-085-loop_ownership

### WHAT WAS DONE
- The user approved PRP-085 ("build in one go"); MEMORY 108.
- Built the loop scan (`codegen/hoist_scan.rs`) and the hoists (`codegen/hoist.rs`): every loop kind, fixed paths, guarded bounds, `own_place` in the write paths.
- Tests first: six IR unit tests; `m56/hoisting`, `hazards`, `bounds` (a panic golden), and `edges` (ten adversarial probes), all with the M55 compiler's output.
- Spec v15 at milestone 56 (§7, §9 lowering note, §11 row 56, §12, the changes summary); draft notes; docs.
- Timings: matmul 131 (eager) → 173 (M55) → 133 ms (M56); histogram 130 → 203 → 145 ms; firefly mlp 4.54 → 4.19 s and gpt 15.7 → 14.5 s (interleaved under load).
- The user said "build in one go": merged on green CI.

### FILES CREATED OR MODIFIED
- New: `src/codegen/hoist.rs`, `src/codegen/hoist_scan.rs`, `tests/programs/m56/`.
- Modified: codegen `array.rs`, `conditional.rs`, `control.rs`, `cow.rs`, `growth.rs`, `iterators.rs`, `lower.rs`, `mod.rs`, `places.rs`; the v15 spec and draft; CLAUDE.md, README, TODO, CHANGELOG, MEMORY (108, 109), PRP-085.

### TESTS WRITTEN
- Unit: `a_counting_loop_owns_its_array_once`, `nested_loops_own_each_fixed_path`, `every_kind_of_loop_hoists`, `reads_without_arrays_dont_block`, `copies_and_calls_keep_the_test`, `changing_indexes_are_not_fixed`; `of_mut_aliases_elements_and_steps_the_place_v13` updated.
- Programs: `m56/hoisting.la`, `hazards.la`, `bounds.la`, `edges.la`.

### DECISIONS MADE
- MEMORY 109 records the built design.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing; merged and tagged `m56`.

---

## SESSION 120 — 2026-10-15 — Discovery: nested `mut self` calls — closed
Branch: main

### WHAT WAS DONE
- Firefly: committed the stacked-layers GPT (`ce6f38b`, not pushed); the user's 4-layer, 20,000-step run reached validation 2.216 (vs 2.242).
- The user picked three TODO items, in order: nested `mut self` calls, any number as a `T: Num`, user-defined `Ord`.
- Discovery for nested `mut self` calls. The user chose every recommended answer:
  - E0508 only for arguments that can be the receiver's storage;
  - assignment order for `mut self` calls;
  - E0510 relaxed for receivers when no index lies between;
  - milestone 57 plus a firefly rewrite.
- Added decisions 29–32 to `Language v15 Draft.md` and wrote PRP-086 (proposed).

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-086-nested_mut_self.md`.
- Modified: `docs/specs/Language v15 Draft.md`, CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: they wait for the user's approval of PRP-086.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Waiting for the user's approval of PRP-086.

---

## SESSION 121 — 2026-10-15 — Discovery: any number as `T: Num`, user-defined `Ord` — closed
Branch: main

### WHAT WAS DONE
- The user asked for all three PRPs to be written, to approve and build them in one go.
- **Numbers as `T`:** the user chose every recommendation:
  - `as` casts with `T: Num`/`Float`;
  - integer literals as `T: Num` and float literals as `T: Float`, with E0402 per instance;
  - milestone 58 plus the glowworm/firefly cleanup.
- **User-defined `Ord`:** the user chose every recommendation:
  - `cmp(self, other: Self): Ordering` with a prelude `Ordering`;
  - tuples through `cmp`, `xs.sort()`, prefixed variants;
  - milestone 59, tests only.
- Added decisions 33–40 to `Language v15 Draft.md`; wrote PRP-087 and PRP-088 (proposed).

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-087-numbers_as_t.md`, `PRPs/prp-088-user_ord.md`.
- Modified: `docs/specs/Language v15 Draft.md`, CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: PRP-086, 087 and 088 wait for the user's approval.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Waiting for the user's approval of PRP-086, PRP-087 and PRP-088.

---

## SESSION 122 — 2026-10-15 — Nested `mut self` calls (milestone 57) — closed
Branch: prp-086-nested_mut_self

### WHAT WAS DONE
- The user approved PRP-086, 087 and 088 ("approved. build in one go"); MEMORY 110.
- Tests first:
  - checker tests for every allowed and still-rejected E0508 form;
  - E0510 receiver tests;
  - a codegen order test;
  - `m57/nested.la` against `m57/flat.la` (goldens from the flat form under M56);
  - `e0508.la`, `e0510.la`.
- Built:
  - E0508 by value tails and pointer-passed types;
  - E0510 for receivers only across an index (`Path::indexed_below`);
  - receivers planned before the arguments and committed after (`methods.rs`, `dispatch.rs`); `place_address` removed.
- Spec v15 at milestone 57 (§4 "No aliasing", §5 order and E0510, §9 rows, §10 "Nested calls", §11 row 57); draft; docs.
- firefly: `layer`/`norm` written nested, identical output (`a891512`, not pushed).

### FILES CREATED OR MODIFIED
- New: `tests/programs/m57/` (nested, flat, e0508, e0510).
- Modified:
  - `src/check/method_calls.rs`, `conflicts.rs`, `array.rs`;
  - `src/codegen/methods.rs`, `dispatch.rs`, `places.rs`;
  - `tests/spec.rs`; the v15 spec and draft;
  - CLAUDE.md, README, TODO, CHANGELOG, MEMORY (110, 111), PRP-086.

### TESTS WRITTEN
- `arguments_may_mention_the_receiver_unless_they_can_be_it`, `receivers_allow_changes_to_containers_without_an_index_between`, `mut_self_arguments_run_before_the_receiver_is_reached`; programs above; §10 "Nested calls".

### DECISIONS MADE
- MEMORY 111 records the built design.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing; merged and tagged `m57`.

---

## SESSION 123 — 2026-10-15 — Numbers as `T` (milestone 58) — closed
Branch: prp-087-numbers_as_t

### WHAT WAS DONE
- MEMORY 112. Tests first:
  - checker tests (casts both ways and E0408, literals and E0401, E0402 per instance incl. methods and closures);
  - `m58/numbers.la` (golden from hand-instantiated code under M57);
  - `m58/e0402_instance.la`.
- Built: `numeric` casts in `check/ops.rs`; any literal as `T` and `instance_literals` in `check/literal.rs`; `errors::out_of_range_in`.
- Spec v15 at milestone 58 (§4 casts and operator bounds, §9 rows, §10 "Numbers as T", §11 row 58); docs.
- glowworm `arange`/`linspace`/`mean` simplified (`b0683c3`, local); firefly unchanged (amendment 1).

### FILES CREATED OR MODIFIED
- New: `tests/programs/m58/`.
- Modified:
  - `src/check/literal.rs`, `ops.rs`, `instances.rs`, `mod.rs`, `modules/mod.rs`, `errors/types.rs`, `operators_tests.rs`;
  - `tests/spec.rs`; the v15 spec and draft;
  - CLAUDE.md, README, TODO, CHANGELOG, MEMORY (112, 113), PRP-087.

### TESTS WRITTEN
- `numeric_type_parameters_cast_both_ways_v15`, `any_literal_is_a_numeric_type_parameter_v15`, `literals_out_of_range_for_an_instance_are_e0402_v15`; the programs above; §10 "Numbers as T".

### DECISIONS MADE
- MEMORY 113 records the built design.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing; merged and tagged `m58`.

---

## SESSION 124 — 2026-10-15 — User-defined `Ord` (milestone 59) — closed
Branch: prp-088-user_ord

### WHAT WAS DONE
- MEMORY 114. Tests first:
  - checker tests (`cmp` shapes, generic `impl` bounds, a module's own `Ordering`, `sort` E0419);
  - `m59/ordering.la` (expected output by hand);
  - `e0404.la`, `e0419.la`;
  - `tests/ordering.rs` (IR calls `cmp`, tuple helper included).
- Built:
  - `check/ordering.rs`, `ord_ok` through `has_cmp`, uses and help;
  - `cmp_targets` in `monomorphize`;
  - `codegen/ordering.rs`;
  - prelude `Ordering` and `sort` (in `ops.la`); `prelude_type` and `enum_named` resolve `Ordering`;
  - non-generic enums print by key.
- Spec v15 at milestone 59 (§4 order, the helper table, §9 rows and lowering, §10 "Ordering", §11 row 59, compatibility note); docs.

### FILES CREATED OR MODIFIED
- New: `src/check/ordering.rs`, `src/codegen/ordering.rs`, `tests/ordering.rs`, `tests/programs/m59/`.
- Modified:
  - `src/check/bounds.rs`, `ops.rs`, `instances.rs`, `results.rs`, `construct.rs`, `types.rs`, `helpers.rs`, `mod.rs`, `closures.rs`, `method_calls.rs`;
  - `src/codegen/arith.rs`, `tuples.rs`, `mod.rs`; `src/prelude/ops.la`;
  - `tests/programs/m9/e0404_struct_order.stderr`, `tests/spec.rs`; the v15 spec and draft;
  - CLAUDE.md, README, TODO, CHANGELOG, MEMORY (114, 115), PRP-088.

### TESTS WRITTEN
- `a_type_with_cmp_is_ord`, `other_shapes_are_not_ord`, `a_module_s_own_ordering_means_its_own`, `ordering_a_user_type_calls_its_cmp`; the programs above; §10 "Ordering".

### DECISIONS MADE
- MEMORY 115 records the built design, including non-generic enums printing by key.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing; merged and tagged `m59`.

---

## SESSION 125 — 2026-10-15 — Discovery: lazy std modules — closed
Branch: main

### WHAT WAS DONE
- Explained "lazy std modules" with a measured example: a program using `random.seeded` and `r.below` also compiles `from_os`, `uniform`, `between`, `chance`, `math.pi` and `math.e`.
- Discovery. The user chose every recommendation:
  - keep checking every std body, compile only what's reached;
  - std only (packages and program modules stay eager);
  - milestone 60 with IR tests.
- Added decisions 41–43 to `Language v15 Draft.md`; wrote PRP-089 (proposed).

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-089-lazy_std.md`.
- Modified: `docs/specs/Language v15 Draft.md`, CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: PRP-089 waits for the user's approval.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Waiting for the user's approval of PRP-089.

---

## SESSION 126 — 2026-10-15 — Lazy std modules (milestone 60) — closed
Branch: prp-089-lazy_std

### WHAT WAS DONE
- The user approved PRP-089 ("yes build"); MEMORY 116.
- Tests first: `tests/lazy_std.rs` (unused std functions absent; reached through a call, a method, a bound, an interface value and a function value present).
- Built: `lazy_key` in the checker, `is_lazy` in codegen, function values recorded as uses, a `monos` fallback for function values.
- Spec v15 at milestone 60 (§6 std modules, §9 lowering note, §11 row 60, the changes summary); docs.
- IR sizes: a `seeded` + `below` program 423 → 314 lines; firefly about 70 lines smaller; every output unchanged.

### FILES CREATED OR MODIFIED
- New: `tests/lazy_std.rs`.
- Modified:
  - `src/check/instances.rs`, `fun_values.rs`;
  - `src/codegen/declare.rs`, `lower.rs`, `methods.rs`, `fun_values.rs`;
  - the v15 spec and draft;
  - CLAUDE.md, README, TODO, CHANGELOG, MEMORY (116, 117), PRP-089.

### TESTS WRITTEN
- `only_what_a_program_reaches_is_compiled`, `every_way_of_reaching_compiles_the_target`.

### DECISIONS MADE
- MEMORY 117 records the built design.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing; merged and tagged `m60`.

---

## SESSION 127 — 2026-10-15 — Discovery: the rest of the v15 TODO — closed
Branch: main

### WHAT WAS DONE
- The user asked to "swallow the remaining TODO in one PRP". I flagged the scope rule, std.process's security review and the §11 "full Unicode tables" exclusion; the user chose one approval with a PRP per item.
- Discovery. The user chose every recommendation:
  - C → Lugha: all three, over one decoder;
  - `std.fs`: plain functions, no recursive delete;
  - `std.env`: read-only `get` and `current_dir`;
  - `std.process`: `run`, `run_with` and `call`, with PATH lookup, no shell, an inherited environment, only fds 0–2, UTF-8 output or `Err`, and `128 + signal`;
  - Unicode: widths only, with the tables staying out of scope.
- Added decisions 44–52 and milestone rows 61–66 to `Language v15 Draft.md`.
- Interrupted by the user before the PRPs were written.

### FILES CREATED OR MODIFIED
- Modified: `docs/specs/Language v15 Draft.md`, CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: the PRPs aren't written or approved yet.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Writing PRP-090 to PRP-095.

---

## SESSION 128 — 2026-10-15 — PRPs for milestones 61–66 — closed
Branch: main

### WHAT WAS DONE
- Wrote PRP-090 to PRP-095 from draft decisions 44–52 (all proposed).
- PRP-095 (`std.process`) carries a full security section for the user's review: `posix_spawnp`, no shell, NUL rejected, close-on-exec, `poll`, `128 + signal`, UTF-8 output.

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-090-one_iter_sum.md`, `prp-091-c_to_lugha.md`, `prp-092-widths_in_chars.md`, `prp-093-std_env.md`, `prp-094-std_fs.md`, `prp-095-std_process.md`.
- Modified: CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: the PRPs wait for approval.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Waiting for the user's approval of PRP-090 to PRP-095 (PRP-095 as a security review).

---

## SESSION 129 — 2026-10-15 — One `iter_sum` (milestone 61) — closed
Branch: prp-090-one_iter_sum

### WHAT WAS DONE
- The user approved PRP-090 to PRP-095 ("approved"); PRP-095's approval is the security review. MEMORY 118.
- Tests first: `m61/sums.la` (golden from M60) and `every_sum_is_one_generic_helper` in `tests/lazy_std.rs`.
- Built: one `iter_sum<T: Num>` in `prelude/iter.la`; `iter_helpers.rs` maps `sum` to it. `m36/overflow`'s prelude line updated.
- Spec v15 at milestone 61 (§9 note, §11 row 61, summary); docs.

### FILES CREATED OR MODIFIED
- New: `tests/programs/m61/`.
- Modified: `src/prelude/iter.la`, `src/check/iter_helpers.rs`, `tests/lazy_std.rs`, `tests/programs/m36/overflow.stderr`, the v15 spec and draft, CLAUDE.md, TODO, CHANGELOG, MEMORY (118, 119), PRP-090 (and approval status in PRP-091 to PRP-095).

### TESTS WRITTEN
- `every_sum_is_one_generic_helper`; `m61/sums.la`.

### DECISIONS MADE
- MEMORY 119.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing; merged and tagged `m61`.

---

## SESSION 130 — 2026-10-15 — C → Lugha (milestone 62) — closed
Branch: prp-091-c_to_lugha

### WHAT WAS DONE
- Tests first:
  - `m62/decode.la` (parser edge cases, all 65,536 two-byte inputs, and chosen 3- and 4-byte sequences; golden from the M61 C implementation);
  - `m62/dollar.la` (E0101);
  - a link test that the C functions are gone.
- Built:
  - `utf8_decode`, `text_byte`, `bytes_byte`, `text_from_utf8`, `text_parse_i64`/`u64` in `prelude/text.la`; `Chars.next` uses the decoder;
  - `check::prelude_helper`, `$string.from_bytes` (checker and codegen);
  - `prelude_intrinsic` in codegen; `lugha_rt_str_from_bytes`; three C functions removed.
- Spec v15 at milestone 62 (§9 runtime rows, §11 row 62, summary); docs.

### FILES CREATED OR MODIFIED
- New: `tests/programs/m62/`.
- Modified:
  - `src/prelude/text.la`;
  - `src/check/input.rs`, `float_ops.rs`, `expr.rs`, `mod.rs`;
  - `src/codegen/input.rs`, `io.rs`, `function.rs`, `expr.rs`;
  - `runtime/input.c`, `bytes.c`, `lugha_rt.c`; `src/link.rs`;
  - the v15 spec and draft, CLAUDE.md, TODO, CHANGELOG, MEMORY (120), PRP-091.

### TESTS WRITTEN
- `m62/decode.la`, `m62/dollar.la`, `the_runtime_no_longer_parses_or_checks_utf8_v15`.

### DECISIONS MADE
- MEMORY 120.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing; merged and tagged `m62`.

---

## SESSION 131 — 2026-10-15 — Format widths in characters (milestone 63) — closed
Branch: prp-092-widths_in_chars

### WHAT WAS DONE
- Tests first: `m63/widths.la` (expected output by hand); format.rs unit tests for non-ASCII fills.
- Built: `FormatSpec.fill: char`; codegen passes the code point; `pad_bytes` counts characters and writes the fill's UTF-8; `utf8_encode` forward-declared in the core.
- `m46/more.stdout` updated for its `é` padding line (the intended fix).
- Spec v15 at milestone 63 (format specifiers, row 63, summary); docs; TODO records the tables ruling.

### FILES CREATED OR MODIFIED
- New: `tests/programs/m63/`.
- Modified: `src/format.rs`, `src/codegen/format.rs`, `runtime/format.c`, `runtime/lugha_rt.c`, `tests/programs/m46/more.stdout`, the v15 spec and draft, CLAUDE.md, README, TODO, CHANGELOG, MEMORY (121), PRP-092.

### TESTS WRITTEN
- `m63/widths.la`; format.rs fill tests.

### DECISIONS MADE
- MEMORY 121.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing; merged and tagged `m63`.

---

## SESSION 132 — 2026-10-15 — `std.env` (milestone 64) — closed
Branch: prp-093-std_env

### WHAT WAS DONE
- Tests first: `m64/env.la` with `env.envvars`; runner `.envvars` support and `envvars_are_key_value_lines`.
- Built: `src/std/env.la`; `$os.env`/`$os.cwd` (checker table `os_signature`, `codegen/os.rs`); `runtime/env.c`.
- `m53/e0312.stderr` lists `std.env`.
- Spec v15 at milestone 64 (std.env table, §9 notes and runtime row, §10 "Environment", §11 row 64); docs.

### FILES CREATED OR MODIFIED
- New: `src/std/env.la`, `src/codegen/os.rs`, `runtime/env.c`, `tests/programs/m64/`.
- Modified:
  - `src/lib.rs`, `src/link.rs`;
  - `src/check/float_ops.rs`; `src/codegen/expr.rs`, `io.rs`, `mod.rs`;
  - `tests/support/runner/args.rs`, `discover.rs`, `execute.rs`; `tests/programs/m53/e0312.stderr`, `tests/spec.rs`;
  - the v15 spec and draft, CLAUDE.md, README, TODO, CHANGELOG, MEMORY (122), PRP-093.

### TESTS WRITTEN
- `m64/env.la`, `envvars_are_key_value_lines`, §10 "Environment".

### DECISIONS MADE
- MEMORY 122.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing; merged and tagged `m64`.

---

## SESSION 133 — 2026-10-15 — `std.fs` (milestone 65) — closed
Branch: prp-094-std_fs

### WHAT WAS DONE
- Tests first: `m65/fs.la` with `.scratch` (expected output by hand: POSIX messages, sorting, the NUL path).
- Built: `src/std/fs.la`; five `$os` built-ins (checker table, `codegen/os.rs`); `runtime/fs.c`; includes in the core.
- `m53/e0312.stderr` lists `std.fs`.
- Spec v15 at milestone 65 (std.fs table, §9 notes and runtime row, §10 "Directories", §11 row 65); docs.

### FILES CREATED OR MODIFIED
- New: `src/std/fs.la`, `runtime/fs.c`, `tests/programs/m65/`.
- Modified:
  - `src/lib.rs`, `src/link.rs`, `runtime/lugha_rt.c`;
  - `src/check/float_ops.rs`, `src/codegen/os.rs`;
  - `tests/programs/m53/e0312.stderr`, `tests/spec.rs`;
  - the v15 spec and draft, CLAUDE.md, README, TODO, CHANGELOG, MEMORY (123), PRP-094.

### TESTS WRITTEN
- `m65/fs.la`, §10 "Directories".

### DECISIONS MADE
- MEMORY 123.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing; merged and tagged `m65`.

---

## SESSION 134 — 2026-10-15 — `std.process` (milestone 66) — closed
Branch: prp-095-std_process

### WHAT WAS DONE
- Tests first: `m66/process.la` with `.scratch` (expected output by hand). It covers:
  - arguments with spaces, stdin, exit codes and signals;
  - a missing program; shell syntax kept as text; stderr;
  - output order around `call`; NULs; non-UTF-8 output;
  - 1 MB on both pipes; and the child's fds.
- Built: `src/std/process.la`; `$os.spawn` (tuple result); `runtime/process.c` (`posix_spawnp`, pipes, `poll`, `SIGPIPE`, `waitpid`); `fopen` mode `"e"`; includes in the core.
- Five repeated `-O2` runs gave identical output.
- Spec v15 at milestone 66 (the std.process section with the PATH caveat, §9 notes and runtime row, §10 "Processes", §11 row 66); docs; the security rule added to CLAUDE.md.

### FILES CREATED OR MODIFIED
- New: `src/std/process.la`, `runtime/process.c`, `tests/programs/m66/`.
- Modified:
  - `src/lib.rs`, `src/link.rs`;
  - `runtime/lugha_rt.c`, `runtime/input.c`;
  - `src/check/float_ops.rs`, `src/codegen/os.rs`;
  - `tests/programs/m53/e0312.stderr`, `tests/spec.rs`;
  - the v15 spec and draft, CLAUDE.md, README, TODO, CHANGELOG, MEMORY (124), PRP-095.

### TESTS WRITTEN
- `m66/process.la`, §10 "Processes".

### DECISIONS MADE
- MEMORY 124.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing; merged and tagged `m66`.

---

## SESSION 135 — 2026-10-15 — Discovery: shared references (`Ref<T>`) — closed
Branch: main

### WHAT WAS DONE
- Explained shared references, how to live without them, a `Ref<T>` design, and why there's no pointer arithmetic.
- Built oyster's shell for the user earlier in this conversation (lexer, parser, executor, REPL; uncommitted in the user's repo).
- Discovery for `Ref<T>`. The user chose every recommendation:
  - `Ref<T>` opens v16;
  - copy in and out only (`new`, `get`, `set`, `update`, `is`);
  - not `Eq`; printing cuts cycles;
  - no `let mut` needed to change a box;
  - freeze v15, milestone 67.
- Started `docs/specs/Language v16 Draft.md` (decisions 1–5); wrote PRP-096 (proposed).

### FILES CREATED OR MODIFIED
- New: `docs/specs/Language v16 Draft.md`, `PRPs/prp-096-ref.md`.
- Modified: CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: PRP-096 waits for approval.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Waiting for the user's approval of PRP-096.

---

## SESSION 136 — 2026-10-15 — Shared boxes `Ref<T>` (milestone 67, v16) — closed
Branch: prp-096-ref

### WHAT WAS DONE
- The user approved PRP-096 ("build it"); MEMORY 125.
- Tests first:
  - checker tests in `check/refs.rs`;
  - `m67/refs.la` (counter closure, one box through struct, array, interface value and return; `get`/`set` copies; generic `bump`; printing);
  - `m67/linked.la` (a doubly linked list; self and two-node cycles printed);
  - `m67/e0404.la`, and `m67/cow.la` (copy-on-write and loop ownership with boxes).
- Built:
  - `Type::Ref` and its checker arms; `Ref.new` and the four methods;
  - codegen `refs.rs`; `call_pair`; the runtime print-cycle stack.
- Froze v15 and started `Language v16 Specification.md` (`lughac spec` prints it), with §4, §7 and §9 text, §10 "Shared boxes", §11 row 67 and §12; docs.

### FILES CREATED OR MODIFIED
- New: `src/check/refs.rs`, `src/codegen/refs.rs`, `tests/programs/m67/`, `docs/specs/Language v16 Specification.md`.
- Modified:
  - `src/check/types.rs`, `generics.rs`, `infer.rs`, `bounds.rs`, `layouts.rs`, `printing.rs`, `levels.rs`, `builtins.rs`, `method_calls.rs`, `ops.rs`, `mod.rs`;
  - `src/codegen/value.rs`, `structs.rs`, `enums.rs`, `copy.rs`, `show.rs`, `methods.rs`, `fun_values.rs`, `mod.rs`;
  - `runtime/print.c`; `src/link.rs`, `src/lib.rs`;
  - `tests/cli.rs`, `tests/spec.rs`;
  - the v15 spec (frozen), the v16 draft, CLAUDE.md, README, TODO, CHANGELOG, MEMORY (125, 126), PRP-096.

### TESTS WRITTEN
- `boxes_are_made_read_and_changed_without_mut`, `boxes_are_not_eq_v16`, `nothing_reaches_inside_a_box`, `recursion_through_a_box_and_shadowing`; the m67 programs; §10 "Shared boxes".

### DECISIONS MADE
- MEMORY 126.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing; merged and tagged `m67`.

---

## SESSION 137 — 2026-10-15 — Discovery: changing the working directory — closed
Branch: main

### WHAT WAS DONE
- The user asked for `cd` in oyster.
- Discovery: the user chose `env.set_current_dir` (process-wide) and milestone 68 plus oyster's `cd`.
- Added v16 draft decisions 6–8; wrote PRP-097 (proposed).

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-097-set_current_dir.md`.
- Modified: `docs/specs/Language v16 Draft.md`, CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: PRP-097 waits for approval.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Waiting for the user's approval of PRP-097.

---

## SESSION 138 — 2026-10-15 — Changing the working directory (milestone 68) — closed
Branch: prp-097-set_current_dir

### WHAT WAS DONE
- The user approved PRP-097 ("build it"); MEMORY 127.
- Tests first: `m68/cd.la` with `.scratch` (expected output by hand).
- Built: `set_current_dir` in `std/env.la`, `$os.chdir`, `lugha_rt_chdir`.
- Spec v16 at milestone 68 (std.env table, runtime row, §10 "Changing directory", §11 row 68); docs.
- After the merge: oyster's `cd` (`cd dir`, `~`, `cd`, `cd -`), checked with a scripted session; uncommitted in the user's repo.

### FILES CREATED OR MODIFIED
- New: `tests/programs/m68/`.
- Modified:
  - `src/std/env.la`, `src/check/float_ops.rs`, `src/codegen/os.rs`, `runtime/env.c`, `src/link.rs`;
  - `tests/spec.rs`; the v16 spec and draft;
  - CLAUDE.md, README, TODO, CHANGELOG, MEMORY (127, 128), PRP-097.
- Outside the repo: oyster `src/exec.la` (uncommitted).

### TESTS WRITTEN
- `m68/cd.la`; §10 "Changing directory".

### DECISIONS MADE
- MEMORY 128.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing; merged and tagged `m68`.

---

## SESSION 139 — 2026-10-15 — Discovery: VS Code extension — closed
Branch: main

### WHAT WAS DONE
- The user asked how to build a VS Code extension for Lugha.
- Discovery: same repo (`editors/vscode/`); highlighting + diagnostics (a language server later); checks on open and save; a file with `main` checks itself, a module checks `lugha.entries`; one notice / output channel on failure; always `--offline`; `node:test` + grammar tests; a local `.vsix` only.
- Found: JSON `col` counts bytes; imported files are named relative to the main file's folder; a module checked alone gives E0303.
- Wrote PRP-098 (proposed).

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-098-vscode_extension.md`.
- Modified: CONTEXT.md.

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: PRP-098 waits for approval.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Waiting for the user's approval of PRP-098, and their confirmation of the GPL variant for `package.json`.
- Later: renumbered to PRP-099 in session 140 (a concurrent session also used 098 and session 139).

---

## SESSION 139 — 2026-10-15 — Discovery: `lughac init` for programs and libraries — closed
Branch: main

### WHAT WAS DONE
- Explained why Lugha has no `target/` folder (whole-program builds, nothing cached; firefly's GPT builds in 0.4 s at `-O0`, 1.3 s at `-O2`).
- Discovery for `lughac init`. The user chose every recommendation:
  - `--lib` flag, a program by default;
  - a program gets `src/main.la`; a library gets `src/<name>.la` plus `examples/hello.la`;
  - never overwrite; refuse an existing manifest or a bad name (exit 2); no git;
  - milestone 69.
- Added v16 draft decisions 9–12; wrote PRP-098 (proposed).

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-098-init.md`.
- Modified: `docs/specs/Language v16 Draft.md`, CONTEXT.md, MEMORY.md (start point).

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: PRP-098 waits for approval.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Waiting for the user's approval of PRP-098.

---

## SESSION 140 — 2026-10-15 — VS Code extension (PRP-099) — closed
Branch: prp-099-vscode_extension

### WHAT WAS DONE
- The user approved the extension ("build it then merge to main").
- A concurrent session had also taken PRP-098 and session 139 (`lughac init`); the user chose to renumber this PRP to 099. Built in a separate git worktree (`../lugha-prp-099`) so that session's checkout of `main` was never touched; merged through a GitHub PR.
- Tests first: `node:test` for diagnostics, targets, store and runner (the runner against the real `lughac`, including an offline check whose fake `git` must never run), a spec-keyword test, `vscode-tmgrammar-test` scope tests.
- Built `editors/vscode/`: grammar, language configuration, `targets`/`runner`/`diagnostics`/`store`, and `extension.ts`; exercised the glue with a stub `vscode` (not committed); the `.vsix` installs.
- CI runs the extension's tests after `cargo test`; `Cargo.toml` excludes `editors/`.

### FILES CREATED OR MODIFIED
- New: `editors/vscode/` (package.json, package-lock.json, tsconfig.json, .gitignore, .vscodeignore, README.md, language-configuration.json, syntaxes/, src/, test/).
- Renamed: `PRPs/prp-098-vscode_extension.md` → `PRPs/prp-099-vscode_extension.md`.
- Modified: `.github/workflows/ci.yml`, `Cargo.toml`, CLAUDE.md (tree, COMMANDS, STACK), README, CHANGELOG, TODO, MEMORY (129), CONTEXT.

### TESTS WRITTEN
- `editors/vscode/test/`: diagnostics (9), targets (7), store (4), runner (8), spec keywords (1), grammar (`basics.la`, `strings.la`).

### DECISIONS MADE
- MEMORY 129. `@vscode/vsce` 4 instead of 3 (an `npm audit` advisory in 3's tree); `store.ts` added as a pure module.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- The user's manual check in a real VS Code window (editors/vscode/README.md), and confirming `GPL-3.0-only` in `package.json` against `LICENSE`.

---

## SESSION 141 — 2026-10-15 — Record the VS Code extension's manual check — closed
Branch: main

### WHAT WAS DONE
- The user installed the extension and passed every step of the manual check in VS Code; they confirmed `GPL-3.0-only`.
- Recorded both in PRP-099; removed the build worktree `../lugha-prp-099` and its merged local branch (session 140's follow-up).

### FILES CREATED OR MODIFIED
- Modified: `PRPs/prp-099-vscode_extension.md`, CONTEXT.md.

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing.

---

## SESSION 142 — 2026-10-15 — `lughac init` (milestone 69) — closed
Branch: prp-098-init

### WHAT WAS DONE
- The user approved PRP-098 ("approve PRP-098 lughac init, build it"); MEMORY 130.
- Built in a git worktree (`../lugha-prp-098`), as session 140 did, so another session's checkout of `main` is never switched; merged through a GitHub PR.
- Tests first: `tests/init.rs` (9: both layouts created, checked and run; current folder and `.`; `--name`; an existing manifest; bad names with hints; keywords and `std`; kept files; an I/O failure), then `driver/init.rs` (3 unit tests).
- Spec v16 at milestone 69 (§9 CLI table and an `init` section, §11 row 69, the changes summary); draft status; docs.

### FILES CREATED OR MODIFIED
- New: `src/driver/init.rs`, `tests/init.rs`.
- Modified: `src/driver/mod.rs`; the v16 spec and draft; CLAUDE.md, README, TODO, CHANGELOG, MEMORY (130, 131), PRP-098, CONTEXT.

### TESTS WRITTEN
- `tests/init.rs` (9); `driver::init` unit tests (3).

### DECISIONS MADE
- MEMORY 131: the `.gitignore` drops the PRP's `/target` (lughac writes no such folder); tests in `tests/init.rs`, not `tests/cli.rs`.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing; merged and tagged `m69`.

---

## SESSION 143 — 2026-10-15 — Sync MEMORY's start point after milestone 69 — closed
Branch: main

### WHAT WAS DONE
- My session's `git checkout -b prp-098-init` failed because another session had that branch in a worktree (`../lugha-prp-098`). The rest of the command ran on `main` and pushed `88ae778` (PRP-098's approval status and a MEMORY plan entry). The other session then built and merged PRP-098 on top of it (sessions 142, tag `m69`) and renumbered MEMORY (129 VS Code, 130–131 `init`). Nothing to revert.
- Checked `lughac init` (a program, a library with its example, a bad name, an existing manifest) with the installed compiler.
- Synced `MEMORY.md`'s start point with `CONTEXT.md`'s (it still said milestone 69 was proposed).

### FILES CREATED OR MODIFIED
- Modified: MEMORY.md (start point), CONTEXT.md.

### TESTS WRITTEN
- None.

### DECISIONS MADE
- None.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing.

---

## SESSION 144 — 2026-10-15 — Discovery: a language server for the VS Code extension — closed
Branch: main

### WHAT WAS DONE
- The user wants work to centre on the VS Code extension from now on, and chose a language server as its next step.
- Discovery: three PRPs (live diagnostics, then hover, then go-to-definition); `lsp-server` + `lsp-types`; `vscode-languageclient` replacing the on-save runner, no fallback; modules checked through entries and mains already seen; panics caught per check.
- Found: `modules::load` already takes a `Read`, so unsaved text needs no loader change; real programs check in ~30–50 ms (debug).
- Added v16 draft decisions 13–17 (milestone 70); wrote PRP-100 (proposed).

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-100-lsp_diagnostics.md`.
- Modified: `docs/specs/Language v16 Draft.md`, CONTEXT.md.

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: PRP-100 waits for approval.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Waiting for the user's approval of PRP-100, including their review of the new crates.

---

## SESSION 145 — 2026-10-15 — `lughac lsp` live diagnostics (milestone 70) — closed
Branch: prp-100-lsp_diagnostics

### WHAT WAS DONE
- The user approved PRP-100 and its crates ("build it"), then "finish it and merge"; MEMORY 132.
- Built in a git worktree (`../lugha-prp-100`), merged through a GitHub PR.
- Tests first: `tests/lsp.rs` (14, a real server over stdio), then `src/driver/lsp/` with unit tests; `pipeline::front_with` reads open documents and never writes a lock.
- Extension 0.2.0 on `vscode-languageclient`; the on-save runner, store, diagnostics and targets modules and their tests removed.
- Spec v16 at milestone 70 (§9 CLI table and `lughac lsp`, §11 row 70, summary); draft status; docs; CI no longer passes `LUGHAC`.

### FILES CREATED OR MODIFIED
- New: `src/driver/lsp/` (6 files), `tests/lsp.rs`, `editors/vscode/src/server.ts`, `editors/vscode/test/server.test.ts`.
- Deleted: `editors/vscode/src/{runner,store,diagnostics,targets}.ts`, their tests, `test/fixtures/`.
- Modified: `Cargo.toml` (+ `Cargo.lock` by cargo), `src/driver/{mod,pipeline,guard}.rs`, the extension's package files, tsconfig, README and extension.ts; CI; the v16 spec and draft; CLAUDE.md, README, TODO, CHANGELOG, MEMORY (132, 133), PRP-100, CONTEXT.

### TESTS WRITTEN
- `tests/lsp.rs` (14); `driver::lsp` unit tests (17); `server.test.ts` (3).

### DECISIONS MADE
- MEMORY 133: malformed messages end the server (exit 2); E0325 in `lugha.toml`; the extension unbundled for now.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- The user's manual check of live diagnostics in VS Code (extension README).

---

## SESSION 146 — 2026-10-15 — Discovery: hover types in the language server — closed
Branch: main

### WHAT WAS DONE
- Discovery for hover: expressions and declared names (bindings, parameters, functions and methods, fields); working while the program has errors, from the checker's partial results; a Lugha code block with `instance:` / `converted to` notes.
- Found: the checker types expressions by `ExprId` and written types by span, carries on after errors but `check_modules` drops its tables then; every local goes through `env.rs::declare`; `tests/lsp.rs` is 484 lines, so hover's tests need the harness moved to `tests/common/`.
- Added v16 draft decisions 18–22 (milestone 71); wrote PRP-101 (proposed).
- Fixed CLAUDE.md's STACK line (milestones 67–70), left stale by session 145.

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-101-lsp_hover.md`.
- Modified: `docs/specs/Language v16 Draft.md`, CLAUDE.md, CONTEXT.md.

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: PRP-101 waits for approval.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Waiting for the user's approval of PRP-101; the user's manual check of extension 0.2.0 is still open from session 145.

---

## SESSION 147 — 2026-10-15 — Hover in the language server (milestone 71) — closed
Branch: prp-101-lsp_hover

### WHAT WAS DONE
- The user approved PRP-101 ("build it"); MEMORY 134. Built in a worktree (`../lugha-prp-101`), merged through a PR.
- Tests first: `check::partial` (6), then the checker's `Recorder` and `check_partial` (`check_modules` shares `modules::run`, output unchanged); the stdio harness moved to `tests/common/lsp_client.rs`; `tests/lsp_hover.rs` (10), then `pipeline::front_partial`, `driver/lsp/hover.rs`, hover in the server.
- Spec v16 at milestone 71 (hover in the `lughac lsp` paragraph, §11 row 71, summary); docs.

### FILES CREATED OR MODIFIED
- New: `src/check/partial.rs`, `src/driver/lsp/hover.rs`, `tests/lsp_hover.rs`, `tests/common/lsp_client.rs`.
- Modified: `src/check/{mod,env,methods,call,method_calls,fun_values,structs,test_util}.rs`, `src/check/modules/mod.rs`, `src/driver/pipeline.rs`, `src/driver/lsp/{mod,server,check}.rs`, `tests/lsp.rs` (harness moved out); the v16 spec and draft; CLAUDE.md, README, the extension README, TODO, CHANGELOG, MEMORY (134, 135), PRP-101, CONTEXT.

### TESTS WRITTEN
- `check::partial` (6), `driver::lsp::hover` (1), `tests/lsp_hover.rs` (10).

### DECISIONS MADE
- MEMORY 135: no hover anywhere while a module doesn't parse; names map uses to declaration spans (reusable for go-to-definition); interface methods fall back to the call's type.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- The user's manual checks in VS Code: extension 0.2.0 (live errors) and hover (extension README steps 2–5).

---

## SESSION 148 — 2026-10-15 — Discovery: go-to-definition in the language server — closed
Branch: main

### WHAT WAS DONE
- Discovery for go-to-definition: jumps from variables, functions and methods, types, fields, variants and modules; nothing for `std`/prelude definitions; an interface call goes to the interface's method.
- Found: declaration spans exist for locals, structs, enums, interfaces, functions and methods, not for fields or variants; hover's `names` already maps function uses to declarations.
- Added v16 draft decisions 23–27 (milestone 72); wrote PRP-102 (proposed).

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-102-lsp_definition.md`.
- Modified: `docs/specs/Language v16 Draft.md`, CONTEXT.md.

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: PRP-102 waits for approval.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Waiting for the user's approval of PRP-102; the user's manual checks of live errors and hover are still open.

---

## SESSION 149 — 2026-10-15 — Go to definition in the language server (milestone 72) — closed
Branch: prp-102-lsp_definition

### WHAT WAS DONE
- The user approved PRP-102 ("build it"); MEMORY 136. Built in a worktree (`../lugha-prp-102`), merged through a PR.
- Tests first: `check::partial` definitions tests, then the `Target` table and its recording points; `tests/lsp_definition.rs` (8), then `driver/lsp/definition.rs` and the request in the server.
- Spec v16 at milestone 72 (go-to-definition in the `lughac lsp` paragraph, §11 row 72, summary); docs.

### FILES CREATED OR MODIFIED
- New: `src/driver/lsp/definition.rs`, `tests/lsp_definition.rs`.
- Modified: `src/check/{partial,mod,expr,iface_values,methods,struct_literal,access,construct,patterns,structs,enums,interfaces}.rs`, `src/check/modules/{mod,lookup}.rs`, `src/driver/lsp/{mod,server,hover}.rs`, `tests/common/lsp_client.rs`; the v16 spec and draft; CLAUDE.md, README, the extension README, TODO, CHANGELOG, MEMORY (136, 137), PRP-102, CONTEXT.

### TESTS WRITTEN
- `check::partial` (3 more), `driver::lsp::definition` (1), `tests/lsp_definition.rs` (8).

### DECISIONS MADE
- MEMORY 137.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- The user's manual checks in VS Code: live errors, hover and go-to-definition (extension README steps 2–6).

---

## SESSION 150 — 2026-10-15 — CI once per PR, never waited on — closed
Branch: main

### WHAT WAS DONE
- Merged PR #5 (milestone 72) and tagged `m72`, finishing session 149's work.
- The user asked for CI to run once per PR and be optional, choosing "runs on PRs, never waited on".
- `ci.yml`: `pull_request` and `workflow_dispatch` only (no `push`), with `concurrency` cancelling an older run of the same ref. README and CLAUDE.md (tree, the merge gate under COMMANDS) say so; MEMORY 138.

### FILES CREATED OR MODIFIED
- Modified: `.github/workflows/ci.yml`, README.md, CLAUDE.md, MEMORY.md, CONTEXT.md.

### TESTS WRITTEN
- None (the workflow file was checked to parse).

### DECISIONS MADE
- MEMORY 138.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- The user's manual checks in VS Code: live errors, hover and go-to-definition.

---

## SESSION 151 — 2026-10-15 — Discovery: completion in the language server — closed
Branch: main

### WHAT WAS DONE
- The user reported that autocompletion doesn't work: it was never built (out of PRP-100–102's scope).
- Discovery: completion after `.` and for plain names, with details, no snippets; mid-typing, types from the last check that parsed, trusted while the text before the receiver is unchanged; no parser change.
- Found: the parser returns no tree on any error; the server deletes a file's snapshot when a check doesn't parse; `Partial` lacks the member tables (fields, methods, variants).
- Added v16 draft decisions 28–32 (milestone 73); wrote PRP-103 (proposed).

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-103-lsp_completion.md`.
- Modified: `docs/specs/Language v16 Draft.md`, CONTEXT.md.

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: PRP-103 waits for approval.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Waiting for the user's approval of PRP-103; the manual checks of live errors, hover and go-to-definition are still open.

---

## SESSION 152 — 2026-10-15 — Completion in the language server (milestone 73) — closed
Branch: prp-103-lsp_completion

### WHAT WAS DONE
- The user approved PRP-103 ("build it"); MEMORY 139. Built in a worktree (`../lugha-prp-103`), merged through a PR without waiting for CI (session 150).
- Tests first: `check::members` (6), then the member tables; `tests/lsp_completion.rs` (10), then `driver/lsp/completion.rs` (4 unit tests), `last_good` in the server, the request and capability.
- Spec v16 at milestone 73 (completion in the `lughac lsp` paragraph, §11 row 73, summary); docs.

### FILES CREATED OR MODIFIED
- New: `src/check/members.rs`, `src/driver/lsp/completion.rs`, `tests/lsp_completion.rs`.
- Modified: `src/check/{mod,partial,helpers,iter_helpers}.rs`, `src/driver/lsp/{mod,server,hover}.rs`, `tests/common/lsp_client.rs`; the v16 spec and draft; CLAUDE.md, README, the extension README, TODO, CHANGELOG, MEMORY (139, 140), PRP-103, CONTEXT.

### TESTS WRITTEN
- `check::members` (6), `driver::lsp::completion` (4), `tests/lsp_completion.rs` (10).

### DECISIONS MADE
- MEMORY 140: alignment by the shared prefix, with a by-name lookup for a receiver past it.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- The user's manual checks in VS Code: live errors, hover, go-to-definition and completion (extension README steps 2–7).

---

## SESSION 153 — 2026-10-15 — Record the language server's manual checks; completion settings note — closed
Branch: main

### WHAT WAS DONE
- The user reported completion not working: their VS Code user settings had `editor.quickSuggestions` off and `editor.suggestOnTriggerCharacters` false. With their approval, added a `[lugha]` block to `~/.config/Code/User/settings.json` turning both on for Lugha only; the user confirmed it works.
- The user confirmed the manual checks of live errors, hover, go-to-definition and completion: marked in PRP-100 to PRP-103.
- Extension README: a "No suggestions popping up?" section with that block.
- CI on PR #6 (milestone 73) came back green after the merge.

### FILES CREATED OR MODIFIED
- Modified: `PRPs/prp-100-lsp_diagnostics.md`, `prp-101-lsp_hover.md`, `prp-102-lsp_definition.md`, `prp-103-lsp_completion.md`, `editors/vscode/README.md`, CONTEXT.md. Outside the repo: the user's VS Code `settings.json`.

### TESTS WRITTEN
- None (docs).

### DECISIONS MADE
- None.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing.

---

## SESSION 154 — 2026-10-15 — Discovery: signature help in the language server — closed
Branch: main

### WHAT WAS DONE
- Explained partial syntax trees and signature help; the user chose signature help first.
- Discovery: every callable (functions, module functions, methods, associated functions, variants, built-ins and helpers, intrinsics); generic functions filled in when the last good check knows the instance, as written otherwise.
- Found: `Partial` lacks resolved parameter types (needed to fill in generics); intrinsics have no declarations, but spec §5's table has their call forms.
- Added v16 draft decisions 33–37 (milestone 74); wrote PRP-104 (proposed).

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-104-lsp_signature_help.md`.
- Modified: `docs/specs/Language v16 Draft.md`, CONTEXT.md.

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: PRP-104 waits for approval.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Waiting for the user's approval of PRP-104.

---

## SESSION 155 — 2026-10-15 — Signature help in the language server (milestone 74) — closed
Branch: prp-104-lsp_signature_help

### WHAT WAS DONE
- The user approved PRP-104 ("build it"); MEMORY 141. Built in a worktree (`../lugha-prp-104`), merged through a PR without waiting for CI.
- Tests first: `check::signatures` (3), then `Sig` and its recording; `tests/lsp_signature.rs` (9), then `driver/lsp/signature.rs` (4 unit tests), the shared `Here::receiver`, the request and capability.
- Spec v16 at milestone 74 (signature help in the `lughac lsp` paragraph, §11 row 74, summary); docs.

### FILES CREATED OR MODIFIED
- New: `src/check/signatures.rs`, `src/driver/lsp/signature.rs`, `tests/lsp_signature.rs`.
- Modified: `src/check/{mod,partial,env,methods}.rs`, `src/driver/lsp/{mod,server,completion}.rs`, `tests/common/lsp_client.rs`; the v16 spec and draft; CLAUDE.md, README, the extension README, TODO, CHANGELOG, MEMORY (141, 142), PRP-104, CONTEXT.

### TESTS WRITTEN
- `check::signatures` (3), `driver::lsp::signature` (4), `tests/lsp_signature.rs` (9).

### DECISIONS MADE
- MEMORY 142.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- The user's manual check of signature help in VS Code (extension README step 8).

---

## SESSION 156 — 2026-10-15 — Discovery: partial syntax trees — closed
Branch: main

### WHAT WAS DONE
- Discovery for partial syntax trees: statement- and item-level (what the parser already recovers), no expression error nodes or lexer recovery; while a file has a syntax error, only syntax errors are shown.
- Found: `parse_from` builds the recovered `(imports, items)` and discards it on any error; `parse_module` then stores `program: None`, so imports of a broken module aren't followed.
- Added v16 draft decisions 38–41 (milestone 75); wrote PRP-105 (proposed).

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-105-partial_trees.md`.
- Modified: `docs/specs/Language v16 Draft.md`, CONTEXT.md.

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: PRP-105 waits for approval.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Waiting for the user's approval of PRP-105; the manual check of signature help.

---

## SESSION 157 — 2026-10-15 — Partial syntax trees for the language server (milestone 75) — closed
Branch: prp-105-partial_trees

### WHAT WAS DONE
- The user approved PRP-105 ("build it"); MEMORY 143. Built in a worktree (`../lugha-prp-105`), merged through a PR without waiting for CI.
- Tests first for the parser (5) and the loader (1); then `parse_partial_from`, `LoadEnv.partial`, `front_partial`, the server's diagnostics rule and snapshot order; `tests/lsp_partial.rs` (5) after.
- Found and fixed the prelude overlapping manifests' offsets in the server (`with_prelude_at`); kept milestone 74's filled-in signatures with `Snapshot.broken`.
- Spec v16 at milestone 75 (§3 recovery, §9 `lughac lsp` paragraph, §11 row 75, summary); docs.

### FILES CREATED OR MODIFIED
- New: `tests/lsp_partial.rs`.
- Modified: `src/parser/mod.rs`, `src/modules/{mod,parse,layout,tests}.rs`, `src/driver/{pipeline,update}.rs`, `src/driver/lsp/{check,server,hover}.rs`, `tests/lsp_completion.rs`; the v16 spec and draft; CLAUDE.md, README, the extension README, TODO, CHANGELOG, MEMORY (143, 144), PRP-105, CONTEXT.

### TESTS WRITTEN
- `parser::partial_tests` (5), `modules::tests` (1), `tests/lsp_partial.rs` (5), `tests/lsp_completion.rs` (1 rewritten, 1 new).

### DECISIONS MADE
- MEMORY 144.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- The user's manual checks in VS Code of signature help and of editing with a syntax error (extension README steps 8–9).

---

## SESSION 158 — 2026-10-16 — Discovery: block comments — closed
Branch: main

### WHAT WAS DONE
- The user asked for JSDoc-style docs (`/** … */`), then for block comments in the compiler first and the doc comments after: two PRPs.
- Discovery for block comments: they nest; comments wherever whitespace may be; E0113 for an unclosed one; inside an interpolation they stay on one line (E0111).
- Added v16 draft decisions 42–46 (milestone 76); wrote PRP-106 (proposed). Doc comments (milestone 77) get their own discovery after.

### FILES CREATED OR MODIFIED
- New: `PRPs/prp-106-block_comments.md`.
- Modified: `docs/specs/Language v16 Draft.md`, CONTEXT.md.

### TESTS WRITTEN
- None (no code).

### DECISIONS MADE
- None final: PRP-106 waits for approval.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Waiting for the user's approval of PRP-106; then the discovery for `/** … */` doc comments.

---

## NEXT SESSION START POINT

**v16: milestones 67–69 complete** (tags `m67` shared boxes, `m68` changing directory, `m69` `lughac init`).

**Focus: the VS Code extension** (the user, session 144). **Milestone 70 done** (tag `m70`): `lughac lsp` with live diagnostics, extension 0.2.0. **Milestone 71 done** (tag `m71`): hover. **Milestone 72 done** (tag `m72`): go-to-definition. **Milestone 73 done** (tag `m73`): completion; the user's manual checks of milestones 70–73 all passed (session 153). **Milestone 74 done** (tag `m74`): signature help. **Milestone 75 done** (tag `m75`): partial syntax trees. **Proposed: milestone 76, block comments** — v16 draft decisions 42–46 and PRP-106; it waits for the user's approval. Then milestone 77, `/** … */` doc comments in the language server (discovery first). Later for the extension: expression error nodes, lexer recovery, snippets, bundling. Later for the extension (each a discovery + PRP): partial syntax trees, signature help and snippets, Find References, Go to Implementations, a read-only view of bundled sources, bundling; the user's manual checks of live errors and hover. Build in a git worktree and merge through a PR: another session has shared this checkout.

Candidates (TODO's v16 section): streaming pipes and timeouts for `std.process`, a package entry module (`import glowworm;`), editing a box in place, a byte buffer, file streams; for editors, a language server.

**VS Code extension done** (PRP-099, `editors/vscode/`, session 140; manually checked by the user and `GPL-3.0-only` confirmed, session 141). A language server and Marketplace publishing are later TODO items.

Unpushed local commits, for the user to push when they ask: firefly `ce6f38b`, `a891512`; glowworm `b0683c3`. oyster is uncommitted in the user's repo.

See `TODO.md`.

### PENDING DECISIONS OPENED
- None.

### STILL OPEN AT CLOSE
- Nothing.
