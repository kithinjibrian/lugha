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

## SESSION 53 — 2026-10-13 — Results and ? (milestone 17) — open

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

---

## NEXT SESSION START POINT

**Lugha v3 is complete** (tag `v3`). **The v4 draft is approved** (session 52, MEMORY 49, `docs/specs/Language v4 Draft.md`): input and output.

Next is **milestone 17, `Result` and `?`**. It covers:
- the predeclared `enum Result<T, E> { Ok(T), Err(E) }`, with `Ok`/`Err` without a prefix, inferred from the expected type (E0412 alone);
- the postfix `?` on `Result` (the same `E`) and `Option`, returning early from the enclosing function or closure, with E0424 for misuse;
- the "Results and `?`" program in the draft's §10.

This milestone starts `Language v4 Specification.md` (as v3 did at M13). Start with discovery for PRP-042. A likely question is whether `Result` is a real generic enum declared in the prelude, or built in like `Option`.

See `TODO.md`.
