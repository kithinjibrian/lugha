## FEATURE: `lughac spec` prints the bundled language specification, every §10 program is tested straight from the spec, and the dead milestone stops are removed — completing v0.

**Status:** approved 2026-10-09 — session 19
**Milestone:** 5, last of four PRPs. Done-when: every program in spec §10 passes, as extracted from the spec itself. Then tag `m5`.
**Spec:** §9 (command-line interface, LLM-ready spec, JSON diagnostics line), §10 (example programs and their expected output), §11 (milestone 5 and the milestone 2 program)
**Decisions:** DECISION-008 / MEMORY 12 (acceptance tests), MEMORY 13 (diagnostic rendering), PRP-015 amendments (remove the `Unsupported` paths in this PRP)

## OBJECTIVE
`lughac spec` prints the language specification matching the installed compiler, so a model or harness can be given Lugha's rules in one command. The tests read the §10 programs and their expected results out of that same embedded text, so the spec, the printed copy and the acceptance tests can't drift apart. Every §10 program passes, and v0 is complete.

## CONTEXT

- Starting state:
  - No `spec` subcommand.
  - `tests/common/spec_programs.rs` hard-codes the §10/§11 programs; the lexer and parser tests use them.
  - Copies live under `tests/programs/m3`–`m5` (hello, recursion, libc, primes, centroid, e0401).
  - Nothing produces `CheckError::Unsupported` or `CodegenError::Unsupported`, and the checker's `Stop` is uninhabited (PRP-015).
- Ending state:
  - `lugha::SPEC` embeds `docs/specs/Language v0 Specification.md`; `lughac spec` prints it.
  - `spec_programs.rs` extracts the programs from `SPEC`.
  - A new `tests/spec.rs` runs every §10 program against the outputs stated in the spec's own sentences.
  - `check::check` returns the CLAUDE.md stage shape, `Result<(Checked, Vec<Diagnostic>), Vec<Diagnostic>>`. The `Unsupported` variants and `Stop` are gone.
- Related existing code: `src/lib.rs`, `src/driver/{mod,pipeline}.rs`, `src/check/mod.rs`, `src/codegen/mod.rs`, `tests/common/spec_programs.rs`, `tests/{lexer,parser,cli}.rs`, `tests/support/`.
- Open decisions that must be resolved first: none.

### Discovery answers (session 19)
1. `lughac spec` prints the **spec verbatim**: the Markdown file embedded with `include_str!`, unchanged, on stdout. It is one source of truth and about 12k tokens.
2. The tests **extract §10 from the spec**: each fenced block is keyed by its bold title, and its expected stdout and exit code are parsed from its sentence ("Prints `25`, exits with 0."). Editing the spec changes what is tested.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Embedding** — `src/lib.rs`
- `pub const SPEC: &str = include_str!("../docs/specs/Language v0 Specification.md");` with a doc comment saying it is what `lughac spec` prints.

**CLI** — `src/driver/mod.rs` (spec §9)
- A `spec` subcommand with no arguments or flags. It writes `SPEC` to stdout exactly (no added newline; the file ends with one) and exits 0.
- Extra arguments are clap usage errors (exit 2), as for other commands.
- A failed write (e.g. a closed pipe, as in `lughac spec | head`) is not a compiler error: it ends quietly with exit 0, like other Unix tools. The `//` comment says why.

**Spec extraction** — `tests/common/spec_programs.rs`
- `section_10() -> Vec<SpecProgram>` parses the `## 10.` section of `lugha::SPEC`. For each `**Title** (milestone N). …` paragraph followed by a fenced block it records:
  - `title`, `source` (the block's text);
  - `stdout`: every backtick span between `Prints` and `, exits with`, joined with newlines, plus a final newline (§10: "Every expected output ends with a newline");
  - `exit`: the number after `exits with`.
- The rejected program, which has no "Prints", has `exit: 1`, and its second fenced block is its expected `stderr`.
- `milestone_2()` returns the §11 block after "The milestone 2 test program", with exit 55 from the §11 table (`exits with 55`, parsed).
- Extraction panics with a clear message if the spec's shape changes (a test-only invariant).
- The lexer and parser tests switch to the extracted programs. The hard-coded constants are removed.

**Acceptance** — new `tests/spec.rs`
- Every §10 program with a "Prints" sentence is built and run at `-O0` and `-O2`; stdout and the exit code must match exactly.
- The rejected program is written to a temp dir as `main.la` (the name its expected output uses). `lughac check main.la`:
  - exits 1;
  - its human stderr equals the spec's second block plus the blank line the renderer adds (the comparison trims only that);
  - its `--diagnostics=json` stderr equals the §9 JSON line exactly.
- The milestone 2 program exits 55.
- `lughac spec` prints `lugha::SPEC` byte-for-byte and exits 0, and `lughac spec extra` exits 2 (in `tests/cli.rs`).
- At least five §10 programs and the rejected one are found, so a silently broken extractor can't pass with zero cases.

**Removing the dead stops** (PRP-015 amendment)
- `check::check` returns `Result<(Checked, Vec<Diagnostic>), Vec<Diagnostic>>`. `CheckError`, `Stop` and `Checking<T>` go; checker functions return plain values. Mechanical: `Checking<T>` → `T` and `?` removed.
- `CodegenError::Unsupported` goes; the driver's handling of both variants goes.
- If the `Checking` removal makes the diff unreviewably large, it may land as its own commit inside this PRP.

**Docs**
- §9 is unchanged: its `lughac spec` row and the LLM-ready paragraph already describe this. §11 needs no change.
- CLAUDE.md: add `tests/spec.rs` to the tree, and update the stage-result wording for `check` if it differs.
- MEMORY, CHANGELOG ("milestone 5 complete — v0 complete"), TODO and CONTEXT updated. Tag `m5` after merge.

### Must NOT Do
- No separate or condensed spec file, and no section filtering.
- No pager, no colour, no `--section` flag.
- No language changes; any spec edit needs separate approval (rule 1).
- No new dependencies, no `unsafe`.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- `lughac spec` can only fail writing to stdout. A broken pipe ends the command with exit 0, and other write errors are internal errors (exit 2).
- The extraction helpers are test code; a malformed spec is a test failure with a message naming what was missing.

## SECURITY CONSIDERATIONS

- `SPEC` is compile-time text; the command reads no input and writes only to stdout.
- No `unsafe`.

## TESTS TO WRITE

- [ ] `tests/cli.rs`: `lughac spec` output equals `lugha::SPEC` and exits 0; `lughac spec extra` exits 2.
- [ ] `tests/spec.rs`:
  - every extracted §10 program's stdout and exit at `-O0` and `-O2`;
  - the rejected program's human and JSON diagnostics;
  - the milestone 2 program exits 55;
  - the extractor finds hello, recursion, primes, centroid, calling C and the rejected program.
- [ ] The lexer and parser tests still pass on the extracted programs.
- [ ] The checker and driver tests are updated for the new `check` return type; all earlier tests pass.

## ROLLBACK PLAN

- Branch `prp-016-spec_command`, merged into `main` on acceptance, then tag `m5`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] `lughac spec` prints the bundled spec.
- [ ] Every §10 program passes as extracted from the spec.
- [ ] No `Unsupported` paths remain.
- [ ] CLAUDE.md, CHANGELOG.md, TODO.md and MEMORY.md updated.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo run -q -- spec | head -1` → `# Lugha v0 Specification`
- `grep -rn "Unsupported\|unsafe" src/` → nothing
