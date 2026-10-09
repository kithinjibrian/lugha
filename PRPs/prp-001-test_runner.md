## FEATURE: A test runner that compiles and runs every `.la` program under `tests/programs/` and compares its output with expectation files.

**Status:** implemented 2026-10-09 — session 4 (branch `prp-001-test_runner`)
**Milestone:** pre-milestone 1 (infrastructure)
**Spec:** §9 (exit codes, CLI), §10 (acceptance programs), §11 (milestone 1 done-when)
**Decisions:** DECISION-008 (resolved)

## OBJECTIVE
Adding an acceptance test means adding files, never code. `cargo test` runs the runner's own unit tests and stays green before lughac can compile anything. `cargo test -- --ignored` runs every program end to end and reports every failure at once, with the program path and what differed.

## CONTEXT

- Starting state: `src/lib.rs` and `src/main.rs` hold only module docs; `lughac` does nothing. No `tests/` directory exists.
- Ending state: `tests/programs.rs`, `tests/support/{mod,fixture}.rs`, `tests/support/runner/{mod,discover,execute,report}.rs`, `tests/programs/m1/arith.{la,stdout,exit}` created (see Structure — amended during implementation).
- Related existing code: `Cargo.toml` (binary name `lughac`), `CLAUDE.md` TESTING RULE.
- Related source files: discovery interview, session 4 (answers recorded below).
- Open decisions that must be resolved first: none.

### Discovery answers (session 4)
1. Test shape: mode chosen by the presence of `.exit` (below).
2. Milestones: subfolders `m1/` … `m5/`; a milestone's programs are added when its work starts; no skip logic.
3. Reporting: one end-to-end test that collects every failure, then fails once.
4. Edge cases: strict; 10-second timeout per program.
5. Bootstrapping: runner logic unit-tested now; end-to-end test `#[ignore]`d until milestone 1.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Case discovery**
- Recursively find every `*.la` file under `tests/programs/`, sorted by path so output order is stable.
- For `<name>.la`, sibling files `<name>.stdout`, `<name>.stderr`, `<name>.exit` define the expectation.

**Two modes, chosen by `.exit`**
- **Run mode** — `<name>.exit` exists:
  - `.stdout` is required (an empty file means "no output").
  - `.stderr` is optional; when present, stderr must match it (panic tests).
  - Command: `lughac run <path>`.
  - Pass when stdout == `.stdout`, exit code == `.exit`, and (if present) stderr == `.stderr`.
- **Reject mode** — no `.exit`, `<name>.stderr` exists:
  - Command: `lughac check <path>`.
  - Pass when stderr == `.stderr` and lughac's exit code == 1.
  - A `.stdout` file in reject mode is malformed.

**Execution**
- `lughac` is the binary from `env!("CARGO_BIN_EXE_lughac")` — never an installed copy.
- Working directory: the repository root (`env!("CARGO_MANIFEST_DIR")`); the program path is passed relative to it, so diagnostics show `tests/programs/m1/arith.la`.
- stdin is null.
- Read stdout and stderr on separate threads while waiting, so a program that fills a pipe can't deadlock the runner.
- Kill the process after 10 seconds and report a timeout.

**Comparison**
- Byte-for-byte. No trimming, no newline normalisation.
- `.exit` contains a decimal integer 0–255, optionally followed by one newline.

**Malformed cases are failures, not skips**
- `.la` with neither `.exit` nor `.stderr`.
- `.exit` without `.stdout`.
- `.stdout` in reject mode.
- `.exit` that isn't an integer 0–255.
- A `.stdout`, `.stderr` or `.exit` file with no matching `.la`.
- Zero `.la` files found → failure "no programs found in tests/programs".

**Reporting**
- Collect every failure; at the end print `N of M programs failed:` followed by one block per failure:
  ```
  --- m1/arith.la: exit code
    expected: 14
    actual:   0
  ```
- Show stdout/stderr differences as escaped strings (`"55\n"`) so whitespace differences are visible. Truncate each side to 2,000 bytes with a `… (N more bytes)` marker.
- A program with several mismatches reports each one.

**Structure** (each file under the 300-line limit)

*Amended 2026-10-09, session 4, with user approval:* the original two-file plan (`runner.rs` + all unit tests in `programs.rs`) came to 351 and 310 lines after `cargo fmt`. Split by job, with each module's unit tests at its bottom per the CLAUDE.md testing rule:
- `tests/programs.rs` — only the end-to-end test `programs()`, marked `#[ignore = "enable in milestone 1 driver PRP"]`.
- `tests/support/mod.rs` — `pub mod fixture; pub mod runner;`
- `tests/support/fixture.rs` — temp-dir `Fixture` (removed on drop) and the `exited` outcome helper.
- `tests/support/runner/mod.rs` — shared types (`Mode`, `Case`, `Malformed`, `Outcome`, `Mismatch`), `DEFAULT_TIMEOUT`, re-exports.
- `tests/support/runner/discover.rs` — `discover(root) -> Result<Vec<Case>, Vec<Malformed>>`, exit-code parsing + tests.
- `tests/support/runner/execute.rs` — `run_case`, `run_command` (timeout, process-group kill) + tests.
- `tests/support/runner/report.rs` — `compare(&Case, &Outcome) -> Vec<Mismatch>`, `format_report`, `format_malformed` + tests.

**First program**
- `tests/programs/m1/arith.la` — `fun main(): i32 { 2 + 3 * 4 }` (spec §11 milestone 1).
- `arith.stdout` — empty. `arith.exit` — `14`.

### Must NOT Do
- Do not modify anything under `src/` — the runner tests lughac, it doesn't change it.
- Do not add dependencies (no `libtest-mimic`, `insta`, `tempfile`, `wait-timeout`). Std only; unit-test fixtures go in a directory under `std::env::temp_dir()` that the test creates and removes.
- Do not normalise output to make a test pass.
- Do not skip malformed cases.
- Do not add programs for milestones 2–5.

## ERROR HANDLING REQUIREMENTS

- Runner functions return `Result`. A failure inside one case (can't spawn lughac, can't read a file) becomes that case's failure in the report — it never aborts the other cases.
- `.expect()` is used only for invariants (e.g. `CARGO_BIN_EXE_lughac` is set by Cargo), with the invariant as the message.
- The only panic is the final `panic!` that fails the test with the formatted report.

## SECURITY CONSIDERATIONS

- The runner executes compiled programs from the repo's own `tests/programs/`. No external input, no network, no secrets.
- The timeout kills runaway programs; temp fixtures are removed even when a unit test fails (cleanup in a `Drop` guard).
- No `unsafe`.

## TESTS TO WRITE

Unit tests at the bottom of each `tests/support/runner/` module, against fixture directories:
- [x] Discovery finds `.la` files recursively and returns them sorted.
- [x] Run mode: `.exit` + `.stdout` → run-mode case; `.stderr` alongside is attached as expected stderr.
- [x] Reject mode: `.stderr` only → reject-mode case expecting exit 1.
- [x] Malformed: `.la` with no `.exit`/`.stderr`.
- [x] Malformed: `.exit` without `.stdout`.
- [x] Malformed: `.stdout` in reject mode.
- [x] Malformed: `.exit` of `abc`, `256`, `-1`.
- [x] Malformed: orphan `.stdout` with no `.la`.
- [x] Empty root → "no programs found".
- [x] Compare: identical outcome → no mismatches.
- [x] Compare: `"55\n"` vs `"55"` → stdout mismatch (no normalisation).
- [x] Compare: wrong exit code and wrong stdout → two mismatches.
- [x] Compare: timeout outcome → timeout mismatch.
- [x] Report: formats the header, escapes `\n`, truncates over 2,000 bytes.
- [x] Timeout: a child that sleeps past the limit is killed and reported (use `sleep 30` via `sh -c` with a shortened limit, e.g. 200 ms — the limit is a parameter, 10 s is only the default).

End-to-end (ignored until milestone 1):
- [x] `programs()` runs `m1/arith.la`; currently fails because lughac is a stub.

## ROLLBACK PLAN

- Work happens on branch `prp-001-test_runner`, merged into `main` when accepted.
- To abandon: delete the branch. Nothing outside `tests/` changes, no migrations.

## ACCEPTANCE CRITERIA
- [x] Every test in TESTS TO WRITE exists and passes (except the ignored end-to-end test).
- [x] `cargo test -- --ignored` runs `m1/arith.la` and prints a readable failure (exit 0 vs 14).
- [x] Adding a new program needs no code change.
- [x] No file over 300 lines; no new dependencies.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass.
- [x] Every `pub` item in `tests/support/` has a doc comment.
- [x] CLAUDE.md FILE ORGANIZATION updated with `tests/`.
- [x] CHANGELOG.md updated.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo test -- --ignored` — expect exactly one failure: `m1/arith.la: exit code, expected 14, actual 0`.
