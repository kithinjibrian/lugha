## FEATURE: tests in the editor — `lughac test --list`/`--json`/`--exact`, and VS Code's Testing view with a ▶ beside each `@test`; milestone 85.

**Status:** proposed 2026-10-16 — session 180 (discovery the same session); waits for approval and the user's security review (the extension starts a second `lughac` command, which runs the user's code).
**Milestone:** 85 (one PRP; tag `m85` at merge).
**Spec:** `Language v16 Draft.md` decisions 87–92; spec §9 (the `lughac test` section's new flags and JSON), §11 (row 85).

**Decisions (discovery, session 180):**
- VS Code's own Testing view and gutter icons, not "Run test" links in a terminal;
- tests run only when asked (plus VS Code's continuous run if the user turns it on); never in an untrusted folder;
- the extension starts `lughac` without a shell, only the `lugha.path` executable.
- No open entries in DECISIONS.md apply.

## OBJECTIVE
```
  ▶ │ @test                                  TESTING
    │ fun area_of_unit() {                     ▾ geo.la
  ✗ │ @test                                      ✓ area_of_unit
    │ fun doubles() {                            ✗ doubles
    │     assert_eq(double(2), 5);
    │     ~~~~ assert_eq failed: left 4, right 5
```

## CONTEXT
- **Starting state:** `lughac test [paths…] [--filter TEXT] [--timeout SECS]` (milestone 84, `driver/test.rs`) prints human lines; the extension (`editors/vscode/src/extension.ts`, 0.7.0) only starts `lughac lsp` through `vscode-languageclient`; VS Code's Testing API (`vscode.tests.createTestController`, test items with ranges, run profiles, `TestRun.passed/failed/appendOutput`) is in `@types/vscode` 1.91.
- **Ending state:**
  - **CLI (`driver/test.rs`, a new `driver/test_json.rs` if it grows past the limit):**
    - `--list`: every test under the paths — lex and parse only, no build — as `test <name> <file>:<line>:<col>` lines, or with `--json` `{"test":"geo.area_of_unit","file":"geo.la","line":4,"col":5}` objects (the name's position; files as given/walked);
    - `--json` on a run: per test as it finishes `{"test":…,"file":…,"line":…,"status":"passed"|"failed"|"timed_out","message":…,"at":{"file":…,"line":…,"col":…},"output":…,"ms":…}` (`message` and `at` from the panic line `panic: <message> at <file>:<line>:<col>`; absent for a pass), then `{"summary":{"passed":P,"failed":F}}`; diagnostics stay on stderr as now (`--diagnostics=json` is not part of this);
    - `--filter` repeatable (any match runs), `--exact` (a filter must equal the whole name);
    - the human output and exit codes unchanged.
  - **Extension 0.8.0:**
    - `src/tests.ts` (pure, no `vscode`): parse `--list`/`--json` lines; build the argument lists; turn a result into what the controller needs (status, message, location, output). Unit-tested with `node:test`.
    - `src/testing.ts` (imports `vscode`, as `extension.ts` does): a `TestController` "Lugha"; items per file (children per test, with ranges from `--list`); discovery on activation and on `.la` save/create/delete (`FileSystemWatcher`), per workspace folder; a Run profile (default) running `lugha.path test --json --exact --filter … <files>` with `cwd` the workspace folder via `child_process.spawn` (no shell), streaming JSON lines into `TestRun` (passed/failed with `TestMessage` at `at`, `appendOutput` of each test's output); cancellation kills the process. Not created when `vscode.workspace.isTrusted` is false (so untrusted folders list nothing either); created on `onDidGrantWorkspaceTrust`.
    - `package.json`: `capabilities.untrustedWorkspaces` description updated (tests don't run there); no new settings; no new dependency.
    - README: "Tests" (the Testing view, the gutter, trust), manual steps.
  - **Spec v16:** §9's `lughac test` section gains `--list`, `--json`, `--exact`, repeatable `--filter`; §9's editor text mentions the Testing view; §11 row 85; the changes summary.
  - **Docs:** CLAUDE.md (tree, STACK note that the extension also starts `lughac test`), README, TODO, CHANGELOG, MEMORY, CONTEXT.
- **Related existing code:** `src/driver/test.rs`, `src/driver/mod.rs`, `src/driver/fmt.rs` (the walk), `editors/vscode/src/{extension,server}.ts`, `editors/vscode/test/*.test.ts`, `tests/test_runner.rs`.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The ending state above, test-first (Rust for the CLI, `node:test` for `tests.ts`).

### Must NOT Do
- No change to `lughac lsp` (it still runs nothing), no new LSP requests.
- No debugging of tests, no coverage profile, no run-on-save default.
- No running in untrusted workspaces, no shell, no executable other than `lugha.path`.
- No new dependency in the crate or the extension.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- `lughac test` failing to start (missing `lughac`, too old for `--json`) → one error message naming `lugha.path` and milestone 85; tests stay listed.
- A file that doesn't compile: its diagnostics are already shown by the language server; the run marks that file's tests errored ("doesn't compile; see Problems").
- Unparsable output lines are logged to the Lugha output channel and ignored.

## SECURITY CONSIDERATIONS
- For the user's review (CLAUDE.md: changes to how the extension starts `lughac` need it): the extension now also runs `lughac test`, which builds and runs the workspace's code. Mitigations: only on an explicit request; never in an untrusted workspace; only the `lugha.path` executable (machine-scoped, so a workspace can't set it); `spawn` with an argument array, no shell; arguments are fixed flags plus test names and file paths that the extension got from `lughac test --list`; `lughac test` itself starts test executables directly from a private temp folder (milestone 84). Listing (`--list`) only lexes and parses, but even it isn't started in an untrusted folder: there the extension starts nothing at all, so the Testing view stays empty until the folder is trusted.

## TESTS TO WRITE
- [ ] **`tests/test_runner.rs`:** `--list` (text and JSON, positions, no build: a file with a type error still lists its tests); `--json` results (passed, failed with message and `at`, timed out, output, summary); `--exact` and repeated `--filter`; the human output unchanged.
- [ ] **Unit tests (`driver/test.rs`):** splitting a panic line into message and location; JSON escaping of names, messages and output.
- [ ] **Extension (`test/tests.test.ts`):** parsing list and result lines (and rejecting garbage); arguments for one test, a file, everything; result → status/message/location mapping; `package.json` describes the untrusted-workspace behaviour.
- [ ] **Regression:** every existing test passes; `npm test`.

## ROLLBACK PLAN
- Branch `prp-115-editor_tests` from `main`; tag `m85`.
- To abandon before merge: delete the branch. After: revert the merge (extension 0.7.0 still works with the newer `lughac`).

## ACCEPTANCE CRITERIA
- [ ] All tests pass; fmt, clippy, `cargo test`, `npm test` (CI in the background).
- [ ] Spec at milestone 85; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] The manual check passes in VS Code (README steps): the Testing view lists the tests, ▶ runs one, a failure shows at its line, Restricted Mode runs nothing.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo test --test test_runner
cd editors/vscode && npm test
```
