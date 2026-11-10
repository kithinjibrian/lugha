## FEATURE: `lughac test` builds each package's test files into one executable instead of one per file; milestone 124 (v17).

**Status:** approved 2026-10-20 (session 295), built (session 296), held after measuring no gain (669 s vs 652 s, LLVM's slow instruction selector dominating), and re-measured after milestones 125–126 made codegen cheap (session 303, the user's choice): on kivuli's `test/` (69 files, 399 tests, same state, back to back) one build per file takes 275 s, one build per package folder 96 s — 2.9× faster, with identical results. Proposed 2026-10-20 (session 295)

## OBJECTIVE
```
$ lughac test test/          # kivuli: 46 files, one build instead of 46
...
612 tests: 612 passed, 0 failed
```
Same output, names, exit codes, `--list`/`--json` and isolation (one process per test) as today; only the number of builds changes.

## CONTEXT
- `src/driver/test.rs`: `build(file, exe)` per file → `pipeline::front_tests` (one root, loaded as a module when it can be) → `pipeline::build` at `-O0`; jobs carry `exe` per file; `run_all` runs tests in parallel.
- `src/modules/mod.rs` `load(main, …)`: one root; a package's file under `test`/`check` becomes its module through a hidden entry (v17 milestone 97), a loose library file the module named after it.
- `src/check` `check_tests(&[Unit])` already takes several units; `codegen/test_main.rs` builds the `main` that runs the test named by `LUGHA_TEST`.
- `src/driver/test_json.rs` `module_name`: a test's displayed name.
- Spec v17 §9 "`lughac test`" (line ~1674) and "A package's file as its module" (~1112). Open decisions: none.

## DECISIONS (draft decisions 276–284)
276. **Groups** (narrowed in session 296, the user's choice, from "by package"): test files are grouped by package and folder — files in the same folder governed by the same `lugha.toml` share one build. A file governed by no manifest is a group alone. Groups are built one after another, in the order of their first file. (The loader resolves a file's relative imports and lock from its folder; one folder per build keeps that unchanged.)
277. **Loading a group**: the loader takes several roots (a new `modules::load_all(roots, …)`; `load` is its one-root case, unchanged). With several roots, each is a module: a package's `source` file its package module as today, any other file the module named by its stem (`diff.la` → `diff`; a stem that isn't a name → a hidden `$test<n>`). So a root that another root imports (`import fixtures;`) is loaded once under one name, and roots' same-named items never clash. A module reached from several roots is loaded once.
278. **Checking**: the group is checked together first. When that has errors, each of its files is checked alone (as today) to find which ones fail; their diagnostics print as today, they count in "K files didn't compile", and the group is built from the files that checked. A file whose own check passes but which fails only with the others (a clash the combined check finds) is a compiler bug: exit 2 with the diagnostics.
279. **Running**: `LUGHA_TEST` names a test by its module and name (`diff.works`, `kivuli.diff.patches`) so files' same-named tests stay apart; the shown names (`test.diff.works`) are today's. Each test still runs as its own process with the same timeout, capture, panic location and exit status rules.
280. **Filters**: with `--filter`/`--exact`, every file is first parsed (as `--list` does, nothing checked) and only files holding a selected test join their group. A file with no `@test` is skipped as today.
281. **Constants**: every constant of the executable is computed before each test, so a constant whose computation panics fails every test of its group, each reporting that panic. Documented in the spec.
282. **Not**: caching builds between runs, parallel builds of groups, changes to codegen's speed (separate work: aggregates through memory, a C object cache), `lughac build`/`run`/`check`.
283. **Docs**: spec §9 `lughac test` ("Which" and "Running" bullets, the constants sentence), "Changes since v16", §11 row 124; Draft; CHANGELOG (Changed: faster); CLAUDE.md.
284. **Success**: kivuli's `lughac test test/` builds once (measured before and after, reported in CONTEXT.md); milestone 124, tag `m124`.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- `modules::load_all` with several roots; `load` delegating to it; the hidden `$test<n>` module paths.
- `driver/test.rs`: group, combined check with per-file fallback, one build per group, jobs keyed by module-qualified `LUGHA_TEST`.
- `codegen/test_main.rs`: dispatch on the module-qualified name.

### Must NOT Do
- No change to output, names, `--list`, `--json`, exit codes or per-test isolation.
- No change to how `git`, `cc` or `pkg-config` run (security-reviewed areas): a group's `[link]` tables are its package's, as one file's are today.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- Diagnostics per file as today. `LoadError` from any root ends the run with exit 2, as one root's does today.
- Decision 278's "passes alone, fails together" is an internal error (exit 2), never silently dropped.

## SECURITY CONSIDERATIONS
- Executables are still started directly from a fresh temporary folder; `LUGHA_TEST` carries only names the checker listed.

## TESTS TO WRITE
- [ ] `tests/test_runner.rs`: two files in one package, each with a same-named helper and a same-named test, both pass under their own names; each test writes `args()[0]` (its executable's path) to a file, and the two paths are equal (one build). Before the change they differ.
- [ ] A broken file beside good ones: its diagnostics, `1 file didn't compile`, the good files' tests run.
- [ ] Files from two packages and a loose file: three groups, all tests run.
- [ ] `--filter` selecting one file's test builds and runs only it; a broken unselected file prints nothing.
- [ ] A panicking constant in one file fails both files' tests of the group with its panic.
- [ ] Panic isolation, timeouts, `skip`, `--json` output unchanged (the existing `test_runner.rs` cases pass).
- [ ] `src/modules` unit tests for `load_all`: two roots sharing an import load it once; two loose roots get `$test0`/`$test1`.

## ROLLBACK PLAN
- Branch to return to: `main`; the work lives on `prp-156-shared_test_builds` until merged.

## ACCEPTANCE CRITERIA
- [ ] kivuli `lughac test test/` before/after timings recorded; one build per package
- [ ] All existing tests pass (`--lib`, `test_runner`, `lsp`'s test-running cases if any, `cli`)
- [ ] New tests written and passing; clippy and fmt pass; no file over 500 lines (propose a split of `driver/test.rs` if needed)
- [ ] Spec, Draft, CHANGELOG, CLAUDE.md updated

## VALIDATION
- `cargo fmt --check`; `cargo clippy --all-targets -- -D warnings`
- `cargo test --lib`; `cargo test --test test_runner --test cli`
- `time lughac test test/` in kivuli (read-only use: it writes nothing there but its temp folder)
