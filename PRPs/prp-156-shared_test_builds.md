## FEATURE: `lughac test` builds each package's test files into one executable instead of one per file; milestone 124 (v17).

**Status:** held (session 297, the user's choice): built on branch `prp-156-shared_test_builds` in session 296 but not merged — on kivuli's `test/` it measured no gain (old 669 s, new 652 s) because generic code is instantiated per test file and LLVM's slow instruction selector dominates. Re-measure after PRP-157 (aggregates through memory). Proposed 2026-10-20 (session 295), from the user's report "lughac test is slow". Measured on kivuli: `lughac test test/diff.la` takes 13 s, of which 0.1 s is checking, 0.4 s IR, ~9 s LLVM machine code (the shared library, rebuilt for every file) and ~2.4 s `cc`; files are built one after another, so `test/` (46 files) costs ~46 builds (estimated ~9 min). Discovery: (1) one executable per package; (2) a file with errors is reported alone and the rest still build; (3) with `--filter`/`--exact`, only files holding a selected test are built; (4) constants computed at start fail every test of the executable when one panics — accepted and documented.

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
276. **Groups**: test files are grouped by package — the folder of the `lugha.toml` that governs each file (the same lookup `load` does). A file governed by no manifest is a group alone. Each group is one build, one executable; groups are built one after another, in the order of their first file.
277. **Loading a group**: the loader takes several roots (a new `modules::load_all(roots, …)`; `load` becomes its one-root case). Each root is loaded as today's single root would be — a package's `source` file as its module, any other file as a hidden module `$test<n>` (never nameable, symbols `lugha_fn_$test<n>/…`), so two files' same-named items never clash. Imports resolve per root as today; a module reached from several roots is loaded once.
278. **Checking**: the group is checked together first. When that has errors, each of its files is checked alone (as today) to find which ones fail; their diagnostics print as today, they count in "K files didn't compile", and the group is built from the files that checked. A file whose own check passes but which fails only with the others (a clash the combined check finds) is a compiler bug: exit 2 with the diagnostics.
279. **Running**: `LUGHA_TEST` names a test by its module and name (`$test3.works`, `kivuli.diff.patches`) so files' same-named tests stay apart; the shown names (`test.diff.works`) are today's. Each test still runs as its own process with the same timeout, capture, panic location and exit status rules.
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
