## FEATURE: Packages and tests — a package's modules tested as modules, `test/` inside its package, one `lugha.lock` per package, `lughac check` on a library file, and `skip(reason)` in tests; milestone 97 (v17).

**Status:** built in session 222; approved 2026-10-17 ("approved all and build in one go", session 220); proposed in session 219, with PRP-126 and PRP-127.
**Milestone:** 97 (tag `m97` at merge).
**Spec:** `Language v17 Draft.md` decisions 43–50; v17 spec §6 (packages: the implicit package, `lugha.lock`), §9 (`lughac check`, `lughac test`, E0303), §11 (row 97).

**Why:** kivuli's quirks 4–8, all from building a package with tests:
4. `lughac test src/diff.la` makes `diff.la` the main file, so a cycle back to it (`diff → ui → live → diff`) is E0319.
5. Files in the package's `test/` folder see only `public` items (E0313), so tests of private modules must live in `src/`.
6. One `lugha.lock` per entry folder (`examples/`, `src/`, `test/` each), each needing its own `lughac update`.
7. `lughac check src/diff.la` demands `main` (E0303).
8. No way to skip a test that needs a display.

**Decisions (discovery, session 219):**
- **The user's choice — skipping:** an intrinsic `skip(reason: string)`, callable inside a test (as Go's `t.Skip`, pytest's `skip`): the test stops and is reported skipped, not failed: `test kivuli.patches_a_button ... skipped (no display)`; the summary counts them (`12 tests: 11 passed, 0 failed, 1 skipped`); `--json` reports `"outcome": "skipped"` with the reason; the VS Code Testing view marks it skipped (extension minor bump); exit 0 unless something failed. Outside a test, `skip` panics ("skip outside a test").
- Mine, for review:
  - **A package's file is tested and checked as its module (4, 7):** when a file given to `lughac test` or `lughac check` lies in the `source` folder of a package (its nearest `lugha.toml` declares `name = N`), lughac builds it as module `N.<path>` reached from a hidden entry, not as a main file: cycles through it are fine, tests are named `N.diff.name`, and no `main` is needed. Other files keep today's rules.
  - **`check` without `main` (7):** `lughac check` reports E0303 only when the file is a main file that some command would run — a file with no `main` that isn't in a package's `source` is checked as a library (no E0303). `build` and `run` still require `main`.
  - **`test/`, `tests/`, `examples/` inside the package (5):** a program whose manifest is its package's (the implicit package) and whose file is under that manifest's folder belongs to the package for visibility: it sees the package's package-level items, as the package's own modules do. `private` stays private to its module.
  - **One lock per package (6):** `lugha.lock` lives beside the `lugha.toml` that governs the build (the root's manifest, found as today), not beside the main file; with no manifest, beside the main file as today. An existing lock beside a main file in a package is still read once if the manifest's folder has none, then written beside the manifest (one migration, noted by `lughac update`).
- No open entries in DECISIONS.md apply.

## OBJECTIVE
```
kivuli/
  lugha.toml        [package] name = "kivuli", source = "src"
  lugha.lock        ← the only lock
  src/diff.la       @test fun …  — `lughac test src/diff.la` builds `kivuli.diff`; diff → ui → live → diff is fine
  test/diff_test.la import kivuli.diff { Patch };   ← package-level items visible
$ lughac check src/diff.la    ← no main needed
$ lughac test
test kivuli.diff.patches_text ... ok
test kivuli.live.patches_a_real_button ... skipped (no display)
```

## CONTEXT
- **Starting state:** `modules/layout.rs` (the bounded manifest lookup, the implicit package), `modules/packages.rs`, `modules/fetch.rs` / `lock.rs` / `driver/update.rs` (the lock beside the main file), `driver/test.rs` + `test_json.rs` (one executable per file, `LUGHA_TEST`, outcomes), `codegen/test_main.rs`, `check/test_fns.rs` (`assert*` intrinsics), `check/modules/mod.rs` (E0303, `check_main`), `check/visibility.rs` + `levels.rs` (package-level visibility), `editors/vscode/src/tests.ts`/`testing.ts` (outcomes).
- **Ending state:** as the decisions; the runtime gets `lugha_rt_test_skip(reason)` (exits with a reserved code the runner reads as skipped — exit code 77, as automake's skip convention — after printing the reason on a channel the runner captures); spec §6 and §9 updated; `docs/errors` unchanged unless a new code is needed (none planned).
- **Related existing code:** above.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- Test-first, one change at a time (5 sub-parts), each with tests through `lughac` in `tests/package_layout.rs`, `tests/test_runner.rs`, `tests/git_packages.rs`.
### Must NOT Do
- No change for programs outside packages; no change to `build`/`run`'s need for `main`.
- `skip` never turns a failure into a pass: a test that panicked before calling `skip` failed.
### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- `skip` outside a test: a runtime panic. A test exiting with the skip code without having called `skip` (an `exit(77)`): reported `FAILED (exit code 77)` — the runner also checks the reason channel.

## SECURITY CONSIDERATIONS
- The lock moves but keeps its format and contents; `lughac update` writes only beside the manifest it read. No new I/O beyond that.

## TESTS TO WRITE
- [ ] `tests/test_runner.rs`: `skip` reported skipped, counted, JSON outcome; `skip` after a failed assert still failed; `exit(77)` without `skip` failed; a package module tested with a cycle through it; tests named `N.module.name`.
- [ ] `tests/package_layout.rs`: `test/` seeing package-level items (and not `private` ones); `lughac check` on a `source` file and on a non-package file without `main`.
- [ ] `tests/git_packages.rs`: one lock beside the manifest for programs in `examples/` and `test/`; the migration of an old lock.
- [ ] Extension: `tests.ts` maps `skipped`; version bump.

## ROLLBACK PLAN
- Branch `prp-128-packages_and_tests` in its own worktree.

## BUILT (session 222)
- Part 1 `skip(reason)`: `runtime/test.c` `lugha_rt_test_skip` (stderr line `lugha-skip: <reason>`, exit 77; outside a test it panics), `check/test_fns.rs`, `codegen/assertions.rs`, `driver/test.rs` (`Outcome::Skipped`), `test_json.rs` (`skipped`, summary `"skipped"`); extension 0.13.0 (`tests.ts`, `testing.ts`).
- Part 2: `pipeline::front_check` — no `fun main` in the main module → checked as `lughac test` checks. E0303's explain example is verified with `build`; `tests/programs/m3/e0303` moved to `tests/cli.rs` (the program runner checks rejected cases with `check`, and the runner isn't edited).
- Part 3: `Packages.own`; root modules join the package after every import resolved.
- Part 4: `LoadEnv.as_module`, `layout::module_path`; `resolve::check` takes `Option<&Path>` for the main file. Tests are qualified by the checker; `test_json::module_name` (moved from `test.rs`, package-aware) and `bare`. Under `check`, `import N.x` inside N's own file is E0312, as in any package module (was E0328 through `check`; that test now uses `build`).
- Part 5: `Fetcher.read_lock` returns whether it found one; `Fetcher.moved`; `Loaded.lock_folder`; `update::write_lock(folder, …)`. The old lock is left in place.
- Tests: `test_runner` (skip, outside a test, package file with a cycle, list/exact names), `package_layout` (test/ visibility, private, outside), `git_packages` (one lock, migration), `cli` (check without main, E0303 through build), test_json unit tests, extension `npm test`. Targeted: `--lib`, `cli`, `package_layout`, `git_packages`, `test_runner`, `modules`, `init`, `lsp`, `interface_values`, `explain`, `spec`, `programs`, fmt, clippy. Full suite not run (the user).
- Not done here: kivuli's own changes (another session owns it).

## ACCEPTANCE CRITERIA
- [ ] kivuli: `views.la` split and wrapper-free layout no longer needed; one lock; the GTK smoke test back with `skip`.
- [ ] Targeted tests (`--lib`, `test_runner`, `package_layout`, `git_packages`, `cli`, extension `npm test`), fmt, clippy pass; CHANGELOG updated.

## VALIDATION
- `cargo test --lib`, `cargo test --test test_runner --test package_layout --test git_packages --test cli`, `cd editors/vscode && npm test`, `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`
