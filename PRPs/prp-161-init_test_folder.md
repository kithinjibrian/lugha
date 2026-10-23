## FEATURE: `lughac init` also writes a `test/` folder holding one passing test, for programs and libraries; milestone 129 (v17).

**Status:** proposed 2026-10-24 (session 308), from the user's side request ("for lugha init add test folder"). Discovery: (1) one real test in both layouts — a library's tests its `greet`; a program's greeting moves into its own module, `src/greeting.la`, since a test can't import `src/main.la` (`main` outside the main file is E0304, tried in session 308); (2) the closing hint gains a second line, `test it with: lughac test …`.

## OBJECTIVE
`lughac init oyster && lughac test oyster` passes one test straight away, and `lughac run oyster/src/main.la` still prints the greeting. The same holds for `--lib`. Every written file is already formatted (`lughac fmt --check` lists nothing). As before, nothing is overwritten.

## CONTEXT
- Starting state: `src/driver/init.rs` (278 lines) writes `lugha.toml`, `src/main.la` and `.gitignore` for a program, and `lugha.toml`, `src/lib.la`, `examples/hello.la` and `.gitignore` for a library. `tests/init.rs` (286 lines) covers it.
- Ending state:
  - Program: `lugha.toml`, `src/main.la` (`import greeting;` and `println(greeting.hello("world"))`), `src/greeting.la` (`fun hello(who: string): string`, giving `Hello, <who>, from N!`), `test/greeting_test.la` (`import N.greeting;` and `@test fun greets()` checking `greeting.hello("world")` with `assert_eq`), `.gitignore`. The program's printed text changes from `Hello from N!` to `Hello, world, from N!`.
  - Library: as now, plus `test/N_test.la` (`import N;` and `@test fun greets()` checking `N.greet("world")`).
  - Output: the `created`/`kept` lines in that order, then `run it with: …` (or `run the example with: …`) and `test it with: lughac test <folder>` (`lughac test` with no path when `init` ran in the current folder).
- Related existing code: `src/driver/init.rs`, `tests/init.rs`; how `test/` sees its package: spec §6 "Programs inside the package" (milestone 97); `lughac test`: spec §9.
- Spec: §9 `lughac init` (the file table and "It prints … how to run the result"), §11's milestone table (new row 129), §0's v17 summary line. Approving this PRP approves exactly these spec edits.
- Open decisions that must be resolved first: none.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- Add the files above through the existing `files` list, so `create_new`'s keep-don't-overwrite rule and the `kept` line apply to them unchanged.
- Generate text that `lughac fmt` leaves alone (`@test` on its own line).
- Keep `init.rs` under 500 lines.
- Update the spec (§9 table and output sentence, §11 row 129, the v17 summary), README's `init` lines, CHANGELOG, and CLAUDE.md's v17 milestone list.

### Must NOT Do
- Don't write a `test/` file when its folder already holds files. Only the one file's own existence decides, as for every other file `init` writes. Reason: one rule for every file.
- Don't run `lughac test` or anything else from `init`. It still runs nothing and asks nothing.
- Don't add flags (`--no-test`). Nobody asked for one.

### New dependencies
- none

## ERROR HANDLING REQUIREMENTS
- No new variants: the new files go through `InitError::Io` like the others, and files already written stay.
- The `let _ = writeln!` on the new hint line has the same reason as the run line: a closed stdout doesn't undo the files.

## SECURITY CONSIDERATIONS
- Nothing new. The new paths are fixed relative paths under the folder `init` already writes to, and the name in them has already passed `valid` (an identifier, so it can't hold `/` or `..`).

## TESTS TO WRITE
- [ ] `tests/init.rs`, program: the exact output (5 `created` lines, the run line, the test line); the file list; `lughac run` prints `Hello, world, from oyster!`; `lughac test oyster` exits 0 and reports `1 passed`; `lughac fmt --check` on the folder lists nothing.
- [ ] `tests/init.rs`, library: `test/shapes_test.la` created; `lughac test` inside the folder exits 0 with `1 passed`; `fmt --check` lists nothing.
- [ ] `tests/init.rs`, current folder: the hint is `test it with: lughac test` with no path.
- [ ] `tests/init.rs`, `--name` overriding the folder: the library test file is named after the package (`test/geo_test.la` in `geo-lib/`) and passes.
- [ ] `tests/init.rs`, kept files: an existing `test/greeting_test.la` is kept byte for byte and reported `kept`.
- [ ] Existing refusals (manifest present, bad name, keyword, unwritable folder) are unchanged.

## ROLLBACK PLAN
- Branch to return to: `main`.
- State: `init` writes the v16/v17 layouts as before. Revert the branch's commits.

## ACCEPTANCE CRITERIA
- [ ] `lughac init x && lughac test x` passes, for both layouts
- [ ] Every generated file is `lughac fmt`-clean
- [ ] Existing `tests/init.rs` cases pass, with their expectations updated for the new files
- [ ] `cargo clippy --all-targets -- -D warnings` passes
- [ ] `cargo fmt --check` passes
- [ ] CHANGELOG.md updated

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test --lib`, `cargo test --test init`, `cargo test --test spec`, `cargo test --test explain`
