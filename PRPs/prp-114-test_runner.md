## FEATURE: `lughac test` — `@test` functions, `assert`/`assert_eq`/`assert_ne`, and a runner that runs each test in its own process, in parallel, with a time limit; milestone 84.

**Status:** approved 2026-10-16 ("build it", which also accepted the security note) — built in session 179; proposed in session 178.
**Milestone:** 84 (one PRP; tag `m84` at merge). The editor's "▶ Run test" link and general decorators are later milestones.
**Spec:** `Language v16 Draft.md` decisions 81–86; spec §2 (`@`), §3 (attributes), §5/§6 (the assertions, tests and `main`), §9 (the CLI table, a `lughac test` section, E0210 and E0434), §11 (row 84).

**Decisions (discovery, sessions 177–178):**
- `@test` (over `test fun` and a `test_` naming convention), with `@` reserved for attributes and Python-style decorators later;
- tests anywhere, `lughac test [paths…]` like `lughac fmt`;
- `assert`, `assert_eq`, `assert_ne`, usable in any code, showing both values;
- each test in its own process, in parallel, 10 s default time limit;
- `--filter`, `--timeout`, captured output shown on failure; exit 0/1/2.
- No open entries in DECISIONS.md apply.

## OBJECTIVE
```
// src/geo.la
fun area(r: f64): f64 = 3.14159 * r * r;

@test
fun area_of_unit() {
    assert(area(1.0) > 3.0);
    assert_eq(area(0.0), 0.0);
}
```
```
$ lughac test
test geo.area_of_unit ... ok
test parse.numbers ... FAILED
  src/parse.la:8:5: assert_eq failed: left 41, right 42
  --- output ---
  reading "41"

2 tests: 1 passed, 1 failed
```

## CONTEXT
- **Starting state:** `@` is a lexer error (E0101). Items have no attributes. Panics go through `lugha_rt_panic(msg, file, line, col)` and exit with 101 (stack overflow too, `runtime/stack.c`). Codegen's `lower.rs::c_main` wraps the user's `main`; a program without `main` is E0303. `lughac fmt`'s `driver/fmt.rs` expands paths. `lughac run` already builds to a temp file and starts the program; `pipeline.rs` builds one main file with its modules.
- **Ending state:**
  - **Lexer:** `@` is a token (`At`).
  - **Parser:** an item may be preceded by attributes `@name` (the AST gains `FunDecl.attributes: Vec<Ident>`, other items reject them): `@test` is kept; any other name is E0210 "unknown attribute `@name`" (help: "only `@test` exists"). `@test` before a non-`fun` item is E0210 too ("`@test` goes before a function"). The s-expression printer shows `(@test …)`; the formatter prints `@test` on its own line above the function (the corpus test covers it).
  - **Checker:** a `@test` function with parameters, type parameters or a return type, or inside an `impl`/interface, is E0434 "a test takes no parameters and returns nothing". `assert(cond: bool)`, `assert_eq(a: T, b: T)`, `assert_ne(a: T, b: T)` are intrinsics (`T` `Eq` and printable, as `println` accepts; else E0419/E04xx as for `==` and `println`). `Checked.tests`: each module's tests in declaration order. In test mode (`check_modules(…, tests: true)`), E0303 isn't reported for a missing `main`.
  - **Codegen:** the assertions lower to a branch and `lugha_rt_panic` with the call's location; `assert_eq`/`assert_ne` build the message with the printing helpers (`show.rs`): `assert_eq failed: left <a>, right <b>`. Test mode emits every test of the file as a function and a C `main` that runs the one named by the environment variable `LUGHA_TEST` (exit 0 on return; the runtime's 101 on a panic), with `LUGHA_TEST=?` listing them; not test mode: tests aren't emitted at all.
  - **Runtime:** `lugha_rt_test_name()` (reads `LUGHA_TEST`) in a new fragment `runtime/test.c`, so `lugha_rt.c` stays a list of parts.
  - **Driver:** `lughac test [paths…] [--filter TEXT] [--timeout SECS]` (`driver/test.rs`): files under the paths (as `fmt`), keeping those whose parse has a `@test`; each built in test mode to a temporary executable (errors → diagnostics, exit 1, its tests skipped); each test run as `LUGHA_TEST=<name> <exe>` with stdout and stderr captured, up to `std::thread::available_parallelism()` at once (threads waiting on child processes; no new dependency), killed after the timeout; results printed in file then declaration order. Test names are `module.name` (the file's module path, `main` for a bare file). Exit 0 / 1 / 2 (decision 85).
  - **Spec v16:** §2 `@`, §3 attributes and `@test`, the assertions with the intrinsics, §9 CLI row + section + E0210/E0434, §11 row 84, the changes summary.
  - **Docs:** CLAUDE.md (tree, COMMANDS), README, TODO (decorators, Run-test lens), CHANGELOG, MEMORY, CONTEXT; the VS Code grammar highlights `@test` (extension bumped).
- **Related existing code:** `src/lexer/punct.rs`, `src/ast/mod.rs`, `src/parser/item.rs`, `src/parser/sexp/item.rs`, `src/formatter/items.rs`, `src/check/{call,printing,errors}`, `src/codegen/{lower,unwrap,show,runtime}.rs`, `src/driver/{mod,pipeline,fmt}.rs`, `src/link.rs` (runtime parts), `editors/vscode/syntaxes/lugha.tmLanguage.json`.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The ending state above, test-first.
- Keep every existing program's output and IR unchanged when it has no tests (the codegen and spec tests guard this).

### Must NOT Do
- No other attributes, no user-defined decorators, no `@test(…)` arguments.
- No editor "Run test" link, no `lughac lsp` changes beyond what parsing `@` needs (the server's checks accept `@test` files).
- No new dependency (the parallel runner uses `std::thread` and `std::process`).
- No change to how `lughac run` or `build` start programs.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- User errors are diagnostics: E0210 (parser), E0434 (checker); a file that doesn't compile is reported and the run continues with the other files (exit 1).
- A test executable that can't be started, a temp file that can't be written → lughac's own error, exit 2.
- A test that exits with anything but 0 fails; its exit code and captured output are shown. A killed (timed-out) test is "timed out after N s".

## SECURITY CONSIDERATIONS
- `lughac test` builds and runs the user's own code, as `lughac run` does; the test executables are started directly (no shell), with the environment plus `LUGHA_TEST`, in the current folder. Temporary executables live in a fresh temp folder removed afterwards. For the user's review: this is a new way of starting programs (CLAUDE.md asks for review of how programs are started).

## TESTS TO WRITE
- [x] **Lexer/parser:** `@` lexes; `@test fun` parses with the attribute; E0210 for `@inline fun`, for `@test struct`; the sexp form; the formatter keeps `@test` and the corpus stays idempotent.
- [x] **Checker:** E0434 for parameters, a return type, type parameters, a method; `assert_eq` of two types, of a non-`Eq` type; E0303 waived only in test mode.
- [x] **Codegen / programs (`tests/programs/m84/`):** `assert` passing and failing (stderr, exit 101), `assert_eq`/`assert_ne` messages with strings, numbers and a struct; a program with `@test` functions builds and runs unchanged by `lughac run` (tests not emitted).
- [x] **`tests/test_runner.rs`:** a passing file (exit 0, summary); a failing assertion (message, location, captured output, exit 1); a panic, an overflow and a stack overflow each failing only their test; a timeout with `--timeout 1`; `--filter`; tests beside `private` code and in `tests/` through an import; a file with a compile error (diagnostics, others still run, exit 1); no tests (`0 tests`, exit 0); deterministic order with parallel runs.
- [x] **Regression:** every existing test passes; the extension's grammar test for `@test`.

## BUILT (session 179)
- As designed, with these specifics:
  - **AST:** `FunDecl.test: Option<Span>` instead of a list of attributes (`@test` is the only one); `parser/attributes.rs` (2 tests). Inside an `impl`, `@` is an ordinary syntax error (E0201), so methods never reach E0434.
  - **Checker:** `Signature.test`; `check/test_fns.rs` (4 tests): E0434 for a test's shape and for any call or value use (through `call_key` and `function_value`, so module calls are covered), the assertions (`assert(1)` reports the literal's E0401), `check_tests` and `Checked.tests: Option<Vec<String>>`; `errors/v16.rs`.
  - **Codegen:** `assertions.rs` (1 test) and `test_main.rs` (2 tests); tests are neither declared nor defined unless `Checked.tests` is `Some`.
  - **Runtime:** `runtime/test.c` (`lugha_rt_test_is`, `lugha_rt_test_unknown`), registered in `link.rs`.
  - **Driver:** `pipeline::front_tests`, the shared `TempDir`, `fmt`'s file walk; `driver/test.rs` (3 unit tests); `lughac test` in `driver/mod.rs`, `--timeout` at least 1.
- `tests/test_runner.rs` (5) and `tests/programs/m84/` (5 cases: assertions passing and failing, `assert_ne`, `assert`, E0434, E0210). The timeout case runs alone with `--timeout 1`; the crash cases use the default limit, as a loaded machine made a 1 s limit flaky once.
- **Changed fixtures:** the E0101 examples used `@`, which is now a token; they use a backtick (`m1/bad_char`, `tests/cli.rs`, lexer tests), and the spec's E0101 row says so.
- **Compatibility:** a function named `assert`, `assert_eq` or `assert_ne` is now E0302 (intrinsic names are reserved); the spec's compatibility line says so.
- **Doc comments** attach through an `@test` too (`lsp/docs.rs`, spec §2).
- The VS Code grammar highlights `@test` (`entity.name.function.decorator`), with a grammar test; extension 0.7.0, installed.

## ROLLBACK PLAN
- Branch `prp-114-test_runner` from `main`; tag `m84`.
- To abandon before merge: delete the branch. After: revert the merge.

## ACCEPTANCE CRITERIA
- [x] All tests pass; fmt, clippy, `cargo test`, `npm test` (CI in the background).
- [x] Spec at milestone 84; docs updated.
- [x] No file over 500 lines; no new dependencies; no `unsafe`.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo test --test test_runner
cargo run -q -- test tests/programs/m84
```
