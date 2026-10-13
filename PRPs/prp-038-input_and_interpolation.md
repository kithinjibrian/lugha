## FEATURE: Input and interpolation — `args()`, `parse_i64`/`parse_f64`, `"{x}"` strings; the v3 spec begins.

**Status:** approved 2026-10-13 — session 48
**Milestone:** 13 (one PRP; first v3 milestone)
**Spec:** `Language v3 Draft.md`:
- decision 4;
- §2 "String interpolation";
- §3 (`interpolated`);
- §5 (interpolated strings, intrinsics);
- §9 (runtime, E0111);
- §10 "Arguments and interpolation";
- §11 (milestone 13).

**Decisions:** MEMORY 44 (v3 direction); MEMORY 35 (`lughac spec` follows the spec in progress, as at milestone 9).

## OBJECTIVE
Programs can read their command-line arguments, turn text into numbers, and build strings with `{…}`.
- `args(): string[]`, with `args()[0]` the program's path.
- `parse_i64(s): Option<i64>` and `parse_f64(s): Option<f64>`.
- `"{name} is {age}"` in every string literal, with `{{` and `}}` for literal braces.
- `Language v3 Specification.md` starts here and becomes what `lughac spec` prints.

## CONTEXT

- **Starting state:**
  - v2 is complete; `lughac spec` prints `Language v2 Specification.md`.
  - A program has no way to read its arguments, and strings are built with `+ to_string(x) +`.
  - The test runner and the spec extractor can't pass arguments to a program.
- **Ending state:**
  - The three intrinsics and interpolation work end to end.
  - `lughac run prog.la -- a b` passes arguments.
  - Tests can give arguments.
  - The v3 spec file exists, with milestone 13 folded in.
- **Related existing code:**
  - `src/lexer/{mod,string,token}.rs`, `src/parser/primary.rs` and `sexp/`, `src/ast/expr.rs`;
  - `src/check/{call,env}.rs`, `src/codegen/{runtime,lower}.rs`;
  - `runtime/*.c`, `src/driver/mod.rs`, `src/lib.rs`;
  - `tests/support/runner/`, `tests/common/spec_programs.rs`.
- **Open decisions that must be resolved first:** none.

### Discovery answers (session 48)
1. **The v3 spec starts now:**
   - `Language v2 Specification.md` is copied to `Language v3 Specification.md` ("in progress: milestone 13"), and milestone 13 is folded in;
   - `lughac spec` prints the new file;
   - the v2 file is frozen.
2. **Tests pass arguments:**
   - the runner reads an optional `<name>.args`, one argument per line;
   - the §10 extractor understands "Run as `prog a b`.";
   - the draft's §10 sentence becomes a single run: "Run as `prog 21`. Prints `21 doubled is 42`, exits with 0."

### Design choices for review
- **Lexing (draft §2):**
  - A string without `{` or `}` is one `Str` token, as today.
  - Otherwise the lexer emits `StrStart("a ")`, the expression's tokens, `StrMid(" b ")`, …, `StrEnd(" c")`. Each part's text has its escapes applied, and `{{`/`}}` are already reduced.
  - A stack tracks open braces inside an interpolation, so `{p.x}` and `{if c { 1 } else { 2 }}` end at the right `}`.
- **E0111 cases**, each reported once at the spot and then lexed on as plain text:
  - a lone `}`;
  - `{` without its `}` before the end of the string;
  - an empty `{}` or `{ }`;
  - a string literal inside an interpolation.

  A raw line break inside an interpolation ends the string with E0102, as today.
- **AST:** `ExprKind::Interp(Vec<Part>)`, where `Part` is `Text(String)` or `Expr(Expr)`. It prints as `(interp "a " x " b")`.
- **Arguments to `lughac run`:** `lughac run prog.la -- 21 x` passes `21` and `x`. The `--` keeps them apart from options like `-O2`. `args()[0]` is the path of the built executable.
- **`parse_i64`:** an optional `-`, then one or more decimal digits; no `+`, spaces, `_` or hex. Out of range is `None`.
- **`parse_f64`:** an optional `-`; then digits, optionally `.` and digits; then optionally `e`/`E` with an optional sign and digits. There is no `inf` or `nan`. `"3"` parses as `3.0`; `".5"`, `"5."` and `""` are `None`.
- **Lowering:**
  - An interpolation becomes a chain of `lugha_rt_str_concat`, with each value converted by the existing `lugha_rt_to_string_*`. A `string` part is used as is.
  - Constant text uses the existing string constants.
- **Runtime:**
  - The C `main` becomes `main(int argc, char **argv)` and passes both to `lugha_rt_init`.
  - `lugha_rt_args()` builds a fresh `string[]` through `lugha_rt_alloc` on each call, so programs can't share or change it.
  - `lugha_rt_parse_i64(s, &out)` and `lugha_rt_parse_f64(s, &out)` return a success flag; codegen builds the `Option` from it.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Lexer and parser**
- Interpolated strings as above, with E0111.
- `primary` accepts `StrStart expr { StrMid expr } StrEnd` into `ExprKind::Interp`.
- `--emit=tokens` and `--emit=ast` show the new tokens and node.

**Checker**
- **Interpolation:** each part must be a number, `bool` or `string`. Anything else is E0403 "can't interpolate a `Point`", with help to convert it first. The whole expression is `string`.
- **New intrinsics:** `args`, `parse_i64` and `parse_f64`, with arity (E0405) and argument (E0403) checks. Their names join `INTRINSICS`, so declaring a function with one of them is E0302.

**Codegen and runtime**
- Lower interpolation, `args()`, `parse_i64` and `parse_f64` as above.
- **Runtime:** new `lugha_rt_args`, `lugha_rt_parse_i64` and `lugha_rt_parse_f64`, and `lugha_rt_init(argc, argv)`. They go in the existing fragments, or a new `runtime/input.c` if a fragment would get too big (one more entry in `RUNTIME_PARTS`).

**Driver**
- `lughac run <file> [options] [-- <args>…]` passes `<args>` to the program.

**Tests infrastructure**
- **Runner:**
  - `tests/support/runner/` discovers `<name>.args` beside a run case and passes each line after `--`;
  - an `.args` file beside a reject case, or with no case, is reported as malformed.
- **Extractor:** `tests/common/spec_programs.rs` reads "Run as `prog a b`." into the program's arguments, and `tests/spec.rs` passes them.

**Spec**
- **Files:**
  - `Language v3 Specification.md` is created from the v2 spec. Its title is "(in progress: milestone 13)", with a status paragraph and a "Changes since v2" list.
  - `src/lib.rs` `SPEC` points to the new file.
  - The v2 file's status line says it is frozen and complete.
- **Folded in from the draft:**
  - §2: interpolation and E0111;
  - §3: `interpolated`;
  - §5: interpolated strings and the three intrinsics;
  - §9: runtime functions, `lughac run … -- args`, and E0111 in the code table;
  - §10: the arguments program, with the single-run sentence;
  - §11: a v3 milestone table starting at 13.
- **Compatibility:** the sentence about braces in strings and the new intrinsic names.
- **The draft's §10 sentence** is updated to the single run (discovery answer 2).

**Docs**
- CLAUDE.md:
  - STACK: the v3 spec is in progress;
  - the tree: new files, the runner's `.args` files.
- README (args and interpolation), TODO, CHANGELOG, MEMORY decision 45, CONTEXT.

### Must NOT Do
- **Later milestones:** no function types, closures or array helpers (milestones 14–16).
- **No `printf`-style formatting** or format specifiers (`{x:.2}`); only `{expr}`.
- **No reading of stdin or files.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- E0111 is a lexer `Diagnostic`; lexing continues.
- The new checker errors are records; checking continues.
- **Bad input text is not an error:** `parse_*` returns `None`, never panics.
- **`args()`:** the runtime trusts `argc`/`argv` from the C runtime only. Each argument is copied into a Lugha string of its length, with no assumption beyond C's NUL termination.

## SECURITY CONSIDERATIONS
- Command-line arguments are external input: they are only ever copied into strings, and parsing rejects anything malformed.
- No `unsafe`, and no new dependencies.

## TESTS TO WRITE

Unit tests — lexer (`string.rs`):
- [ ] `"a {x} b {y}"` gives `StrStart`, tokens, `StrMid`, tokens, `StrEnd`.
- [ ] `"{{x}}"` is one `Str("{x}")`.
- [ ] Nested braces `"{if c { 1 } else { 2 }}"` end at the right `}`.
- [ ] Escapes inside the parts are applied.
- [ ] E0111 for `"a } b"`, `"{x"`, `"{}"` and `"{f(\"s\")}"`.

Unit tests — parser:
- [ ] `"a {x + 1} b"` parses to `(interp "a " (+ x 1) " b")`.
- [ ] An interpolation of a call or a field works.

Unit tests — checker:
- [ ] Interpolating `i64`, `f64`, `bool` and `string` gives a `string`.
- [ ] A struct is E0403 with the help.
- [ ] `args()` is `string[]`.
- [ ] `parse_i64("1")` is `Option<i64>`.
- [ ] `parse_i64(1)` is E0403; `args(1)` is E0405.
- [ ] `fun args() {}` is E0302.

Runtime / codegen (`tests/codegen.rs` or programs):
- [ ] `parse_i64`: `"42"`, `"-7"`, `"9223372036854775807"`; and `None` for `""`, `"+1"`, `"1_0"`, `"0x1"`, `"9223372036854775808"`, `" 1"`.
- [ ] `parse_f64`: `"2.5"`, `"-1e3"`, `"3"`; and `None` for `".5"`, `"5."`, `"nan"`, `"1e"`.
- [ ] Interpolation prints `f64` as `print` does (`2.0`).

Acceptance — `tests/programs/m13/` (run at `-O0` and `-O2`):
- [ ] `double.la` + `double.args` (`21`) prints `21 doubled is 42`; `usage.la` with no args prints the usage line.
- [ ] `args.la` with `.args` `a`, `b c` (a space inside one argument) prints the count and each argument.
- [ ] `parse.la`: the parse cases above.
- [ ] `interp.la`: numbers, `bool`, strings, expressions, nested braces and `{{ }}`.
- [ ] `e0111.la`: reject golden.
- [ ] The §10 arguments program runs from the spec with `prog 21`.

Infrastructure:
- [ ] The runner's own tests cover `.args` discovery and a malformed orphan `.args`.
- [ ] `tests/cli.rs`: `lughac run p.la -- x y` passes both.

Regression:
- [ ] Every earlier test passes. No existing test program has a brace inside a string (checked in discovery).

## ROLLBACK PLAN

- Branch `prp-038-input_and_interpolation`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The §10 arguments program and the `m13/` programs pass.
- [ ] E0111 and the checker errors are reported as specified.
- [ ] `lughac spec` prints `Language v3 Specification.md` (in progress: milestone 13); v2 is frozen.
- [ ] CLAUDE.md, README, TODO, CHANGELOG and MEMORY are updated; tag `m13`.
- [ ] No file over 300 lines (splits proposed first); no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m13/double.la -- 21
```
