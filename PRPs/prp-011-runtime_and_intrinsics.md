## FEATURE: The C runtime `lugha_rt.c`, the intrinsics `print`, `println`, `panic` and `to_string`, and string literals as values — so programs can print.

**Status:** implemented 2026-10-09 — session 14 (branch `prp-011-runtime_and_intrinsics`)
**Milestone:** 4, first of two PRPs. Done-when: the spec §10 hello world and recursion programs print their exact output. PRP-012 adds `extern fun` and the overflow and division panics.
**Spec:** §4 (`string` type), §5 (intrinsics, float formatting, panics), §6 (`panic` diverges; entry point and `GC_INIT`), §7 (heap layout, `lugha_rt_alloc`, string literals as constant globals), §9 (runtime library, link command)
**Decisions:** DECISION-009, resolved here: embed the runtime source and compile it in the existing `cc` call

## OBJECTIVE
`lughac run hello.la` prints `Hello, world!`, and `println(fib(30))` prints `832040`. Every primitive prints in its spec form, including the shortest round-trip `f64` format (`2.0`, `0.1`, `1.0e16`, `inf`, `NaN`). `panic("…")` prints `panic: … at file:line:col` to stderr and exits 101. Strings are ordinary values: bound, passed, returned, printed. Their operations arrive in milestone 5.

## CONTEXT

- Starting state:
  - No runtime. The C `main` only calls `lugha_fn_main`.
  - The checker stops on string literals, `string` annotations and intrinsics (milestone 4); codegen likewise.
  - `link` runs `cc prog.o -lgc -lm -o out`.
- Ending state:
  - New `runtime/lugha_rt.c`, embedded with `include_str!` and passed to `cc`.
  - `check::Type::String`; intrinsic typing in the checker.
  - Intrinsics, string constants and the runtime calls in codegen.
  - Codegen receives the source name and text, for panic locations.
  - New `tests/programs/m4/`. DECISION-009 resolved.
- Related existing code: `src/link.rs`, `src/check/{types,call,env,errors,ops}.rs`, `src/codegen/{lower,expr,function,value}.rs`, `src/driver/pipeline.rs`.
- Open decisions that must be resolved first: none (DECISION-009 is resolved by this PRP).

### Amendments during implementation (session 14)
- **Checker bug found and fixed:** `literal()` let an integer literal take any non-`f64`/`bool` expected type, so `let s: string = 1;` was accepted. It is now E0401 "integer literal where string expected" — only integer types hold integer literals.
- **Runtime source location:** `link` writes the runtime to its own private temp file (`create_new`, removed on drop), not next to the objects. A link whose object directory doesn't exist must still report `cc`'s error, not an I/O error.
- **C ABI for narrow types:** `bool` and `u8` cross into the runtime zero-extended to `i32` (recorded in spec §9), so nothing depends on C's narrow-parameter extension rules.
- **Updated earlier tests:**
  - The checker tests no longer expect strings and `println` to stop; a string `+` stop replaces them.
  - `tests/cli.rs`'s "not implemented" example is now an array literal (milestone 5).
- **Goldens:** the `m4/` expected outputs, including the float table and the panic line, were written by hand from the spec and matched on the first run.

### Discovery answers (session 14)
1. Milestone 4 is two PRPs: 011 runtime and intrinsics; 012 `extern fun` and overflow/division panics.
2. DECISION-009: `runtime/lugha_rt.c` is embedded with `include_str!`, written to the build's temp dir, and compiled by the existing link step: `cc prog.o lugha_rt.c -lgc -lm -o out`.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Runtime** — `runtime/lugha_rt.c` (spec §7, §9)
- `typedef struct { int64_t len; char bytes[]; } LughaString;` — `bytes` is NUL-terminated (§7).
- `void lugha_rt_init(void)` calls `GC_INIT()`. The C `main` calls it first, because a macro can't be called from IR. Recorded in spec §9.
- `void *lugha_rt_alloc(int64_t size)` → `GC_malloc`. On failure it panics with "out of memory".
- `void lugha_rt_panic(const LughaString *msg, const char *file, int64_t line, int64_t col)`:
  - prints `panic: <msg> at <file>:<line>:<col>` and a newline to stderr
  - flushes stdout
  - calls `exit(101)`
- Printing, all through C `stdout` so output interleaves with libc (§9):
  - `lugha_rt_print_i32/i64/u8/f64/bool/str(x)` print the value.
  - `lugha_rt_print_newline()` prints a newline.
  - `u8` prints as a number; `bool` as `true`/`false`.
- `lugha_rt_to_string_i32/i64/u8/f64/bool(x)` return a new `LughaString*` from `lugha_rt_alloc`.
- **`f64` formatting (§5)**, shared by printing and `to_string`:
  - The shortest decimal that round-trips: try `%.{p}e` for `p` = 0…16 until `strtod` gives back the same bits.
  - Written in Lugha float syntax, always with `.` and a digit on each side.
  - An exponent (`1.0e16`, `2.5e-7`, no `+`, no leading zeros) when the magnitude is ≥ `1e16`, or below `1e-5` and nonzero; fixed notation otherwise.
  - `inf`, `-inf`, `NaN`; `-0.0` prints as `-0.0`.
- No other public symbols; helpers are `static`.

**Linking** — `src/link.rs`
- `pub const RUNTIME_SOURCE: &str = include_str!("../runtime/lugha_rt.c");`
- `link(objects, output)` writes the runtime next to the first object (the caller's private temp dir) and runs `cc <objects> <dir>/lugha_rt.c -lgc -lm -o <output>`.
- `cc`'s errors still come back as `LinkError::Failed`.

**Checker** (spec §4, §5, §6)
- `Type::String`, displayed as `string`. `string` annotations resolve for `let`, parameters and return types.
- String literals have type `string`.
- Intrinsic calls, in `check/call.rs`:
  - `print(x)`: exactly one argument, of any primitive type or `string`. Returns `void`.
  - `println()` or `println(x)`: zero or one argument, same rule. Returns `void`.
  - `panic(msg)`: one `string` argument. Its type is `Never` (§6: a `panic(...)` statement definitely returns).
  - `to_string(x)`: one argument of a primitive type, not `string`. Returns `string`.
  - Wrong argument count → E0405.
  - A wrong argument type → E0403, with exact messages:
    - `print`/`println`: "`println` expects a number, bool or string, found T"
    - `to_string`: "`to_string` expects a number or bool, found string"
    - `panic`: "`panic` expects a string, found T"
  - A void argument stays E0407.
  - Intrinsic names used as values → E0406.
- String operations stop with "not implemented yet: string operations (milestone 5)":
  - binary operators with a `string` operand (`+`, `==`, `<`, …)
  - `.len` and `s[i]`, which already stop with milestone 5
- `extern fun` still stops with milestone 4 (PRP-012).

**Codegen**
- `string` lowers to an opaque `ptr`.
- **String literals** are private `unnamed_addr` constant globals `{ i64 len, [len+1 x i8] }` holding the bytes and a NUL. The value is a pointer to the global (§7). Identical literals may share one global.
- Intrinsics:
  - `print` and `println` call `lugha_rt_print_<type>`, chosen by the argument's checked type; `println` then calls `lugha_rt_print_newline`.
  - `to_string` calls `lugha_rt_to_string_<type>`.
  - `panic(msg)` calls `lugha_rt_panic(msg, file, line, col)` with the call's location, then emits `unreachable` and continues in a dead block. Its value is `Never`.
- The location comes from the source file the driver passes in. The API becomes `emit_ir(program, checked, source)` and `emit_object(program, checked, source, opt, path)`, with `pub struct SourceInfo<'a> { pub name: &'a str, pub text: &'a str }`. The file name is a private constant C string; line and column are 1-based, with byte columns (§9).
- The C `main` calls `lugha_rt_init()` before `lugha_fn_main()` (§6, §9).
- Runtime functions are declared on first use with their exact C signatures.

**Docs**
- DECISION-009 → resolved (outcome above), copied to MEMORY.md.
- Spec §9: the runtime table gains `lugha_rt_init` (it runs `GC_INIT()`) and `lugha_rt_print_newline`; the link command shows `lugha_rt.c` compiled in the same `cc` call.
- CLAUDE.md file tree gains `runtime/`.

**Acceptance** — `tests/programs/m4/`, run mode, cross-checked at `-O2` by `tests/codegen.rs` (extended to `m4/`):
- `hello.la`: the spec §10 hello world → `Hello, world!\n`, exit 0.
- `recursion.la`: the spec §10 recursion program → `832040\n`, exit 0.
- `float_format.la`: prints, one per line:
  - `2.0`, `0.1`, `-0.0`, `100.0`, `1.5e-7`, `1.0e16`, `123456789012345.6`, `1.4142135623730951`
  - `1.0 / 0.0`, `-1.0 / 0.0`, `0.0 / 0.0`

  Expected stdout written by hand from §5.
- `print_values.la`: `i32`, `i64` min, a `u8`, `true`/`false`, `print` without a newline followed by `println`, and `println()` alone.
- `strings.la`: a string bound to a `let`, passed to and returned from a Lugha function, then printed; `to_string` of each primitive printed.
- `panic.la`: `panic("boom")` in a helper → stderr `panic: boom at tests/programs/m4/panic.la:L:C\n`, empty stdout, exit 101. This also proves stdout written before the panic is flushed.

### Must NOT Do
- No string operations (`+`, `==`, `.len`, indexing), arrays or structs (milestone 5).
- No `extern fun` and no overflow/division panics (PRP-012); arithmetic keeps wrap and trap.
- No new Rust dependencies, no `unsafe` in Rust. The C runtime contains no undefined behaviour: every format buffer is sized for its worst case.

## ERROR HANDLING REQUIREMENTS

- Runtime: allocation failure → panic "out of memory". Formatting never overflows a buffer: `snprintf` everywhere, with buffers sized for the longest `%.17e` and for `i64` minimum.
- Linking: a C compile error in the runtime would be a compiler bug. It surfaces as `LinkError::Failed` with `cc`'s stderr (exit 2).
- Checker: intrinsic misuse follows the existing codes; nothing new is allocated.

## SECURITY CONSIDERATIONS

- The runtime handles program-controlled strings. `lugha_rt_print_str` writes exactly `len` bytes with `fwrite`, so embedded NULs and `%` characters print literally and can't act as format specifiers.
- The panic message goes through `fwrite`, never as a `printf` format.
- Runtime and object files are written only to lughac's private temp dir.

## TESTS TO WRITE

Unit tests:
- [x] Checker: string literal typing; `string` annotations; each intrinsic's arity (E0405) and argument rules (E0403/E0407); `to_string("s")` rejected; `panic` makes a function definitely return (no E0503); string `+` and `==` stop with milestone 5; `let p = println;` → E0406.
- [x] Codegen IR: a literal becomes a `private unnamed_addr constant { i64, [N x i8] }` with the right length and NUL; `println(1)` calls `lugha_rt_print_i64` then `lugha_rt_print_newline`; `panic` passes the right line and column; the C `main` calls `lugha_rt_init` first.
- [x] Link: `RUNTIME_SOURCE` is non-empty and defines every `lugha_rt_*` symbol codegen declares (checked by name).

Acceptance:
- [x] All six `m4/` programs pass through `lughac run` and agree at `-O0`/`-O2`.
- [x] All existing tests still pass (programs without intrinsics link the runtime too).

## ROLLBACK PLAN

- Branch `prp-011-runtime_and_intrinsics`, merged into `main` on acceptance.
- To abandon: delete the branch. Nothing persists outside the repo.

## ACCEPTANCE CRITERIA
- [ ] `lughac run tests/programs/m4/hello.la` prints `Hello, world!`; `recursion.la` prints `832040`.
- [ ] Every test above exists and passes.
- [ ] DECISION-009 resolved; spec §9, CLAUDE.md, MEMORY.md, CHANGELOG.md, TODO.md updated.
- [ ] No Rust file over 300 lines (`lugha_rt.c` is held to the same limit); no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass. `cc -Wall -Wextra -Werror -c runtime/lugha_rt.c` compiles cleanly (a test runs it).

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo run -q -- run tests/programs/m4/hello.la` → `Hello, world!`
