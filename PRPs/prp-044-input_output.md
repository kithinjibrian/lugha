## FEATURE: Input and output — stdin lines, text files, stderr and `exit`; v4 complete.

**Status:** approved 2026-10-13 — session 55
**Milestone:** 19 (one PRP; completes milestone 19 and the v4 spec, tags `m19` and `v4`)
**Spec:** `Language v4 Draft.md`:
- decision 5;
- §5 (intrinsics);
- §7 (I/O strings);
- §9 (runtime, tests);
- §10 "Reading input" (corrected, below);
- §11 (milestone 19);
- §12 (very large files, `exit` and buffering).

**Decisions:** MEMORY 49 (v4 direction), MEMORY 50 (`Result`), MEMORY 45 (the runner's `.args` and the extractor's run sentence).

## OBJECTIVE
Programs read stdin line by line, read and write text files, write to stderr, and stop with an exit code. The §10 reading-input program, given `5`, `7` and `x` on stdin, prints `12` and `skipped 1`.

| Intrinsic | Type |
| --- | --- |
| `read_line()` | `Option<string>`: the next stdin line without `\n`/`\r\n`; `None` at the end |
| `read_file(path)` | `Result<string, string>` |
| `write_file(path, text)` | `Result<i64, string>`: bytes written |
| `append_file(path, text)` | `Result<i64, string>` |
| `eprint(x)`, `eprintln(x)` | `void`: like `print`/`println`, to stderr, after flushing stdout |
| `exit(code)` | never returns: flushes stdout and exits with `code` (`i32`) |

When this is done, the v4 spec is complete.

## CONTEXT

- **Starting state:**
  - Programs read only `args()`, and write only to stdout.
  - The test runner gives every program an empty stdin and runs it from the repository root.
  - The draft's §10 reading-input program has an invalid arm, `Some(n) => total += n,` (assignment is a statement).
- **Ending state:**
  - The seven intrinsics work end to end.
  - The runner can feed stdin and give a case a scratch directory.
  - The extractor reads stdin from the §10 sentence.
  - The spec is complete.
- **Related existing code:**
  - `src/check/input.rs` (input intrinsics), `src/check/call.rs`, `src/check/env.rs` (`is_intrinsic`);
  - `src/codegen/input.rs`, `src/codegen/runtime.rs` (print);
  - `runtime/input.c`, `runtime/lugha_rt.c`, `src/link.rs`;
  - `tests/support/runner/` (discover, execute, args), `tests/common/spec_programs.rs`, `tests/spec.rs`.
- **Open decisions that must be resolved first:** none.

### Discovery answers (session 55)
1. **Scratch directories are opt-in.** An empty `<name>.scratch` beside a run case makes the runner run it in a fresh temporary directory, removed afterwards, with the program passed by absolute path. Other cases are unchanged. Scratch cases don't compare panic output, whose path would be absolute.

### Design choices for review
- **The corrected §10 program:** the arms become `Some(n) => { total += n; }` and `None => { skipped += 1; }`. Its sentence reads "Run with `5`, `7` and `x` on three lines of stdin.", and the extractor turns each backticked item into a line.
- **`read_line`:**
  - reads one line, with no length limit;
  - removes one trailing `\n` and then one trailing `\r`;
  - returns a last line that has no `\n`;
  - returns `None` only at the end of input;
  - treats a read error like the end of input (`None`): a program reading stdin can't do better.
- **File errors** carry the C library's message, such as `No such file or directory` or `Permission denied`, as the draft says. Tests check the missing-file text, which is the glibc wording on Linux and in CI.
- **`write_file`/`append_file`** open the file as text in binary mode (no newline translation), write every byte, and return their count. A short write is an `Err` with the system's message.
- **`eprint`/`eprintln`** accept exactly what `print`/`println` accept and format identically, sharing the formatting code: a number is turned into text as `to_string` does, then written. stdout is flushed first.
- **`exit(code)`:**
  - flushes stdout, then calls C `exit(code)`;
  - its type is `never`, so code after it is dead (W0101 applies, as after `panic`);
  - the exit code is the low 8 bits, as for `main`'s result.
- **Names:** the seven become intrinsics, so declaring a function with one of these names is E0302. They join the `input.rs` list, and `is_intrinsic` covers them.
- **Large files:** `read_file` reads the whole file (the draft's §12, resolved as proposed).

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Checker** (`check/input.rs`)
- **Types:** the seven intrinsics, with arity (E0405) and argument (E0403/E0401) checks.
- **Print-like checks:** `eprint`/`eprintln` reuse the `print`/`println` argument rules.
- **Divergence:** `exit` is `never`.

**Codegen** (`codegen/input.rs`, or a new `codegen/io.rs` if `input.rs` would pass 300 lines)
- **Results:** build `Option`/`Result` values from each runtime function's success flag and out-parameters, as `parse_i64` does.
- **stderr:** `eprint`/`eprintln` convert the value to text (strings as they are), then call `lugha_rt_eprint(text, newline)`.
- **`exit`:** calls `lugha_rt_exit(code)`, then marks the code after it unreachable.

**Runtime** (`runtime/input.c`)
- `lugha_rt_read_line(&out)`;
- `lugha_rt_read_file(path, &text, &error)`;
- `lugha_rt_write_file(path, text, append, &count, &error)`;
- `lugha_rt_eprint(text, newline)`;
- `lugha_rt_exit(code)`.

Each allocates through `lugha_rt_alloc`, and each joins `RUNTIME_SYMBOLS`.

**Tests infrastructure**
- **Runner:**
  - an optional `<name>.stdin` beside a run case is fed to the program; without one, stdin stays empty;
  - an empty `<name>.scratch` runs the case in a temporary directory;
  - either beside a reject case, or with no case, is malformed.
- **Extractor:** "Run with `a`, `b` and `c` on … stdin." gives the program's stdin, and `tests/spec.rs` feeds it.

**Spec** — `Language v4 Specification.md`
- **Folded in:**
  - §5: the intrinsics and the `exit`/`eprint` notes;
  - §7: I/O strings;
  - §9: the runtime functions;
  - §10: the corrected reading-input program;
  - §11: milestone 19 done.
- **Completion:** the title drops "(in progress: …)", the status says v4 is complete, and "Changes since v3" gains I/O.
- **The compatibility sentence** adds the seven intrinsic names.
- **The draft:** its §10 program and sentence are corrected (with a note), and its §12 file-size and `exit` questions are resolved.

**Docs**
- CLAUDE.md: STACK (v4 complete), anti-pattern 5, the tree (runner `.stdin`/`.scratch`).
- README, TODO (milestone 19 ✅, v4 complete, the draft-bug item done), CHANGELOG, MEMORY decision 52, CONTEXT; tags `m19` and `v4`.

### Must NOT Do
- **No binary I/O, directories, streams or processes,** and no environment variables.
- **No `unwrap()`,** and no error conversion in `?`.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **I/O failures are values** (`Err`, `None`), never panics.
- **User mistakes** are diagnostics.
- **Codegen invariants** are `expect`s stating the invariant.

## SECURITY CONSIDERATIONS
- **File paths and stdin are external input:**
  - paths are passed to the C library unchanged, exactly as the program gives them;
  - every read is bounded by the size actually read;
  - lines have no length limit, but come from the program's own stdin;
  - no shell is ever involved.
- **Writes:** `write_file` can replace any file the program's user can write. That is the program's intent, so the compiler adds nothing.
- No `unsafe` in Rust; the C runtime checks every `fopen`/`fread`/`fwrite` result.

## TESTS TO WRITE

Unit tests — checker (`check/input.rs`):
- [ ] **Types:**
  - `read_line()` is `Option<string>`;
  - `read_file("p")` is `Result<string, string>`;
  - `write_file("p", "t")` and `append_file` are `Result<i64, string>`;
  - `eprintln(1)` checks;
  - code after `exit(1)` gets W0101.
- [ ] **Errors:**
  - `read_line(1)` is E0405;
  - `write_file(1, "t")` is E0403;
  - `exit("x")` is E0403;
  - `eprint([1])` is E0403;
  - `fun exit() {}` is E0302.

Runner (`tests/support/runner/`):
- [ ] **Discovery:** `.stdin` and `.scratch` are found; beside a reject case, or orphaned, they are malformed.
- [ ] **Running:** a `.stdin` case gets its input, and a `.scratch` case runs in a temp directory that is gone afterwards.

Acceptance — `tests/programs/m19/` (run at `-O0` and `-O2`):
- [ ] `sum.la` + `sum.stdin`: sums numbers, counts bad lines, and handles `\r\n` and a last line without `\n`.
- [ ] `empty.la`: an empty stdin gives `None` at once.
- [ ] `files.la` + `files.scratch`:
  - `write_file`, `append_file`, then `read_file` back;
  - the byte counts;
  - a missing file gives `Err("No such file or directory")`;
  - `?` across the I/O calls in a helper returning `Result`.
- [ ] `stderr.la`: `eprint`/`eprintln` with a `.stderr` golden, and stdout flushed first (the order is checked through a test of the runtime's flush; streams are captured apart).
- [ ] `exit.la`: `exit(3)` after buffered `print` output gives exit code 3 with the output present.
- [ ] `e0403.la`: reject golden (`write_file(1, "t")`).
- [ ] The §10 reading-input program runs from the spec with its stdin.

Regression:
- [ ] Every earlier test passes; programs without I/O give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-044-input_output`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The §10 reading-input program and the `m19/` programs pass.
- [ ] The spec is complete (no "in progress"); docs updated; tags `m19` and `v4`.
- [ ] No file over 300 lines (splits proposed first); no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
printf '5\n7\nx\n' | cargo run -q -- run tests/programs/m19/sum.la
```
