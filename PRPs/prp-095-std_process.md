## FEATURE: `std.process` — run another program with an argument list (never a shell), wait, and get its exit code and output; milestone 66.

**Status:** approved 2026-10-15 — session 129 ("approved"); proposed in session 128 (discovery in session 127). Its approval is the user's security review.
**This PRP needs the user's security review.** Approving it is that review (draft decision 51).
**Milestone:** 66 (one PRP; tag `m66` at merge)
**Spec:** `Language v15 Draft.md` decisions 50–51; spec "The standard library", §9 (runtime table), §10 (a program), §11 (row 66).

**Decisions:**
- the v15 discovery's "processes" item (session 103; security review required, like PRP-079);
- session 127's answers;
- follows the M53/M54/M64/M65 std pattern;
- no open entries in DECISIONS.md apply.

## OBJECTIVE
```
import std.process;
let out = process.run("git", ["rev-parse", "HEAD"])?;      // captured
if out.code == 0 { println(out.stdout.trim()); }
let code = process.call("cc", ["-o", "x", "x.c"])?;         // inherits the terminal
let sorted = process.run_with("sort", [], "b\na\n")?;      // stdin text
```

## CONTEXT
- **Starting state:** no way to start a program from Lugha.
- **Ending state:** decisions 50–51.
- **Related existing code:** `src/std/` (new `process.la`), `check/float_ops.rs` and `codegen/float_ops.rs` (`$os.*`), `runtime/` (new `process.c`), `runtime/lugha_rt.c` (core includes; close-on-exec of runtime fds), `src/link.rs`, `src/modules/`.
- **Open decisions that must be resolved first:** none.

### Design
1. **`src/std/process.la`:**
   - `public struct Output { code: i32, stdout: string, stderr: string }`;
   - `run(cmd: string, args: string[]): Result<Output, string>`;
   - `run_with(cmd: string, args: string[], input: string): Result<Output, string>`;
   - `call(cmd: string, args: string[]): Result<i32, string>`.
2. **One runtime entry,** `lugha_rt_spawn(cmd, args, input, capture, &code, &out, &err, &error) -> i32`, through `$os.spawn`. `capture = 0` for `call`: the child's 0/1/2 are the parent's. `input` is empty for `run`: the child's stdin is `/dev/null`, so a child that reads stdin can't hang on the terminal.
3. **Starting** (decision 51):
   - **No shell, no splitting:** `posix_spawnp(cmd, argv = [cmd, args…, NULL])`. A `cmd` without `/` is looked up on `PATH`; with `/`, it is used as a path.
   - **NUL bytes:** a NUL in `cmd` or any argument returns `Err("… contains a NUL byte")` before anything starts.
   - **Environment:** `environ`, unchanged.
   - **Descriptors:** pipes are created `O_CLOEXEC` (`pipe2`), and the file actions `dup2` them onto 0/1/2 in the child. Every descriptor the runtime opens elsewhere (`read_file` and friends, `fopen` mode `"…e"`, `/dev/urandom`) is close-on-exec, and a test checks it.
4. **Talking to the child:** a `poll` loop writes `input` and reads stdout and stderr together, so no pipe can fill and deadlock. `EINTR` is retried. `SIGPIPE` is blocked around the writes, so a child that closes its stdin early just ends the input (`EPIPE`) instead of killing the program.
5. **Waiting:**
   - `waitpid`; an exit gives its status, and death by signal `n` gives `128 + n`;
   - a spawn failure (not found, not executable) is `Err` with the system's message, e.g. `"git: No such file or directory"`.
6. **Output:** captured into GC strings with growing buffers. Each must be valid UTF-8, else `Err("stdout is not valid UTF-8")` (or stderr); the decoder is shared with M62.
7. **Limits (stated in the spec):** no timeout, no output size limit, no working directory or environment arguments, all in v15.

### Risk
- **This is the first Lugha feature that runs other programs.**
  - **Shell injection:** impossible by construction, since no shell is involved and arguments are passed as given.
  - **PATH hijacking:** the usual caveat, documented.
  - **Leaked descriptors:** prevented by close-on-exec, with a test.
  - **Deadlock:** prevented by `poll`.
  - **Zombies:** every path waits.
- **Tests run real programs:** they use only `echo`, `sort`, `cat`, `sh -c 'exit 3'` and `kill -9 $$`. The last two run inside `sh` as the *child program*, which is fine: Lugha passes `-c` as an argument. Any test that needs them is skipped where they are missing.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- **Code:** the above.
- **Spec v15:** "The standard library": a `std.process` table with the exact rules of Design 3–7, including the PATH caveat; §9 runtime rows; §10 program "Processes"; §11 row 66; the changes summary.
- **Docs:** CLAUDE.md (tree, `RUNTIME_SYMBOLS`; security note next to `modules/git.rs`'s: changes to `runtime/process.c` need the user's review), README, TODO, CHANGELOG, MEMORY plan and built entries, CONTEXT; tag `m66`.

### Must NOT Do
- **Never run a shell on the program's behalf:** no `system`, `popen` or `sh -c` built from strings.
- **No environment or working-directory changes** for the child; no timeouts, and no detaching or backgrounding (not chosen).
- **`lughac` itself keeps running `cc` and `git` as it does today** (no change to `link.rs` or `modules/git.rs`).

### New dependencies
None (libc's `posix_spawn`, `pipe2`, `poll` and `waitpid`).

## ERROR HANDLING REQUIREMENTS
- **`Err(message)`** when the program can't start, an argument contains NUL, or the output isn't UTF-8.
- **Any exit status is `Ok`:** a normal exit gives its code, and a signal gives `128 + n`.

## SECURITY CONSIDERATIONS
- **External input:** command names, arguments and stdin text come from the program. They are passed to `posix_spawnp` verbatim, and NUL is rejected.
- **No shell:** metacharacters (`;`, `|`, `$()`, globs) have no meaning.
- **PATH:** a bare `cmd` resolves through the inherited `PATH`; whoever controls `PATH` controls which program runs. Documented, and use `/`-paths when that matters.
- **Descriptors:** only 0/1/2 reach the child.
- **Signals:** `SIGPIPE` is handled during writes; other signal handling is unchanged.
- **Memory:** growing GC buffers, with overflow-checked sizes.
- **No `unsafe`, no dependencies.**

## TESTS TO WRITE
- [ ] **Program `m66/process.la`:**
  - `run("echo", ["a b", "c"])` gives stdout `"a b c\n"` and code 0, showing an argument with a space stays one argument;
  - `run_with("sort", [], "b\na\n")` gives `"a\nb\n"`;
  - `run("sh", ["-c", "exit 3"])` gives code 3;
  - `run("sh", ["-c", "kill -9 $$"])` gives code 137;
  - `run("no-such-program-xyz", [])` gives `Err`;
  - `run("echo", ["; echo injected"])` prints the literal text;
  - `call("echo", ["hi"])` lets `hi` through;
  - a NUL argument gives `Err`.
- [ ] **Descriptor test:**
  - after using `random.from_os()` and reading and appending to files, a program runs `sh -c 'ls /proc/self/fd'`;
  - only 0, 1, 2 and the listing's own descriptor appear.
- [ ] **Deadlock test:** a child writing 1 MB to stderr and 1 MB to stdout completes.
- [ ] **E0312 help** lists `std.process`.
- [ ] **Regression.**

## ROLLBACK PLAN
- Branch `prp-095-std_process`; tag `m66`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The user approved this PRP as its security review.
- [ ] All tests pass at `-O0`; `m66/` at `-O2`.
- [ ] Spec at milestone 66; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] fmt, clippy and tests pass; CI green.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
```
