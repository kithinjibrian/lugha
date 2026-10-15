## FEATURE: Changing the working directory — `env.set_current_dir(path)`, so a Lugha program (oyster's `cd`) can move; milestone 68.

**Status:** proposed 2026-10-15 — session 137.
**Milestone:** 68 (one PRP; tag `m68` at merge)
**Spec:** `Language v16 Draft.md` decisions 6–8; spec "The standard library" (`std.env`), §9 (runtime table), §10 (a program), §11 (row 68).

**Decisions:**
- partly reverses v15 draft decision 48 (MEMORY 118/122: `std.env` read-only) for the working directory only;
- follows M64's `$os` pattern;
- no open entries in DECISIONS.md apply.

## OBJECTIVE
```
import std.env;
env.set_current_dir("/tmp")?;
println(env.current_dir()?);          // /tmp
```
After that, relative paths in `read_file`/`write_file`/`std.fs` and every program `std.process` starts use the new directory.

## CONTEXT
- **Starting state:** `std.env` has `get` and `current_dir` over `$os.env`/`$os.cwd` (`runtime/env.c`, `codegen/os.rs`, `check::float_ops::os_signature`).
- **Ending state:** decisions 6–7.
- **Related existing code:**
  - `src/std/env.la`, `runtime/env.c`, `src/link.rs`;
  - `src/check/float_ops.rs` (`os_signature`), `src/codegen/os.rs`;
  - `tests/programs/m64/`;
  - oyster: `~/Documents/kithinji/oyster/src/exec.la`.
- **Open decisions that must be resolved first:** none.

### Design
1. **`std/env.la`:** `public fun set_current_dir(path: string): Result<i64, string> = $os.chdir(path);`
2. **`$os.chdir(path): Result<i64, string>`** goes to `lugha_rt_chdir(path, &error) -> i32`: a NUL in `path` is `Err("path contains a NUL byte")`, otherwise `chdir(path)` gives `Ok(0)` or the system's message. In `runtime/env.c`, added to `RUNTIME_SYMBOLS`.
3. **Codegen** follows `make_dir`'s shape in `codegen/os.rs`.
4. **oyster** (uncommitted, as the rest of it):
   - `cd dir` (with a leading `~` or `~/` expanded from `$HOME`);
   - `cd` alone goes to `$HOME`;
   - `cd -` goes to the previous directory and prints it, as bash does;
   - errors are `oyster: cd: <dir>: <message>`.

   The previous directory is kept by the shell (passed through the loop, or in a `Ref<string>`, M67).

### Risk
- **Low:** one system call. `chdir` affects the whole program, which is the point; documented in the spec.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- **Code:** the above.
- **Spec v16:**
  - the `std.env` table gains `set_current_dir`, and its intro drops "read-only" for the directory;
  - §9 runtime row; §10 program "Changing directory"; §11 row 68; the changes summary.
- **Docs:** CLAUDE.md (STACK), README, TODO (mark the item done), CHANGELOG, MEMORY plan and built entries, CONTEXT; tag `m68`.

### Must NOT Do
- **No setting of environment variables** (decision 6).
- **No per-call directory option for `std.process`** (not chosen).

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **`Err` with the system's message;** `Err("path contains a NUL byte")` for a NUL.

## SECURITY CONSIDERATIONS
- **No `unsafe`, no dependencies.**
- **The path is program input;** NUL is rejected and nothing is interpreted (no `~` expansion in the language).

## TESTS TO WRITE
- [ ] **Program `m68/cd.la`** (`.scratch`):
  - `make_dir("sub")`, write `sub/note.txt`, then `set_current_dir("sub")`;
  - `current_dir()` ends with `/sub`; `read_file("note.txt")` reads it; `fs.list_dir(".")` lists it; `process.run("ls", [])` lists it;
  - `set_current_dir("..")` goes back;
  - `Err` for a missing folder (`No such file or directory`), a file (`Not a directory`) and a NUL.
- [ ] **E0312 help** unchanged (no new module).
- [ ] **oyster:** a scripted session (`cd /tmp`, `pwd`, `cd -`, `cd`, `cd ~`, `cd nope`) printing the expected directories and error.
- [ ] **Regression:** every earlier program unchanged.

## ROLLBACK PLAN
- Branch `prp-097-set_current_dir`; tag `m68`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] All tests pass at `-O0`; `m68/` at `-O2`.
- [ ] Spec at milestone 68; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] fmt, clippy and tests pass; CI green.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
```
