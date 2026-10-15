## FEATURE: `std.fs` — list, create and remove directories and files, and test what exists; milestone 65.

**Status:** proposed 2026-10-15 — session 128 (discovery in session 127).
**Milestone:** 65 (one PRP; tag `m65` at merge)
**Spec:** `Language v15 Draft.md` decision 49; spec §5 (file intrinsics, for consistency), "The standard library", §9 (runtime table), §10 (a program), §11 (row 65).

**Decisions:**
- the v15 discovery's "directories" item (session 103) and session 127's answers;
- follows the M53/M54/M64 std pattern;
- no open entries in DECISIONS.md apply.

## OBJECTIVE
```
import std.fs;
fs.make_dirs("out/runs")?;
for name of fs.list_dir("data")? { … }
if fs.is_file("out/log.txt") { fs.remove_file("out/log.txt")?; }
```

## CONTEXT
- **Starting state:** file contents via the intrinsics (`read_file` …, `read_bytes` …), and nothing for directories.
- **Ending state:** decision 49.
- **Related existing code:** `src/std/` (new `fs.la`), `check/float_ops.rs` and `codegen/float_ops.rs` (`$os.*`), `runtime/` (new `fs.c`), `src/link.rs`, `src/modules/` (std module list).
- **Open decisions that must be resolved first:** none.

### Design
1. **`src/std/fs.la`** (all `public`):
   - `list_dir(path): Result<string[], string>`, names sorted by bytes, without `.` and `..`;
   - `make_dir(path)` and `make_dirs(path)`, `Result<i64, string>` with `Ok(0)`; `make_dirs` succeeds if the folder already exists;
   - `remove_file(path)` (a file or symlink, never a folder) and `remove_dir(path)` (an empty folder only), both `Result<i64, string>`;
   - `exists(path)`, `is_dir(path)` and `is_file(path)`, all `bool`. They follow symlinks, as `stat` does; `exists` is false for a broken link.
2. **Built-ins** `$os.list_dir`, `$os.make_dir`, `$os.remove_file`, `$os.remove_dir` and `$os.stat_kind`:
   - **runtime** `runtime/fs.c` uses `opendir`/`readdir`, `mkdir(path, 0777)` (the umask applies), `unlink`, `rmdir` and `stat`;
   - **`make_dirs`** is Lugha: it splits on `/` and calls `make_dir` on each prefix, ignoring "File exists" when that prefix is a folder;
   - **sorting** is done in Lugha (`sort`, M59).
3. **Errors:** the system's message (`strerror(errno)`), like `read_file`'s. A path containing NUL is `Err("path contains a NUL byte")`. A directory entry name that isn't UTF-8 is `Err("a name in <path> is not valid UTF-8")`.
4. **Runtime fragment rules** as for `env.c`.

### Risk
- **Deleting the wrong thing:** there is no recursive delete. `remove_dir` refuses non-empty folders and `remove_file` refuses folders, both through the OS's own checks.
- **Platform:** Linux and POSIX only, as the rest of the runtime.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- **Code:** the above.
- **Spec v15:** "The standard library": a `std.fs` table; §9 runtime rows; §10 program "Directories"; §11 row 65; the changes summary.
- **Docs:** CLAUDE.md (tree, `RUNTIME_SYMBOLS`), README, TODO, CHANGELOG, MEMORY plan and built entries, CONTEXT; tag `m65`.

### Must NOT Do
- **No recursive delete, no rename/copy/permissions** (not chosen).
- **No change to the existing file intrinsics.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **`Err` with the system's message** for every failing call. `exists`, `is_dir` and `is_file` never fail: any error is `false`.

## SECURITY CONSIDERATIONS
- **Paths are external input:** NUL bytes are rejected, and nothing is interpreted (no globbing, no `~` expansion).
- **Deletion is limited** to single files and empty folders.
- **Bounded memory:** directory listings grow a GC array, with no fixed buffers.
- **No `unsafe`, no dependencies.**

## TESTS TO WRITE
- [ ] **Program `m65/fs.la`** (in the case's scratch directory, `.scratch`):
  - `make_dir`, `make_dirs` (nested, then again: still `Ok`);
  - write files with `write_file`; `list_dir` sorted;
  - `is_dir`/`is_file`/`exists`;
  - `remove_file` on a folder gives `Err`; `remove_dir` on a non-empty folder gives `Err`, then on an empty one `Ok`;
  - `list_dir` on a missing path gives `Err("No such file or directory")`.
- [ ] **NUL path** gives `Err`.
- [ ] **E0312 help** lists `std.fs`.
- [ ] **Regression.**

## ROLLBACK PLAN
- Branch `prp-094-std_fs`; tag `m65`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] All tests pass at `-O0`; `m65/` at `-O2`.
- [ ] Spec at milestone 65; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] fmt, clippy and tests pass; CI green.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
```
