## FEATURE: `fs.sync(path)` — flush a file's or folder's data to the disk, so a saved file survives a power cut; milestone 116 (v17).

**Status:** approved 2026-10-18 (session 280); proposed in session 279, from the user's request ("fs.sync"), the follow-up named when `fs.rename` shipped (milestone 115).

## OBJECTIVE
```lugha
import std.fs;

write_file("state.tmp", data)?;
fs.sync("state.tmp")?;          // the new bytes are on the disk
fs.rename("state.tmp", "state")?;
fs.sync(".")?;                  // and so is the rename (the folder's entry)
```
After these four steps, a power cut leaves either the old `state` or the new one, complete — never an empty or half-written file.

## CONTEXT
- `fs.rename` (milestone 115) makes the swap atomic for anyone reading, but the kernel may still hold the new data and the new directory entry in memory; a power cut can then lose them. `fsync(2)` on the file writes its data out; `fsync` on the folder writes out the rename.
- `std.fs` is Lugha over `$os.*` built-ins in `runtime/fs.c`, lowered through `codegen/net.rs`'s signature-driven path (milestone 115).

## DECISIONS (draft decisions 225–230)
225. **`fs.sync(path: string): Result<(), string>`**: opens `path` read-only (`O_RDONLY | O_CLOEXEC`; a folder too, with `O_DIRECTORY` tried when it is one), calls `fsync`, closes it. `Err` with the system's message when it can't be opened or synced; `Err` for a NUL byte. It follows symbolic links (as `open` does).
226. **Files and folders alike**: one function, since syncing the folder is how a rename (or a new file) becomes durable; the docs show the four-step pattern above.
227. **`EINTR`** retries; `fsync`'s other errors (`EIO`, `ENOSPC`) are `Err` — the data may not be on the disk, and the program should know.
228. **Not**: `fdatasync`, a combined `write_atomic(path, bytes)` helper (the pattern is four lines; a helper can follow if Rapidkoi wants one), syncing whole file systems.
229. **Docs**: the spec's `std.fs` table (with the pattern), the milestones table (116), the "Changes since v16" list; CHANGELOG.
230. **Success**: milestone 116, tag `m116`.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- `runtime/fs.c`: `lugha_rt_sync(path, error)`; `RUNTIME_SYMBOLS`; `$os.sync` in `os_signature`, the no-value-slot list in `codegen/net.rs` and `codegen/os.rs`'s routing; `src/std/fs.la` `sync` with docs.
### Must NOT Do
- Decision 228.
### New dependencies
- none.

## BUILT (session 280)
- As planned. `open(O_RDONLY | O_CLOEXEC)` works for folders too, so no `O_DIRECTORY` retry was needed.
- Tests: `tests/fs_time.rs` `sync_writes_files_and_folders_out` (file, folder, the four-step pattern, a missing path, a NUL byte; the runtime calls `fsync`).

## ERROR HANDLING REQUIREMENTS
- `Err` with the system's message for a missing path or a failed `fsync`; `Err` for a NUL byte. Never a panic.

## SECURITY CONSIDERATIONS
- Opens only the path given, read-only, close-on-exec, and closes it on every path; writes nothing.

## TESTS TO WRITE
- [ ] In `tests/fs_time.rs`: `sync` on a file and on a folder is `Ok(())`; on a missing path `Err("No such file or directory")`; a NUL byte `Err`; the four-step pattern leaves the new contents in place.
- [ ] (A real power cut can't be tested; the test checks the calls succeed and that `fsync` is what runs — through the runtime source.)

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] `fs.sync` works on files and folders; narrow tests (`fs_time`, `programs`, lib link tests), fmt, clippy; CHANGELOG.

## VALIDATION
- `cargo test --test fs_time`, `cargo test --lib -- link`, fmt, clippy
