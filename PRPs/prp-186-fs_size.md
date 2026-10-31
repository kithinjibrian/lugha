## FEATURE: `fs.size(path)` — a regular file's size in bytes without opening it; milestone 153 (v17).

**Status:** approved 2026-10-31; proposed the same day, from Rapidkoi's request (its DECISION-015 and PRP-056): it needs to refuse a file over 16 MB (or a local module over 32 MB) before `read_bytes` reads all of it. Discovery: Q1 shape → `fs.size` (not a `metadata` struct); Q2 errors → `Is a directory` for a folder, `not a regular file` for other kinds.

## OBJECTIVE
```lugha
import std.fs;

fun load(path: string): Result<u8[], string> {
    if fs.size(path)? > 16 * 1024 * 1024 {
        return Err("This file is over 16 MB");
    }
    read_bytes(path)
}
```
A 4 GB file chosen by mistake is refused after one `stat(2)`, without reading a byte.

## CONTEXT
- `src/std/fs.la` (`exists`/`is_dir`/`is_file` over `$os.stat_kind`), `runtime/fs.c` (`lugha_rt_stat_kind`, `fs_has_nul`, `fs_fail`), `src/check/float_ops.rs` (`os_signature`), `src/codegen/os.rs` (any `file_*` op already routes to `net_call`'s signature-driven lowering, with a value slot), `src/link/mod.rs` (`RUNTIME_SYMBOLS`).
- Spec: "The standard library", `std.fs`'s table; §11 milestones table (row 153); "Changes since v16".
- Decisions: none open. Follows milestones 115/116 (`$os.*` built-ins, `Err` with the system's message).

## DECISIONS (draft decisions, numbered on landing)
1. **`fs.size(path: string): Result<i64, string>`**: `stat(2)`'s `st_size`, following symbolic links (as `is_file` does). An empty file is `Ok(0)`.
2. **Only regular files**: a folder is `Err("Is a directory")` (`strerror(EISDIR)`, what `remove_file` and the read intrinsics already say); a FIFO, socket or device is `Err("not a regular file")`. A caller never mistakes a folder's size for a file's.
3. **Errors**: `stat` failing is `Err` with the system's message (`No such file or directory`, `Permission denied`); a NUL byte is `Err("path contains a NUL byte")`. Never a panic.
4. **Built-in**: `$os.file_size(path)` → `lugha_rt_file_size(path, &out, &error)`; the `file_` prefix routes it through the existing lowering, so codegen needs no new case.
5. **Not**: a `metadata` struct, modification times, `lstat` (a link's own size), a size on an open `File`. `fs.size` can sit beside a `metadata` later if a need arrives.
6. **Docs**: `std.fs`'s table, §11 row 153, "Changes since v16", CHANGELOG; tag `m153`.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- `runtime/fs.c`: `lugha_rt_file_size` (stays under 500 lines); `RUNTIME_SYMBOLS`; `"file_size"` in `os_signature`; `size` in `src/std/fs.la` with a `/** … */` comment.
### Must NOT Do
- Decision 5. Open or read the file.
### New dependencies
- none.

## ERROR HANDLING REQUIREMENTS
- Decisions 2–3.

## SECURITY CONSIDERATIONS
- `stat` only reads metadata; nothing is opened. The path is the program's own input, checked for NUL as elsewhere. No `unsafe`, no process, no `git`.

## TESTS TO WRITE
- [ ] `tests/programs/m153/size.la` (+ `.stdout`, `.exit`, `.scratch`): a written file's size; an empty file is 0; through a symbolic link is the target's size (if a link can be made from Lugha; otherwise covered in `tests/fs_time.rs`); a folder `Err(Is a directory)`; a missing path `Err(No such file or directory)`; a NUL byte `Err`.
- [ ] A large file: a sparse 5 GB file (`truncate` via `std.process` or the test harness) gives `5368709120`, proving sizes past 32 bits; in `tests/fs_time.rs` if it needs Rust to make the file.

## ROLLBACK PLAN
- Revert the merge.

## ACCEPTANCE CRITERIA
- [ ] `fs.size` behaves as decisions 1–3; `cargo test --test programs` (m153), `--test fs_time`, `cargo test --lib -- link`; fmt and clippy clean; spec, CHANGELOG updated.

## VALIDATION
- `cargo test --test programs -- m153`, `cargo test --test fs_time`, `cargo test --lib -- link`, `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`

## BUILT (2026-10-31)
- As planned; `$os.file_size` needed no codegen change (the `file_` prefix routes it). `tests/programs/m153/size.la` covers every case, the symbolic link, FIFO and sparse 5 GB file made with `std.process` (`ln`, `mkfifo`, `truncate`), so `tests/fs_time.rs` is unchanged.
