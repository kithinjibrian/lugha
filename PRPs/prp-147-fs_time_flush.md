## FEATURE: Four small std additions — `fs.rename`, `fs.set_mode`, `flush()` and `std.time`; milestone 115 (v17).

**Status:** approved 2026-10-18 (session 278); proposed in session 277, from the user's brief (session 277): Rapidkoi keeps data on disk safely (write a temporary file, then rename it over the old one) and keeps a secret key file private (mode 0600).

## OBJECTIVE
```lugha
import std.fs;
import std.time;

write_file("state.tmp", data)?;
fs.rename("state.tmp", "state")?;               // replaces `state` atomically
fs.set_mode("key.pem", 0b110_000_000)?;         // 0600: owner reads and writes
print("listening on {addr}");
flush();                                        // shows at once
let started = time.monotonic_ms();
let stamp = time.now_ms();                      // milliseconds since 1970
```

## CONTEXT
- `std.fs` (milestone 65) is Lugha over `$os.*` built-ins (`check/float_ops.rs` `os_signature`, `codegen/os.rs`, `runtime/fs.c`); its functions return `Result<i64, string>` (`Ok(0)`) and refuse paths holding a NUL byte.
- Output goes through C's `stdout`, flushed only when a line is read, at exit, or before stderr output (spec §9); there's no way to flush otherwise.
- `$os.now_ms()` (milestone 111, `lugha_rt_now_ms`) already reads `CLOCK_MONOTONIC` for `std.http`; nothing reads the wall clock.
- **Octal literals: Lugha has none.** It has decimal, `0x…` and `0b…` (milestone 27), and a leading zero is still decimal: `0600` is six hundred. Callers write 0600 as `0b110_000_000` (each group of three binary digits is one octal digit), `0x180` or `384`. The spec and `set_mode`'s docs say so and warn about `0600`. (Adding `0o` literals would be a language change: a separate PRP if wanted.)
- Related: `src/std/fs.la`, `runtime/{fs,input,random}.c`, `src/check/input.rs` (intrinsics), `src/codegen/io.rs`, `src/driver/lsp/signature.rs` (the intrinsics table), spec §5 (built-ins) and the standard-library section.

## DECISIONS (draft decisions 215–224)
215. **`fs.rename(from: string, to: string): Result<(), string>`**: `rename(2)`. On one file system it replaces `to` atomically; across devices it is `Err` with the system's message (`Invalid cross-device link`), never a copy. A path holding a NUL byte is `Err`.
216. **`fs.set_mode(path: string, mode: i64): Result<(), string>`**: `chmod(2)`. `mode` must be from 0 to 4095 (`0o7777`: the permission bits plus setuid, setgid and sticky, as `chmod` takes); otherwise `Err("mode must be from 0 to 4095 (0o7777)")`. A NUL byte or the system refusing is `Err`. Symbolic links are followed, as `chmod(2)` does.
217. **Writing 0600:** no octal literals (context); docs show `0b110_000_000`, and warn that `0600` is decimal 600.
218. **`flush()`**: a built-in, like `print`: `fflush(stdout)`; returns `()`; never fails visibly (a failing `fflush` is ignored, as `print`'s errors are). A program's own function named `flush` becomes E0302 (a v16/v17 program using that name needs renaming: stated in the spec's compatibility line).
219. **`std.time`** (new module):
     - `now_ms(): i64` — milliseconds since 1970-01-01 UTC (`clock_gettime(CLOCK_REALTIME)`): for timestamps; it can jump when the system clock is set.
     - `monotonic_ms(): i64` — milliseconds from an unspecified start (`CLOCK_MONOTONIC`): for measuring intervals; never goes back.
     - Built-ins `$os.wall_ms` (new, `lugha_rt_wall_ms`) and the existing `$os.now_ms`.
220. **Nothing panics**: every failure is `Err` (fs) or impossible (clocks; `clock_gettime` on these clocks can't fail on Linux).
221. **Not**: file locking, other permission APIs (owners, ACLs, `umask`), time zones, formatting dates, sleeping (that's `std.task`).
222. **Security**: `rename` and `chmod` act only on the paths given; NUL bytes refused; no shell; `set_mode`'s range check keeps out file-type bits.
223. **Docs**: spec §5 (`flush` in the built-ins table), the standard-library section (`std.fs`'s two functions, `std.time`), the milestones table (115), the "Changes since v16" list and the compatibility line (`flush`); CHANGELOG; the LSP's intrinsics table.
224. **Success**: milestone 115, tag `m115`.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- `runtime/fs.c`: `lugha_rt_rename(from, to, error)`, `lugha_rt_chmod(path, mode, error)`; `runtime/input.c`: `lugha_rt_flush()`; `runtime/random.c`: `lugha_rt_wall_ms()`; `RUNTIME_SYMBOLS`.
- Checker/codegen: `$os.rename`, `$os.chmod`, `$os.wall_ms`; the `flush` intrinsic (`check/input.rs`, `codegen/io.rs`, `driver/lsp/signature.rs`).
- `src/std/fs.la` (`rename`, `set_mode`), `src/std/time.la` (new), registered in `STD`; doc comments.
### Must NOT Do
- Decision 221; a copy-then-delete fallback for cross-device renames; octal literals.
### New dependencies
- none.

## BUILT (session 278)
- As planned.
- Tests: `tests/fs_time.rs` (rename replacing a file, a missing source, a NUL byte; set_mode to 0600 read back by Rust, -1, 4096, a missing file; a cross-device rename from `/dev/shm`, which ran here; `flush` proven by the program still running when its unflushed-otherwise output arrives — checked to fail without `flush`; both clocks; E0302 for `fun flush`), the LSP intrinsics table test, `programs` (E0312's list with `std.time`).

## ERROR HANDLING REQUIREMENTS
- `rename`, `set_mode`: `Err` with the system's message; `Err` for a NUL byte; `set_mode` `Err` for a mode outside 0–4095.
- `flush`, `now_ms`, `monotonic_ms`: no errors.

## SECURITY CONSIDERATIONS
- Decision 222. The runtime fragments involved (`fs.c`, `input.c`, `random.c`) start no process and open no network connection.

## TESTS TO WRITE
- [ ] `tests/programs/m115/` (scratch folders, as the `std.fs` cases do): `rename` moves a file, replaces an existing one (its old contents gone), fails for a missing source (`No such file or directory`) and for a NUL byte; `set_mode` sets `0b110_000_000` (read back through `stat` in the test runner, or by `fs`'s existing checks) and refuses -1, 4096 and a missing file.
- [ ] A cross-device rename through `tests/cli.rs` or a Rust test when `/dev/shm` and the temp folder are on different file systems (skipped with a note when they aren't): `Err("Invalid cross-device link")`.
- [ ] `flush`: a Rust test runs a program that prints without a newline, flushes, then sleeps; the line is readable from its piped stdout before it exits.
- [ ] `std.time`: `now_ms()` is within a few seconds of the test's own clock; `monotonic_ms()` never decreases across a `task.sleep(20)` and grows by at least 20.
- [ ] E0302 for a program declaring `fun flush()`; the LSP intrinsics test (`flush` listed, in the spec).

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] Each function works as specified; narrow tests (the new cases, `programs`, the intrinsics tests, `lazy_std`, `lsp_docs`), fmt, clippy; CHANGELOG.

## VALIDATION
- `cargo test --test programs --test std_time` (or the chosen test file), `cargo test --lib -- intrinsic`, fmt, clippy
