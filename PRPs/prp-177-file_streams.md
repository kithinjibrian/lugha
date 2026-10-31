## FEATURE: File streams in `std.fs`: a `File` handle from `open`, `create` and `append`, with `read`, `read_line`, `write`, `write_text` and `close`; milestone 146, v17.

**Status:** approved 2026-10-31 ("build"); proposed 2026-10-31, from TODO.md ("File streams: open a file, read it line by line or in chunks, write incrementally, close"), after a discovery of 4 questions, each answered with the recommended option: (1) one `File` type in `std.fs`, shaped like `net.Conn`; (2) writes unbuffered (`write(2)` until every byte is out, no `flush`); (3) `read_line(): Result<Option<string>, string>` with stdin `read_line()`'s line rules, UTF-8 checked, no length limit, no iterator yet; (4) modes fixed at opening, clear `Err`s, `O_CLOEXEC`, blocking I/O documented, no seeking or read-write mode.

## OBJECTIVE
```lugha
import std.fs;

fun main(): Result<(), string> {
    let out = fs.create("notes.txt")?;            // truncates or makes it (0666 minus the umask)
    out.write_text("first\r\nsecond\n")?;
    out.write("last".to_bytes())?;               // no final newline
    out.close();

    let log = fs.append("notes.txt")?;
    log.write_text("!\n")?;
    log.close();

    let f = fs.open("notes.txt")?;
    while true {
        match f.read_line()? {
            Some(line) => println("[{line}]"),    // [first] [second] [last!]
            None => {
                break;
            }
        }
    }
    println(f.read(10));                          // Ok([]): at the end
    println(f.write_text("x"));                   // Err("not open for writing")
    f.close();
    f.close();                                    // fine
    println(f.read(10));                          // Err("closed")
    Ok(())
}
```
A program can now read a file far larger than memory line by line or in chunks, and write output piece by piece.

## CONTEXT
- Read first: `src/std/fs.la`, `src/std/net.la` (`Conn`: `fd: Ref<i64>`, idempotent `close`, `Err("closed")`), `src/std/http_wire.la` (`Reader.line`: a buffered line reader over `read`), `runtime/fs.c` (`fs_fail`, `fs_has_nul`), `runtime/input.c` (stdin `read_line`'s rules), how `$os` built-ins are typed (`check/float_ops.rs`, `os_signature`) and lowered (`codegen/os.rs`, `codegen/net.rs`'s `net_call`), `link::RUNTIME_SYMBOLS`.
- Changes: `src/std/fs.la` (`File`, `open`, `create`, `append`, the methods); a new runtime fragment `runtime/file.c` (~90 lines; `fs.c` keeps paths, `file.c` keeps descriptors) and its place in `RUNTIME_PARTS`; four `$os` built-ins (below) in `check/float_ops.rs` and `codegen/os.rs` and `codegen/net.rs`; `link::RUNTIME_SYMBOLS`.
- Spec: the `std.fs` section (spec line ~1198: "a file's contents stay with the intrinsics" becomes "whole files with the intrinsics, streams with `File`"), §9's runtime table, §11 row 146, "Changes since v16"; the v17 Draft gains the milestone.
- Decisions: none open. Follows DECISION 123 (`std.fs`: system messages, NUL paths) and the `net.Conn` shape (decision 174's green threads: blocking calls documented, as name lookup is). No dependency; not security-sensitive (no process, git, `cc`/`pkg-config`, credentials, `unsafe`); the outside input is the path and the file's bytes, both validated (NUL, UTF-8).

## REQUIREMENTS

### Must Do
- **`public struct File`** (fields private): the descriptor in a `Ref<i64>` (−1 once closed), its mode (reading or writing), the read buffer and the line count in `Ref`s, so every copy of a `File` shares one stream, as `Conn`'s copies do.
- **`open(path: string): Result<File, string>`** for reading (`O_RDONLY`); **`create(path)`** for writing (`O_WRONLY|O_CREAT|O_TRUNC`); **`append(path)`** for adding to the end (`O_WRONLY|O_CREAT|O_APPEND`). All with `O_CLOEXEC`; created files get 0666 minus the umask, as `write_file`'s do. `Err` with the system's message (`No such file or directory`), or `path contains a NUL byte`.
- **`read(self, max: i64): Result<u8[], string>`**: up to `max` bytes (1 to 16777216, else `Err("max must be from 1 to 16777216")`), first from what `read_line` has buffered, else one `read(2)`; `[]` at the end of the file. `EINTR` is retried.
- **`read_line(self): Result<Option<string>, string>`**: the next line without its `\n` and one `\r` before it; a last line without `\n` is still returned; `None` at the end. Reads in 64 KiB chunks; no length limit (as `read_file`). Invalid UTF-8 is `Err("line N is not valid UTF-8")`, lines counted from 1; the bad line is consumed, so the next call goes on.
- **`write(self, bytes: u8[]): Result<(), string>`** and **`write_text(self, s: string): Result<(), string>`**: every byte, straight to the kernel (`write(2)` looped over short writes and `EINTR`); nothing buffered, so nothing is lost on a panic or a forgotten `close`.
- **`close(self)`**: closes the descriptor; every copy is closed; closing twice is fine. The collector never closes a `File` (documented, as for sockets).
- Wrong-mode calls: `read`/`read_line` on a writing `File` is `Err("not open for reading")`; `write`/`write_text` on a reading one `Err("not open for writing")`. Any call after `close` is `Err("closed")`.
- Documented: reads and writes block the OS thread while they run (quick for disk files; a FIFO or terminal stalls the other green threads).
- Nothing panics on any input. Milestone 146; tag `m146`.

### Must NOT Do
- Seeking, a position or size, read-write mode, locking, buffered writes or `flush`, a `lines()` iterator, polling pipes and FIFOs as green-thread waits: out of scope (discovery Q3, Q4).
- Change `read_file`, `write_file`, `append_file` or the stdin intrinsics.
- Let the collector close descriptors (no finalizers: a copy could still be in use).

## BUILT — milestone 146
- As planned. `runtime/file.c` (84 lines) is a new fragment after `fs.c` (reusing `fs_fail`, `fs_has_nul`); the four `lugha_rt_file_*` are in `link::RUNTIME_SYMBOLS`; `codegen/os.rs` sends `file_*` through `net_call`, with `file_write` among the success-only calls.
- `File` holds `fd`, `buf`, `pos` and `line` in `Ref`s and `writing` as a plain `bool`; `read_line` works on a local copy of the buffer (copy-on-write, so `get` is free), keeps its state before each read so a failed read loses nothing, and drops the consumed prefix only when it is over half the buffer: linear overall. 200 000 lines read back in ~0.4 s at `-O0`.
- Tests: `tests/programs/m146/stream.la` (the OBJECTIVE plus copies sharing a stream and wrong-mode, closed and re-create cases), `chunks.la`, `errors.la`; unit test `file_streams_take_descriptors_v17` in `check/float_ops.rs`. Lugha has no `u8` literal suffix and `break` needs a block in a `match` arm: the OBJECTIVE is corrected to match.

## DESIGN
- **Built-ins** (typed in `os_signature`, lowered through `codegen/os.rs` and `net_call` like `rename`):
  - `$os.file_open(path: string, mode: i64): Result<i64, string>` (mode 0 read, 1 create, 2 append) → `lugha_rt_file_open(path, mode, out, error)`.
  - `$os.file_read(fd: i64, max: i64): Result<u8[], string>` → `lugha_rt_file_read`, buffer from `lugha_rt_alloc`.
  - `$os.file_write(fd: i64, bytes: u8[]): Result<i64, string>` → `lugha_rt_file_write`, looping until done.
  - `$os.file_close(fd: i64)` → `lugha_rt_file_close`.
  The `$os` namespace is reachable only from bundled sources, so a program can't pass an arbitrary descriptor.
- **Line reading** in Lugha inside `std.fs`: a buffer `Ref<u8[]>` and a start index; scan for `\n`, else read another chunk and join; slice off the line, drop `\r`, `string_from_utf8`. Kept linear: the buffer is compacted when the start passes half its length.
- Sizes: `std/fs.la` 114 → ~260 lines; `runtime/file.c` ~90; checker/codegen +20.

## TESTS
- [ ] `tests/programs/m146/stream.la`: the OBJECTIVE (create, append, `read_line` over `\r\n`, a last line without `\n`, `read` at the end, wrong mode, double close, after close).
- [ ] `tests/programs/m146/chunks.la`: a 200 000-line file written piece by piece and read back by `read_line` (count and last line) and by `read(4096)` (total bytes); mixing `read_line` then `read` gives the buffered bytes first; an empty file (`None` at once); a line longer than 64 KiB.
- [ ] `tests/programs/m146/errors.la`: `open` on a missing file, a NUL in the path, `read(0)` and `read(16777217)`, invalid UTF-8 on line 2 then line 3 read fine, `create` in a missing folder.
- [ ] Unit tests for the four built-ins' typing in `check/float_ops.rs`'s tests.

## ACCEPTANCE
- [ ] The OBJECTIVE prints its lines.
- [ ] `cargo test --lib`, `--test programs`, `--test spec`, `--test lsp_docs`; `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`.
- [ ] Every `pub` item documented (`/** */` on the std items); spec, v17 Draft, CHANGELOG, TODO (remove "File streams") updated.
