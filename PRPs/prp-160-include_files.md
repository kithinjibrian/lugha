## FEATURE: `include_bytes("path")` and `include_str("path")` put a file's contents into the program at compile time; milestone 128 (v17).

**Status:** proposed 2026-10-22 (session 306), from Rapidkoi's request (its session 157, DECISION-038): ship the signed new-tab bundle (`newtab.rkb`, tens of KB) inside `./main`, and later its SVG icons, without pasting bytes into source files. Discovery: (1) two built-ins usable anywhere, constant expressions; (2) paths stay inside the package's folder, symlinks resolved; (3) two codes, E0447 (can't include) and E0448 (not UTF-8), 16 MiB cap; (4) stored as read-only globals, `u8[]` marked shared; (5) read by the checker, so `check` and the language server report errors; hover shows the size, go-to-definition opens the file.

## OBJECTIVE
A program writes `const NEWTAB: u8[] = include_bytes("../../apps/newtab.rkb");` and the binary carries the file's bytes in read-only data, with no startup cost. A path that is empty, absolute, outside the package, missing, not a regular file or over 16 MiB is E0447 at the literal; `include_str` on bytes that aren't UTF-8 is E0448. Editing the file and rebuilding picks up the new bytes.

## CONTEXT
- Constants (milestone 109): `src/check/consts.rs` (constant expressions, E0445), `src/codegen/consts.rs` (`lugha_const_` globals, `lugha_const_init`).
- Intrinsics recognised by name: `skip`, `flush` (E0302 for a user function of that name), `src/check/test_fns.rs`, `src/check/builtins.rs`.
- String literals as constant globals: `src/codegen/runtime.rs`; the array header and shared bit: `src/codegen/cow.rs`, `heap.rs`.
- Module loading: `src/modules/mod.rs` (`Module { file, package, … }`), `layout.rs` (`LoadEnv`, the manifest lookup), `packages.rs` (package folders).
- Language server: `src/driver/lsp/hover.rs`, `definition.rs`, `src/check/partial.rs` (the `Recorder`).
- Spec v17 §2 (constants), §6 (packages, "What runs"), §7 (heap layout, copy-on-write), §9 (diagnostics). Open decisions: none.

## DECISIONS (draft decisions 317–326)
317. **Shape**: two intrinsics, `include_bytes(path): u8[]` and `include_str(path): string`, whose one argument is a string literal: no interpolation, no `format`, no constant name. Anything else, or another argument count, is E0447 ("expects a string literal path"). They work in any expression and are constant expressions (an exception to E0445's no-calls rule), so `const` accepts them. A user function, method-less name or import alias named `include_bytes`/`include_str` is E0302; the v17 compatibility line lists them like `skip` and `flush`.
318. **Paths**: relative to the folder of the `.la` file that names them; separators are `/`; not empty; no `\`, NUL or absolute path. `.` and `..` segments are allowed, as long as the result stays inside the boundary.
319. **The boundary** is the package's folder: the folder of the `lugha.toml` that governs the file's package (for the root package, the program's manifest, found as today), or the main file's folder when there is none. Both the boundary and the target are canonicalised (`std::fs::canonicalize`, symlinks resolved) and the target must lie under the boundary and be a regular file. So `..`, absolute paths and symlinks that lead out are refused. Git packages already check out symlinks as plain files (`core.symlinks=false`). Bundled `std` and the prelude can't use it (E0447: "only a package's own files").
320. **E0447** "can't include": at the path literal, naming the path and the reason: not a string literal; empty; absolute or containing `\`; outside the package folder (naming the folder); no such file; not a regular file; can't be read (the OS message); over the 16 MiB (16 777 216 bytes) limit (giving the size). **E0448**: `include_str` on bytes that aren't UTF-8, giving the offset and value of the first bad byte, with help to use `include_bytes`. Each gets a page in `docs/errors/`.
321. **Reading**: the checker reads each distinct file once per check and keeps the bytes in `Checked` (a side table keyed by the call's `ExprId`). Codegen uses those bytes and never reads the file again, so what was checked is what is built. There is no total cap beyond the per-file one.
322. **Storage**: like a string literal, a private constant global with the heap layout. `include_str` → `{ i64 len, [len x i8], i8 0 }`. `include_bytes` → `{ i64 len, i64 cap, [len x i8] }` with `cap` = `len` and the shared bit set, so copy-on-write clones before any write (`push`, `b[0] = …`, `mut` passing) and nothing writes to read-only memory. One global per distinct file and kind per module (`lugha_inc.<n>`, private), no copy at startup; a `const` holding it stores the pointer in `lugha_const_init`.
323. **Rebuilds**: lughac has no cache of compiled Lugha, so every `build`/`run`/`test` rereads the file; nothing to invalidate. The C object cache (milestone 127) is unaffected.
324. **Tooling**: `check`, `build`, `run`, `test` and `lughac lsp` (offline, reading only) all report E0447/E0448. Hover on the call shows its type and the file, relative to the package folder, with its size in bytes (`apps/newtab.rkb — 41213 bytes`). Go-to-definition on the path opens the file. `lughac fmt` formats the call like any call. `--emit=ir` shows the globals.
325. **Not**: globs or folders (`include_dir`), `include_str` of other encodings, a compile-time hash, files outside the package for any reason, an opt-out of the cap, a new crate.
326. **Docs**: spec §2 (the intrinsics, constant expressions), §4 (types), §6 ("What runs": lughac also reads the files a package names in `include_*`, never runs them), §7 (the globals), §9 (E0447/E0448), the compatibility line; Draft (decisions, milestone 128 row); CHANGELOG (Added); CLAUDE.md tree. **Success**: a `tests/programs/m128/` directory case embedding a binary file and a text file prints their lengths, bytes and text; milestone 128, tag `m128`.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- `src/modules/`: record each module's package folder (canonical) so the checker knows the boundary.
- `src/check/includes.rs`: recognise the intrinsics, validate the literal, resolve and read the file, E0447/E0448, the side table; count them as constant expressions in `consts.rs`; E0302 for the names.
- `src/codegen/includes.rs`: the globals; the `u8[]` one marked shared.
- `src/check/partial.rs` + `src/driver/lsp/hover.rs`, `definition.rs`: hover size and go-to-definition.
- Error pages `docs/errors/E0447.md`, `E0448.md` (checked by `tests/explain.rs`).

### Must NOT Do
- Never follow a path out of the package folder, never read a file the program doesn't name, never run anything.
- Never reread the file in codegen (decision 321).
- Never let a write reach the read-only global (the shared bit must be set).
- No new dependency.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- Every failure to resolve or read a path is a `Diagnostic` (E0447), never a panic or an internal error; non-UTF-8 text is E0448. The checker continues after either.
- `canonicalize` failing on the boundary itself (a removed folder mid-build) is E0447 with the OS message.

## SECURITY CONSIDERATIONS
- **New external input**: lughac now reads files a package names, beyond `.la` files and `lugha.toml`. A git package can read only files within its own checkout; the boundary check uses canonical paths, so `..` and symlinks can't escape. A race (a symlink swapped between the check and the read) is possible on a hostile local filesystem; to narrow it, the file is opened once, and its canonical path checked through that open handle (`/proc/self/fd` isn't portable, so: canonicalise, open, re-canonicalise, compare) before reading. Worth the user's look.
- The 16 MiB cap bounds memory per file. A program naming many files can still use a lot of memory, as one with huge sources can.
- No file contents are logged; diagnostics show paths and sizes only.

## TESTS TO WRITE
- [ ] `tests/programs/m128/`: a directory case with `data/blob.bin` (bytes 0–255) and `data/hello.txt`: a `const` and an in-function use of each, prints `len()`, a few bytes, the text; `let mut b = BLOB; b[0] = 9u8;` leaves `BLOB[0]` unchanged; `b.push(1u8)` works; a sibling module including relative to its own folder; an empty file gives `0` and `""`.
- [ ] `tests/programs/m128/` rejected cases (`.stderr`, exit 1): each E0447 reason (non-literal, empty, absolute, `\`, `..` out of the package, missing, a folder), E0448 on invalid UTF-8, E0302 for `fun include_str`.
- [ ] `tests/includes.rs` (temp folders through lughac): a symlink inside the package pointing out is E0447; one pointing inside works; a package's module includes from its own folder, not the program's; a file over 16 MiB is E0447 (sparse file); a changed file rebuilds with the new bytes; `check` reports the errors; `--emit=ir` shows a constant global with the shared bit.
- [ ] Unit tests in `src/check/includes.rs` for the path rules (empty, absolute, `\`, NUL) and the UTF-8 offset message.
- [ ] `tests/lsp_hover.rs`: hover on `include_bytes(...)` shows the size; `tests/lsp_definition.rs`: definition opens the file.
- [ ] Existing `programs`, `cli`, `modules`, `fmt_corpus`, `explain`, `lsp*` pass.

## ROLLBACK PLAN
- Branch to return to: `main`; the work lives on `prp-160-include_files` until merged. Nothing persists outside the repository.

## ACCEPTANCE CRITERIA
- [ ] Rapidkoi's example `const NEWTAB: u8[] = include_bytes("../apps/newtab.rkb");` builds and the bytes come out equal (checked with an equivalent program in m128)
- [ ] Every E0447 reason and E0448 tested; symlink escapes refused
- [ ] Tests above pass; `--lib`, `programs`, `cli`, `modules`, `explain`, `lsp_hover`, `lsp_definition`, `includes` run; clippy, fmt clean; no file over 500 lines
- [ ] Spec, Draft, CHANGELOG, CLAUDE.md updated; the user's review of the file-reading security note

## VALIDATION
- `cargo fmt --check`; `cargo clippy --all-targets -- -D warnings`
- `cargo test --lib`; `cargo test --test programs --test includes --test cli --test modules --test explain --test lsp_hover --test lsp_definition`
