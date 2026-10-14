## FEATURE: Bytes and indexes — binary file I/O, `to_bytes`, `string_from_utf8`, `read_stdin_bytes`, any-integer indexes; milestone 45.

**Status:** approved 2026-10-14 — session 96. This is part of the user's "in one go" (milestones 45–49), merged once CI is green.
**Milestone:** 45 (one PRP; tag `m45` at merge; `Language v14 Specification.md` begins)
**Spec:** `Language v14 Draft.md`:
- decision 7, and the choices on bytes and indexes;
- §2 (intrinsic names);
- §9;
- §11 (milestone 45).

**Decisions:** MEMORY 91 (v14), MEMORY 50 (v4 I/O).

## OBJECTIVE
A program writes bytes to a file, reads them back, decodes them with `string_from_utf8`, rejects invalid UTF-8 with its byte offset, and indexes with `u8`, `u32` and `i32`, printing the expected results. That is the §10 program, written here with its output.

## CONTEXT

- **Starting state:**
  - text I/O through `lugha_rt_read_file`/`lugha_rt_write_file` (`codegen/io.rs`, `runtime/input.c`);
  - indexes must be `i64` (`check/access.rs::index`, `codegen/heap.rs::element_place`).
- **Ending state:** byte I/O, byte/text conversion, and any integer index.
- **Related existing code:**
  - `src/check/{input,access,builtins}.rs`;
  - `src/codegen/{io,function,heap,methods}.rs`;
  - `runtime/`, `src/link.rs`.
- **Open decisions that must be resolved first:** none.

### Design
- **Checker:**
  - `INPUT` gains `read_bytes(string): Result<u8[], string>`, `write_bytes`/`append_bytes(string, u8[]): Result<i64, string>`, `read_stdin_bytes(): u8[]` and `string_from_utf8(u8[]): Result<string, string>`; these become intrinsic names (E0302 to declare);
  - `builtins.rs` gains `s.to_bytes(): u8[]` on `string`;
  - `access.rs::index` accepts an index of any integer type (an unannotated literal is `i64`), and E0403 for anything else.
- **Codegen:**
  - `io.rs` lowers the five intrinsics as `read_file`/`write_file` are, with out-slots and `result`;
  - `methods.rs` lowers `to_bytes` to `lugha_rt_str_to_bytes`;
  - `heap.rs::element_place` reads the index at its own type and sign- or zero-extends it to `i64` before the existing bounds check. A `u64` above `i64.MAX` becomes negative and fails the check; the panic shows that negative value, noted in §12.
- **Runtime** (new fragment `runtime/bytes.c`, joined last):
  - `lugha_rt_read_bytes` (through `lugha_rt_read_file`);
  - `lugha_rt_write_bytes(path, bytes, append, count, error)`;
  - `lugha_rt_read_stdin_bytes`;
  - `lugha_rt_str_to_bytes`;
  - `lugha_rt_utf8_check(bytes, out, error)`, which rejects overlong forms, surrogates, code points above U+10FFFF and truncated sequences, with `invalid UTF-8 at byte N`.

  `RUNTIME_SYMBOLS` grows to 43.
- **Housekeeping:** the approved verbatim move of `Checked` into `check/checked.rs` landed first, on its own.
- **Programs not using these** give byte-identical IR. An `i64` index lowers exactly as before.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** everything above.
- **Spec:** start `Language v14 Specification.md` from v13 (in progress: milestone 45); point `lib.rs` and `tests/cli.rs` at it. Fold in:
  - the changes summary, with the v14 compatibility notes that apply so far;
  - the intrinsics table;
  - `to_bytes`;
  - indexing (§4 and §5);
  - the runtime table;
  - the §10 program;
  - §11.
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 92, CONTEXT. The tag `m45` is added at the merge.

### Must NOT Do
- **No streams, directories or processes.**
- **No unchecked byte→string conversion.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0403:** a non-integer index; wrong argument types.
- **E0405:** the wrong argument count.
- **E0302:** declaring one of the new intrinsic names.
- **At run time:**
  - file errors come back as `Err(system message)`;
  - invalid UTF-8 gives `Err("invalid UTF-8 at byte N")`;
  - an out-of-range index panics as before.

## SECURITY CONSIDERATIONS
- **File I/O:** paths are checked as `read_file`'s are (`path_ok`: no embedded NUL).
- **Buffers:** every buffer is sized from known lengths with overflow checks.
- **UTF-8:** the checker is strict: no overlong forms, no surrogates, nothing above U+10FFFF.
- No `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — checker:
- [x] The intrinsics' types; `to_bytes`; E0302 for a user `read_bytes`.
- [x] An index of type `u8`, `u32`, `i32` or `u64` checks; an `f64` or `bool` index is E0403.

Unit tests — codegen: a `u8` index is zero-extended (`zext i8`) and an `i32` index sign-extended before the bounds check.

Acceptance — `tests/programs/m45/`:
- [x] `bytes.la` (with a `.scratch` folder): the §10 program.
- [x] `more.la`:
  - an empty file;
  - every UTF-8 error kind;
  - `append_bytes`;
  - a negative `i32` index panicking;
  - a string index with a `u16`.
- [x] `stdin.la` with `.stdin`: `read_stdin_bytes` length.
- [x] `index_panic.la`: `xs[-1 as i32]` panics with exit 101.
- [x] The §10 program runs from the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-074-bytes_and_indexes`, merged into `main` after CI passes; tag `m45`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m45/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] `Language v14 Specification.md` begins; docs updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m45/bytes.la
```

## AMENDMENTS (during build, session 96)

- **The approved split of `check/mod.rs`** was committed directly to `main`, before this branch: a slip in procedure, with no behaviour change, and checked by the full suite.
- **`widen_index`** lives in `codegen/cast.rs`: adding it to `codegen/heap.rs` took that file to 309 lines.
- **Two earlier `check/access.rs` tests changed:**
  - `s[1.5]` is now E0403 (the index is checked without an expected type);
  - an `i32` index is now valid, so its error case uses a `bool`.
- **The runtime fragment** also had to be added to `RUNTIME_SOURCE`, as in milestone 44.
- **IR:** the 157 earlier programs that compile give byte-identical IR. `-O2` was checked by hand for `m45/bytes` and `m45/more`.
- **No other file splits.**
