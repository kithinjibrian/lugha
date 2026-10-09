## FEATURE: The `lughac` command line — `build`, `run`, `check`, `--emit`, `-O`, human and JSON diagnostics, and exit codes 0/1/2 — completing milestone 1.

**Status:** implemented 2026-10-09 — session 8 (branch `prp-005-driver`)
**Milestone:** 1 (final PRP; done-when `m1/arith.la` exits 14 through `lughac`)
**Spec:** §9 (CLI, exit codes, diagnostics, JSON format), §10 (rejected-program output), §2 (lexical errors)
**Decisions:** DECISION-006 (codespan-reporting) — outcome amended here as DECISION-011; DECISION-007 (clap); DECISION-008 (test runner)

## OBJECTIVE
`lughac build arith.la && ./arith` and `lughac run arith.la` both exit with 14, and `cargo test` runs the `tests/programs` acceptance suite un-ignored. Errors print in a rustc-like layout via codespan-reporting, or as JSON lines with `--diagnostics=json`. Both come from the same `Diagnostic` records. Exit codes follow §9: 0 success, 1 errors in the program, 2 bad usage or an internal error.

## CONTEXT

- Starting state:
  - `lex`, `parse`, `codegen::emit_object` and `link::link` exist as library calls.
  - `src/main.rs` is an empty `main`.
  - `tests/programs.rs::programs` is `#[ignore]`d.
  - `Diagnostic` has no primary-label text.
  - `src/lexer/mod.rs` is at 299 lines.
- Ending state:
  - `src/driver/{mod,pipeline,source,render,json}.rs` are created; `src/main.rs` calls `lugha::driver::main()`.
  - `Diagnostic` gains `label`.
  - Spec §2, §9 and §10 are amended. DECISIONS.md gains DECISION-011.
  - New `tests/cli.rs`; new `tests/programs/m1/` cases; `programs()` is no longer ignored.
- Related existing code: `src/diagnostic.rs`, `src/parser/sexp.rs`, `src/codegen/mod.rs`, `src/link.rs`, `tests/support/runner/`.
- Open decisions that must be resolved first: none.

### Amendments during implementation (session 8)
- **Default output:** a source file with no extension builds to `<stem>.out`, so `lughac build prog` never overwrites `prog` itself.
- **Boxed failure payloads:** `Failure::Internal` holds a `Box<Report>` and `LoadError::Utf8` a `Box<Diagnostic>`. clippy's `result_large_err` flagged ~190-byte error variants on every `Result` in the pipeline.
- **`--emit=tokens`** lexes only; it doesn't need the file to parse.
- **Warnings** from the lexer and parser are printed on success, though no stage produces any yet.
- **Golden files:** the two hand-written `.stderr` files (`bad_char`, `unclosed`) matched codespan's output on the first run.

### Discovery answers (session 8)
0. Pre-check: codespan-reporting 0.13 (ASCII) renders E0401 with `  --> ` instead of ` --> `, trailing spaces on the connector line, and a blank line after the diagnostic. It cannot match spec §10 byte-for-byte.
1. Renderer: keep codespan, strip trailing whitespace, and update the spec §10 example to codespan's layout (DECISION-011).
2. Primary-label text: a new `Diagnostic::label: Option<String>` and a JSON `"label"` key after `"span"`.
3. `run` passes the program's exit code through (128+N for signal N). `--emit` prints to stdout and stops: tokens as `line:col Kind`, ast as S-expressions, ir as LLVM text.
4. Edge cases:
   - A missing or unreadable file is exit 2.
   - Invalid UTF-8 is new code E0110, exit 1.
   - `build` writes `./<stem>` unless `-o` is given, with the object file in a temp dir.
   - Internal errors have no code (`"code":null` in JSON).
   - Any file extension is accepted.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**CLI** — `src/driver/mod.rs` (clap derive)
- `lughac build <file> [-o <out>] [--emit=tokens|ast|ir] [--diagnostics=human|json] [-O0|-O2]`
- `lughac run <file> [--diagnostics=…] [-O0|-O2]`
- `lughac check <file> [--diagnostics=…]` — lexes and parses only. The checker joins in milestone 3 and is documented as such.
- `-O` takes `0` or `2` (`-O2` parses as `-O 2`); the default is `0`.
- `lughac spec` is not part of this PRP (milestone 5).
- clap usage errors exit 2, which is clap's default. `--help` and `--version` exit 0.
- `pub fn main() -> ExitCode` reads `std::env::args_os()` and writes to the real stdout and stderr. `src/main.rs` contains only the call.
- `pub fn run(args, stdout: &mut dyn Write, stderr: &mut dyn Write) -> u8` is the testable core. `main` wraps it.

**Pipeline** — `src/driver/pipeline.rs`
- Each command runs read → lex → parse → (codegen → link) and stops at the first stage that reports an error.
- `Failure::Program(Vec<Diagnostic>)` → exit 1.
- `Failure::Internal(InternalError)` → exit 2. It covers:
  - a file that can't be read
  - `CodegenError` (Unsupported, Verify, Emit)
  - `LinkError`
  - I/O errors on temp files
- Warnings are printed and don't change the exit code.
- `build`:
  - Writes the object file into a fresh temp dir, which is removed on drop, then links to `-o <out>` or `./<file stem>`.
  - `--emit=tokens|ast|ir` prints to stdout and stops before writing any files:
    - `tokens`: one per line, `line:col {kind:?}`, without `Eof`.
    - `ast`: `parser::sexp::program`.
    - `ir`: `codegen::emit_ir`.
- `run`:
  - Builds into a temp dir, then runs the executable with inherited stdin, stdout and stderr.
  - Exits with the program's exit code, or `128 + N` if it was killed by signal N.
  - The temp dir is removed afterwards.

**Source loading** — `src/driver/source.rs`
- Reads the file as bytes.
- An I/O error is internal: `cannot read `<path>`: <reason>`, exit 2.
- Invalid UTF-8 is a program error: `E0110 "source is not valid UTF-8"`. Its span is empty, at the first bad byte. Rendering uses the valid prefix as the source text.

**Diagnostic record** — `src/diagnostic.rs`
- Add `label: Option<String>`, the text shown under the primary span, and a builder `with_primary_label(text)`.
- Existing constructors set it to `None`. No change to lexer or parser behaviour.

**Human rendering** — `src/driver/render.rs`
- Converts each `Diagnostic` to a codespan diagnostic, using ASCII characters and no colour:
  - code → `error[E0401]`
  - message → the header line
  - primary span + `label` → the caret line
  - `labels` → secondary labels
  - `help` → a `help: …` note
- Every line has its trailing whitespace stripped.
- Internal errors with a span (`Unsupported`) render the same way with no code. Those without a span print `error: <message>`.

**JSON rendering** — `src/driver/json.rs`
- One object per line on stderr, nothing else. Keys in this order:
  - `severity`, `code` (string or `null`), `message`, `file`
  - `span` (`{start:{line,col,offset},end:{…}}` or `null`)
  - `label` (string or `null`)
  - `labels` (array of `{span, message}`)
  - `help` (string or `null`)
- Lines and columns are 1-based; columns and offsets count UTF-8 bytes (§9).
- Strings are escaped per JSON (`"` `\` and control characters). It is hand-written — no serde.

**Spec**
- §9:
  - The JSON example gains `"label":"expected i32"` after `"span"`.
  - Add text explaining `label`, `"code":null` for internal errors, and `"span":null` when there is no location.
  - Human output is "rustc-like, rendered by codespan-reporting, followed by a blank line".
  - `check` runs the checker from milestone 3.
- §10: the rejected program's human output is updated to the codespan layout (`  --> `).
- §2: add E0110 "Source is not valid UTF-8" to the lexical error table.

**Decisions**
- DECISIONS.md: add DECISION-011 (resolved): "Human diagnostics: codespan-reporting with trailing whitespace stripped; spec example follows codespan's layout". Cross-reference it from DECISION-006. Copy it to MEMORY.md.

**Acceptance tests** — `tests/programs/m1/`
- `arith.la` (exists), expected exit 14.
- `void_main.la`: `fun main() { }`, expected exit 0.
- `div_zero.la`: `fun main(): i32 { 1 / 0 }`, expected exit 132 (128 + SIGILL) — the trap behaviour until milestone 4.
- `bad_char.la`: `fun main(): i32 { 2 @ 3 }`, reject mode. `.stderr` holds the exact human E0101 output.
- `unclosed.la`: `fun main(): i32 { (1 + 2 }`, reject mode. `.stderr` holds the exact E0201 output with the "to match this `(`" label.
- `tests/programs.rs`: remove the `#[ignore]`.

### Must NOT Do
- No type checker; `check` is lex + parse only.
- No `lughac spec`.
- No serde or other new dependencies.
- No colour output — that can come later behind a flag.
- Don't change codegen or link behaviour, and don't touch the lexer. E0110 lives in `driver/source.rs`, because `src/lexer/mod.rs` is at 299 lines.
- Don't special-case codespan text with string replacements, apart from stripping trailing whitespace.

## ERROR HANDLING REQUIREMENTS

- `driver::run` never panics on any input file or arguments. Everything maps to an exit code.
- A failure writing to stdout or stderr (e.g. a closed pipe) is ignored for exit-code purposes. The exit code reflects the compile result.
- Internal errors are exit 2 and never exit 1. A Rust panic inside lughac is a bug; installing a panic hook that maps panics to exit 2 is out of scope.

## SECURITY CONSIDERATIONS

- The file path comes from the user. Only that file is read. Output goes only to `-o`, `./<stem>` or temp dirs that lughac creates and removes.
- `run` executes the program it just compiled, from a private temp dir, with arguments passed via `Command` — never a shell.
- Temp dir names include the pid and a counter. They are created fresh, and creation fails if the dir already exists, so a pre-planted directory is never reused.
- No `unsafe`.

## TESTS TO WRITE

Unit tests:
- [x] render: a hand-built E0401 diagnostic (primary label + secondary label) renders exactly the new spec §10 text, including the trailing blank line and no trailing spaces.
- [x] render: `help` appears as a `help:` note; an internal error without a span prints `error: …`.
- [x] json: the same diagnostic serialises exactly to the new spec §9 example line.
- [x] json: escaping of `"`, `\`, `\n` and control characters; `"code":null` and `"span":null` cases.
- [x] source: line/col for offsets on line 1, after `\n`, and after multi-byte characters (byte columns); E0110 position for invalid UTF-8.

Integration — `tests/cli.rs` (runs the real binary, temp dirs removed on drop):
- [x] `build` writes `./<stem>` (in a temp cwd) and `-o` overrides it; the binary exits 14.
- [x] `run` passes the exit code through (14) and maps SIGILL to 132.
- [x] `check` on a valid file → exit 0 and no output; on a lexer error → exit 1 with E0101 on stderr.
- [x] `--emit=tokens` first lines are `1:1 Fun`, `1:5 Ident("main")`; `--emit=ast` prints the milestone 1 S-expression; `--emit=ir` contains `define i32 @main()`. No files are written.
- [x] `--diagnostics=json` gives exactly one JSON line per diagnostic, and nothing else on stderr.
- [x] Missing file → exit 2, `cannot read`.
- [x] Invalid UTF-8 → exit 1, E0110.
- [x] Unsupported construct (`let`) → exit 2, `not implemented yet: `let` statements (milestone 2)`, and in JSON `"code":null`.
- [x] No arguments / an unknown flag → exit 2.

Acceptance (`tests/programs`):
- [x] `programs()` runs un-ignored, and all five `m1/` cases pass.

## ROLLBACK PLAN

- Branch `prp-005-driver`, merged into `main` on acceptance.
- To abandon: delete the branch. No migrations or runtime state. The `#[ignore]` returns with the revert.

## ACCEPTANCE CRITERIA
- [ ] Every test above exists and passes; `cargo test` includes the un-ignored `programs()`.
- [ ] `lughac run tests/programs/m1/arith.la` exits 14 — **milestone 1 done** (spec §11).
- [ ] Spec §2, §9 and §10 updated; DECISION-011 recorded and copied to MEMORY.md.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass.
- [ ] Every `pub` item documented; functions returning `Result` document `# Errors`.
- [ ] CLAUDE.md COMMANDS and FILE ORGANIZATION, CHANGELOG.md and TODO.md updated.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo run -q -- run tests/programs/m1/arith.la; echo $?` → `14`
