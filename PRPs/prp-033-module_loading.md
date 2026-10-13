## FEATURE: Modules front end — `private`/`public` syntax, loading every imported file, and diagnostics that name their file.

**Status:** merged into `main` 2026-10-13 — session 41
**Milestone:** 10 (first of two PRPs: **033 front end**, 034 names, visibility and symbols)
**Spec:** `Language v2 Draft.md`:
- decision 9 (visibility);
- §2 (keywords);
- §3 (`visibility`, `field`, `impl` grammar);
- §6 (modules, root, E0312, E0314);
- §9 (driver: `"file"`, `--emit` across modules).

These fold into `Language v2 Specification.md`.
**Decisions:**
- MEMORY 34 (gate, v2 AST), MEMORY 37 (visibility);
- MEMORY 13 (diagnostics rendering), MEMORY 30 (runner layout);
- architecture rules 2, 3 and 5.

## OBJECTIVE
`lughac` reads a program that spans several files:
- it loads the main file and every module it imports, directly or through others, with cycles allowed;
- import errors are reported at the import (E0312, E0314, E0319);
- every diagnostic is shown against the right file;
- `--emit=tokens|ast` shows every module;
- `private` and `public` replace `pub` in the syntax.

The checker isn't touched: a program that imports anything still stops at the gate with "not implemented yet: modules (milestone 10)" until PRP-034.

## CONTEXT

- **Starting state:**
  - The driver reads one file. `Span` is a byte range into that file.
  - `pub` is a keyword (PRP-030), and `import` parses but is gated.
  - The test runner treats every `.la` file as its own program.
- **Ending state:**
  - **Loading:** a new library stage, `src/modules.rs`, loads the module graph. Files share one offset space.
  - **Driver:** it renders against a source map of all files.
  - **Visibility syntax:** `private` and `public` are parsed on items, fields and methods.
  - **Tests:** the runner understands multi-file cases.
- **Related existing code:**
  - `src/{span,diagnostic,lib}.rs`;
  - `src/lexer/{token,mod}.rs`;
  - `src/parser/{import,item,decl,describe,recover}.rs`, `src/parser/sexp/`;
  - `src/ast/{mod,header}.rs`;
  - `src/driver/{mod,pipeline,source,render,json}.rs`, `src/driver/gate/`;
  - `tests/support/runner/`, `tests/{cli,parser}.rs`.
- **Open decisions that must be resolved first:** none.

### Amendments during implementation (session 41)
- **Directory layout:** `src/modules/` is a directory (`mod.rs`, `resolve.rs`, `tests.rs`) rather than one file, to stay under 300 lines.
- **Parameters and imports:** a visibility on a parameter, or before `import`, is reported once at the keyword, and parsing continues.
- **Offsets:**
  - each file's base is the previous base plus its length plus 1, so a span at a file's end can't be mistaken for the next file's start;
  - JSON offsets are local to their file, which keeps single-file output identical.
- **`--emit=tokens`** re-lexes each loaded module for file-local lines. Only lex errors (`E01xx`) stop it, so a module that doesn't parse still shows its tokens. Modules it can't reach because of a parse error aren't shown.
- **Approved split:** `driver/mod.rs` (313 lines) moved `build`, `tokens` and `header` verbatim to `driver/build.rs`. `opt_level` stays in `mod.rs`, because clap's argument parser uses it.
- **Runner:** the directory-case logic lives in a new `runner/dirs.rs`, keeping `discover.rs` under 300 lines.
- **Tests:**
  - **Where the CLI tests went:** the multi-module tests are in a new `tests/modules.rs` (one file per area), because `tests/cli.rs` was near the limit.
  - **Test order:** they, and the source-map, render and JSON tests, were written after the driver code. The loader, parser, lexer, gate and runner tests were written first and failed first.

### Discovery answers (session 41)
1. **M10 split:** two PRPs. This one is the front end; PRP-034 covers names, visibility and symbols, and completes milestone 10.
2. **Spans:** one offset space.
   - Each loaded file gets a base offset after the previous file, and its tokens' spans include it.
   - `Span`, the AST, the checker, codegen and `Diagnostic` are unchanged. Only the driver maps an offset back to a file, line and column (like rustc's source map).
3. **Multi-file tests:** a case can be a directory such as `tests/programs/m10/two_modules/`, holding `main.la` and its modules at any depth.
   - The expectations sit beside it: `two_modules.stdout`, `.exit` or `.stderr`.
   - `.la` files inside such a directory are modules, not cases.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Keywords and syntax** (draft §2, §3)
- **Lexer:** `pub` is removed; `private` and `public` become keywords.
- **AST:**
  - `public: Option<Span>` becomes `visibility: Option<Visibility>`, where `Visibility { kind: Private | Public, span }`. It sits on `FunDecl` (functions and methods), `ExternDecl`, `StructDecl` and `EnumDecl`.
  - Struct fields become their own `Field { visibility, name, ty }`. Today `Field` is an alias of `Param`.
- **Parser:**
  - a visibility is accepted before `fun`, `extern`, `struct` and `enum` items, before a method in an `impl`, and before a struct field;
  - anywhere else it is E0201, for example on a parameter, a `let`, or `private impl`.
- **Sexp:** `(private fun …)`, `(public struct …)`, and fields as `(private lo i64)`.

**Loading** — new `src/modules.rs`, a library stage between parser and checker (lexer → parser → **modules** → check)
- **API:** `load(main, read)`, where `read` is a closure returning a file's bytes (`io::Result<Vec<u8>>`), so unit tests load from memory.
- **Result:** every module in load order, root first. Each module has:
  - its path segments (empty for the main file);
  - its file path, base offset and text;
  - its `Program`;
  - its lex and parse warnings.

  Errors come back as diagnostics. Only an unreadable file other than a missing one is an internal error.
- **Resolution:**
  - the root is the main file's directory;
  - `import a.b;` loads `<root>/a/b.la` once, however many files import it, so cycles simply work;
  - a file that doesn't exist is **E0312** "module `a.b` not found", at the import, with help naming the path it looked for.
- **Clashes (E0314, at the later import):**
  - the same module imported twice in one file;
  - two imports whose last segments clash;
  - an import whose last segment equals one of the file's own top-level item names.
- **The root:** importing the main file is **E0319** "the main file can't be imported". This is a new code and needs approval: the draft says it's forbidden but gives no code.
- **Per-file errors:** invalid UTF-8 in an imported file is E0110 at its first bad byte. Lex and parse errors in any module are reported against that module's file.
- **Error flow:** load diagnostics are user-program errors (exit 1) and stop the pipeline before the gate, like any stage's errors.

**Driver**
- **Source map:** `Source` becomes a map of loaded files, each with a name, base offset and text. Every failure and report carries it.
  - **Rendering:** codespan gets one file per module, and a label is placed in whichever file contains its span.
  - **Names:** the main file keeps the name given on the command line. Other modules are named by their path relative to the main file's directory, for example `geometry.la` or `shapes/circle.la`.
- **JSON:** `"file"` is the primary span's file. A label in another file gets its own `"file"` key, which only appears when it differs, so single-file output, including the spec's §9 example, is byte-for-byte unchanged.
- **Pipeline:**
  - `parsed` / `front` load all modules;
  - the gate runs over every module;
  - the checker and codegen still receive the root `Program`, which is the whole program whenever nothing is imported, since imports are gated.
- **`--emit=tokens|ast`:** with more than one module, each module's output follows a `; module <name>` line (`main` for the root). A single file prints exactly as today.

**Gate**
- `private` and `public` (items, fields, methods) are gated as `visibility`, milestone 10.
- `import` and qualified names stay gated as `modules`, milestone 10.

**Tests runner** — `tests/support/runner/`
- **Discovery:** a directory with a `main.la` and sibling expectation files (`<dir>.stdout`/`.exit` or `.stderr`) is one case. `.la` files inside it are not cases.
- **Running:** the case is built from `<dir>/main.la`.
- **Malformed cases:** the existing reports gain two: a case directory without `main.la`, and expectations without a case.

**Spec** — `Language v2 Specification.md` (what `lughac` does now)
- **Keywords:** the "Changes since v1.2" sentence becomes "unless it uses `import`, `private` or `public` as a name".
- **§6:** a "Modules (loading)" paragraph covering:
  - file = module, named by its path;
  - the root is the main file's directory;
  - each file is loaded once, and cycles are fine;
  - E0312, E0314, E0319;
  - programs that import still stop at the milestone 10 gate.
- **§9:** diagnostics name their file (`"file"`, and a label's `"file"` when it differs); `--emit` across modules; the new codes.
- **Draft:** E0319 is added to §9.

**Docs**
- CLAUDE.md:
  - the tree: `src/modules.rs`, the runner's directory cases, `tests/programs/m10/`;
  - architecture rule 2's stage list;
  - the KNOWN ISSUES gate line.
- TODO, CHANGELOG, MEMORY decision 38, CONTEXT.

### Must NOT Do
- No name resolution across modules, visibility checking, or module symbols (all PRP-034).
- No package, manifest or search path. Only the main file's directory is searched.
- No change to `Span`'s shape, no `unsafe`, no new dependencies.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- **User errors:** E0312, E0314, E0319, E0110 and lex/parse errors in any module are `Diagnostic` records, exit 1. The loader keeps going, so all modules' errors are reported together.
- **Internal errors:** a module that exists but can't be read (permissions, I/O) is an internal error, exit 2, as for the main file today.
- **Offsets:** they are `usize` sums of file lengths, so they can't overflow on any real input. The sum is checked anyway, and an overflow is an internal error.

## SECURITY CONSIDERATIONS

- **Paths:**
  - import paths come from source text, so the loader joins only identifier segments (the parser guarantees `[A-Za-z_][A-Za-z0-9_]*`) under the root;
  - `..`, absolute paths and separators can't be written, so no import reads outside the main file's directory tree;
  - symlinks inside the tree are followed like any file read today.
- **Bounded work:** each file is read at most once, so a cycle can't loop or read without end.
- **No `unsafe`.**

## TESTS TO WRITE

Unit tests — `src/modules.rs` (in-memory `read`):
- [x] **Loading:** a root importing `a` and `b.c` loads three modules, root first, with increasing bases and spans offset by each base.
- [x] **Cycles and repeats:** `a` ↔ `b` loads each once; a module imported by two files loads once.
- [x] **E0312** with the looked-for path in the help.
- [x] **E0314:** for a repeated import, a last-segment clash, and a clash with an item.
- [x] **E0319:** a module importing `main`'s path.
- [x] **Errors in imported files:** E0110, and lex and parse errors, all at offsets inside that module's range.
- [x] **Internal errors:** a read error other than not-found is internal.

Unit tests — lexer, parser, sexp:
- [x] **Keywords:** `private` and `public` are keywords; `pub` is an identifier again.
- [x] **Visibility:**
  - on functions, externs, structs, enums, methods and fields;
  - E0201 on a parameter, `let`, `private impl`, and `public` before `import`.
- [x] **Unchanged:** the existing sexp outputs, apart from the `pub` cases rewritten.

Unit tests — driver (source map, render, JSON):
- [x] **Line and column:** an offset in the second file maps to its line and column.
- [x] **Labels in other files:** they render under that file.
- [x] **JSON:** a label in another file carries `"file"`; single-file JSON is unchanged.

Integration — `tests/cli.rs`:
- [x] **`--emit=ast`** on a two-module program prints both modules after `; module` lines; a single file prints as before.
- [x] **`check`** on a valid two-module program exits 2 with the milestone 10 gate message.
- [x] **Errors in an imported module** name that module's file in human and JSON output.

Acceptance — `tests/programs/m10/`:
- [x] **`e0312.la`:** a missing module.
- [x] **`e0314/`:** a directory case with a clashing import.
- [x] **`parse_error/`:** a directory case with a syntax error in a module.

The goldens are captured and reviewed.

Runner:
- [x] Malformed directory cases are reported; existing cases are still discovered unchanged.

Regression:
- [x] Every earlier test passes.
- [x] The `pub` tests from PRP-030 become `private`/`public`.

## ROLLBACK PLAN

- Branch `prp-033-module_loading`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] Multi-file programs load with cycles allowed; E0312, E0314, E0319 and per-file errors behave as above; programs that import stop at the gate.
- [x] Diagnostics and `--emit` are file-aware; single-file output is unchanged.
- [x] Spec updated; CLAUDE.md, TODO, CHANGELOG, MEMORY updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `lughac build --emit=ast tests/programs/m10/<dir>/main.la`
