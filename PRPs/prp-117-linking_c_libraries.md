## FEATURE: Linking C libraries — a `[link]` table in `lugha.toml` naming libraries, search folders, pkg-config packages and C files to compile into the program; milestone 87.

**Status:** approved 2026-10-16 ("yes build", security review included) — built in session 185; proposed in session 184.
**Milestone:** 87 (one PRP; tag `m87` at merge).
**Spec:** `Language v16 Draft.md` decisions 98–104; spec §6 (Packages: the manifest), §8 (C interop: what is linked), §9 (stage 6, the diagnostics table), §11 (row 87).

**Decisions (discovery, session 184):**
- libraries are declared in `lugha.toml`, not in source or on the command line;
- `pkg-config` is part of this PRP (GTK's link line differs between systems);
- the `[link]` tables of every package the program uses apply, not just the program's own;
- packages may ship C files (shims) that lughac compiles into the program;
- link problems the user can fix are diagnostics with new codes, exit 1, not internal errors.
- No open entries in DECISIONS.md apply. DECISION-009 (how `lugha_rt.c` is built) is unchanged: the runtime stays in the one `cc` call.

**Why:** the first step toward GUI programs (and any C library: zlib, sqlite, raylib). Today the link line is fixed at `cc prog.o lugha_rt.c -lgc -lm`, so an `extern fun` outside libc/libm can never link. This PRP does **not** widen what crosses the C boundary (no structs, pointers or callbacks); GTK becomes linkable but stays unusable until callbacks exist (a later PRP). raylib becomes usable through a C shim.

## OBJECTIVE
```toml
# raylib-lugha/lugha.toml — a binding package
[package]
name = "raylib"
version = "0.1.0"

[link]
libs = ["raylib"]
paths = ["vendor/lib"]          # extra -L folders, inside the package
include = ["vendor/include"]    # extra -I folders for this package's C files
c = ["shim/colors.c"]           # compiled with cc, linked into every program using the package
pkg-config = ["gl"]             # flags from the system pkg-config
```
```toml
# game/lugha.toml — a program using it; no [link] needed
[packages]
raylib = { git = "https://github.com/me/raylib-lugha", version = "0.1" }
```
```
$ lughac run game/main.la         # links -lraylib, the shim, and pkg-config's flags
$ lughac build main.la            # with libs = ["raylib"] and no raylib installed:
error[E0331]: linking failed
  --> lugha.toml:2:1
  |
2 | libs = ["raylib"]
  | ^^^^^^^^^^^^^^^^^
  = note: /usr/bin/ld: cannot find -lraylib: No such file or directory
  = help: install the library, or add the folder holding it to `paths`
```
A program with no `[link]` table anywhere builds exactly as today, byte for byte the same `cc` command.

## CONTEXT
- **Starting state:** `src/modules/manifest.rs` (251 lines) parses `[packages]` and `[package]` (via `package_table.rs`); any other table is E0321. `modules::Loaded.manifests` lists every manifest read (some of packages no module imports, read only for version checks). `src/link.rs` (246 lines) runs one `cc <objects> lugha_rt.c -lgc -lm -o out`; `driver/pipeline.rs:235` maps every `LinkError` to an internal error (exit 2). `lughac test` builds through the same `pipeline::build`. `lughac check` and `lughac lsp` never link.
- **Ending state:**
  - **`src/modules/link_table.rs`** (new): `LinkTable { libs, paths, include, c, pkg_config }`, each entry with its span; parsing one `key = ["…", …]` line (a single-line array of double-quoted strings, the subset's only new form). Checks (all E0321 at the line): unknown key; a key twice; a `libs`/`pkg-config` name not matching `[A-Za-z0-9_+.-]+` or starting with `-`; a `paths`/`include`/`c` path that is empty, absolute, holds `\` or `"`, or has a `..` that leaves the package's folder; a `c` file not ending in `.c`. A `paths`/`include` folder or `c` file that doesn't exist is E0322 (as for `[packages]` paths).
  - **`src/modules/manifest.rs`:** a `[link]` header (at most once, any order); `Manifest.link: LinkTable`.
  - **`modules::Loaded`:** `link_inputs()` — the `[link]` tables of the manifests whose package has at least one loaded module (the program's own always), in load order (program first), with each package's folder; empty when none.
  - **`src/link/`** (split from `link.rs`, which reaches the 500-line limit): `mod.rs` (`link`, `LinkError`, `RuntimeFile`, the runtime constants — moved unchanged), `inputs.rs` (`LinkInputs`: the ordered, de-duplicated `-L`/`-l` list and the C files with their per-package `-I`/`-D` flags), `pkg_config.rs` (running `pkg-config`, filtering its output), `c_files.rs` (compiling each C file to an object in the build's temp dir).
  - **Running `pkg-config`** (build, run and test only): `pkg-config --libs <names…>` per package, and `pkg-config --cflags <names…>` only when that package has C files; from an argument list, never a shell; inheriting the environment (`PKG_CONFIG_PATH` is the user's). Allowed in `--libs` output: `-l<name>`, `-L<folder>`, `-pthread`; in `--cflags`: `-I<folder>`, `-D<name>[=value]`, `-pthread`. Anything else is E0329 naming the flag.
  - **Compiling C files:** each with its own `cc -c <file> -I<include…> <cflags…> -o <tmp>/c<n>.o` (not in the runtime's `cc` call, so a package's `-D` never reaches `lugha_rt.c`), then the objects go to the link.
  - **The link line:** `cc prog.o <c objects…> lugha_rt.c -lgc -lm <-L…> <-l…> -o out` — `-L` folders first, then libraries in order of first appearance (the program before its packages, so a static dependency comes after what uses it), each once.
  - **Errors** (`driver/pipeline.rs`): E0329 (pkg-config), E0330 (a C file doesn't compile, cc's output as a note), E0331 (`cc` fails linking when the build has any `[link]` input, cc's output as a note, at the first `[link]` line of the program's manifest, else the first package's). All exit 1. A link failure with no `[link]` input stays an internal error, exit 2.
  - **`lughac check` / `lughac lsp`:** parse and validate `[link]` (E0321, E0322) but never run `pkg-config` or `cc`.
  - **Spec v16:** §6 manifest paragraph and example; §8 "always linked" paragraph; §9 stage 6 and the diagnostics table (E0329–E0331); §11 row 87; the changes list. **Draft:** decisions 98–104, row 87 (written in session 184).
  - **`docs/errors/E0329.md`, `E0330.md`, `E0331.md`** (rule 5); E0321/E0322 pages gain the `[link]` cases.
  - **Docs:** CLAUDE.md (tree: `src/link/`, `modules/link_table.rs`; STACK: pkg-config and C files; anti-pattern 5: changes to how `pkg-config` and `cc` run on package input need the user's security review), README (prerequisites: `pkg-config` only if a manifest uses it), TODO, CHANGELOG, MEMORY, CONTEXT.
- **Related existing code:** `src/modules/{manifest,package_table,packages,paths,mod}.rs`, `src/link.rs`, `src/driver/{pipeline,build,test}.rs`, `src/modules/git.rs` (how an external program is run safely — the model for `pkg_config.rs`).
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The ending state above, test-first.
- The split of `link.rs` into `src/link/` as a pure move first (its own commit), tests passing, before any new behavior.
- Spans for every `[link]` entry, so E0321/E0322/E0329/E0330 point at the right line of the right `lugha.toml`.

### Must NOT Do
- **No raw flags** (`flags = […]`, `-Wl,…`): a package must not be able to add arbitrary options to your link or compile.
- **No shell** for `pkg-config` or `cc`, and no `--libs`/`--cflags` text passed through unfiltered.
- **No absolute or escaping paths** in `paths`/`include`/`c`; system folders come from `pkg-config` or cc's own environment (`LIBRARY_PATH`, `C_INCLUDE_PATH`).
- **Nothing runs during `check` or `lsp`**: opening a folder in VS Code must never compile C or run `pkg-config`.
- No change to the extern type rules (§8) — structs, pointers and callbacks are a later PRP.
- No change to the link of programs without `[link]`.
- No new crate.

### New dependencies
- None. `pkg-config` (or `pkgconf`) is a runtime tool needed only by programs whose manifests use `pkg-config`.

## ERROR HANDLING REQUIREMENTS
- **E0321** (existing): a malformed `[link]` line (see the checks above).
- **E0322** (existing): a `paths`/`include` folder or a `c` file that doesn't exist.
- **E0329** (new) at the `pkg-config` line: `pkg-config` not installed ("install pkg-config or pkgconf"); it exits non-zero (its stderr as a note, e.g. "Package gtk4 was not found"); its output holds a flag outside the allowed set (named).
- **E0330** (new) at the `c` line: a C file fails to compile (cc's stderr as a note).
- **E0331** (new): linking fails while the build has `[link]` input (cc's stderr as a note, a help line).
- `LinkError` gains variants for the above, mapped to diagnostics in `pipeline.rs`; `CcNotFound` and `Io` stay internal errors (exit 2).
- No new `.expect()`.

## SECURITY CONSIDERATIONS
**Needs the user's security review** (anti-pattern 5): lughac starts running two external programs on input that a git package controls.
- **`pkg-config`:** started from an argument list with names validated (`[A-Za-z0-9_+.-]+`, never starting with `-`, so a name can't be an option); output tokens filtered to the allowed flags, everything else rejected, not dropped.
- **C files:** lughac compiles a package's C at build time. Compiling doesn't run the code, but the built program does — no more trust than a package's Lugha code and `extern` calls already need (spec §6 "What runs"). `check`/`lsp` never compile.
- **Paths:** relative, inside the package after normalizing; git checkouts already have `core.symlinks=false`.
- **Temp files:** C objects go in the build's private temp dir, created like `RuntimeFile` (`create_new`).
- No `unsafe`, crypto or credentials; nothing logged beyond cc's and pkg-config's own messages.

## TESTS TO WRITE
- [ ] `link_table.rs` unit: each key parsed; arrays of 0, 1, many strings; every E0321 case (unknown key, key twice, bad name, `-name`, absolute path, escaping `..`, `\`, non-`.c` file, unterminated array).
- [ ] `manifest.rs` unit: `[link]` with `[packages]`/`[package]` in any order; `[link]` twice is E0321.
- [ ] `inputs.rs` unit: order (program first), de-duplication, `-L` before `-l`; packages with no loaded module ignored.
- [ ] `pkg_config.rs` unit (output filtering, pure): allowed flags kept; `-Wl,…`, `-framework`, a bare word → error naming it.
- [ ] `tests/link_libs.rs` (through `lughac`; fixtures build a static `libfixture.a` with `cc` + `ar` in a temp dir):
  - [ ] `libs` + `paths` in the program's manifest: `run` prints the C function's result;
  - [ ] the same through a path package: the program's manifest has no `[link]`;
  - [ ] `c` + `include` in a package: a shim calling a header-only helper links and runs;
  - [ ] `pkg-config` with a fake `pkg-config` script first on `PATH` (prints `-L… -lfixture`): runs;
  - [ ] E0329: no `pkg-config` on `PATH`; it exits 1; it prints `-Wl,-rpath,/x`;
  - [ ] E0330: a C file with a syntax error;
  - [ ] E0331: `libs = ["nosuchlib"]`, exit 1, cc's message in the note;
  - [ ] E0322: a missing `c` file; `lughac check` reports E0321/E0322 and never runs `pkg-config` (the fake script writes a marker file; it must not exist);
  - [ ] `lughac test` links the same inputs;
  - [ ] JSON diagnostics carry the codes.
- [ ] A program with no `[link]` gets the same `cc` arguments as before (unit test on the argument list).
- [ ] `tests/explain.rs` covers the three new pages automatically.

## ROLLBACK PLAN
- Branch to return to: `main` at the commit before the merge.
- State: the feature lives on branch `prp-117-linking_c_libraries` in worktree `../lugha-prp-117`; abandoning it means deleting the branch. Manifests with `[link]` become E0321 again.

## ACCEPTANCE CRITERIA
- [ ] Manual: `libs = ["z"]` and `extern fun zlibCompileFlags(): u64;` builds and runs against the system zlib.
- [ ] Every test above passes; a program without `[link]` links with the identical command.
- [ ] `lughac check`/`lsp` never start `pkg-config` or `cc` (test).
- [ ] Spec, draft, `docs/errors/` and CLAUDE.md updated; `lughac explain E0331` works.
- [ ] All existing tests pass
- [ ] New tests written and passing
- [ ] `cargo clippy --all-targets -- -D warnings` passes
- [ ] `cargo fmt --check` passes
- [ ] Every `pub` item has a doc comment
- [ ] CHANGELOG.md updated

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo run -q -- run <zlib example>` (manual check above)

## BUILT (session 185)
- As planned, test-first, with `link.rs` → `src/link/mod.rs` moved alone first (`ece0599`).
- **Stricter than planned:** `paths`/`include`/`c` may hold no `..` at all (as `source`), rather than "no `..` leaving the package".
- **`$PKG_CONFIG`** names the `pkg-config` program when set (the usual convention); the tests use it for their fake, so they don't need `pkg-config` installed.
- **Messages:** `Diagnostic` has no notes, so cc's and pkg-config's output goes in `help` ("the linker said: …", then the advice).
- **E0331's location:** the first `[link]` line that lists something, program first.
- **Found and fixed:** the CLI placed the prelude right after the last module, overlapping the manifests' offsets, so a manifest diagnostic made after checking showed in `<prelude>`. `check_parsed` now places it past every loaded file, as the language server already did.
- **Tests:** `tests/link_libs.rs` (10), unit tests in `link_table.rs` (5), `manifest.rs` (1), `link_tests.rs` (5), `inputs.rs` (5), `pkg_config.rs` (4), `link/mod.rs` (3); the three new pages through `tests/explain.rs`. Manually: `libs = ["z"]` with `zlibCompileFlags()` prints `zlib flags: 169`; `libs = ["raylib"]` without raylib is E0331 at the line, exit 1.
