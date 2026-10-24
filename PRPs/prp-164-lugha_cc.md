## FEATURE: `$LUGHA_CC` chooses the C compiler lughac runs for the runtime, the collector, `[link]` C files and linking; milestone 132 (v17), portability roadmap step A3a.

**Status:** proposed 2026-10-24 (session 314). **Needs the user's security review**: it changes how `cc` runs (CLAUDE.md anti-pattern 5, PRP-117). Discovery: (1) A3 is split. This PRP is only the compiler choice; `--target`, LLVM's sizes per target and the C calling rules behind one interface move into stage B (a new step B0), where an ARM toolchain and qemu make them testable. (2) The variable is `LUGHA_CC`, holding words: a program and its first arguments, split on spaces, never run through a shell.

## OBJECTIVE
`LUGHA_CC=clang lughac build app.la` builds with clang, and `LUGHA_CC="zig cc"` with zig, everywhere lughac runs a C compiler today. Unset or empty means `cc`, exactly as now. Switching compilers recompiles the cached objects, because the cache key already includes the compiler's `--version` text, which now comes from the chosen compiler. This is the hook stage B's cross builds use: `LUGHA_CC="zig cc -target aarch64-linux-gnu"`.

## CONTEXT
- **Starting state:** `cc` is hard-coded in five places:
  - `src/link/mod.rs:294`, the link (the file is at 500 lines);
  - `src/link/runtime.rs:144`, the runtime;
  - `src/link/gc.rs:322`, the collector;
  - `src/link/c_files.rs:46`, the `[link]` C files;
  - `src/link/object_cache.rs:54`, `cc --version` for the key.
  - `LinkError::CcNotFound` says "C compiler `cc` not found; install gcc or clang".
- **Ending state:**
  - `src/link/compiler.rs` (new): `pub struct Compiler { program: OsString, args: Vec<OsString> }`.
    - `Compiler::from_env()` reads `LUGHA_CC` once per build.
    - `Compiler::command()` gives a `Command` with the leading arguments already added.
    - `Compiler::name()` gives the text for messages: `cc`, or `zig cc (from $LUGHA_CC)`.
    - `LUGHA_CC`, unset, empty or only spaces, means `cc`. Otherwise it splits on ASCII spaces, empty words dropped: the first word is the program, the rest come before every argument lughac adds.
  - `Cache::open` and the five call sites take the `Compiler`, created once in `link_with` (and in `gc::bundled`).
  - `LinkError::CcNotFound` carries the name: "C compiler `clang` (from $LUGHA_CC) not found", or as today for `cc`.
  - `LinkError::BadCompiler` (new, exit 2): `LUGHA_CC` isn't UTF-8.
- **Split needed:** `src/link/mod.rs` is at 500 lines, so `LinkError` and its `diagnostic`, about 90 lines, move to `src/link/errors.rs`, re-exported from `link`. That's a move only, done first, in its own commit.
- **Related existing code:** `src/link/pkg_config.rs`. `$PKG_CONFIG` is the precedent: a user-chosen program, run without a shell.
- **Spec:** §9 (Link) gains a sentence on `LUGHA_CC`, plus §11 row 132 and a §0 line. Approving this PRP approves these edits.
- **Roadmap (TODO.md):** A3 becomes A3a, this PRP, and a new B0 holds `--target`, LLVM data-layout sizes, the `CAbi` interface and `[build] cpu`.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- One `Compiler` per build, so every compile, the link and the cache key agree.
- Run it from an argument list. Splitting is on the space character only; quotes, `$`, globs, `~` and backslashes are ordinary characters.
- The leading arguments go first: `zig cc -c x.c …`, `zig cc prog.o … -o out`.
- Keep every `src/link/` file under 500 lines (the split above).
- Docs: README (an "Other C compilers" line), CLAUDE.md (STACK and COMMANDS), CHANGELOG, `TODO.md` (the roadmap change).

### Must NOT Do
- Don't read `CC`, `CFLAGS` or `LDFLAGS` (discovery answer 2).
- Don't use a shell or any quoting rules: a path with spaces needs a wrapper script, and the README says so.
- Don't let `LUGHA_CC` reach `pkg-config`: it has its own `$PKG_CONFIG`.
- Don't add `--target`, a config file or a `lugha.toml` key (stage B0 and later).
- Don't change the test harnesses (`tests/tasks_runtime.rs`, `tests/codegen.rs`), which run `cc` themselves on purpose.

### New dependencies
- none

## ERROR HANDLING REQUIREMENTS
- `LUGHA_CC` names a program that isn't there: `LinkError::CcNotFound` with the name (exit 2).
- `LUGHA_CC` isn't UTF-8: `LinkError::BadCompiler` (exit 2). The text would otherwise be split unreliably.
- The compiler runs and fails: as today, `LinkError::Failed`, `CFile` (E0330) or `Libraries` (E0331).
- Its `--version` doesn't run: the cache caches nothing, as today (milestone 127).

## SECURITY CONSIDERATIONS
- **For the user's review:** lughac runs the program `LUGHA_CC` names, with lughac's own arguments (source paths, flags from `[link]` and `pkg-config`, output paths).
  - This is the trust `PATH` already gives: whoever can set the environment can already put a `cc` first on `PATH`. So it adds no new attacker, only a new spelling.
  - No shell is involved, so a `LUGHA_CC` holding `;`, `$( )` or backticks runs nothing extra; those characters reach the program as words.
  - `[link]`'s input still passes through the existing filters (PRP-117, PRP-129).
- The variable's value appears only in error messages, as the user wrote it.
- No `unsafe` and no new dependencies.

## TESTS TO WRITE
- [ ] `src/link/compiler.rs` units: unset, empty and spaces → `cc` with no arguments; `clang` → `clang`; `"zig cc -target x"` → `zig` plus `cc -target x`; runs of spaces collapse; `a;b` and `$(x)` stay single words; `name()` for each.
- [ ] `tests/c_cache.rs` (logging wrapper): `LUGHA_CC=<wrapper> extra-word` makes every compile and the link go through the wrapper, with `extra-word` first; switching `LUGHA_CC` to another wrapper recompiles all three objects; switching back hits the cache.
- [ ] `tests/cli.rs`: `LUGHA_CC=no-such-compiler lughac build` is exit 2, with stderr naming `no-such-compiler` and `$LUGHA_CC`; a `LUGHA_CC` of `"true; touch hacked"` creates no `hacked` file.
- [ ] `tests/link_libs.rs`: a `[link]` C file builds with `LUGHA_CC` set (the C files path).
- [ ] Every existing test passes with `LUGHA_CC` unset (today's behaviour).

## ROLLBACK PLAN
- Branch to return to: `main`.
- State: `cc` hard-coded as before. The `errors.rs` split can stay, since it is behaviour-neutral.

## ACCEPTANCE CRITERIA
- [ ] Every C compile and the link run `$LUGHA_CC` when set, `cc` otherwise
- [ ] Changing `LUGHA_CC` recompiles cached objects
- [ ] A missing compiler is exit 2 naming it
- [ ] `cargo clippy --all-targets -- -D warnings` passes
- [ ] `cargo fmt --check` passes
- [ ] Every `pub` item has a doc comment
- [ ] CHANGELOG.md updated
- [ ] The user has reviewed the security section

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test --lib`, `--test c_cache`, `--test cli`, `--test link_libs`, `--test programs`, `--test callbacks`, `--test tls`
