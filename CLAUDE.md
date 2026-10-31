# CLAUDE.md — lugha

lugha is the compiler for Lugha, a small statically typed language: `lughac` (Rust, LLVM 21) lexes, parses, type-checks and compiles `.la` programs to native executables, linked with a C runtime and a garbage collector. It also ships the standard library, a formatter, a test runner, a language server and a VS Code extension. The language is defined by `docs/specs/Language v17 Specification.md` (`lughac spec` prints it); v17 is in progress and its approved plan is `Language v17 Draft.md`.

Where things are recorded: **DECISIONS.md** (every decision and its why: core decisions, then open, deferred and resolved), **PRPs/** (a spec per feature), **CHANGELOG.md** (what shipped), **TODO.md** (what's next), the spec's §11 (the milestone table), and git's log. Commit messages say what changed and why.

---

## HOW WE WORK

- **A feature needs an approved PRP** in `PRPs/prp-{NNN}-{feature_name}.md` (next unused number, never reused; from `PRPs/TEMPLATE.md`). Without one: no code; run a discovery (`PRPs/DISCOVERY.md`), one question at a time, each with a recommended answer, then write the PRP and wait for approval. Chores and small fixes skip the PRP when the user says so.
- **One PRP at a time.** If the scope grows mid-way: stop, say what you found, update the PRP and get approval before going on. The user decides what's small enough to add.
- **A branch per PRP or chore**, from `main`. Push the branch and open the PR; merging and tagging are the user's: end with a `!` line they can run (`gh pr merge N --merge && git checkout main && git pull -q && git tag mNNN && git push origin mNNN`). Stage files by name, never `git add -A`.
- **Never make an architectural choice silently.** Check DECISIONS.md first: `open` blocks the work (ask the user); `resolved` is followed, not re-litigated; a new question is added as `open`, and you stop.
- **The spec is the source of truth.** Read the relevant section (and the Draft, for features not yet landed) before implementing, and cite it in the PRP. If code and spec disagree, the code is wrong; if the spec looks wrong, propose a change, never edit it without approval.
- **Files stay at or under 500 lines**, tests included. At the limit, propose a split (file names and what moves) and wait. `third_party/` is vendored, not ours: exempt, never edited by hand (see its `VERSION`).
- **Never add a crate without a PRP line saying why.** Every dependency is attack surface and must be GPL-3.0-compatible.
- After shipping: update CHANGELOG, the spec (§11 row, "Changes since v16", the relevant section), TODO (remove what's done) and this file if a command, rule or stack item changed.

## TESTING

- **Test first.** For every new function, a failing test, then the least code that passes it. Every new `pub` function gets a happy-path test and one per error it can return. Test behaviour callers see, not private internals or third-party crates.
- Unit tests in a `#[cfg(test)] mod tests` block at the bottom of the file; integration tests (public API) in `tests/`, one file per area; end-to-end cases are files in `tests/programs/` (`<name>.la` plus `.stdout`, `.exit`, `.stderr` for rejected programs; a directory `<name>/` with `main.la` is one case). Add a case by adding files, never by editing the runner.
- **Run only what the change touches**: `cargo test --lib`, the touched `--test <name>` files, and `--test programs` when language behaviour changed. The full `cargo test` only when the user asks or a change reaches broadly across codegen or the runtime. CI runs everything once per PR in the background; merges don't wait for it.
- A task is done when `cargo fmt --check` and `cargo clippy --all-targets -- -D warnings` are clean and the targeted tests pass.
- The spec's §10 programs are acceptance tests (`tests/spec.rs`): their stdout and exit codes must match exactly.

## SECURITY — NON-NEGOTIABLE

Never implement without the user's explicit review: authentication or sessions, cryptography, secrets or credentials, any `unsafe` block, and changes to how `git` runs (`src/modules/git.rs`), how programs are started (`runtime/process.c`), how `pkg-config` and `cc` run on a package's `[link]` input (`src/link/`), or what the VS Code extension starts and the language server may do. If unsure whether something is security-sensitive, stop and ask.

- Never hardcode credentials, keys or tokens, not even in examples. Never log sensitive data.
- Never trust external input (files, CLI args, network, env) without validating it.

## PROTECTED FILES

Never read, modify or delete: `.env` and `.env.*`; `Cargo.lock` (changes only as a side effect of cargo); `target/`; `LICENSE` (GPL-3.0, the user's). If a task seems to need one, stop and ask. Keep `.llmignore` and `.claude/settings.json`'s deny rules in step with this list.

---

## COMMANDS

```bash
cargo build                                   # build
cargo test --lib                              # unit tests; then cargo test --test <name>
cargo fmt --check                             # formatting
cargo clippy --all-targets -- -D warnings     # lint

cargo run -q -- build prog.la [-o out] [--emit=tokens|ast|ir] [-O0|-O2] [--cpu=x86-64|x86-64-v2|x86-64-v3|x86-64-v4|native] [--target=x86_64-unknown-linux-gnu|aarch64-unknown-linux-gnu] [--diagnostics=human|json]
cargo run -q -- run prog.la                   # exit code = the program's
cargo run -q -- check prog.la [--offline]     # lex, parse, type-check
cargo run -q -- test [paths…] [--filter T] [--timeout S] [--cpu=…]  # @test functions, each in its own process
cargo run -q -- fmt [paths…] [--check]        # format .la files in place
cargo run -q -- update prog.la                # re-resolve git packages, rewrite lugha.lock
cargo run -q -- init [dir] [--lib] [--name N] # start a program or library; never overwrites
cargo run -q -- explain [CODE]                # an error code's page (docs/errors/<CODE>.md)
cargo run -q -- spec                          # the bundled language spec
cargo run -q -- lsp                           # the language server on stdin/stdout

python3 bench/run.py [names…] [--lang L,…] [--runs N] [--check] [--save]  # benchmarks vs C, Rust, Go, Node (bench/README.md); never in CI

cd editors/vscode && npm ci && npm test       # the VS Code extension
npm run package                               # lugha-<version>.vsix; bump the version (npm version minor --no-git-tag-version) whenever editors/vscode changes
```

End-to-end tests need `llvm-21-dev` and `cc`; `tests/arm.rs` needs `gcc-aarch64-linux-gnu` and `qemu-user` (it skips without them); `std.tls`/`std.crypto` tests need `libssl-dev`.

## STACK

- **Rust 1.99.0** (pinned in `rust-toolchain.toml`), edition 2024 — package `lugha`, binary `lughac`.
- **inkwell 0.10**, feature `llvm21-1-prefer-dynamic`, with LLVM 21 (`llvm-21-dev`). Never another LLVM major; never static LLVM linking (fails on Ubuntu, DECISION-005).
- **The C runtime** (`runtime/*.c`, fragments joined into one `lugha_rt.c`, each ≤ 500 lines), linked into every program; OpenSSL 3 only into programs loading `std.tls` or `std.crypto`.
- **Boehm GC** bdwgc 8.2.12, vendored unmodified in `third_party/bdwgc/`, compiled once (no OS threads) and linked statically.
- **The C compiler** is `cc` or `$LUGHA_CC` (words split on spaces, no shell); `[link]` tables add C libraries, C files and filtered `pkg-config` flags, only at build time, through `src/link/`. Objects are cached in `<cache root>/objects/`.
- **Targets:** x86-64 Linux (default) and ARM64 Linux (`--target aarch64-unknown-linux-gnu`, build only, `aarch64-linux-gnu-gcc` unless `$LUGHA_CC`).
- `codespan-reporting` (human diagnostics), `clap` (CLI), `thiserror` (internal errors); `lsp-server` + `lsp-types` + `serde_json` for `lughac lsp` only. Git packages need the system `git`, run only through `modules/git.rs`.
- **The prelude** (`src/prelude/*.la`) and **std** (`src/std/*.la`, loaded only when imported; internal parts are `std.$name` modules) are written in Lugha and embedded.
- **`editors/vscode/`**: TypeScript on Node 22, one runtime dependency (`vscode-languageclient`); it starts `lughac lsp`, and `lughac test` only when asked in a trusted workspace.

## ARCHITECTURE

1. **Stages only depend backwards:** lexer → parser → modules → check → codegen → driver (`link/` beside codegen). A stage never imports a later one, so `check` and `--emit` can run the pipeline partway.
2. **Every token and AST node carries a byte-offset `Span`**: every diagnostic needs a location.
3. **Codegen never infers types**: it reads the checker's expression → type side table (`Checked`).
4. **User-program errors are `Diagnostic` records**, never panics or strings, each with a stable code from its stage's range (E01xx lex … E05xx, W01xx). A new code takes the next unused number and gets a page in `docs/errors/` (`tests/explain.rs` checks it); codes are never reused or renumbered. Stages return records; only the driver renders them (human and JSON from one record). The checker never stops at the first error.
5. **Exit codes are 0 / 1 / 2:** 1 = the user's program has errors; 2 = bad CLI usage or an internal error (`module.verify()` failure, a Rust panic).
6. **Symbol prefixes are fixed and disjoint** (`lugha_fn_`, `lugha_rt_`, `lugha_ref_`, `lugha_closure_`, `lugha_vt_`, …; module paths joined with `/`, generic arguments as they print): see spec §8 and core decision 6 before adding a new kind of symbol.
7. **Generated code allocates only through `lugha_rt_alloc`**, never `malloc`: Boehm can't see `malloc` memory.
8. **No implicit conversions** (spec §4), except a value to an interface where one is expected (`Checked.conversions`).
9. **Never implement a §11 "out of scope" feature**, even partially, nor a v17 feature the Draft doesn't plan yet.

**Layout:** `src/lexer/`, `parser/`, `ast/`, `modules/` (loading, packages, git, manifests), `check/` (the type checker; `errors/` holds the diagnostic builders), `codegen/` (LLVM IR, C ABI, frames), `link/` (runtime, GC, `[link]`, object cache), `driver/` (CLI, build pipeline, `test`, `fmt`, `lsp/`), `formatter/`, `prelude/`, `std/`; `runtime/` (C); `tests/` (one file per area, `programs/` end-to-end, `fixtures/` vendored suites, `common/` and `support/` helpers); `docs/specs/`, `docs/errors/`; `editors/vscode/`. Find code with grep: names follow the feature (`check/maps.rs`, `codegen/maps.rs`).

**Errors in lughac itself:** expected failures return `Result<T, E>` with the module's own `thiserror` enum; panics are for bugs only. Never `.unwrap()`/`.expect()` on anything derived from external input (`.expect("reason")` only for true invariants, stating the invariant); never `Option` to signal an error; never swallow an error with `let _ =` or `.ok()` without a comment saying why it's safe; propagate with `?` and `From` impls. Stages return `Result<(Output, Vec<Diagnostic>), Vec<Diagnostic>>`.

## CODE STYLE

Read `docs/CODE_STYLE.md` before writing code. Every `pub` item has a `///` doc comment; every non-obvious decision inside a function has a `//` comment saying why. Never silence a clippy lint with `#[allow(...)]` without a comment saying why. Std and prelude items have `/** … */` comments (the language server shows them).

## KNOWN ISSUES — DO NOT FIX

Specified behaviour that looks like a bug:
- A function whose only exit is a `return` inside `while true` is rejected as missing a return (spec §6); write it as `loop { … }` (milestone 152), which counts as returning.
- Integer overflow always panics, even at `-O2` (spec §12). `as` casts truncate or saturate silently (spec §4), the only place values wrap.
- Boehm may keep garbage alive when an integer looks like a pointer (spec §7).
- Extern C code keeping a Lugha pointer is undefined behaviour (spec §7–8).
