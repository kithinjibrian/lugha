# CLAUDE.md — lugha

Behavioral instructions for AI coding assistants. Each rule exists to prevent a specific mistake.

---

## SESSION HANDOFF RULE — NON-NEGOTIABLE

Every session in `CONTEXT.md` must have:
- A **name** — short descriptive title of what the session accomplished
- A **state** — `open` while work is in progress, `closed` once handoff is done
- A **branch** — the git branch this session's work lives on

Format:

    ## SESSION {n} — {YYYY-MM-DD} — {Name} — {state}
    Branch: {branch-name}

Rules:
- **Step 1 of every session, no exceptions:** append a new session entry to `CONTEXT.md` with state `open` and the current branch name. Do this before reading any other file, before planning, before writing any code. Commit it before any other work begins.
- Mark it `closed` only after CONTEXT.md is updated, committed and pushed.
- Never leave a session `open` at the end of a turn.
- Never start a new session without closing the previous one first.
- The NEXT SESSION START POINT block is rewritten at the end of every session.
- Sessions are never deleted — the full history stays in CONTEXT.md.

---

## PRP RULE — NON-NEGOTIABLE

Never write code for a new feature without a PRP file in `/PRPs`.

If a feature request is given without a PRP:
1. Do not write any code.
2. Run a discovery interview (`PRPs/DISCOVERY.md`) — one question at a time.
3. Write the PRP to `/PRPs/prp-{NNN}-{feature_name}.md` from `PRPs/TEMPLATE.md`.
   - `NNN` is the next unused number, zero-padded to three digits. Numbers are never reused or renumbered.
   - `feature_name` is lowercase snake_case: `prp-001-lexer.md`, `prp-002-type_checker.md`.
4. Present it to the user and build only after explicit approval.

A vague prompt is not a starting point. It is the beginning of a discovery.

---

## SCOPE RULE — NON-NEGOTIABLE

One PRP at a time. Never implement more than one feature's scope in a single session.

If mid-implementation the scope turns out larger than the PRP described: stop, document what was discovered, update or create a PRP, and get approval before continuing. The human decides what is "small enough to add", not the model.

---

## TESTING RULE — NON-NEGOTIABLE

Write the test before writing the implementation. No exceptions.

- For every new function, write a failing test first, then the minimum code to make it pass.
- Unit tests live in a `#[cfg(test)] mod tests` block at the bottom of the file they test.
- Integration tests (public API only) live in `tests/`, one file per area: `src/lexer.rs` → `tests/lexer.rs`.
- End-to-end tests are files in `tests/programs/`: `<name>.la` plus `<name>.stdout`, `<name>.exit`, and `<name>.stderr` for rejected programs. Add a test by adding files — never by editing the runner.
- Test behavior visible to callers — inputs, outputs, and every error variant. Do not test private internals or third-party crate behavior.
- Every new `pub` function gets at least one happy-path test and one test per error variant it can return.
- Before calling a change done, run `cargo test --lib` and the integration tests for the area touched (`cargo test --test <name>`, and `--test programs` when language behaviour changed). The full suite (`cargo test`) is optional: run it when the user asks, or when a change reaches broadly across codegen or the runtime (the user, session 217).

If the PRP does not describe what to test, add the test cases to the PRP before writing code.

---

## SECURITY RULE — NON-NEGOTIABLE

Never implement without explicit human review and approval:
- Authentication, authorization, or session management
- Cryptographic operations (hashing, signing, encrypting)
- Secrets or credential handling
- Any `unsafe` block

For everything else:
- Never hardcode credentials, API keys, tokens, or secrets — not even in comments or examples.
- Never trust external input (files, CLI args, network, env) without validating it first.
- Never log sensitive data.
- Never add a dependency without stating why in the PRP — every crate is attack surface.

If unsure whether something has a security implication, stop and ask.

---

## CODE DOCUMENTATION RULE — NON-NEGOTIABLE

Read `docs/CODE_STYLE.md` before writing any function, type, or module.

Every `pub` item has a `///` doc comment. Every non-obvious decision inside a function has a `//` comment explaining *why*, not what.

---

## PENDING DECISIONS RULE — NON-NEGOTIABLE

Before writing code that depends on an unresolved architectural question, check `DECISIONS.md`.

- `open` → stop. Do not implement. Ask the user to resolve it.
- `resolved` → follow the recorded outcome. Do not re-litigate.
- New unresolved question mid-implementation → add it to `DECISIONS.md` as `open` and stop. Do not guess.

Never make an architectural choice silently.

---

## FILE SIZE RULE

No file exceeds 500 lines (including tests). (Raised from 300 by the user in session 107.)

`third_party/` is vendored code, not ours (PRP-163): exempt from this rule, the CODE DOCUMENTATION RULE and `docs/CODE_STYLE.md`, and never edited by hand; update it only as its `VERSION` file says.

When a file reaches the limit: stop, propose a specific split (new file names and what moves where), wait for approval, split, then continue.

---

## PROTECTED FILES — NON-NEGOTIABLE

Never read, modify, or delete:

- `.env` and `.env.*`
- `Cargo.lock` — never hand-edit; it changes only as a side effect of cargo commands
- `target/` — build output
- `LICENSE` — GPL-3.0, changed only by the human

If a task seems to require touching a protected file, stop and ask. See also `.llmignore`. `.claude/settings.json` denies Claude's Read and Edit tools on the same paths. Keep all three in sync. The deny rules block only Claude's own tools, not a sandbox: cargo still writes `Cargo.lock` and `target/`.

---

## COMMANDS

```bash
cargo build                                   # build
cargo test                                    # all tests
cargo fmt --check                             # formatting
cargo clippy --all-targets -- -D warnings     # lint

cargo run -q -- build prog.la [-o out] [--emit=tokens|ast|ir] [-O0|-O2] [--cpu=x86-64|x86-64-v2|x86-64-v3|x86-64-v4|native] [--target=x86_64-unknown-linux-gnu|aarch64-unknown-linux-gnu] [--diagnostics=human|json]
cargo run -q -- run prog.la                   # exit code = the program's
cargo run -q -- check prog.la [--offline]     # lex, parse, type-check
cargo run -q -- update prog.la                # re-resolve git packages, rewrite lugha.lock
cargo run -q -- spec                          # print the bundled language spec
cargo run -q -- explain [CODE]                # explain an error code (v16); docs/errors/<CODE>.md
cargo run -q -- init [dir] [--lib] [--name N] # start a program or library (v16); never overwrites
cargo run -q -- fmt [paths…] [--check]        # format .la files in place (v16); --check lists, writes nothing
cargo run -q -- test [paths…] [--filter T] [--timeout S] [--cpu=…]  # run @test functions, each in its own process (v16)
cargo run -q -- lsp                           # the language server on stdin/stdout (v16); editors start it

cd editors/vscode && npm ci                   # the VS Code extension (PRP-099, PRP-100)
npm test                                      # its server-command, keyword and grammar tests
npm run package                               # lugha-<version>.vsix; bump the version (`npm version minor --no-git-tag-version`) whenever editors/vscode changes, or VS Code keeps the old build
```

Run fmt, clippy and the targeted tests (see TESTING RULE) after every non-trivial change. A task is not done until they pass. These local runs are the merge gate: the full `cargo test` is optional (the user, session 217), and CI runs everything once per pull request as a background safety net that merges don't wait for (the user, session 150).

> End-to-end tests also need `llvm-21-dev` and `cc` installed (the collector is bundled since milestone 131).

---

## STACK

- Rust 1.99.0 (pinned in `rust-toolchain.toml`), edition 2024 — package `lugha`, binary `lughac`.
- `inkwell` 0.10, feature `llvm21-1-prefer-dynamic` + LLVM 21 (`llvm-21-dev`) — codegen. Never use another LLVM major; never switch to static LLVM linking (fails on Ubuntu, see DECISION-005).
- C (`lugha_rt.c`, kept as three fragments in `runtime/`) — the runtime linked into every compiled program.
- Boehm GC — bdwgc 8.2.12, vendored unmodified in `third_party/bdwgc/` (v17, PRP-163), compiled once without OS threads (`-DALWAYS_SMALL_CLEAR_STACK`) by `src/link/gc.rs` and linked statically; `libc`, `libm` — linked into every compiled program via the system `cc`.
- OpenSSL 3 (`libssl-dev`, Apache-2.0) — linked only into programs that load `std.tls` (v17, PRP-144; `runtime/tls.c`) or `std.crypto` (PRP-145; `runtime/crypto.c`, `-lcrypto`), appended to their runtime.
- `[link]` tables (v16, PRP-117) add C libraries, folders, C files (each compiled alone with `cc -c`) and `pkg-config` flags (`$PKG_CONFIG` or `pkg-config`, output filtered to `-l`/`-L`/`-I`/`-D`/`-pthread`, and from `--cflags` exactly `-mfpmath=sse`/`-msse`/`-msse2`, PRP-129) — only at build time (`build`/`run`/`test`), never in `check` or `lsp`; all through `src/link/`. Their objects and the runtime's are cached in `<cache root>/objects/` (v17, PRP-159, `src/link/object_cache.rs`).
- Two targets (v17, PRP-165): x86-64 Linux (default) and ARM64 Linux (`--target aarch64-unknown-linux-gnu`, build only; `codegen::Target`, `src/codegen/targets.rs`; extern structs by AAPCS64 there, `src/codegen/c_abi_arm.rs`, PRP-166; E0449 retired). ARM builds use `aarch64-linux-gnu-gcc` unless `$LUGHA_CC` is set; `tests/arm.rs` runs them under `qemu-aarch64` (`gcc-aarch64-linux-gnu`, `qemu-user`).
- The C compiler is `cc` or `$LUGHA_CC` (v17, PRP-164: words split on spaces, no shell), one `link::compiler::Compiler` per build, held by the object cache and used for every compile and the link.
- `codespan-reporting` (ASCII chars) — human diagnostics. `clap` (derive) — CLI. `thiserror` — internal errors.
- `lsp-server` + `lsp-types` + `serde_json` — `lughac lsp` only (PRP-100): rust-analyzer's synchronous stdio loop and typed protocol structures; no async runtime. Nothing else in lughac uses them.
- License: GPL-3.0 — every dependency must be GPL-3.0-compatible.
- `editors/vscode/` — the VS Code extension (PRP-099, PRP-100): TypeScript on Node 22, one runtime dependency (`vscode-languageclient`, MIT) and dev dependencies (`typescript`, `@types/vscode`, `@types/node`, `vscode-tmgrammar-test`, `@vscode/vsce`), tests with `node:test`. It starts `lughac lsp`, which never runs `git` or writes a file, and, only when asked in a trusted workspace, `lughac test` (PRP-115), which runs the workspace's tests; changes to how it starts `lughac`, or to what the server may do, need the user's security review.

The language itself is defined by `docs/specs/Language v17 Specification.md` (`lughac spec` prints it): Lugha v17, in progress (milestones 91–135 done: small stack frames (slots shared between arms, stack probes); ARM64 struct passing (AAPCS64, E0449 retired); Linux on ARM64 (`--target aarch64-unknown-linux-gnu`); `$LUGHA_CC`; the collector built in (bdwgc, no `libgc`); portable x86-64 executables (`--cpu`, the baseline by default); `lughac init` writing `test/`; `include_bytes`/`include_str` (E0447/E0448, inside the package's folder); the C object cache; one `lughac test` build per package folder; struct results through a hidden pointer and struct values split into fields (`-O0` builds ~3× faster); methods sharing a field's or (with `self`) a variant's name; `xs + ys` joining arrays, `+=` on arrays and strings; `net.resolve` and `tls.Options.address`; `hkdf_sha256` in `std.crypto`; `request_tls_with` in `std.http`; `and_then` on `Option` and `Result`; octal literals `0o600` (W0107 for a leading zero); `fs.sync`; `fs.rename`, `fs.set_mode`, `flush()`, `std.time`; `std.toml` (a TOML 1.0 parser in Lugha); `std.crypto` (SHA-256, Ed25519, random bytes, `equal`, over OpenSSL 3); TLS (`std.tls` over the system OpenSSL 3, `https://` in `std.http`, E0332); `std.http` (HTTP/1.1 server and client in Lugha); `std.net` (TCP, non-blocking sockets in the scheduler); global constants (`const NAME: T = value;`, E0445/E0446); float bits (`x.to_bits()`, `f64.from_bits(b)`); the unit type `()` replacing `void` (W0106; E0407 retired); nullable handle fields (`Option<H>` held as C's pointer) and plain-data unions; pointers at the C boundary (`ptr`/`mut` parameters, arrays, `extern union`, `CPtr` with `c_read`/`c_write`, callback arrays); `out` parameters on externs (`out fuel: u64`, C writes through a pointer); maps and sets (`Map<K, V>`, `Set<T>`, `#{…}` literals, written in Lugha in `src/prelude/maps.la` over `$hash.value`); glob re-exports (`public import m { * };`); package entry modules (`import N;` → the package's `lib.la`); module doc comments (a file's top `/** */` before a blank line or `import`, shown on hover); packages and tests (`skip(reason)`, a package's file tested as its module, `test/` inside its package, one `lugha.lock` per package, `check` without `main`); inference from siblings (`assert_eq(text("hi"), expected)`); `lughac fmt` hugging trailing arrays and struct literals; variants as function values (`Msg.Named`, `Some`, `Ok`, `Err`); struct update syntax `S { f: e, ..base }` (Rust's rules, E0440, W0104); green threads — `spawn`, `Task<T>`, `Chan<T>`, `select`, `std.task`, all on one OS thread; actors — `actor` declarations, handles, call/cast messages, `stop`/`join`/`is`, panics ended in their actor, sharing nothing; `Language v17 Draft.md` is the approved plan). v16 is complete (milestones 67–90: C structs by value (`extern struct`, x86-64 System V), strings from C (`string`/`Option<string>` results and callback parameters, copied), callbacks and handles (`extern type`, callbacks with `data`/`release`, `extern "symbol" fun`), linking C libraries (`[link]` in `lugha.toml`), `lughac explain`, tests in the editor, `lughac test`, Find References and Rename, more quick fixes, formatting in the editor, `lughac fmt`, the add-missing-arms quick fix, snippets, doc comments `/** … */` shown by the language server (std and the prelude documented), block comments `/* … */` (nested), partial syntax trees for the language server, signature help, completion, go-to-definition, hover and `lughac lsp`; `lughac init`; `env.set_current_dir`; shared boxes, `Ref<T>` — `Ref.new`, `get`, `set`, `update`, `is`; not `Eq`; cyclic printing) and frozen. v15 is complete (milestones 51–66: package layout, operator overloading, the std library `std.math`/`random`/`env`/`fs`/`process`, copy-on-write arrays and loop ownership, nested `mut self` calls, numbers as `T`, user-defined `Ord`, widths in characters, C → Lugha parsers and UTF-8) and frozen, like v0–v14 and v16. `Result`, `Iter`, `Display`, `Ordering`, the iterator types and the array helpers are written in Lugha in `src/prelude/` (`core.la`, `iter.la`, `more.la`, `text.la`, `ops.la`), loaded with every program. Git packages need the system `git` at build time (PRP-079); it runs only through `modules/git.rs`.

---

## ARCHITECTURE RULES

1. **The spec is the source of truth.** Read the relevant section of `docs/specs/Language v2 Specification.md` (and, for features not yet landed, `Language v2 Draft.md`) before implementing any language feature, and cite the section in the PRP. If code and spec disagree, the code is wrong. If the spec looks wrong, stop and propose a spec change — never edit the spec without explicit approval. *Why:* `lughac spec` ships the spec as the language reference; drift makes it lie.
2. **Stages only depend backwards.** lexer → parser → modules → check → codegen → driver. A stage never imports a later one. *Why:* `lughac check` and `--emit=tokens|ast|ir` must run the pipeline partway.
3. **Every token and AST node carries a byte-offset `Span`.** *Why:* every diagnostic needs a location, and spans can't be recovered later.
4. **Codegen never infers types.** It reads the checker's expression → type side table. *Why:* opaque pointers need the element type on every load/store/GEP (spec §9), and two type computations will disagree.
5. **User-program errors are `Diagnostic` records, never panics or strings.** Each has a stable code from its stage's range (E01xx lex … E05xx, W01xx). New codes take the next unused number and get a page in `docs/errors/` (`tests/explain.rs` checks it); a code is never reused or renumbered. Human and JSON output render from the same record. *Why:* spec §9; codes are a public contract.
6. **Exit codes are 0 / 1 / 2.** 1 = the user's program has errors; 2 = bad CLI usage or internal compiler error. A `module.verify()` failure or a Rust panic inside lughac is exit 2, never 1. *Why:* spec §9; tools tell "your bug" from "our bug" by it.
7. **Symbol prefixes are fixed.** Lugha functions → `lugha_fn_<name>`, methods → `lugha_fn_<Type>.<name>`, items of a module other than the main file → the module path joined with `/` first (`lugha_fn_shapes/circle/area`, `lugha_fn_shapes/circle/Circle.area`), generic instances → the type arguments as they print, module paths with `/` (`lugha_fn_max<i64>`, `lugha_fn_Stack<string>.push`, `lugha_fn_a/b/max<a/b/Point>`), functions used as values → wrappers `lugha_ref_<symbol>` (v3), variants used as values → `lugha_ref_<Enum>.<Variant>` (v17), closure bodies → `lugha_closure_<enclosing>.<n>` (v3), `spawn` entries → `lugha_task_<enclosing>.<n>` (v17), constants → globals `lugha_const_<key>` filled by `lugha_const_init` (v17), the private copy-on-write sink `lugha_cow_sink` (v17, programs with includes), actor message handlers → `lugha_actor_<Type>.<method>` (v17), interface dispatch tables → `lugha_vt_<Type>.<Interface>` (a generic interface with its arguments, `lugha_vt_Count.Source<i64>`, v6) with identities `lugha_type_<Type>` and box copies `lugha_copy_<type>_box` (v5), conversions between interfaces → `lugha_up_<J>.<I>` and derived tables `lugha_vt_<Type>.<J>.<I>` (v6), default methods → `lugha_fn_<Interface>.<method><Type>` (v5; a generic interface's arguments go on the interface, `lugha_fn_Source<i64>.skip<Count>`, v6), runtime → `lugha_rt_<name>`, copy helpers → `lugha_copy_<type>`, equality helpers → `lugha_eq_<type>`, externs verbatim (or the string of `extern "symbol" fun`, v16), callbacks without `data` → `lugha_cfn_<n>` reading the value stored in `lugha_cbv_<n>`, trampolines for callbacks with `data` → `lugha_cb_<signature>` (`lugha_cb_fun(GtkWidget, data)`, v16). *Why:* disjoint prefixes are what prevents symbol collisions (spec §8).
8. **Generated code allocates only through `lugha_rt_alloc`.** Never emit calls to `malloc`. *Why:* Boehm can't see `malloc` memory and will free live objects.
9. **Follow the milestone order (spec §11).** The lexer and parser cover the full §2–§3 syntax. The checker and codegen implement only the current milestone's subset; in milestones 1–2 treat every value as `i64`. *Why:* each milestone must end in a running program, while syntax is cheap to do once and fully specified.
10. **The §10 programs are the acceptance tests.** Their stdout and exit codes must match exactly. *Why:* they define "conforming compiler".

---

## ERROR HANDLING — NON-NEGOTIABLE

Expected failures return `Result<T, E>`. Panics are for bugs only.

```
Expected failure  →  return Err(...)
Truly unexpected  →  panic (invariant violated, programmer error)
```

```rust
// Good — the caller sees every failure mode in the type
pub fn parse_number(src: &str) -> Result<i64, ParseError> {
    src.trim().parse().map_err(|_| ParseError::InvalidNumber(src.to_owned()))
}

// Bad — hides a failure path and crashes on user input
pub fn parse_number(src: &str) -> i64 {
    src.trim().parse().unwrap()
}
```

- Each module defines its own error enum; errors are typed, never `String`, derived with `thiserror`.
- **Never** `.unwrap()` / `.expect()` on anything derived from external input. `.expect("reason")` is allowed only for true invariants, and the message states the invariant.
- **Never** return `Option` to signal an error — `None` cannot say *why*.
- **Never** swallow an error with `let _ =` or `.ok()` without a comment explaining why ignoring it is safe.
- Propagate with `?`; convert between error types with `From` impls, not ad-hoc `map_err` everywhere.

**Two kinds of error in this project — keep them apart:**
- **Errors in the user's `.la` program** (bad token, type mismatch) are expected output of the compiler. They become `Diagnostic` records (architecture rule 5) and the stage keeps going to find more where it can. Every stage returns `Result<(Output, Vec<Diagnostic>), Vec<Diagnostic>>`: `Ok` = output + warnings, `Err` = all errors and warnings.
- **Errors in lughac itself** (I/O failure, linker not found) use `thiserror` enums and `Result`. A violated compiler invariant is a bug → exit 2.

---

## FILE ORGANIZATION

```
.
├── CLAUDE.md        — this file
├── MEMORY.md        — resolved decisions + project state
├── CONTEXT.md       — session handoff log
├── DECISIONS.md     — open/deferred/resolved questions
├── CHANGELOG.md     — what shipped
├── TODO.md          — outstanding tasks
├── .llmignore       — protected paths
├── .claude/         — settings.json: harness deny rules mirroring .llmignore
├── .github/         — workflows/ci.yml: fmt, clippy, test in ubuntu:26.04 (LLVM 21), once per pull request or by hand; merges don't wait for it
├── README.md        — overview, prerequisites, usage, tests
├── Cargo.toml       — package `lugha`, binary `lughac`
├── runtime/         — the C runtime, embedded in lughac (DECISION-009): lugha_rt.c (core: includes, types,
│                      init, alloc, panics, strings), stack.c (overflow handler), print.c (formatting),
│                      input.c (args, parse_i64/f64, stdin lines, files, stderr, exit), format.c (format
│                      specifiers, v13), bytes.c (binary I/O, UTF-8 check, v14), chars.c (`char` printing and quoting, v14), random.c (`lugha_rt_os_seed`, v15), env.c (`lugha_rt_env`, `lugha_rt_cwd`, v15; `lugha_rt_chdir`, v16), fs.c (folders and `stat`, v15), process.c (`lugha_rt_spawn`: `posix_spawnp`, no shell, v15), test.c (`lugha_rt_test_is`/`_unknown`, v16), callbacks.c (`lugha_rt_cb_pin`/`_release`: closures C keeps, v16), cstr.c (`lugha_rt_str_from_c`: C's text copied, U+FFFD, v16), tasks.c (v17: fibers, `mmap`'d stacks, the x86-64 and AArch64 switches (milestone 133), the scheduler, the collector's view of every stack), spawn.c (v17: `lugha_rt_task_spawn`/`_join`/`_yield`/`_sleep`/`_preempt`), chans.c (v17: channels, `lugha_rt_task_select`), cmem.c (v17: `lugha_rt_c_read`/`_write`, copies through C pointers), actors.c (v17: mailboxes, `lugha_rt_actor_*`, `panic_end` ending only an actor), net.c (v17: TCP sockets, the `epoll` wait, `idle_wait` for the scheduler), resolve.c (v17: `lugha_rt_net_resolve`); fragments joined in that order
│                      into one lugha_rt.c (PRP-028)
├── third_party/     — vendored code, not ours: bdwgc/ (the Boehm collector, 8.2.12, unmodified; VERSION records the release, hash and file list; PRP-163)
├── rust-toolchain.toml — pinned Rust 1.99.0
├── src/
│   ├── lib.rs       — compiler library; SPEC (the embedded spec `lughac spec` prints), PRELUDE, STD (v15)
│   ├── explain.rs   — `EXPLANATIONS`: every code's page from docs/errors/, embedded; `explain`, `title` (v16)
│   ├── std/         — the standard library in Lugha (v15), embedded as `lugha::STD` and loaded only when imported:
│   │                  math.la (`std.math`: float functions over `T: Float` via `$float.*` built-ins, abs/min/max/clamp, pi/e),
│   │                  random.la (`std.random`: xoshiro256** `Rng`, `seeded`/`from_os` via `$os.seed`),
│   │                  env.la (`std.env`: `get`, `current_dir`, `set_current_dir` via `$os.env`/`$os.cwd`/`$os.chdir`),
│   │                  fs.la (`std.fs`: `list_dir`, `make_dir(s)`, `remove_file`, `remove_dir`, `exists`, `is_dir`, `is_file`),
│   │                  process.la (`std.process`: `run`, `run_with`, `call` via `$os.start`), task.la (`std.task`: `sleep`, `yield_now` via `$os.sleep`/`$os.yield`, v17), time.la (`std.time`: `now_ms`, `monotonic_ms`, v17), net.la (`std.net`: `Listener`, `Conn` over `$os.net_*`, v17), tls.la (`std.tls`: TLS over OpenSSL, always verified, v17), crypto.la (`std.crypto`: SHA-256, Ed25519, random bytes, `equal`, HKDF-SHA256, v17), toml.la (`std.toml`: TOML 1.0 parsing, v17) with the internal toml_value.la, toml_strings.la, toml_scan.la, toml_parse.la (`std.$toml_*`), http.la (`std.http`: HTTP/1.1 server and client, `https://`, v17), http_wire.la (`std.$http_wire`, internal: the reader and parser `std.http` uses)
│   ├── prelude/     — the prelude in Lugha, joined in this order into the one module `$prelude` (`lugha::PRELUDE`):
│   │                  core.la (Result<T, E>, Iter<T> (v6), the array helpers, `$concat` (v17), the string/Option/Result helpers),
│   │                  iter.la (iterator sources, adapters and consumers, v10; `Display`, v12), more.la (`zip` yielding
│   │                  tuples, `chain`, `take_while`, `skip_while`, `flat_map`, `min`, `max`, `position`, v13), text.la
│   │                  (`Chars`, `char_count`, `char_from`, Latin-1 classification and case, v14; `utf8_decode`, `text_from_utf8`, `text_parse_i64`/`u64`, v15), ops.la (the
│   │                  operator bounds `Add`…`Neg` and `Num`, v15; `enum Ordering` and `sort`, v15), maps.la (v17: `Map`, `Set`, `$map_of`/`$set_of`, the compact table over `$hash.value`)
│   ├── main.rs      — `lughac` entry point (CLI only)
│   ├── span.rs      — byte-offset Span
│   ├── formatter/   — `lughac fmt` (v16, milestone 80): mod.rs (`format`, `format_bundled`, `FormatError`, the `Printer`), doc.rs (the
│   │                  document printer: groups, 100 columns, hugging; v17: `Hug`/`Broken`/`Huggable`, `call_args`, Prettier's measuring), items.rs, stmts.rs, exprs.rs (precedence), leaves.rs (types,
│   │                  patterns, parameters), comments.rs (sequences, attached comments, verbatim fallback), verify.rs (re-parse, compare
│   │                  trees and comments), tests.rs (the style rules)
│   ├── format.rs    — format specifiers (v13): `parse`, `check`, shared by the checker and codegen
│   ├── template.rs  — `format` templates (v14): `pieces`, `holes`, `parts`, shared by the checker and codegen
│   ├── diagnostic.rs — Diagnostic record (spec §9 fields), no rendering
│   ├── lexer/       — mod.rs (lex, main loop), token.rs, punct.rs (operators), number.rs, string.rs,
│   │                  interp.rs (string interpolation, E0111), format_spec.rs (the `FmtSpec` token, v13), char_lit.rs (`char` literals, E0112, v14); mod.rs also skips block comments, nested, E0113, and `doc_comments` finds `/** */` spans (v16); string.rs also reads `format` templates raw (v14); `lex_bundled` lets `$` start a name in bundled sources (v15); `comments` lists every comment (v16, the formatter)
│   ├── ast/         — mod.rs (items, statements), types.rs (written types), expr.rs (expressions, ExprId, StructLiteral, Part),
│   │                  pattern.rs (arms, nested patterns, guards), header.rs (imports, type parameters, bounds,
│   │                  interfaces)
│   ├── parser/      — mod.rs (parse, cursor, errors; `parse_partial_from`: the recovered program for the language server, v16), recover.rs, describe.rs, operators.rs (operator tables, `>>` as two `>`), conditional.rs (`if let`, `while let`, `let … else`), self_type.rs (`Self` in `impl` blocks, v10, and in struct and enum declarations, v13),
│   │                  expr.rs (Pratt), primary.rs, stmt.rs, item.rs, decl.rs (enum, impl, receivers),
│   │                  pattern.rs (match, patterns, guards), generic.rs (type params, m.T / T<args>),
│   │                  import.rs (import, pub), interface.rs (interfaces, bounds), closure.rs (closure
│   │                  literals, E0209), sexp/ (mod.rs exprs,
│   │                  item.rs items and types), test_util.rs, tuples.rs (tuple types, literals, patterns, `t.0`, `for (i, w)`, v14), format.rs (`format("…", …)` with a literal template, v14), attributes.rs (`@test` before a `fun`, E0210, v16), hashed.rs (v17: `#{ … }` map and set literals, `HashLiteral`), out_params.rs (v17: contextual `out` in extern parameters and call arguments), consts.rs (v17: `const` items), tasks.rs (v17: `spawn call` wrapped in a closure, `select` arms, E0211), actors.rs (v17: `actor` → a struct marked `actor` plus an `impl` marked `actor`)
│   ├── check/       — mod.rs (check), checked.rs (the `Checked` output), test_util.rs, types.rs, env.rs (globals, main, scopes),
│   │                  expr.rs, literal.rs (literals; any literal as `T: Num`/`Float` and its range per instance, v15), call.rs, ops.rs (operators, casts), body.rs (function bodies), stmt.rs,
│   │                  assign.rs (places, mutability), flow.rs (returns, loops, W0101), access.rs
│   │                  (fields, indexing), array.rs (literals, for … of, E0507 guard), structs.rs
│   │                  (declarations, E0307/E0308, literals), liveness/ (mod.rs move on last use; walk.rs the walk), enums.rs
│   │                  (declarations), construct.rs (E.V, Some, None), matching.rs (match, guards, W0103), patterns.rs (pattern typing),
│   │                  usefulness.rs (E0417 witnesses, W0102), methods.rs (impl,
│   │                  E0310), method_calls.rs (calls, receivers, E0415, E0508), arguments.rs (call arguments, arity), builtins.rs (push, pop, clear), swap.rs (swap, E0509), conflicts.rs (E0510), modules/ (mod.rs check_modules,
│   │                  keys, passes; lookup.rs m.f / m.T, externs, main), visibility.rs (E0313/E0318), levels.rs (v8 visibility levels: private, package, public),
│   │                  struct_literal.rs (S { … }, m.S { … }), generics.rs (type parameters, E0315/E0316,
│   │                  E0418, substitution), bounds.rs (Eq/Ord, E0419), infer.rs (type arguments, E0412,
│   │                  Checked.instances), instances.rs (instance list, E0420, Checked.monos), layouts.rs
│   │                  (concrete generic types, substituted tables, Checked.used), interfaces.rs (declarations,
│   │                  interface bounds, E0315, E0423), satisfy.rs (calls through bounds E0421, satisfaction
│   │                  E0422), input.rs (args, parse_i64/f64, interpolation), fun_values.rs (function types
│   │                  and values, calls of values), closures.rs (closure types, captures, E0511), helpers.rs
│   │                  (xs.map(f) etc. as prelude calls), results.rs (Ok/Err, `?`, E0424), errors/ (mod.rs, names.rs E03xx, types.rs E04xx, places.rs E05xx/W01xx,
│   │                  generics.rs milestone 11–12 codes, v17.rs v17 codes, v4.rs v4 codes, v5.rs v5 codes, v6.rs v6 codes, v8.rs v8 visibility, v9.rs v9 codes, v10.rs v10 codes, v14.rs v14 codes, v16.rs v16 codes),
│   │                  iface_values.rs (interface types, E0425, conversion, Checked.conversions), iface_calls.rs
│   │                  (calls on interface values), iface_tables.rs (Checked.interfaces, conversion reachability), type_patterns.rs
│   │                  (`c: Circle` patterns, E0426), defaults.rs (default methods: `Self`, bodies, instances),
│   │                  applied.rs (generic interfaces: `Applied`, items at arguments), bound_args.rs (bounds with
│   │                  arguments, E0418/E0419), bound_infer.rs (inferring type arguments through bounds and
│   │                  interface-typed parameters), iterators.rs (`for … of` over `next`, `Iter`, Checked.iterations),
│   │                  self_type.rs (`Self` in interfaces: items for a type, E0427),
│   │                  upcast.rs (converting between interfaces: `undeclared`, E0422),
│   │                  numbers.rs (v7/v10 numeric types: `is_float`, `is_unsigned`, keyword types, `I.MIN`/`I.MAX`), slots.rs (enum payload slots),
│   │                  or_patterns.rs (or-pattern alternatives and bindings E0428, `mut` bindings), conditional.rs
│   │                  (`if let`, `while let`, `let … else`, E0429), includes.rs (v17: `include_bytes`/`include_str`: paths inside the package's folder, reading once, E0447/E0448, `Checked::includes`), concat.rs (v17: `xs + ys` as `$concat`, `T[]` satisfying `Add`), concrete_defaults.rs (default methods on
│   │                  concrete types, E0430), named_imports.rs (`import m { X }`, re-exports: aliases, `member`, E0318), globs.rs (v17: `public import m { * }`: public names, aliases, clashes), maps.rs (v17: `#{ … }` as `$map_of`/`$set_of` calls, `m[k] = v`, E0441/W0105, the `$equal` pseudo-use and `same_instances`), hashing.rs (v17: `$hash.value`), out_params.rs (v17: `out` arguments: `extern_inputs`, E0443), unit.rs (v17: the unit type `()`, W0106), consts.rs (v17: global constants: collection, values E0445, order and cycles E0446), printing.rs (what prints, `show` instances, `$print` uses), for_mut.rs (`for x of mut e`: place checks, E0501/E0502/E0506), iter_helpers.rs (iterator helpers as prelude calls, prelude iterator type names), tuples.rs (tuples as `$tuple<n>` structs, patterns, E0433, v14), templates.rs (`format`: holes against arguments, E0432, v14), operators.rs (v15: operators as method calls, built-in satisfaction of the operator bounds, `Num`; operators_tests.rs), impl_bounds.rs (v15: bounds on `impl` blocks), float_ops.rs (v15: `$float.<op>` and `$os.seed` built-ins, `float_op`, `os_op`), float_bits.rs (v17: `f64.from_bits`, `bits_of`), ordering.rs (v15: `cmp` makes a type `Ord`, `has_cmp`, `order_uses`, `cmp_targets`, `ORDERING`), reexports.rs (`as`, whole-module re-exports: `module_path`, `member_through`, `m.v.X`, v14), refs.rs (v16: `Ref<T>`: `Ref.new`, `predeclared_ref`, `REF_NEW`; tests), handles.rs (v16: `extern type`: the `handles` pass; tests), callbacks.rs (v16: `CParam`, `CParams`, `extern_params`: callbacks, `data`, `release` in extern signatures, E0435; `static_callbacks` E0436), test_fns.rs (v16: `@test` shapes and calls E0434, `assert`/`assert_eq`/`assert_ne`, `tests_of`; `check_tests` in modules/), tasks.rs (v17: `spawn` → `Task<T>`, predeclared `Task`/`Chan` types, E0438 for a spawned change to a copy), actors.rs (v17: `Type::Actor` handles, `spawn` of an actor, messages, `handle_members`, E0439), channels.rs (v17: `Chan.new`/`buffered`, `CHAN_NEW`), select.rs (v17: `select` arms, E0438), signatures.rs (v16: `Sig`: functions' and methods' parameter names and types, filled in for a generic call's instance), members.rs (v16: `TypeMembers`, `value_members`, `type_members`: what can follow `.`, built-ins filtered through the checker's resolvers, for completion), member_names.rs (the candidate built-in and helper names `members.rs` filters), partial.rs (v16: `check_partial`, `Partial`, `Name`, `Target`, `variants` at W0103: the checker's findings even with errors, for hover and go-to-definition; the `Recorder` of names, definitions, field and variant declarations and expression spans)
│   ├── codegen/     — mod.rs (emit_ir, emit_object, CodegenError), lower.rs (module, C main),
│   │                  value.rs (Value, Type → LLVM), scope.rs (locals), expr.rs, arith.rs (ops at
│   │                  each width), cast.rs (§4 casts), control.rs (blocks, if, loops, jumps), stmt.rs,
│   │                  function.rs (bodies, return, calls), declare.rs (signatures, symbols, C ABI), runtime.rs (intrinsics, string
│   │                  literals, panics), heap.rs (headers, element layout, bounds checks, string ops),
│   │                  array.rs (allocation, list/repeat literals, for … of), copy.rs (copy sites, clone helpers, mangle), cow.rs (copy-on-write: the shared flag, `own`), places.rs (write places: plan, commit, `for … of mut` aliases, `own_place`; `mut self` receivers planned before their arguments, v15), hoist.rs (owning arrays once per loop: `hoist_loop`, fixed paths, guarded hoists, `hoisted`, v15), hoist_scan.rs (the loop scan: roots that may share, changed and bound names, written arrays), equality.rs (deep ==, lugha_eq_*),
│   │                  structs.rs (named types, layout, literals, fields, place addresses), enums.rs
│   │                  (enum layout, Option types, payload addresses), growth.rs (push, pop, clear), construct.rs (building enum values,
│   │                  indirect slots), matching.rs (match lowering, switch), patterns.rs (nested/guarded arms),
│   │                  methods.rs (method symbols, receivers, calls), input.rs (interpolation, args, parsing),
│   │                  io.rs (read_line, files, eprint, exit), fun_values.rs ({ code, env } values, lugha_ref_
│   │                  wrappers, indirect calls), closures.rs
│   │                  (environments, lugha_closure_ bodies), results.rs (`?`: branch and early return),
│   │                  interfaces.rs (interface values: conversion, boxes, lugha_vt_ tables, box copies), dispatch.rs
│   │                  (dynamic calls, interface copies, type-pattern tests), defaults.rs (default instances, `?` boxing),
│   │                  instances.rs (generic instances:
│   │                  substitution, instance symbols),
│   │                  swap.rs (swap: loads and crossed stores), frames.rs (v17: struct and array slots shared where their live blocks never meet — before -O0's passes, after -O2's — and `probe-stack` on every function; frames_phi.rs: slots joined by a `phi` merged first; frames_tests.rs), sret.rs (v17: struct results through a hidden `sret` pointer, one pass over the lowered module), rebuild.rs (v17: a function rebuilt with a new type, the old one deleted), split.rs (v17: struct values split into fields; split_tests.rs; split_big.rs: big ones by `memcpy` and field loads), split_leaves.rs (v17: leaves, paths, block order, alignment), split_params.rs (v17: struct parameters as fields, `zeroext` `bool` results), bits.rs (bit operators, the shift range check), int_methods.rs (wrapping and counting methods), int_checked.rs (v10 checked/saturating/rotating methods, `I.MIN`/`I.MAX`), or_patterns.rs (alternatives in the arm chain, `mut` copies), format.rs (format specifiers → `lugha_rt_fmt_*`), show.rs (printing: `text`, `lugha_show_<type>` helpers), show_iface.rs (printing interface values by identity), unwrap.rs (`unwrap`/`expect`), conditional.rs (`if let`, `while let`, `let … else`), iterators.rs (iterator loops: a hidden copy, `next` until `None`), tuples.rs (tuple literals, patterns, printing, `lugha_cmp_` order), operators.rs (v15: operator method calls; numbers keep the built-in operators), float_ops.rs (v15: `$float.<op>` → `llvm.<op>.f32/f64`), os.rs (v15: `$os.<op>` → runtime calls with flags and out-slots), ordering.rs (v15: `<` through `cmp`, `user_order`), refs.rs (v16: `Ref.new`, `get`/`set`/`update`/`is`, `show_box` with the cycle stack), externs.rs (v16: `Option` of a handle or string ↔ a nullable C pointer, `value_from_c` copies C's text), c_abi.rs (v16: x86-64 System V classification of extern structs: `Piece`, `Passing`, `Free` registers), c_abi_arm.rs (v17: AAPCS64 classification and signatures: `ArmPassing`, HFAs, `Indirect`), c_calls.rs (v16: `CSig`: clang's lowering — split pieces, `byval`, `sret` — for calls into C and entries C calls), callbacks.rs (v16: `callback_arguments`, `lugha_cfn_`/`lugha_cbv_` entries, `lugha_cb_` trampolines, pinning), assertions.rs (v16: `assert*` → branch and panic with both values), hashing.rs (v17: `$hash.value` → `lugha_hash_<type>`, FNV-1a), maps.rs (v17: literals, `m[k]` with its panic at the `[`, `m[k] = v`, `==` through `same`), out_params.rs (v17: `out` slots and the write-backs after an extern call), net.rs (v17: `$os.net_*` → `lugha_rt_net_*`), consts.rs (v17: `lugha_const_` globals, `lugha_const_init`, reads), test_main.rs (v16: a test executable's `main`, `LUGHA_TEST`), tasks.rs (v17: `spawn` → `lugha_task_` entries and early arguments (`presets`), `join`, `$os.sleep`/`yield`, `preempt_point`), actors.rs (v17: `lugha_actor_` handlers, calls and casts on handles, `spawn_actor`), c_arrays.rs (v17: callback arrays — C's pointer and count, `counted_from_c`, `copy_back`), channels.rs (v17: channel calls, `select`'s case table and switch), concat.rs (v17: `xs + ys` → the `$concat` instance), targets.rs (v17: `Target`, `by_value_errors` → E0449), includes.rs (v17: included files as private constant globals, a `u8[]` born shared; cow.rs's `lugha_cow_sink`),
│   │                  upcast.rs (between interfaces: table registry, derived tables, `lugha_up_` lookups)
│   ├── modules/     — mod.rs (load: the main file and every import, one offset space; with_prelude), resolve.rs
│   │                  (import checks, E0312/E0314/E0319), manifest.rs (`lugha.toml`, E0321; `[link]`, v16), link_table.rs (v16: `LinkTable`, `PackageLink`), link_tests.rs (v16: which `[link]` tables a build gets, E0322), inline.rs (inline
│   │                  entries `{ path, version }`, v11), version.rs (versions, requirements, E0324), packages.rs
│   │                  (manifests of a build, package imports, E0320/E0322/E0323, version checks), tests.rs
│   │                  (in-memory loading), version_tests.rs (loading with versions), git.rs (v14: URL checks, `ls-remote`
│   │                  tags, the `Git` trait, `SystemGit` with fixed `-c` flags and env), cache.rs (cache root, escaped
│   │                  folder names, scratch clones, install), lock.rs (`lugha.lock`, E0321), fetch.rs (`Fetcher`, `Mode`:
│   │                  lock, cache, clone, E0325/E0326; fetch_tests.rs with a fake `Git`), parse.rs (one module: decode, lex,
│   │                  parse), paths.rs (`exists`, `normalized`, E0320), package_table.rs (v15: `[package]` version, name,
│   │                  source), layout.rs (v15: `LoadEnv`, the bounded manifest lookup, `name`/`source`, the implicit
│   │                  package, E0327/E0328, `LoadEnv.partial` keeping broken modules' programs for the language server (v16); layout_tests.rs), std_tests.rs (v15: `std` modules from `lugha::STD`, reserved `std`), roots_tests.rs (v17: several roots in one load, `load_all`), entry_tests.rs (v17: `import N;` → `lib.la`)
│   ├── link/        — mod.rs (`cc … runtime.o gc.o -lm -o out`, errors.rs (`LinkError` → E0330–E0332, v17), compiler.rs (v17: `Compiler`, `$LUGHA_CC`'s words), `link_with`, RUNTIME_PARTS, RUNTIME_SOURCE, RUNTIME_SYMBOLS, LinkError → E0330/E0331),
│   │                  inputs.rs (v16: the `Plan` from `[link]` tables: `-L`/`-l` in order, each once; C files with their flags; E0329),
│   │                  pkg_config.rs (v16: running `pkg-config`, the allowed flags), c_files.rs (v16: `cc -c` each C file), gc.rs (v17: the bundled collector: `GC_FILES` embedded from third_party/bdwgc/, restored in `<cache root>/gc/<version>-<hash>/`, compiled once; `bundled()` for harnesses), object_cache.rs (v17: objects kept between builds, `cc -MD` headers, FNV-1a; object_cache_tests.rs), runtime.rs (v17: the runtime temp file, its own cached `cc -c`, `TLS_SOURCE` and `CRYPTO_SOURCE` appended for the programs that load them, `missing_openssl` → E0332)
│   └── driver/      — mod.rs (clap CLI, exit codes, Output), build.rs (build, --emit per module), guard.rs
│                      (panics → internal error, exit 2), pipeline.rs (load all modules + the prelude, check, codegen),
│                      source.rs (source map of all files, line_col),
│                      render.rs (codespan, Report), json.rs (JSON lines), update.rs (v14: the fetcher, writing
│                      `lugha.lock`, `lughac update`), init.rs (v16: `lughac init`, the program and library layouts), test.rs (v16: `lughac test`: files, parallel isolated runs, timeouts, the report), test_build.rs (v17: one executable per package folder, a broken file checked alone), test_json.rs (v16: `--list`, `--json` lines, panic locations; v17: skips, package-aware module names), explain.rs (v16: `lughac explain`, the hint after diagnostics), fmt.rs (v16: `lughac fmt`: paths, `--check`, temp file + rename),
│                      lsp/ (v16: `lughac lsp`: mod.rs the stdio loop and capabilities, server.rs state and debounced
│                      scheduling, documents.rs open text and `file:` URIs, targets.rs which mains to check, check.rs one
│                      guarded offline check via `pipeline::front_partial`, publish.rs reports → UTF-16 LSP diagnostics, per-check store, hover.rs `Snapshot` and the hover under a position, definition.rs the declaration of the name under a position, completion.rs members and names from the last check that parsed (`Here`, `Receiver`, shared with signature help), signature.rs the call around the cursor and its signature, docs.rs doc comments: parsing, `@param`/`@returns`, attachment, a per-snapshot cache, module docs (v17), snippets.rs call placeholders and the postfix `.match`, code_actions.rs quick fixes for E0417 (and dispatch), fixes.rs `let mut`, a misspelled variant and a std import (v16, milestone 82), references.rs every place resolving to one declaration across checks (`Key`, `find`), reexports.rs completion of a module's named and glob re-exports (v17), rename.rs prepare/rename with the refusals and the in-memory re-check (v16, milestone 83), formatting.rs Format Document through the formatter (v16, milestone 81))
├── tests/
│   ├── programs.rs  — end-to-end acceptance test (runs every tests/programs/ case)
│   ├── spec.rs      — spec §10 acceptance: programs and expected results extracted from lugha::SPEC
│   ├── lexer.rs     — lexer public-API tests (every spec program lexes)
│   ├── parser.rs    — parser public-API tests (every spec program parses)
│   ├── codegen.rs   — builds and runs real executables at -O0 and -O2
│   ├── cli.rs       — the lughac binary: commands, emit, diagnostics, exit codes
│   ├── modules.rs   — programs of several files through lughac (emit, per-file diagnostics, symbols)
│   ├── interface_values.rs — interface values through lughac (check, run, IR tables)
│   ├── tasks_runtime.rs — the green-thread runtime through a C harness against `lugha_rt.c`: order, rounding mode across switches, sleeps, channels, `select`, suspended stacks and the collector, preemption, 10 000 threads, deadlock and overflow panics; actors' calls, casts, stop, isolated panics (v17)
│   ├── includes.rs  — `include_bytes`/`include_str` through lughac: symlinks in and out, the manifest folder, a package's own files, the 16 MiB cap, rereads, the read-only global (v17)
│   ├── arm.rs       — Linux on ARM64 (v17): `tests/programs` cases and the `extern_structs` shapes built with `--target aarch64-unknown-linux-gnu` and run under qemu-aarch64 (skips without the cross gcc and qemu)
│   ├── fs_time.rs   — `fs.rename`, `fs.set_mode`, `flush()` and `std.time` through lughac (v17)
│   ├── toml.rs      — `std.toml` through lughac: toml-test v2.2.0's TOML 1.0 cases (`tests/fixtures/toml-test`, MIT), limits, hostile input, error positions, helpers (v17)
│   ├── crypto.rs    — `std.crypto` through lughac: FIPS 180-4 and RFC 8032 vectors, every rejection, random bytes, `equal`, abandoned hashers freed (v17)
│   ├── tls.rs       — `std.tls` and `https://` through lughac with per-run certificates from `openssl`: verification failures, `ca_file`, `serve_tls` (v17)
│   ├── http.rs      — `std.http` through lughac: a server sent raw good and hostile requests, the client against a raw Rust server (v17)
│   ├── net.rs       — `std.net` through lughac on loopback: echo, 200 concurrent clients, timeouts and errors, IPv6, `main` alone (v17)
│   ├── c_cache.rs   — the C object cache through lughac with a logging `cc`: hits, edits to a header/source/flag, damaged entries, an unwritable cache, TLS runtime entries, E0332 (v17)
│   ├── link_libs.rs — `[link]` through lughac: a static library built by the test, a package's table, C files, a fake `pkg-config`, E0321/E0322/E0329–E0331, `check` running nothing (v16)
│   ├── git_packages.rs — git packages through lughac and a real `file://` repository (lock, offline, E0325/E0326)
│   ├── lazy_std.rs  — lazy std modules through lughac: only reached std functions in the IR (v15)
│   ├── ordering.rs  — user-defined order through lughac: `<` and tuple helpers call `cmp` (v15)
│   ├── fmt.rs       — `lughac fmt` through lughac: in place, folders, `--check`, untouched files, syntax errors (v16)
│   ├── fmt_corpus.rs — every program in tests/programs, the spec and std/prelude formats, verifies, is idempotent (v16)
│   ├── explain.rs   — every code explained, every page example checked, `lughac explain`, the hint (v16)
│   ├── test_groups.rs — `lughac test` sharing one build per package folder: files kept apart, a broken file alone, filters, shared constants (v17)
│   ├── test_runner.rs — `lughac test` through lughac: pass/fail, isolation (overflow, bounds, stack overflow), timeouts, `--filter`, compile errors (v16)
│   ├── init.rs      — `lughac init` through lughac: both layouts created, checked and run; refusals; kept files (v16)
│   ├── lsp.rs       — `lughac lsp` over real stdio: live diagnostics, unsaved text, modules through seen mains, entries, offline (v16)
│   ├── lsp_hover.rs — hover through `lughac lsp`: bindings, functions and methods, fields, expressions, notes, with errors (v16)
│   ├── lsp_definition.rs — go-to-definition through `lughac lsp`: names, types, fields, variants, modules, none for std (v16)
│   ├── lsp_code_actions.rs — quick fixes through `lughac lsp`: missing arms in full, a `_` arm, the fixed program checks (v16)
│   ├── lsp_references.rs — Find References through `lughac lsp`: locals, functions across files, types, fields, variants, methods, modules, std (v16)
│   ├── lsp_rename.rs — Rename through `lughac lsp`: edits applied and checked; every refusal, the self-check's clash and capture (v16)
│   ├── lsp_formatting.rs — Format Document through `lughac lsp`: unsaved text as `lughac fmt` would, none when formatted, quiet on syntax errors, no file written (v16)
│   ├── extern_structs.rs — `extern struct` by value through lughac against C code: every shape, `byval`/`sret`, full registers, callbacks (v16)
│   ├── c_strings.rs — strings from C through lughac: copies, `NULL`, U+FFFD by the prelude's rules, callbacks, the `NULL` panic (v16)
│   ├── out_params.rs — `out` parameters through lughac against C code via `[link]`: numbers, `bool`, handles, `Option` NULL, extern structs, fields and elements, E0443 (v17)
│   ├── c_arrays.rs — arrays and unions at the C boundary through lughac (v17)
│   ├── c_memory.rs — `CPtr`, `c_read`/`c_write`, callback arrays through lughac (v17)
│   ├── callbacks.rs — handles and callbacks through lughac, each case with its own C file via `[link]`: `NULL`, closures across forced collections, `release`, renamed externs, E0409/E0435/E0436 (v16)
│   ├── lsp_snippets.rs — the server's snippets: call placeholders, postfix `.match`, client support (v16)
│   ├── snippets.rs  — the extension's static snippets expand and pass `lughac check` (v16)
│   ├── lsp_docs.rs  — doc comments through `lughac lsp`: hover, completion, signature help, type and variant names, the library (v16)
│   ├── lsp_partial.rs — the language server in files with syntax errors: only syntax errors shown, hover, definitions, completion, signature help (v16)
│   ├── lsp_signature.rs — signature help through `lughac lsp`: functions, generics filled in, methods, modules, variants, built-ins, intrinsics (v16)
│   ├── lsp_completion.rs — completion through `lughac lsp`: members, modules, variants, names in scope, while the text doesn't parse (v16)
│   ├── package_layout.rs — package layout through lughac: `source`, `examples/` by name, the `$HOME` bound, E0327/E0328
│   ├── common/      — c_shapes.rs: the extern struct shapes (C side, declarations, programs) shared by extern_structs.rs and arm.rs (v17); spec_programs.rs: extracts the §10/§11 programs from the embedded spec; lsp_client.rs: a `lughac lsp` client over stdio
│   ├── programs/    — m1/ … m68/, m76/, m84/, m88/, m91/, m92/, m100/, m101/, m107/, m108/, m109/, m118/, m122/, m123/, m128/, m131/, m135/, v1_1/, v1_2/, v4_1/: <name>.la + .stdout/.exit/.stderr expectations (+ .args, .stdin, .scratch);
│   │                  a directory <name>/ with main.la (+ modules) and <name>.* beside it is one case
│   └── support/     — fixture.rs (temp dirs), runner/ (discover, dirs (directory cases), args (.args/.stdin/.scratch/.envvars),
│                      execute, report)
├── editors/
│   └── vscode/      — the VS Code extension (PRP-099): package.json (language, grammar, settings `lugha.path` (machine scope)
│                      and `lugha.entries`), language-configuration.json, syntaxes/lugha.tmLanguage.json, snippets/lugha.json (declarations and control flow, v16); src/ extension.ts
│                      (imports `vscode`: a `LanguageClient` running `lugha.path lsp`), server.ts (the server
│                      command, start-failure text), testing.ts (imports `vscode`: the Testing view's controller, `lughac test`
│                      in trusted workspaces, v16), tests.ts (pure: `--list`/`--json` lines, arguments, outcomes), explain.ts (pure: `lugha-explain:` documents); test/
│                      (`node:test`, spec keywords, tests.ts, grammar/ scope tests); not in the crate
├── PRPs/            — feature briefs prp-{NNN}-{feature_name}.md (+ TEMPLATE.md, DISCOVERY.md)
├── docs/            — errors/ (one page per diagnostic code, `lughac explain`; a new code needs one), CODE_STYLE.md, setup.md (the context-system guide), source/, decisions/, incidents/, status/
│   └── specs/       — Language v17 Specification.md (the language definition, in progress, printed by `lughac spec`);
│                      Language v17 Draft.md (approved v17 plan, milestones 91–92: green threads, actors); Language v16 Draft.md (approved v16 plan, milestones 67–90); Language v15 Draft.md (approved v15 plan, milestones 51–66);
│                      Language v14 Draft.md (approved v14 plan, milestones 45–50);
│                      Language v13 Draft.md (approved v13 plan, milestones 41–44);
│                      Language v12 Draft.md (approved v12 plan, milestones 39–40);
│                      Language v11 Draft.md (approved v11 plan, milestones 37–38);
│                      Language v10 Draft.md (approved v10 plan, milestones 34–36); Language v9 Draft.md (approved v9 plan, milestones 31–33); Language v8 Draft.md (approved v8 plan, milestone 30); Language v7 Draft.md (approved v7 plan, milestones 27–29); Language v6 Draft.md (approved v6 plan, milestones 23–26); Language v5 Draft.md (approved v5 plan, milestones 20–22); Language v4 Draft.md (approved v4 plan, milestones 17–19); v3 and v2 Drafts (past plans);
│                      Language v16, v15, v14, v13, v12, v11, v10, v9, v8, v7, v6, v5, v4, v3, v2, v1 (v1.2) and v0 Specification.md (frozen)
└── reports/         — EOD reports (YYYY-MM-DD.md)
```

Update this tree when modules or `tests/` are added.

---

## ANTI-PATTERNS

1. **Never add a crate without a PRP line justifying it.** Dependencies are permanent cost and licensing risk.
2. **Never write `unsafe` without human approval.** It voids the guarantees every other rule relies on.
3. **Never silence a clippy lint with `#[allow(...)]` without a comment saying why.** Silent allows hide real bugs.
4. **Never add an implicit conversion or coercion.** Spec §4 forbids them, except the one it names: a value to an interface where one is expected (v5, recorded in `Checked.conversions`). Every other one is a type hole the checker can't see.
5. **Never implement a §11 "out of scope" feature** (a package registry, `ssh` URLs, a default entry point, full Unicode tables, streams…) even partially, nor a v17 feature that `Language v17 Draft.md` doesn't plan yet. Changes to how `git` runs (`modules/git.rs`) need the user's security review, as PRP-079 had, and so do changes to how programs are started (`runtime/process.c`, PRP-095) and to how `pkg-config` and `cc` run on a package's `[link]` input (`src/link/`, PRP-117).
6. **Never stop the checker at the first error.** Spec §9 requires reporting as many as it can find.
7. **Never print diagnostics directly from a stage.** Stages return records; only the driver renders. Otherwise human and JSON output diverge.

---

## KNOWN ISSUES — DO NOT FIX

These look like bugs but are specified behavior:

- **A function whose only exit is a `return` inside `while true` is rejected** as missing a return. Spec §6 rejects it on purpose to keep return checking simple; users add `panic("unreachable");`.
- **Integer overflow always panics**, even at `-O2`. Spec §12 decided against wrapping.
- **Boehm may keep garbage alive** when an integer looks like a pointer. Conservative GC, spec §7.
- **Extern C code keeping a Lugha pointer is undefined behavior.** Spec §7–8 accept this for v0.
- **`as` casts truncate/saturate silently.** The only place values wrap (spec §4).
