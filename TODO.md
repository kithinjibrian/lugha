# TODO — lugha

What's next. Shipped work is in CHANGELOG.md (and the spec's §11 milestone table); questions that need a decision go in DECISIONS.md. Every feature below needs a PRP before code. Done items are removed, not ticked.

---

## Known bugs and limits

- [ ] Copy-on-write: `x = f(x)` where `f` takes `x` by value and changes it copies `x`'s arrays every time (the argument isn't treated as a move although `x` is overwritten); worked around in the prelude.
- [ ] `None` in a closure's result inside a generic function, with nothing fixing its type, is still E0412 (found testing milestone 94).

## Language and library

- [ ] **JSON, later:** per-field options (renaming, skipping, defaults), an `@json` attribute, base64 bytes; inference through `.unwrap()`/`.expect()` (only `?` and annotations pass a type to `json.decode` today).
- [ ] **Boxes and bytes** (v16 §12): editing a box in place (`r.with`); a bounds-checked byte buffer.
- [ ] **`std.process`:** streaming pipes to a running child and timeouts (found with oyster). Needs the user's security review.
- [ ] **File streams:** open a file, read it line by line or in chunks, write incrementally, close.
- [ ] **A separate debug form, and parsing values back from text.** Also module paths in printed type names, so two modules' `Point`s print differently.
- [ ] **One-element tuples.**
- [ ] **Explicit type arguments and `.len()`.**
- [ ] **Format specifiers chosen at run time.**
- [ ] **Interfaces implemented once per type argument (`From<T>`).** Needs overloading by type.
- [ ] **Literal patterns on `f64`.**
- [ ] **Warnings:** an irrefutable `if let`, and a `show` of the wrong shape.
- [ ] **Small values stored inline in interface values** (an optimisation only).
- [ ] **Packages, more:** a registry; `ssh` URLs; branches or commits as requirements; two versions of one package in a build; full version ranges and pre-release versions.
- [ ] **Imports, more:** re-export chains (`m.v.w.X`), a `--package` flag, package-level (not `public`) re-exports.
- [ ] **Listing a git package's `extern` functions at `lughac update`.** Needs a security review, like PRP-079.
- [ ] **Checking the program's own `[package]` version,** e.g. against a minimum `lughac` version.
- [ ] **Spec housekeeping:** tick §12 entries that later versions settled: `%=` and `mut` bindings (v9), re-exports (v11), iterating a copy / `for … of mut` (v13), Unicode text (v14).
- Ruled out by the user (session 127): full Unicode `char` classification and case tables (§11's exclusion stands).

## Concurrency (v17)

- [ ] Built-in supervisors (restart policies), links/monitors; generic actors; actors as interface values; bounded mailboxes; actors on several OS threads; a stack overflow inside an actor ending only that actor.
- [ ] Non-blocking input and output (a waiting read pauses only its green thread); green threads on several OS threads.

## Portability — one platform at a time

Done: portable x86-64 binaries (`--cpu`), the collector built in, `$LUGHA_CC`, Linux on ARM64 with AAPCS64 struct passing (milestones 130–134). Stages B3–E are probably v18 milestones (a `Language v18 Draft.md`).

- [ ] **B3 — Building and testing for Linux ARM:** `run`/`test` under `qemu-aarch64`, ARM CI; `[build]` in `lugha.toml` (`cpu`, target).
- [ ] **C1 — Runtime split by OS:** shared, POSIX and per-OS files (event waiting behind `lugha_rt_poll_*`).
- [ ] **C2 — Mac runtime:** `kqueue` for `net.c`; `spawn.c`'s `MAP_STACK`, `random.c`'s `getrandom`.
- [ ] **C3 — Mac calling rules and linking:** Apple's AArch64 differences (variadics), Mach-O, signing; built and tested on a Mac runner.
- [ ] **D1–D3 — Windows:** runtime (files, processes, env, stack overflow), sockets, Windows x64 calling rules, `.exe` linking with MinGW first. Large.
- [ ] **E — WebAssembly (wasm32-wasi):** decisions first (garbage collector, green threads or none, which std modules exist). Large.

## Editor support and tools

- [ ] **Expression error nodes** — keep a broken statement with a hole, so `foo(p).` on the broken line completes too.
- [ ] **Lexer recovery** — tokens up to an unclosed string, so a fresh `"` without auto-closing doesn't lose the tree.
- [ ] Format Selection (the statements or items a selection touches).
- [ ] Formatter: break long binary chains and method chains; `fmt --check` in CI once the repo's own `.la` files are formatted (moves panic lines).
- [ ] Later quick fixes: did-you-mean for any undefined name (E0301), an import for a user module.
- [ ] **Go to Implementations** — from an interface method to every type's.
- [ ] **A read-only view of bundled sources** (`std`, the prelude) for hover and definitions that land there.
- [ ] **Bundling** the extension (vsce warns about 231 files; needs a bundler such as esbuild as a dev dependency).
- [ ] **Publishing** the extension to the Marketplace and Open VSX (publisher accounts, a token as a CI secret).
- [ ] Tests in the editor, later: a Debug profile (needs debug info), test items from unsaved text, UTF-16 columns from `lughac test --list`.
- [ ] **Outline and breadcrumbs** (`textDocument/documentSymbol`).
- [ ] **Go to Symbol in Workspace** (`workspace/symbol`, `Ctrl+T`).
- [ ] **Highlight uses** (`textDocument/documentHighlight`).
- [ ] **Inlay hints** (`textDocument/inlayHint`) — a `let`'s inferred type and parameter names at calls.
- [ ] **Semantic highlighting** (`textDocument/semanticTokens`).
- [ ] **Run from the editor** — a "▶ Run" code lens running `lughac run`; needs a security review (it starts programs).
- [ ] **Debugging** — breakpoints and stepping through a Debug Adapter; needs debug info (DWARF) from codegen first. Large.
- [ ] **Call hierarchy** (`callHierarchy/*`).
- [ ] **Go to Type Definition** (`textDocument/typeDefinition`).
- [ ] **Smart expand selection** (`textDocument/selectionRange`).
- [ ] **Folding by syntax** (`textDocument/foldingRange`).
- [ ] **A `lughac doc` tool** (HTML from doc comments).

## Ecosystem

- [ ] lugha-gtk: more widgets (CheckButton, Scale, DropDown, ListView), layout (Grid), dialogs, CSS.

## Ideas — noted, not scheduled

- **Decorators:** user-written `@name` attributes that wrap a function, as Python's do (`@logged`, `@memo`); `@` is already reserved for them. Needs a way to wrap any signature (variadic generics or compile-time rewriting). Also `@test(skip)`-style arguments.
- **Printing, `==` and hashing on derive blocks** (after milestone 142): rewrite the compiler's per-type code for them as derivable interfaces in the prelude, as milestone 141 did for JSON.
- **`const` calling pure functions** (DECISIONS 208): work before the program runs, through an LLVM JIT that reuses codegen; needs core decision 4's "no interpreter" amended.
- **`@cfg` on items** (DECISIONS 208): code for one OS or architecture, every target checked; with portability stage C1.
