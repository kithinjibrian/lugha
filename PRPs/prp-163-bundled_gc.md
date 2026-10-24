## FEATURE: lughac carries the Boehm collector's source and compiles it into every program, so neither lughac's users nor its programs need the system's `libgc`; milestone 131 (v17), portability roadmap step A2.

**Status:** approved 2026-10-24 ("build", session 313; the SHA-256 check stays on the acceptance list for the user before merging); proposed 2026-10-24 (session 312). **Needs the user's security review**: it adds about 1.3 MB of third-party C (bdwgc 8.2.12) that lughac compiles and links into every program. Discovery: (1) the source is vendored unmodified in `third_party/bdwgc/` and embedded like the runtime; (2) it is built without OS-thread support; (3) there is no fallback to the system's `libgc`.

## OBJECTIVE
Today every program is linked with `-lgc`. That needs `libgc-dev` wherever lughac runs, and `libgc.so` wherever the program runs. A cross build (roadmap stages B–D) would also need a `libgc` built for the target, which is the hardest part to find. After this change lughac compiles the collector from source once, caches the object, and links it in statically. `ldd ./prog` no longer lists `libgc`, and lughac works on a machine without `libgc-dev`.

## CONTEXT
- **The experiment (session 312, scratchpad):**
  - Release `gc-8.2.12.tar.gz`, SHA-256 `42e5194ad06ab6ffb806c83eb99c03462b495d979cda782f3c72c08af833cd4e`, the version Ubuntu 26.04 ships.
  - Compiled as one file, `extra/gc.c`, with `cc -c -O2 -fno-strict-aliasing -DALWAYS_SMALL_CLEAR_STACK`: 3.4 s, a 216 KB object.
  - With a `cc` wrapper replacing `-lgc` by that object, these suites passed: `programs`, `spec`, `tasks_runtime`, `codegen`, `callbacks`, `c_memory`, `c_strings`, `extern_structs`, `net`, `http`, `crypto`.
  - **`ALWAYS_SMALL_CLEAR_STACK` is required.** Without threads, `GC_clear_stack` clears down to a high-water mark recorded on the main stack. Called on a green thread's 1 MiB stack, it recursed off the end, and a program allocating in two `spawn`ed threads died with `panic: stack overflow`. Ubuntu's threaded build doesn't use that code path.
- **Starting state:**
  - `src/link/mod.rs` `args` adds `-lgc -lm`.
  - `runtime/lugha_rt.c` includes `<gc.h>` and `<gc/gc_mark.h>` from `/usr/include`.
  - `src/link/runtime.rs` compiles the runtime through the object cache (`src/link/object_cache.rs`, milestone 127).
  - `tests/tasks_runtime.rs` compiles its C harness with `-lgc`.
  - CI, README and CLAUDE.md list `libgc-dev`.
- **Ending state:**
  - `third_party/bdwgc/`: the about 45 files `extra/gc.c` reads (the list `cc -MD` gave), byte for byte from the release. Also upstream's `README.md`, which carries the licence notice, and `VERSION`: version, URL, SHA-256, the date copied, and the build flags.
  - `src/link/gc.rs` (new):
    - `GC_FILES`, each file as `(path, include_str!(…))`.
    - `GC_FLAGS`: `-O2 -fno-strict-aliasing -DALWAYS_SMALL_CLEAR_STACK`.
    - `GC_VERSION`.
    - `pub fn compile(cache) -> Result<GcObject, LinkError>`: writes the files to `<cache root>/gc/<hash of GC_FILES>/`, then compiles `extra/gc.c` through the object cache. The key covers every file's text and `GC_FLAGS`, so it compiles once per bdwgc version and flag set.
    - `GcObject` holds the object path and the include folder.
  - `src/link/runtime.rs` compiles the runtime with `-I <gc include folder>`.
  - `runtime/lugha_rt.c` includes `<gc.h>` and `<gc_mark.h>` (bdwgc's source layout has no `gc/` folder).
  - `src/link/mod.rs` links the GC object in place of `-lgc` and keeps `-lm`.
  - `tests/tasks_runtime.rs` uses `lugha::link::gc::compile` for its object and headers.
  - Docs: CI, README, CLAUDE.md and `docs/setup.md` drop `libgc-dev`.
- **Related existing code:** `src/link/mod.rs`, `src/link/runtime.rs`, `src/link/object_cache.rs`, `runtime/lugha_rt.c`, `runtime/tasks.c` (the collector hooks: `GC_set_stackbottom`, `GC_set_push_other_roots`), `tests/tasks_runtime.rs`, `tests/c_cache.rs` (the logging `cc`).
- **Spec:** §7 ("Boehm …") gains a sentence that the collector is bdwgc 8.2.12, built into lughac, single-threaded and linked statically. §8's link step (`cc prog.o lugha_rt.c -lgc -lm`) shows the GC object instead of `-lgc`. §11 gets row 131 and §0 a summary line. Approving this PRP approves these edits.
- **Rule exemption needed (CLAUDE.md):** `third_party/` is vendored code, not ours. It is exempt from the 500-line FILE SIZE RULE, the CODE DOCUMENTATION RULE and `docs/CODE_STYLE.md`, and is never edited by hand. Approving this PRP approves adding that line to CLAUDE.md, and `third_party/` to the FILE ORGANIZATION tree.
- **Open decisions that must be resolved first:** none. A later "several OS threads" milestone would revisit (2) and rebuild with `GC_THREADS`.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- Copy the files from the release tarball whose SHA-256 is the one above, unmodified. `VERSION` records how to redo it: the URL, the hash, and how the file list was made (`cc -MD`).
- Compile bdwgc only through the object cache, with no `-march`, so the object is baseline x86-64 like the runtime (milestone 130).
- Keep `src/link/` files under 500 lines; `gc.rs` is new.
- A comment in `gc.rs` explains why `ALWAYS_SMALL_CLEAR_STACK` is there, citing the fiber-stack overflow.

### Must NOT Do
- Don't patch bdwgc's sources. A needed change goes in `GC_FLAGS`, or the PRP is revisited.
- Don't download anything at build time.
- Don't keep `-lgc` or a system-GC switch (discovery answer 3).
- Don't compile with `GC_THREADS` (discovery answer 2).
- Don't change the runtime's use of the collector beyond the include lines.

### New dependencies
- **bdwgc 8.2.12** (C, vendored, not a crate). It's needed so programs and lughac stop depending on the system's `libgc`, which a cross build can't supply. Its licence (README.md, "Copyright & Warranty") is permissive, keep-the-notice: "Permission is hereby granted to use or copy this program for any purpose, provided the above notices are retained on all copies. Permission to modify the code and to distribute modified code is granted…". That is GPL-3.0-compatible. `libatomic_ops` isn't needed: the single-threaded build doesn't use it.

## ERROR HANDLING REQUIREMENTS
- Writing the files or compiling the GC fails as the runtime's compile does: `LinkError::Io`, `LinkError::CcNotFound` or `LinkError::Failed` (exit 2, lughac's fault). No new variants and no new diagnostic codes.
- A damaged cached GC object is handled by the object cache as for any entry (milestone 127).

## SECURITY CONSIDERATIONS
- **For the user's review:** third-party C compiled into every program. Mitigations:
  - A pinned release, its SHA-256 recorded, so anyone can re-download and compare.
  - No patches.
  - The upstream that Debian and Ubuntu already ship.
  - Code that runs exactly where today's `libgc.so` runs: inside the program.
- **For the user to confirm before merging:** the SHA-256 against upstream's release page (https://github.com/ivmai/bdwgc/releases/tag/v8.2.12). I recorded it from my own download.
- No new external input: the files are fixed text embedded at lughac's build. The cache folder name is a hash. No new processes beyond the `cc` already run.
- No `unsafe` and no new Rust dependencies.

## TESTS TO WRITE
- [ ] `src/link/gc.rs` unit: every file `extra/gc.c` needs is in `GC_FILES` (the `.c` files it includes and the headers); `GC_FLAGS` holds `-DALWAYS_SMALL_CLEAR_STACK` and no thread flag; the folder name changes when a file's text changes.
- [ ] `src/link/mod.rs` unit: `args` has the GC object and `-lm`, and no `-lgc`.
- [ ] `tests/programs/m131/`: two `spawn`ed threads each pushing 300 000 elements while `main` waits. It prints `500000`, the program that overflowed in the experiment.
- [ ] `tests/cli.rs`: a built program's file contains no `libgc.so` (its dynamic section names none).
- [ ] `tests/c_cache.rs` (logging `cc`): the first build compiles `gc.c` once; a second build doesn't; a damaged cached GC object is rebuilt.
- [ ] `tests/tasks_runtime.rs`: passes using the bundled GC.
- [ ] CI drops `libgc-dev` and the suite still passes there. That's the proof lughac works without the system's `libgc`.

## ROLLBACK PLAN
- Branch to return to: `main`.
- State: `-lgc` and the system headers as before; delete `third_party/` and `src/link/gc.rs`.

## ACCEPTANCE CRITERIA
- [ ] No program links `libgc.so`; lughac needs no `libgc-dev`
- [ ] The m131 green-thread allocation program passes
- [ ] The GC compiles once per bdwgc version (cache test)
- [ ] `cargo clippy --all-targets -- -D warnings` passes
- [ ] `cargo fmt --check` passes
- [ ] Every `pub` item has a doc comment
- [ ] CHANGELOG.md updated
- [ ] The user has reviewed the vendored source and confirmed its SHA-256

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test --lib`, `--test programs`, `--test spec`, `--test tasks_runtime`, `--test c_cache`, `--test cli`, `--test codegen`, `--test callbacks`, `--test net`, `--test http`, `--test crypto`. Linking changes under every program, so the full `cargo test` is run once too.
- On this machine with `libgc-dev` uninstalled, or in CI: the same suites pass.
