## FEATURE: Compiled C objects — the runtime and packages' `[link]` C files — are cached between builds; milestone 127 (v17).

**Status:** proposed 2026-10-20 (session 304), following milestones 124–126 (faster `-O0` builds and tests). After them, `cc` is most of a kivuli build: lugha-gtk's five `[link]` C files take ~2.8 s and the runtime another ~0.3 s of `examples/backgrounds.la`'s 4.1 s, recompiled on every `build`, `run` and `test`. Discovery: (1) cache both the runtime and `[link]` C files; (2) an entry is stale when the C file's bytes, its flags, the compiler, lughac's version or any header it read changes; (3) no new crate — exact comparison of source and flags, FNV-1a fingerprints for headers; (4) under the cache root git packages use, no eviction. **Changes how `src/link/` runs `cc` on a package's input: needs the user's security review (CLAUDE.md anti-pattern 5).**

## RESULT (session 305)
Built in `src/link/object_cache.rs` (+ `object_cache_tests.rs`), `c_files.rs`, `runtime.rs` (the runtime's own `cc -c`). Added while building: the object's own fingerprint is checked on a hit (a damaged `object.o` is a miss), and header fingerprints are taken once per build (5 entries × ~300 GTK headers made the check take 0.8 s; now 0.13 s). Measured on kivuli: warm `lughac build examples/backgrounds.la` 4.5–5.0 s → 2.5 s, `examples/hello.la` 4.3 s → 2.6 s, `lughac test test/diff.la` 4.9 s → 3.2 s; whole `lughac test test/` unchanged (~120 s: since milestone 124 a folder is one build, so its C files compiled once per run anyway). A warm build runs `cc --version` and the link only.

## OBJECTIVE
The second `lughac build` of kivuli's example runs no `cc -c` at all: the runtime and lugha-gtk's C files come from `~/.cache/lugha/objects/`, and only the link runs. Editing a C file, a header it includes (GTK's included), its flags or the compiler recompiles exactly that file. Builds behave as before in every other way.

## CONTEXT
- `src/link/mod.rs` `link_with`: compiles the plan's C files (`c_files::compile`, `cc -c <file> <flags> -o <temp>/c<n>.o`), writes the runtime (`runtime::RuntimeFile`, with `TLS_SOURCE`/`CRYPTO_SOURCE` appended for programs that load them) and runs one `cc <objects> <c objects> lugha_rt.c -lgc -lm … -o out`; `runtime::missing_openssl` turns its stderr into E0332.
- `src/modules/cache.rs` `root`: `$LUGHA_CACHE`, else `$XDG_CACHE_HOME/lugha`, else `$HOME/.cache/lugha` (git packages live in `git/` under it).
- `codegen/hashing.rs`: FNV-1a in generated code; the same 64-bit FNV-1a is written in Rust here (a few lines).
- `tests/link_libs.rs`: `[link]` through lughac with a fake `pkg-config` on `PATH` — the pattern for a logging `cc` wrapper.
- Spec v17 §8 (`[link]`), §9 (linking). Open decisions: none.

## DECISIONS (draft decisions 307–316)
307. **What is cached**: the object of each `[link]` C file, and the runtime's object per variant (plain, with TLS, with crypto, with both). The link itself always runs. `check` and `lsp` still never run `cc`.
308. **The runtime** is compiled on its own (`cc -c lugha_rt.c -o …`, no other flags, as its part of the link command compiles today) and linked as an object; a compile failure naming OpenSSL is still E0332.
309. **An entry** lives in `<root>/objects/<key>/`, `<root>` as for git packages. `<key>` is the FNV-1a fingerprint, in hex, of lughac's version, `cc --version`'s output (run once per build), the kind (`runtime` or `c`), the source's bytes and the flags. The folder holds `object.o`, `source` (the source's bytes), `flags` (one per line), `compiler` (the `cc --version` text) and `headers` (one `fingerprint path` line per header).
310. **A hit** needs all of: `source`, `flags` and `compiler` equal to the current ones byte for byte, and every header in `headers` present with the same fingerprint. Then `object.o` is linked straight from the cache. Anything else is a miss.
311. **A miss** compiles as today plus `-MD -MF <temp>/deps.d`, reads the headers `cc` lists there (make syntax, `\\ ` escapes), fingerprints each, and installs the entry: written to a fresh folder `<root>/objects/.tmp-<pid>-<n>` and renamed over `<key>` (an older entry for the key is removed first), so a concurrent build sees a whole entry or none. The object linked is the one just compiled, from the build's temp folder.
312. **Never a failure**: no cache root, a folder that can't be created, read or written, a malformed entry, a failed rename — each falls back to compiling into the build's temp folder as today, silently. Only the compile's own errors (E0330/E0331/E0332 as today) are reported.
313. **Trust**: the cache is the user's, like the git package cache beside it; whoever can write it can already change those packages. Entries are created with the user's default permissions; nothing is followed out of `<root>/objects` (keys are hex, names fixed). No defence against deliberate tampering, as the user chose (Q3).
314. **No eviction**: one entry per key; growth follows the distinct C files and flag sets built. Deleting `objects/` (or the whole root) is always safe; the spec says so.
315. **Not**: caching Lugha's own object (`prog.o`), parallel C compiles, a cache command or size limit, an opt-out switch, a new crate.
316. **Docs**: spec §8/§9 (the cache, its folder, staleness, deleting it), Draft, CHANGELOG (Changed: faster builds), CLAUDE.md (`src/link/` tree, the cache under STACK). **Success**: kivuli's second `lughac build examples/backgrounds.la` runs no `cc -c` and is timed before/after; milestone 127, tag `m127`.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- `src/link/object_cache.rs`: key, entry read/match/install, FNV-1a, dependency-file parsing, the fallback.
- `c_files.rs` and the runtime compile go through it; `link_with` links objects only.
- Unit tests for the pure parts; integration tests through lughac with a logging `cc` on `PATH`.

### Must NOT Do
- No new dependency; no change to which flags reach `cc` (beyond `-MD -MF` on a miss), to `pkg-config`, or to `git`.
- Never report a cache problem as a build error.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- Cache I/O errors are swallowed by design (decision 312), each `let _`/`.ok()` with a comment saying why it is safe.
- Compile errors as today: `LinkError::CFile` (E0331), `LinkError::Tls` (E0332), `LinkError::CcNotFound`/`Io`.

## SECURITY CONSIDERATIONS
- **Needs the user's review**: how `cc` runs on package input changes (one more pair of flags, objects linked from the cache).
- Keys and file names are generated, never taken from input; paths stay inside `<root>/objects`.
- Cached objects are trusted as the git cache is (decision 313).

## TESTS TO WRITE
- [ ] Unit: FNV-1a known vectors; `deps.d` parsing (continuation lines, `\\ ` in paths, the target first); an entry matching and each mismatch (source, a flag, compiler, a header's contents, a missing header); keys differ by kind and flags.
- [ ] `tests/c_cache.rs` (lughac with `LUGHA_CACHE` in a temp folder and a `cc` wrapper logging its arguments): a second build runs no `cc -c`; editing the C file, a header it includes, or its flags recompiles only that file; a corrupted entry (truncated `object.o`, garbled `headers`) recompiles and the program runs; an unwritable cache root still builds; a TLS program's runtime variant is its own entry; the runtime compile failing on a missing OpenSSL header is still E0332.
- [ ] Existing `link_libs`, `callbacks`, `extern_structs`, `c_strings`, `tls`, `crypto`, `programs` pass unchanged.

## ROLLBACK PLAN
- Branch to return to: `main`; the work lives on `prp-159-c_object_cache` until merged. Deleting `~/.cache/lugha/objects` undoes any cache state.

## ACCEPTANCE CRITERIA
- [ ] kivuli's second build of `examples/backgrounds.la` runs no `cc -c`; times before/after recorded
- [ ] All tests above pass; full `cargo test` passes; clippy and fmt pass; no file over 500 lines
- [ ] Spec, Draft, CHANGELOG, CLAUDE.md updated; the user's security review done

## VALIDATION
- `cargo fmt --check`; `cargo clippy --all-targets -- -D warnings`; `cargo test`
- In kivuli (read only): `lughac build examples/backgrounds.la` twice under `strace -f -e trace=execve`
