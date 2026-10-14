## FEATURE: Package layout — `[package] name` (E0327) and `source`, programs inside a package using it by name, one file one module (E0328), and the `..` manifest path fix; milestone 51.

**Status:** proposed 2026-10-14 — session 104, with `Language v15 Draft.md`. Waiting for the user's approval of both, and of the two file splits below.
**Milestone:** 51 (one PRP; tag `m51` at merge)
**Spec:** `Language v15 Draft.md` (decisions 1–7, the choices, §6, E0327, E0328, §10, §11 row 51).

**Decisions:** MEMORY 82 (packages), MEMORY 86 (versions), MEMORY 97 (git packages).

## OBJECTIVE
A library laid out as
```
glowworm/
├── lugha.toml        [package] name = "glowworm", version = "0.1.0", source = "src"
├── src/array.la      → glowworm.array
└── examples/basics.la   (no lugha.toml) → import glowworm.array;
```
- runs with `lughac run glowworm/examples/basics.la` from any folder;
- users write `import glowworm.array;`, through a path entry or a git entry.

## CONTEXT

- **Starting state:**
  - a package's modules start at its folder;
  - a package has no name of its own;
  - a program's manifest must sit beside its main file;
  - `example/..` folds to an empty path (E0322 bug).
- **Ending state:** the draft's decisions, implemented.
- **Related existing code:**
  - `src/modules/{mod,packages,manifest,resolve,fetch}.rs`;
  - `src/modules/tests.rs`;
  - `src/driver/source.rs` (file names in diagnostics);
  - `tests/cli.rs`.
- **Open decisions that must be resolved first:** none.

### Design
- **Manifest** (new `modules/package_table.rs`, moving `package_line` out of `manifest.rs`):
  - `[package]` takes `version`, `name` and `source`, each at most once;
  - `Manifest` gains `name: Option<String>` and `source: Option<String>`;
  - `name` must be an identifier, and `source` a relative path with `/`, no `..`, not absolute and not empty (E0321).
- **Module roots:**
  - `Packages` keeps each package's folder (its manifest's) and its module root (folder + `source`);
  - `resolve` reads a package's manifest before building the file path, so `source` applies to its first import too.
- **Names:**
  - reading a listed package's manifest with `name = N`, listed under key `K ≠ N`, is E0327 at the listing's line;
  - the help says to list it as `N`, and to use `import N.m as K;` for a short name.
- **Upward lookup** (new `modules/layout.rs`):
  - when there is no `lugha.toml` beside the main file, walk up the parent folders. Stop at the first holding a `lugha.toml` (use it), else at one holding `.git` or equal to `$HOME` (no manifest). The home folder comes from the driver; the loader gets it as data, so tests can set it;
  - **with the found manifest:**
    - its `[packages]` are the program's, with paths relative to its folder;
    - with `name = N`, `N` is listed implicitly, with that folder and its `source`;
  - E0320 applies to the implicit name as to a listed one;
  - the manifest's diagnostics point into it, as any manifest's do.
- **One file, one module:**
  - the loader maps each loaded file, after `normalized`, to its module path;
  - an import that reaches a loaded file under another path is E0328 at the import, naming both paths.
- **Path fix:** `normalized` returns `.` instead of an empty path, so `example/..` resolves to the current folder.
- **Codegen and checker:** unchanged. Module paths are as before, with `source` only moving files.

### File splits (need approval, per the file-size rule)
- **`modules/mod.rs` is at 300 lines.** Move `parse_module` and `shift` (≈ 70 lines, unchanged) to a new `modules/parse.rs`.
- **`modules/packages.rs` is at 285 lines.** Move `exists`, `normalized` and `clash` (≈ 45 lines, unchanged) to a new `modules/paths.rs`.
- **All new code** goes in `layout.rs` and `package_table.rs`.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** everything above.
- **Spec:**
  - `Language v15 Specification.md` begins from v14's, and v14 is frozen;
  - `lughac spec` prints v15's (`src/lib.rs`, `tests/spec.rs`);
  - the status, changes, compatibility, §6 (packages, loading), the E0327 and E0328 rows, §10, §11 row 51, §12.
- **Docs:** CLAUDE.md (tree, STACK line, anti-pattern 5), README, TODO, CHANGELOG, MEMORY decisions 98 (the v15 draft) and 99 (M51), CONTEXT. The tag `m51` is added at the merge.

### Must NOT Do
- **No default entry point,** no workspaces, no required `name`.
- **No lookup past `.git`, `$HOME` or the first `lugha.toml`.**
- **No change to module paths or symbols.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0321:** a `name` that isn't an identifier; `source` absolute, empty or with `..`; `name` or `source` twice.
- **E0322:** a `source` folder that doesn't exist.
- **E0327:** a listing key that differs from the declared `name`.
- **E0328:** a file imported as two modules.
- **E0320:** the implicit package's name clashing with a folder or file beside the main file.

## SECURITY CONSIDERATIONS
- **The upward lookup reads a `lugha.toml` the user didn't name,** and such a manifest can list git packages. It is bounded by the first manifest, `.git` and `$HOME`, so a manifest planted in a shared folder above them (such as `/tmp` or `/`) is never read. A program whose main file sits *directly* in a shared folder still reads a manifest there, as in v14.
- **`source` can't leave its package:** no `..` and no absolute paths. So a git package can't make lughac read files outside its checkout.
- **No new process, network or `unsafe`.**

## TESTS TO WRITE

Unit tests — `package_table.rs`: `name`, `source`, each E0321.

Unit tests — `layout.rs`:
- [ ] The lookup stops at a manifest, at `.git`, and at the home folder.
- [ ] A manifest found beside `.git` is used.

Unit tests — the loader (in memory):
- [ ] `source` for a path package.
- [ ] E0327 with its help.
- [ ] The implicit package from `examples/`, and its `[packages]`.
- [ ] E0328.
- [ ] `example/..` resolves.
- [ ] E0320 for the implicit name.
- [ ] The E0322 for a missing `source`.

Integration — `tests/package_layout.rs` (`lughac` in a temp folder):
- [ ] The objective's layout, run from the parent folder and from inside `examples/`.
- [ ] A program with its own `lugha.toml` listing the package by path: `import glowworm.array;`.
- [ ] A manifest above `$HOME` (set in the test) is not used.
- [ ] E0327 and E0328 from the CLI.

Spec §10: the M51 program needs a main file under `examples/`, which the spec extractor's `main.la` convention can't express, so `tests/package_layout.rs` is its acceptance test and §11's row points there. This is as for M50.

Regression:
- [ ] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-080-package_layout`, merged into `main` after CI passes; tag `m51`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] `tests/package_layout.rs` passes.
- [ ] glowworm's layout works with `source = "src"` and `examples/`.
- [ ] The spec is at milestone 51; docs updated.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo test --test package_layout
```
