## FEATURE: Loading packages — `lugha.toml`, package imports, per-package manifests (E0320–E0323); milestone 30, part 1.

**Status:** approved 2026-10-14 — session 74. The user asked for milestone 30 "in one go": PRP-058 then PRP-059, each merged once CI is green.
**Milestone:** 30, the first of two PRPs: **058 is loading and resolution**, and 059 is `public` and visibility (completes v8, tags `m30` and `v8`).
**Spec:** `Language v8 Draft.md`:
- decisions 2, 4 and 5, and the choices on paths, package modules, symbols and cycles;
- §6 "The manifest" and "Packages and imports";
- §9;
- E0320–E0323.

**Decisions:** MEMORY 69 (v8), MEMORY 33–34 (modules: loading, keys, symbols).

## OBJECTIVE
A program with a `lugha.toml` can import a package's modules, and a package can list its own packages:

```
# lugha.toml
[packages]
geo = "libs/geo"

// main.la
import geo.shapes;            // libs/geo/shapes.la, module key geo.shapes, symbols lugha_fn_geo/shapes/…
```

Until PRP-059, every item that isn't `private` is visible across packages, and `public` stays E0317.

## CONTEXT

- **Starting state:**
  - `modules::load` reads the main file and every `import a.b;` as `<root>/a/b.la`.
  - Module paths come straight from import names, the checker's `enter` maps each import's last name to its written path, and diagnostics map offsets to files through `driver/source.rs`.
- **Ending state:**
  - Manifests are read and checked.
  - Imports resolve through them.
  - Each `Module` records its package and its resolved import paths.
  - Manifest errors point at lines of `lugha.toml`.
- **Related existing code:**
  - `src/modules/mod.rs` (`load`, `Module`, `Loaded`), `modules/resolve.rs`, `modules/tests.rs`;
  - `src/check/modules/mod.rs` (`Unit`, `enter`);
  - `src/driver/pipeline.rs` (`loaded`, units), `driver/source.rs`;
  - `tests/modules.rs`.
- **Open decisions that must be resolved first:** none.

### Design
- **Manifest parser** (new `src/modules/manifest.rs`):
  - **What it reads:** `parse(text, base) -> (Vec<Entry>, Vec<Diagnostic>)`, where `Entry { name, path, span }`. Accepted are blank lines, `#` comments, one `[packages]` header, and `name = "path"` lines (an identifier, `=`, a double-quoted string without `"` or `\`), with spaces around tokens.
  - **E0321:** anything else, including a line before the header, another table, a name listed twice, an empty path, or a missing quote, at the line's span.
  - **Where the file lives:** the manifest's text joins the shared offset space, after the files read before it, so its diagnostics map to `lugha.toml` (`Loaded.manifests`, which `Source::of` merges by base).
- **Finding manifests:**
  - the root manifest is `<root>/lugha.toml`;
  - a package's is `<package dir>/lugha.toml`, read when the package is first reached;
  - a missing manifest is no error.
- **Folders:** a manifest path joins the manifest's folder and is normalised lexically (`.` and `..`, no symlinks).
  - **E0322:** a folder that doesn't exist, at the entry's line. Existence is probed through the loader's `read` callback: reading a directory fails with an error other than `NotFound`, which is what `std::fs::read` does. That keeps `load`'s signature, and lets the tests describe folders.
  - **E0323:** one name with two different folders across manifests, at the later entry's line.
- **Resolving an import,** in a module of package P (the root package, or a named one):
  - **When P's manifest lists the first name `a`:**
    - the module path is the import's names, and the file is `<a's folder>/<rest>.la`;
    - `import a;` with nothing after it is E0312, with help "`a` is a package: import one of its modules, `import a.<module>;`".
    - **E0320:** if P's own folder also has `a/` or `a.la`. It is reported at the import.
  - **Otherwise:** the module path is P's prefix plus the import's names (the prefix is empty for the root, and `[P]` for a package), and the file is in P's folder, as today.
  - **The root's folder:** every package name in the build is also checked against the root's own folder (`<root>/a/` or `<root>/a.la`), at the manifest entry. That is E0320. Root modules have unprefixed paths, so their keys and symbols could otherwise collide with a package's. This reaches a little further than the draft, which checks only P's own folder.
- **`Module` gains:**
  - `package: Option<String>`, `None` for the root;
  - `imports: Vec<Vec<String>>`, each import's resolved module path, in order.
- **Checker:** `Unit` gains `imports: &[Vec<String>]`. `enter` maps each import's last name to that resolved path. An empty slice, for single-file checks, means the paths as written.
- **Symbols and keys:** unchanged rules, on the resolved paths (`geo.shapes`, `lugha_fn_geo/shapes/circle`).
- **The main file:** a package importing the main file is E0319, as before. A package's `main` is E0304 (unchanged).

## IMPLEMENTATION REQUIREMENTS

### Must Do
- `src/modules/manifest.rs`, plus the loader changes in `modules/mod.rs` and `modules/resolve.rs` (new code in new files where those reach their limit);
- `Module.package`, `Module.imports`, `Loaded.manifests`;
- `Unit.imports` and `enter`;
- the driver: units pass the resolved imports, and `Source::of` includes the manifests;
- **Spec:** begin `Language v8 Specification.md` from v7 (in progress: milestone 30, part 1). Fold in:
  - §6 "The manifest" and "Packages and imports";
  - §9 symbols and diagnostics in `lugha.toml`;
  - E0320–E0323;
  - "Changes since v7";
  - §11 milestone 30 in progress.

  `lughac spec` prints it.
- **Docs:** CLAUDE.md (tree, STACK), README, TODO, CHANGELOG, MEMORY decision 70, CONTEXT.

### Must NOT Do
- **No `public`, and no package-level visibility:** both are PRP-059. `public` stays E0317.
- **No downloading, no versions, no `--package` flag.**
- **No new crate:** the TOML subset is parsed by hand.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **New codes (all `Diagnostic`s):**
  - **E0320:** a package name that is also a local folder or file (at the import, or at the manifest entry for the root folder);
  - **E0321:** a malformed manifest line;
  - **E0322:** a missing folder;
  - **E0323:** one name for two folders.
- **E0312:** importing a package without a module, and a missing module inside a package.
- **A manifest that exists but can't be read** is a `LoadError::Io` (exit 2), like a module.

## SECURITY CONSIDERATIONS
- **External input:** `lugha.toml` and the paths it names are user input.
  - The parser rejects anything outside the subset (E0321).
  - Paths are only joined and lexically normalised, and read by the existing `read` callback, the same trust as `import`. A manifest may name any folder the user can read, as an import of a local file can.
- No `unsafe`, no new dependencies, nothing logged.

## TESTS TO WRITE

Unit tests — `modules/manifest.rs`:
- [ ] Comments, blank lines and `geo = "libs/geo"` lines parse into entries with spans.
- [ ] **E0321:**
  - a line before `[packages]`;
  - `[deps]`;
  - `geo = libs` (no quotes);
  - `geo = ""`;
  - a duplicate name;
  - `1x = "a"`;
  - trailing junk.

Unit tests — `modules/tests.rs` (in-memory files and folders):
- [ ] **Loading:**
  - `import geo.shapes;` loads `libs/geo/shapes.la` with path `[geo, shapes]` and package `geo`;
  - a package's own `import util.x;` resolves to `[geo, util, x]` in the package's folder;
  - a package's manifest lists `mathx`, which the package can import but the root can't (E0312).
- [ ] **Errors:**
  - E0320 for a root folder `geo/` next to package `geo`, both at the import and for a nested manifest's name;
  - E0322 for a missing folder;
  - E0323 for two folders under one name;
  - E0312 for `import geo;` and for a missing module inside the package.
- [ ] **Cycles:** packages importing each other load each file once.

Integration — `tests/modules.rs` (the real binary, temp directories):
- [ ] A program with `lugha.toml` and a package runs.
- [ ] A bad manifest reports E0321 at `lugha.toml:2:…` in human and JSON output.
- [ ] Symbols are `lugha_fn_geo/shapes/…` in `--emit=ir`.

Acceptance — `tests/programs/m30/` (directory cases with `lugha.toml`):
- [ ] `packages/`: a program using two packages, one of them through another's manifest.
- [ ] `e0320/` and `e0321/`: reject goldens.

Regression:
- [ ] Every earlier test passes, and programs without a manifest give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-058-package_loading`, merged into `main` after CI passes (no tag until PRP-059).
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The `m30/` programs and the new loader and manifest tests pass.
- [ ] `Language v8 Specification.md` begins; docs updated.
- [ ] No file over 300 lines: new code goes in new files, and any split of an existing file is proposed first. No new dependencies, no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m30/packages/main.la
```
