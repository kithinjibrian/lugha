# Lugha v15 — Draft (delta against v14)

**Status:** approved 2026-10-14 — session 105 (proposed in session 104). Milestone 51 is planned here. The rest of v15 is planned in `TODO.md` (directories, processes, environment variables, Unicode follow-ups); it gets milestones in this draft once its discovery resumes.
**Base:** `Language v14 Specification.md`, complete (milestone 50). It stays the source of truth, and what `lughac spec` prints, until v15's first milestone starts `Language v15 Specification.md`. That file then grows milestone by milestone, and v14 is frozen.

This draft lists only what changes, in the order of the v14 sections. Everything not mentioned keeps its v14 meaning.

**Compatibility.** Every valid v14 program and package stays valid:
- **`name` and `source` are optional.** A package without them behaves as before.
- **The upward lookup** only finds a manifest with `[package] name`, which no v14 manifest has.
- **A manifest path such as `".."`,** used from a main file given with a folder (`lughac run example/main.la`), now resolves; in v14 it failed with E0322 (a bug).

## Decisions behind this draft (discovery, session 104)

1. **Where it goes:** v15's first milestone (M51), ahead of the system-access milestones.
2. **`source` is explicit:**
   - `[package] source = "src"` makes `src/array.la` the package's module `array`, so users write `import glowworm.array;`;
   - without `source`, modules start at the package's folder, as in v14.
3. **`[package] name` is optional and checked:** a manifest listing the package under another key is an error (E0327). A short local name comes from an import alias (`import glowworm.array as ga;`, v14).
4. **Programs inside a package use it by name:**
   - a main file with no `lugha.toml` beside it looks upward for the nearest one;
   - if that manifest has `[package] name`, the program may import that package by its name, and the manifest's `[packages]` are its packages too;
   - so `glowworm/examples/basics.la` writes `import glowworm.array;` with no manifest of its own;
   - a `lugha.toml` beside the main file wins, as in v14.
5. **The file argument stays required:** `lughac run examples/basics.la`. A default entry point may come later.
6. **One file, one module:** a file reached under two module names (`cli.args` and `mytool.cli.args`) is an error (E0328).
7. **The lookup is bounded:**
   - it stops at the first folder holding a `lugha.toml`, a `.git` or that is `$HOME`;
   - so a manifest in a shared folder above a repository or a home folder, such as `/tmp`, is never used.

**Choices made while writing this draft** (each settles a gap the answers left open):
- **`name`:** a Lugha identifier, as manifest keys are (E0321 otherwise). The root program may declare one: when its main file sits in that folder, that program is the package itself, so `import mytool.x;` and `import x;` reach the same file (E0328 if both are used).
- **`source`:**
  - a relative path with `/`, with no `..`, not absolute, and not empty (E0321);
  - a missing folder is E0322;
  - it applies wherever the folder is used as a package: through a path entry, a git entry, or the upward lookup;
  - a program's own modules always resolve from its main file's folder.
- **The upward lookup:**
  - a folder holding a `.git` *and* a `lugha.toml` uses that manifest, so a repository's own root manifest is found;
  - the main file's own folder counts as the first step, which is the v14 rule;
  - a found manifest without `[package] name` gives no implicit package, but its `[packages]` still apply, as a manifest beside the main file would.
- **E0320 still applies:** a folder or `.la` file beside the main file named like the implicit package clashes, as for a listed one.
- **Manifest paths:**
  - paths are resolved against the manifest's folder as given, before `.`/`..` are folded;
  - a folder that folds to empty is the current folder;
  - so `example/..` is `.`, which fixes the v14 bug.

## §2–§5 — no changes

## §6 Functions and program structure — changes

- **Packages:**
  - `[package]` gains `name = "ident"` and `source = "path"`, both optional and at most once (E0321);
  - E0327 when a listing's key differs from the declared name;
  - `source` moves the package's module root.
- **Loading:**
  - the upward lookup for the program's manifest (decisions 4 and 7), and the implicit package;
  - E0328 for a file loaded under two module names.

## §9 Compilation model — changes

- Module paths and symbols are unchanged: with `source = "src"`, `src/array.la` of package `glowworm` is module `glowworm.array` and its function `zeros` is `lugha_fn_glowworm/array/zeros`.

**New error codes.** The numbers are proposed, and become final when their milestone's PRP is approved.

| Code | Meaning | Examples |
| --- | --- | --- |
| E0327 | A package listed under another name than it declares | `gw = { path = "../glowworm" }` when glowworm's `[package]` says `name = "glowworm"` |
| E0328 | One file loaded as two modules | `src/main.la` importing both `cli.args` and `mytool.cli.args` |

## §10 Example programs — additions (v15 acceptance tests)

- **M51:** a directory case: a package with `name` and `source = "src"`, and an `examples/` program with no manifest that imports it by name, run from the package's parent folder (`lughac run glowworm/examples/basics.la`).

## §11 Milestones — additions

| # | Milestone | Adds |
| --- | --- | --- |
| 51 | Package layout | `[package] name` (E0327), `source`, the upward lookup and implicit package, E0328, the `..` path fix |

Later v15 milestones (directories, processes, environment variables, Unicode follow-ups) are added when their discovery resumes; processes need a security review.

**Out of scope for v15** (for later): a default entry point (`lughac run` without a file), workspaces of several packages, `name` being required.

## §12 Open questions — additions

- [ ] **A default entry point** (v15). `[package] main = "src/main.la"` and `lughac run` with no file could come later.
- [ ] **Requiring `name`** (v15). A later version could require it once packages have adopted it.
