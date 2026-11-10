# Lugha v8 — Draft (delta against v7)

**Status:** approved 2026-10-14 — session 73
**Base:** `Language v7 Specification.md`, complete (milestones 27–29). It stays the source of truth, and what `lughac spec` prints, until v8's first milestone starts `Language v8 Specification.md`. That file then grows milestone by milestone, and v7 is frozen, as earlier versions were.

This draft lists only what changes, in the order of the v7 sections. Everything not mentioned keeps its v7 meaning. Every valid v7 program stays valid in v8:
- a program without a `lugha.toml` is one package, the root package;
- an item without a visibility keyword is visible throughout its package, which for such a program is everywhere, as before;
- `public`, which was reserved (E0317), is now allowed.

## Decisions behind this draft (discovery, session 73)

1. **Headline: packages.** A program can use code from folders outside its own tree, through packages, and a package chooses what other packages see. The `match` additions, iterator helpers and more number types wait (§11).
2. **A manifest lists the packages.** A file named `lugha.toml` next to the main file has a `[packages]` table of `name = "path"` lines. lughac reads this small subset of TOML itself, with no new dependency, and it stays valid TOML so a later version can add fields. There is no downloading and no versions.
3. **Visibility has three levels:**
   - no keyword means visible anywhere in the item's own package;
   - `public` means visible to other packages too;
   - `private` means its own module only, as before.
4. **Imports keep their syntax.** In `import geo.shapes;`, the first name is a package when the manifest lists it, and otherwise a local folder, as before. A name that is both a listed package and a local folder is an error (E0320), never a silent choice.
5. **A package may have its own `lugha.toml`.** The packages it lists are visible only inside it, never to the packages that use it. One name means one folder in a whole build: two manifests giving one name different folders is an error (E0323).

**Choices made while writing this draft** (each settles a gap the answers left open):
- **Paths** in a manifest are relative to the manifest's own folder, and use `/`.
- **A package's modules** are found from its folder, as a program's are from the main file's folder. A package cannot reach the modules of the program that uses it.
- **No `main`:** a package has no main file, and `main` declared in a package is E0304, as in any module.
- **Symbols:** a package's items are named by their module path, which starts with the package name, as for local modules: `lugha_fn_geo/shapes/circle`. E0320 guarantees this can't collide with a local folder.
- **No leaking** (amends v2's E0318): a `public` declaration may not mention a type that isn't `public`.
- **Cycles** between packages are allowed, as between modules: every file is loaded once.

## §2 Lexical structure — changes

None for `.la` files. `lugha.toml` is read by its own rules (§6).

## §3 Grammar — changes

- **`public`** is accepted wherever `private` is: on items, `impl` methods and struct fields. It was E0317.

## §4 Type system — changes

None. Visibility is name resolution (§6).

## §6 Functions and program structure — changes

**The manifest.**
- **Where:** a program's manifest is `lugha.toml` in the main file's folder. A package's is `lugha.toml` in the package's folder. Both are optional.
- **What lughac reads:**
  - blank lines, and comments from `#` to the end of a line;
  - one table header, `[packages]`;
  - under it, lines of the form `name = "path"`, where `name` is a Lugha identifier and `path` is a double-quoted string without escapes.
- **Mistakes (E0321):** anything else, a name listed twice, or a path that is empty. Each is reported at its line in `lugha.toml`.
- **Missing folders:** a path whose folder doesn't exist is E0322, also at its line.

**Packages and imports.**
- **The root package:** a program is its main file's package. A package is the folder a manifest names, and all the `.la` files under it.
- **Resolving an import:** in `import a.b.c;`, written in a file of package P:
  - if P's manifest lists `a`, it loads `b/c.la` from that package's folder;
  - otherwise it loads `a/b/c.la` from P's own folder, as before.
- **Errors:**
  - a name that P's manifest lists and that is also a folder or a `.la` file in P's own folder is E0320, at the import;
  - a missing file is E0312, as before;
  - `import geo;` naming only a package, with no module, is E0312, with help to name a module.
- **One name, one folder:** across every manifest in a build, one package name must mean one folder, compared after resolving the paths. Otherwise it is E0323, reported at the second manifest's line.
- **Loading:** every file is loaded once, however many packages import it, so cycles are fine.

**Visibility** (amends v2's rules).

| Written | Visible in |
| --- | --- |
| `private` | its own module only (E0313 elsewhere) |
| nothing | every module of its own package (E0313 from another package) |
| `public` | every package that imports its module |

- **What it covers:** items, methods (in `impl`) and struct fields, in the same places v2's `private` rule covers: naming, calling, field reads and writes, struct literals, and satisfying an interface. A method another package can't see doesn't satisfy that package's interface bounds, as for `private` today.
- **E0313's message** names the reason: "`helper` is not `public`: it is only visible inside package `geo`".
- **No leaking (E0318):** a `public` declaration may not mention a type that isn't `public`. That covers parameters, return types, fields, payloads and bounds, looking inside arrays, `Option`, `Result`, function types and type arguments. The v2 rule for `private` types stays.
- **The prelude** and the built-in names are visible everywhere, as before.

## §8 C interop — changes

- **Externs** follow item visibility like functions: a `public extern fun` can be called from other packages. Its C symbol is still verbatim.

## §9 Compilation model — changes

- **Symbols:** a package's items get their module path, which starts with the package name (`lugha_fn_geo/shapes/circle`, `lugha_fn_geo/shapes/Circle.area`). These are the same rules as v2 module paths.
- **Diagnostics in `lugha.toml`** carry that file's path, relative to the main file's folder (`libs/geo/lugha.toml`), like a module's.
- **Unchanged:** one LLVM module and one object file per program; packages change resolution only.

**New error codes.** The numbers are proposed, and become final when their milestone's PRP is approved.

| Code | Error | Example |
| --- | --- | --- |
| E0320 | A package name that is also a local folder or file | `geo` listed in `lugha.toml` while `geo/` sits next to `main.la` |
| E0321 | A malformed manifest | `geo = ../geo` (no quotes); `[deps]`; `geo` listed twice |
| E0322 | A manifest path that doesn't exist | `geo = "libs/goe"` with no such folder |
| E0323 | One package name for two folders | `app/lugha.toml` and `geo/lugha.toml` both list `mathx`, with different folders |

**Changed codes:**
- **E0317** is no longer reported, because `public` is allowed. The code is retired and never reused.
- **E0313** also covers a non-`public` item of another package.
- **E0318** also covers a non-`public` type in a `public` signature.

## §10 Example programs — additions (v8 acceptance tests)

**Packages** (milestone 30). Prints `12.0`, exits with 0. The files sit as their first lines say, and `lughac run main.la` builds them all.

```
# lugha.toml
[packages]
geo = "libs/geo"
```

```
// libs/geo/shapes.la
public struct Circle { r: f64 }

impl Circle {
    public fun area(self): f64 = 3.0 * self.r * self.r;
    fun diameter(self): f64 = 2.0 * self.r;
}

public fun circle(r: f64): Circle = Circle { r: r };
```

```
// main.la
import geo.shapes;

fun main() {
    let c = shapes.circle(2.0);
    println(c.area());
}
```

**A rejected package program** (milestone 30). In the same layout, a `main.la` that calls `c.diameter()` and writes `shapes.Circle { r: 1.0 }` reports two E0313 errors: `diameter` and the field `r` are not `public`.

## §11 Milestones — additions

| # | Milestone | Adds | Done when |
| --- | --- | --- | --- |
| 30 | Packages | `lugha.toml`, package imports, per-package manifests, `public` and package visibility, E0320–E0323 | The packages programs pass, and the v8 spec is complete |

**Order:** one milestone, which may be built as two PRPs as milestones 20 and 23 were. The first loads and resolves packages, treating every item that isn't `private` as visible. The second adds `public` and the visibility rules.

**Out of scope for v8** (for later, roughly in order):
- package versions, downloading, and a lock file;
- or-patterns and `mut` bindings in `match`, `unwrap()`, `%=`;
- iterator helpers, strings as iterators, `for n of mut c`;
- `Self` outside interfaces, and default methods called on concrete types;
- `i8`, `i16`, `u16`, rotations, and checked and saturating methods; `parse_u64`; unsigned indexes;
- binary I/O, directories, streams and processes;
- shared references and closures that change their captures;
- explicit type arguments and `.len()`.

## §12 Open questions — additions

- [ ] **Re-exports:** a package can't yet expose another package's items as its own (`public import`). Users import both.
- [ ] **A `--package` flag:** pointing at a package without editing `lugha.toml` (for example in a one-off build) could come later as a command-line override.
