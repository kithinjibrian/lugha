# Lugha v11 — Draft (delta against v10)

**Status:** proposed 2026-10-14 — session 84
**Base:** `Language v10 Specification.md`, complete (milestone 36). It stays the source of truth, and what `lughac spec` prints, until v11's first milestone starts `Language v11 Specification.md`. That file then grows milestone by milestone, and v10 is frozen.

This draft lists only what changes, in the order of the v10 sections. Everything not mentioned keeps its v10 meaning. Every valid v10 program and `lugha.toml` stay valid in v11, because everything v11 adds was an error before:
- a `[package]` table and an inline table `geo = { … }` were E0321;
- an import followed by `{` was a parse error.

## Decisions behind this draft (discovery, session 84)

1. **Headline: packages grow up.** Package versions and re-exports. Downloading stays out (§11).
2. **Versions are checked, and packages stay local.** A package declares its version in its own `lugha.toml`; a manifest that lists it may require a compatible version, and a mismatch is an error. No network, no cache, no lock file.
3. **Requirements use one rule, Cargo's "compatible".** `"1.2"` means 1.2.0 or later, below 2.0.0, and below 1.0 the minor number counts as major.
4. **Re-exports name their items.** `public import mathx.vec { Vec2, add };` makes `Vec2` and `add` part of the importing module. The same braces without `public` bring names into one file.

**Choices made while writing this draft** (each settles a gap the answers left open):
- **A version is `MAJOR.MINOR.PATCH`,** three decimal numbers without leading zeros (`0` alone is fine) and without pre-release or build suffixes.
- **A package's own manifest** may hold a `[package]` table with one line, `version = "1.4.0"`, before or after its `[packages]` table. The program's manifest may hold one too; nothing checks it.
- **A requirement** is written in an inline table, `geo = { path = "libs/geo", version = "1.2" }`, with `path` required and `version` optional; `geo = "libs/geo"` still means no requirement. A requirement is `MAJOR`, `MAJOR.MINOR` or `MAJOR.MINOR.PATCH`, with the missing parts taken as 0.
- **Compatibility:** a requirement `R` with first non-zero part `X` matches a version `V >= R` that keeps every part up to and including `X`. `"1.2"` accepts 1.2.0 up to, but not including, 2.0.0; `"0.3"` accepts 0.3.x; `"0.0.4"` accepts only 0.0.4; `"0"` accepts 0.x.y.
- **A version mismatch is a new error, E0324,** at the requirement's line, naming the required range and the version found. A requirement on a package whose manifest declares no version is E0324 too. A malformed version or requirement is E0321.
- **Several manifests may require one package** (it is one folder, E0323); each requirement is checked against its one version.
- **Named imports:** `import a.b { X, y };` imports module `a.b` as before (the name `b` is bound too), and also binds each listed name in this file, to the module's item: a function, extern, struct, enum or interface. Each listed item must be visible here (E0313). An unknown name is E0301, or E0305 where it is used as a type.
- **Re-exports:** `public import a.b { X, y };` does the same, and also makes `X` and `y` items of the importing module, visible to every package. A program that imports this module can then write `m.X` and `m.y`, and a later `public import` can pass them on again. They are the same items: no copy, one symbol, and `m.X` and `b.X` are the same type.
- **A re-exported item must itself be `public`** (E0318 otherwise, as for a declaration exposing a less visible type). A re-export is always `public`; package-level re-exports aren't a thing in v11.
- **Clashes:** a listed name that the file also declares, imports twice, or binds as a module name is E0302 (defined more than once) or E0314 (import clash). A local named like a listed type is E0311, as for any type.
- **Methods and fields need nothing new:** they belong to their type, wherever it is named from.

## §2 Lexical structure — changes

None. `{`, `}` and `,` are existing tokens.

## §3 Grammar — changes

```ebnf
import       = [ "public" ] "import" IDENT { "." IDENT } [ "{" IDENT { "," IDENT } [ "," ] "}" ] ";" ;
```

- **`public import` without braces** is E0201: only named items are re-exported.

## §4 Type system — changes

- **Named items:** an item brought in by `import m { X }`, or re-exported by `public import`, is the module's item itself. Its type prints with its declaring module's path, as before (`mathx.vec.Vec2`).

## §6 Functions and program structure — changes

**Package versions** (milestone 37).

```
# libs/geo/lugha.toml
[package]
version = "1.4.0"

# lugha.toml, next to main.la
[packages]
geo = { path = "libs/geo", version = "1.2" }
```

- **The `[package]` table:** one `version = "MAJOR.MINOR.PATCH"` line. Another key, a second `[package]` table, or a malformed version is E0321.
- **Inline entries:** `name = { path = "…", version = "…" }`, with the keys in either order. A missing `path`, an unknown key, a key twice, or a malformed requirement is E0321.
- **Checking:** after loading every manifest, each requirement is checked against the listed package's declared version (E0324).

**Named imports and re-exports** (milestone 38).

```
// libs/geo/shapes.la, in package geo, whose manifest lists mathx
public import mathx.vec { Vec2, add };

public fun centre(a: Vec2, b: Vec2): Vec2 = add(a, b);

// main.la, whose manifest lists only geo
import geo.shapes;

fun main() {
    let c = shapes.centre(shapes.Vec2 { x: 1, y: 2 }, shapes.Vec2 { x: 3, y: 4 });
    println(shapes.add(c, c).x);
}
```

- **Names:** a listed name resolves in its file before imported module names, like the file's own items.
- **Not transitive for manifests:** a program can use `shapes.Vec2` without listing `mathx`, and still can't `import mathx.vec;` unless its own manifest lists `mathx` (v8).

## §9 Compilation model — changes

- **Versions** are checked by the loader, before parsing ends; nothing reaches codegen.
- **Named imports and re-exports** are resolved by the checker to the declaring module's keys, so symbols are unchanged (`lugha_fn_mathx/vec/add`) and programs using none of this give byte-identical IR.

**New error code.** The number is proposed, and becomes final when its milestone's PRP is approved.

| Code | Error | Example |
| --- | --- | --- |
| E0324 | A package version that doesn't meet a requirement, or that a requirement can't check | `geo = { path = "libs/geo", version = "1.2" }` when geo's manifest says `version = "2.0.0"`, or declares no version |

**Reused codes:**
- **E0321:** a malformed `[package]` table, inline entry, version or requirement.
- **E0201:** `public import` without braces.
- **E0301 / E0305:** an unknown listed name.
- **E0313:** a listed item that isn't visible here.
- **E0318:** re-exporting an item that isn't `public`.
- **E0302 / E0314:** a listed name that clashes.

## §10 Example programs — additions (v11 acceptance tests)

**Package versions** (milestone 37). A directory case: `main.la` and `lugha.toml` (`geo = { path = "libs/geo", version = "1.2" }`), with `libs/geo/lugha.toml` (`version = "1.4.0"`) and `libs/geo/shapes.la` (`public fun area(w: i64, h: i64): i64 = w * h;`). Prints `12`, exits with 0.

```
import geo.shapes;

fun main() {
    println(shapes.area(3, 4));
}
```

**Re-exports** (milestone 38). A directory case: the §6 example above, with `libs/mathx/vec.la` declaring `public struct Vec2 { public x: i64, public y: i64 }` and `public fun add(a: Vec2, b: Vec2): Vec2 = Vec2 { x: a.x + b.x, y: a.y + b.y };`, geo's manifest listing `mathx = "../mathx"`, and the program's listing only geo. Prints `8`, exits with 0.

## §11 Milestones — additions

| # | Milestone | Adds | Done when |
| --- | --- | --- | --- |
| 37 | Package versions | `[package]` versions, inline entries with requirements, E0324 | The package-versions program passes |
| 38 | Re-exports | `import m { X }`, `public import m { X }` | The re-exports program passes, and the v11 spec is complete |

**Order:**
- **M37 first:** it touches only the manifest and the loader.
- **M38:** it touches the parser and the checker's name resolution.

**Out of scope for v11** (for later, roughly in order):
- downloading packages (git or a registry), a cache, and a lock file;
- two versions of one package in a build;
- full version ranges (`>=`, `<`, `~`, `*`) and pre-release versions;
- whole-module re-exports (`public import m;`) and import aliases (`import m as n;`);
- a `--package` flag;
- `zip`, `chain` and the other iterator helpers; `for n of mut c`; unsigned indexes;
- `Self` in declarations and patterns; a `Display`-like interface; Unicode text; binary I/O.

## §12 Open questions — additions

- [ ] **Re-exports and visibility levels:** a re-export is always `public`. A package-level re-export (`import m { X }` visible to the package's other modules) could come later if packages need internal façades.
- [ ] **Checking the program's own version:** the root package may declare a version that nothing reads. A later `lughac` could print it, or check it against a minimum `lughac` version.
