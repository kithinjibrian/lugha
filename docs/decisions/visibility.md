# Lugha Visibility Model

Design note by the project owner, adopted for v2 in session 40 (2026-10-13), with the adaptations below. The amended rules are in `docs/specs/Language v2 Draft.md` (decision 9, §2, §3, §5, §6, §8, §9).

## Adaptations for v2

- **Keywords:** `private` (only this module) and `public` (other packages), chosen over the note's `pub`.
- **No packages in v2:** the whole program is one package, so the default ("package") means visible to every module of the program. `public` is reserved and is an error until v3 packages give it a meaning (E0317).
- **Error codes:** E0310 is already "method declared twice". Private-item and private-field errors are E0313, and the no-leak rule is E0318.
- **Paths:** source files are `.la`, and module names come from paths relative to the main file's directory. There is no `src/` directory and no manifest until v3.
- **Deferred to v3:** interfaces (and structural satisfaction), methods on built-in types, re-exports, `lughac api-diff`, and the test-visibility question (Lugha has no test files).
- **Decided:** `private` means private to the module.

---

## Summary

Lugha has three visibility levels. The default is **package**: an item is visible to every module in the same package and to nothing outside it. Applications almost never write a visibility keyword. Libraries write `pub` only on their real API, and `private` marks the few things that need protecting inside a package.

| Level | Visible to | Keyword |
| --- | --- | --- |
| Private | Its own module only | `private` |
| Package (default) | Every module in the same package | none |
| Public | Other packages that depend on this one | `pub` |

## The problem

Languages that draw the privacy line at the file or module, such as Rust and TypeScript, make every cross-file use need an annotation. Most code in an application is used only inside its own project, so `pub` and `export` end up on nearly everything. The keyword stops meaning "this is public API" and starts meaning "this is used in another file", which tells a reader nothing.

Making everything public by default removes the noise but gives up two things:

- **Safe evolution.** Making an item more visible later never breaks anyone. Making it less visible breaks every caller that used it. The default should be the one that can always be widened safely.
- **Invariants.** A struct whose fields must stay consistent, such as a `Range` where `lo <= hi`, can only guarantee that if code outside its module cannot assign the fields directly.

Package-level default visibility keeps both while removing the noise, because the boundary that matters for API stability is the package, not the file.

## Assumptions about modules

Visibility needs a module system. This note assumes a minimal one; its syntax is a sketch, not part of this proposal.

- One source file is one module. Its name comes from its path within the package, for example `src/geometry/shapes.lugha` is `geometry.shapes`.
- A package is a directory with a manifest (for example `lugha.toml`) naming the package and its dependencies.
- `import geometry.shapes;` makes the module's visible items available as `shapes.Circle`, `shapes.area(...)`.

## Rules

**Items.** Functions, structs, enums, interfaces, extern functions, methods, associated functions and struct fields each have a visibility. Without a keyword, it is package.

```
pub struct Range {
    private lo: i64,
    private hi: i64,
}

impl Range {
    pub fun new(lo: i64, hi: i64): Range =
        if lo <= hi { Range { lo: lo, hi: hi } } else { Range { lo: hi, hi: lo } };

    pub fun len(self): i64 = self.hi - self.lo;

    fun midpoint(self): i64 = self.lo + self.len() / 2;   // package
}

private fun clamp_raw(x: i64): i64 { ... }               // this module only
```

**Fields.** A field can be no more visible than its struct. Reading or assigning a field requires it to be visible at that point. A struct literal requires every field to be visible, so a struct with any private field can only be constructed outside its module through an associated function such as `Range.new`. That is how invariants are protected.

**Methods.** Each method has its own visibility, independent of the type, so a public type can keep helper methods package-level or private.

**Enums and interfaces.** Variants share their enum's visibility, and an interface's methods share the interface's visibility. Per-variant or per-requirement visibility would make exhaustive `match` and structural satisfaction depend on where code lives, which is confusing for little benefit.

**No leaking.** A declaration's signature may only mention types at least as visible as the declaration itself. A `pub fun` cannot take or return a package-level struct, and a package-level function cannot expose a `private` one. Violations are compile errors at the declaration, so a library's public API is always usable by the packages that depend on it.

**Structural interfaces.** A method satisfies an interface requirement only where that method is visible. A type whose `area` method is private does not count as a `Shape` outside its module.

**Methods on built-in types.** An `impl i64 { ... }` block follows the same rules: its methods are package-level unless marked. Whether two packages may both add a `pub` method with the same name to a built-in type is an open question (below).

**Entry point.** `main` needs no keyword; it is found by the compiler, not imported.

## Diagnostics

Visibility errors use the names-and-scopes range, E03xx, and say which level would fix them:

```
error[E0310]: field `lo` of `Range` is private to module `geometry.range`
 --> src/main.lugha:7:18
  |
7 |     let r = Range { lo: 1, hi: 5 };
  |                     ^^ private field
  |
help: use `Range.new(1, 5)`, or remove `private` from the field
```

Like every diagnostic, these appear in `--diagnostics=json` with stable codes.

## Alternatives considered

| Alternative | Used by | Why not |
| --- | --- | --- |
| Private by default, `pub` to share across modules | Rust | Annotations on most items in applications; the keyword stops carrying meaning |
| Public by default | Python (by convention), Kotlin today | Narrowing later is breaking, and invariants are unprotected unless every author remembers `private` |
| Visibility by capitalization | Go | No keywords, but renaming is needed to change visibility, and it clashes with Lugha's naming style |
| Export lists at the top of a file | Haskell, ES module re-exports | Visibility lives far from the declaration, and lists drift out of date |

Kotlin's history is relevant: it originally defaulted to `internal`, its package-level visibility, and later switched to public because of the same annotation-noise complaint. It kept `internal` as an option. Lugha keeps package as the default instead, because applications get the same quiet code either way, and libraries get a deliberate, visible API boundary.

## Open questions

- **Meaning of `private`.** Module-private (proposed), or private to a type's own `impl` blocks?
- **Tests.** Whether test files in the same module can see `private` items.
- **Re-exports.** Whether a module can re-export items from another, so a library can present a flat public API.
- **Methods on built-in types across packages.** Whether two packages may add `pub` methods with the same name to `i64` or `string`, and how a conflict is reported.
- **API checking.** A future `lughac api-diff` could report when a release removes or narrows a `pub` item, since that is the change that breaks dependents.
