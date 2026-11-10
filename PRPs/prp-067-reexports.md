## FEATURE: Named imports and re-exports — `import m { X }`, `public import m { X }`; milestone 38, v11 complete.

**Status:** merged 2026-10-14 — session 86. This is the last part of the user's "in one go" (milestones 37–38), merged once CI is green.
**Milestone:** 38 (one PRP; completes v11; tags `m38` and `v11` at merge)
**Spec:** `Language v11 Draft.md`:
- decision 4, and the choices on named imports, re-exports, `public`, clashes, methods and fields;
- §3 (`import`);
- §4;
- §6 "Named imports and re-exports";
- §9;
- §10 "Re-exports";
- §11 (milestone 38);
- §12.

**Decisions:** MEMORY 80 (v11), MEMORY 71 (visibility levels), MEMORY 34 (module keys).

## OBJECTIVE
The §10 re-exports program prints `8`. `geo/shapes.la` writes `public import mathx.vec { Vec2, add };`, and a program that lists only `geo` uses `shapes.Vec2` and `shapes.add`.

## CONTEXT

- **Starting state:**
  - Imports bind one module name per file (`check/modules/mod.rs::enter`).
  - Names resolve through `Checker::key` (own module), or through `qualify(imported path, name)` at the `m.X` sites:
    - `lookup.rs` (types, calls);
    - `construct.rs` (`m.E.V`);
    - `iface_values.rs` and `interfaces.rs` (interfaces);
    - `fun_values.rs`.
  - `private import`/`public import` are E0201.
- **Ending state:** named imports and re-exports, with v11 complete.
- **Related existing code:**
  - `src/ast/header.rs` (`Import`), `src/parser/{item,import}.rs`;
  - `src/check/modules/{mod,lookup}.rs`, `src/check/{construct,iface_values,interfaces,fun_values,visibility,levels}.rs`.
- **Open decisions that must be resolved first:** none.

### Design
- **AST:** `Import` gains `items: Vec<Ident>` (the braces) and `public: Option<Span>`.
- **Parser:**
  - `import.rs` reads an optional `{ X, y, }` (a missing name or `}` is E0201);
  - `program` accepts `public` before an `import` that has braces;
  - `public import a;` without braces stays E0201, as does `private import`.
- **The loader** needs nothing: it already resolves the module.
- **Checker** (new `check/named_imports.rs`). An alias table, `aliases: HashMap<String, Alias>`, maps `qualify(module, X)` to the item's key, and to whether it is `public`.
  - **Registration, before every other pass,** for each module and each listed name:
    - E0302 if the module declares that name;
    - E0314 if the file lists it twice, or it equals an imported module's name.

    Otherwise the alias is registered, with a privacy record: `public` for a re-export, `private` (own module) otherwise.
  - **Following:**
    - `Checker::key` follows aliases, so bare names in the file resolve to the item;
    - a new `Checker::member(path, &Ident)` replaces `qualify(&imported, name)` at the `m.X` sites: it checks the alias's own visibility (E0313 for a file-only name used from elsewhere), then follows.

    Chains (re-exports of re-exports) are followed, with at most 64 steps.
  - **Validation, after the declaration passes:**
    - each alias's target must be an item (a function, extern, struct, enum or interface), else E0301 at the listed name;
    - it must be visible from the importing module (E0313);
    - for a re-export, it must be `public` (E0318).
- **Codegen** is unchanged: the checker records the item's own keys, so types, callees and symbols are the declaring module's.
- **Programs without named imports** give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- The AST, parser, checker table and hooks above, and the sexp form of an import with items, if imports print.
- **Spec:**
  - §3 grammar;
  - §6 (named imports and re-exports, and the import-visibility rule);
  - §4 note;
  - §9;
  - the E0201, E0302, E0313, E0314 and E0318 rows;
  - the §10 program (multi-file);
  - §11 row 38 done;
  - the title and status say v11 is complete;
  - §12.
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 82, CONTEXT. The tags `m38` and `v11` are added at the merge.

### Must NOT Do
- **No `public import m;` without braces, no `as` aliases, no package-level re-exports.**
- **No change to symbols or codegen.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0201:** `public import m;` (no braces); an empty or malformed brace list.
- **E0301:** a listed name that the module doesn't declare.
- **E0302:** a listed name that the file also declares.
- **E0314:** a name listed twice, or listed and also an imported module's name.
- **E0313:**
  - a listed item that isn't visible here;
  - a file-only named import used from another module (`shapes.Vec2` when `shapes` imported `Vec2` without `public`).
- **E0318:** a `public import` of an item that isn't `public`.

## SECURITY CONSIDERATIONS
- No external input beyond source files, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — parser (`parser/import.rs`):
- [x] `import a.b { X, y };`, `public import a.b { X, };` parse.
- [x] E0201 for `public import a;`, `import a { };` and `import a { X`.

Unit tests — checker (`check/named_imports.rs`, multi-module through `check_modules` test utilities):
- [x] **Named imports:** a type, a function, an enum and an interface work unqualified in the importing file.
- [x] **Re-exports:**
  - `m.X` works from another module;
  - a chain of two re-exports works;
  - the re-exported type is the same type (a value passes to the original module's function).
- [x] **Errors:**
  - E0301 for an unknown name;
  - E0302 for a clash with a declaration;
  - E0314 for a name listed twice;
  - E0313 for a private item, and for using a file-only name from another module;
  - E0318 for re-exporting a non-`public` item.

Acceptance — `tests/programs/m38/`:
- [x] `reexports/` (directory case): the §10 program.
- [x] `named/` (directory case):
  - named imports of a struct, a function, an enum (with a `match`) and an interface (as a bound) in one file;
  - a re-export chain across two packages.
- [x] `e0318/` (directory case): reject golden.
- [x] The §10 program runs from the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-067-reexports`, merged into `main` after CI passes; tags `m38` and `v11`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m38/` cases pass.
- [x] The v11 spec is complete; docs updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m38/reexports/main.la
```

## AMENDMENTS (during build, session 86)

- **An unclosed brace list is E0202** (unclosed delimiter), the parser's existing code, not E0201.
- **E0318 for a re-export has its own wording:** "`helper` is not `public`, so it can't be re-exported". The v8 message talks about types.
- **`parser/item.rs` got shorter:** the `private`/`public` before an import moved into `parser/import.rs`.
- **`interfaces.rs::interface_key`** (no span, `&self`) follows aliases without the alias-visibility check. A bound written as `m.X` through another module's file-only name is then checked only for the target's own visibility.
- **IR:** the 143 earlier programs that compile give byte-identical IR.
- **No file splits.** `check/mod.rs` is at 298 lines.
