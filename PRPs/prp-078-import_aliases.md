## FEATURE: Import aliases and re-exports — `import a.b as c;`, `import a.b { X as Y };`, `public import a.b [as v];` giving `m.v.X`; milestone 49.

**Status:** approved 2026-10-14 — session 100. This is part of the user's "in one go" (milestones 45–49), merged once CI is green.
**Milestone:** 49 (one PRP; tag `m49` at merge)
**Spec:** `Language v14 Draft.md`:
- decision 4, and the choices on imports;
- §3 (`import`, `item`, `qualified`);
- §11 (milestone 49).

**Decisions:** MEMORY 91 (v14), MEMORY 87 (named imports and re-exports), MEMORY 85 (visibility levels).

## OBJECTIVE
The §10 directory program:
- imports a module under another name (`import shapes.circle as c;`);
- renames an item (`import shapes.vec { Vec2 as V };`);
- uses a module that `shapes` re-exports whole (`public import shapes.vec;` in `shapes.la`) as `shapes.vec.Vec2`, `shapes.vec.add(…)` and `shapes.vec.Dir.Up`.

## CONTEXT

- **Starting state:**
  - an import is bound by its last segment;
  - `{ X, y }` binds names as written;
  - `public import` needs named items (E0201).
- **Ending state:** `as` on modules and items, and whole-module re-exports reached through three-part names.
- **Related existing code:**
  - **AST, parser:** `src/ast/header.rs`, `src/parser/{import,generic,interface,expr}.rs`, `src/parser/sexp/item.rs`.
  - **Loader:** `src/modules/{mod,resolve,packages}.rs`.
  - **Checker:**
    - `src/check/modules/{mod,lookup}.rs`, `src/check/named_imports.rs`;
    - the `imported` callers (`construct.rs`, `interfaces.rs`, `iface_values.rs`, `fun_values.rs`).
- **Open decisions that must be resolved first:** none.

### Design
- **AST:**
  - `Import` gains `alias: Option<Ident>`;
  - `items` becomes `Vec<ImportItem { name, alias }>`;
  - `Import::bound()` gives the alias or the last segment, and `ImportItem::bound()` the alias or the name.
- **Parser:**
  - `import path [as IDENT] [{ IDENT [as IDENT], … }];`;
  - `public import a.b;` is allowed (the v11 E0201 goes);
  - in types, bounds and struct literals, `m.v.T` gives the module ident `"m.v"`, spanning both;
  - an expression `m.v.X` is already `Field(Field(m, v), X)`;
  - sexp `(import a.b as c (X as Y))`.
- **Loader:** clashes (E0314) and the item-clash check use `bound()`.
- **Checker:**
  - **Imports:** `imports` is keyed by `bound()`; a named item registers its alias under `bound()` with the target `path.name`.
  - **Re-exports:** `public import a.b [as v];` (no items) records a whole-module re-export, `reexports[qualify(module, v)] = path`, in the named-imports pass, before every declaration pass.
  - **Lookup:**
    - `module_path("m" | "m.v")` resolves the module, following one re-export for `m.v`; a local `m` shadows as before;
    - `member_through(module, name)` replaces the `imported` + `member` pairs;
    - through a re-export, an item visible by the ordinary rules but not `public` is E0313;
    - expression sites read a module name from `Name(m)` or `Field(Name(m), v)` (`module_name`);
    - chains beyond one re-export are not followed.
- **Codegen:** nothing changes: keys, symbols and side tables are the declaring module's.
- **Programs without these forms** give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** everything above.
- **Spec:**
  - §3 grammar;
  - §6 (imports: `as`, item aliases, whole-module re-exports, `m.v.X`);
  - the E0313/E0314 rows' examples;
  - the §10 directory program;
  - §11;
  - the changes summary;
  - the out-of-scope list (aliases and whole-module re-exports leave it).
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 96, CONTEXT. The tag `m49` is added at the merge.

### Must NOT Do
- **No re-export chains (`m.v.w.X`), no glob imports, no `--package` flag.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0314:**
  - two imports bound to one name (`import a.util; import b.util;` without `as`, or the same alias twice);
  - an alias that clashes with an item;
  - an item alias listed twice.
- **E0313:** a non-`public` item reached through `m.v.X`, or a private one as before.
- **E0301/E0305:** `m.v.nope`, or `m.v` where `m` has no such re-export.
- **E0318:** `public import m { X as Y }` with `X` not `public`.

## SECURITY CONSIDERATIONS
- No new input: the paths are the same module files as before.
- No `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — parser: `import a.b as c;`, `{ X as Y, z }`, `public import a.b as v;`, a type `m.v.T`, a bound `m.v.I`, a literal `m.v.S { … }`.

Unit tests — loader: E0314 for `import a.util; import b.util;`; `as` resolves it.

Unit tests — checker (in-memory modules):
- [ ] An aliased module's items, types and variants.
- [ ] An aliased item (the original name is not bound).
- [ ] `m.v.f()`, `m.v.T`, `m.v.S { … }`, `m.v.T.f()`, `m.v.E.V`, a bound `m.v.I`, an alias of a re-export.
- [ ] E0313 for a package-level item through `m.v.X`.
- [ ] E0314 clashes; E0301 for `m.v.nope`.

Acceptance — `tests/programs/m49/`:
- [ ] `imports/` (a directory case): the §10 program.
- [ ] `e0313/`: a reject golden for a non-`public` item through a re-export.
- [ ] The §10 program runs from the spec, if the extractor takes directory programs; otherwise the directory case stands in for it, as for earlier module programs.

Regression:
- [ ] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-078-import_aliases`, merged into `main` after CI passes; tag `m49`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The `m49/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [ ] The spec is at milestone 49; docs updated.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m49/imports/main.la
```
