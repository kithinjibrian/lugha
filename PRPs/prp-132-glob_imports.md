## FEATURE: Glob re-exports — `public import raw { * };` makes every `public` item of `raw` an item of this module; milestone 100 (v17).

**Status:** proposed in session 232, after a discovery of six questions answered by the user (kivuli / lugha-gtk's request, with recommendations attached).

## OBJECTIVE
A package's entry flattens a sub-module into its own namespace without a hand-kept list. lugha-gtk's `src/lib.la` writes `public import raw { * };`, and programs write `import gtk { GtkWidget, gtk_box_new };` or `gtk.gtk_box_new(…)` for every public item of `gtk.raw` — extern functions, extern types, structs, enums, interfaces and functions — including ones `raw.la` gains later. Each name is the same item as its declaration, so the editor's hover, definition, references and rename land there.

## CONTEXT
- Starting state: named imports and re-exports (v11, v14; spec v17 §6 "Named imports"): `public import m { X, y };` re-exports listed items (E0318 for a non-`public` one); `public import m;` re-exports the module as a member (`gtk.raw.X`). A file imports a module once (E0314). Since PR #42 and #43, named imports record their names for the editor and accept extern types.
- Ending state: `{ * }` in the parser and AST; the checker expands it into one alias per re-exported item before the declaration passes; spec v17 §3 (grammar), §6 (named imports and re-exports), changes list, row 100; E0314's page; draft decisions 58–66.
- Related existing code: `src/parser/import.rs`, `src/ast/header.rs` (`Import`), `src/check/named_imports.rs` (`named_imports`, `Alias`, `follow`, `check_named_imports`), `src/check/reexports.rs`, `src/check/levels.rs`, `src/formatter/items.rs` (imports), `src/check/partial.rs` (the editor's recorder).
- Open decisions: none.

## DECISIONS (session 232; draft decisions 58–66)
58. **Syntax:** `{ * }` inside an import's braces (Q1). `*` stands alone: `{ *, X }` or `{ *, X as Y }` is E0201 (Q6).
59. **Only re-exports:** `{ * }` needs `public` (Q2). `import raw { * };` is E0201, its help saying to list the items or `import raw;`. Plain globs could be added later without breaking programs.
60. **What it covers:** every item of the module that is `public` — functions, externs, extern types, structs, enums, interfaces — exactly the longest explicit list `public import raw { … };` would accept; package-level and `private` items are skipped, never an error (Q3).
61. **Re-exports inside the module:** items the module itself re-exports by name (`public import x { Y };`, or its own `public import x { * };`) are its public items and come along, chained as named re-exports already are; modules it re-exports whole (`public import x;`) are not items and stay `gtk.raw.x` (Q4). A cycle of globs (`a` globs `b` globs `a`) adds each item once.
62. **Clashes:** a name `{ * }` brings that the module already has — its own item, another import's name, or another `{ * }` — is E0314 at the glob import, naming the name and both sources, unless both are the same item; help: list the wanted items instead of `*` (Q5). No silent shadowing.
63. **One import per module (unchanged):** `public import raw;` and `public import raw { * };` in one file are E0314, as any two imports of one module (Q6). Like a named import, `public import raw { * };` also binds `raw` in its file.
64. **The same items:** a glob-re-exported name is the declaration itself: `gtk.GtkWidget` and `gtk.raw.GtkWidget` are one type, calls reach one function, types print with the declaring module's path.
65. **Editor and formatter:** hover, Go to Definition, completion (`gtk.` lists the flattened items), Find References and Rename through a glob-re-exported name land on the declaring module's item; `*` itself has no hover. `lughac fmt` prints `{ * }` as written.
66. **Success:** milestone 100, tag `m100`; lugha-gtk's `lib.la` can replace its list with `public import raw { * };`.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- Parser: `{ * }` as `Import.glob: Option<Span>` (items empty); `*` with other items, or `{ * }` without `public`, E0201 with a help.
- Checker, in `named_imports` (before the declaration passes): expand each glob into one `Alias` per public item of the target module, from the target unit's `public` declarations and its named public aliases (expanding its own globs first; a visited set breaks cycles); register each as a `public` item of the importing module; E0314 per decision 62 with both sources.
- Visibility: unchanged rules (`item_visible`, E0313/E0318 can't arise for public items).
- The editor: completion's `value_members`/`type_members` for a module list glob-re-exported names (as for named re-exports today).
- Formatter: `{ * }` round-trips (`fmt_corpus` and a formatter rule test).
- Spec §3 grammar (`import_items = "{" ( "*" | item { "," item } ) "}"`), §6, changes list, row 100; draft; E0314's page (glob clashes); E0201 messages.

### Must NOT Do
- Plain (non-`public`) glob imports (decision 59).
- Re-export package-level or `private` items, or whole-module members (decisions 60–61).
- Shadow silently (decision 62).

### New dependencies
- none

## ERROR HANDLING REQUIREMENTS
- E0201 (existing): `*` with other items; `{ * }` without `public`.
- E0314 (existing): a glob name clashing with an item, an import or another glob, naming both sources.
- No new codes.

## SECURITY CONSIDERATIONS
- None: a compile-time naming rule; nothing runs.

## TESTS TO WRITE
- [ ] Parser units: `public import a { * };` parses (s-expression `(public import a (items *))`); `import a { * };`, `public import a { *, X };`, `public import a { X, * };` are E0201.
- [ ] Checker units (`named_imports` tests): every public item kind re-exported; package-level and private skipped; raw's own named re-exports and globs carried, a glob cycle terminating; E0314 for a clash with a local item, another named import and another glob, both sources in the message; no error when two globs bring the same item.
- [ ] `tests/programs/m100/`: a package entry with `public import raw { * };` used by a dependent program through `import gtk { GtkWidget, make, Point };` and `gtk.make()` (extern types, extern functions — not called if no C — structs, functions), `gtk.GtkWidget` the same type as `gtk.raw.GtkWidget`; a clash case (`.stderr`) naming both sources; a plain glob import (`.stderr`, E0201); a private item not leaking (`.stderr`, E0301).
- [ ] `tests/lsp_definition.rs` / `lsp_hover.rs` / `lsp_completion.rs`: hover and Go to Definition on a glob-re-exported name used as `gtk.x` and in `import gtk { x }` land on `raw.la`; completion after `gtk.` lists it.
- [ ] Formatter: `public import raw { * };` unchanged by `lughac fmt`.

## ROLLBACK PLAN
- Branch to return to: `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] lugha-gtk's case: `public import raw { * };` in an entry; `import gtk { GtkWidget, gtk_box_new };` checks in a dependent program.
- [ ] Targeted tests (`--lib`, `programs`, `package_layout`, `modules`, `lsp_definition`, `lsp_hover`, `lsp_completion`, `fmt`, `fmt_corpus`, `explain`, `spec`), fmt, clippy pass; CHANGELOG updated.

## VALIDATION
- `cargo test --lib`, `cargo test --test programs --test package_layout --test modules --test lsp_definition --test lsp_hover --test lsp_completion --test fmt --test fmt_corpus --test explain --test spec`, `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`
