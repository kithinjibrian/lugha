## FEATURE: Find References and Rename — every use of a name across the checked files, and renaming them safely; milestone 83.

**Status:** proposed 2026-10-16 — session 174 (discovery the same session); waits for approval.
**Milestone:** 83 (one PRP; tag `m83` at merge). Go to Implementations is a later milestone.
**Spec:** `Language v16 Draft.md` decisions 76–80; spec §9 (the `lughac lsp` section gains **References** and **Rename**), §11 (row 83).

**Decisions (discovery, session 174):**
- References and Rename together, built on one search;
- the checked files only; Rename refuses a non-`private` module-level item of a module no checked main imports;
- Rename re-checks the renamed text in memory and refuses on any change of meaning or new error.
- No open entries in DECISIONS.md apply.

## OBJECTIVE
```
fun area(s: Shape): f64 = …     Shift+F12 on `area`:  main.la:12, main.la:15, geo.la:4 (+ the declaration)
                                F2 on `area` → `surface`: all four renamed in one edit, or refused with why
```

## CONTEXT
- **Starting state:** `check::Partial.definitions` maps each written name's span (uses and declarations) to its `Target` — `Decl(span)` or `Module(path)` — in the check's shared offset space; `definition.rs` reads it from use to declaration. Each main file's last check is a `Snapshot` (`server.rs`: `snapshots`, `seen`: the files each check loaded); `Snapshot::base` maps a file to its offset base. `check::check(main, docs, alone)` checks a main against a `Documents` overlay of editor texts. Names in `std` and the prelude have declarations in bundled sources (no file). Visibility: `private` items are file-local (v2 draft §6).
- **Ending state:**
  - **`src/driver/lsp/references.rs`** (new): `Place { file: PathBuf, start: usize, end: usize }` (bytes in that file's text). `target_at(snapshot, file, offset)` → the declaration as a `Place` (or a module path, or "bundled"), using the innermost name as `definition.rs` does (shared, not copied). `find(snapshots, target, include_declaration)` → every `Place` whose definition is that declaration, across all snapshots, deduplicated (a module loaded by two mains is found once), sorted by file and offset. `textDocument/references` → `Location`s.
  - **`src/driver/lsp/rename.rs`** (new):
    - `prepare(…)`: the name's range under the cursor, or an error for the refusals of decision 77 (`std`/prelude, a module, an unsafe module-level item);
    - `rename(…, new_name)`: validates the identifier (`lexer` keywords, ASCII identifier rule), collects the places, builds the edited texts in memory (a `Documents` copy with each affected file's text renamed), checks every main whose check loaded an affected file (`check::check` on the copy), and compares (decision 78): each renamed place resolves to the renamed declaration; the declaration has exactly as many uses as before; no file has more errors than before. On success a `WorkspaceEdit` with `changes` per file (one `TextEdit` per place); otherwise an error response (`RequestFailed`) whose message is the reason, which VS Code shows.
    - "Visible outside its file": a function, struct, enum, interface or extern without `private`, or a member (field, method, variant) of one; "imported by a checked main": some snapshot of a main other than the declaring file loaded the declaring file. A declaration in a main file (`fun main` present) is always safe: nothing imports a main file.
  - **`server.rs`/`mod.rs`:** `references_provider: true`, `rename_provider: { prepareProvider: true }`; the three requests, each first bringing pending checks up to date (`fresh`), as hover does.
  - **Extension:** README (References, Rename, the "checked files" rule) and manual steps; version 0.6.0. No TypeScript change.
  - **Spec v16:** §9 **References** and **Rename**; §11 row 83; the changes summary.
  - **Docs:** CLAUDE.md (tree, status), README, TODO, CHANGELOG, MEMORY, CONTEXT.
- **Related existing code:** `src/driver/lsp/{definition,hover,server,check,documents,targets,mod}.rs`, `src/check/partial.rs`, `tests/common/lsp_client.rs`, `tests/lsp_definition.rs`.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The ending state above, test-first.
- If the definitions table turns out to miss a kind of use (a field in a struct-literal pattern, a variant in a pattern, a method through an interface), record it in the checker as part of this PRP only if it is a recording gap of the same kind as milestone 72's; otherwise stop and report (SCOPE RULE).

### Must NOT Do
- No Go to Implementations, no rename of files or modules, no workspace scan for mains.
- No renaming inside `std` or the prelude, no edits outside the checked files.
- No writing of files by the server (`WorkspaceEdit` only).
- No new dependency.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- Nothing under the cursor → `null` (references) / an error "nothing to rename here" (prepare and rename).
- Refusals are `RequestFailed` errors with a one-line reason, never partial edits.
- A panic in the self-check's checks is caught by `check.rs`'s guard and refuses the rename ("couldn't verify the rename").

## SECURITY CONSIDERATIONS
- Unchanged: reads only the editor's texts and the files checks already load; the extra checks are the same offline checks (no `git`); returns edits, writes nothing.

## TESTS TO WRITE
- [ ] **Unit tests (`references.rs`, `rename.rs`):** identifier validation (keywords, digits first, `_`, non-ASCII); dedup across two snapshots; the visibility rule (private, main file, module imported or not).
- [ ] **`tests/lsp_references.rs`** over real stdio: a local (declaration and uses, `includeDeclaration` both ways); a function used in two files; a method; a struct field (declaration, `p.x`, `P { x: … }`); a variant (construction and pattern); a type in annotations; a module (`geo` in `import geo;` and `geo.f`); a prelude function's uses; nothing under the cursor → `null`.
- [ ] **`tests/lsp_rename.rs`:** a local renamed everywhere (the edit applied and checked clean); a function across two files; a field; refusals: a keyword, `1abc`, a prelude name, a module, a clash (`let a`, `let b`; rename `b` → `a` used after) — refused by the self-check, a shadowing capture refused, a public function in a module no checked main imports; `prepareRename` returns the name's range or the refusal; no file written.
- [ ] **Extension:** `npm test` passes.
- [ ] **Regression:** every existing test passes.

## ROLLBACK PLAN
- Branch `prp-113-references_rename` from `main`; tag `m83`.
- To abandon before merge: delete the branch. After: revert the merge.

## ACCEPTANCE CRITERIA
- [ ] All tests pass; fmt, clippy, `cargo test`, `npm test` (CI in the background).
- [ ] Spec at milestone 83; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] The manual check passes in VS Code (README steps).

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo test --test lsp_references --test lsp_rename
cd editors/vscode && npm test
```
