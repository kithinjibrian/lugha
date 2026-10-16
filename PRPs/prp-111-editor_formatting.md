## FEATURE: formatting in the editor — `lughac lsp` answers Format Document with `lughac fmt`'s formatter, and the VS Code extension is Lugha's default formatter; milestone 81.

**Status:** approved 2026-10-16 ("build it") — built in session 171; proposed in session 170.
**Milestone:** 81 (one PRP; tag `m81` at merge). The second of the two formatter PRPs (PRP-110 was milestone 80).
**Spec:** `Language v16 Draft.md` decisions 66–70; spec §9 (the `lughac lsp` section gains **Formatting**), §11 (row 81).

**Decisions (discovery, session 170):**
- Format Document only — no range or on-type formatting;
- format-on-save stays the user's setting; the extension only makes itself the default formatter for Lugha;
- syntax errors: no edits, no message; a verification failure: no edits and one error message;
- the server still writes no file and runs no `git`.
- No open entries in DECISIONS.md apply.

## OBJECTIVE
In VS Code, Shift+Alt+F (Format Document) on a `.la` file — or a save, with `"editor.formatOnSave": true` — rewrites the editor's text exactly as `lughac fmt` would rewrite the file, unsaved changes included. Undo restores it in one step.

## CONTEXT
- **Starting state:** `lughac fmt` (milestone 80) formats files: `formatter::format(src)` returns the text, `FormatError::Syntax` or `FormatError::Unsafe`. `lughac lsp` (milestones 70–79) keeps every open document's text (`documents.rs`), answers hover, definition, completion, signature help and code actions in `mod.rs`'s request loop, and declares its capabilities in `mod.rs::capabilities`. The extension (`editors/vscode/`, 0.3.2) starts `lughac lsp` through `vscode-languageclient`, which already registers a formatting provider when the server declares one.
- **Ending state:**
  - **`src/driver/lsp/formatting.rs`** (new): `format(text) -> Formatted` — `Edits(Vec<TextEdit>)` (one edit replacing the whole document, or none when it is already formatted), `Unchanged` for syntax errors, `Failed(reason)` for a verification failure. The edit's end is the document's end in UTF-16 units (`publish.rs`'s position conversion). VS Code reduces a whole-document edit to the lines that changed, so the cursor and folds stay where they were.
  - **`mod.rs`:** `document_formatting_provider: true`; `textDocument/formatting` answered from the open document's text: the edits, `null` for `Unchanged` or a document the server doesn't hold, and for `Failed` `null` plus a `window/showMessage` (type Error): ``Lugha: `<file>` was left as written — formatting failed its own check (<reason>). Please report this.``. The request's `options` (tab size, spaces) are ignored: the style has no options (decision 61).
  - **Extension 0.4.0:** `package.json` `configurationDefaults` `"[lugha]": { "editor.defaultFormatter": "kithinjibrian.lugha" }`; the README gains "Formatting" (Shift+Alt+F, the format-on-save setting to add, that it is `lughac fmt`) and a manual-check step; `lugha.path`'s description says formatting needs lughac from milestone 81. No TypeScript change.
  - **Spec v16:** §9's `lughac lsp` section gains **Formatting** (decisions 66–69); §11 row 81; the changes summary.
  - **Docs:** CLAUDE.md (tree, status), README, TODO, CHANGELOG, MEMORY, CONTEXT.
- **Related existing code:** `src/formatter/mod.rs`, `src/driver/lsp/{mod,server,documents,publish,code_actions}.rs`, `tests/common/lsp_client.rs`, `tests/lsp_code_actions.rs` (a request test to copy), `editors/vscode/package.json`.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The ending state above, test-first.
- Bump the extension (`npm version minor --no-git-tag-version` → 0.4.0), rebuild the `.vsix`, install it, reinstall `lughac`.

### Must NOT Do
- No range formatting, on-type formatting or formatting code actions.
- No `editor.formatOnSave` default.
- No writing of files by the server, no `workspace/applyEdit`: the client applies the returned edits.
- No change to the formatter's style or to `lughac fmt`.
- No TypeScript change in the extension, no new dependency.

### New dependencies
None (`lsp-types` already has the formatting types).

## ERROR HANDLING REQUIREMENTS
- Bad params → `InvalidParams`, as for the other requests.
- A panic inside the formatter is caught by the server's existing guard (as for checks) and answered as `Failed`.
- `Syntax` is not an error for the editor: its diagnostics are already published.

## SECURITY CONSIDERATIONS
- The server reads only the text the editor sent; it opens, writes and runs nothing new. The edits go back to the client, which applies them to its buffer. (The extension's README and `untrustedWorkspaces` text stay true.) For the user's review, as CLAUDE.md asks for changes to what the server may do.

## TESTS TO WRITE
- [ ] **`formatting.rs` unit tests:** a messy document → one edit spanning the whole text with `lughac fmt`'s output; a formatted one → no edits; a syntax error → `Unchanged`; the end position counts UTF-16 units (a line with `é` and an emoji) and a document without a final newline.
- [ ] **`tests/lsp_formatting.rs`** over real stdio: the capability is declared; formatting an opened, unsaved, messy document returns the edit whose text equals `lughac fmt`'s; a formatted document → `[]`; a document with a syntax error → `null` and no `window/showMessage`; an unopened URI → `null`; the server wrote no file (the document's file on disk unchanged).
- [ ] **Verification failure path:** a unit test of the reply for `Failed` (the message text and type) through a test-only formatter hook, since no real input is known to fail.
- [ ] **Extension:** `npm test` passes; a test that `package.json` sets `editor.defaultFormatter` for `[lugha]` to the extension's own id (`publisher.name`).
- [ ] **Regression:** every existing test passes.

## ROLLBACK PLAN
- Branch `prp-111-editor_formatting` from `main`; tag `m81`.
- To abandon before merge: delete the branch. After: revert the merge (the extension's previous `.vsix` still works with the new `lughac`).

## ACCEPTANCE CRITERIA
- [ ] All tests pass; fmt, clippy, `cargo test`, the extension's `npm test` (CI in the background).
- [ ] Spec at milestone 81; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] The manual check passes in VS Code (README step: Shift+Alt+F on a messy file, then undo).

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo test --test lsp_formatting
cd editors/vscode && npm test && npm run package
```
