## FEATURE: `lughac explain` — an explanation for every diagnostic code, with checked examples, a hint after diagnostics, and code links in the editor; milestone 86.

**Status:** approved 2026-10-16 ("build it") — built in session 183; proposed in session 182.
**Milestone:** 86 (one PRP; tag `m86` at merge).
**Spec:** `Language v16 Draft.md` decisions 93–97; spec §9 (the CLI table, the diagnostics section and its examples, a `lughac explain` paragraph, the language server's diagnostics), §11 (row 86).

**Decisions (discovery, session 182):**
- every one of the 99 codes explained, every runnable example checked by the tests;
- the editor links each diagnostic's code to its explanation in a read-only VS Code tab, from the installed `lughac`;
- a hint line after diagnostics in the human format (the user chose it, knowing every expected `.stderr` changes).
- No open entries in DECISIONS.md apply.

## OBJECTIVE
```
$ lughac check main.la
error[E0501]: cannot assign to `total`, which is not `mut`
  --> main.la:3:5
  …
For more about E0501, run `lughac explain E0501`.

$ lughac explain E0501
# E0501: assignment to an immutable binding

A `let` binding can't be changed unless it is declared `let mut` …

```lugha
fun main() {
    let total = 0;
    total = 5; // error: E0501
}
```
…
```

## CONTEXT
- **Starting state:** 99 codes (E01xx lexer … E05xx, W01xx) in spec §9's table, each with a title and an example. Diagnostics render in `driver/render.rs` (human) and `driver/json.rs`, through `Output::diagnostics` in `driver/mod.rs`. 158 expected `.stderr` files in `tests/programs/` hold human output; the spec has diagnostic examples (one checked by `render.rs`'s `e0401_matches_the_spec_example_exactly`). The language server publishes `lsp_types::Diagnostic` with `code` (`publish.rs`), no `code_description`.
- **Ending state:**
  - **`docs/errors/<CODE>.md`** (99 files): `# CODE: title` (the spec table's title), then plain-language prose — what it means, why Lugha has the rule, how to fix it — with a reporting example and a fixed one. Written in batches by code range.
  - **`src/explain.rs`** (new, beside `lib.rs`'s `SPEC`): `pub const EXPLANATIONS: &[(&str, &str)]` (code, text via `include_str!`), `pub fn explain(code) -> Option<&str>` (case-insensitive), `pub fn title(code)`.
  - **CLI:** `lughac explain [CODE]` in `driver/mod.rs` (a small `driver/explain.rs`): the text, the list, or exit 2 for an unknown code.
  - **The hint** (`driver/mod.rs` / `render.rs`): after a batch of human diagnostics, the line of decision 95. Every expected `.stderr` in `tests/programs/` regenerated, reviewed as a diff that only adds that line; the spec's examples and any test that matches whole stderr (`tests/cli.rs`, `modules.rs`, …) updated.
  - **Language server:** each published diagnostic with a code gets `codeDescription.href = lugha-explain:<CODE>.md`.
  - **Extension 0.9.0:** `src/explain.ts` (pure: the URI ↔ code, the arguments), a `TextDocumentContentProvider` for `lugha-explain:` running `lugha.path explain <CODE>` with `execFile` (no shell), registered whatever the workspace trust (it runs no program code); README and a manual step. If VS Code won't open a custom-scheme `codeDescription` link from the Problems panel, the fallback (recorded in the BUILT notes) is a code action "Explain E0501" opening the same document.
  - **Spec v16:** §9 CLI row and a `lughac explain` paragraph; the diagnostics section's examples gain the hint line; the language server text mentions the links; §11 row 86; the changes summary.
  - **Docs:** CLAUDE.md (tree: `docs/errors/`, `src/explain.rs`; COMMANDS; a rule that a new code needs its explanation), README, TODO, CHANGELOG, MEMORY, CONTEXT.
- **Related existing code:** `src/lib.rs`, `src/driver/{mod,render,json}.rs`, `src/driver/lsp/publish.rs`, `tests/programs.rs` runner (unchanged), `editors/vscode/src/extension.ts`.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The ending state above, test-first (the explanation checks are written before the 99 files).
- Explanations in plain language, short: the reader has just seen the error.

### Must NOT Do
- No change to any diagnostic's code, message, label or span.
- No hint in the JSON format, the language server, or for internal errors (no code).
- No network: nothing links to github.com.
- No new dependency.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- `lughac explain X` for an unknown code: `error: no code X; run \`lughac explain\` for the list`, exit 2.
- The extension: `lughac explain` failing to start (old `lughac`) → the tab says so, naming `lugha.path` and milestone 86.

## SECURITY CONSIDERATIONS
- `lughac explain` only prints embedded text. The extension starts `lugha.path explain <CODE>` (the code checked against `^[EW][0-9]{4}$` first) without a shell; this is a new way the extension starts `lughac`, for the user's review, but it runs no workspace code, so it is allowed in untrusted folders.

## TESTS TO WRITE
- [ ] **`tests/explain.rs`:** every code in spec §9's table has an explanation and vice versa; each title equals the table's; each checked example reports its code (and the fixed ones check clean); the command (text, list, lower case, unknown → exit 2).
- [ ] **The hint:** one code, several codes in first-appearance order, warnings only, none for JSON, none for an internal error (`tests/cli.rs`).
- [ ] **`tests/programs/`:** every `.stderr` regenerated; the diff adds only hint lines.
- [ ] **Language server:** a published diagnostic carries `codeDescription.href` (`tests/lsp.rs`).
- [ ] **Extension:** URI ↔ code, rejecting anything but a code; `npm test`.
- [ ] **Regression:** every existing test passes.

## ROLLBACK PLAN
- Branch `prp-116-explain` from `main`; tag `m86`.
- To abandon before merge: delete the branch. After: revert the merge.

## ACCEPTANCE CRITERIA
- [ ] All tests pass; fmt, clippy, `cargo test`, `npm test` (CI in the background).
- [ ] Spec at milestone 86; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] The manual check passes in VS Code (README step): clicking a code opens its explanation.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo test --test explain
cargo run -q -- explain E0501
cd editors/vscode && npm test
```
