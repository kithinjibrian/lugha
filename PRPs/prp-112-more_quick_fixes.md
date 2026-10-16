## FEATURE: more quick fixes — `let mut` for E0501, a misspelled variant for W0103, a std import for E0301; milestone 82.

**Status:** approved 2026-10-16 ("build") — built in session 173; proposed in session 172.
**Milestone:** 82 (one PRP; tag `m82` at merge). Builds on milestone 79's code actions (PRP-109).
**Spec:** `Language v16 Draft.md` decisions 71–75; spec §9 (the `lughac lsp` section's **Quick fixes**), §11 (row 82).

**Decisions (discovery, session 172):**
- three fixes: E0501 → `let mut`, W0103 → the closest variant, E0301 → `import std.<m>;`; no general "did you mean" for names;
- W0103's fix only when one variant is within 2 edits (or differs only in case), the closest; on a tie or nothing close, no fix;
- the std import only for the five std modules.
- No open entries in DECISIONS.md apply.

## OBJECTIVE
```
let total = 0;            💡 Make `total` mutable      →  let mut total = 0;
total = 5;   // E0501

match o {                 💡 Change to `Empty`         →  Some(Empty) => 0,
    Some(Emtpy) => 0,   // W0103
    …

math.sqrt(2.0)            💡 Add `import std.math;`    →  import std.math;   (after the last import)
// E0301
```

## CONTEXT
- **Starting state:** `driver/lsp/code_actions.rs` (240 lines) answers `textDocument/codeAction` with the E0417 fixes, from `Snapshot.partial` of a check of the current text that parsed (`Server::code_action` skips broken snapshots). `check::Partial` records names and definitions (`names`, `definitions`: a use's span → its binding's), `missing_arms`, and the diagnostics. E0501 points at the place (`x`, `p.x`, `xs` in `for v of mut xs`); W0103 at the unused binding (`matching.rs`); E0301 at the undefined name.
- **Ending state:**
  - **`check::Partial`** gains `variants: HashMap<Span, Vec<String>>` — at each W0103, the variant names of the type the binding's slot matches (recorded in `matching.rs`, as `missing_arms` is; none for a non-enum slot).
  - **`driver/lsp/fixes.rs`** (new): `mutable(…)`, `variant(…)`, `import(…)`, each turning one diagnostic into at most one `CodeAction` (title, one `TextEdit`, `isPreferred: true`):
    - `mutable`: the root name of the place at the E0501 span (the text up to the first `.` or `[`); its binding through `partial.definitions`; a fix only when the source just before the binding is `let ` (so not `let mut`, a parameter, a loop variable, `self` or a pattern) and it is in the same file — the edit inserts `mut `.
    - `variant`: the closest of `partial.variants[span]` by edit distance (Levenshtein, case-insensitive first); a fix only within 2 edits and with no tie — the edit replaces the binding.
    - `import`: the E0301 name is a key of `lugha::STD`, the next character after it is `.`, and no import in the file binds it; the edit inserts `import std.<m>;\n` at the start of the line after the last import (found from the snapshot's parsed imports), or at offset 0.
  - **`code_actions.rs`:** dispatches by code (E0417 as now, E0501, W0103, E0301) to `fixes.rs`.
  - **Spec v16:** §9 **Quick fixes** gains the three; §11 row 82; the changes summary.
  - **Extension:** README feature list and a manual step; version bumped (0.5.0) since the README changes. No TypeScript change.
  - **Docs:** CLAUDE.md (tree, status), README, TODO (the stale "Go-to-definition — the PRP after hover" item ticked), CHANGELOG, MEMORY, CONTEXT.
- **Related existing code:** `src/driver/lsp/{code_actions,hover,definition,server}.rs`, `src/check/{partial,matching,assign,for_mut}.rs`, `tests/lsp_code_actions.rs`, `lugha::STD`.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The ending state above, test-first.

### Must NOT Do
- No "did you mean" for other names, no fixes for other codes, no edits in other files.
- No change to the diagnostics themselves (messages, spans, codes).
- No new dependency (edit distance written here, a few lines).

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- Anything not found (no binding, spans out of the text, a broken snapshot) → no fix for that diagnostic, never an error reply.

## SECURITY CONSIDERATIONS
- Unchanged: the server reads only the editor's text and the files it already loads; the fixes are edits the client applies. It writes nothing and runs nothing.

## TESTS TO WRITE
- [ ] **`fixes.rs` unit tests:** edit distance (equal, one edit, case only, ties); the root of `x`, `p.x.y`, `xs[0]`; the `let ` check (`let x`, `let mut x`, a parameter, a `for` variable).
- [ ] **`check::Partial`:** a W0103 records its slot type's variants; a non-enum slot records none.
- [ ] **`tests/lsp_code_actions.rs`** over real stdio, each fix applied and the result re-checked by the server without that diagnostic:
  - E0501 on `x = 5`, `p.x = 1`, `xs[0] = 1` and `for v of mut xs` → `let mut`; a parameter and a `for` variable → no fix;
  - W0103 `Some(Emtpy)` → `Empty`; `Some(empty)` → `Empty`; a binding far from every variant → no fix; a tie → no fix;
  - E0301 `math.sqrt(2.0)` with other imports → inserted after them; with none → at the top; `nope.x` → no fix; `math` used without `.` → no fix.
- [ ] **Regression:** the E0417 fixes unchanged; every existing test passes; `npm test`.

## ROLLBACK PLAN
- Branch `prp-112-more_quick_fixes` from `main`; tag `m82`.
- To abandon before merge: delete the branch. After: revert the merge.

## ACCEPTANCE CRITERIA
- [ ] All tests pass; fmt, clippy, `cargo test`, `npm test` (CI in the background).
- [ ] Spec at milestone 82; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] The manual check passes in VS Code (README step).

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo test --test lsp_code_actions
cd editors/vscode && npm test
```
