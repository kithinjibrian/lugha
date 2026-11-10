## FEATURE: The add-missing-arms quick fix — code actions on E0417 that add every missing arm, or a `_` arm; milestone 79.

**Status:** approved 2026-10-16 ("build it") — built in session 167; proposed in session 166.
**Milestone:** 79 (one PRP; tag `m79` at merge). The language server's first code actions.
**Spec:** `Language v16 Draft.md` decisions 56–59; spec §5 (exhaustiveness, E0417), §9 (the `lughac lsp` paragraph), §11 (row 79). No language change.

**Decisions (discovery, session 166):**
- new arms' bodies are `panic("not handled")`;
- two quick fixes: "Add missing arms" and "Add a `_` arm".
- No open entries in DECISIONS.md apply.

## OBJECTIVE
```
match s {                         E0417: non-exhaustive match: `Rect`, `Dot` not covered
    Circle(r) => 3.14 * r * r,       💡 Add missing arms   💡 Add a `_` arm
}
                     ↓ Add missing arms
match s {
    Circle(r) => 3.14 * r * r,
    Rect(_, _) => panic("not handled"),
    Dot => panic("not handled"),
}
```

## CONTEXT
- **Starting state:** `check/matching.rs::exhaustive` computes every missing pattern (`self.space().missing(…)`) and reports E0417 with at most three, a top-level variant with payloads shown bare (`show(&w[0], true)`); integers and strings get `non_exhaustive_values`. The server has no code actions. Diagnostics carry the E0417 span (`match` to the scrutinee's end).
- **Ending state:**
  - **Checker:** `Partial.missing_arms: HashMap<Span, Vec<String>>` — by the E0417 span, every missing pattern in full (`show(&w[0], false)`: `Rect(_, _)`), recorded where E0417 is reported; for `non_exhaustive_values`, an empty list (only `_` applies).
  - **Server:** `driver/lsp/code_actions.rs` (new): for each E0417 diagnostic in the request's range (from `context.diagnostics`, or the file's current ones), find the `match` in the snapshot's AST whose span starts at the diagnostic's start; build the edits — after the last arm (a `,` if it lacks one), each new arm on its own line at the first arm's indentation (the `match` line's plus four spaces when there are no arms), before the closing `}`. Actions of kind `quickfix`: "Add missing arms" (skipped when the list is empty) and "Add a `_` arm", each with the diagnostic attached; "Add missing arms" is `isPreferred`. `mod.rs` advertises `codeActionProvider` (kinds `quickfix`) and routes `textDocument/codeAction`; `server.rs` answers from the current snapshot only when it isn't broken.
  - **Tests:** `tests/lsp_code_actions.rs`; unit tests for the edit text (indentation, the trailing comma, an empty match).
  - **Docs:** spec v16 §9 (code actions in the `lughac lsp` paragraph), §11 row 79, the summary; CLAUDE.md tree; README; the extension README (its feature list and manual check — a docs-only change there, but per session 164 any change in `editors/vscode/` bumps the version: 0.3.2); TODO; CHANGELOG; MEMORY; CONTEXT.
- **Related existing code:** `src/check/{matching,partial,usefulness}.rs`, `src/check/errors/types.rs`; `src/driver/lsp/{mod,server,publish}.rs`; `src/ast/pattern.rs` (match arms and spans).
- **Open decisions that must be resolved first:** none.

### Design
1. **Finding the match:** the E0417 span starts at the `match` keyword; the snapshot's modules are walked for the `Match` expression with that start (the expression's span ends at its `}`).
2. **Text:** for each missing pattern `p`: `{indent}{p} => panic("not handled"),\n`; for the `_` fix: `{indent}_ => panic("not handled"),\n`. Inserted at the start of the line holding the closing `}` (or, if the `}` shares a line with the last arm, after the last arm with a line break first).
3. **Freshness:** as hover — a pending check runs first; no actions from a broken (partial) or stale check.

### Risk
- **Low:** new, read-only data in `Partial` and a new request. The edit is plain text placed from AST spans.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The design above, test-first.
- Spec v16 §9, §11, summary; docs; tag `m79`.

### Must NOT Do
- **No other code actions** (unused bindings, imports…) — later items.
- **No change to E0417's message or the checker's rules.**
- **No extension change** (the client asks for code actions on its own).

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **Never fails the server:** no snapshot, a broken one, a diagnostic that isn't E0417, a match not found → no actions (`[]`). Malformed params → `InvalidParams`.

## SECURITY CONSIDERATIONS
- The server only proposes edits; the editor applies them on the user's choice. Nothing is read, run or written by the server.

## TESTS TO WRITE
- [x] **`check::partial` unit test:** `missing_arms` holds full patterns (`Rect(_, _)`, nested `Some(Dot)`) beyond the three the message shows; an integer match records an empty list.
- [x] **`driver::lsp::code_actions` unit tests:** arm text and placement for a multi-line match, a match whose last arm lacks a comma, a one-line match, an empty match.
- [x] **`tests/lsp_code_actions.rs`** (real server): `initialize` advertises `codeActionProvider`; on an E0417 for an enum both actions with the expected edits, and applying "Add missing arms" gives a program that checks; on an `Option<Shape>` match nested patterns; on an integer match only "Add a `_` arm"; no actions without an E0417 or in a file with a syntax error.
- [x] **Regression:** every existing test passes; E0417's text unchanged.
- [ ] **Manual check** (extension README), by the user: the lightbulb on a non-exhaustive match, both fixes.

## BUILT (session 167)
- As designed: `Partial.missing_arms` (recorded in `matching.rs::exhaustive`; 1 test written first); `driver/lsp/code_actions.rs` (5 unit tests); `textDocument/codeAction` with `codeActionProvider` (`quickfix`); `tests/lsp_code_actions.rs` (4), which applies every fix and runs `lughac check` on the result.
- **Placement from text, not the AST:** the AST has no general expression walker, so the edit is found by scanning from the scrutinee's end to the matching `}` (strings, `char`s, line and nested block comments skipped); one replace edit covers the comma after the last arm, the new arms and the closing line.
- An interface value's single missing case `_` yields only the `_` fix.
- Extension 0.3.2 (README only).
- The manual check in VS Code is left to the user (extension README, step 13).

## ROLLBACK PLAN
- Branch `prp-109-missing_arms` from `main`; tag `m79`.
- To abandon before merge: delete the branch. After: revert the merge.

## ACCEPTANCE CRITERIA
- [x] All tests pass; fmt, clippy, `cargo test` (CI in the background).
- [x] Spec at milestone 79; docs updated.
- [x] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] The manual check passes in VS Code.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo test --test lsp_code_actions
```
