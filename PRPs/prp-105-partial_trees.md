## FEATURE: Partial syntax trees for the language server — check the text being edited even with syntax errors, so hover, definitions, completion and signature help work in the rest of the file; milestone 75.

**Status:** approved 2026-10-15 ("build it") — built in session 157; proposed in session 156.
**Milestone:** 75 (one PRP; tag `m75` at merge).
**Spec:** `Language v16 Draft.md` decisions 38–41; spec §3 (recovery), §9 (the `lughac lsp` paragraph), §11 (row 75).

**Decisions (discovery, session 156):**
- statement- and item-level partial trees: what the parser already recovers; no expression error nodes, no lexer recovery;
- while a file has a syntax error, the editor shows only the syntax errors; the partial check is used silently.
- No open entries in DECISIONS.md apply.

## OBJECTIVE
With a syntax error in a file — a half-typed line, a missing `;` — the editor keeps working everywhere else:

```
fun main() {
    let p = Point { x: 1.0 };
    let total = sum(xs)          // ← missing `;`: a syntax error
    println(p.x);                // hover `p`, Ctrl+click `Point`, `p.` completion — all still work
}
```

Today one syntax error anywhere leaves no tree, so hover and go-to-definition answer nothing and completion falls back to an older check. `lughac check`, `build` and `run` are unchanged.

## CONTEXT
- **Starting state:**
  - `parser::parse_from` recovers after an error — `sync_stmt` skips to the statement's end, `sync_item` to the next item — and builds `(imports, items)`, then returns only `Err(diagnostics)` if there were any (`src/parser/mod.rs`);
  - `modules/parse.rs::parse_module` stores `program: None` for such a module, so its imports aren't followed and `pipeline::parse_loaded` stops before checking;
  - the server keeps `snapshots` (current text, only when it parsed) and `last_good` (completion and signature help, milestone 73).
- **Ending state:**
  - **Parser:** `parser::parse_partial_from(tokens, first) -> (Program, Vec<Diagnostic>)` — the same parse, always returning the recovered program with its diagnostics. `parse` and `parse_from` keep their signatures and results (they become thin wrappers).
  - **Loader:** `LoadEnv` gains `partial: bool` (default `false`; `LoadEnv::plain()` and the CLI's env unchanged). With it, `parse_module` keeps a module's recovered program beside its syntax errors, so its imports are followed too.
  - **Pipeline:** `pipeline::front_partial` loads with `partial: true`. If every module has a program, it checks them with `check_partial` even when the loader reported syntax errors; the result carries the loader's diagnostics apart from the checker's. If a module has none (it didn't lex), it fails as before.
  - **Server:**
    - `check.rs` publishes the loader's diagnostics when they hold an error (syntax only, decision 40), otherwise the checker's as today;
    - `snapshots` now also hold checks of text with syntax errors, so hover and definitions answer from them;
    - completion and signature help use `snapshots` first and `last_good` only when the current text has none (decision 39); the alignment rule stays for that fallback (and is trivially full for a current check).
  - **Tests:** parser unit tests; a loader test; `tests/lsp_partial.rs` (hover, definitions, completion, signature help and diagnostics with a syntax error in the file).
  - **Docs:** spec v16 §3 (the parser's recovery as a language-server partial tree, the CLI unchanged), §9 (the `lughac lsp` paragraph: hover's "while a module doesn't parse" sentence becomes "doesn't lex"; completion's fallback), §11 row 75, the summary; CLAUDE.md tree; README; the extension README; TODO; CHANGELOG; MEMORY; CONTEXT.
- **Related existing code:** `src/parser/{mod,recover,stmt,item}.rs`, `src/modules/{parse,mod,layout}.rs` (`LoadEnv`), `src/driver/pipeline.rs`, `src/driver/lsp/{check,server,completion,signature}.rs`.
- **Open decisions that must be resolved first:** none.

### Design
1. **What a partial program holds:** exactly what the parser's recovery already keeps — items that parsed; in a function body, the statements that parsed; the broken statement or item left out. Expression ids stay dense and unique (`expr_count` as today), since the parser numbers everything it creates.
2. **The checker on a partial program** runs unchanged. Names a dropped statement would have declared are simply unknown; those errors are never shown (decision 40). A panic is caught per check as before.
3. **Diagnostics published:** with a syntax error (an error from the lexer or parser stage, `E01xx`/`E02xx`), the loader's diagnostics only; without, today's behaviour.
4. **Freshness and fallback:** unchanged for hover and definitions (they now answer more often). Completion and signature help: a current snapshot for the file → use it; none (a lex error) → `last_good`.
5. **CLI:** `LoadEnv.partial` is false everywhere outside the server; `lughac check`'s output stays byte-identical — the existing CLI, programs and spec tests prove it.

### Risk
- **Medium:** the checker sees programs it never saw before (statements missing). Mitigations: panics are caught per check; diagnostics from such checks are never shown; tests cover typical broken files (a missing `;`, an unclosed call, a half-typed `let`, a stray token in an item).
- **Cascades** are hidden, not fixed: hover may say nothing on a name whose `let` is the broken line.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The design above, test-first.
- **Spec v16:** §3 (partial trees for the language server; the CLI unchanged), §9 (`lughac lsp` paragraph updated), §11 row 75, the changes summary.
- **Docs:** CLAUDE.md (tree: `tests/lsp_partial.rs`; the parser's and loader's partial modes), README, the extension README (feature note, manual check), TODO (partial trees done; expression error nodes and lexer recovery as later items), CHANGELOG, MEMORY plan and built entries, CONTEXT; tag `m75`.

### Must NOT Do
- **No expression error nodes; no lexer recovery** (not chosen; later items).
- **No change to `lughac check`, `build`, `run`, their output or exit codes.**
- **No type errors shown** for a file while it has a syntax error (decision 40).
- **No change to diagnostic codes or messages.**
- **No extension code change.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **`parse_partial_from` never fails:** it returns whatever was recovered (possibly no items) and all diagnostics.
- **A module that doesn't lex** still has no program: the server falls back as before (decision 39).
- **A panic** in the checker on a partial program: caught (PRP-100); one internal-error diagnostic is not published while syntax errors are (decision 40), but is logged.

## SECURITY CONSIDERATIONS
- Nothing new is read, run or written; the server checks the same files with the same offline, no-write rules (PRP-100). Recovery is bounded by the parser's existing nesting limit (E0206).

## TESTS TO WRITE
- [ ] **`parser` unit tests:** `parse_partial_from` on a valid program equals `parse_from`'s; with a broken statement keeps the function and its other statements; with a broken item keeps the other items; with a broken import keeps the rest; diagnostics equal `parse_from`'s `Err` list.
- [ ] **`modules` test:** with `partial: true`, a module with a syntax error has a program and its imports are loaded; with `false`, as today.
- [ ] **`tests/lsp_partial.rs`** (real server, a file with a syntax error — a missing `;` — on another line):
  - diagnostics show only the syntax error (no `E03xx`/`E04xx`);
  - hover on a variable and a call works; go-to-definition on a type and a method works;
  - completion on `p.` works in a file that never parsed (no last good check at all);
  - signature help on `max(1.5, ` filled in from the current check;
  - a module with a syntax error still has its functions completed and hovered from the main file;
  - an unclosed string (a lex error) → completion falls back to the last good check.
- [ ] **Regression:** `tests/cli.rs`, `tests/programs.rs`, `tests/spec.rs` byte-identical; all language-server tests pass.
- [ ] **Manual check** (extension README), by the user: delete a `;` and hover, Ctrl+click and complete elsewhere in the file.

## ROLLBACK PLAN
- Branch `prp-105-partial_trees` from `main`; tag `m75`.
- To abandon before merge: delete the branch. After: revert the merge; the server returns to needing a tree that parses.

## ACCEPTANCE CRITERIA
- [ ] All tests pass; fmt, clippy, `cargo test`, the extension's `npm test` (CI in the background).
- [ ] Spec at milestone 75; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] The manual check passes in VS Code.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo test --test lsp_partial --test cli
```
