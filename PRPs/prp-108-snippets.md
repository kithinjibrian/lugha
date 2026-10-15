## FEATURE: Snippets — declaration and control-flow snippets in the VS Code extension, call placeholders and a postfix `.match` from the language server; milestone 78.

**Status:** proposed 2026-10-16 — session 162 (discovery the same session); waits for approval.
**Milestone:** 78 (one PRP; tag `m78` at merge).
**Spec:** `Language v16 Draft.md` decisions 52–55; spec §9 (the `lughac lsp` paragraph), §11 (row 78). No language change.

**Decisions (discovery, session 162):**
- static snippets in the extension, plus smart ones from the server;
- the static set: declarations and control flow (not common calls or example layouts);
- the smart `match` is a postfix completion (`s.match`), not a quick fix on E0417.
- No open entries in DECISIONS.md apply.

## OBJECTIVE
In VS Code:

```
fun⇥          →  fun ${1:name}(${2}): ${3:i64} {
                     ${0}
                 }
for⇥          →  for ${1:x} of ${2:xs} {
                     ${0}
                 }
max  (pick from completion)          →  max(a, b)       with `a` then `b` as tab stops
Shape.Circle  (pick from completion) →  Shape.Circle(f64)
s.match       (s: Shape)             →  match s {
                                            Circle(x) => ,
                                            Dot => ,
                                        }
```

## CONTEXT
- **Starting state:** the extension has no snippets. Completion (PRP-103) offers members after `.` and names, with details and docs; `Here::receiver` resolves the value before `.`; `check::Sig` (PRP-104) has every function's parameter names; `Member` has variants with payload text. The client's capabilities aren't recorded by the server.
- **Ending state:**
  - **Extension:** `editors/vscode/snippets/lugha.json` (VS Code snippet format) with: `fun`, `main`, `struct`, `enum`, `impl`, `interface`, `import`, `doc` (`/** … @param … */`); `if`, `ifelse`, `while`, `forof` (`for … of`), `forin` (`for … in a..b`), `iflet`, `letelse`, `match`. `package.json` contributes it (`contributes.snippets`).
  - **Server:**
    - `server.rs` records whether the client supports snippets (`textDocument.completion.completionItem.snippetSupport`);
    - `driver/lsp/snippets.rs` (new; `completion.rs` is near the file limit, so the snippet logic lives here and completion calls it): a function, method or associated function item gets `insert_text` `name(${1:a}, ${2:b})` (`insert_text_format: Snippet`) — names from `Sig`; a variant with payloads `Circle(${1:f64})`; a built-in its types as placeholders; none of this when the client lacks snippet support or the character after the cursor is `(`;
    - also in `snippets.rs`, the postfix `.match`: for a receiver of type enum, `Option` or `Result` (by the type of the expression before `.`, or of the local named there), a `match` item (kind `Snippet`) whose `text_edit` replaces `receiver.word` with `match receiver {\n\tV(${n:x}) => ${m},\n\t…\n}` — every variant in declaration order, one placeholder per payload slot;
  - **Tests:**
    - `tests/snippets.rs` (Rust, in `cargo test`): reads `editors/vscode/snippets/lugha.json`, expands each body with its placeholder defaults (`${1:x}` → `x`, `${1}`/`$0` → a fitting filler), places it in a small program, and runs `lughac check` — each must check (declarations at the top level, control flow inside `fun main`);
    - `tests/lsp_snippets.rs`: call placeholders (function, method, generic, variant, built-in), none before an existing `(`, none without snippet support; `.match` on an enum, `Option` and `Result` value, and not on a struct;
    - the extension's `npm test` checks the snippet file is valid JSON with the expected prefixes.
  - **Docs:** spec v16 §9 (snippets in the `lughac lsp` paragraph), §11 row 78, the summary; CLAUDE.md tree; README; the extension README; TODO; CHANGELOG; MEMORY; CONTEXT.
- **Related existing code:** `src/driver/lsp/{completion,server,mod}.rs`, `src/check/{members,signatures}.rs`; `editors/vscode/package.json`.
- **Open decisions that must be resolved first:** none.

### Design
1. **Static snippet bodies** use VS Code's syntax (`${1:default}`, `$0`, `\t` for one indentation step). The `doc` snippet produces `/**\n * ${1:What it does.}\n * @param ${2:name} ${3:its meaning}\n */`.
2. **Call placeholders:** `name(` + `${i:param}` joined by `, ` + `)`; a method's `self` is not a parameter; a function value position can't be told apart, so the user deletes the parentheses there (rare).
3. **Postfix `.match`:** the item's `text_edit` range runs from the receiver's start (the shared-prefix expression's span start, or the name's start) to the cursor; the receiver's text is copied verbatim. Placeholders number the payload slots across arms, then each arm's body; `$0` after the closing `}`.
4. **No snippet support** in the client → plain inserts as today, and no `.match` item.

### Risk
- **Low:** additive completion data and a JSON file. The snippet-check test keeps the static set valid as the language evolves.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The design above, test-first.
- Spec v16 §9, §11, summary; docs; tag `m78`.

### Must NOT Do
- **No quick fixes / code actions** (E0417's "add missing arms" is a later item).
- **No snippets for common calls or file layouts** (not chosen).
- **No language change.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **Never fails the server:** a receiver whose type isn't known gets no `.match`; an item without a `Sig` gets no placeholders.

## SECURITY CONSIDERATIONS
- Nothing new is read, run or written. `tests/snippets.rs` runs the freshly built `lughac check` on generated programs in a temporary folder.

## TESTS TO WRITE
- [ ] **`tests/snippets.rs`:** every static snippet expands and checks; the file has every chosen prefix.
- [ ] **`tests/lsp_snippets.rs`:** call placeholders for `twice(${1:n})`, `p.scale(${1:k})`, `max(${1:a}, ${2:b})`, `Shape.Circle(${1:f64})`, `xs.push(${1:i64})`; none when `(` follows; none when the client lacks snippet support; `s.match` on an enum (every variant, payload placeholders), an `Option` and a `Result`; no `.match` on a struct.
- [ ] **Unit tests** in `snippets.rs`: arm text for variants with 0, 1 and 2 payloads; placeholder numbering.
- [ ] **Extension:** `npm test` validates the snippet file.
- [ ] **Regression:** every existing test passes.
- [ ] **Manual check** (extension README), by the user: `fun⇥`, `forof⇥`, a call from completion, `s.match`.

## ROLLBACK PLAN
- Branch `prp-108-snippets` from `main`; tag `m78`.
- To abandon before merge: delete the branch. After: revert the merge.

## ACCEPTANCE CRITERIA
- [ ] All tests pass; fmt, clippy, `cargo test`, the extension's `npm test` (CI in the background).
- [ ] Spec at milestone 78; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] The manual check passes in VS Code.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo test --test snippets --test lsp_snippets
cd editors/vscode && npm test
```
