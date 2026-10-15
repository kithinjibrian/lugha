## FEATURE: Doc comments — `/** … */` before a declaration, Markdown with `@param`/`@returns`, shown by the language server; std and the prelude documented; milestone 77.

**Status:** approved 2026-10-16 ("build it") — built in session 161; proposed in session 160.
**Milestone:** 77 (one PRP; tag `m77` at merge). The second of the two steps the user asked for (milestone 76 added block comments).
**Spec:** `Language v16 Draft.md` decisions 47–51; spec §2 (comments: the doc-comment convention), §9 (the `lughac lsp` paragraph), §11 (row 77).

**Decisions (discovery, session 160):**
- Markdown text plus `@param name text` and `@returns text` (JSDoc style);
- any named declaration may carry one;
- the public std functions and the prelude helpers get docs, from the spec's tables.
- No open entries in DECISIONS.md apply.

## OBJECTIVE
```
/**
 * Returns the larger of `a` and `b`.
 * @param a the first value
 * @param b the second; ties return `a`
 * @returns the larger one
 */
fun max<T: Ord>(a: T, b: T): T { … }
```

- **Hover** on `max` (or a call): the header, then "Returns the larger of `a` and `b`.", then *a* — the first value, *b* — …, *returns* — the larger one.
- **Completion:** the item's documentation pane shows the same.
- **Signature help** on `max(1.5, |`: the description, and "the second; ties return `a`" under the highlighted `b`.
- **Types, fields and variants** too: hovering `Point` shows `struct Point` and its doc (hover answers on type and variant names now).
- **The library:** `math.sqrt`, `fs.list_dir`, `xs.map`, `s.split`, `it.filter`… show their documentation.

Nothing changes for the compiler: a doc comment is a comment.

## CONTEXT
- **Starting state:** block comments exist (milestone 76; `lexer::skip_block_comment`), and the lexer discards them. The server renders headers from source (`hover::declaration`), member details (`completion.rs`), and signatures (`signature.rs`); it keeps each check's `Source` (every file's text) and modules (ASTs with declaration spans). Hover answers on locals, functions and methods, fields at their declaration, and expressions — not on type or variant names. The std modules (`src/std/*.la`) and the prelude (`src/prelude/*.la`) have `//` comments only.
- **Ending state:**
  - **Lexer:** `lexer::doc_comments(src) -> Vec<Span>` — the spans of every doc comment (`/**`, not `/**/`), found by the lexer's own scan, so strings and line comments are respected. `lex` is unchanged.
  - **Server:** `driver/lsp/docs.rs` (new):
    - `Doc { text, params: Vec<(String, String)>, returns: Option<String> }` parsed from a comment (leading ` * ` stripped, common indentation removed, `@param`/`@returns` with continuation lines, other tags kept in the text);
    - `doc_for(snapshot, decl_span) -> Option<Doc>`: the doc comment whose end is followed only by whitespace and an optional `public`/`private` up to the declaration's first token, in the same file; the file's doc spans are computed once per snapshot (cached beside it);
    - Markdown rendering for hover and completion; plain text for signature help's description and parameter docs.
  - **Hover:** after the code block, the doc; and new answers on type names (`struct Point`, `enum Shape`, `interface Area`) and variant names (`Circle(f64)`) from `definitions` → their declaration, with its doc.
  - **Completion:** `CompletionItem.documentation` (Markdown) for every item whose declaration has a doc (members, module items, functions, types, built-in helpers from the prelude).
  - **Signature help:** `SignatureInformation.documentation` = the description (+ `@returns`), `ParameterInformation.documentation` = the parameter's `@param` text; a filled-in generic keeps the written header too.
  - **Library docs:** `/** … */` on every `public` function and method in `src/std/*.la`, and on every prelude function behind a helper completion offers (`check::members` candidates and the iterator helpers) and every other prelude function a program may call by name (`char_from`, `utf8_decode`, `sort`, …) — text adapted from the spec's §"The standard library" tables and §5. A unit test lists each such declaration without a doc and fails if there is one.
  - **Spec v16:** §2 names the convention (what a doc comment is, its tags, that the compiler ignores it); §9's `lughac lsp` paragraph: docs in hover, completion and signature help, hover on type and variant names; §11 row 77; the changes summary.
  - **Tests:** `tests/lsp_docs.rs`; unit tests in `lexer` (`doc_comments`) and `driver/lsp/docs.rs` (parsing, attachment).
  - **Docs:** CLAUDE.md tree; README; the extension README; TODO; CHANGELOG; MEMORY; CONTEXT.
- **Related existing code:** `src/lexer/mod.rs`, `src/driver/lsp/{hover,completion,signature,server}.rs`, `src/check/members.rs`, `src/std/*.la`, `src/prelude/*.la`, spec §5 and "The standard library".
- **Open decisions that must be resolved first:** none.

### Design
1. **Attachment:** `/** … */` → whitespace (and line breaks) → optionally `public` or `private` → the declaration's keyword or name (`fun`, `extern`, `struct`, `enum`, `interface`, a field's or variant's name). Anything else between (code, another comment, a blank line is fine) breaks it. A doc before nothing is ignored.
2. **Text:** strip `/**` and `*/`; from each line, a leading run of spaces, then one `*` and one space if present; then remove the common indentation; trim blank lines at both ends.
3. **Tags:** a line starting with `@param name` starts that parameter's text (to the next tag); `@returns` (or `@return`) the result's; any other `@tag` line stays in the description as written. A `@param` naming no parameter is kept and shown as written.
4. **Rendering (Markdown):** description; then, if any, a line per parameter `*name* — text` and `*returns* — text`.
5. **Performance:** doc spans are found once per checked file (one lexer pass); lookups are by binary search on spans.

### Risk
- **Low-medium:** server-side additions plus documentation text in std/prelude (no code changes there; a doc comment is a comment — the prelude and std still check, which `cargo test` proves). The library docs are the bulk of the work.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The design above, test-first.
- Spec v16 §2, §9, §11, summary; docs; tag `m77`.

### Must NOT Do
- **No compiler behaviour change:** doc comments never affect checking, codegen, diagnostics or `--emit`.
- **No `///` doc comments** (not chosen).
- **No doc-generation tool** (`lughac doc`) — the editor is the only reader for now.
- **No change to the library's code** — comments only.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **Never fails the server:** a malformed doc (no `*` prefixes, unknown tags, an unclosed one — which is already E0113) is shown as best it can be or not at all.
- No new diagnostics.

## SECURITY CONSIDERATIONS
- Doc text comes from the user's files and the bundled sources and is sent to the editor as Markdown, which VS Code renders without HTML or scripts (`MarkupContent`); nothing is executed. No new reads, runs or writes.

## TESTS TO WRITE
- [x] **`lexer` unit tests:** `doc_comments` finds `/** a */` and nested ones, skips `/**/`, `/* */`, and `/**` inside strings and line comments.
- [x] **`driver::lsp::docs` unit tests:** text cleanup (star prefixes, indentation, blank lines); `@param`/`@returns` with continuation lines; unknown tags kept; attachment through `public`, a blank line, and broken by code between.
- [x] **`tests/lsp_docs.rs`** (real server): hover on a documented function, method, struct, field, enum, variant and interface method (and on a type and a variant name in use); completion items carry the documentation; signature help carries the description and the active `@param`; an undocumented declaration shows no doc; `math.sqrt`, `xs.map` and `s.split` show their library docs.
- [x] **Library coverage test:** every `public` std function and method, and every prelude function behind a completion helper or callable by name, has a doc comment.
- [x] **Regression:** every existing test passes (the std and prelude still check; CLI output unchanged).
- [ ] **Manual check** (extension README), by the user: write a `/** */` on a function and see it in hover, completion and signature help; hover `math.sqrt`.

## BUILT (session 161)
- As designed: `lexer::doc_comments`; `driver/lsp/docs.rs` (`Doc`, `parse`, `attached`, `DocCache`, `doc_for`); docs in hover (plus `struct`/`enum`/`interface`/variant names through `definitions`), completion (`documentation`, `Member.decl`), signature help (description, written header for a filled-in generic, per-parameter `@param`). Helper calls (`xs.map`, `it.filter`) now record their name, so hover and docs reach the prelude function.
- Library: 44 doc comments in `src/std/` (every public function and method), 67 in `src/prelude/` (every top-level function: user-facing helpers describe their method form, `xs.map(f)`; internal ones say what uses them). Implementation notes that were `//` comments above a function are kept, moved above its doc (a comment between a doc and its declaration would detach it).
- Tests: lexer (1), `driver::lsp::docs` (4, incl. the coverage test and a check that it catches a miss), `tests/lsp_docs.rs` (5).
- **Visible side effect:** the bundled sources grew, so a panic inside them reports a later line (`<prelude>:135:9` → `154:9`, `<std>/math.la:40:9` → `60:9`, …); four `.stderr` expectations updated after checking each still names the same statement. Noted in the CHANGELOG.
- The manual check in VS Code is left to the user (extension README, step 11).

## ROLLBACK PLAN
- Branch `prp-107-doc_comments` from `main`; tag `m77`.
- To abandon before merge: delete the branch. After: revert the merge; the docs in std and the prelude are comments and harmless either way.

## ACCEPTANCE CRITERIA
- [x] All tests pass; fmt, clippy, `cargo test`, the extension's `npm test` (CI in the background).
- [x] Spec at milestone 77; docs updated.
- [x] No file over 500 lines (the std/prelude files included); no new dependencies; no `unsafe`.
- [ ] The manual check passes in VS Code.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo test --test lsp_docs
```
