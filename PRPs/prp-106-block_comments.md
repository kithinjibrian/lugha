## FEATURE: Block comments — `/* … */`, nested, in the compiler and the VS Code grammar; milestone 76.

**Status:** proposed 2026-10-16 — session 158 (discovery the same session); waits for approval.
**Milestone:** 76 (one PRP; tag `m76` at merge). The first of two: `/** … */` doc comments in the language server follow as milestone 77 (its own discovery and PRP).
**Spec:** `Language v16 Draft.md` decisions 42–46; spec §2 (whitespace and comments, lexical errors), §10 (a program), §11 (row 76).

**Decisions (discovery, session 158):**
- block comments nest (Rust, Swift, Kotlin);
- the rest from the draft: comments wherever whitespace may be, E0113 for an unclosed one, `*/` outside a comment unchanged.
- No open entries in DECISIONS.md apply.

## OBJECTIVE
```
/* A block comment
   over several lines. */
fun main() {
    let x = 1 /* inline */ + 2;
    /* commented out:
    println("old");   /* a comment inside */
    */
    println(x);       // 3
}
```

`/* never closed` is E0113 at its `/*`. In VS Code, block comments are highlighted as comments (nested ones too) and `Shift+Alt+A` toggles them.

## CONTEXT
- **Starting state:** `src/lexer/mod.rs::run` skips whitespace and `//` comments (`skip_comment`); `/` is `Slash`, `*` is `Star`; there are no block comments (spec §2). The lexer's codes end at E0112. Interpolations are lexed by the same loop, with a line break inside one an error (E0111). The VS Code grammar (`editors/vscode/syntaxes/lugha.tmLanguage.json`) knows only `//`; `language-configuration.json` has only `lineComment`.
- **Ending state:**
  - `src/lexer/mod.rs`: `run` sends `/*` to a new `skip_block_comment(start)`: depth counting `/*` and `*/` (nesting), skipping everything else including `//`, `"` and `'`; at the end of input with depth > 0, E0113 at the outermost `/*` (span of those two bytes) with help ``close it with `*/` ``, and lexing stops. Inside an interpolation, a line break inside the comment is reported as E0111 for that interpolation, as any line break there is.
  - Spec v16 §2: block comments (nesting, where they may appear, inside strings, inside interpolations), E0113 in the lexical errors table; §10 a program "Block comments"; §11 row 76; the changes summary; §2's "v0 has no block comments" sentence kept as history ("v0 had no block comments; v16 adds them").
  - `tests/programs/m76/`: a program using block comments (inline, multi-line, nested, beside `//`, inside an interpolation on one line), with `.stdout`/`.exit`; a rejected program with an unclosed comment, with `.stderr` (E0113).
  - **VS Code extension:** the grammar gains `comment.block.lugha` with a nested rule; `language-configuration.json` gains `"blockComment": ["/*", "*/"]`; a grammar scope test (`test/grammar/comments.la`).
  - Docs: CLAUDE.md (tree: the lexer line), README (the feature list), the extension README, TODO, CHANGELOG, MEMORY, CONTEXT.
- **Related existing code:** `src/lexer/{mod,interp,string}.rs`, `src/lexer/token.rs`; `editors/vscode/syntaxes/lugha.tmLanguage.json`, `language-configuration.json`, `test/grammar/`.
- **Open decisions that must be resolved first:** none.

### Design
1. **Where:** in `run`, `'/' if next is '*'` (before the `/` operator), alongside `'/' if next is '/'`. A comment produces no token; spans of later tokens are unaffected (byte offsets).
2. **Nesting:** scan bytes from after the `/*`: `/*` → depth + 1, `*/` → depth − 1, stop at depth 0. Nothing else is special — no strings or line comments inside a block comment, so `/* "*/" */` ends at the first `*/` inside the quotes, as in Rust.
3. **Unclosed:** at the end of input with depth > 0: E0113 `unterminated block comment` at the outermost `/*`, label `this comment is never closed`, help ``close it with `*/` ``; the lexer stops (the rest of the file is the comment).
4. **Inside an interpolation:** if the comment's text contains a `\n`, report E0111 at the interpolation's `{` (the existing line-break rule), and continue after the comment.
5. **Multi-byte text** inside a comment is skipped byte-wise safely: `/` and `*` are ASCII, so a scan by bytes never splits a character.

### Risk
- **Low:** one lexer path; no valid program changes meaning (decision 45). Existing lexer, parser, program and spec tests guard the rest.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The design above, test-first.
- Spec v16 §2, §10, §11 and summary; docs; tag `m76`.

### Must NOT Do
- **No doc comments yet** — `/**` is an ordinary block comment until milestone 77.
- **No change to `//` comments** or to any other token.
- **No `*/`-without-`/*` error** — it stays `*` then `/`, a parse error where it doesn't fit.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0113** (new, next lexer code) for an unclosed block comment, at the outermost open `/*`; one report per file.
- **E0111** (existing) for a line break inside a block comment inside an interpolation.

## SECURITY CONSIDERATIONS
- Input is source text; the scan is linear and bounded by the input; nesting depth is a counter (no recursion), so deep nesting can't overflow the stack.

## TESTS TO WRITE
- [ ] **Lexer unit tests:** `/* x */` → no tokens; `a/*x*/b` → `a`, `b`; multi-line; nested (`/* a /* b */ c */ d` → `d`); `/**/` and `/***/`; `//` and `"` inside a comment ignored; `/*` inside a string and after `//` is text; spans of tokens after a comment; unclosed → E0113 at the outermost `/*` (also when an inner one closed); a comment spanning a line inside an interpolation → E0111; `*/` alone → `Star`, `Slash`; very deep nesting (10,000 levels) without a stack overflow.
- [ ] **`tests/programs/m76/`:** `block_comments.la` (+ `.stdout`, `.exit`); `unclosed_comment.la` (+ `.stderr`, `.exit` 1).
- [ ] **Spec §10 program** "Block comments" (extracted and run by `tests/spec.rs`).
- [ ] **Extension:** `test/grammar/comments.la` — a block comment, a nested one, one inline — scopes `comment.block.lugha`; the extension's `npm test`.
- [ ] **Regression:** every existing test passes.
- [ ] **Manual check** (extension README), by the user: highlighting and `Shift+Alt+A`.

## ROLLBACK PLAN
- Branch `prp-106-block_comments` from `main`; tag `m76`.
- To abandon before merge: delete the branch. After: revert the merge; no program depended on block comments before.

## ACCEPTANCE CRITERIA
- [ ] All tests pass; fmt, clippy, `cargo test`, the extension's `npm test` (CI in the background).
- [ ] Spec at milestone 76; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cd editors/vscode && npm test
```
