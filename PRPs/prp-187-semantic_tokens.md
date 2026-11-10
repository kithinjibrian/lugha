## FEATURE: Semantic tokens — `lughac lsp` colours every token of a `.la` file, names by what the checker resolved them to, so editors without the TextMate grammar (Neovim, Helix, Zed) highlight Lugha; milestone 154 (v17).

**Status:** approved and built 2026-11-03; proposed the same day, from the user setting up `lughac lsp` in Neovim 0.11: everything works but `.la` files have no colour, because the only Lugha grammar is VS Code's TextMate one (`editors/vscode/syntaxes/lugha.tmLanguage.json`) and Neovim reads none. TODO.md lists it ("Semantic highlighting").
**Milestone:** 154 (one PRP; tag `m154` at merge).
**Spec:** §9 (the `lughac lsp` section gains **Semantic tokens**), §11 (row 154), "Changes since v16"; the Draft gains the decisions below.

**Decisions (discovery, 2026-11-03):**
- **Scope: everything.** Lexer tokens (keywords, strings, numbers, comments, operators) and names classified by what the checker resolved them to — so a client with no grammar colours the whole file.
- **VS Code keeps its TextMate grammar.** It colours at once, before the server answers; semantic tokens refine names on top (VS Code's normal layering). No extension code change.
- **`textDocument/semanticTokens/full` only.** No `range`, no `full/delta`: files are small and a check already runs per change; add them when a measured file needs them.
- **Modifiers: `declaration`, `mutable`, `defaultLibrary`, `documentation`.**
- No open entries in DECISIONS.md apply.

## OBJECTIVE
```lugha
/** Doubles. */                 doc comment            comment + documentation
fun twice(mut n: i64): i64 {     fun keyword; twice     function + declaration; n parameter + declaration + mutable; i64 type + defaultLibrary
    n = n * 2;                   n parameter + mutable; * operator; 2 number
    return n;
}
fun main(): i32 {
    let p = Point { x: 1.0 };    p variable + declaration; Point struct; x property
    println("{twice(3)}");       println function + defaultLibrary; "{ … }" string pieces; twice function
    return 0;
}
```
In Neovim 0.11 with only `vim.lsp.enable("lugha")`, a `.la` file is coloured through the default `@lsp.type.*` highlight groups; in VS Code the grammar colours first and the tokens refine names (a parameter coloured as a parameter, a struct as a struct).

## CONTEXT
- **Starting state:** `src/driver/lsp/` answers hover, definition, references, rename, completion, signature help, formatting and code actions from each main file's last check (`Snapshot` in `server.rs`; `check::Partial` with `names: Span → Name` — `Local { mutable }`, `Function`, `Field`, `Const`, `Included` — and `definitions: Span → Target` — `Decl(span)`, `Module(path)`, `File`). `lexer::lex` gives tokens (`TokenKind`: literals, the interpolation pieces `StrStart`/`StrMid`/`StrEnd`/`FmtSpec`, `Ident`, keywords, punctuation); `lexer::comments` and `lexer::doc_comments` give comment spans. Positions are UTF-16 (`position_encoding`). A file that doesn't lex has no tokens (`lex` returns `Err`).
- **Ending state:**
  - **`src/driver/lsp/tokens.rs`** (new; split in two if it nears 500 lines: `tokens.rs` for the legend and encoding, `tokens/classify.rs` for names):
    - The legend, fixed and in this order — types: `namespace`, `type`, `struct`, `enum`, `interface`, `typeParameter`, `parameter`, `variable`, `property`, `enumMember`, `function`, `method`, `decorator`, `keyword`, `comment`, `string`, `number`, `operator`; modifiers: `declaration`, `mutable`, `defaultLibrary`, `documentation`. All are LSP's predefined names, so every client and theme already maps them.
    - `tokens(text, snapshot, file) → Vec<SemanticToken>`: every lexer token and comment in source order, classified, encoded as LSP's relative five-integer form with UTF-16 lengths. A token spanning lines (a block comment, a multi-line string) is split into one token per line (neither VS Code nor Neovim asks for `multilineTokenSupport`).
    - **Lexical kinds:** keywords (`true`/`false`/`self` included) → `keyword`; `Int`/`Float` → `number`; `Str`, `Char` and every interpolation piece (with its quotes and braces) → `string`; `FmtSpec` → `string`; operators → `operator`; brackets, `,`, `;`, `:`, `.` → no token (themes colour them as plain text); `//` and `/* */` → `comment`, a doc comment → `comment` + `documentation`; an attribute such as `@test` → `decorator`.
    - **Names** (`Ident`), from the check: `Name::Local` → `parameter` or `variable` (+ `mutable` when `mutable`); `Name::Function` → `function`, or `method` for a method or associated function; `Name::Field` and a `.x` resolving to a field → `property`; `Name::Const` → `variable`; `Target::Module` → `namespace`; `Target::Decl` of a struct, enum, interface, variant or type parameter → `struct`/`enum`/`interface`/`enumMember`/`typeParameter`; a built-in type name (`i64`, `string`, `Option`, …) → `type` + `defaultLibrary`; an intrinsic (`println`) or a name declared in `std` or the prelude → its kind + `defaultLibrary`. A name at its own declaration adds `declaration`. A name the check couldn't resolve gets no token, never a guess.
    - **How a `Decl(span)` gets its kind:** by the declaration the span names. If no existing table says whether a span is a struct, enum, interface, variant, type parameter, method or function, the checker's `Recorder` gains `kinds: HashMap<Span, DeclKind>`, filled where it already records declarations — a recording gap of the same kind as milestone 72's, allowed by this PRP; anything larger stops the work (SCOPE RULE).
    - **A file that doesn't lex** (an unclosed string while typing): the tokens of the text before the first lex error, lexed alone and classified from the last check, so colour stays up to the cursor instead of vanishing.
  - **`mod.rs`/`server.rs`:** `semantic_tokens_provider: SemanticTokensOptions { legend, full: Some(SemanticTokensFullOptions::Bool(true)), range: None }`; `textDocument/semanticTokens/full` answered after bringing pending checks up to date (`fresh`), as hover does; a document the server doesn't have → `null`.
  - **Extension:** no TypeScript change (`vscode-languageclient` requests tokens itself). README: a line on semantic highlighting; version bump (minor).
  - **Neovim:** the repository README gains a short "Neovim" section with the 0.11 setup (`vim.filetype.add`, `vim.lsp.config("lugha", …)`, `vim.lsp.enable`). No plugin, no files under `editors/`.
  - **Spec v17:** §9 **Semantic tokens** (the legend, what each kind covers, unresolved names have none, the lex-error rule); §11 row 154; "Changes since v16"; the Draft's decisions behind milestone 154 (the four above).
  - **Docs:** CHANGELOG, TODO (remove "Semantic highlighting"), CLAUDE.md if the layout line changes.
- **Related existing code:** `src/driver/lsp/{hover,definition,references,server,mod}.rs`, `src/check/partial.rs`, `src/lexer/{mod,token}.rs`, `tests/common/lsp_client.rs`, `tests/lsp_definition.rs`.
- **Open decisions that must be resolved first:** none.

## REQUIREMENTS
### Must Do
- The ending state above, test-first.
- One source of token spans: the lexer. The checker only classifies; no second scanner.
- Tokens sorted, non-overlapping, never past the end of the text — the encoding rejects nothing silently: a token that would overlap the previous one is a bug (`debug_assert!`), not dropped.

### Must NOT Do
- No `semanticTokens/range` or `full/delta`; no `multilineTokenSupport` reliance.
- No TextMate grammar change and no grammar removal; no tree-sitter grammar (a separate idea, not planned).
- No custom token types or modifiers beyond the legend (custom names need client-side theme setup).
- No new dependency (`lsp-types` already has the semantic-token types).

### New dependencies
None.

## DESIGN
- **Errors:** none new. Bad request parameters → `InvalidParams` (the existing `invalid` helper). A panic while classifying is caught by `check.rs`'s guard like any request; the reply is `null`.
- **Security:** unchanged — reads only the editor's texts and the files checks already load; offline; writes nothing.

## TESTS
- [ ] **Unit (`tokens.rs`):** the relative encoding (same line, next line, UTF-16 lengths with a non-ASCII string); a block comment over three lines split into three tokens; a doc comment carries `documentation`; interpolation pieces and a format spec are `string`; a text that doesn't lex gives the tokens before the error.
- [ ] **`tests/lsp_semantic_tokens.rs`** over real stdio, decoding the reply against the legend: the OBJECTIVE program — keywords, a function's declaration and call, a `mut` parameter (`parameter` + `mutable` + `declaration`), a local, a struct and its field, a variant in a `match`, an imported module (`namespace`), `println` and `i64` (`defaultLibrary`), a constant, a generic's type parameter, `@test` (`decorator`); an unresolved name has no token; the capability is declared with the legend in order; an unopened document → `null`.
- [ ] **Extension:** `npm test` passes.
- [ ] **Regression:** `cargo test --lib`, the `lsp_*` integration tests.

## ACCEPTANCE
- [ ] Neovim 0.11, with only the README's setup, colours `rapidkoi/src/main.la` (the user checks).
- [ ] VS Code still colours from the grammar at once, and names are refined once the server answers (the user checks).
- [ ] `cargo test --lib` and the touched integration tests pass; `cargo fmt --check` and `cargo clippy --all-targets -- -D warnings` are clean.
- [ ] Every `pub` item documented; spec, CHANGELOG, TODO and README updated; no file over 500 lines; no `unsafe`; no new dependency.

## BUILT (2026-11-03)
- `src/driver/lsp/tokens.rs` (legend, lexical kinds, relative encoding, the lex-error cut; 6 unit tests) and `token_names.rs` (`Names`: declaration kinds and parameters from the modules' ASTs, then `names`/`definitions`). **No checker change:** locals are named only at their binding, so a use is classified through its declaration; a type parameter's declaration, which the check doesn't record, through the AST's kinds.
- **Split (approved):** `server.rs` was at 488 lines; `signature_help` and `completion` moved to the child module `server/last_good.rs`, with `semantic_tokens` beside them.
- `Some`, `None`, `Ok`, `Err` are `enumMember` + `defaultLibrary` when unresolved, as intrinsics are `function` + `defaultLibrary` at a call: both have no declaration.
- `tests/lsp_semantic_tokens.rs` (5), written first. Checked in Neovim 0.11 on `rapidkoi/src/main.la`: imports, package types, functions, locals, fields and variants coloured.
- **Found, not fixed (TODO):** an imported enum used as a qualifier (`Msg.Launched` after `import m { Msg };`) has no definition recorded, so Go to Definition misses it too — a checker gap outside this PRP.
- Extension 0.23.0 (README and manual step 24 only).

## ROLLBACK PLAN
- Branch `prp-187-semantic_tokens` from `main`; tag `m154` at merge.
- To abandon before merge: delete the branch. After: revert the merge (the capability disappears; clients fall back to no tokens).
