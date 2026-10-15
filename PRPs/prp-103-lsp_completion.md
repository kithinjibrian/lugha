## FEATURE: Completion in `lughac lsp` — members after `.` and names in scope, with types and headers, from the last check that parsed; milestone 73.

**Status:** proposed 2026-10-15 — session 151 (discovery the same session); waits for approval.
**Milestone:** 73 (one PRP; tag `m73` at merge). The fourth language-server PRP (diagnostics, hover, go-to-definition).
**Spec:** `Language v16 Draft.md` decisions 28–32; spec §9 (the `lughac lsp` paragraph), §11 (row 73).

**Decisions (discovery, session 151):**
- completion after `.` and for plain names, with details; no snippets;
- mid-typing, types come from the last check that parsed, trusted while the text before the receiver is unchanged; no parser change.
- No open entries in DECISIONS.md apply.

## OBJECTIVE
In VS Code, typing offers Lugha-aware suggestions:

```
p.|          →  x: f64 (field) · len — fun P.len(self): f64 (method)
xs.|         →  len · push · pop · map · filter · … (built-in and prelude helpers)
geo.|        →  double — fun double(n: i64): i64 · Point (struct)
Shape.|      →  Circle (variant) · Dot (variant) · new — fun Shape.new(): Shape
tot|         →  total — let total: i64 · twice — fun twice(n: i64): i64 · Point · geo · println · Some · …
```

It works while you type `p.` (which doesn't parse yet) because the receiver's type comes from the last check that did. The extension needs no change: `vscode-languageclient` asks once the server advertises completion.

## CONTEXT
- **Starting state:**
  - the server keeps one `Snapshot` per main file (source texts, modules, the checker's `Partial`) and **deletes** it when a check fails to load or parse, so hover and definitions answer nothing rather than answer from old text;
  - `Partial` has every expression's type and span, names with declarations (`names`, `definitions`) — but not the type tables: struct fields, methods, enum variants, interface items;
  - the parser returns no tree when there is an error (it recovers only to report more errors);
  - intrinsics are listed in `check/env.rs` (`INTRINSICS`, `input::INPUT`), built-in array and string methods in `check/builtins.rs::builtin`; prelude helpers (`xs.map`, `it.filter`, …) are resolved in `helpers.rs`/`iter_helpers.rs`.
- **Ending state:**
  - **Checker:** `Partial.members: HashMap<String, Vec<Member>>` — for each struct, enum and interface key, its fields, variants, methods and associated functions: `Member { name, kind, ty_or_decl }` (a field's type, a variant's payloads, a method's declaration span for its header). Built once at the end of `check_partial` from tables the checker already has. Plus `Partial.builtins(&Type) -> Vec<Member>`-style listing for arrays and strings (the methods `builtin` accepts and the prelude helpers that apply), kept next to the code that resolves them so the lists can't drift (a test checks each listed name resolves).
  - **Server:**
    - `server.rs` keeps `last_good: HashMap<PathBuf, Snapshot>` beside `snapshots`: a check that parses updates both; one that doesn't only removes from `snapshots` (hover and definitions unchanged).
    - `driver/lsp/completion.rs` (new):
      - **context** from the current text at the cursor: the word being typed (identifier characters before the cursor) and whether a `.` precedes it;
      - **alignment:** the last good snapshot's text of this file must equal the current text up to the receiver's end (member completion) or up to the word's start (plain names); otherwise offsets aren't trusted (decision 29);
      - **members:** the smallest checked expression ending exactly at the `.` → its type → `members` (structs, enums, interfaces; `Option`/`Result` and arrays/strings through the built-in listing); a module name (`definitions` → `Target::Module`) → that module's items, minus `private` ones from another module; a type name (`definitions` → a struct or enum declaration) → its variants and associated functions;
      - **plain names:** locals and parameters (`names` → `Local`) declared inside the enclosing function's span and before the cursor (only when aligned); the program's functions, types, interfaces and enums of the current module; imported module names; `INTRINSICS` and `INPUT`; `Some`, `None`, `Ok`, `Err`; the keywords from the lexer's list;
      - **items:** `CompletionItem { label, kind, detail }` — detail as hover renders it (`x: f64`, `fun P.len(self): f64`, `let total: i64`); methods get `insert_text` of their name only (no snippets).
    - `mod.rs` advertises `completionProvider { triggerCharacters: ["."] }` and routes `textDocument/completion`.
  - **Tests:** `tests/lsp_completion.rs` (the shared client gains `completion()`); unit tests in `check/partial.rs` (members) and `driver/lsp/completion.rs` (context, alignment).
  - **Docs:** spec v16 §9 (completion in the `lughac lsp` paragraph), §11 row 73, the summary; CLAUDE.md tree; README; the extension README; TODO; CHANGELOG; MEMORY; CONTEXT.
- **Related existing code:** `src/check/partial.rs`, `structs.rs`, `enums.rs`, `methods.rs`, `interfaces.rs`, `builtins.rs`, `helpers.rs`, `iter_helpers.rs`, `env.rs` (`INTRINSICS`), `input.rs` (`INPUT`), `levels.rs`/`visibility.rs` (what `private` hides); `src/lexer/token.rs` (`keyword`); `src/driver/lsp/{server,hover,definition,mod}.rs`.
- **Open decisions that must be resolved first:** none.

### Design
1. **Freshness.** As hover: a pending check for the file runs first. Completion then uses `last_good` for the first planned main that loaded the file.
2. **Context**, at byte offset `c` of the current text: `word` = the identifier characters ending at `c`; `dot` = `true` if the character before `word` is `.`.
3. **Alignment.** Let `p` be the receiver's end (the `.`'s offset) or `word`'s start. The snapshot's text of the file must equal the current text on `[0, p)`; then an offset below `p` means the same thing in both. Member completion needs it; plain names need it only for locals.
4. **Candidates** as in the ending state; the editor filters by `word` and orders them. Duplicates (a name both local and global) keep the innermost.
5. **Nothing found** (no snapshot, not aligned for a member, an unknown receiver type) → an empty list, not `null`.

### Risk
- **Medium:** the member tables are new data but read-only views of existing checker tables; the built-in listing must match what resolves (guarded by a test). No change to checking or codegen.
- **Stale offsets** are the main way to be wrong; alignment (design 3) refuses rather than guesses.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The design above, test-first.
- **Spec v16:** the `lughac lsp` paragraph gains completion (what is offered where, the last-good-check rule, empty results); §11 row 73; the changes summary.
- **Docs:** CLAUDE.md (tree: `driver/lsp/completion.rs`, `tests/lsp_completion.rs`), README, the extension README (feature list, manual check), TODO, CHANGELOG, MEMORY plan and built entries, CONTEXT; tag `m73`.

### Must NOT Do
- **No snippets**, signature help, auto-imports or completion item resolve (not chosen / later).
- **No parser change** and no partial trees (decision 29).
- **No answering hover or definitions from `last_good`** — they keep answering only from the current text's check.
- **No ranking** beyond the editor's own (decision 31).
- **No change to `lughac check`, its output or codegen's input; no extension code change.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **Never fails the server:** no snapshot, misaligned text, an unknown document or position → an empty list. Malformed params → `InvalidParams`.
- **A panic** during the check a completion triggers: caught as before; the answer is an empty list.

## SECURITY CONSIDERATIONS
- Nothing new is read, run or written; completion reads results the server already computed. Offline, no writes (PRP-100).

## TESTS TO WRITE
- [ ] **`check::partial` unit tests:** `members` for a struct (fields with types, methods with declarations), an enum (variants, associated functions), an interface (its methods); the built-in listing for arrays and strings, each name of which the checker resolves.
- [ ] **`driver::lsp::completion` unit tests:** context (`p.`, `p.le`, `tot`, inside a string or comment → nothing); alignment (equal prefix → trusted; an edit before the receiver → not).
- [ ] **`tests/lsp_completion.rs`** (real server over stdio):
  - `initialize` advertises `completionProvider` with `.` as a trigger;
  - `p.` typed after a check (the text no longer parses) → `x` (field, `f64`) and `len` (method, its header);
  - `xs.` on an array → `len`, `push`, `map`; `s.` on a string → its methods;
  - `geo.` → `double` and a public struct, not a `private` function;
  - `Shape.` → its variants and an associated function;
  - a plain word in a function → its earlier locals and parameters (not later ones, not another function's), the program's functions and types, `println`, `Some`, keywords;
  - an edit earlier in the file since the last good check → member completion empty, plain names without locals;
  - no snapshot (a file never opened) → empty.
- [ ] **Regression:** hover and definitions unchanged (`tests/lsp_hover.rs`, `tests/lsp_definition.rs`); every existing test passes.
- [ ] **Manual check** (extension README), by the user: `p.`, `geo.`, `Shape.` and a plain word in VS Code.

## ROLLBACK PLAN
- Branch `prp-103-lsp_completion` from `main`; tag `m73`.
- To abandon before merge: delete the branch. After: revert the merge; the server stops advertising completion.

## ACCEPTANCE CRITERIA
- [ ] All tests pass; fmt, clippy, `cargo test`, the extension's `npm test` (CI runs in the background, not waited on).
- [ ] Spec at milestone 73; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] The manual check passes in VS Code.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo test --test lsp_completion --test lsp_definition --test lsp_hover --test lsp
```
