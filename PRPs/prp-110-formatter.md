## FEATURE: `lughac fmt` — a tree-based formatter that rewrites `.la` files in one canonical style, keeps every comment, and verifies its own output; milestone 80.

**Status:** approved 2026-10-16 ("build it") — built in session 169; proposed in session 168.
**Milestone:** 80 (one PRP; tag `m80` at merge). The first of two: Format Document and format-on-save in the language server follow as milestone 81 (its own PRP).
**Spec:** `Language v16 Draft.md` decisions 60–65; spec §9 (the CLI table and a `lughac fmt` section with the style), §11 (row 80).

**Decisions (discovery, session 168):**
- tree-based (like rustfmt), not token-based or indentation-only;
- comments attached above or at the end of items, statements, fields, variants and arms; a comment anywhere else keeps that statement or item verbatim;
- 4-space indents, 100 columns;
- two PRPs (the formatter, then the editor);
- the output is always verified before anything is written.
- No open entries in DECISIONS.md apply.

## OBJECTIVE
```
$ lughac fmt src/            # rewrites every .la under src/
$ lughac fmt --check .       # lists files that would change; exit 1 if any
```

```
fun  area(shape:Shape,scale :f64):f64{            fun area(shape: Shape, scale: f64): f64 {
  let total=compute(first_value,second_value,         let total = compute(
      third_value,fourth_value,fifth_value)  // c         first_value,
  match shape{Circle(r)=>3.14*r*r,Dot=>0.0}                second_value,
}                                                          third_value,
                                                           fourth_value,
                                                           fifth_value,
                                                       ); // c
                                                       match shape {
                                                           Circle(r) => 3.14 * r * r,
                                                           Dot => 0.0,
                                                       }
                                                   }
```

## CONTEXT
- **Starting state:** no formatter. The parser builds the whole tree with spans (`ast`); the lexer discards comments (it records only doc-comment spans, `lexer::doc_comments`); `parser::sexp::program` prints a tree without positions (used by `--emit=ast`). Literals in the tree are values (`Int(u64)`, decoded strings), so printing them back would change `0xFF` to `255`.
- **Ending state:**
  - **`src/formatter/`** (new; `src/format.rs` is format specifiers, so the folder is named apart): `mod.rs` (`pub fn format(src: &str) -> Result<String, FormatError>`: lex, parse, attach comments, print, verify), `doc.rs` (a small document algebra — text, line breaks that become spaces when a group fits, nesting — with a width-100 fitting printer, written here: no crate), `items.rs` (imports, functions, externs, structs, enums, impls, interfaces), `stmts.rs` (blocks and statements), `exprs.rs` (expressions with minimal parentheses by precedence, calls, literals, closures, match, patterns, types), `comments.rs` (the source's comments — the lexer's own scan, as `doc_comments` does — and their attachment, decision 62), `verify.rs` (re-parse; compare `sexp::program` of both; compare the comments' texts).
  - **Lexer:** `lexer::comments(src) -> Vec<Comment { span, kind: Line | Block }>` (the same scan that skips them), `lex` unchanged.
  - **Driver:** `lughac fmt [paths…] [--check]` in `driver/mod.rs` and a new `driver/fmt.rs`: expand folders to their `.la` files (sorted; hidden folders and `target/` skipped); for each, format; write only when the text changes; `--check` lists `would reformat <path>` and writes nothing. Syntax errors are printed as diagnostics (the file is skipped). Exit 0; 1 when `--check` found a file to change or a file had syntax errors; 2 when verification failed (a formatter bug) or a file couldn't be read or written.
  - **Spec v16:** §9's CLI table gains `lughac fmt`, and a section lays out the style (decision 61), comments (62), safety (63) and exit codes; §11 row 80; the changes summary.
  - **Tests:** `tests/fmt.rs` (CLI: in place, `--check`, folders, syntax errors, exit codes); unit tests per style rule in `src/formatter/`; a corpus test: every `.la` in `tests/programs/` that parses, every spec §10 program, and the std and prelude sources format, verify, and format again to the same text.
  - **Docs:** CLAUDE.md (tree, COMMANDS), README, TODO, CHANGELOG, MEMORY, CONTEXT.
- **Related existing code:** `src/ast/`, `src/parser/` (precedence in `operators.rs`; `sexp/`), `src/lexer/mod.rs`, `src/driver/{mod,build}.rs`, `src/template.rs` (format templates are literals, printed as written).
- **Open decisions that must be resolved first:** none.

### Design
1. **Literals and names exactly as written** (decision 61): integers, floats, strings (with their escapes and interpolations' inner expressions — the hole's expression is formatted, its surrounding text kept), `char`s and format templates are copied from the source by their spans.
2. **Parentheses:** printed from the tree by precedence and associativity (the parser's table): `(a + b) * c` keeps them, `(a)` loses them. The verification compares trees, where parentheses don't appear, so dropping redundant ones is checked to be safe.
3. **Layout:** every list-like construct is a group: first try it on one line; if it doesn't fit in 100 columns, one element per line with a trailing comma. Blocks are always multi-line (except `{}` empty); a `match` always puts each arm on its own line; a short function body `= expr;` stays on one line when it fits.
4. **Blank lines:** between items and between statements, a run of blank lines in the source becomes one; none at the start or end of a block.
5. **Comments** (decision 62): leading comments (on their own lines before a node) print above it at its indentation; a trailing comment (after a node on its line) prints after it, one space before. Any comment strictly inside a statement or item spanning lines that isn't one of those → that statement or item is copied verbatim (re-indented only at its first line). Doc comments are leading comments.
6. **Verification** (decision 63): parse the output; it must parse, its `sexp::program` must equal the original's, and its comments' texts (in order) must equal the original's. Otherwise `FormatError::Unsafe` (exit 2, nothing written).
7. **What it never does:** rename, reorder items or imports, add or remove `;`, change literals, touch a file that doesn't parse.

### Risk
- **High effort, low danger:** a large printer, but verification (design 6) means a printer bug can only refuse, never corrupt a file. The corpus test runs every program in the repo through it.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The design above, test-first, rule by rule.
- Spec v16 §9 (CLI row and the style section), §11 row 80, summary; docs; tag `m80`.

### Must NOT Do
- **No editor integration** (milestone 81).
- **No reformatting of the repo's own `.la` files** in this PRP (it would move panic locations again, as milestone 77 did); `fmt --check` in CI can come later.
- **No configuration** (`lugha.toml` options, width flags): one style.
- **No new dependency** (the document printer is written here).

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- `FormatError` (thiserror): `Syntax(Vec<Diagnostic>)` → printed, exit 1; `Unsafe { file, reason }` → internal error, exit 2, the file untouched; `Io { path, source }` → exit 2.
- A file that doesn't change isn't rewritten (its timestamps stay).

## SECURITY CONSIDERATIONS
- Reads and writes only the paths given (and `.la` files under given folders); never follows symbolic links out of a folder it expands; writes through a temporary file and a rename in the same folder, so a crash can't leave half a file.

## TESTS TO WRITE
- [ ] **Unit tests, one per rule:** spacing (operators, `,`, `:`, `=>`); indentation; brace placement; group breaking at 100 columns with trailing commas (calls, arrays, struct literals, parameters, imports); minimal parentheses (each precedence pair, associativity); literals as written (`0xFF`, `1_000`, `"a\n{b}"`, `'\u{1F600}'`, a `format` template); blank-line collapsing; match arms; closures; `if`/`else` chains; `impl`, `interface`, `enum` with payloads; leading, trailing and doc comments; a comment inside an expression → verbatim statement.
- [ ] **Verification tests:** a forced printer fault (a test-only hook) is caught as `Unsafe`; comment loss is caught.
- [ ] **Corpus test:** every parsing `.la` in `tests/programs/`, every spec §10 program, the std and prelude sources: format → verified → format again → identical.
- [ ] **`tests/fmt.rs`:** a file in place; a folder; `--check` lists and exits 1, writes nothing; an already formatted file is untouched (timestamp); a syntax error → diagnostics, exit 1, untouched; exit 0 otherwise.
- [ ] **Regression:** every existing test passes.

## ROLLBACK PLAN
- Branch `prp-110-formatter` from `main`; tag `m80`.
- To abandon before merge: delete the branch. After: revert the merge; nothing depends on it until milestone 81.

## ACCEPTANCE CRITERIA
- [ ] All tests pass; fmt, clippy, `cargo test` (CI in the background).
- [ ] Spec at milestone 80; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo test --test fmt
cargo run -q -- fmt --check tests/programs   # reports, changes nothing
```
