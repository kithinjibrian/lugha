## FEATURE: Hover in `lughac lsp` — the type of the expression or declared name under the cursor, also while the program has errors; milestone 71.

**Status:** approved 2026-10-15 ("build it") — built in session 147; proposed in session 146.
**Milestone:** 71 (one PRP; tag `m71` at merge). The second language-server PRP (PRP-100 was live diagnostics; go-to-definition follows).
**Spec:** `Language v16 Draft.md` decisions 18–22; spec §9 (the `lughac lsp` paragraph), §11 (row 71).

**Decisions (discovery, session 146):**
- hover shows expressions' types and declared names (bindings, parameters, functions and methods, struct fields);
- it works while the program has errors, from the checker's partial results;
- the text is a Lugha code block plus short notes (`instance: max<f64>`, `converted to Shape`).
- No open entries in DECISIONS.md apply.

## OBJECTIVE
Hover over something in a `.la` file and VS Code shows what it is:

```
let total = sum(xs);        hover `total`   →  let total: i64
s.area()                    hover `area`    →  fun Circle.area(self): f64
max(a, b)                   hover the call  →  f64            instance: max<f64>
show(c)   // show(s: Shape) hover `c`       →  Circle         converted to Shape
```

It still works with an error elsewhere in the program. The extension needs no change: `vscode-languageclient` asks for hovers once the server says it answers them.

## CONTEXT
- **Starting state:**
  - `lughac lsp` (PRP-100) publishes diagnostics from `pipeline::front_with`; it keeps nothing from a check but its diagnostics and the files it loaded;
  - the checker records every expression's type (`types`, by `ExprId`), every written type (`annotations`, by span), calls' callees, generic instances and interface conversions — but `check_modules` returns only diagnostics when there is an error, though it carries on after errors and has most types (`check/modules/mod.rs`);
  - every local name is declared through `env.rs`'s `declare(name: &Ident, ty, binding)`; functions have a `Signature` (`check/mod.rs`);
  - `tests/lsp.rs` is 484 lines.
- **Ending state:**
  - **Checker:**
    - `check/partial.rs` (new): `pub fn check_partial(units) -> Partial` — the same checking as `check_modules`, always returning what it found: `types: Vec<Option<Type>>`, `spans: Vec<Option<Span>>` (each checked expression's span), `annotations`, `instances`, `conversions`, `names: HashMap<Span, Name>`, `diagnostics`. `check_modules` is unchanged in behaviour and output.
    - `Name` (new, in `partial.rs`): `Local { name, ty, mutable }`, `Function { text }` (the signature as written: `fun max<T: Ord>(a: T, b: T): T`, `fun Circle.area(self): f64`), `Field { name, ty }`.
    - recorded at: `env.rs::declare` (locals of every kind), function and method declarations (the name's span), calls and method calls (the callee name's span), struct field declarations;
  - **Server:** `driver/lsp/hover.rs` (new): position → byte offset → the smallest name or expression span containing it → the text (design 3). `server.rs` keeps, per main file, its last check's `Partial` and `Source`; `check.rs` runs `check_partial` instead of `check_modules` (diagnostics identical); `mod.rs` advertises `hoverProvider` and answers `textDocument/hover`.
  - **Pipeline:** `pipeline::front_partial` (new) — the front end through `check_partial`, for the server only.
  - **Tests:** the stdio harness moves from `tests/lsp.rs` into `tests/common/lsp_client.rs` (used by both), and hover's tests go in `tests/lsp_hover.rs` — the split keeps `tests/lsp.rs` under 500 lines (approved with this PRP).
  - **Docs:** spec v16 §9 (hover in the `lughac lsp` paragraph), §11 row 71, the changes summary; CLAUDE.md tree; README; extension README; TODO; CHANGELOG; MEMORY; CONTEXT.
- **Related existing code:** `src/check/modules/mod.rs`, `checked.rs`, `env.rs`, `mod.rs` (`Signature`, `Type`'s `Display`), `method_calls.rs`, `structs.rs`; `src/driver/lsp/*`, `driver/pipeline.rs`; `tests/lsp.rs`.
- **Open decisions that must be resolved first:** none.

### Design
1. **Partial results.** `check_partial` runs the checker exactly as `check_modules` does and stops before building codegen's tables. An expression the checker never typed, or typed `Error`, has no hover. A module that didn't parse has no expressions, so no hover. The diagnostics are the same list `check_modules` returns, so publishing is unchanged.
2. **Freshness.** A hover for file `F` first runs any pending check due for `F` (ignoring the 200 ms wait), then answers from the result of the first target that loaded `F` (`F` itself, an entry or a seen main — PRP-100's planning).
3. **What answers** at byte offset `o` of `F`, after mapping `F`'s offset into the check's shared offset space:
   - the **smallest** span containing `o` among `names` and checked expressions; a name wins a tie;
   - a `Local` → `let total: i64` (`let mut n: i64` when mutable; parameters and bindings use the same form);
   - a `Function` → its signature text; with generic arguments known at that use, a note `instance: max<f64>`;
   - a `Field` → `x: f64`;
   - an expression → its type; notes `instance: …` for a generic call or construction (from `instances`), `converted to Shape` for a converted value (from `conversions`);
   - nothing (whitespace, comments, keywords, a broken expression) → `null`.
4. **The reply:** `Hover { contents: MarkupContent { kind: markdown, value: "```lugha\n<line>\n```" + notes } , range: the span in UTF-16 }`. Types print exactly as diagnostics print them (`Type`'s `Display`).
5. **Cost.** One more table per check; the server keeps one `Partial` per main file. No re-check on hover unless one is pending.

### Risk
- **Medium-low:** the checker gains recording points but no new rules; `check_modules`'s output stays byte-identical (all existing tests). The partial path is used only by the server.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The design above, test-first.
- **Spec v16:** the `lughac lsp` paragraph gains hover (what it shows, with errors, `null`); §11 row 71; the changes summary.
- **Docs:** CLAUDE.md (tree: `check/partial.rs`, `driver/lsp/hover.rs`, `tests/lsp_hover.rs`, `tests/common/lsp_client.rs`), README, the extension README (hover in its feature list and manual check), TODO (hover done; go-to-definition next), CHANGELOG, MEMORY plan and built entries, CONTEXT; tag `m71`.

### Must NOT Do
- **No change to `lughac check`, its output, or codegen's input** (`Checked`).
- **No new diagnostics or checking rules.**
- **No go-to-definition, completion or signature help** (later PRPs).
- **No comments as documentation** in hovers (not chosen).
- **No extension change** beyond its README.

### New dependencies
None (`lsp-types` already has `Hover`).

## ERROR HANDLING REQUIREMENTS
- **A hover never fails the server:** an unknown document, a position past the end, a file no check has loaded → `null` result, not an error. A malformed `hover` request → `InvalidParams`.
- **A panic** inside the checker during the check a hover triggers is caught as in PRP-100 (one internal-error diagnostic); the hover answers `null`.

## SECURITY CONSIDERATIONS
- Nothing new is read, run or written; hover only reads results the server already computed from the same inputs. Offline and no writes, as PRP-100.

## TESTS TO WRITE
- [ ] **`check::partial` unit tests:** a valid program gives the same types as `check_modules`; a program with an error still types its other expressions and lists the same diagnostics; `names` holds a `let`, a `let mut`, a parameter, a `for` and a `match` binding, a function and a method declaration and call, a generic function's signature, and a struct field.
- [ ] **`driver::lsp::hover` unit tests:** smallest span wins, names win ties; each text form (local, mutable local, function, method, field, expression, `instance:`, `converted to`); UTF-16 position → byte offset after `é`/emoji; nothing → `None`.
- [ ] **`tests/lsp_hover.rs`** (real server over stdio):
  - `initialize` advertises `hoverProvider`;
  - hover on `let total` → `let total: i64`; on a parameter; on `fun area` and on a call of it; on `p.area()`'s `area`; on a struct field; on a generic call (`instance: max<f64>`); on a value converted to an interface;
  - with an unrelated type error in the program → still answers; on the broken expression → `null`;
  - in a module checked through a seen main;
  - right after an unsaved change (no wait) → the new text's types;
  - on whitespace, a comment, a keyword → `null`; in a file never opened → `null`.
- [ ] **Regression:** `tests/lsp.rs` unchanged in behaviour after the harness move; every existing test passes (`check_modules` byte-identical).
- [ ] **Manual check** (extension README), by the user: hovers in VS Code on a variable, a method call and a generic call; still working with an error in the file.

## ROLLBACK PLAN
- Branch `prp-101-lsp_hover` from `main`; tag `m71`.
- To abandon before merge: delete the branch. After: revert the merge; the server stops advertising hover, diagnostics unchanged.

## ACCEPTANCE CRITERIA
- [ ] All tests pass; fmt, clippy, `cargo test`, the extension's `npm test`; CI green.
- [ ] Spec at milestone 71; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] The manual check passes in VS Code.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo test --test lsp_hover --test lsp
```
