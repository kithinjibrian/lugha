## FEATURE: Go to Definition in `lughac lsp` — from variables, functions and methods, types, fields, variants and modules to where they are declared; milestone 72.

**Status:** approved 2026-10-15 ("build it") — built in session 149; proposed in session 148.
**Milestone:** 72 (one PRP; tag `m72` at merge). The third language-server PRP (PRP-100 diagnostics, PRP-101 hover).
**Spec:** `Language v16 Draft.md` decisions 23–27; spec §9 (the `lughac lsp` paragraph), §11 (row 72).

**Decisions (discovery, session 148):**
- jumps from names, types, fields, variants and modules;
- `std` and prelude definitions: nothing (no file to open);
- an interface call goes to the interface's method declaration;
- no Find References or Go to Implementations (later PRPs).
- No open entries in DECISIONS.md apply.

## OBJECTIVE
F12 or Ctrl+click in VS Code:

```
let c = Circle { r: 1.0 };      Circle → struct Circle      r → the field `r: f64`
println(c.area());              c → `let c`                 area → `fun area(self)` in `impl Circle`
show(c)  // fun show(s: Shape)  show → `fun show`           (in show) Shape → `interface Shape`
s.area()                        area → `fun area(self): f64;` inside `interface Shape`
Shape.Circle(2.0)               Circle → the variant `Circle(f64)`
import geo;  geo.double(2)      geo → geo.la                double → `fun double` in geo.la
math.sqrt(2.0)                  sqrt → nothing (bundled std)
```

The extension needs no change: `vscode-languageclient` asks once the server says it answers definitions.

## CONTEXT
- **Starting state:**
  - hover (PRP-101): `check_partial` returns `names`, mapping each function or method use to its declaration's name span (`Name::Function { decl, .. }`), and locals, fields and functions at their declarations; the server keeps a `Snapshot` per main file;
  - the checker knows declaration spans for locals (`Local.span`), structs and enums (`StructInfo.span`, `EnumInfo.span`), interfaces (`InterfaceInfo.span`), functions and methods (`Signature.span`, `Method.span`); **not** for fields or variants (`fields: Vec<(String, Type)>`, `variants: Vec<(String, Vec<Type>)>`);
  - type names resolve through `modules/lookup.rs::type_key`; local reads through `env.rs::local`; imports keep each path segment as an `Ident`.
- **Ending state:**
  - **Checker** (`check/partial.rs`): `Partial.definitions: HashMap<Span, Target>` — each written use → `Target::Decl(Span)` (a declaration's name, in the shared offset space) or `Target::Module(Vec<String>)` (a module path). Recorded at:
    - local reads and writes (an expression or place naming a local) → the binding;
    - every point hover's `Name::Function` records (declarations point at themselves) → the declaration;
    - interface method calls (`through_interface`) and bound calls → the interface's method;
    - type names in annotations, struct literals, `impl` headers, constructions and associated calls → the type's declaration (`type_key` and `resolve_named` callers);
    - field access `p.x`, a literal's field names, field declarations → the field (new: field spans kept beside `StructInfo.fields`);
    - variants in constructions and patterns → the variant (new: variant spans beside `EnumInfo.variants`);
    - import paths and module prefixes (`geo` in `geo.f`, `geo.T`) → `Target::Module`.
  - **Server:** `driver/lsp/definition.rs` (new): position → smallest use span containing it → the target's file and range (`Target::Decl` through the snapshot's `Source`, skipping `<std>`/`<prelude>`; `Target::Module` through the snapshot's modules → that file, range 0:0). `server.rs` answers through the same freshness rule as hover; `mod.rs` advertises `definitionProvider` and routes `textDocument/definition`.
  - **Tests:** `tests/lsp_definition.rs` (the shared client in `tests/common/lsp_client.rs` gains `definition()`); unit tests in `check/partial.rs` and `driver/lsp/definition.rs`.
  - **Docs:** spec v16 §9 (go-to-definition in the `lughac lsp` paragraph), §11 row 72, the summary; CLAUDE.md tree; README; the extension README; TODO; CHANGELOG; MEMORY; CONTEXT.
- **Related existing code:** `src/check/partial.rs`, `env.rs`, `expr.rs` (names), `assign.rs` (places), `modules/lookup.rs` (`type_key`, module prefixes), `generics.rs` (`resolve_named`), `structs.rs`, `struct_literal.rs`, `access.rs` (fields), `enums.rs`, `construct.rs`, `patterns.rs` (variants), `iface_calls.rs`, `satisfy.rs` (bound calls), `modules/resolve.rs` (imports); `src/driver/lsp/{hover,server,mod}.rs`.
- **Open decisions that must be resolved first:** none.

### Design
1. **One table.** `definitions` is separate from hover's `names`; recording a use is one call, `self.hover.define(use_span, target)`, at the resolution points above — where the checker already looked the name up, so no second resolution can disagree.
2. **Answer** at byte offset `o` of file `F` (same mapping as hover): the smallest use span containing `o` → its target.
   - `Target::Decl(span)` → the file holding `span` and its UTF-16 range; a span in `<std>` or `<prelude>` → `null`.
   - `Target::Module(path)` → the module of the snapshot with that path → its file, range 0:0 → `null` for a std module.
   - Nothing under the cursor → `null`.
3. **Freshness:** as hover — a pending check for `F` runs first; then the first planned target whose snapshot loaded `F` answers.
4. **Reply:** `GotoDefinitionResponse::Scalar(Location)`, or `null`.

### Risk
- **Medium-low:** many small recording points, no new rules; `check_modules`'s output unchanged. Field and variant spans are new data beside existing tables, not changes to them.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The design above, test-first.
- **Spec v16:** the `lughac lsp` paragraph gains go-to-definition (what jumps where, `null` cases); §11 row 72; the changes summary.
- **Docs:** CLAUDE.md (tree: `driver/lsp/definition.rs`, `tests/lsp_definition.rs`), README, the extension README (feature list, manual check), TODO (done; Find References and Go to Implementations as later items), CHANGELOG, MEMORY plan and built entries, CONTEXT; tag `m72`.

### Must NOT Do
- **No Find References, Go to Implementations, rename or document symbols** (later or not planned).
- **No virtual documents or written copies** of `std`/prelude sources (decision 25).
- **No change to `lughac check`, its output, codegen's input, or the checking rules.**
- **No extension code change** beyond its README.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **Never fails the server:** unknown document, position past the end, no check, a bundled target → `null`. Malformed params → `InvalidParams`.
- **A panic** during the check it triggers: caught as before; the answer is `null`.

## SECURITY CONSIDERATIONS
- Nothing new is read, run or written: answers come from results already computed. Module targets are files the check already loaded. Offline, no writes (PRP-100).

## TESTS TO WRITE
- [x] **`check::partial` unit tests:** `definitions` holds, for one program: a local read and an assignment → its `let`; a parameter use → the parameter; a function call and value → its declaration; a method call → the method; an interface call → the interface's method; a type in an annotation, a struct literal, an `impl` header → the struct; an enum in a construction → the enum, the variant → the variant; `p.x` and a literal's `x:` → the field; `import geo;` and `geo.f` → `Module(["geo"])`; declarations → themselves.
- [x] **`driver::lsp::definition` unit tests:** smallest span wins; a `<std>`/`<prelude>` target → `None`; a module target → its file at 0:0.
- [x] **`tests/lsp_definition.rs`** (real server over stdio):
  - `initialize` advertises `definitionProvider`;
  - each row of the objective's table jumps to the expected file and range (two files: `main.la`, `geo.la`);
  - `math.sqrt` → `null`; whitespace, a comment, a keyword → `null`; a file never opened → `null`;
  - with an unrelated error in the program → still jumps;
  - right after an unsaved change (no wait) → the new text's declaration.
- [x] **Regression:** every existing test passes; hover unchanged (`tests/lsp_hover.rs`).
- [ ] **Manual check** (extension README), by the user: Ctrl+click a variable, a method call, a type and an import in VS Code.

## BUILT (session 149)
- As designed: `Target` and `Partial.definitions` in `check/partial.rs`, with field and variant declaration spans kept in its `Recorder`; recorded at `expr.rs` (locals), the existing function hook, `modules/lookup.rs` (`type_key`, `type_named_by`, `module_call`, new `type_decl` and `module_named`), `iface_values.rs`, `methods.rs` (`impl` headers), `struct_literal.rs`, `access.rs`, `construct.rs`, `patterns.rs`, `structs.rs`/`enums.rs`/`interfaces.rs` declarations, and import paths in `modules/mod.rs::enter`. `driver/lsp/definition.rs`; the server's hover and definition share `fresh` and `snapshot`.
- Tests: 3 more in `check::partial` (9 in all), 1 in `driver::lsp::definition`, 8 in `tests/lsp_definition.rs`; hover's 10 and the server's 14 unchanged.
- **Differences from the plan:** none in behaviour. Assignments needed no hook of their own: their places are checked as expressions, so `total += …` already resolves through `expr.rs`.
- The manual check in VS Code is left to the user (extension README, step 6).

## ROLLBACK PLAN
- Branch `prp-102-lsp_definition` from `main`; tag `m72`.
- To abandon before merge: delete the branch. After: revert the merge; hover and diagnostics unchanged.

## ACCEPTANCE CRITERIA
- [x] All tests pass; fmt, clippy, `cargo test`, the extension's `npm test`; CI green.
- [x] Spec at milestone 72; docs updated.
- [x] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] The manual check passes in VS Code.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo test --test lsp_definition --test lsp_hover --test lsp
```
