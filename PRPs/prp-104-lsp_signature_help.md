## FEATURE: Signature help in `lughac lsp` — the signature of the call being typed, with the current parameter marked; milestone 74.

**Status:** approved 2026-10-15 ("build it") — built in session 155; proposed in session 154.
**Milestone:** 74 (one PRP; tag `m74` at merge). The fifth language-server PRP (diagnostics, hover, go-to-definition, completion).
**Spec:** `Language v16 Draft.md` decisions 33–37; spec §5 (the intrinsics table), §9 (the `lughac lsp` paragraph), §11 (row 74).

**Decisions (discovery, session 154):**
- every callable: functions, module functions, methods, associated functions, variants with payloads, built-in methods and prelude helpers, intrinsics;
- a generic function with its type parameters filled in when the last good check knows the instance at that call, as written otherwise.
- No open entries in DECISIONS.md apply.

## OBJECTIVE
While typing a call's arguments in VS Code, a popup shows its signature with the current parameter highlighted:

```
max(1.5, |            fun max(a: f64, b: f64): f64        (b highlighted; note: fun max<T: Ord>(a: T, b: T): T)
p.scale(|             fun Point.scale(self, k: f64): Point (k highlighted)
geo.double(|          fun double(n: i64): i64
Shape.Circle(|        Circle(f64)
xs.push(|             push(i64)
read_file(|           read_file(path)                       (from the spec's intrinsics table: argument `string`)
```

It appears on `(`, moves on `,`, and goes away outside the call. The extension needs no change.

## CONTEXT
- **Starting state:**
  - completion (PRP-103) resolves a receiver before `.` from the last check that parsed: an expression ending there (when inside the shared prefix), or by name among locals, imports and types; `check::value_members`/`type_members` list methods (with declarations), associated functions, variants (payload text), built-ins (type-only signatures) and helpers (prelude declarations);
  - hover renders a declaration's header as written (`hover::declaration`); `Partial.names` maps a call's callee name to `Name::Function { decl, call }` and `Partial.instances` holds a generic call's type arguments;
  - `Partial` has no resolved parameter types for functions; intrinsics have no declarations (`check::intrinsics()` lists the names; spec §5 has their call forms and argument types).
- **Ending state:**
  - **Checker:** `Partial.signatures: HashMap<Span, Signature>` — for each function and method, by its declaration's name span: its generics' names, its parameters' names and resolved types, its return type, its receiver. Built at the end of `check_partial` from `functions` and `methods` (parameter names from the declarations).
  - **Server:** `driver/lsp/signature.rs` (new):
    - **call context** from the current text: back from the cursor to the unclosed `(` (skipping nested brackets and strings, over lines), the commas at that depth → the active parameter, the callee text before the `(` (`name`, `x.name`, `m.name`, `T.name`);
    - **resolution** as completion: the callee name's `Name::Function` in the last good check when the name is inside the shared prefix (that gives the call's id, hence the instance); else by name — a function of this module, an import's function, a method or built-in of a receiver resolved as completion does, a type's associated function or variant; else an intrinsic;
    - **text:** a declared function's header as written, or, when the call's instance is known and the function is generic, `fun name(a: f64, b: f64): f64` built from `signatures` with the instance substituted, plus the written header as documentation; a built-in's or variant's member text; an intrinsic's call form from a table `INTRINSIC_SIGNATURES` (`println(x)`, `read_file(path)`, …);
    - **parameters** as label offsets into the text, so VS Code highlights the active one; a method's `self` is not a parameter the caller writes.
  - `mod.rs` advertises `signatureHelpProvider { triggerCharacters: ["(", ","], retriggerCharacters: [","] }` and routes `textDocument/signatureHelp`; `server.rs` answers through the same freshness rule and `last_good` as completion.
  - **Tests:** `tests/lsp_signature.rs` (the shared client gains `signature_help()`); unit tests in `driver/lsp/signature.rs` (call context: nesting, strings, lines, commas) and `check/partial.rs` (signatures).
  - **Docs:** spec v16 §9 (signature help in the `lughac lsp` paragraph), §11 row 74, the summary; CLAUDE.md tree; README; the extension README; TODO; CHANGELOG; MEMORY; CONTEXT.
- **Related existing code:** `src/check/{partial,members,env,methods}.rs` (`Signature`, `Method`), `src/driver/lsp/{completion,hover,server,mod}.rs`, `docs/specs/Language v16 Specification.md` §5 (intrinsics table).
- **Open decisions that must be resolved first:** none.

### Design
1. **Call context.** Scan back from the cursor: track depth over `()`, `[]`, `{}`; skip string and `char` literals and `//` comments; stop at the first `(` at depth 0 (or give up at the start of the enclosing function or after 2,000 bytes). Commas at depth 0 since that `(` → the active parameter. The callee is the identifier before the `(` (with an optional `x.`, `m.` or `T.` before it). A `(` with no identifier before it (a grouping) → nothing.
2. **Resolution order:** (a) the last good check's `names` at the callee name's span, when that span is inside the shared prefix → the declaration and the call id; (b) by name: a receiver as completion resolves it (`value_members`, `type_members`, module items); a function of this module; an intrinsic. Locals that hold function values (`let f = twice; f(`) resolve through their `fun(…)` type: types only.
3. **Filled-in generics:** only when (a) found the call and `instances[call]` has as many types as the function's own type parameters; `self` parameters and an `impl`'s parameters are filled from the receiver's type when known, else left as written.
4. **Intrinsic signatures:** one table, `INTRINSIC_SIGNATURES: [(&str, &str); N]` (`("println", "println(x)")`, …), whose parameter names and order are the spec §5 table's; a test checks its names equal `check::intrinsics()` exactly, and that each call form appears in the embedded spec (`lugha::SPEC`), so neither can drift.
5. **Reply:** `SignatureHelp { signatures: [SignatureInformation { label, documentation, parameters: [ParameterInformation { label: LabelOffsets }] }], active_signature: 0, active_parameter }`; `null` when nothing is found.

### Risk
- **Low-medium:** server-side code over existing results, plus one read-only table in `Partial`. The scanner is the new logic; it is unit-tested on nesting, strings, comments and lines.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The design above, test-first.
- **Spec v16:** the `lughac lsp` paragraph gains signature help; §11 row 74; the changes summary.
- **Docs:** CLAUDE.md (tree: `driver/lsp/signature.rs`, `tests/lsp_signature.rs`), README, the extension README (feature list, manual check), TODO, CHANGELOG, MEMORY plan and built entries, CONTEXT; tag `m74`.

### Must NOT Do
- **No overloads** (Lugha has none) — always one signature.
- **No comments as documentation**; the only documentation is the written header under a filled-in generic.
- **No parser or checker rule changes; no change to `lughac check`'s output or codegen.**
- **No extension code change.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **Never fails the server:** not in a call, an unknown callee, no snapshot, a string or comment → `null`. Malformed params → `InvalidParams`.
- **A panic** during the check it triggers: caught as before; the answer is `null`.

## SECURITY CONSIDERATIONS
- Nothing new is read, run or written. The scanner reads only the open document's text and is bounded (design 1). Offline, no writes (PRP-100).

## TESTS TO WRITE
- [ ] **`check::partial` unit tests:** `signatures` for a function (names, types, return), a generic function (its generics), a method (receiver, the `impl`'s and its own generics).
- [ ] **`driver::lsp::signature` unit tests:** call context for `max(1.5, |`, `f(a, g(b, |`, `f([1, 2], |`, `f("a,b", |`, a call over two lines, a comment after `(`, a grouping `(a + |` → none, outside any call → none; the intrinsic table matches `check::intrinsics()` and the spec.
- [ ] **`tests/lsp_signature.rs`** (real server over stdio, the text mid-typing so it doesn't parse):
  - `initialize` advertises `signatureHelpProvider` with `(` and `,`;
  - `twice(|` → `fun twice(n: i64): i64`, active parameter 0;
  - `max(1.5, |` in a call the last good check had → the filled-in text, the written header as documentation, active parameter 1; on a new line (not in the last good check) → the written header;
  - `p.scale(|` → the method's header, parameter `k` active; `geo.double(|`; `Shape.Circle(|` → `Circle(f64)`; `xs.push(|` → `push(i64)`; `read_file(|` → `read_file(path)`;
  - `(a + |` and plain code → `null`; a file never opened → `null`.
- [ ] **Regression:** completion, hover and definitions unchanged; every existing test passes.
- [ ] **Manual check** (extension README), by the user: a function, a method, a generic call and `println(` in VS Code.

## ROLLBACK PLAN
- Branch `prp-104-lsp_signature_help` from `main`; tag `m74`.
- To abandon before merge: delete the branch. After: revert the merge; the server stops advertising signature help.

## ACCEPTANCE CRITERIA
- [ ] All tests pass; fmt, clippy, `cargo test`, the extension's `npm test` (CI in the background, not waited on).
- [ ] Spec at milestone 74; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] The manual check passes in VS Code.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo test --test lsp_signature --test lsp_completion
```
