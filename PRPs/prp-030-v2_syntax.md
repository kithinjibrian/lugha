## FEATURE: v2 syntax — lex and parse all of the v2 draft's grammar, with a driver gate for constructs not yet built.

**Status:** implemented 2026-10-12 — session 37
**Milestone:** 9 (first of three PRPs: 030 syntax, 031 deep `==` and string `<`, 032 nested patterns and guards)
**Spec:** `Language v2 Draft.md` §2 (keywords), §3 (grammar, E0208), §10 (example programs). v1.2 stays authoritative for everything the gate doesn't stop.
**Decisions:**
- MEMORY 33 (v2 direction);
- MEMORY 24 (the v1 gate approach: syntax first, gated before checking, exit 2, entries deleted as milestones land);
- architecture rule 9 (the parser covers the full syntax).

## OBJECTIVE
Every program in the v2 draft parses, and `--emit=ast` shows its new forms.
- **Gate:** `check`, `build` and `run` stop at the first v2 construct whose milestone hasn't landed. It prints `not implemented yet: <what> (milestone N)` and exits 2, as v1's gate did.
- **v1.2 programs:** every existing one behaves exactly as before, unless it uses `import` or `pub` as a name.

## CONTEXT

- **Starting state:**
  - The lexer and parser cover v1.2.
  - Patterns are flat: `PatSlot` is a name or `_`.
  - Types are `Named(String)` or `Option<T>`.
  - There are no imports, `pub`, type parameters or guards.
- **Ending state:**
  - The AST represents all of the draft's §3. The lexer has `import` and `pub`.
  - A new `driver/gate.rs` stops M9, M10 and M11 constructs before checking.
  - The checker and codegen see only v1.2 shapes, because the gate guarantees it. They keep `unreachable!` arms for gated nodes, as in v1.
- **Related existing code:**
  - `src/lexer/token.rs`;
  - `src/ast/{mod,pattern,expr}.rs`;
  - `src/parser/{item,decl,pattern,primary,stmt,sexp,describe}.rs`;
  - `src/driver/{mod,pipeline}.rs`, `tests/{parser,cli}.rs`.

  Prior art: the old v1 gate, deleted in bf528e9.
- **Open decisions that must be resolved first:** none.

### Amendments during implementation (session 37)
- **Boxing for stack depth:**
  - `ExprKind::StructLit` holds `Box<StructLiteral>`, and `TypeKind::Applied` holds `Box<AppliedType>`, instead of inline fields.
  - The inline version grew `ExprKind` from 72 to 128 bytes (casts hold a `Type`), and the 256-level E0206 test overflowed a 2 MiB test thread. It is now 64 bytes.
  - Member access in `postfix` and `struct_lit_after` are kept out of line (`#[inline(never)]`), like `colon_colon`.
- **Gate and `Box<L>`:** type arguments are gated only on the program's own structs and enums. An unknown `Box<L>` still reaches the checker for its E0305 help, which the checker now also gives for `Applied` names.
- **Splits approved mid-way** (file-size rule):
  - `parser/sexp.rs` (316 lines) became `parser/sexp/{mod,item}.rs`;
  - the new gate (327 lines) became `driver/gate/{mod,walk}.rs`.
- **New AST file:** `ast/header.rs` (`Import`, `TypeParam`) keeps `ast/mod.rs` under 300 lines.
- **Test order:**
  - the parser, gate and lexer unit tests were written before their code, and ran red first;
  - the lexer keyword test was added right after the keyword change;
  - the two integration tests (draft programs parse, CLI gate) were written after the code and passed first time.

### Discovery answers (session 37)
1. **Milestone 9 plan:** syntax first (this PRP, with a gate), then deep `==` and string `<` (PRP-031), then nested patterns and guards (PRP-032). This follows v1's approach and architecture rule 9.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Lexer**
- `import` and `pub` become keywords.

**AST.** Only additions and generalizations; existing node shapes keep their meaning.
- **Imports:** `Program.imports: Vec<Import>`, where `Import { path: Vec<Ident>, span }` holds `a.b.c` as segments.
- **Visibility:** `public: bool` on `FunDecl` (for both functions and methods), `ExternDecl`, `StructDecl` and `EnumDecl`, along with the `pub` keyword's span.
- **Type parameters:** `type_params: Vec<TypeParam>` on `FunDecl`, `StructDecl` and `EnumDecl`, with `TypeParam { name: Ident, bounds: Vec<Ident> }`. Bounds are names; the checker resolves them (E0315, in M11).
- **`impl` headers:** `ImplDecl` gains `module: Option<Ident>` and `params: Vec<Ident>`, for `impl geometry.Vec2` and `impl Stack<T>`.
- **Types:** a new `TypeKind::Applied { module: Option<Ident>, name: Ident, args: Vec<Type> }` for a module-qualified name or a name with type arguments. Plain names stay `Named`, and `Option<T>` stays `TypeKind::Option` (M11 unifies them).
- **Struct literals:** `ExprKind::StructLit` gains `module: Option<Ident>`, for `geometry.Vec2 { … }`.
- **Patterns:**
  - a payload slot is now a full `Pattern`, so `PatSlot` is removed;
  - a bare name in a slot is `PatternKind::Variant(name, None)`, which the checker treats as a binding in v1.2, as today;
  - `Arm` gains `guard: Option<Expr>`.
- **Docs:** every new node and field has a `///` doc citing the draft section.

**Parser** (draft §3)
- **Imports:**
  - `import a.b;` is read only before the first item;
  - an `import` after an item is **E0208**, "`import` must come before other items", labelled at the first item; parsing continues;
  - a bad import path is E0201.
- **`pub`:**
  - accepted before `fun`, `extern`, `struct` and `enum` at the top level, and before `fun` inside an `impl`;
  - `pub impl`, `pub let` and a `pub` before anything else is E0201.
- **Type parameters:**
  - `<T, U: Eq + Ord>` after a function, struct, enum or method name, with a trailing comma allowed;
  - an empty `<>` is E0201;
  - `impl Name<T, U>` and `impl m.Name` headers.
- **Types:** `m.T`, `T<args>` and `m.T<args>` in every type position; nested arguments `Stack<Option<i64>>` need no special lexing, since there is no `>>` token.
- **Struct literals:** `m.T { … }` follows the same no-struct-literal rule as `T { … }` (spec §3).
- **Patterns:** nested patterns in payload slots (`Some(Circle(r))`, `Pair(1, _)`, `Some("x")`), and an optional `if expr` guard before `=>`, parsed as a `cond`.
- **Errors:** recovery and `describe` messages are extended for the new tokens; there are no new codes besides E0208.
- **`--emit=ast` (sexp):** new forms render readably, e.g. `(import a b)`, `(pub fun …)`, `(type-params (T Eq Ord))`, `(applied geometry Vec2 (i64))`, `(guard …)` and nested `(variant …)`. Existing v1 output is unchanged.
- **New files** where needed to stay under 300 lines: `parser/generic.rs` (type parameters and type arguments) and `parser/import.rs` (imports, `pub`). `parser/stmt.rs` (292 lines) must not grow.

**Gate** — new `driver/gate.rs`, run in `pipeline::front` between parse and check, as in v1. `--emit=tokens|ast` is not gated.

| Construct | Message `what` | Milestone |
| --- | --- | --- |
| a nested pattern in a payload slot (anything but a name or `_`) | `nested patterns` | 9 |
| a guard | `match guards` | 9 |
| `import` | `modules` | 10 |
| `pub` | `pub` | 10 |
| a module-qualified type, struct literal or `impl` | `modules` | 10 |
| type parameters, an `impl` with parameters, a type with arguments other than `Option<T>` | `generics` | 11 |

- **First construct only:** the gate reports the first gated construct in source order, as one internal error (`Report::internal`, span included, exit 2).
- **No gate entry for `==`:** `==` on structs, enums and arrays needs no new syntax. Until PRP-031 it stays E0404 from the checker, as in v1.2.

**Docs**
- CLAUDE.md:
  - the tree (`driver/gate.rs`, the new parser files, `ast/pattern.rs` description);
  - a KNOWN ISSUES line: "v2 constructs stop with 'not implemented yet (milestone N)', exit 2, until their milestone".
- TODO (milestone 9 PRPs), CHANGELOG, MEMORY (a decision entry), CONTEXT.

### Must NOT Do
- No checker or codegen support for any v2 construct, and no spec change. v1.2 stays what `lughac spec` prints.
- No change to v1.2 programs' behaviour beyond the two new keywords.
- No `unsafe`, no new dependencies.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- **Parse errors:** they use `Diagnostic` codes and recover to the next item or statement, as today.
- **Gate:** its stop is an internal error (exit 2), not a user-program error. The program may be correct v2; the compiler just can't build it yet. This is identical to v1's gate.

## SECURITY CONSIDERATIONS

- **Untrusted input:** parsing grows, but the existing nesting limit (E0206) applies to nested patterns and type arguments too, so deep nesting can't overflow the stack.
- **No new I/O:** imports are only parsed here; no file is opened until M10.
- **No `unsafe`.**

## TESTS TO WRITE

Lexer:
- [x] `import` and `pub` are keywords; `important` and `pubs` are identifiers.

Parser unit tests (sexp):
- [x] **Imports:**
  - `import a;` and `import a.b.c;`;
  - E0208 for an import after a function, with parsing continuing;
  - E0201 for `import ;` and `import a.;`.
- [x] **`pub`:**
  - on `fun`, `extern`, `struct`, `enum` and an `impl` method;
  - E0201 for `pub impl` and for `pub let` in a block.
- [x] **Type parameters:**
  - `fun max<T: Ord>(…)`, `struct Pair<A, B>`, `enum Tree<T: Eq>`, `<T: Eq + Ord,>`;
  - `impl Stack<T>`, `impl geometry.Vec2`;
  - E0201 for `<>`.
- [x] **Types:** `Stack<i64>`, `Stack<Option<i64>>[]`, `geometry.Vec2`, `m.Pair<i64, string>` in parameters, returns, fields, `let` annotations and `as`.
- [x] **Struct literals:** `geometry.Vec2 { x: 1.0 }`; inside an `if` condition it needs parentheses (E0205), as for plain names.
- [x] **Patterns:**
  - `Some(Circle(r))`, `Rect(_, Some(1))`, `Some("x")`, `Some(-2)`;
  - a guard `Circle(r) if r > 1.0 =>`;
  - the nesting limit applies to deep nested patterns.
- [x] **Unchanged output:** every existing parser test output stays the same.

Integration:
- [x] **`tests/parser.rs`:** every §10 program in `Language v2 Draft.md` parses with no diagnostics, the two-module example as two files. The draft is read from disk by the test; it is not embedded in lughac.
- [x] **`tests/cli.rs`:**
  - `--emit=ast` on a generic program succeeds (exit 0) and shows `(type-params …)`;
  - `check` on it exits 2 with `not implemented yet: generics (milestone 11)`, in both human and JSON format.
- [x] **Gate:** a unit test per row in `driver/gate.rs`, plus "first in source order" and "v1.2 programs pass through ungated".
- [x] **Regression:** the whole existing suite passes (252), including every spec program and the `-O0`/`-O2` cross-check.

## ROLLBACK PLAN

- Branch `prp-030-v2_syntax`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] All of the v2 draft's grammar parses; the draft's examples parse; `--emit=ast` shows them.
- [x] Gated constructs exit 2 with the milestone message; every v1.2 program behaves as before.
- [x] CLAUDE.md, TODO.md, CHANGELOG.md, MEMORY.md updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `lughac build --emit=ast` on the draft's generics program → exit 0; `lughac check` on it → exit 2, milestone 11
