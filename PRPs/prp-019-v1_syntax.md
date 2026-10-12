## FEATURE: v1 syntax — the lexer, parser and AST for all of v1 (`enum`, `match`, `impl`, `self`, `Option<T>`, `=>`, and a helpful E0207 for `::`), with a driver gate that reports the not-yet-implemented constructs.

**Status:** approved 2026-10-12 — session 25
**Milestone:** 6, first of three PRPs (019 syntax, 020 enums/`match`/`Option`, 021 recursive enums and copies)
**Spec:** `docs/specs/Language v1 Draft.md` §2 (keywords, `=>`, `::`), §3 (grammar: `enum_decl`, `impl_decl`, `method`, `receiver`, `Option<T>`, `match_expr`, patterns, `self`)
**Decisions:** MEMORY 23 (v1 direction), architecture rule 9 (the lexer and parser cover the full syntax at once), PRP-016 (no `Stop`/`Checking` machinery)

## OBJECTIVE
Every v1 program lexes and parses, and `--emit=tokens|ast` shows it. Code that writes `Shape::Circle` gets E0207 with help to write `Shape.Circle`, and parsing continues.

Until the PRPs that implement them land, `lughac check`, `build` and `run` stop at the first v1 construct with an internal error (exit 2) naming its milestone:
- `not implemented yet: enums (milestone 6)`;
- `not implemented yet: methods (milestone 7)`.

v0 programs behave exactly as before, unless they use the four new keywords as names.

## CONTEXT

- **Starting state:** the lexer, parser and AST cover v0 §2–§3. There is no milestone-stop mechanism (removed in PRP-016).
- **Ending state:**
  - new tokens and keywords;
  - new AST items, expressions, patterns and types;
  - an S-expression form for each;
  - E0207;
  - a new `driver/gate.rs`.
- **Related existing code:**
  - `src/lexer/{mod,token}.rs`;
  - `src/ast/{mod,expr}.rs`;
  - `src/parser/{mod,item,stmt,expr,primary,sexp,describe,recover}.rs`;
  - `src/driver/pipeline.rs`;
  - `tests/{lexer,parser,cli}.rs`.
- **Open decisions that must be resolved first:** none.

### Discovery answers (session 25)
1. **Spec file:** milestone 6 creates `Language v1 Specification.md` (v0 plus folded-in sections), and `lugha::SPEC` points at it. The v0 file is frozen. This happens in PRP-020, when the first v1 feature works; PRP-019 changes no spec text.
2. **Split:** three PRPs, as above.
3. **Interim gate:** a driver pass reports the first unimplemented v1 construct as an internal error (exit 2), so the checker and codegen never see one. PRP-020/021 shrink the gate, M7 empties it, and it is deleted when v1 is complete.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Lexer** (draft §2)
- Keywords `enum`, `match`, `impl`, `self`.
- Tokens:
  - `=>` (`FatArrow`). `=` followed by `>` lexes as one token, by longest match.
  - `::` (`ColonColon`), used only by the parser to report E0207.
- `Option`, `Some` and `None` stay identifiers.
- **Proposed split** (`lexer/mod.rs` is at 299 lines): move `punctuation()` and its operator tests into a new `lexer/punct.rs`.

**AST** — `ast/mod.rs`, `ast/expr.rs`, new `ast/pattern.rs`
- **Items:**
  - `Item::Enum(EnumDecl { name, variants: Vec<Variant { name, payloads: Vec<Type>, span }>, span })`.
  - `Item::Impl(ImplDecl { ty: Ident, methods: Vec<FunDecl>, span })`.
- **Receivers:** `FunDecl` gains `receiver: Option<Receiver { mutable, span }>`, which is always `None` outside `impl`.
- **Types:** `TypeKind::Option(Box<Type>)`.
- **Expressions:**
  - `ExprKind::Match(Box<Expr>, Vec<Arm>)`, where `Arm { pattern: Pattern, body: Expr, span }`.
  - `self` is `ExprKind::Name("self")`: the checker will bind it like a parameter (M7), and the parser accepts it only as a primary.
- **Patterns:** in `ast/pattern.rs`:
  - `Pattern { kind, span }`;
  - `PatternKind::{Wildcard, Variant(Ident, Option<Vec<PatSlot>>), Int(u64, negative: bool), Str(String), Bool(bool)}`;
  - `PatSlot::{Bind(Ident), Ignore(Span)}`.

**Parser** (draft §3) — new `parser/decl.rs` (enum and impl declarations) and `parser/pattern.rs` (`match` and patterns), keeping `item.rs`, `stmt.rs` and `expr.rs` under 300 lines
- **`enum_decl`:** variants with optional payload type lists; trailing commas allowed.
- **`impl_decl`:** `impl IDENT { method* }`.
  - **Receivers:** a method's first parameter may be `self` or `mut self`.
  - **Outside `impl`:** `self` as a parameter is E0201, with help "`self` is only allowed in methods".
- **`Option<T>`** in type position: `Option` followed by `<`, a type, `>`. Elsewhere `Option` is an ordinary identifier.
- **`match_expr`:**
  - The scrutinee is a `cond`, in no-struct-literal mode.
  - Arms are `pattern => expr`, separated by `,`. The comma is optional after an arm whose body is a block; a trailing `,` is allowed.
  - `match` is `block_like`, in statements and as an expression, exactly like `if`.
- **Patterns:** `_`, `IDENT`, `IDENT(slots)`, `[-]INT`, `STRING`, `true`, `false`. Anything else is E0201 "expected a pattern".
- **`::`:**
  - Wherever it appears, the parser reports **E0207** "`::` is not Lugha syntax", with help "write `Shape.Circle`" (using the actual names when the tokens around it are identifiers).
  - It then continues as if `.` had been written, so `Shape::Circle(1.0)` yields the same AST as `Shape.Circle(1.0)` plus one error.
- **Nesting:** the depth limit (E0206) also covers patterns and `match` nesting.

**S-expressions** — `parser/sexp.rs`:
- `(enum Shape (Circle f64) (Rect f64 f64) Empty)`;
- `(impl Point (fun dist (self (q Point)) f64 …))`, with `(mut self)` for a mutable receiver;
- `(match s (=> (Circle r) …) (=> _ …))`;
- `(Option i64)`;
- `self` prints as `self`.

**Driver gate** — new `driver/gate.rs`, called in `pipeline::front` after parsing and before checking
- **What it finds:** the first v1 construct in source order:
  - an `enum` item, a `match`, `Option<…>` in any type, or a variant pattern → **enums (milestone 6)**;
  - an `impl` item, or a `self` expression or receiver → **methods (milestone 7)**.
- **How it reports:** an internal error `not implemented yet: <what> (milestone <n>)` with the construct's span, exit 2, the same rendering as other internal errors (`code: null` in JSON).
- **What it doesn't affect:** `--emit=tokens|ast` stops before the gate, so it works on all v1 syntax now.
- **Later PRPs** delete entries from the gate as they implement them.

**Docs**
- CLAUDE.md tree:
  - `lexer/punct.rs`, `ast/pattern.rs`, `parser/{decl,pattern}.rs`, `driver/gate.rs`;
  - the known-issues list gains "v1 constructs stop with 'not implemented yet' until their milestone".
- No spec text changes; PRP-020 creates the v1 spec file.
- CHANGELOG, TODO, MEMORY, CONTEXT.

### Must NOT Do
- No checker or codegen support for any v1 construct (PRP-020/021, M7, M8).
- No spec file changes in this PRP.
- No reintroduction of `Stop`/`Checking` or `CheckError::Unsupported`.
- No new dependencies, no `unsafe`.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- E0207 is a parse error (exit 1) that recovers. Every other malformed v1 construct uses the existing E0201–E0206 with recovery at item or statement boundaries, so several errors are reported per run.
- The gate's internal error is exit 2 with a span. It runs only on programs without parse errors.

## SECURITY CONSIDERATIONS

- Parsing only; the depth limit (E0206) guards the new recursive constructs (patterns, `match`).
- No `unsafe`.

## TESTS TO WRITE

Unit tests:
- [ ] Lexer:
  - the four keywords;
  - `=>` and `::` by longest match;
  - `= >` with a space is two tokens;
  - `Option`/`Some`/`None` are identifiers.
- [ ] Parser, via S-expressions:
  - enums (payloads, payload-less variants, trailing commas);
  - `impl` with `self`, `mut self` and associated functions;
  - `Option<Option<i64>>` and `Option<i64>[]`;
  - `match` as an expression and as a statement, with block arms without commas, every pattern kind, and a negative integer pattern;
  - `self` in expressions.
- [ ] Parser errors:
  - `Shape::Circle(1.0)` → E0207 with help, and the AST equals the `.` form;
  - `self` as a parameter outside `impl` → E0201 with help;
  - a bad pattern → E0201;
  - `match` arms missing `=>` recover and report later errors too.
- [ ] Gate:
  - each construct maps to its milestone;
  - the first one in source order wins;
  - a v0 program passes the gate untouched.

Integration and acceptance:
- [ ] `tests/parser.rs`: the four v1 draft example programs and the rejected one parse without diagnostics. They are inline fixtures until PRP-020 moves them into the spec.
- [ ] `tests/cli.rs`:
  - `lughac check` on an enum program exits 2 with "not implemented yet: enums (milestone 6)", in human and JSON (`code: null`);
  - `--emit=ast` on it exits 0.
- [ ] All earlier tests pass.

## ROLLBACK PLAN

- Branch `prp-019-v1_syntax`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] Every v1 draft program parses; `--emit=ast` prints it.
- [ ] `::` gives E0207 with recovery.
- [ ] v1 constructs stop with the gate's internal error and their milestone.
- [ ] CLAUDE.md, CHANGELOG.md, TODO.md, MEMORY.md updated.
- [ ] No file over 300 lines (with the splits above); no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo run -q -- build --emit=ast <shapes example>` prints the S-expression; `cargo run -q -- check <shapes example>` exits 2 with the gate message
