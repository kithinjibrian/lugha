## FEATURE: A parser that turns lexer tokens into an AST for the full §3 grammar, reporting several syntax errors per run with stable codes.

**Status:** implemented 2026-10-09 — session 6 (branch `prp-003-parser`)
**Milestone:** 1 (second pipeline stage; full syntax per CLAUDE.md rule 9)
**Spec:** §3 (grammar, precedence, assignment, struct literals in conditions, block-like statements), §6 (expression bodies), §9 (error codes)
**Decisions:** DECISION-002 (stage return type)

## OBJECTIVE
`lugha::parser::parse(&tokens)` returns a `Program` AST for any syntactically valid `.la` file. For invalid input it reports every syntax error it can find, skipping ahead to a safe point after each one, as `Diagnostic`s with codes E0201–E0206. An S-expression printer shows any AST as compact text. Tests use it, and `--emit=ast` will reuse it later.

## CONTEXT

- Starting state: `src/lexer/` produces `Vec<Token>` ending in `Eof`; `src/span.rs`, `src/diagnostic.rs` exist. `src/lexer/mod.rs` is at 299 lines — touch it only if unavoidable.
- Ending state: `src/ast/{mod,expr}.rs`, `src/parser/{mod,recover,describe,expr,primary,stmt,item,sexp,test_util}.rs`, `tests/parser.rs`, `tests/common/spec_programs.rs` created; `src/lib.rs` declares `ast` and `parser`; spec §3 amended.

### Amendments during implementation (session 6)
- **File split (user-approved).** After `cargo fmt`, four of the planned files were over 300 lines. Split by concern, with public paths unchanged:
  - `ast` became a folder: `mod.rs` holds items, types and statements; `expr.rs` holds the expression nodes, re-exported.
  - Recovery, the nesting limit and comma lists moved to `parser/recover.rs`.
  - Primary expressions, struct literals, arrays and `if`/block expressions moved to `parser/primary.rs`.
  - Test helpers moved to `parser/test_util.rs`.
- **`parser/describe.rs`.** Token names for error messages. The lexer has no display text and is untouched.
- **E0203 help** uses the actual operators with placeholder operands (`(a < b) && (b < c)`). The parser has no source text to quote real operands.
- **Shared test programs.** The spec programs moved from `tests/lexer.rs` to `tests/common/spec_programs.rs` so the lexer and parser tests share one copy.
- Related existing code: `src/lexer/token.rs` (`TokenKind`), `src/diagnostic.rs`.
- Open decisions that must be resolved first: none.

### Discovery answers (session 6)
1. AST: an owned `Box` tree. Every `Expr` has a span and an `ExprId` numbered by the parser, which keys the checker's type table.
2. Recovery: at statement boundaries, and at item boundaries at top level.
3. Error codes: E0201–E0205, plus E0206 (nesting too deep) added in the draft — confirm on approval.
4. Grammar gaps: an optional `;` after a block-like statement is allowed and recorded; the no-struct-literal rule lifts inside every bracket pair, including blocks; a lone `;` is E0201.
5. Output: an S-expression printer.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**AST** — `src/ast.rs`
- `Program { items: Vec<Item>, expr_count: u32 }`. `expr_count` lets the checker size its table.
- `Ident { name: String, span }`.
- `Item`:
  - `Fun(FunDecl)` — name, params, `ret: Option<Type>` (`None` means `void`), body `Block`, span.
  - `Extern(ExternDecl)` — name, params, `ret`, span.
  - `Struct(StructDecl)` — name, fields, span.
- `Param` / `Field { name: Ident, ty: Type }`.
- `Type { kind: TypeKind, span }`, where `TypeKind` is `I32 | I64 | U8 | F64 | Bool | String | Named(String) | Array(Box<Type>)`.
- `Block { stmts: Vec<Stmt>, tail: Option<Box<Expr>>, span }`.
- `Stmt { kind: StmtKind, span }`. `StmtKind`:
  - `Let { mutable, name, ty: Option<Type>, init: Expr }`
  - `Assign { op: AssignOp, place: Expr, value: Expr }`
  - `Expr { expr: Expr, semicolon: bool }`
  - `While { cond, body }`
  - `For { var: Ident, iter: ForIter, body }`, where `ForIter` is `Range(Expr, Expr) | Array(Expr)`
  - `Return(Option<Expr>)`, `Break`, `Continue`
- `Expr { id: ExprId, span, kind: ExprKind }`. `ExprKind`:
  - Literals: `Int(u64)`, `Float(f64)`, `Str(String)`, `Bool(bool)`
  - `Name(String)`
  - `Unary(UnOp, Box<Expr>)`, `Binary(BinOp, Box<Expr>, Box<Expr>)`, `Cast(Box<Expr>, Type)`
  - `Call(Box<Expr>, Vec<Expr>)`, `Index(Box<Expr>, Box<Expr>)`, `Field(Box<Expr>, Ident)`
  - `StructLit(Ident, Vec<(Ident, Expr)>)`
  - `Array(Vec<Expr>)`, `Repeat(Box<Expr>, Box<Expr>)`
  - `If { cond, then: Block, else_: Option<Box<Expr>> }` — the else branch is an `If` or `Block` expression
  - `Block(Block)`
- `ExprId(u32)` values are dense, `0..expr_count`, in creation order.

**Entry point** — `src/parser/mod.rs`
- `pub fn parse(tokens: &[Token]) -> Result<(Program, Vec<Diagnostic>), Vec<Diagnostic>>`. `Ok` = AST + warnings (none today), `Err` = every diagnostic.
- Precondition: `tokens` ends in `Eof`, as `lex` guarantees. Never panics on any token sequence that does.
- Contains the `Parser` state and token helpers (`peek`, `bump`, `expect`), plus recovery.

**Items and types** — `src/parser/item.rs`
- `fun`, `extern fun`, `struct`, params and fields with optional trailing commas, types with `[]` suffixes.
- Expression bodies: `fun f(): T = e;` produces exactly the AST of `fun f(): T { e }` — a block with no statements and tail `e`.
- At top level anything else is E0201 "expected `fun`, `extern` or `struct`", with help "statements and variables must be inside a function" when the token is `let` or starts an expression.

**Statements and blocks** — `src/parser/stmt.rs`
- `let`, `while`, `for` (with or without parentheses around the head), `return [expr];`, `break;`, `continue;`.
- Statements starting with `if` or `{`:
  - Parsed as block-like with no `;` needed.
  - A `;` directly after one is consumed and recorded (`semicolon: true`).
  - A block-like element followed by `}` is the block's tail.
- Other statements parse an expression, then an optional assignment operator and right-hand side, then `;`.
  - An expression directly followed by `}` is the tail.
- A lone `;` where a statement should start is E0201 "expected statement, found `;`".

**Expressions** — `src/parser/expr.rs`
- Pratt parsing with the §3 binding powers: `||` 1, `&&` 2, `== !=` 3, `< <= > >=` 4, `+ -` 5, `* / %` 6, `as` 7.
- Prefix `-` and `!` bind tighter than every infix operator; postfix call, index and field bind tightest.
- `as` takes a type, including `[]` suffixes.
- Levels 3 and 4 are non-associative: a second operator at the same level is E0203.
- Struct literals:
  - `IDENT {` starts a struct literal except in no-struct-literal mode, which is active while parsing an `if`/`while` condition and the `in`/`of` expressions of a `for` head.
  - The mode is lifted inside `( )`, `[ ]`, `{ }` and call arguments, and restored afterwards.
  - In that mode, `IDENT { IDENT :` is E0205 "struct literal in a condition must be in parentheses", with a help showing the parenthesised form. The parser consumes the literal and continues.
- Array literals: `[]`, `[a, b, c,]`, `[v; n]`.
- Numbers are not range-checked or folded with `-` — that is the checker's job.

**Errors**

| Code | When | Extra |
| --- | --- | --- |
| E0201 | Unexpected token: "expected X, found Y" | When it's a missing closer, a label on the opener ("to match this `(`") |
| E0202 | Unclosed delimiter: `Eof` reached while a `(`, `[` or `{` is open | Primary span on the opener |
| E0203 | Chained comparison or equality (`a < b < c`, `a == b != c`) | Help: `write (a < b) && (b < c)`, built from the actual operands |
| E0204 | Assignment operator where an expression continues or a closer was expected (`a = b = c`, `if (x = 1)`, `f(x = 1)`) | Help: `use == to compare`, for `=` only |
| E0205 | Struct literal in a condition | Help with the parenthesised form |
| E0206 | Nesting deeper than 256 levels of expressions or blocks | Stops recursion before it can overflow the stack |

**Recovery**
- After an error inside a block, skip tokens until a `;` (consumed) or a `}` (not consumed) at the same bracket depth. Then continue with the next statement.
- After an error at top level, skip to the next `fun`, `extern` or `struct` at bracket depth 0.
- Every recovery step consumes at least one token, so parsing always terminates.
- A failed sub-expression abandons its whole statement, so one mistake produces one diagnostic, not a cascade.

**S-expressions** — `src/parser/sexp.rs`
- `pub fn program(&Program) -> String` (one line per item) and `pub fn expr(&Expr) -> String`. No ids or spans in the output.
- Forms:
  - Operators by their symbol: `(+ 2 (* 3 4))`. Unary minus is `neg` and `!` is `not`. Casts are `(as e T)`.
  - `(call f a b)`, `(index a i)`, `(. a b)`, `(struct-lit Point (x 1.0) (y 2.0))`, `(array 1 2)`, `(repeat v n)`.
  - `(if c (block …) (block …))`. A block is `(block stmt… tail)`: statements are wrapped and a tail is printed bare.
  - Statements: `(let x e)`, `(let mut x i32 e)`, `(= p v)`, `(+= p v)`, `(; e)` with a semicolon, `(stmt e)` for a block-like without one, `(while c b)`, `(for i (range a b) b)`, `(for x (of xs) b)`, `(return e)`, `(return)`, `(break)`, `(continue)`.
  - Items: `(fun name ((a i64) (b i64)) i64 (block …))`, with `void` when there is no return type. `(extern puts ((s string)) i32)`. `(struct Point (x f64) (y f64))`.
  - Literals: integers in decimal, floats with Rust `{:?}`, strings with Rust `{:?}`, `true`/`false`.

**Spec update** — §3:
- An optional `;` after a block-like statement.
- The no-struct-literal rule lifts inside every bracket pair, including blocks.
- A lone `;` is an error.
- A table of parse error codes E0201–E0206 and the 256-level nesting limit.

### Must NOT Do
- No type checking, name resolution, place-expression validation (§3 assigns that to the checker), literal range checks or `-` folding.
- No desugaring beyond expression bodies — `for` loops and `+=` stay as written.
- No printing of diagnostics, and no `--emit=ast` CLI flag (driver PRP).
- No new dependencies. No changes to the lexer unless a lexer bug is found; if one is, stop and report it.

## ERROR HANDLING REQUIREMENTS

- `parse` returns `Err` if any error was recorded. Diagnostics are in source order, with codes E0201–E0206 only.
- Internal parse functions return `Result<T, Reported>`, where the zero-sized `Reported` means "a diagnostic was already pushed". Callers recover rather than report again.
- Never panics. The token cursor never moves past `Eof`, and every `expect` handles `Eof`.

## SECURITY CONSIDERATIONS

- Tokens come from untrusted source.
  - Recursion is bounded by E0206 at 256 levels; the limit is checked on every recursive descent into an expression or block.
  - Work is linear in the token count, apart from recovery skipping, which is also linear.
- No `unsafe`.

## TESTS TO WRITE

Unit tests (module bottoms), mostly as `source → s-expression` pairs:
- [x] Precedence: `2 + 3 * 4`, `a || b && c`, `a + b == c * d`, `-x as f64`, `!a && b`, `a * b as f64`, `-a * b`.
- [x] Left associativity: `a - b - c`, `a / b / c`.
- [x] Postfix chains: `a.b[i](c)`, `f(1, 2,)`, `pts[i].x`.
- [x] Literals: struct literal (trailing comma), `[]`, `[1, 2,]`, `[false; n + 1]`, strings, bools, floats.
- [x] `if` / `else if` / `else` as an expression in `let`.
- [x] Blocks: tail vs statement; `{ x * x; }` has no tail; `if c { a } - 1` is a statement followed by the tail `(neg 1)`; `if c { f(); };` records the semicolon.
- [x] Statements: `let`, `let mut x: i32 = 5;`, every assignment operator, `while`, `for i in 0..n`, `for (i in 0..n)`, `for x of xs`, `return;`, `return e;`, `break;`, `continue;`.
- [x] Conditions: `if (x > 0) {` and `if x > 0 {` give the same AST; `if p == (Point { x: 0.0 }) {}` parses; `if ok { Point { x: 1.0 } } else { o }` parses.
- [x] Items: fun with and without a return type, params with a trailing comma, the expression body equals the block form, extern, struct, array types `i64[][]`.
- [x] E0201: missing `;`, `expected expression, found )`, top-level `let` with help, lone `;`.
- [x] E0202: `fun main() { (1 + 2`, with the primary span on the opener.
- [x] E0203: `a < b < c`, `a == b != c` with help; `(a == b) == c` is fine.
- [x] E0204: `a = b = c;`, `if (x = 1) {}`, `f(x = 1);`.
- [x] E0205: `if p == Point { x: 0.0, y: 0.0 } { }`.
- [x] E0206: 300 nested `(` gives one E0206 and no stack overflow.
- [x] Recovery: three bad statements in one function plus a following good function give exactly 3 diagnostics, and the good function's body is never reported.
- [x] Ids: dense and unique across a program (`expr_count` equals the number of `Expr` nodes).
- [x] Spans: a binary expression's span covers both operands.

Integration (`tests/parser.rs`):
- [x] Every spec §10/§11 program parses with no diagnostics (lex, then parse).
- [x] The milestone 1 program prints exactly `(fun main () i32 (block (+ 2 (* 3 4))))`.
- [x] Parsing never panics on any token prefix of a large sample (truncate the token list and append `Eof`).

## ROLLBACK PLAN

- Branch `prp-003-parser`, merged into `main` on acceptance.
- To abandon: delete the branch. No migrations or runtime state.

## ACCEPTANCE CRITERIA
- [ ] Every test above exists and passes.
- [ ] Spec §3 updated (block-like `;`, restriction in blocks, lone `;`, E0201–E0206 table, nesting limit).
- [ ] No file over 300 lines; no new dependencies; `src/lexer/` untouched.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass.
- [ ] Every `pub` item documented; functions returning `Result` document `# Errors`.
- [ ] CLAUDE.md FILE ORGANIZATION, CHANGELOG.md, TODO.md updated.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo test -- --ignored` still shows only `m1/arith.la: exit code` (parser not wired into lughac yet).
