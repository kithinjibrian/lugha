## FEATURE: Conditional patterns — `if let`, `while let`, `let … else` (E0429); milestone 33, v9 complete.

**Status:** merged 2026-10-14 — session 79. This is the last part of the user's "in one go" (milestones 31–33), merged once CI is green.
**Milestone:** 33 (one PRP; completes v9; tags `m33` and `v9` at merge)
**Spec:** `Language v9 Draft.md`:
- decision 5, and its choices on `if let` and `let … else`;
- §3 (`if_expr`, `while_stmt`, `var_decl`);
- §5;
- §9;
- E0429;
- §10 "Conditional patterns";
- §11 (milestone 33).

**Decisions:** MEMORY 72–74 (v9).

## OBJECTIVE
The §10 conditional-patterns program prints `skipped x`, `7`, `321`, `42`, `-1`:

```
if let Some(n) = parse_i64(w) { total += n; } else { println("skipped {w}"); }
while let Some(top) = stack.pop() { print(top); }
let Some(n) = parse_i64(s) else { return -1; };
```

## CONTEXT

- **Starting state:**
  - Patterns, including or-patterns and `mut` (milestone 31), work only in `match`.
  - Pattern typing is `Checker::arm_pattern`, and codegen tests a pattern with `codegen/patterns.rs::test`.
- **Ending state:** three new forms that reuse that machinery; v9 is complete.
- **Related existing code:**
  - `src/ast/{expr,mod}.rs`;
  - `src/parser/{primary,stmt}.rs`;
  - `src/check/{expr,stmt,matching,patterns,flow}.rs`, `src/check/liveness/`;
  - `src/codegen/{expr,stmt,patterns,control}.rs`.
- **Open decisions that must be resolved first:** none.

### Design
- **Native forms, not a rewrite into `match`:** a rewrite would add a hidden `_` arm, making W0102 fire for an irrefutable `if let`, and would turn the rest of a block into an arm for `let … else`.
- **AST:**
  - `ExprKind::IfLet { pattern, scrutinee, then, else_ }`;
  - `StmtKind::WhileLet { pattern, scrutinee, body }`;
  - `StmtKind::LetElse { pattern, init, else_ }`.
- **Parser** (new `parser/conditional.rs`):
  - after `if` or `while`, a `let` starts the pattern form: `let pattern = cond`;
  - a `let` statement that has an `else` after its initialiser is `let … else`. When the first tokens are `name =`, it is first read as an ordinary `let` and becomes `let … else` if `else` follows.
- **Checker** (new `check/conditional.rs`):
  - **The pattern** is typed as a top-level arm pattern (`arm_pattern`), against the scrutinee's type. Or-patterns and `mut` work, and there is no exhaustiveness check.
  - **`if let`:** its bindings are in scope in `then`. With `else`, the branches agree as `if`'s do; without `else`, it is `void`.
  - **`while let`:** its bindings are in scope in the body, and the body is a loop body (`break`, `continue`).
  - **`let … else`:** the `else` block must have type `never`, or it is E0429 at the block. The bindings are declared in the current scope, for the rest of the block.
  - **Liveness and flow** visit the new forms as they visit `match` and `while`.
- **Codegen** (new `codegen/conditional.rs`):
  - **`if let`:** test, then the then-block, or jump to `else`; the result is stored in a slot, as for `match`.
  - **`while let`:** a condition block evaluates and tests the scrutinee, the body follows, and a failed test exits; `continue` goes to the condition.
  - **`let … else`:** test with `next` set to the `else` block (which ends in a terminator), then continue where the test succeeded, with the bindings declared in the current scope.
- **Programs without these forms** give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- AST, parser and sexp (`(if-let p e then else)`, `(while-let p e body)`, `(let-else p e else)`);
- the checker and codegen above, with E0429 in `errors/v9.rs`;
- every exhaustive walk over expressions and statements gains the new forms;
- **Spec:**
  - §3;
  - §5;
  - §9;
  - the E0429 row;
  - the §10 program;
  - §11 milestone 33 done;
  - the title and status say v9 is complete;
  - §12 gains the two v9 questions.
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 75, CONTEXT. The tags `m33` and `v9` are added at the merge.

### Must NOT Do
- **No exhaustiveness check for these forms.**
- **No irrefutable-pattern warning:** that is a v9 §12 question.
- **No guards in `if let`.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0429:** a `let … else` whose `else` block can finish.
- **The pattern's errors are `match`'s:** E0403, E0415, E0416, E0426, E0428.

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — parser:
- [x] `if let Some(x) = o { } else { }`, `else if let …`, `while let Some(x) = it.next() { }`, `let Some(n) = o else { return; };` and `let x = e else { return; };` parse and print.

Unit tests — checker (`check/conditional.rs`):
- [x] **Bindings:**
  - visible in `then` and in the body, and after `let … else`;
  - not visible in `else`.
- [x] **Types:**
  - `if let` with `else` gives the branches' type;
  - an or-pattern with `mut` works.
- [x] **E0429:** an `else` that finishes, versus `return`, `break` (in a loop) and `panic(…)`.
- [x] **Pattern errors:** a wrong pattern type is E0403.

Unit tests — codegen: an `if let` tests the tag and branches to its `else`.

Acceptance — `tests/programs/m33/`:
- [x] `conditional.la`: the §10 program.
- [x] `more.la`:
  - `else if let` chains;
  - `if let` as a value;
  - `while let` with `break` and `continue`;
  - `let … else` with `continue` in a loop;
  - an or-pattern in `if let`;
  - `mut` in `while let`.
- [x] `e0429.la`: reject golden.
- [x] The §10 program runs from the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-062-conditional_patterns`, merged into `main` after CI passes; tags `m33` and `v9`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m33/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] The v9 spec is complete; docs updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m33/conditional.la
```

## AMENDMENTS (during build, session 79)

- **Boxed AST fields:** the patterns and the statement blocks are boxed (`Box<Pattern>`, `Box<Block>`), to satisfy clippy's `large_enum_variant` on the parser's `Element`.
- **`if_expr`'s merging** became `Checker::join_branches`, shared with `if let`; `matching::unused` became `pub(super)`, so `if let` and `while let` warn about unused bindings (W0103). `let … else` doesn't, since its bindings live on in the block.
- **`-O2`** was checked by hand for `m33/conditional` and `m33/more`.
- **No file splits.**
- **`ExprKind::IfLet(Box<IfLet>)`:** the first CI run overflowed the 2 MB test stack in `deep_nesting_is_e0206_not_a_stack_overflow`, because the inline `IfLet` fields grew `Expr` from 88 to 96 bytes and the recursive-descent parser's frames with it. Boxing the whole payload returns `Expr` to 88 bytes; the test passes with `RUST_MIN_STACK=2097152`.
