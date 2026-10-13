## FEATURE: Bits — `& | ^ ~ << >>`, their compound assignments, binary literals, and the out-of-range shift panic; milestone 28.

**Status:** approved 2026-10-13 — session 71. The user asked for milestones 28 and 29 "in one go": build both within the approved v7 draft, merging each once CI is green.
**Milestone:** 28 (one PRP; tag `m28` at merge)
**Spec:** `Language v7 Draft.md`:
- decisions 3 and 5, and the choices "Both operands … one integer type", "Bit operators work on integers only" and "Compound assignment";
- §2;
- §3 (precedence, `assign_op`);
- §4 "Bit operators";
- §5;
- §9;
- §10 "Bits";
- §11 (milestone 28).

**Decisions:** MEMORY 65 (v7), MEMORY 66 (the v7 numeric types).

## OBJECTIVE
The §10 bits program prints `true`, `11`, `5`, `5`, `1024`, `-4`, `65280`:

```
let flags: u32 = 0b1010;
println(flags & 0b0010 == 0b0010);   // true: `&` binds tighter than `==`
println(~flags & 0xf);               // 5
println(-8 >> 1);                    // -4: arithmetic shift for signed types
m <<= 8;
```

## CONTEXT

- **Starting state:**
  - A lone `&` or `|` is E0101 (with help "did you mean `&&`?").
  - There are no bit operators, no `0b` literals, and only `= += -= *= /=`.
  - Precedence is `||` 1 through `* / %` 6, `as` 7.
- **Ending state:** the operators lex, parse, check and run, and the spec is at milestone 28.
- **Related existing code:**
  - lexer: `src/lexer/punct.rs`, `lexer/number.rs`, `lexer/token.rs`;
  - AST: `src/ast/expr.rs` (`BinOp`, `level`, `UnOp`), `src/ast/mod.rs` (`AssignOp`);
  - parser: `src/parser/expr.rs` (Pratt loop, `binop`, `prefix`), `parser/stmt.rs` (`assign_op`);
  - checker: `src/check/ops.rs` (`binary`), `check/expr.rs` (`unary`);
  - codegen: `src/codegen/arith.rs`, `codegen/expr.rs` (unary), `codegen/stmt.rs` (compound assignment).
- **Open decisions that must be resolved first:** none.

### Design
- **Lexer:**
  - new tokens `&` `&=` `|` `|=` `^` `^=` `~` `<<` `<<=`;
  - binary literals `0b…` (E0105 for a misplaced `_`, E0106 for no digits, E0108 for a bad digit or `0B`);
  - the E0101 help for `&`/`|` goes away.
- **`>>` is two `>` tokens:** the lexer does not make `>>` or `>>=` tokens, because `Option<Option<i64>>` closes two type-argument lists. The expression parser reads a `>` immediately followed by `>`, with no space between, as `>>`, and `>` immediately followed by `>=` as `>>=`. With a space between (`a > > b`), they remain two `>` tokens and a parse error, as today.
- **Precedence** follows the draft table:
  - `||` 1, `&&` 2, `== !=` 3, comparisons 4, `|` 5, `^` 6, `&` 7, `<< >>` 8, `+ -` 9, `* / %` 10, `as` 11;
  - the bit levels are left-associative, and chained-comparison detection (levels 3 and 4) is unchanged;
  - prefix `~` sits with `-` and `!`.
- **New code:** a new `parser/operators.rs` takes the operator table out of `expr.rs`, which is at 298 lines, and handles the `>>` pairing. Moving `binop` is the only change to existing code.
- **Checker:**
  - **Bit operators:** `& | ^` take two operands of one integer type and give it; a literal operand takes the other side's type.
  - **Shifts:** `<< >>` take the shifted value's type for the amount, as the draft chose.
  - **`~`** takes an integer.
  - **Anything else** is E0404.
  - **Compound forms** check as `x = x op y`.
- **Codegen:**
  - **Operations:** `and`, `or`, `xor`, `shl`, `lshr` (unsigned) and `ashr` (signed); `~x` is `xor x, -1`.
  - **Shift range check:** before a shift, `icmp uge n, width`. An unsigned compare catches negative amounts too. On failure it panics with `shift out of range` at the operator.
  - **Compound forms** go through the existing place-once path (`codegen/stmt.rs`).
- **Programs without bit operators** give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Lexer:** the tokens and `0b` literals.
- **AST:** `BinOp::{BitAnd, BitOr, BitXor, Shl, Shr}` with symbols and levels, `UnOp::BitNot`, and `AssignOp::{BitAnd, BitOr, BitXor, Shl, Shr}`.
- **Parser:** `parser/operators.rs` (the operator table and `>>` pairing), the prefix `~`, and the compound forms.
- **Checker:** the operator rules above.
- **Codegen:** the operations and the shift check.
- **Spec** (`Language v7 Specification.md`):
  - §2 tokens and literals;
  - §3 the precedence table and `assign_op`;
  - §4 bit operators;
  - §5 the shift panic and compound forms;
  - §9;
  - the §10 bits program;
  - §11 milestone 28 done;
  - "Changes since v6".
- **Docs:** CLAUDE.md (tree, STACK), README, TODO, CHANGELOG, MEMORY decision 67, CONTEXT. The tag `m28` is added at the merge.

### Must NOT Do
- **No integer methods:** they are milestone 29.
- **No `%=`:** that is v7's §12 open question.
- **No bit operators on `bool` or the floats:** they are E0404.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **New mistakes reuse existing codes:**
  - **E0404:** `true & false`, `1.5 | 2.0`, `a & b` with `u32` and `u64`, `~true`;
  - **E0105, E0106, E0108:** `0b_1`, `0b`, `0b102` and `0B1`.
- **At run time:** an out-of-range shift panics, `panic: shift out of range at <file>:<line>:<col>` (exit 101).

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — lexer (`lexer/punct.rs`, `lexer/number.rs`):
- [ ] The new operators lex with longest match, and `>>` lexes as `>` `>`.
- [ ] `0b1010`, `0b1_0` and `0b0` lex; `0b`, `0b_1`, `0b102` and `0B1` give E0106, E0105, E0108 and E0108.

Unit tests — parser (`parser/operators.rs`):
- [ ] **Precedence:**
  - `a & b == c` is `(== (& a b) c)`;
  - `a + b << 2` is `(<< (+ a b) 2)`;
  - `a | b ^ c & d` is `(| a (^ b (& c d)))`;
  - `~a & b` is `(& (~ a) b)`.
- [ ] **`>>`:** `x >> 1` is a shift, `a > > b` is a parse error, and `Option<Option<i64>>` still parses as a type.
- [ ] **Compound forms:** `x >>= 2;` and the other compound forms parse.

Unit tests — checker (`check/ops.rs` or a new `check/bits.rs`):
- [ ] `u32 & u32` is `u32`, and a literal takes the other side's type.
- [ ] E0404 on `bool`, the floats and mixed integer types.
- [ ] `~` on an integer is fine, and `~true` is E0404.

Unit tests — codegen (`codegen/bits.rs` or `arith.rs`):
- [ ] `and`, `or`, `xor`, `shl`, `lshr` for unsigned and `ashr` for signed.
- [ ] The shift check uses `icmp uge`.

Acceptance — `tests/programs/m28/`:
- [ ] `bits.la`: the §10 program.
- [ ] `more.la`:
  - every operator at `u8`, `i32`, `u32`, `i64` and `u64`;
  - the compound forms;
  - shifting by the width minus one;
  - `>>` on negative numbers;
  - precedence cases.
- [ ] `shift_panic.la`: `1 << 64` panics with exit 101.
- [ ] `e0404.la`: reject golden.
- [ ] The §10 program runs from the spec.

Regression:
- [ ] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-056-bits`, merged into `main` after CI passes; tag `m28`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The §10 program and the `m28/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [ ] The spec is at milestone 28; docs updated.
- [ ] No file over 300 lines: new code goes in new files, and any split of an existing file is proposed first. No new dependencies, no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m28/bits.la
```
