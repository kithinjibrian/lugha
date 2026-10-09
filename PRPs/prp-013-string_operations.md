## FEATURE: String operations — `+` concatenation, `==`/`!=` by content, `.len`, and bounds-checked `s[i]` — with an informative out-of-bounds panic.

**Status:** implemented 2026-10-09 — session 16 (branch `prp-013-string_operations`)
**Milestone:** 5, first of four PRPs (014 arrays, 015 structs, 016 `lughac spec` and the full §10 acceptance)
**Spec:** §4 (strings: immutable, `.len`, `s[i]` as `u8`, `+`, `==`), §5 (panics), §7 (string layout), §9 (bounds checks with `icmp ult`, runtime table, codes)
**Decisions:** DECISION-009 / MEMORY 15 (runtime ABI); PRP-012 (operator spans for panic locations)

## OBJECTIVE
Strings can be joined, compared, measured and read byte by byte: `"hello, " + name`, `a == b`, `s.len`, `s[0]`. Reading past the end — or before the start — stops with `panic: index out of bounds: the length is 5 but the index is 7 at file:line:col`, exit 101, pointing at the `[`. Misuse gets coded errors. Strings stay immutable.

## CONTEXT

- Starting state:
  - Strings are values (PRP-011), but any binary operator on a `string` stops the checker with "not implemented yet: string operations (milestone 5)".
  - `.field` and `x[i]` stop with milestone 5.
  - `ExprKind::Index(base, index)` carries no bracket span.
- Ending state:
  - The checker types string operators, `.len` and `s[i]`, and reports E0410/E0411/E0506.
  - Codegen calls new runtime functions for `+`/`==` and lowers `.len`/`s[i]` inline, with a bounds check.
  - `ExprKind::Index` gains the `[` span.
  - New `codegen/heap.rs` holds the safe address arithmetic arrays will reuse.
  - New `tests/programs/m5/`; spec §5 and §9 updated.
- Related existing code: `src/check/{ops,expr,assign,errors}.rs`, `src/codegen/{arith,expr,runtime}.rs`, `runtime/lugha_rt.c`, `src/ast/expr.rs`, `src/parser/{expr,sexp}.rs`.
- Open decisions that must be resolved first: none.

### Amendments during implementation (session 16)
- **Checker:** field and index typing live in a new `check/access.rs`, keeping `expr.rs` small.
- **Codegen:** the source-file C string is shared through `file_name()`, and `runtime()` is visible to sibling modules, so `heap.rs` can declare runtime calls.
- **Updated test:** the checker test listing milestone 5 stops dropped string `+` and `.len` (now real rules) and gained a repeat-literal case.
- **Goldens:** the `m5/` stdout and panic goldens were written by hand and matched on the first run. The three reject goldens were captured and reviewed.

### Discovery answers (session 16)
1. Milestone 5 is four PRPs: 013 strings, 014 arrays with their copies and `for … of`, 015 structs with their copies, 016 `lughac spec` and every §10 program.
2. Codes:
   - **E0410** no such field
   - **E0411** not indexable
   - **E0506** assignment into a string (strings are immutable)

   Index-type mistakes reuse E0401/E0403; `<` on strings reuses E0404.
3. Bounds panic: `index out of bounds: the length is N but the index is I`, formatted by a new runtime function `lugha_rt_panic_bounds(len, index, file, line, col)`. It is located at the `[`, so `Index` gains the bracket span.
4. Run-time-indexed addresses use **integer address arithmetic** (`ptrtoint` + offset + `inttoptr`), with no `unsafe`. Performance is a v0 non-goal (§1).

## IMPLEMENTATION REQUIREMENTS

### Must Do

**AST and parser**
- `ExprKind::Index(Box<Expr>, Span, Box<Expr>)`, where the `Span` is the `[`.
- The parser records it; S-expressions are unchanged.

**Runtime** — `runtime/lugha_rt.c`
- `LughaString *lugha_rt_str_concat(const LughaString *a, const LughaString *b)`: a new string from `lugha_rt_alloc`, holding the length, the bytes and a NUL.
- `int32_t lugha_rt_str_eq(const LughaString *a, const LughaString *b)`: 1 if the lengths and bytes are equal (`memcmp`), else 0.
- `void lugha_rt_panic_bounds(int64_t len, int64_t index, const char *file, int64_t line, int64_t col)`:
  - flushes stdout;
  - prints `panic: index out of bounds: the length is <len> but the index is <index> at <file>:<line>:<col>` and a newline to stderr;
  - calls `exit(101)`.
- These join `RUNTIME_SYMBOLS`.

**Checker** (spec §4)
- `string + string` → `string`. `==` and `!=` on two strings → `bool`. Any other operator with a `string` operand → **E0404**. The "string operations (milestone 5)" stop is removed.
- `s.len` → `i64`.
  - Any other field on a `string` → **E0410** "no field `size` on `string`".
  - Any field on a number or `bool` → E0410 "no field `len` on `i64`".
- `s[i]` → `u8`. The index expects `i64`, so a float literal is E0401 and an `i32` is E0403.
  - Indexing a number or `bool` → **E0411** "cannot index into a value of type `i64`".
- Assigning to `s[i]` or `s.len` → **E0506** "cannot assign into a string: strings are immutable" (§4). The place and value are still checked.
- Fields and indexing on arrays and structs still stop with milestone 5; PRP-014/015 add them.

**Codegen** — new `codegen/heap.rs`, plus `arith.rs` and `expr.rs`
- `heap.rs` provides:
  - `length(object_ptr) -> i64`: the 8-byte header at offset 0 (§7).
  - `element_address(object_ptr, index, element_size) -> ptr`: `inttoptr(ptrtoint(ptr) + 8 + index * size)`, with no `unsafe`.
  - `bounds_check(len, index, at)`: `icmp ult index, len` (an unsigned compare, so negative indexes fail too, §9). On failure it calls `lugha_rt_panic_bounds` and `unreachable`.
- Operators:
  - `+` on strings → `lugha_rt_str_concat`.
  - `==` → `lugha_rt_str_eq(..) != 0`; `!=` → `== 0`.
- `s.len` → `length(s)`.
- `s[i]`:
  - evaluates `s`, then `i` (§5 left to right);
  - bounds-checks at the `[` span;
  - loads an `i8` from `element_address(s, i, 1)`.

**Spec and docs**
- §5: name the panic message `index out of bounds: the length is N but the index is I`.
- §9: runtime table gains `lugha_rt_str_concat`, `lugha_rt_str_eq` and `lugha_rt_panic_bounds`; the code table gains E0410, E0411 and E0506.
- CLAUDE.md: add `codegen/heap.rs` to the file tree.

**Acceptance** — `tests/programs/m5/`, also cross-checked at `-O2` (`tests/codegen.rs` gains `m5`):
- `string_ops.la`:
  - `"hello, " + name + "!"` printed;
  - `==`/`!=` true and false cases;
  - `.len` of an ASCII string and of `"é👋"` (6 bytes);
  - `s[0]` printed as a number (104 for `h`);
  - a loop summing the bytes of a string.

  Expected stdout is written by hand.
- `index_oob.la`: `s[7]` on `"hello"` → exact stderr `panic: index out of bounds: the length is 5 but the index is 7 at …:L:C`, exit 101.
- `index_negative.la`: `s[-1]` → `… the index is -1 …`, exit 101.
- Reject mode: `e0410.la`, `e0411.la`, `e0506.la`. Goldens captured and reviewed.

### Must NOT Do
- No arrays, structs, `for … of`, or copies (PRP-014/015).
- No string `<`/`>` ordering, slicing, iteration over strings (§5: strings aren't iterable in v0), or indexing by anything other than `i64`.
- No `unsafe`. No new dependencies.

## ERROR HANDLING REQUIREMENTS

- Every new checker rule reports a code and keeps going (`Type::Error` suppresses cascades).
- Every string index is bounds-checked before the load. No path reads outside the object. The IR verifies.
- Runtime allocation failure already panics with "out of memory" (PRP-011).

## SECURITY CONSIDERATIONS

- Bounds checks use an unsigned compare against the header length, so negative or huge indexes can't reach memory outside the string.
- `lugha_rt_str_concat` sizes its allocation from the two lengths (`int64_t`) and copies exactly those bytes. A concatenation larger than memory panics with out of memory.
- Address arithmetic happens only after the bounds check. No `unsafe`.

## TESTS TO WRITE

Unit tests:
- [x] Parser: the `Index` bracket span slices to `[`.
- [x] Checker:
  - `+` and `==`/`!=` types; `"a" < "b"` and `"a" + 1` → E0404;
  - `.len` → `i64`; E0410 (`"s".size`, `(5).len`); `s[0]` → `u8`;
  - `s[1.5]` → E0401; `s[k]` with `k: i32` → E0403; E0411 (`n[0]`);
  - E0506 (`s[0] = 1;`, `s.len = 2;`).
- [x] Codegen IR:
  - `+` calls `lugha_rt_str_concat`; `==` calls `lugha_rt_str_eq` then `icmp ne`;
  - `s[i]` has `icmp ult`, a `lugha_rt_panic_bounds` call at the `[` location, `ptrtoint`/`inttoptr`, and a `load i8`;
  - no `getelementptr` with a run-time index.
- [x] Runtime: the new symbols are in `RUNTIME_SYMBOLS`, and the C file still compiles with `-Werror`.

Acceptance:
- [x] The three run programs and three reject programs pass; all earlier tests pass.

## ROLLBACK PLAN

- Branch `prp-013-string_operations`, merged into `main` on acceptance.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] Every test above exists and passes.
- [ ] Spec §5 and §9, CLAUDE.md, CHANGELOG.md, TODO.md and MEMORY.md updated.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `grep -rn "unsafe" src/` → nothing
