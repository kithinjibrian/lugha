## FEATURE: Inference from sibling arguments — a generic call whose type arguments only its result decides (`text<M>("hi"): View<M>`) takes them from what the other arguments fix (`assert_eq(text("hi"), expected)`); milestone 96 (v17).

**Status:** approved 2026-10-17 ("approved all and build in one go", session 220); proposed in session 219, with PRP-126 and PRP-128.
**Milestone:** 96 (tag `m96` at merge).
**Spec:** `Language v17 Draft.md` decisions 39–42; v17 spec §4 (type inference), §11 (row 96).

**Why:** kivuli's quirk 3. `assert_eq(text("hi"), expected)` is E0412 even when `expected: View<Click>` says what `M` is, so kivuli's tests need wrappers (`t()`, `col()`) for every view, and so will `ui.testing` users.

**Decisions (mine, for review):**
- **Order of arguments in inference:** today the arguments that can be typed alone go first, then literals and `None`, then closures (`check/infer.rs`). A call to a generic function whose type parameters appear only in its result (nothing in its arguments can fix them) now waits with the literals: it is checked after its siblings, with what they fixed as its expected type — as `None` already is.
- **The same for `assert_eq` / `assert_ne`:** their two operands must have one type; the operand that can't be typed alone is checked second, expecting the other's type.
- **And for `==` / `!=` operands** (`text("hi") == expected`), the same rule.
- **Unchanged:** a call that its own arguments can type keeps its place; nothing new is inferred from later statements (no whole-function inference).

## OBJECTIVE
```
@test
fun buttons_send_their_message() {
    let expected: View<Msg> = View.Button(ButtonNode { … });
    assert_eq(button("Add", Msg.Add), expected);
    assert_eq(text("hi"), testing.text_of(view(model)));   // M fixed by the other side
    assert(text("hi") == expected_text);                     // `==` the same way
}
```

## CONTEXT
- **Starting state:** `check/infer.rs` (`infer`: `now` / `later` / closures; `input` with hints), `check/ops.rs` (`defers`), `check/test_fns.rs` (`assert_eq`/`assert_ne`), `check/ops.rs` binary `==`.
- **Ending state:** `defers` (or a checker-aware variant) also true for a call to a generic function whose type parameters appear only in its result; such an argument is checked with `known(want)` as its expected type (the hint closures get); `assert_eq`/`assert_ne` and `==`/`!=` check the deferring side second with the other side's type expected; spec §4's inference paragraph says so.
- **Related existing code:** `src/check/{infer,ops,test_fns,call}.rs`.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- Test-first; byte-identical IR for programs that already checked (only E0412 programs change meaning).
### Must NOT Do
- No bidirectional inference across statements; no change to which programs already check.
### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- E0412 only when no sibling fixes the parameters either; its message unchanged.

## SECURITY CONSIDERATIONS
- None.

## TESTS TO WRITE
- [ ] Checker: `assert_eq(f(), x)` and `assert_eq(x, f())` with `f<M>(): View<M>`; a generic function's arguments `pair(make(), 1)`; `==` both ways; still E0412 with nothing fixing `M`; a program that already checked keeps its types.
- [ ] `tests/programs/m96`: the OBJECTIVE shape, run.
- [ ] IR unchanged for the compiling `tests/programs`.

## ROLLBACK PLAN
- Branch `prp-127-sibling_inference` in its own worktree.

## ACCEPTANCE CRITERIA
- [ ] kivuli's diff tests can drop their `t()`/`col()` wrappers.
- [ ] Targeted tests (`--lib`, `--test programs`), fmt, clippy pass; CHANGELOG updated.

## VALIDATION
- `cargo test --lib`, `cargo test --test programs`, `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`
