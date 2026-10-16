## FEATURE: Formatter hugging — a call whose last argument is an array, closure or struct literal keeps that argument "hugged" (`column([` … `])`), and a function's header isn't broken just because its body is long; milestone 95 (v17).

**Status:** built in session 220; approved 2026-10-17 ("approved all and build in one go", session 220); proposed in session 219, with PRP-127 and PRP-128.
**Milestone:** 95 (tag `m95` at merge).
**Spec:** `Language v17 Draft.md` decisions 35–38; v17 spec §9 (`lughac fmt`'s style), §11 (row 95).

**Why:** kivuli's quirk 2. Every kivuli view is a nested `column([...])`; today `lughac fmt` breaks the *signature* first and puts the whole array on one long line:
```
fun view(
    m: Model,
): View<Msg> = column(
    [text("Clicked {m.count} times"), button("Add", Msg.Add), button("Reset", Msg.Reset)],
);
```

**Decisions (mine, for review — the user asked to "fix the rest"):**
- **Hugging:** when a call doesn't fit on its line and its *last* argument is an array literal, a closure or a struct literal, and the arguments before it fit on the first line, the call prints as `f(a, b, [` / one element per line / `])` (the closure's body or the struct's fields likewise). Rustfmt, Prettier, gofmt and Dart all do this for trailing collections and closures.
- **Nested hugging:** an element that is itself such a call hugs the same way (`column([ row([ … ]), … ])`).
- **The header first:** an expression body (`fun f(…): T = e;`) breaks inside `e` before the parameter list is broken; the parameter list breaks only when the header alone doesn't fit.
- **Unchanged:** everything that fits keeps its one-line form; the formatter still checks its own output (re-parse, same tree, same comments), so a hugging mistake falls back to the current layout with no file changed.

## OBJECTIVE
```
fun view(m: Model): View<Msg> = column([
    text("Clicked {m.count} times"),
    row([
        button("Add", Msg.Add),
        button("Reset", Msg.Reset).disabled(m.count == 0),
    ]),
]).padding(24);

items.iter().map(fun(item) {
    …
});

draw(Rect {
    x: 1.0,
    y: 2.0,
    w: 3.0,
    h: 4.0,
});
```

## CONTEXT
- **Starting state:** `src/formatter/` — `doc.rs` (groups, 100 columns), `exprs.rs` (calls, arrays, closures, struct literals), `items.rs` (functions); `tests.rs` holds the style rules; `tests/fmt_corpus.rs` formats every program, the spec and std/prelude and checks idempotence. CLAUDE.md lists "formatter limits (binary and method chains never break)".
- **Ending state:** hugging in `exprs.rs` (a call's last argument) on top of the document printer's groups (a "hug" form: the last argument's own group may break while the call's stays flat); `items.rs` keeps the header on one line when it fits by itself; spec v17 §9 style text; `tests.rs` cases for each rule; the corpus re-formatted (its own diff reviewed: only hugging changes).
- **Related existing code:** `src/formatter/{doc,exprs,items,tests}.rs`, `tests/fmt_corpus.rs`, `tests/fmt.rs`, `tests/lsp_formatting.rs`.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- Test-first: one `tests.rs` case per rule (array, closure, struct literal, nested, header kept, fallback when the earlier arguments don't fit, method calls on a hugged call `column([…]).padding(24)`).
- Idempotent: formatting the output again changes nothing (`fmt_corpus`).
### Must NOT Do
- No change to anything that fits on one line; no new style options.
### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- None new: the formatter's self-check still refuses output that doesn't parse back to the same tree.

## SECURITY CONSIDERATIONS
- None.

## TESTS TO WRITE
- [ ] `tests.rs`: the OBJECTIVE's three shapes; nested arrays; a hugged call followed by a method chain; arguments before the last too long (no hugging); a short header kept with a long body; comments inside a hugged array kept.
- [ ] `fmt_corpus` idempotence; `lsp_formatting` unchanged behaviour.

## ROLLBACK PLAN
- Branch `prp-126-fmt_hugging` in its own worktree.

## ACCEPTANCE CRITERIA
- [ ] kivuli's `examples/counter.la` formats as in the OBJECTIVE.
- [ ] Targeted tests (formatter lib tests, `fmt`, `fmt_corpus`, `lsp_formatting`), fmt, clippy pass; CHANGELOG updated.

## VALIDATION
- `cargo test --lib formatter`, `cargo test --test fmt --test fmt_corpus --test lsp_formatting`, `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`

## BUILT (session 220)
- Prettier's conditional group in the printer: `Doc::Hug(normal, hugged)`, with `Broken` and `Huggable`; `call_args` for calls only (parameter lists and arrays inside arrays don't hug).
- Closures needed nothing: a closure with a block body already hugged (a last element with a hard break).
- The header rule is the measuring change: what follows a group is measured with its groups as able to break.
- Tests: 5 formatter cases (array, nested + method chain, struct literal, no hug when the first line doesn't fit, header kept); `fmt`, `fmt_corpus` (idempotence over every program, the spec, std, the prelude), `lsp_formatting` pass. kivuli's `examples/counter.la` (a copy) formats as intended. Full suite not run (the user).
