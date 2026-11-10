# Code Style — lugha

How lugha's code is written and commented: the Rust compiler, the C runtime, the prelude and std in
Lugha, and the VS Code extension in TypeScript. Read this before writing any function, type or
module.

Formatting is not a style choice: `cargo fmt` decides it. Run it, and don't argue with it.
`cargo clippy --all-targets -- -D warnings` must pass. `tests/comments.rs` fails on any comment that
names history or a document.

---

## What a comment is for

A comment says what the code can't: what an item is for, and why a line is the way it is. It is
read long after the conversation that produced it, by someone who has only the code. So it stands
on its own:

- **No history.** Never name a language version (`v17`), a milestone, a PRP, a decision number, a
  session or who asked for something. "Since v15", "was", "now", "no longer" and "used to" are
  history too. DECISIONS.md and git record how the code got here; the comment says why it is right
  now.
- **No documents.** Never point at a spec section (`§4`), a draft or a `.md` file. Documents are
  rewritten and the pointer goes stale. If a rule from the spec matters here, state the rule. The
  spec may point at code; code never points at the spec.
- **Full sentences.** Plain words, nothing dropped to save space.
- **Delete rather than repeat.** A comment that says what the next line says is removed, not
  reworded.

---

## Doc comments (what this is)

Every `pub` function, struct, enum, trait and type alias gets a `///` doc comment; private items
only when their purpose isn't obvious.

Start with one sentence saying what the item is or does, from the caller's side, starting with a
verb: "Parses", "Returns", "Lowers". Never "This function…". Add more only for what a caller needs:

- **Units**, every time: bytes or characters, milliseconds or seconds.
- **`# Errors`**: for a `Result`, every variant or diagnostic code and when it comes back.
- **`# Panics`**: for a function that can panic, the invariant that makes it panic.
- **`# Safety`**: for every `unsafe fn`.
- **Surprises:** anything the name and type don't suggest.

An `# Examples` section runs as a doctest, so it must compile. Write one only when how to call the
item isn't obvious.

```rust
/// Parses a decimal integer literal.
///
/// Leading and trailing whitespace is ignored.
///
/// # Errors
///
/// Returns [`ParseError::InvalidNumber`] if `src` is not a valid `i64`.
pub fn parse_number(src: &str) -> Result<i64, ParseError> { ... }
```

Std and prelude items carry `/** … */` comments, which the language server shows on hover. The same
rules apply: say what the item does for a Lugha programmer, never where it came from.

---

## Inline comments (why this is)

Inline `//` comments explain decisions, not code. Put the comment above the line it explains, never
trailing.

Comment when:
- a number appears whose value isn't self-explanatory,
- a crate or LLVM is used in a way that isn't obvious,
- a guard prevents a bug that isn't obvious,
- a performance trade-off was made,
- the code works around something missing upstream.

A workaround names what's missing and where: `// inkwell has no X yet, so …`. That is how it's
found and removed once the gap is fixed. Every `unsafe` block has a `// SAFETY:` comment.

If code needs a comment to say *what* it does, rewrite the code.

**Good:**

```rust
// Peek two tokens ahead: `a.b` and `a..b` share a prefix.
if self.peek_at(1) == Token::Dot {
```

**Bad:**

```rust
// v17 range syntax (milestone 152, spec §5)
if self.peek_at(1) == Token::Dot {
```

The bad one says when the line arrived and where it's written down; it never says why.

---

## Module comments

Every Rust file starts with a `//!` block, and every C fragment with a `/* … */` block, saying what
the module is responsible for, and what it isn't:

```rust
//! Turns source text into tokens.
//!
//! Doesn't build syntax trees: that's `parser`.
```

Don't list imports or importers: the imports and a search already say that, and a list goes stale.

---

## Pointing at other code

A comment may name another module, type or function when that's where the rest of the story lives.
Write it as code would, in backticks: `` `check::maps` ``, `` `Checked::conversions` ``,
`` `lugha_rt_alloc` ``. Never a file path. Don't use pointers to list who calls what.

---

## TODOs

Don't leave them. Work that isn't done goes in TODO.md.

---

## Anti-patterns

1. **Describing the code.** The code is the description.
2. **History.** Versions, milestones, PRPs, decision numbers, "since", "now", "no longer".
3. **References to documents.** Spec sections, drafts, any `.md` file.
4. **Stale comments.** Change the comment in the same edit as the code.
5. **Over-documenting.** Obvious helpers need no comment; most items need one sentence.
6. **Under-documenting the error contract.** Every `Result` names its errors.
