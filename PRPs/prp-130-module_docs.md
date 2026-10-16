## FEATURE: Module doc comments — a `/** … */` at the top of a file documents its module, shown when hovering the module's name; the std modules documented; milestone 98 (v17).

**Status:** proposed in session 224, after two questions answered by the user (the rule, std included).

## OBJECTIVE
Hovering `diff` in `import kivuli.diff;` or in `diff.patch(x)` shows `module kivuli.diff` and the doc comment at the top of `diff.la`, the way hovering a function shows its doc. Files written like kivuli's (a `/** … */`, a blank line, then the imports) need no change. `import std.math;` shows `std.math`'s description.

## CONTEXT
- Starting state: doc comments (milestone 77, spec v17 §2) document functions, methods, externs, structs, fields, enums, variants, interfaces and interface methods. Hovering a module name shows nothing: `lsp/hover.rs` skips `Target::Module`. The std modules open with `//` header comments.
- Ending state: spec §2 (doc comments) and §9 (the language server) describe module docs; `lsp/docs.rs` finds a file's module doc; `lsp/hover.rs` renders module targets; `src/std/*.la` headers become `/** … */`.
- Related existing code: `src/driver/lsp/{docs,hover,definition}.rs`, `src/check/partial.rs` (`Target::Module`, recorded at import path segments and at `m` in `m.f`), `src/lexer` (`doc_comments`).
- Open decisions: none.

## DECISIONS (session 224)
1. **Which comment:** a file's first `/** … */`, when nothing but whitespace and `//`/`/* */` comments comes before it, documents the module if what follows it (apart from whitespace and plain comments) is a blank line, an `import`, or the end of the file. A doc comment directly above an item (no blank line) still documents that item: nothing that works today changes meaning.
2. **std:** the six std modules' `//` headers become module doc comments (text kept, reworded only where it reads as code commentary).

## IMPLEMENTATION REQUIREMENTS

### Must Do
- `docs::module_doc(text) -> Option<Span>`: the rule above, from the lexer's `doc_comments` (so a `/**` inside a string never counts).
- Hover on a `Target::Module(path)`: ```` ```lugha\nmodule a.b\n``` ```` and, when the module's file has one, its doc's Markdown (`@tags` as for items).
- A comment attached as a module doc is never also an item's doc (the blank line already separates them; a test proves it).
- Spec §2 doc comments and §9 hover lines; draft row 98; std headers.

### Must NOT Do
- Change the compiler's treatment of comments: module docs stay comments to the lexer, parser, checker and formatter.
- Add completion or signature-help documentation for modules (not asked; a later PRP if wanted).
- Read files outside the snapshot (the module's text is already loaded).

### New dependencies
- none

## ERROR HANDLING REQUIREMENTS
- No new diagnostics. A module without a doc shows only `module a.b`; an unknown module (not in the snapshot) shows nothing.

## SECURITY CONSIDERATIONS
- None: the server reads only text it already holds; it still runs nothing and writes nothing.

## TESTS TO WRITE
- [ ] `docs.rs` units: module doc found before a blank line, before `import`, at the end of a file; not found when directly above an item; not found when an item or import comes first; leading `//` comments allowed.
- [ ] `tests/lsp_docs.rs`: hovering a module in an import path and in `m.f()` shows `module …` plus its doc; a module without one shows just `module …`; the item below a module doc keeps its own doc; `std.math` shows its description.
- [ ] `fmt_corpus` (std sources still format and verify).

## ROLLBACK PLAN
- Branch to return to: `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] Hovering `diff` in kivuli's `import kivuli.diff;` shows its top comment, with no change to kivuli.
- [ ] Targeted tests (`--lib`, `lsp_docs`, `lsp_hover`, `fmt_corpus`, `lazy_std`), fmt, clippy pass; CHANGELOG updated.

## VALIDATION
- `cargo test --lib`, `cargo test --test lsp_docs --test lsp_hover --test fmt_corpus --test lazy_std`, `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`
