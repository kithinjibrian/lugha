<!-- Save as PRPs/prp-{NNN}-{feature_name}.md — next unused 3-digit number, snake_case name -->

## FEATURE: [one sentence; the milestone it becomes]

## OBJECTIVE
[What "done" looks like for someone writing Lugha: a short program and what it prints, or the command and what it does.]

## CONTEXT
- Files to read first, and the files this creates or changes.
- The spec sections it implements or changes (`docs/specs/Language v17 Specification.md`, the Draft): cite them.
- Decisions it depends on (DECISIONS.md): none may be open.

## REQUIREMENTS

### Must Do
- [specific requirement]

### Must NOT Do
- [exclusion, and why]

## DESIGN
[Only what isn't obvious: new diagnostics (codes, messages, `docs/errors/` pages), runtime or ABI changes, error enums, security notes (external input, `unsafe`, new dependencies and why, anything needing the user's review).]

## TESTS
- [ ] [happy path, each error variant or diagnostic, edge cases; which test file each lives in]

## ACCEPTANCE
- [ ] [testable criterion]
- [ ] `cargo test --lib` and the touched integration tests pass; `cargo fmt --check` and `cargo clippy --all-targets -- -D warnings` are clean
- [ ] Every `pub` item documented; spec, CHANGELOG and CLAUDE.md updated where they change
