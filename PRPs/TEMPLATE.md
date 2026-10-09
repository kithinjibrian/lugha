<!-- Save as PRPs/prp-{NNN}-{feature_name}.md — next unused 3-digit number, snake_case name -->

## FEATURE: [one sentence]

## OBJECTIVE
[2–3 sentences describing what "done" looks like from a user perspective]

## CONTEXT

- Starting state: [which files currently exist and are relevant]
- Ending state: [which files will be created or modified]
- Related existing code: [specific file paths to read before starting]
- Related source files: [docs/source/... notes that shaped this feature, if any]
- Open decisions that must be resolved first: [DECISIONS.md entries that block this feature]

## IMPLEMENTATION REQUIREMENTS

### Must Do
- [specific requirement]

### Must NOT Do
- [explicit exclusion — and why]

### New dependencies
- [crate — version — why it is needed — license] (or "none")

## ERROR HANDLING REQUIREMENTS

- [Error enum variants this feature returns, and when]
- [Errors it may ignore, and why]
- [Any `.expect()` and the invariant it asserts]

## SECURITY CONSIDERATIONS

- [Input validation — what must be checked before processing]
- [Any `unsafe`, crypto, or credential handling — requires human review before merging]
- [Data that must never be logged]

## TESTS TO WRITE

List the test cases before implementation begins:
- [ ] Happy path: [describe]
- [ ] Error path: [one per error variant]
- [ ] Edge case: [describe]

## ROLLBACK PLAN

- Branch to return to: [branch name]
- State the codebase should be in: [describe]

## ACCEPTANCE CRITERIA
- [ ] [testable criterion]
- [ ] All existing tests pass
- [ ] New tests written and passing
- [ ] `cargo clippy --all-targets -- -D warnings` passes
- [ ] `cargo fmt --check` passes
- [ ] Every `pub` item has a doc comment
- [ ] CHANGELOG.md updated

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- [any feature-specific check]
