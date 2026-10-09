# Discovery Interview Protocol

Use when a feature request arrives without a PRP.

## When to Run a Discovery Interview

Any time a feature is described in one or two sentences without specifying:
- What triggers it and what it produces
- Who is affected and how
- What the error and edge-case behavior should be
- Which existing files it touches, and what it must not touch
- Which open decisions in DECISIONS.md are relevant

## Question Sequence

Ask one question at a time. Do not batch questions. Wait for each answer.

1. What does it do — input, processing, output
2. Who uses it and when
3. What happens when it fails — the error returned on each failure path
4. Edge cases — empty input, huge input, invalid input, concurrency
5. Which existing files it reads from or writes to
6. What it must never modify
7. Which open entries in DECISIONS.md it depends on
8. Security implications — external input, `unsafe`, new dependencies
9. What rollback looks like if it is abandoned
10. How success is verified — which commands prove it works

## After the Interview

Write the PRP to `/PRPs/prp-{NNN}-{feature_name}.md` using `TEMPLATE.md`. `NNN` is the next unused three-digit number; `feature_name` is lowercase snake_case (e.g. `prp-002-type_checker.md`).
Present it to the user. Wait for explicit approval before writing any code.

## PRP Quality Check

- [ ] Described in user-visible behavior, not implementation
- [ ] Every existing file it will touch is listed
- [ ] "Must NOT do" covers the common wrong approaches
- [ ] Every failure path names the error variant returned
- [ ] Security considerations filled in
- [ ] Test cases listed
- [ ] Rollback plan specified
- [ ] Blocking DECISIONS.md entries listed
- [ ] Acceptance criteria verifiable by command or specific check
