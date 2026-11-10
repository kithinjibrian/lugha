# Discovery Interview Protocol

Use when a feature request arrives without a PRP: any time a feature is described in a sentence or two without saying what it produces, how it fails, what it touches, and which open decisions it depends on.

## Questions

Ask one question at a time and wait for the answer. Every question comes with a recommended answer, the reason for it, and the realistic alternatives, so the user can accept with one word (the AskUserQuestion tool, the recommendation first). Skip what the request or the code already settles.

Cover, in order:
1. What it does: the syntax, call or command, and what it produces
2. Who uses it and when
3. How it fails: the diagnostic or error on each failure path
4. Edge cases: empty, huge and invalid input, concurrency
5. Which existing files it reads or changes, and what it must never touch
6. Which open entries in DECISIONS.md it depends on
7. Security: external input, `unsafe`, new dependencies, running programs or `git`
8. How success is verified: which tests and commands prove it

## After the interview

Write the PRP to `PRPs/prp-{NNN}-{feature_name}.md` from `TEMPLATE.md`, present it, and wait for explicit approval before writing any code.
