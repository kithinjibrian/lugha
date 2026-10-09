# Repo Setup Guide for AI Coding Assistants

Give this document to any LLM at the start of a new project. It describes how to set up a context engineering system that keeps AI coding sessions consistent, prevents drift, and maintains state across sessions.

---

## What You Are Setting Up

Ten interlocking files that form the complete information environment every AI session operates in:

| File | Purpose | Changes |
|------|---------|---------|
| `CLAUDE.md` | Permanent behavioral rules — what to do, what never to do | Rarely |
| `MEMORY.md` | Resolved architectural decisions with rationale | When significant decisions are made |
| `CONTEXT.md` | Session handoff log — what happened, what is open, what is next | Every session |
| `DECISIONS.md` | Pending decisions — open questions that block or fork the design | When a decision is identified, resolved, or deferred |
| `CHANGELOG.md` | Human-readable history of what shipped | When something is completed and merged |
| `.llmignore` | Files the LLM must never read or modify | When protected files are identified |
| `PRPs/` | Per-feature task context — one file per feature before any code is written | Per feature |
| `docs/source/` | Raw human context — meeting notes, stakeholder direction, research — distilled into files the LLM reads | When a meeting, decision, or research finding affects the project |
| `docs/DESIGN.md` | Design tokens and component rules (if UI exists) | When visual system changes |
| `docs/CODE_STYLE.md` | Code documentation rules and patterns the LLM must follow | When style decisions are made |

---

## Step 1 — Create CLAUDE.md

`CLAUDE.md` is loaded automatically at the start of every session. Every line must answer: *"What mistake does this prevent?"* Do not write documentation. Write behavioral constraints.

Keep it focused. A precise 400-line file outperforms a sprawling 1,500-line file. Add rules only when you observe a consistent gap — not pre-emptively.

### Minimum Required Sections

```markdown
# CLAUDE.md — [Project Name]

Behavioral instructions for AI coding assistants. Each rule exists to prevent a specific mistake.

---

## SESSION HANDOFF RULE — NON-NEGOTIABLE

Every session in `CONTEXT.md` must have:
- A **name** — short descriptive title of what the session accomplished
- A **state** — `open` while work is in progress, `closed` once handoff is done
- A **branch** — the git branch this session's work lives on

Format:
  ## SESSION {n} — {YYYY-MM-DD} — {Name} — {state}
  Branch: {branch-name}

Rules:
- **Step 1 of every session, no exceptions:** append a new session entry to `CONTEXT.md` with state `open` and the current branch name. Do this before reading any other file, before planning, before writing any code. The entry must exist and be committed before any other work begins.
- Mark it `closed` only after CONTEXT.md is updated and committed and pushed.
- Never leave a session `open` at the end of a turn.
- Never start a new session without closing the previous one first.
- The NEXT SESSION START POINT block is always rewritten at the end of every session.
- Sessions are never deleted — the full history stays in this file.

The open entry looks like this — write it immediately:

  ## SESSION {n} — {YYYY-MM-DD} — {Name} — open
  Branch: {branch-name}

Replace `{Name}` with a short title for what this session intends to do.
Commit this entry before proceeding.

---

## PRP RULE — NON-NEGOTIABLE

Never write code for a new feature without a PRP file in /PRPs.

If a feature request is given without a PRP:
1. Do not write any code.
2. Run a discovery interview — one question at a time.
3. Cover: what it does, who it affects, edge cases, error states, what files it touches, what it must not touch.
4. Write the PRP to /PRPs/[feature-name].md.
5. Present it to the user for approval.
6. Only build after explicit approval.

A vague prompt is not a starting point. It is the beginning of a discovery.

---

## SCOPE RULE — NON-NEGOTIABLE

One PRP at a time. Never implement more than one feature's scope in a single session.

If mid-implementation you discover the scope is larger than the PRP described:
1. Stop immediately. Do not continue implementing.
2. Document what was discovered.
3. Update or create a new PRP for the expanded scope.
4. Get approval before continuing.

The model does not decide that something is "small enough to add." The human decides.

---

## TESTING RULE — NON-NEGOTIABLE

Write the test before writing the implementation. No exceptions.

Rules:
- For every new function, write a failing test first. Then write the minimum code to make it pass.
- Tests live in [test directory — fill in]. Mirror the source tree: `src/users/user.service.ts` → `tests/users/user.service.test.ts`.
- What to test: behavior visible to callers — inputs, outputs, and error paths.
- What NOT to test: implementation internals, private functions, third-party library behavior, framework wiring.
- Every new exported function must have at least one test covering its happy path and one covering each error path in its Result type.
- After any non-trivial change, run the full test suite before considering the task done.

If the PRP does not describe what to test, add the test cases to the PRP before writing any code.

---

## SECURITY RULE — NON-NEGOTIABLE

Never implement the following without explicit human review and approval:
- Authentication or session management
- Authorization / permission checks
- Cryptographic operations (hashing, signing, encrypting)
- Payment processing
- Secrets or credential handling

For everything else:
- Never hardcode credentials, API keys, tokens, or secrets. Not even in comments or example values.
- Never construct SQL queries by string concatenation. Use parameterized queries or the ORM's query builder.
- Never trust input from the user, URL params, or external APIs without validation first.
- Never log sensitive data (passwords, tokens, PII).
- Never expose internal error details to the client — map them to a generic user-facing message.
- When generating code that accepts external input, add an input validation step before any processing.

If you are unsure whether something has a security implication, stop and ask before implementing.

---

## DESIGN RULE — NON-NEGOTIABLE

Read `docs/DESIGN.md` before writing any UI code.

Every colour, spacing value, font size, border radius, and shadow must come from the design system defined there. Never hardcode a colour, spacing value, or font size. Never use framework arbitrary values. Only use the design tokens defined in `docs/DESIGN.md`.

---

## CODE DOCUMENTATION RULE — NON-NEGOTIABLE

Read `docs/CODE_STYLE.md` before writing any function, class, or module.

Every exported function, class, and type must have a documentation block. Every non-obvious decision inside a function must have an inline comment explaining *why*, not what. The what is in the code. The why is what degrades when context is lost.

---

## PENDING DECISIONS RULE — NON-NEGOTIABLE

Before writing any code that depends on an unresolved architectural question, check `DECISIONS.md`.

- If the decision is listed as `open`, stop. Do not implement. Ask the user to resolve it first.
- If the decision is listed as `resolved`, follow the outcome recorded there — do not re-litigate it.
- If you encounter a new unresolved question mid-implementation, add it to `DECISIONS.md` as `open` and stop. Do not guess.

Never make an architectural choice silently. If you are guessing, you are making a decision that belongs in DECISIONS.md.

---

## FILE SIZE RULE

No file exceeds [N] lines. 300 is a reasonable default; set the number that fits your project.

When a file reaches the limit:
1. Stop before adding more code.
2. Propose a split to the user — show the proposed new file names and what moves where.
3. Wait for approval.
4. Split, then continue.

Do not ask "should I split this?" — propose the specific split.

---

## PROTECTED FILES — NON-NEGOTIABLE

Never read, modify, or delete the following files or directories under any circumstances:

- `.env` and `.env.*` (any environment file)
- `package-lock.json`, `yarn.lock`, `pnpm-lock.yaml` (lockfiles — managed by the package manager)
- `*.migration.ts` and `migrations/` (database migration history)
- `[add any other project-specific protected paths]`

If a task seems to require touching a protected file, stop and ask the user how to proceed.

See also: `.llmignore` at the project root.

---

## COMMANDS

```bash
# Fill in your actual commands — every one of these must work
[dev server command]
[test command]
[typecheck / lint command]
[build command]
[database migration command]
```

Run typecheck and tests after every non-trivial change. Do not consider a task done until both pass.

---

## STACK

- [List every technology with the version that matters]
- [Note any constraint on how each is used]

---

## ARCHITECTURE RULES

[List the structural decisions that govern the whole codebase. Each rule should say what to do AND why deviating will break something.]

Examples of what belongs here:
- What shape every API response takes and which error variant is used (see ERROR HANDLING below)
- Where errors are caught and how they are returned
- Which layer is allowed to query the database
- Where shared types live
- What is and is not allowed in components vs services

---

## ERROR HANDLING — NON-NEGOTIABLE

This project uses **return-based error handling** (Result/union types). Do not throw exceptions for expected failures.

### The Rule

```
Expected failure  →  return { data: null, error: ... }
Truly unexpected  →  let it throw (programmer error, unrecoverable state)
```

### Why

TypeScript has no checked exceptions. A function that throws gives callers no type-level signal that failure is possible. A function that returns a Result type makes every failure path visible, compiler-checked, and impossible to silently ignore. Exceptions also carry a measurable performance cost when used for control flow.

### The Pattern

```typescript
// The Result type — define once in lib/result.ts
type Result<T, E = AppError> =
  | { ok: true;  data: T; error: null }
  | { ok: false; data: null; error: E }

function ok<T>(data: T): Result<T, never> {
  return { ok: true, data, error: null }
}

function err<E>(error: E): Result<never, E> {
  return { ok: false, data: null, error }
}

// Usage — service layer
async function getUser(id: string): Promise<Result<User, "NOT_FOUND" | "DB_ERROR">> {
  try {
    const user = await db.users.findUnique({ where: { id } })
    if (!user) return err("NOT_FOUND")
    return ok(user)
  } catch {
    return err("DB_ERROR")
  }
}

// Usage — caller is forced to handle both paths
const result = await getUser(id)
if (!result.ok) {
  // result.error is typed — compiler knows what it can be
  return handleError(result.error)
}
// result.data is typed User here
```

### What Goes Where

| Layer | Rule |
|-------|------|
| Service / domain | Returns `Result<T, E>`. All try/catch lives here. |
| Controller / route handler | Calls service, maps Result to HTTP response. No try/catch. |
| UI component | Calls service, reads `result.ok` to decide what to render. |
| Third-party library calls | Wrapped in a thin adapter that converts throws into `err(...)`. |

### What Still Throws

- Programmer errors: accessing a property on null when it should never be null (bugs, not expected failures — let them surface)
- Process-level failures: database connection at startup, missing env vars
- Framework internals: do not catch framework exceptions that signal misconfiguration

### Error Handling Anti-Patterns

- **Never** `throw new Error(...)` in a service function for a predictable failure.
- **Never** `try/catch` in a controller — errors from the service layer come back as typed Results.
- **Never** return `undefined` to signal failure — the caller cannot distinguish "not found" from "returned nothing on purpose."
- **Never** expose a raw error message from a library or the database to the client.

---

## FILE ORGANIZATION

```
[Paste your actual directory tree with one-line descriptions]
```

---

## ANTI-PATTERNS

[Number these. Be specific. Include the reason after each one.]

1. **Never [specific thing].** [Why — what breaks if you do it.]
2. **Never [specific thing].** [Why.]
...

---

## KNOWN ISSUES — DO NOT FIX

[List anything that looks like a bug but is intentional. Include the reason.]
```

### What Makes a Rule Good vs Bad

| Bad (model ignores this) | Good (model acts on this) |
|--------------------------|--------------------------|
| "Write clean code" | "Max 300 lines per file. Propose a split before exceeding it." |
| "Use the Button component correctly" | "Use `<Button variant='outline' size='sm'>`. Never use raw `<button>` HTML." |
| "Follow best practices" | "Never throw in service functions — catch all errors and return `err(message)`" |
| "Handle errors properly" | "Errors are caught in the service layer only. Controllers contain no try/catch." |
| "Document your code" | "Every exported function needs a JSDoc block with `@param`, `@returns`, and one `@example`." |
| "Be careful with security" | "Never construct SQL by string concatenation. Never log tokens or passwords." |
| "Write tests" | "Write the failing test before writing the implementation. Every error path in the Result type gets its own test." |

---

## Step 2 — Create MEMORY.md

`MEMORY.md` records the architectural decisions you have already made, with the reasons, and what each decision rules out. It prevents the model from re-litigating settled questions or introducing patterns you already considered and rejected.

This is the record of *resolved* decisions. Open and pending decisions live in `DECISIONS.md`.

```markdown
# MEMORY.md — [Project Name]

Records resolved architectural decisions and current project state.
Read this at the start of every session before writing any code.

---

## ARCHITECTURAL DECISIONS

### 1. [Decision Name]

**Decision:** [What was decided, in one or two sentences]

**Why:** [The reason this was chosen — what problem it solves, what property it preserves]

**Rules out:** [What approaches are now off the table because of this decision]

---

### 2. [Decision Name]

[Repeat for each significant architectural decision]

---

## CURRENT PROJECT STATE

### Fully Working
- [List what is done and verified]

### In Progress
- [List what is being built right now]

### Not Started
- [List what is planned but untouched]

---

## NEXT SESSION START POINT

[One paragraph: what to read first, what to build, which files to look at before writing anything]
```

### When to Add to MEMORY.md

Add an entry every time a DECISIONS.md entry moves from `open` → `resolved`. Also add entries for:
- Choosing a library over an alternative
- Deciding on an API shape or response structure
- Rejecting a pattern and settling on a different one
- Making a performance or security trade-off
- Deciding how data flows between layers

Do not add entries for things that are obvious from reading the code. Do not add open questions here — those go in DECISIONS.md.

---

## Step 3 — Create CONTEXT.md

`CONTEXT.md` is the session handoff log. It is written by the AI at the end of every session and read at the start of the next. It is the answer to: *"What happened last time and where do I start?"*

```markdown
# CONTEXT.md — [Project Name]

Session handoff file. Updated at the end of every session.
Read at the start of the next session alongside CLAUDE.md, MEMORY.md, and DECISIONS.md.

Every session has a name and a state: open | closed.
A session is closed only after CONTEXT.md is committed and pushed.

---

## SESSION 1 — [YYYY-MM-DD] — [Name] — closed

Branch: [branch-name]

### WHAT WAS DONE

[Prose summary of what the session accomplished]

### FILES CREATED OR MODIFIED

```
path/to/file.ts   — one-line description of what changed
path/to/file.html — one-line description of what changed
```

### TESTS WRITTEN

- [test file] — [what behavior is covered]

### DECISIONS MADE

- [Decision made during the session and why]

### PENDING DECISIONS OPENED

- [Any new open question added to DECISIONS.md this session]

### STILL OPEN AT CLOSE

- [What was not finished]
- [What was deferred]

---

## NEXT SESSION START POINT

[Exact instructions: what to read first, what to build, which files to open, any known gotchas]
```

### The Session Handoff Protocol

**At session start, paste this:**
```
Before anything else: append a new session entry to CONTEXT.md with state `open` and the current branch name. Commit it. Do not read any other file or write any code until this is done.

Then read CLAUDE.md, MEMORY.md, DECISIONS.md, and CONTEXT.md — in that order.
Confirm you've read them by summarizing: current stack, last thing built, any open decisions blocking today's work, and what we're doing this session.
Then: [task or PRP reference]
```

**At session end, paste this:**
```
Before we finish: write a session handoff to CONTEXT.md covering:
1. What we accomplished (prose summary)
2. Every file modified or created (with one-line description of the change)
3. Tests written and what behavior they cover
4. Decisions made and why
5. Any new pending decisions added to DECISIONS.md
6. What is still open or broken
7. The exact next step for the next session

Then update CHANGELOG.md with anything that shipped.
Then mark this session `closed`, commit, and push.
```

### When to Kill a Session and Start Fresh

Context rot is real. End the session and start a fresh one when:

- The model recommends something that contradicts an earlier decision — without noticing the contradiction
- It hallucinates method names or API signatures that don't exist
- It suggests a library you already use as if it's new
- It reverts a change it made earlier in the session
- Responses become vague, generic, or heavily hedged
- It starts touching files you never mentioned
- It implements something without checking DECISIONS.md for open questions

End the session. Write the handoff. Fresh context is always better than degraded context.

---

## Step 4 — Create DECISIONS.md

`DECISIONS.md` is the pending decisions register. It is distinct from `MEMORY.md` (which records *resolved* decisions) and distinct from `PRPs` (which are feature briefs). It is the answer to: *"What architectural questions are still open, and what did we decide when they were resolved?"*

**Why this file exists:** LLMs will make silent assumptions when they hit an architectural fork. They do not flag the choice — they just pick one and continue. `DECISIONS.md` turns silent guesses into explicit questions that require human input before code is written.

```markdown
# DECISIONS.md — [Project Name]

Tracks architectural and design questions that are open, deferred, or resolved.

Rules:
- Every open decision blocks implementation of the code it affects.
- The AI must not implement anything that depends on an open decision.
- When a decision is resolved, move it to the RESOLVED section and record the outcome.
- Once resolved, copy the outcome to MEMORY.md as an architectural decision.

---

## OPEN — Requires human input before implementation

### DECISION-001 — [Short title]

**Status:** open
**Raised:** [YYYY-MM-DD] — [Session N]
**Resolved by:** human | technical constraint check | [role]
**Blocks:** [What cannot be built until this is answered]

**Question:** [The exact question that needs answering]

**Options:**
- A) [Option with tradeoffs]
- B) [Option with tradeoffs]

**Notes:** [Any context the human should know before deciding]

---

## DEFERRED — Acknowledged, not yet needed

### DECISION-002 — [Short title]

**Status:** deferred
**Raised:** [YYYY-MM-DD] — [Session N]
**Revisit when:** [The condition that makes this decision urgent]

**Question:** [The exact question]

**Notes:** [Why it is safe to defer]

---

## RESOLVED

### DECISION-003 — [Short title]

**Status:** resolved
**Raised:** [YYYY-MM-DD] — [Session N]
**Resolved:** [YYYY-MM-DD] — [Session N]

**Question:** [The original question]

**Outcome:** [What was decided]

**Rationale:** [Why]

**Copied to MEMORY.md:** yes
```

### When to Add to DECISIONS.md

Add a decision entry any time you encounter:
- A question about architecture where two reasonable approaches exist and the choice has long-term consequences
- An API shape that affects multiple features (change it later = breaking change)
- A data modeling choice (database schema, state shape, type structure)
- A library or tool where alternatives exist and you are not certain which the project has committed to
- A performance or security trade-off
- Any question where the AI is about to pick one approach silently

Do not add entries for questions with obvious answers or questions the codebase already answers.

### Decision Lifecycle

```
New question → OPEN (blocks code)
          ↓
   Human answers → RESOLVED (outcome recorded, copied to MEMORY.md)

   OR

New question → OPEN
          ↓
   Not urgent yet → DEFERRED (has a revisit condition)
          ↓
   Condition met → OPEN again → RESOLVED
```

---

## Step 5 — Create CHANGELOG.md

`CHANGELOG.md` is the human-readable history of what shipped. It is distinct from `CONTEXT.md` (which is the AI-facing session log) and `reports/` (which are daily human summaries). It is the answer to: *"What changed in this codebase, and when?"*

```markdown
# CHANGELOG — [Project Name]

Follows [Keep a Changelog](https://keepachangelog.com) format.
Updated at the end of every session when something is completed and merged.
Never deleted. Older entries are never modified.

---

## [Unreleased]

### Added
- [Feature or capability added]

### Changed
- [Existing behavior that changed]

### Fixed
- [Bug fixed]

### Removed
- [Something removed]

---

## [0.1.0] — YYYY-MM-DD

### Added
- Initial project setup
```

### Rules

- Update CHANGELOG.md at the end of every session when something ships — not at release time.
- "Unreleased" accumulates completed work. On release, rename it to the version number and date, and open a new Unreleased block.
- Entries are written for humans, not commit messages. "Added user authentication" not "feat: add auth middleware."
- Never log internal refactors that don't change observable behavior. If the user can't see or feel the change, it doesn't go here.
- One line per item. If it needs more than one line to explain, the change is either two changes or belongs in a spec.

### When the LLM Updates It

At session end, after writing the CONTEXT.md handoff, the LLM appends any completed items to the `[Unreleased]` section. The prompt at session end includes: *"Update CHANGELOG.md with anything that shipped this session."*

---

## Step 6 — Create .llmignore

`.llmignore` declares files and directories the LLM must never read, modify, or delete. It is the machine-readable equivalent of the PROTECTED FILES section in CLAUDE.md, and the two must stay in sync.

```
# .llmignore
# Files and directories the AI must never touch.
# Keep this in sync with the PROTECTED FILES section in CLAUDE.md.

# Environment and secrets
.env
.env.*
.env.local
.env.production

# Dependency lockfiles — managed by the package manager, not the AI
package-lock.json
yarn.lock
pnpm-lock.yaml

# Database migration history — order is sacred, never regenerate
migrations/
*.migration.ts
*.migration.js

# Build output — generated, not source
dist/
build/
.next/
out/

# [Add any other project-specific files the AI must not touch]
```

**Rules:**
- Every entry in `.llmignore` must also appear in the PROTECTED FILES section of CLAUDE.md. One file is machine-readable, one is human-readable for the LLM.
- If the LLM's task seems to require modifying a protected file, it stops and asks the human how to proceed. It does not attempt a workaround.

---

## Step 7 — Set Up the PRPs Folder

A PRP (Product Requirements Prompt) is a structured task brief written specifically for a coding agent. Its goal is one-pass implementation success — the agent implements the feature correctly without asking clarifying questions mid-build.

Create `PRPs/TEMPLATE.md`:

```markdown
## FEATURE: [one sentence]

## OBJECTIVE
[2–3 sentences describing what "done" looks like from a user perspective]

## CONTEXT

- Starting state: [which files currently exist and are relevant]
- Ending state: [which files will be created or modified]
- Related existing code: [specific file paths to read before starting]
- Open decisions that must be resolved first: [list any DECISIONS.md entries that block this feature]

## IMPLEMENTATION REQUIREMENTS

### Must Do
- [specific requirement]
- [specific requirement]

### Must NOT Do
- [explicit exclusion — be specific about why]
- [explicit exclusion]

## ERROR HANDLING REQUIREMENTS

- [Which errors this feature must surface and how]
- [Which errors it can silently ignore and why]
- [What the caller receives on each failure path — use the project's Result type]

## SECURITY CONSIDERATIONS

- [Input validation requirements — what must be validated before processing]
- [Auth requirements — which routes/functions require authentication]
- [Data exposure risks — what must never appear in logs or client responses]
- [If any of the restricted categories apply (auth, crypto, payments), note that human review is required before merging]

## TESTS TO WRITE

List the specific test cases before any implementation begins:
- [ ] Happy path: [describe]
- [ ] Error path: [describe each Result error variant]
- [ ] Edge case: [describe]

## ROLLBACK PLAN

If this feature needs to be abandoned mid-implementation:
- Branch to return to: [branch name]
- Migration to reverse: [migration name, or "none"]
- State the codebase should be in: [describe]

## ACCEPTANCE CRITERIA
- [ ] [testable criterion]
- [ ] [testable criterion]
- [ ] All existing tests pass
- [ ] New tests written and passing
- [ ] Typecheck passes
- [ ] No undocumented exports (every export has a JSDoc block)
- [ ] CHANGELOG.md updated

## VALIDATION
Run these commands to verify completion:
- [typecheck command]
- [test command]
- [any feature-specific check]
```

Create `PRPs/DISCOVERY.md`:

```markdown
# Discovery Interview Protocol

Use when a feature request arrives without a PRP.

## When to Run a Discovery Interview

Any time a feature is described in one or two sentences without specifying:
- What triggers it and what it produces
- Who is affected and how
- What the error and edge-case behavior should be
- Which existing files it touches
- What it must not touch
- Which open decisions in DECISIONS.md are relevant

## Question Sequence

Ask one question at a time. Do not batch questions. Wait for the answer before continuing.

Cover in order:
1. What does it do — input, processing, output
2. Who uses it and when
3. What happens when it fails — user-facing error on each failure path
4. Edge cases — empty states, concurrent requests, invalid input
5. Which existing files it reads from or writes to
6. What it must never modify
7. Are there any open entries in DECISIONS.md this feature depends on?
8. What are the security implications — does it accept external input, touch auth, handle payments?
9. What does rollback look like if this needs to be abandoned?
10. How success is verified — what commands prove it works?

## After the Interview

Write the completed PRP to `/PRPs/[feature-name].md` using the template.
Present it to the user.
Wait for explicit approval before writing any code.
```

### PRP Quality Check

A good PRP eliminates guessing. Before approving one, verify:

- [ ] The feature is described in terms of user-visible behavior, not implementation
- [ ] Every existing file the implementation will touch is listed
- [ ] "Must NOT do" covers the most common wrong approaches for this type of feature
- [ ] Error handling requirements describe what the caller receives on every failure path
- [ ] Security considerations are filled in — not left blank
- [ ] Test cases are listed before implementation starts
- [ ] Rollback plan is specified
- [ ] Open decisions in DECISIONS.md that block this feature are listed
- [ ] Acceptance criteria are testable — each one can be verified with a command or specific check
- [ ] The validation section has runnable commands, not just "test it"

---

## Step 8 — Create docs/CODE_STYLE.md

`docs/CODE_STYLE.md` governs how code is documented. The LLM reads this before writing any function, class, or module. Without it, documentation style will drift across sessions and the model will default to either too little (no comments) or too much (comments that describe what the code does instead of explaining why decisions were made).

```markdown
# Code Style — [Project Name]

Documentation rules for all code in this project.
Read this before writing any function, class, or module.

---

## The Two Layers of Documentation

### Layer 1 — Block documentation (what this is)

Every exported function, class, interface, and type alias gets a documentation block.
Internal (non-exported) functions get a block only if their purpose is not immediately obvious.

**Format (TypeScript/JavaScript):**

  /**
   * [One-sentence description of what this does from the caller's perspective.]
   *
   * [Optional second paragraph: when to use it, what to watch out for.]
   *
   * @param name - [What this param is. Include units, constraints, allowed values.]
   * @param options - [What the options object controls]
   * @returns [What is returned. For Result types, describe both ok and error paths.]
   * @throws [Only if this function is one of the deliberate exceptions to the Result rule]
   *
   * @example
   * const result = await getUser("user_123")
   * if (!result.ok) { ... }
   */

Rules:
- The first line is always a single sentence. No "This function...". Start with the verb: "Fetches", "Creates", "Returns", "Validates".
- `@param` and `@returns` are required on every exported function. No exceptions.
- One `@example` is required on every public API function. Not optional.
- `@throws` is used only for the deliberate exceptions to the Result pattern (programmer errors, startup failures). Do not use it on service functions.

### Layer 2 — Inline comments (why this decision was made)

Inline comments explain decisions, not code.

**Good:** `// Retry once — the upstream API returns 429 on cold start ~20% of the time`
**Bad:** `// Increment counter by 1`

Rules:
- Comment above the line it explains, not at the end.
- Use inline comments when: a magic number appears, a library is used non-obviously, a performance trade-off was made, a guard clause prevents a non-obvious bug, a workaround exists for a known issue.
- Never comment what the code does. If the code needs a comment to explain what it does, the code should be rewritten to be self-explanatory.
- Known issues get a comment with a ticket reference: `// TODO(#123): Remove after upstream fixes their pagination`

---

## Module-Level Documentation

Every file gets a top-of-file comment block:

  /**
   * [Module name]
   *
   * [One paragraph: what this module is responsible for and what it is NOT responsible for.]
   *
   * Depends on: [other modules this one imports from]
   * Used by: [other modules that import from this one — omit if it is a leaf module]
   */

---

## What Good Documentation Looks Like

  /**
   * user.service.ts
   *
   * All user read/write operations. Does not handle auth — auth lives in auth.service.ts.
   *
   * Depends on: lib/db, lib/result, types/user
   * Used by: user.controller, auth.service
   */

  /**
   * Fetches a single user by ID.
   *
   * Returns NOT_FOUND if the user does not exist.
   * Returns DB_ERROR if the database query fails — the caller should not retry automatically,
   * as DB_ERROR indicates a connection-level failure, not a transient issue.
   *
   * @param id - The user's UUID. Must be a valid UUIDv4.
   * @returns Result<User, "NOT_FOUND" | "DB_ERROR">
   *
   * @example
   * const result = await getUser("550e8400-e29b-41d4-a716-446655440000")
   * if (!result.ok) return respondWithError(result.error)
   * return respond(result.data)
   */
  export async function getUser(id: string): Promise<Result<User, "NOT_FOUND" | "DB_ERROR">> {
    // Validate before hitting the DB — avoids a round-trip on obviously bad input
    if (!isUUID(id)) return err("NOT_FOUND")

    try {
      const user = await db.users.findUnique({ where: { id } })
      if (!user) return err("NOT_FOUND")
      return ok(user)
    } catch {
      return err("DB_ERROR")
    }
  }

---

## What Bad Documentation Looks Like — Do Not Write This

  // BAD: no block, no params, no example
  export async function getUser(id: string) {
    const user = await db.users.findUnique({ where: { id } })
    return user  // BAD: throws on DB failure, returns undefined on not-found
  }

  // BAD: describes what, not why
  // Find the user in the database by their ID
  // If found, return the user
  // If not found, return null

---

## Documentation Anti-Patterns

1. **Describing the code.** The code is the description. Comments explain what the code cannot.
2. **Stale comments.** A comment that contradicts the code is worse than no comment. If you change code, update its comment in the same edit.
3. **`// TODO` without a ticket.** Undated, untracked TODOs accumulate and rot. Every TODO gets a ticket reference or a date.
4. **Over-documenting internals.** Not every helper function needs a block. Obvious private helpers do not.
5. **Under-documenting the error contract.** Every exported function's `@returns` must describe the failure paths. This is the most important part of the documentation for callers.
```

---

## Step 9 — Create the Design System (if UI exists)

Create `docs/DESIGN.md` before writing any UI code. This file is the single source of truth for every visual decision.

```markdown
# Design System — [Project Name]

## Colour Tokens

  --color-bg-primary: [value];
  --color-bg-secondary: [value];
  --color-text-primary: [value];
  --color-text-secondary: [value];
  --color-border: [value];
  --color-accent: [value];
  --color-error: [value];
  --color-success: [value];

## Typography

- Base font: [font name and source]
- Scale: [list sizes with token names — e.g. `--text-sm: 0.875rem`]
- Line heights: [list with token names]
- Weight: [list with token names]

## Spacing

[List spacing tokens — e.g. `--space-4: 1rem`]

## Border Radius

[List radius tokens]

## Shadows

[List shadow tokens]

## Component Rules

[For each reusable component, describe its allowed variants and states. Include a code example
showing correct usage and one showing what NOT to do.]

## What Is Forbidden

- No hardcoded hex values, pixel values, or rem values outside this file
- No arbitrary values in utility-class frameworks
- No inline styles
```

---

## Step 10 — Create docs/source/

`docs/source/` is the project's raw human context layer. It captures everything that shapes how the project grows but lives outside the code: meeting notes, stakeholder direction, user research, competitor analysis, product pivots, and constraint changes. Without it, this information either never reaches the LLM, or it gets buried in session notes and evaporates after one session.

**Why this is a separate folder and not MEMORY.md or DECISIONS.md:**

MEMORY.md records *resolved* architectural decisions in a structured format. DECISIONS.md tracks *pending* questions. `docs/source/` holds the *raw material* — the actual conversations, findings, and directives that eventually produce entries in those files. It is the input layer; they are the output layer.

The LLM does not automatically read `docs/source/`. Files from it are referenced explicitly when relevant — in PRPs, at session start, or when a decision needs context. This keeps the context window clean while making the information retrievable.

### Folder Structure

```
docs/source/
├── meetings/          ← Notes from any meeting that affects the project
│   └── YYYY-MM-DD-[slug].md
├── research/          ← User research, competitor analysis, market findings
│   └── [topic].md
├── stakeholder/       ← Direction from clients, founders, product owners
│   └── [topic]-[YYYY-MM-DD].md
└── constraints/       ← External constraints: legal, compliance, platform limits
    └── [topic].md
```

### Meeting Notes Template

```markdown
# Meeting — [YYYY-MM-DD] — [Short Title]

**Attendees:** [who was in the room]
**Type:** planning | review | stakeholder | research | incident

---

## What Was Discussed

[Prose summary — what topics were covered. Not a transcript. Not bullet fragments.
One paragraph per major topic.]

## Decisions Made

- [Decision made in this meeting — the actual outcome, not "we talked about X"]
- [Each decision that affects the project goes here]

## Constraints Introduced

- [Any new constraint on scope, timeline, technology, or design]
- [If none, omit this section]

## Open Questions Raised

- [Questions that came up but weren't resolved]
- [These should become entries in DECISIONS.md]

## Action Items

- [ ] [Person] — [What they will do] — by [date]

## Impact on the Project

[One paragraph: what changes as a result of this meeting. What should the LLM know
the next time it reads context? What PRP needs updating? What DECISIONS.md entry
should be opened?]
```

### Research / Stakeholder Notes Template

```markdown
# [Type]: [Topic] — [YYYY-MM-DD]

**Source:** [who provided this — user interview, client brief, desk research, etc.]
**Reliability:** high | medium | low
**Expires:** [date after which this should be re-verified, or "stable"]

---

## Summary

[2–3 sentences: what this source tells us and why it matters to the project.]

## Key Findings

- [Finding — stated as a fact or observation, not as a recommendation]
- [Finding]

## Implications for the Project

[What this means for what we build, how we build it, or what we prioritise.
This is the section the LLM is most likely to need.]

## Conflicts With

[Any existing decisions, assumptions, or plans in MEMORY.md or DECISIONS.md
that this finding contradicts or complicates. If none, omit.]

## Raw Notes

[Optional: paste verbatim excerpts, quotes, or raw data here for traceability.]
```

### Constraints Template

```markdown
# Constraint: [Name] — [YYYY-MM-DD]

**Type:** legal | compliance | platform | technical | contractual | budget
**Enforced by:** [who or what enforces this]
**Verified:** [date last confirmed to be accurate]

---

## The Constraint

[One paragraph: what the constraint is and what it prohibits or requires.]

## What This Rules Out

- [Specific approach or feature that cannot be built because of this]
- [Another ruled-out option]

## What Must Be Done

- [Required action or design choice that follows from this constraint]

## References

[Link to the document, regulation, contract clause, or platform policy that
establishes this constraint.]
```

### How the LLM Uses docs/source/

The LLM never reads `docs/source/` automatically. Files are brought in explicitly:

**At PRP creation:** When writing a PRP for a feature that was discussed in a meeting or shaped by research, reference the relevant source files in the CONTEXT section:
```
- Related source files: docs/source/meetings/2024-03-15-auth-direction.md
```

**At session start:** When the session's work was triggered by a stakeholder conversation or new constraint, paste this into the session-start prompt:
```
Before planning: read docs/source/[file]. It affects what we're building today.
Summarise its implications before starting.
```

**When opening a DECISIONS.md entry:** If a pending decision has relevant research or stakeholder context, add a reference in the Notes field:
```
Notes: See docs/source/research/competitor-auth-patterns.md for context on why
Option B may cause user friction.
```

**When updating MEMORY.md:** When a meeting decision becomes a resolved architectural decision, reference the meeting notes in the rationale:
```
Why: Stakeholder direction from 2024-03-15 meeting (docs/source/meetings/2024-03-15-auth-direction.md)
ruled out third-party auth providers due to data residency requirements.
```

### Rules

- Every meeting that produces a decision, constraint, or open question gets a note. Not every meeting — only ones that affect what gets built or how.
- Notes are written the same day or the session immediately after. Notes written a week later are reconstructions, not records.
- The "Impact on the Project" section is mandatory. A note without it is just an archive. The impact section is what makes the file useful to the LLM.
- If a source note introduces a constraint that conflicts with an existing decision in MEMORY.md, open a DECISIONS.md entry immediately. Do not silently override a recorded decision.
- Source files are never deleted. If a constraint is lifted or a stakeholder direction reverses, add a dated note at the top of the original file: `[SUPERSEDED — see docs/source/stakeholder/[new-file].md]`

### What Belongs in docs/source/ vs Elsewhere

| Information | Goes in |
|-------------|---------|
| "The client said no external auth providers" | `docs/source/stakeholder/` → then DECISIONS.md if it was open, MEMORY.md once resolved |
| "We chose Postgres over MySQL" | MEMORY.md directly — already a resolved decision |
| "Should we use REST or GraphQL?" | DECISIONS.md directly — it's a pending question, not source material |
| "User research shows 70% of users drop off at step 3" | `docs/source/research/` |
| "GDPR requires data residency in the EU" | `docs/source/constraints/` |
| "In today's standup we decided to delay the payment feature" | `docs/source/meetings/` → then update the relevant PRP |

---

## What the Full File Structure Looks Like

```
project-root/
├── .llmignore              ← Files the LLM must never touch
├── CLAUDE.md               ← Behavioral rules (permanent)
├── MEMORY.md               ← Resolved architectural decisions
├── CONTEXT.md              ← Session handoff log (updated every session)
├── DECISIONS.md            ← Pending decisions register
├── CHANGELOG.md            ← Human-readable shipping history
├── PRPs/
│   ├── TEMPLATE.md         ← PRP template
│   ├── DISCOVERY.md        ← Discovery interview protocol
│   └── [feature].md        ← One file per feature, written before code
├── docs/
│   ├── source/             ← Raw human context (read on demand, not automatically)
│   │   ├── meetings/       ← YYYY-MM-DD-[slug].md
│   │   ├── research/       ← [topic].md
│   │   ├── stakeholder/    ← [topic]-[YYYY-MM-DD].md
│   │   └── constraints/    ← [topic].md
│   ├── DESIGN.md           ← Design tokens and component rules (if UI exists)
│   ├── CODE_STYLE.md       ← Documentation rules and patterns
│   ├── specs/              ← Technical specifications
│   ├── decisions/          ← ADRs (ADR-{n}-{slug}.md)
│   ├── incidents/          ← Post-mortem reports
│   └── status/             ← Project status reports
└── reports/                ← EOD reports (YYYY-MM-DD.md)
```

---

## Common Mistakes When Setting This Up

**Mistake: Writing CLAUDE.md like a README**
The model already knows what the project does after reading a few files. Write constraints, not descriptions. Every line should prevent a specific mistake.

**Mistake: Rules without examples**
Abstract rules have weak effect. Show a correct example and a wrong example side by side. This is especially critical for error handling, testing, and documentation patterns — show the exact shape of the code, not a description of it.

**Mistake: Letting CLAUDE.md go stale**
Update it every time the model makes a mistake you didn't anticipate, and every time you make a significant architectural change. Stale context is worse than no context — it actively misleads.

**Mistake: CLAUDE.md that is too long**
A focused, precise CLAUDE.md outperforms a sprawling one. A file that tries to cover everything has lower rule-compliance than one that covers the ten things that actually matter. Add rules only when you observe a consistent gap. Remove rules that no longer apply.

**Mistake: One long session for everything**
Time-box sessions to ~45–60 minutes of focused work. Write a handoff. Start fresh. Context rot sets in silently — responses get vaguer, contradictions appear, hallucinations start. The model's performance degrades as context fills.

**Mistake: Skipping the PRP for "small" features**
Every unanswered question in a task prompt is a decision the model will make for you. Small features have fewer questions, so PRP writing takes five minutes. Skip it and you'll spend twenty minutes undoing the wrong defaults.

**Mistake: Writing code before writing the test**
Without the testing rule enforced in CLAUDE.md, the model will implement first and test as an afterthought — or not at all. Tests written after the fact verify that the implementation exists, not that it behaves correctly. Tests written first verify behavior by definition.

**Mistake: No security rules in CLAUDE.md**
The model will not spontaneously check for injection, hardcoded secrets, or missing input validation. It must be told explicitly. Auth and crypto are especially dangerous — they must be flagged for human review, not generated silently.

**Mistake: Letting architectural questions live in the model's context**
If you discuss an architectural question mid-session but don't write it to DECISIONS.md, it evaporates when the session ends. The next session starts without that context and makes a different silent choice.

**Mistake: Conflating MEMORY.md and DECISIONS.md**
MEMORY.md records what was decided. DECISIONS.md tracks what has not been decided yet. They serve opposite functions. An open question in MEMORY.md is invisible to the rule that blocks implementation.

**Mistake: No protected files list**
Without `.llmignore` and the PROTECTED FILES section in CLAUDE.md, the model will helpfully "fix" a lockfile, regenerate a migration, or tidy up a `.env.example`. These are some of the most destructive silent behaviors.

**Mistake: Treating documentation as optional**
The model generates undocumented code by default when not constrained. Without CODE_STYLE.md, every session produces a different documentation style. Well-documented code is also richer context for the model's own future reads — it helps the model understand the codebase it is operating in.

**Mistake: No CHANGELOG**
Without it, there is no clean record of what shipped and when — only a session log full of AI-internal detail. CHANGELOG.md gives you and your team a one-file answer to "what changed this week?"

**Mistake: Meeting decisions that never reach the codebase context**
A stakeholder says "no third-party auth" in a call. It doesn't get written down. Three sessions later the model generates an OAuth integration and nobody remembers why that was ruled out. `docs/source/` exists to capture this layer. If it isn't written the same day, it is effectively lost — reconstructed notes a week later are unreliable. Every meeting that produces a constraint, decision, or open question needs a note before the session ends.

---

## Ongoing Maintenance

The context system is a living document, not a one-time setup.

- **After every session:** update CONTEXT.md, update CHANGELOG.md if something shipped, commit, push
- **After every significant decision:** add to MEMORY.md; resolve the corresponding DECISIONS.md entry
- **After every meeting or stakeholder conversation that affects the project:** write a note to docs/source/ the same day; open any new DECISIONS.md entries it surfaces
- **After every model mistake:** add a rule to CLAUDE.md
- **After every architectural change:** review CLAUDE.md for stale rules
- **Before every feature:** write a PRP, including test cases and rollback plan; check docs/source/ for relevant context
- **When a question arises mid-session:** add it to DECISIONS.md before continuing

The system compounds. A well-maintained CLAUDE.md means fewer mistakes, which means less time correcting, which means more time building.

---

## Step 11 — End of Day Reports

EOD reports are outward-facing summaries written for a human audience. They are distinct from the session handoff in `CONTEXT.md` (AI-facing, detailed) and `CHANGELOG.md` (permanent, skimmable shipping history).

| File | Audience | Purpose |
|------|----------|---------|
| `CONTEXT.md` | Next AI session | State transfer — where to pick up |
| `CHANGELOG.md` | Team / stakeholders | Permanent shipping record |
| `reports/YYYY-MM-DD.md` | You / your team | Daily summary — what happened today |

Store EOD reports in `reports/` at the project root. Never delete them.

### EOD Report Template

```markdown
# EOD Report — [YYYY-MM-DD]

## What Shipped Today
[Bullet list of completed items. Each item must be verifiable — a passing test, a merged branch,
a working UI state. No vague claims like "made progress on X".]

## What Did Not Ship
[What was attempted and not completed. One sentence per item.]

## Decisions Resolved Today
[Any DECISIONS.md entries that moved from open → resolved. Include the outcome.]

## Decisions Opened Today
[Any new entries added to DECISIONS.md. Include the question and what blocks on it.]

## Blockers
[What is preventing forward movement. Be specific.]

## Tomorrow — First Task
[Single most important task. One sentence. This becomes the opening prompt of the next session.]

## Metrics (optional)
[Tests passing, build time, bundle size, open issues — only if actively tracked.]
```

### How to Generate It

At the end of every session, paste this prompt:

```
Generate an EOD report for reports/[YYYY-MM-DD].md.

Source it from:
- The session summary in CONTEXT.md
- Any acceptance criteria checked off in PRPs today
- Any DECISIONS.md entries opened or resolved today
- Any blockers noted during the session

Follow the EOD template exactly. Do not add sections not in the template.
Then update CHANGELOG.md with anything that shipped.
Commit and push both files after writing them.
```

### EOD Rules — NON-NEGOTIABLE

- **Never combine EOD and session handoff into one file.**
- **"What Shipped" must be verifiable.** If it cannot be confirmed with a command or visible change, it goes in "What Did Not Ship."
- **One tomorrow task only.** Multiple priorities means no priority. Force the choice.

---

## Step 12 — Document Writing

Documents are written on demand, not automatically. Triggered by an explicit instruction: *"Write a technical spec for X"*, *"Generate an incident report for Y"*, *"Produce a board summary of Z"*.

The AI never writes a document unless told to. It never adds sections beyond what the declared structure specifies.

### The Document Contract

Every document is governed by a contract made at the start:

1. **Declare the type** — what kind of document this is
2. **Declare the structure** — the exact sections, in order, as a numbered list
3. **Approve the structure** — the user confirms before any content is written
4. **Lock it** — the structure cannot change once approved

**STRUCTURAL INTEGRITY RULE — NON-NEGOTIABLE**

Once a document structure is approved:
- The AI must refuse to add any section not in the approved list
- The AI must refuse to introduce new concepts in the conclusion that were not covered in the body
- The AI must refuse to add appendices, notes, or addenda not declared upfront
- If new content is needed, the user must amend the structure first, then approve the amendment

---

### Document Types and Their Structures

#### Technical Specification

```
1. Executive Summary      — What this is and why it exists (max 150 words)
2. Background             — Context and motivation
3. Goals                  — What success looks like, numbered
4. Non-Goals              — What this explicitly does not cover
5. Design / Architecture  — How it works
6. Implementation Plan    — Steps, in order, with owners if known
7. Open Questions         — Unresolved decisions, numbered (each maps to a DECISIONS.md entry)
8. References             — Links to related docs, PRPs, tickets
```

**Rules:**
- Goals must be testable. "Improve performance" is not a goal. "Reduce p95 API latency to under 200ms" is.
- Non-Goals must exist. A spec without Non-Goals is incomplete.
- Open Questions map to entries in DECISIONS.md. Do not resolve them in the spec — let DECISIONS.md track the outcome.

#### Incident / Post-Mortem Report

```
1. Executive Summary      — What broke, when, for how long, impact (max 100 words)
2. Timeline               — Chronological events with timestamps
3. Root Cause             — The specific technical reason, not a symptom
4. Contributing Factors   — What made this possible or worse
5. Resolution             — What was done to restore service
6. Action Items           — Numbered, each with an owner and due date
7. Lessons Learned        — What the system or team will do differently
```

#### Architecture Decision Record (ADR)

```
1. Title                  — ADR-{n}: [Decision in one sentence]
2. Status                 — Proposed | Accepted | Deprecated | Superseded by ADR-{n}
3. Context                — The situation that forced a decision
4. Decision               — What was decided, in one paragraph
5. Consequences           — What this makes easier, harder, or impossible
6. Alternatives Considered — What was rejected and why
```

ADRs live in `docs/decisions/ADR-{n}-{slug}.md`. They are numbered sequentially and never renumbered. When an ADR is created, the corresponding DECISIONS.md entry is marked resolved and points to the ADR file.

#### Project Status Report

```
1. Executive Summary      — One paragraph: where things stand right now
2. Progress Since Last Report — What shipped, bulleted, verifiable
3. In Progress            — What is actively being built
4. Blocked               — What cannot move and why
5. Risks                  — What might go wrong, with likelihood and impact
6. Next Period Plan       — What will be done before the next report
```

#### Board / Investor Report

```
1. Executive Summary      — Headline metrics and overall status (max 200 words)
2. Key Metrics            — Table of the most important numbers vs last period
3. Highlights             — What went well, bulleted
4. Challenges             — What did not go well, bulleted, with no euphemism
5. Financial Summary      — Revenue, costs, burn, runway (if applicable)
6. Outlook                — What the next period looks like
7. Asks / Decisions Required — What the board or investors need to decide or provide
```

---

### Document Anti-Patterns

1. **Never introduce a concept in the conclusion that was not in the body.**
2. **Never write an Executive Summary first.** Write it last.
3. **Never use "it is worth noting that…" to introduce undeclared content.**
4. **Never write a section that does not correspond to a TOC entry.**
5. **Never combine two document types.**
6. **Never leave placeholders in a delivered document.** Mark as draft explicitly if information is missing.

---

### Document Storage

```
docs/
├── source/         ← Raw human context (meetings, research, stakeholder, constraints)
├── specs/          ← Technical specifications
├── decisions/      ← ADRs (ADR-{n}-{slug}.md)
├── incidents/      ← Post-mortem reports (YYYY-MM-DD-{slug}.md)
├── status/         ← Project status reports (YYYY-MM-DD.md)
├── DESIGN.md       ← Design system
└── CODE_STYLE.md   ← Documentation rules

reports/            ← EOD reports (YYYY-MM-DD.md)
```

Documents are never deleted. Superseded specs are marked `[SUPERSEDED by docs/specs/{filename}]` at the top.

---

## The Mental Model

Every file in the system serves one of three audiences:

| Audience | Files |
|----------|-------|
| **The LLM operating right now** | CLAUDE.md, CODE_STYLE.md, DESIGN.md, .llmignore |
| **The LLM starting the next session** | CONTEXT.md, MEMORY.md, DECISIONS.md |
| **The LLM when given explicit context** | docs/source/ (meetings, research, stakeholder, constraints) |
| **The human reviewing progress** | CHANGELOG.md, reports/, docs/specs/, docs/status/ |

No file serves two audiences. If you find yourself writing something that belongs in two places, you have found a gap — add a new file for that gap rather than polluting an existing one.

The system works because each piece is narrow. CLAUDE.md tells the model how to behave. DECISIONS.md stops it from guessing. CONTEXT.md picks up where the last session left off. MEMORY.md stops it from relitigating settled questions. CODE_STYLE.md and DESIGN.md stop it from drifting in style. .llmignore stops it from touching what it should not. The PRP stops it from building before the plan is clear. CHANGELOG.md records what actually shipped. And docs/source/ preserves the human context — the meetings, research, and stakeholder direction that explain *why* the project is being built the way it is.

Remove any one piece and a specific failure mode reappears.
