# TODO — lugha

Outstanding tasks. Check items off as they are done; move shipped work to CHANGELOG.md.
Questions that need a decision go in DECISIONS.md, not here.

---

## Setup

- [ ] Resolve DECISION-001 — what lugha is and its crate layout
- [ ] Resolve DECISION-002 — error type crate
- [ ] Resolve DECISION-003 — Rust edition and MSRV
- [ ] `cargo init` per DECISION-001 and commit `Cargo.toml` + `Cargo.lock`
- [ ] Fill in CLAUDE.md: STACK, ARCHITECTURE RULES, FILE ORGANIZATION
- [ ] Copy resolved decisions into MEMORY.md
- [ ] Replace the one-line README.md with a real project description
- [ ] Add CI (GitHub Actions): `cargo fmt --check`, `cargo clippy -- -D warnings`, `cargo test`
- [ ] Mirror `.llmignore` as deny rules in `.claude/settings.json` — Claude Code does not read `.llmignore` itself
- [ ] Decide whether `setup.md` stays at the root or moves to `docs/`

## Features

- [ ] Write the first PRP (after DECISION-001)
