## FEATURE: Housekeeping — a real README, CI on every push and PR, harness-enforced protected files, and `setup.md` moved into `docs/`.

**Status:** implemented 2026-10-09 — session 20 (branch `prp-017-housekeeping`, awaiting merge)
**Milestone:** none; post-v0 housekeeping (TODO "Setup" items)
**Spec:** §9 (command-line interface, which the README documents), §10 (the README's example program)
**Decisions:** DECISION-005 / MEMORY 10 (LLVM 21, dynamic linking), MEMORY 12 (tests), CLAUDE.md PROTECTED FILES and `.llmignore`

## OBJECTIVE
A visitor to the repository can learn what Lugha is, install the prerequisites, build `lughac`, compile a program, and run the tests from the README alone. Every push and pull request runs fmt, clippy and the full test suite in CI, on the same Ubuntu and LLVM as the development machine. Claude Code's harness itself refuses to read or edit the protected files. The context-system guide lives under `docs/`.

## CONTEXT

- Starting state:
  - `README.md` is `# lugha.`
  - No CI. The protected files are listed in `.llmignore` and CLAUDE.md only.
  - `setup.md` sits at the root.
- Ending state:
  - `README.md` filled in.
  - `.github/workflows/ci.yml`.
  - `.claude/settings.json` with deny rules.
  - `docs/setup.md`, with references updated.
- Related existing code: `rust-toolchain.toml`, `Cargo.toml`, `.llmignore`, CLAUDE.md (PROTECTED FILES, COMMANDS, FILE ORGANIZATION), TODO.md, DECISIONS.md.
- Open decisions that must be resolved first: none.

### Amendments during implementation (session 20)
- **CI was green on its first run** (run 37964219151): no extra packages were needed beyond `llvm-21-dev libgc-dev build-essential curl ca-certificates git`, and all 191 tests ran in the container.
- **Actions** pinned to `actions/checkout` v7.0.1 and `actions/cache` v6.1.0, the latest releases at the time.
- **`rust-toolchain.toml`** already listed `rustfmt` and `clippy`, so it was unchanged.
- **The first `git push`** hung inside `ssh git-receive-pack`; it was killed and retried with a timeout, which went through at once. This was not a repository problem.

### Discovery answers (session 20)
1. **Scope:** all four items — README, CI, Claude deny rules, and moving `setup.md`.
2. **CI LLVM:** the job runs in the `ubuntu:26.04` container and installs `llvm-21-dev` and `libgc-dev` from the stock Ubuntu repositories, matching the development machine. No third-party apt source.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**README.md**
- What Lugha is (one paragraph, from spec §1), and a short example: the §10 hello world or recursion program with `lughac run`.
- Prerequisites:
  - Rust via rustup (the toolchain is pinned in `rust-toolchain.toml`);
  - `llvm-21-dev`, `libgc-dev`, and a C compiler (`cc`).
  - The Ubuntu `apt` line.
  - A note that LLVM must be major version 21 and is linked dynamically.
- Build and install: `cargo build --release`, `cargo install --path .`.
- Usage: `lughac build|run|check|spec`, `--emit`, `-O0/-O2`, `--diagnostics=json`, and exit codes 0/1/2, matching spec §9.
- Tests: `cargo test`, and the `tests/programs/` convention (add a test by adding `.la` + expectation files).
- Pointers: the spec (`docs/specs/…`, or `lughac spec`), CLAUDE.md and the PRPs for contributors (human or AI), and the license (GPL-3.0).

**CI** — `.github/workflows/ci.yml`
- Triggers: `push` and `pull_request`.
- One job, `runs-on: ubuntu-latest`, `container: ubuntu:26.04`. Steps:
  1. `apt-get update && apt-get install -y --no-install-recommends llvm-21-dev libgc-dev build-essential curl ca-certificates git`, plus any library `llvm-sys` needs to link that the build reveals (e.g. `zlib1g-dev`, `libzstd-dev`), each justified in a comment.
  2. `actions/checkout`, pinned to a full commit SHA with the version in a comment.
  3. Install rustup non-interactively with `--default-toolchain none`. The pinned toolchain and its `rustfmt`/`clippy` components then come from `rust-toolchain.toml`; add the components there if they aren't listed.
  4. Cache `~/.cargo/registry`, `~/.cargo/git` and `target/` with `actions/cache` (SHA-pinned), keyed on `Cargo.lock` and `rust-toolchain.toml`.
  5. `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test`.
- `permissions: contents: read`; no secrets used.
- **Verified by a real run:** push the branch and watch the workflow (`gh run watch`). Merge only when it's green. Fixes found by that run go into this PRP's amendments.

**Claude deny rules** — `.claude/settings.json`
- `permissions.deny` mirrors `.llmignore` and CLAUDE.md PROTECTED FILES:
  - `Read` and `Edit` of `.env` and `.env.*`, `Cargo.lock`, `target/**` and `LICENSE`.
- Committed (shared project settings, not `settings.local.json`).
- `.llmignore` and CLAUDE.md say the deny list must stay in sync with them, and CLAUDE.md names the file.
- The deny rules can only block Claude's own tools. They are not a security boundary, and the docs say so: cargo still writes `Cargo.lock` and `target/`.

**Move `setup.md`**
- `git mv setup.md docs/setup.md`.
- Update CLAUDE.md's file tree, and the live references in TODO.md and DECISIONS.md.
- CONTEXT.md and earlier session history are left as written (sessions are never edited), as are historical PRPs.

**Docs**
- TODO: tick the four Setup items.
- CLAUDE.md: tree (`.github/`, `.claude/`, `docs/setup.md`), PROTECTED FILES pointer to `.claude/settings.json`.
- CHANGELOG, MEMORY (CI and protection facts), CONTEXT.

### Must NOT Do
- No compiler or language changes; no new crates.
- No third-party apt sources, and no unpinned or third-party GitHub Actions beyond `actions/checkout` and `actions/cache`.
- No release automation, badges for services not set up, or publishing.

### New dependencies
- None (Rust crates). CI uses `actions/checkout` and `actions/cache`, both GitHub-maintained and pinned by SHA.

## ERROR HANDLING REQUIREMENTS

- CI fails the job on any failing step; no `continue-on-error`.

## SECURITY CONSIDERATIONS

- **Workflow token:** read-only (`permissions: contents: read`), no secrets, and every action pinned by SHA against supply-chain substitution.
- **CI packages:** come only from the official Ubuntu archive and rustup's official installer over HTTPS.
- **Deny rules:** they reduce accidental access by Claude's tools; they are not a sandbox, and the docs say so.

## TESTS TO WRITE

- [x] CI itself is the test: a green run on the branch, with fmt, clippy and every test executed in the container (the log shows all test binaries, including `tests/spec.rs`).
- [x] The README's commands are run locally as written: the apt line is checked with `apt-get -s`; `cargo build --release`, the example program via `lughac run`, `lughac spec | head -1`.
- [x] `.claude/settings.json` is valid JSON with the expected deny entries (checked with `python3 -m json.tool`).
- [x] `cargo test` still passes after the move (nothing reads `setup.md`).

## ROLLBACK PLAN

- Branch `prp-017-housekeeping`, merged into `main` after a green CI run.
- To abandon: delete the branch. To disable CI later, delete the workflow file.

## ACCEPTANCE CRITERIA
- [x] README covers the overview, an example, prerequisites, build, usage, tests, pointers and the license.
- [x] CI is green on the branch.
- [x] `.claude/settings.json` denies the protected files.
- [x] `setup.md` lives in `docs/`; the live references are updated.
- [x] CLAUDE.md, TODO.md, CHANGELOG.md, MEMORY.md updated.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `gh run watch` → success
