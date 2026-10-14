## FEATURE: Downloaded packages — `{ git = "URL", version = "R" }` manifest entries, a cache, `lugha.lock`, `lughac update`, `--offline` (E0325, E0326); milestone 50.

**Status:** approved 2026-10-14 — session 102. Proposed in session 101; the user reviewed the security design and approved it as written.
**Milestone:** 50 (one PRP; tags `m50` and `v14` at merge)
**Spec:** `Language v14 Draft.md`:
- decision 5, and the choices on downloaded packages;
- E0325, E0326;
- §11 (milestone 50), §12 (git packages and `extern`).

**Decisions:** MEMORY 91 (v14), MEMORY 86 (package versions), MEMORY 82 (packages, PRP-058).

## OBJECTIVE
A program's `lugha.toml` can say `geo = { git = "https://example.org/geo.git", version = "1.4" }`. Then:
- `lughac build` fetches the newest matching `vX.Y.Z` tag into a cache and records its commit in `lugha.lock`;
- later builds use exactly the locked commit;
- `lughac update` re-resolves;
- `--offline` never touches the network.

## CONTEXT

- **Starting state:**
  - manifest entries are `name = "path"` or `{ path, version }` (v8, v11);
  - the loader reads files only through its `Read` closure;
  - nothing in lughac runs another program except `cc` at link time.
- **Ending state:** git entries resolved through the system `git`, a per-user cache, and a lock file.
- **Related existing code:**
  - `src/modules/{mod,manifest,inline,packages,version}.rs`;
  - `src/driver/{mod,pipeline}.rs`;
  - `tests/cli.rs`, `.github/workflows/ci.yml` (it already installs `git`).
- **Open decisions that must be resolved first:** none in `DECISIONS.md`. **This PRP itself is the security decision to review.**

### Design

**Manifest** (`inline.rs`):
- an entry may be `{ git = "URL", version = "R" }`;
- `path` and `git` are exclusive, and `git` needs `version`.

**Lock file** (new `modules/lock.rs`):
- `lugha.lock` sits next to the main file, in the manifest subset:
  ```toml
  # Written by lughac; do not edit.
  [locked]
  geo = { git = "https://example.org/geo.git", version = "1.4.2", commit = "<40 lowercase hex>" }
  ```
- Parsing follows the manifest's rules. A malformed lock is E0321.

**Fetching** (new `modules/git.rs`, behind a `Fetch` trait so the loader's unit tests use a fake):
- **`ls_remote(url)`:** `git ls-remote --tags --refs -- URL`. Only refs `refs/tags/vMAJOR.MINOR.PATCH` are read; anything else is ignored.
- **`clone(url, tag, dir)`:** `git clone --depth 1 --branch <tag> --no-recurse-submodules -- URL <dir>`.
- **`head(dir)`:** `git -C <dir> rev-parse HEAD`, which must be 40 hex digits.
- **Choosing a tag:** the highest tag meeting the requirement, with v11's version rules.

**Cache** (new `modules/cache.rs`):
- the root is `$LUGHA_CACHE`, else `$XDG_CACHE_HOME/lugha`, else `$HOME/.cache/lugha`; each must be an absolute path, or it is skipped;
- a package lives in `<root>/git/<escaped URL>/<commit>`;
- a clone goes into `<root>/tmp/<random name>`, then its `.git` folder is removed and the folder is renamed into place;
- a folder already in place is used as is.

**Loading** (`packages.rs`):
- a git entry's folder is its cache folder; reading its modules and manifest is unchanged;
- a git package's own git entries are resolved the same way, and they all go in the root's lock;
- `Loaded` gains `locked: Vec<Locked>`.

**Driver:**
- **`lughac update [prog.la]`:** re-resolves every git entry, ignoring the lock, and writes it.
- **`build`, `run` and `check`:**
  - use the lock;
  - resolve only entries it doesn't have;
  - write the lock when it changed (a temporary file, then a rename).
- **`--offline`** on all three: no `git` is run; anything not cached is E0325.
- The driver, not the loader, writes the lock file (stages return records; I/O stays at the edge).

### Security design — the part to review

**1. What may be fetched:**
- only `https://` and `file://` URLs (E0321 otherwise). `ssh`, `git://`, `ext::` and other helpers are refused;
- as a second guard, `git` runs with `GIT_ALLOW_PROTOCOL=https:file`;
- a URL with user info (`https://user:token@host/…`) is E0321, so no credential is ever written into `lugha.toml` or `lugha.lock`, or echoed in a message;
- a URL may not contain whitespace or control characters.

**2. How `git` runs:**
- `std::process::Command` with an argument list, never a shell;
- `--` comes before every URL and path, so a value can't become an option;
- the tag passed to `--branch` is rebuilt from the parsed version (`v1.4.2`), never copied from `git`'s output;
- the environment adds:
  - `GIT_TERMINAL_PROMPT=0` (no password prompts);
  - `GIT_ALLOW_PROTOCOL=https:file`;
  - `GIT_HTTP_LOW_SPEED_LIMIT=1000` and `GIT_HTTP_LOW_SPEED_TIME=30` (a stalled transfer fails instead of hanging);
- every command gets:
  - `-c core.hooksPath=/dev/null` (no hooks run);
  - `-c core.fsmonitor=false`;
  - `-c core.symlinks=false` (a symlink in a package is checked out as a small text file, so a package can't point a `.la` file at, say, `~/.ssh/id_rsa` and have it echoed in a diagnostic);
  - `-c submodule.recurse=false`;
- the user's own git configuration (proxies, `insteadOf`, credential helpers) still applies: it is theirs, and lughac never reads or logs credentials.

**3. What lughac writes:**
- **Only these:**
  - the cache, under the root above;
  - `lugha.lock` next to the main file.
- **Cache folder names:**
  - built from the URL by an injective escape: `[A-Za-z0-9.-]` kept, `_` → `__`, and any other byte → `_XX` hex;
  - plus the 40-hex commit, checked before use;
  - so no name can contain `/`, `..` or a NUL.
- **Clones:** each goes into a fresh folder inside the cache and is renamed into place, so a failed or interrupted clone never leaves a half folder where a package should be.

**4. What runs:**
- lughac never runs code from a package: it reads `.la` files and `lugha.toml`, and nothing else;
- no build scripts, no hooks (above);
- a built program runs a package's code as it runs any code, including its `extern` C calls, as today for path packages (§12 notes listing a package's externs as a later idea).

**5. Integrity:**
- the lock pins a commit, and a cached folder is named by that commit;
- a fresh clone of a locked entry must have that commit at `HEAD` (E0326 otherwise), so a moved tag is caught.

**6. Hashing:** no cryptography of lughac's own (CLAUDE.md's security rule). Commit ids are git's; trusting git's object ids is stated as a limit.

**7. Messages:**
- E0325 carries `git`'s stderr, trimmed to its first 10 lines;
- the URL is already credential-free (point 1).

**8. Offline:** `--offline` runs no `git` at all, for builds in sandboxes or CI.

**Not defended against** (stated in the spec):
- a malicious package's code doing harm when the built program runs;
- a compromised host serving a new commit under a new tag;
- git's own vulnerabilities;
- a very large repository filling the disk.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** everything above.
- **Spec:**
  - §6 (packages: git entries, the cache, the lock, commands, offline, the security rules above as user-facing facts);
  - the CLI table (`update`, `--offline`);
  - the E0321 row examples;
  - the E0325 and E0326 rows;
  - the §10 note: the M50 program can't run from the spec, since it needs a repository, so `tests/git_packages.rs` builds one;
  - §11; §12;
  - the changes summary;
  - the status "complete".
- **Docs:** CLAUDE.md, README (the prerequisite `git`, only for git packages), TODO, CHANGELOG, MEMORY decision 97, CONTEXT. The tags `m50` and `v14` are added at the merge.

### Must NOT Do
- **Never** a shell, or `ssh`/`git://` URLs.
- **No** credential handling of lughac's own, no registry, no package build scripts.
- **No** two versions of one package in a build, and no pre-release versions.

### New dependencies
None. The system `git` is a runtime prerequisite only when a manifest has a git entry. Random temporary names come from the process id, the time and a counter, not a crate.

## ERROR HANDLING REQUIREMENTS
- **E0321:**
  - `git` with `path`;
  - `git` without `version`;
  - a non-`https`/`file` URL, a URL with user info or whitespace;
  - a malformed `lugha.lock` (at its line).
- **E0325, at the manifest entry, with `git`'s message:**
  - no `git` on `PATH`;
  - `ls-remote` or `clone` failed;
  - no tag meets the requirement;
  - `--offline` and the package isn't cached;
  - `HEAD` isn't 40 hex digits.
- **E0326:** the locked commit isn't what the tag gives now.
- **Internal errors** (exit 2): no cache root can be found (`$HOME` unset); writing the cache or lock fails.

## SECURITY CONSIDERATIONS
See "Security design" above. Approved by the user in session 102.

## TESTS TO WRITE

Unit tests — `inline.rs`/`manifest.rs`: git entries; each E0321 (`ssh://`, `git@host:x`, `ext::sh`, user info, `path` and `git` together, `git` without `version`).

Unit tests — `lock.rs`: parse and write round-trip; E0321 for a bad line or a non-hex commit.

Unit tests — `cache.rs`:
- [ ] The escape is injective (`a/b` vs `a_b` vs `a_2Fb`) and never yields `/`, `..` or NUL.
- [ ] The root comes from the variables in order, and a relative value is skipped.

Unit tests — `git.rs`:
- [ ] Parsing `ls-remote` output: tags kept, others ignored, peeled `^{}` refs ignored.
- [ ] The command lines built: `--`, the `-c` flags and the environment, checked as data without running `git`.

Unit tests — the loader with a fake `Fetch`:
- [ ] Resolution picks the highest matching tag.
- [ ] The lock is used when present.
- [ ] E0326 for a moved tag.
- [ ] Offline: E0325 when not cached.
- [ ] Transitive git entries.

Integration — new `tests/git_packages.rs` (needs `git`, as CI has). It makes a `file://` repository in a temp folder with tags `v1.0.0` and `v1.1.0`, then:
- [ ] builds and runs a program using it, checks `lugha.lock`'s commit, then builds again `--offline` with the same cache;
- [ ] moves the tag and expects E0326; runs `lughac update` and expects the new commit;
- [ ] E0325 for a requirement no tag meets;
- [ ] a symlink in the repository is checked out as a plain file.

Regression:
- [ ] Every earlier test passes, and programs without git entries load exactly as before (no `git` run, no lock written).

## ROLLBACK PLAN

- Branch `prp-079-git_packages`, merged into `main` after CI passes; tags `m50` and `v14`.
- To abandon: delete the branch. The cache is per-user and can be deleted.

## ACCEPTANCE CRITERIA
- [ ] `tests/git_packages.rs` passes locally and in CI.
- [ ] The spec is complete for v14; docs updated.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo test --test git_packages
```
