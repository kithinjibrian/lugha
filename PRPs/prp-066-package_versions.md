## FEATURE: Package versions — `[package] version`, inline entries with requirements, E0324; milestone 37.

**Status:** merged 2026-10-14 — session 85. This is part of the user's "in one go" (milestones 37–38), merged once CI is green.
**Milestone:** 37 (one PRP; tag `m37` at merge; `Language v11 Specification.md` begins)
**Spec:** `Language v11 Draft.md`:
- decisions 2–3, and the choices on versions, `[package]`, requirements, compatibility and E0324;
- §6 "Package versions";
- §9;
- §10 "Package versions";
- §11 (milestone 37).

**Decisions:** MEMORY 80 (v11), MEMORY 70 (loading packages).

## OBJECTIVE
The §10 package-versions program prints `12`. Its manifest is `geo = { path = "libs/geo", version = "1.2" }`, and geo's manifest says `version = "1.4.0"`. If geo said `2.0.0`, it would be E0324.

## CONTEXT

- **Starting state:**
  - `modules/manifest.rs` parses `[packages]` and `name = "path"` lines (E0321).
  - `modules/packages.rs` reads each package's manifest when the package is first imported (E0320, E0322, E0323).
- **Ending state:**
  - manifests carry a version and requirements;
  - the loader checks every requirement;
  - `Language v11 Specification.md` begins.
- **Related existing code:** `src/modules/{mod,manifest,packages}.rs`, `src/modules/tests.rs`, `tests/modules.rs`, `tests/programs/m30/`.
- **Open decisions that must be resolved first:** none.

### Design
- **New `modules/version.rs`:**
  - `Version { major, minor, patch }`, parsed from exactly three decimal parts without leading zeros;
  - `Requirement`, parsed from one to three parts, with `matches(&Version)` by the compatible rule: `>=` the requirement, and below the next increment of its first non-zero part, or of its last written part when every part is 0 (`"0"` accepts 0.x.y, `"0.0"` 0.0.x, `"0.0.4"` only 0.0.4);
  - the E0324 constructor.
- **New `modules/inline.rs`:** the inline entry `{ path = "…", version = "…" }`: keys in either order, `path` required, nothing else (E0321).
- **`modules/manifest.rs`:** `parse` returns a `Manifest { entries, version }`.
  - A `[package]` table (at most one, before or after `[packages]`) takes exactly a `version = "…"` line.
  - `Entry` gains `requirement: Option<Requirement>`.
- **`modules/packages.rs`:**
  - `read_manifest` records each package's declared version (none without a manifest or a `version`), and every requirement with its entry's span.
  - After the module queue empties, `load` calls `check_versions`. It reads the manifest of any required package not yet read (its folder exists), then reports E0324 for each requirement whose package's version doesn't match or is missing.
- **Nothing reaches the checker or codegen.**

## IMPLEMENTATION REQUIREMENTS

### Must Do
- `modules/version.rs`, `modules/inline.rs`, and the manifest and packages changes above.
- **Spec:** start `Language v11 Specification.md` from v10 (in progress: milestone 37); point `lib.rs` `SPEC` and `tests/cli.rs` at it. Fold in:
  - §6 manifest and versions;
  - the E0321 and E0324 rows;
  - the §10 program, as text and as a directory case;
  - §11;
  - "Changes since v10".
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 81, CONTEXT. The tag `m37` is added at the merge.

### Must NOT Do
- **No downloading, lock file, ranges beyond the compatible rule, or pre-release versions.**
- **No named imports or re-exports** (milestone 38).

### New dependencies
None. The TOML subset is still parsed by hand.

## ERROR HANDLING REQUIREMENTS
- **E0321:**
  - a malformed `[package]` table or version;
  - an inline entry missing `path`, with an unknown or repeated key, or with a malformed requirement;
  - a second `[package]`.
- **E0324:**
  - a declared version that doesn't meet a requirement;
  - a requirement on a package without a version.

  Both are at the requirement's line, naming the range and the version found.
- **E0322 / E0323:** unchanged. A requirement on a missing folder reports only E0322.

## SECURITY CONSIDERATIONS
- **Manifests are external input.** Every line is validated: numbers parse with overflow checks (`u64::from_str`), never `unwrap`, and quoted strings reject `"` and `\`.
- No network, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — `modules/version.rs`:
- [x] **Versions:** `1.4.0`, `0.0.0`; not `1.4`, `01.2.3`, `1.2.3-beta`, `1..2`, or a part too large.
- [x] **Requirements:**
  - `"1.2"` matches 1.2.0 and 1.9.9, not 1.1.9 or 2.0.0;
  - `"0.3"` matches 0.3.5, not 0.4.0;
  - `"0.0.4"` matches only 0.0.4;
  - `"0"` matches 0.9.0, not 1.0.0.

Unit tests — `modules/inline.rs` and `manifest.rs`:
- [x] `{ path = "a", version = "1.2" }` and `{ version = "1", path = "a" }` parse.
- [x] E0321 for:
  - a missing path, an unknown key, a key twice, a bad requirement, a missing `}`;
  - a `[package]` without a version, with another key, twice, or with a bad version.

Unit tests — `modules/tests.rs` (in-memory loading):
- [x] A matching requirement loads cleanly.
- [x] E0324 for:
  - a mismatch;
  - a missing version;
  - a required package that is never imported but mismatches.
- [x] A requirement on a missing folder gives only E0322.

Acceptance — `tests/programs/m37/`:
- [x] `versions/` (directory case): the §10 program.
- [x] `e0324/` (directory case): reject golden for a mismatch.
- [x] The §10 program text is in the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-066-package_versions`, merged into `main` after CI passes; tag `m37`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m37/` cases pass.
- [x] `Language v11 Specification.md` begins; docs updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m37/versions/main.la
```

## AMENDMENTS (during build, session 85)

- **Loader tests:** they are in `modules/version_tests.rs`, since `modules/tests.rs` is at 281 lines. `run` and `codes` there became `pub(super)`.
- **The E0321 help text** now names the inline entry and `[package]` forms, so `m30/e0321.stderr` was regenerated (one line).
- **A required package's manifest is read only if its folder passed E0322/E0323:** requirements are recorded after those checks.
- **IR:** the 142 earlier programs that compile give byte-identical IR.
- **No file splits.**
