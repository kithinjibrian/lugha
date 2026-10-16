## FEATURE: `pkg-config --cflags` may give `-msse`, `-msse2` and `-mfpmath=sse` (GTK does), so a package can list C files and GTK together; v17.

**Status:** built in session 223; approved 2026-10-17 by the user as a security decision (session 223): an exact list of three flags, not dropping unrecognised `-m…` flags.

## OBJECTIVE
A package whose `[link]` table lists C files and `pkg-config = ["gtk4"]` builds: GTK's `--cflags` on x86-64 include `-mfpmath=sse -msse -msse2`, which lughac refused (E0329). These three are x86-64 System V defaults, so passing them changes nothing; every other flag outside the list stays refused by name.

## CONTEXT
- Starting state: `src/link/pkg_config.rs` keeps `-l`/`-L`/`-pthread` from `--libs` and `-I`/`-D`/`-pthread` from `--cflags`, refusing the rest (PRP-117, spec v17 §8).
- Ending state: `--cflags` also keeps exactly `-msse`, `-msse2`, `-mfpmath=sse`; spec §8 and the refusal message say so.
- Found by: the kivuli/lugha-gtk session (a package with C shims and GTK).

## IMPLEMENTATION REQUIREMENTS

### Must Do
- A `Flag::X86_64(&'static str)` for the three exact strings, from `--cflags` only.
- The refusal message lists them.

### Must NOT Do
- Accept any other `-m…` flag (`-m32`, `-mabi=…`, `-mno-red-zone`, `-march=…`): they change the ABI or the code the C file is compiled to.
- Drop flags silently: refusing by name keeps failures explainable.
- Accept them from `--libs`.

### New dependencies
- none

## ERROR HANDLING REQUIREMENTS
- Unchanged: a refused flag is E0329 with its name.

## SECURITY CONSIDERATIONS
- Changes what `cc` receives from a package's `pkg-config` output (CLAUDE.md anti-pattern 5): reviewed and approved by the user. The three flags select instruction-set defaults Lugha's only target already has; none reads or writes files or loads code.

## TESTS TO WRITE
- [ ] `pkg_config.rs`: GTK's real `--cflags` line keeps the three flags, in order, with `-I` and `-pthread`.
- [ ] `pkg_config.rs`: `-m32`, `-march=native`, `-mno-red-zone`, `-msse3`, and `-msse2` from `--libs` are still refused by name.
- [ ] `tests/link_libs.rs`: a fake `pkg-config` giving `-mfpmath=sse -msse -msse2` with a package's C file builds and runs.

## ROLLBACK PLAN
- Branch to return to: `main`; revert the merge.

## BUILT (session 223)
- As planned: `X86_64_DEFAULTS` and `Flag::X86_64` in `src/link/pkg_config.rs`; `inputs.rs` ignores it for `--libs` (never produced there). Spec §8, E0329's page, CLAUDE.md.
- Tests: `cflags_keep_gtks_x86_64_defaults_v17`, five more refusals, `link_libs::c_files_build_with_gtks_sse_cflags_v17`. Targeted: `--lib`, `link_libs`, `explain`, fmt, clippy.

## ACCEPTANCE CRITERIA
- [ ] A package with C files and GTK's flags builds.
- [ ] Targeted tests (`--lib`, `link_libs`), fmt, clippy pass; CHANGELOG updated.

## VALIDATION
- `cargo test --lib link`, `cargo test --test link_libs`, `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`
