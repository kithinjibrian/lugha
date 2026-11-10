## FEATURE: A panic inside lughac is an internal compiler error — one error in the chosen diagnostics format, exit code 2.

**Status:** merged into `main` 2026-10-12 — session 30
**Milestone:** none; a bug fix from TODO ("Found while working"), seen in session 26
**Spec:** §9 (exit codes 0/1/2: 2 for internal compiler errors; JSON diagnostics: "one JSON object per line … and nothing else"; internal errors have `"code":null`)
**Decisions:** architecture rule 6 (a Rust panic inside lughac is exit 2, never 1), MEMORY 13 (diagnostics rendering)

## OBJECTIVE
If `lughac` ever panics (a bug in the compiler), it reports one internal error instead of Rust's `thread 'main' panicked at …`, and exits with 2, not 101.
- **Human output:** `error: internal compiler error: <message>`, with a note giving the panic's location in lughac's source and asking for a report.
- **JSON output:** one `"code":null` line, like other internal errors, so tools still get only JSON on stderr.

## CONTEXT

- **Starting state:** `driver::main` calls `run` directly. A panic unwinds out of `main`, Rust prints its default message, and the process exits 101 (session 26 hit this with a codegen bug, since fixed).
- **Ending state:**
  - `driver::main` installs a panic hook (record the location, print nothing) and runs `run` inside a guard.
  - The guard catches the panic and reports it as an internal error in the requested format, exiting 2.
- **Related existing code:** `src/main.rs`, `src/driver/{mod,render,json}.rs`, `tests/cli.rs`.
- **Open decisions that must be resolved first:** none.

### Amendments during implementation (session 30)
- **Placement:** the guard lives in a new `driver/guard.rs`, since `driver/mod.rs` was at 258 lines. `Command::file()` was added for the report's file name.
- **The note** uses the report's help line, so it renders as `= help: this is a bug in lughac (src/…:LINE); please report it`.
- **Stale spec wording:** the same §9 sentence dropped "a construct the compiler doesn't support yet" from its list of internal errors; nothing produces that since the gate was removed (PRP-022).
- **End-to-end check:** a throwaway build with a `panic!` injected into `check()`, never committed, gave exit 2 with exactly one human error, and a single JSON line.

### Discovery answers (session 30)
1. **One internal error in the chosen format:**
   - **Human:** `error: internal compiler error: <message>`, plus the note "this is a bug in lughac (<source file>:<line>); please report it".
   - **JSON:** a single line with `"code":null` and `"span":null`.
2. **Tested through the guard:** unit tests pass the guard a closure that panics. The shipped binary has no deliberate crash switch.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Guard** — `driver/mod.rs` (or a new `driver/guard.rs` if `mod.rs` would pass 300 lines)
- `fn guarded(format, file: Option<&Path>, stderr: &mut dyn Write, work: impl FnOnce() -> u8) -> u8`:
  - **Runs `work`** under `std::panic::catch_unwind` (`AssertUnwindSafe`, documented: the compiler's state is discarded after a panic, so nothing observes it half-updated).
  - **On a panic:**
    - takes the message from the payload (`&str` or `String`; anything else becomes "unknown panic");
    - writes one internal error, human or JSON per `format`, naming the source file if one was given;
    - returns 2.
  - **Location:** read from a process-wide slot that only the hook fills (absent in unit tests, which then omit it from the note).
- **Rendering:** the same `Report::internal` / JSON paths as other internal errors, so the formats stay identical.

**Entry point** — `driver::main`
- Before parsing arguments, install `std::panic::set_hook`. The hook stores the panic location (`file:line`) in the slot and prints nothing, so stderr carries only the internal error.
- Then call `run` through `guarded`. The diagnostics format and the input file come from the arguments, defaulting to human with no file if they can't be parsed.
- `run` itself (the library entry tests use) is unchanged, and installs no hook.

**Docs**
- Spec §9: one sentence. An internal compiler error, including a crash inside the compiler, is reported as `error: internal compiler error: …` and exits with 2. This is a spec text change, approved with this PRP.
- TODO: the "Found while working" item ticked.
- CHANGELOG, MEMORY (short), CONTEXT.

### Must NOT Do
- No hidden switch to make the binary panic (discovery answer 2).
- No change to how compiler-detected internal errors (I/O, link, verify) are reported.
- No `panic = "abort"`, no new dependencies, no `unsafe`.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- The guard never panics itself.
  - **Writing the report:** if it fails, the failure is ignored, with a comment saying why: the exit code still says 2.
  - **Payloads:** a non-string payload is handled.
- Normal errors keep their codes: 0 for success, 1 for program errors, 2 for usage and internal errors.

## SECURITY CONSIDERATIONS

- The panic message and lughac source location are printed. They contain no user data beyond what the message already had.
- No `unsafe`.

## TESTS TO WRITE

Unit tests — the guard:
- [x] A closure that panics with a `&str` → returns 2. Human stderr starts with `error: internal compiler error: boom` and includes the report note.
- [x] A `String` payload (`panic!("{}", x)`) and a non-string payload (`std::panic::panic_any(5)`) → "unknown panic".
- [x] JSON format → exactly one line, valid JSON shape, with `"code":null` and `"span":null`.
- [x] A closure that returns normally → its own code (0, 1, 2) passes through untouched, and nothing is written.

Integration:
- [x] All existing CLI tests pass (normal runs go through the guard).

## ROLLBACK PLAN

- Branch `prp-024-internal_error_exit`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] A panic in lughac exits 2 with one internal error in the chosen format.
- [x] Every test above passes; spec §9 sentence added; TODO, CHANGELOG, MEMORY updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
