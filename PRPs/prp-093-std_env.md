## FEATURE: `std.env` — read-only environment variables and the current directory; milestone 64.

**Status:** approved 2026-10-15 — session 129 ("approved"); proposed in session 128 (discovery in session 127).
**Milestone:** 64 (one PRP; tag `m64` at merge)
**Spec:** `Language v15 Draft.md` decision 48; spec §6 (std modules), "The standard library", §9 (runtime table), §10 (a program), §11 (row 64).

**Decisions:**
- the v15 discovery's "environment variables" item (session 103) and session 127's answers;
- follows M53/M54's std pattern (MEMORY 102–105): a module in `src/std/`, bundled `$os.*` built-ins, and a runtime fragment;
- no open entries in DECISIONS.md apply.

## OBJECTIVE
```
import std.env;
let data = env.get("DATA").unwrap_or("data");
let here = env.current_dir()?;
```

## CONTEXT
- **Starting state:** `std.math` and `std.random` exist; `$os.seed` is the only `$os` built-in (`check/float_ops.rs::os_op`, `codegen/float_ops.rs`).
- **Ending state:** decision 48.
- **Related existing code:**
  - `src/std/` (new `env.la`), `src/lib.rs` (`STD`), `src/modules/` (std module list, E0312 help);
  - `check/float_ops.rs`, `codegen/float_ops.rs` (`$os.*`);
  - `runtime/` (new `env.c`), `src/link.rs` (`RUNTIME_PARTS`, `RUNTIME_SOURCE`, `RUNTIME_SYMBOLS`).
- **Open decisions that must be resolved first:** none.

### Design
1. **`src/std/env.la`:**
   - `public fun get(name: string): Option<string>`;
   - `public fun current_dir(): Result<string, string>`.
2. **Built-ins, through `os_op`:**
   - `$os.env(name): Option<string>` → `lugha_rt_env(name, &out)`. It uses `getenv`, copies the value into a GC string, and returns `None` when unset or not valid UTF-8; a NUL in `name` returns `None`.
   - `$os.cwd(): Result<string, string>` → `lugha_rt_cwd`. It uses `getcwd` with a growing buffer, and the system's message on failure.
3. **Runtime fragment:** `runtime/env.c`, added to `RUNTIME_PARTS`, `RUNTIME_SOURCE` and `RUNTIME_SYMBOLS`. It has no `#include` lines; only the core may have them, so any header it needs goes in the core fragment.
4. **Lazy (M60):** unused functions aren't compiled.

### Risk
- **Lifetime of `getenv`'s pointer:** the value is copied at once, so later environment changes (none possible from Lugha) can't affect it.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- **Code:** the above.
- **Spec v15:** "The standard library": a `std.env` table; §9 runtime rows; §10 program "Environment", which needs no variables set: it prints `env.get("LUGHA_SURELY_UNSET").is_none()` and whether `current_dir()` is `Ok`; §11 row 64; the changes summary.
- **Docs:** CLAUDE.md (tree: `std/env.la`, `runtime/env.c`; `RUNTIME_SYMBOLS` count), README, TODO, CHANGELOG, MEMORY plan and built entries, CONTEXT; tag `m64`.

### Must NOT Do
- **Read-only:** no setting or unsetting variables, no changing directory, no listing all variables (decision 48).
- **Never print or log variable values** from the compiler.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **`get`:** `None` for unset, non-UTF-8, or a name containing NUL.
- **`current_dir`:** `Err` with the system's message, such as a deleted working directory.

## SECURITY CONSIDERATIONS
- **Reading the environment** is reading external input. Values are validated as UTF-8 before becoming strings, and nothing is written.
- **Fixed-size buffers:** none (growing `getcwd` buffer, with an overflow check).
- **No `unsafe`, no dependencies.**

## TESTS TO WRITE
- [ ] **Program `m64/env.la`:**
  - `env.get("LUGHA_TEST_VALUE")`, where the test runner sets the variable from a new optional `<name>.envvars` file beside the case (one `KEY=VALUE` per line), read like `.args` and `.stdin`;
  - an unset name gives `None`;
  - `current_dir()` ends with the case's run directory (printed as a suffix check: `true`).
- [ ] **Runner unit test** for `.envvars` parsing (reject lines without `=`).
- [ ] **E0312 help** lists `std.env`.
- [ ] **Regression.**

## ROLLBACK PLAN
- Branch `prp-093-std_env`; tag `m64`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] All tests pass at `-O0`; `m64/` at `-O2`.
- [ ] Spec at milestone 64; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] fmt, clippy and tests pass; CI green.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
```
