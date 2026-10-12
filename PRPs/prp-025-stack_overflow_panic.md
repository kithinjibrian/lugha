## FEATURE: Too-deep recursion panics with `panic: stack overflow` (exit 101) instead of crashing with a signal (exit 139).

**Status:** merged into `main` 2026-10-12 — session 31
**Milestone:** none; a TODO idea from PRP-021's discovery
**Spec:** §5 (panics), §7 ("Recursion depth"), §9 (runtime)
**Decisions:**
- DECISION-009 / MEMORY 15 (the runtime is embedded C, compiled in the link step);
- MEMORY 26 (deep recursion documented as a crash, which this PRP replaces).

## OBJECTIVE
A Lugha program that recurses too deeply stops the way every other runtime error does: it flushes its output, prints `panic: stack overflow` to stderr, and exits with 101. Other segfaults behave as today; ordinary Lugha code can't produce one, only C code called through `extern fun`. Normal calls cost nothing extra.

## CONTEXT

- **Starting state:**
  - Spec §7 says too-deep recursion crashes with a signal (exit 139).
  - CLAUDE.md lists that as a known issue.
  - `lugha_rt_init` only runs `GC_INIT()`.
- **Ending state:**
  - `lugha_rt_init` also records the main thread's stack bounds and installs a SIGSEGV handler on an alternate signal stack.
  - The handler turns a fault at the stack's guard area into the panic.
  - The spec and CLAUDE.md are updated.
- **Related existing code:** `runtime/lugha_rt.c`, `src/link.rs` (the link command), `tests/codegen.rs` (`-Werror` runtime build), `tests/programs/`.
- **Open decisions that must be resolved first:** none.

### Amendments during implementation (session 31)
- **Re-raise:** a non-overflow SIGSEGV restores the default action and *re-raises*, rather than only returning. A real fault would repeat anyway, but a SIGSEGV sent by `raise()`/`kill()` would otherwise be swallowed. A second program, `m8/segv_from_c.la` (`raise(11)` through `extern fun`), pins exit 139.
- **Link step:** `-pthread` was not needed; glibc on Ubuntu 26.04 includes pthread in libc.
- **Bounds:** only the stack's lowest address is kept, since the handler checks a window around it.

### Discovery answers (session 31)
1. **Guard-page signal handler.** There is no per-call counter. Detection happens only when the stack's guard area is hit, so normal code pays nothing.

### Needs approval with this PRP — spec change
`panic: stack overflow` is the one panic without ` at <file>:<line>:<col>`, because the runtime can't know which call went too deep. Spec §5 "Panics" gains it, with that exception stated. §7 "Recursion depth" says too-deep recursion panics instead of crashing.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Runtime** — `runtime/lugha_rt.c` (Linux with glibc, the supported platform; `#define _GNU_SOURCE` before the includes)
- **In `lugha_rt_init`, after `GC_INIT()`** (Boehm may use signals during its own start-up):
  - record the main thread's stack range with `pthread_getattr_np` / `pthread_attr_getstack`;
  - install a 64 KiB alternate signal stack (`sigaltstack`, static storage), so the handler can run when the normal stack is exhausted;
  - install a `SIGSEGV` handler with `SA_SIGINFO | SA_ONSTACK`.
- **The handler:**
  - **Stack overflow:** a fault address (`si_addr`) within 64 KiB below the stack's lowest address up to 64 KiB above it. The handler flushes stdout (best effort, as in other panics, so earlier output isn't lost), writes `panic: stack overflow\n` to stderr with `write`, and exits with `_exit(101)`.
  - **Anything else:** it restores the default action and returns, so the fault repeats and the process dies with SIGSEGV as before (exit 139 from `lughac run`).
- **Failure tolerance:** if any setup call fails, the runtime keeps the old behaviour. It never aborts the program.

**Compiler** — none expected. The link command (`cc prog.o lugha_rt.c -lgc -lm`) needs nothing extra on glibc ≥ 2.34, where pthread is part of libc. If CI shows otherwise, `-pthread` is added, recorded in the amendments.

**Spec and docs**
- **§5 Panics:** add stack overflow (message `stack overflow`, no location) to the list.
- **§7 Recursion depth:** "panics with `stack overflow` (exit 101)" instead of "crashes with a signal (exit 139)". Keep "walk very long lists with a loop".
- **§9 runtime table:** `lugha_rt_init` also installs the stack-overflow handler.
- **CLAUDE.md:** remove the "Too-deep recursion crashes with a signal" known issue.
- TODO (tick the idea), CHANGELOG, MEMORY (update decision 26's note), CONTEXT.

### Must NOT Do
- No per-call depth counting or codegen change.
- No catching of other signals, and no change to how genuine segfaults (from C code) end.
- No `unsafe` Rust, no new dependencies.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- Every setup step checks its return value. On failure the program simply runs without the handler, which is today's behaviour.
- The handler uses only async-signal-safe calls (`write`, `_exit`), plus a best-effort `fflush(stdout)` for consistency with other panics. If that flush deadlocks (the overflow happened inside stdio), the documented risk is a hang instead of a crash, which is acceptable for a program already out of stack. A comment states this.

## SECURITY CONSIDERATIONS

- The handler distinguishes stack overflows by address only, and re-raises everything else with the default action, so it can't hide memory corruption from C code.
- The alternate stack is static and fixed-size, with no heap allocation in the handler.
- No `unsafe` Rust.

## TESTS TO WRITE

- [x] **`tests/programs/m8/stack_overflow.la`:** a function recursing without end, after printing a line first.
  - stdout: that line.
  - stderr: exactly `panic: stack overflow\n`.
  - exit 101 (also at `-O2` via the cross-check). It must not be turned into a loop by LLVM's tail-call optimization: the recursion is not in tail position.
- [x] **`tests/codegen.rs`:** the runtime still compiles with `-std=c11 -Wall -Wextra -Werror`.
- [x] **Regression:** `m6/recursive.la` (a million-node loop, a deep tree) and every other program still pass.

## ROLLBACK PLAN

- Branch `prp-025-stack_overflow_panic`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] Unbounded recursion prints `panic: stack overflow` and exits 101; earlier output is not lost.
- [x] The spec (§5, §7, §9), CLAUDE.md, TODO, CHANGELOG, MEMORY updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo test`
- `lughac run tests/programs/m8/stack_overflow.la` → `panic: stack overflow`, exit 101
