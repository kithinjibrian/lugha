## FEATURE: Green threads — `spawn f(args)` runs a call concurrently on one OS thread, with `Task<T>.join()`, channels `Chan<T>`, `select`, and `std.task` (`sleep`, `yield_now`); milestone 91, the first of v17.

**Status:** approved 2026-10-16 ("yes approved, build it", session 201); built in session 201. Proposed in session 200, after discovery in sessions 195–200.
**Milestone:** 91 (tag `m91` at merge).
**Spec:** `Language v17 Draft.md` decisions 1–13 (the user's choices are 1–9; 10–13 are mine, for review); v17 spec §2–§11 as listed in the draft.

**Decisions (discovery, sessions 195–200; the user's choices):**
- M:1: every green thread on the one OS thread; no parallelism yet.
- Scope: `spawn`, `join`, `yield_now`, `sleep`, `Chan<T>` with `select`. Non-blocking I/O later; actors later in v17.
- `spawn` takes only a call; Go's `go func() { … }()` is `spawn fun() { … }();`.
- Like Go: a panic in any green thread ends the program; `main` returning ends it.
- Channels like Go: unbuffered `Chan.new()`, `Chan.buffered(n)`, `recv(): Option<T>`.
- Deadlock panics.
- `select` in this PRP: recv arms binding `Option<T>`, send arms, `after(ms)`, `default`, a random pick.
- v16 is frozen; v17 (concurrency) opens with this milestone.
- `Task` and `Chan` predeclared; `sleep` and `yield_now` in `std.task`.
- 1 MiB reserved stacks with a guard page.
- A call returning nothing gives a plain `Task`.
- No open entries in DECISIONS.md apply.

**Why:** concurrency without async/await and function colouring, and the ground the actor model will stand on.

## OBJECTIVE
```
import std.task;

fun square(n: i64): i64 = n * n;

fun main() {
    let a = spawn square(3);
    let b = spawn square(4);
    println(a.join() + b.join());            // 25

    let ch: Chan<i64> = Chan.buffered(2);
    spawn fun() {
        for i in 0..3 { ch.send(i); }
        ch.close();
    }();
    while let Some(x) = ch.recv() { println(x); }   // 0 1 2

    let slow: Chan<string> = Chan.new();
    select {
        m = slow.recv() => println(m),
        after(50) => println("timed out"),
    }
    task.sleep(10);
}
```
Ordinary functions stay ordinary: `square` knows nothing about threads.

## CONTEXT
- **Starting state:** v16 complete (milestones 67–90). One stack, one thread of execution; `runtime/stack.c` turns a fault near the main stack's end into `panic: stack overflow`; closures are `{ code, env }` with a GC environment (`codegen/closures.rs`); `Ref<T>` is the model for a predeclared shared handle (`check/refs.rs`, `codegen/refs.rs`); `$os.*` built-ins back `std` modules (`check/float_ops.rs`, `codegen/os.rs`). `lugha_rt_spawn` already names the process starter (std.process), so green-thread runtime symbols are `lugha_rt_task_*` and `lugha_rt_chan_*`.
- **Ending state:**
  - **Specs:** v16 frozen; `Language v17 Specification.md` starts from v16 plus milestone 91; `lughac spec` prints v17 (`src/lib.rs` SPEC); `Language v17 Draft.md` marked approved and built.
  - **Lexer/parser:** keywords `spawn`, `select`; `Expr::Spawn { call }`, `Expr::Select { arms }` with `SelectArm::{Recv, Send, After, Default}`; E0211 (new) for a `spawn` without a call and a malformed arm; partial trees keep working; s-expression printer.
  - **Checker:** `check/tasks.rs` (spawn typing, `Task`/`Task<T>`, `join`, E0438 for `mut self`/built-in mutators), `check/channels.rs` (`Chan` predeclared, its API), `check/select.rs` (arm typing, the E0438 arm rules, arm value types like `match`); `Checked.spawns` (each site's entry), `Checked.uses_tasks`; members for completion (`check/members.rs`), hover and signature help through the existing tables.
  - **Codegen:** `codegen/tasks.rs` (each `spawn` site → an entry `lugha_task_<enclosing>.<n>` taking its env, calling the target, storing the result in the task; `join`), `codegen/channels.rs` (element size and copy sites), `codegen/select.rs` (a case table → `lugha_rt_task_select`, then a switch to the arms), `codegen/preempt.rs` (the countdown at loop back edges and function entries, only when `uses_tasks`; `"probe-stack"="inline-asm"` on functions then, so a large frame can't skip a guard page).
  - **Runtime:** `runtime/tasks.c` (new fragment): green-thread records, an `mmap`'d 1 MiB stack plus guard page each, an x86-64 context switch (callee-saved registers, `rsp`, MXCSR and the x87 control word) in a top-level `__asm__` block, the FIFO run queue, sleepers ordered by deadline (`clock_gettime(CLOCK_MONOTONIC)`, `nanosleep` when only sleepers remain), deadlock detection, `join`; channels (ring buffer of fixed-size slots, queues of waiting senders and receivers, `close`), `select` (random pick with the runtime's own xorshift seeded from `lugha_rt_os_seed`); Boehm: `GC_set_stackbottom` on every switch and `GC_set_push_other_roots` pushing each suspended stack from its saved `rsp` to its top. `runtime/stack.c`: a fault in any green thread's guard page is a stack overflow too.
  - **std:** `src/std/task.la` (`sleep`, `yield_now` via `$task.sleep`, `$task.yield`), in `lugha::STD`.
  - **Formatter:** `spawn` and `select` (arms like `match` arms); `fmt_corpus` covers the new programs.
  - **Language server:** keywords in completion; hover on `Task`, `Chan` and their methods; doc comments on `std.task`.
  - **Docs:** `docs/errors/E0211.md`, `E0438.md`; CHANGELOG, CLAUDE.md tree and STACK line, README.
- **Related existing code:** `src/parser/{expr,primary,closure,conditional}.rs`, `src/check/{refs,closures,matching,fun_values,float_ops,members}.rs`, `src/codegen/{closures,refs,control,function,os,matching}.rs`, `runtime/{lugha_rt,stack,random}.c`, `src/link/mod.rs` (RUNTIME_PARTS), `src/formatter/exprs.rs`.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- Test-first, as listed below; new code in new files; every touched file stays under 500 lines (a split proposed first if not).
- The VS Code grammar and snippets for `spawn`/`select` are a follow-up extension change (as session 193 was for milestones 87–90), not this PRP.
- Interleavings in tests are deterministic (decision 10), so `.stdout` expectations are exact; tests with `select` among several ready arms assert only what every pick allows.
### Must NOT Do
- No second OS thread, no `pthread_create`, no locks or atomics: M:1 is the decision.
- No async/await, no `async` marker on functions, no change to how ordinary calls are lowered.
- No signals or timers for preemption (decision 10); no change to IR of programs that never use `spawn`.
- No catching panics per thread, no unwinding, no supervision (actors' milestone).
- No non-blocking I/O, no change to `runtime/input.c`, `process.c`, or `src/link/`'s running of tools.
- No growable or copied stacks; no `malloc` from generated code (all GC objects through `lugha_rt_alloc`; stacks are `mmap`'d by the runtime, which is not generated code).
- No actors, no `Mutex`, no `WaitGroup`.
### New dependencies
- None (Boehm's `GC_set_stackbottom` and `GC_set_push_other_roots` are in the libgc already linked).

## ERROR HANDLING REQUIREMENTS
- **E0211** (new, parser): `spawn` followed by something other than a call (`spawn x`, `spawn 1 + f()`); a `select` arm that isn't `p = e.recv()`, `e.send(v)`, `after(e)` or `default`.
- **E0438** (new, checker): a `select` with no recv or send arm; two `default`s; two `after`s; `after` with `default`; `recv`/`send` arms on a value that isn't a `Chan`; `spawn` of a `mut self` method call or of `push`/`pop`/`clear`/`swap`.
- Existing codes for the rest: E0403 (a sent value, an `after` duration, arm values of different types), E0407 (`join()` of a plain `Task` used as a value), E0404 (`==` on `Task`/`Chan`), E0419 (`Task`/`Chan` for `T: Eq`).
- Runtime panics (exit 101): `deadlock: every green thread is waiting`; `send on a closed channel`; `close of a closed channel`; `negative channel buffer`; `stack overflow` in a green thread.
- Internal errors (exit 2): none new; the runtime's `mmap` failing is `panic: out of memory`, as an allocation failure is today.

## SECURITY CONSIDERATIONS
- No `unsafe` Rust, no new crate, no external input. The runtime gains hand-written x86-64 assembly for the context switch (C, not Rust; the user may want to review it like other runtime changes). A wrong switch corrupts the program, so it is tested directly (registers survive a switch, floats keep their rounding mode) and every e2e program runs at `-O0` and `-O2`.
- Guard pages are `PROT_NONE`; stacks are `MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE`. Stacks of finished threads are reused or unmapped, never left readable after a thread is joined and dropped.

## TESTS TO WRITE
- [ ] Lexer/parser: `spawn f(x)`, `spawn m.f(x)`, `spawn obj.method(x)`, `spawn fun() { … }()`, `spawn (fun(): i64 = 1)()`; every E0211 case; `select` with each arm kind, trailing commas; `after`/`default` still names outside `select`; partial trees.
- [ ] Checker: `Task<T>` / `Task` typing; `join` of each; `spawn` evaluating arguments; every E0438 case; `select` arm types and bindings; `Task`/`Chan` not `Eq` (E0404, E0419); a module's own `Chan` shadows the predeclared one.
- [ ] Runtime (`tests/tasks_runtime.rs` building a C harness against `lugha_rt.c`): context switch keeps callee-saved registers and MXCSR; FIFO order; sleepers wake in deadline order; deadlock detection.
- [ ] e2e `tests/programs/m91/`: fan-out and join; join twice; void `Task`; spawn of a closure literal with captures and a `Ref`; unbuffered ping-pong; buffered producer/consumer; `close` and draining; `try_recv`; `select` with recv, send, `after`, `default`; `sleep` order; `yield_now` interleaving; a busy loop preempted (another thread still prints); deep recursion in a green thread (stack overflow panic, exit 101); 10 000 threads; panic in a spawned thread (exit 101, stderr); `main` returning with threads still running; deadlock (exit 101); send on closed, close twice; a GC-heavy program across many threads (objects kept alive only by suspended stacks survive collection); every E0211/E0438 rejection.
- [ ] IR: a program without `spawn` is byte-identical to v16's output (an existing corpus program compared).
- [ ] Formatter: `spawn`/`select` round-trip, idempotent; `fmt_corpus` passes.
- [ ] Language server: completion of `Chan` members; hover on `t.join()`.
- [ ] `tests/explain.rs`: E0211 and E0438 pages and their examples.
- [ ] §10 programs extracted by `tests/spec.rs`.

## ROLLBACK PLAN
- Branch to return to: `main` at the commit before the merge.
- Built on branch `prp-122-green_threads` in a git worktree and merged through a PR; abandoning it means not merging, and v16 stays the current spec.

## ACCEPTANCE CRITERIA
- [ ] The OBJECTIVE program prints `25`, `0`, `1`, `2`, `timed out` and exits 0 at `-O0` and `-O2`.
- [ ] Every test above written first and passing.
- [ ] `lughac spec` prints the v17 spec; v16 frozen.
- [ ] All existing tests pass; programs without `spawn` produce unchanged IR.
- [ ] `cargo clippy --all-targets -- -D warnings` passes
- [ ] `cargo fmt --check` passes
- [ ] Every `pub` item has a doc comment
- [ ] CHANGELOG.md updated

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo run -q -- run tests/programs/m91/fan_out.la` and the OBJECTIVE program, by hand, at `-O2`

## BUILT (session 201)
- As planned, on branch `prp-122-green_threads` in a worktree. Runtime first (`runtime/tasks.c` fibers and scheduler, `spawn.c`, `chans.c`; each fragment under 300 lines), with `tests/tasks_runtime.rs`, a C harness against `lugha_rt.c` (13 cases).
- **`spawn` lowering:** the parser wraps the call in a closure with no parameters (`ExprKind::Spawn`), so the checker's capture rules and codegen's closures do the work. Arguments that compute something, a receiver that is a call and a callee that isn't a name, member or closure literal are evaluated first by the spawning thread and handed to the closure as hidden captures (`Lowerer::presets`). A receiver like `xs[f()].m()` (an index, not a call) is evaluated in the new thread.
- **E0438 for a `mut self` call** replaces the closure's own E0511/E0501/E0502/E0508 at the call, which would talk about a wrapper the user never wrote.
- **Not done from the plan:** `"probe-stack"="inline-asm"` on functions. The guard area is 64 KiB instead (the main stack's overflow window), so only a single frame larger than 64 KiB could skip it.
- **`select`'s pick** is pseudo-random with a fixed seed (xorshift), the same on every run, which keeps test output exact.
- **`std.process`:** its private `spawn` and the `$os.spawn` built-in are renamed `start`/`$os.start`, since `spawn` is a keyword; `runtime/process.c` is unchanged.
- **Language server:** `spawn`/`select` in keyword completion; `join`, `send`, `recv`, `try_recv`, `close` in member completion. Hover over a spawned call shows the call's type (the call's id comes before its closure's).
- **Tests:** runtime harness (13), lexer (1, `tests/lexer.rs`), parser (7), formatter (2), checker (16: tasks, channels, select), codegen (3), members (1), e2e `tests/programs/m91/` (20 runs, 5 rejections), the two §10 programs, `tests/explain.rs` (E0211, E0438 pages). All 20 runs agree at `-O0` and `-O2`.
- **VS Code:** highlighting and snippets for `spawn`/`select` are a separate extension change (TODO).

