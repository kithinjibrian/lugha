# Lugha v17 — Draft (delta against v16)

**Status:** milestone 91 (green threads) proposed in session 200, after discovery in sessions 195–200. Not approved.
**Base:** `Language v16 Specification.md` (milestones 67–90). It stays the source of truth, and what `lughac spec` prints, until milestone 91 starts. Then v16 is frozen and `Language v17 Specification.md` starts from it, growing milestone by milestone.

**Theme:** concurrency without async/await. Every function stays an ordinary function: a green thread pauses only where it waits (`join`, a channel, `select`, `sleep`), and the scheduler preempts long loops on its own, so nothing is marked `async` and nothing is awaited: no function colouring. Milestone 91 brings green threads and channels. Actors, which build on them (a green thread owning a channel as its mailbox), follow in a later v17 milestone.

This draft lists only what changes, in the order of the v16 sections. Everything not mentioned keeps its v16 meaning.

**Compatibility.** Every valid v16 program stays valid unless it uses `spawn` or `select` as a name (keywords now). A module's own `Task` or `Chan` still means its own, as for `Ref`. `after` and `default` are keywords only at the start of a `select` arm.

## Decisions behind milestone 91 (discovery, sessions 195–200)

The user's choices:

1. **One OS thread (M:1).** Every green thread runs on the program's one OS thread, so there are no data races: `Ref<T>`, copy-on-write arrays and the collector need no locking. Concurrency, not parallelism. Using more cores is later work, through actors that share nothing.
2. **`spawn` takes a call.** `spawn e(args)` evaluates `e` and the arguments at once, in the spawning thread, by the argument copy rules, then runs the call in a new green thread. There is no block form: a closure literal called on the spot does the job, as Go's `go func() { … }()`:
   ```
   let k = 2;
   let t = spawn fun(): i64 { k * 10 }();
   println(t.join());   // 20
   ```
3. **`Task<T>`.** `spawn` of a call returning `T` is a `Task<T>`; of a call returning nothing, a plain `Task`. `t.join()` waits for the call to finish and gives a copy of its result (nothing, for `Task`). Joining again gives another copy at once. Copies of a task share it, as `Ref` copies do. Tasks aren't `Eq` and print as `<task>`.
4. **Like Go when things end.** A panic in any green thread ends the whole program, with its `panic:` message and exit 101, as panics do today. When `main` returns, the program exits; green threads still running are dropped.
5. **Channels, like Go.** `Chan<T>` is a shared handle like `Ref<T>`:
   - `Chan.new(): Chan<T>` is unbuffered: `send` waits until a receiver takes the value. `Chan.buffered(n: i64): Chan<T>` holds up to `n` values (`n == 0` is unbuffered; `n < 0` panics).
   - `c.send(v: T)` waits for room or a receiver; sending on a closed channel panics.
   - `c.recv(): Option<T>` waits for a value; `None` once the channel is closed and empty, so `while let Some(x) = c.recv() { … }` drains it.
   - `c.try_recv(): Option<T>` never waits: `None` when nothing is ready (or closed and empty).
   - `c.close()` ends the channel; closing it twice panics. `c.is(d)` tells whether both are the same channel.
   - Sent values are copied, as arguments are. Channels aren't `Eq` and print as `<chan>`.
6. **Deadlock panics.** When no green thread can run, none is sleeping and at least one waits, the program panics with `panic: deadlock: every green thread is waiting`, exit 101, like Go's "all goroutines are asleep".
7. **`select`, Go's set plus a timeout.** An expression, typed like `match` (every arm's value the same type):
   ```
   select {
       msg = inbox.recv() => match msg {
           Some(m) => handle(m),
           None => println("inbox closed"),
       },
       out.send(42) => println("sent"),
       after(500) => println("timed out"),
   }
   ```
   - **Arms:** `name = c.recv() => e` (also `_ = c.recv()`) binds `name: Option<T>`, `None` meaning closed; `c.send(v) => e`; at most one `after(ms) => e` (waits at most `ms` milliseconds, an `i64`); at most one `default => e` (runs at once when nothing else is ready, so the `select` never waits).
   - **Order:** on entry, every channel, sent value and `after` duration is evaluated once, top to bottom. Then, if arms are ready, one of them is picked at random, so none starves; otherwise `default` runs if present; otherwise the thread waits until one is ready or the timeout passes.
   - **Rejected:** a `select` with no `recv` or `send` arm, two `default`s, two `after`s, or both `after` and `default` (the timeout could never fire) (E0438).
8. **`Task` and `Chan` are predeclared**, as `Ref` is, because `spawn` and `select` are syntax about them. **`std.task`** holds the rest, loaded only when imported: `task.sleep(ms: i64)` (pauses this green thread; others run; `ms <= 0` just yields) and `task.yield_now()` (lets the others run first).
9. **Stacks:** each green thread reserves 1 MiB of address space, with a guard page below; the system commits only pages it touches. Running off the end is `panic: stack overflow`, exit 101, as on the main stack. `main` runs on the program's own stack, as today.

Decided by me, for review:

10. **Preemption without colouring.** A green thread that loops without waiting must not starve the others. In a program that uses `spawn`, every loop's back edge and every function entry counts down a counter; at zero (every 4096 steps, tuned while building) the thread yields if another is ready. No signals and no timers, so a run's interleaving is deterministic for a given program and input. Programs that never use `spawn` get byte-identical IR.
11. **Blocking I/O blocks every green thread** in this milestone: `read_line`, file and process calls, and `extern` calls run to completion on the one OS thread. Non-blocking I/O is a later milestone.
12. **C and green threads:** an extern called from a green thread runs on that thread's stack (1 MiB). A callback C calls runs in the green thread that called into C; it may spawn and use channels like any code. The v16 rule stands: a callback is called on the OS thread that registered it, which is now always the one thread.
13. **`spawn` of a `mut self` method call** (`spawn xs.push(1)`, `spawn p.move_by(1)`) or of `push`/`pop`/`clear`/`swap` is E0438: the change would happen to a copy nobody sees.

## Changes by section

- **§2:** keywords `spawn`, `select`; `after` and `default` contextual at the start of a `select` arm.
- **§3:** `spawn` *call* is a prefix expression (binding like a unary operator, its operand a postfix call); `select { arm, … }` is a primary expression; arms `pattern = place.recv() => e`, `place.send(e) => e`, `after(e) => e`, `default => e`. A `spawn` whose operand isn't a call, or a malformed arm, is E0211.
- **§4:** `Task<T>`, `Task`, `Chan<T>`: shared handles, not `Eq`, not `Ord`, print as `<task>` / `<chan>`; type rules of `spawn` and `select`.
- **§5:** evaluation order of `spawn` and `select`; when a green thread switches (`join`, `send`, `recv`, `select`, `sleep`, `yield_now`, preemption points).
- **§6:** "Green threads": scheduling, the end of the program, deadlock, panics; `std.task` in "The standard library".
- **§7:** green-thread stacks, guard pages, the collector scanning every stack; tasks and channels as GC objects.
- **§8:** externs and callbacks on green threads (decision 12).
- **§9:** lowering of `spawn` (an entry function per site), `select` (a case table to the runtime), preemption points; runtime functions `lugha_rt_task_*`, `lugha_rt_chan_*`; symbol prefix `lugha_task_<enclosing>.<n>` for spawn entries; E0211 and E0438 rows.
- **§10:** the green-threads programs (fan-out with `join`, a producer and consumer over a buffered channel, a `select` with a timeout, a deadlock).
- **§11:** row 91.

## Milestones

| # | Milestone | Spec subset | Done when |
| --- | --- | --- | --- |
| 91 | Green threads (v17) | `spawn`, `Task`, `Chan`, `select`, `std.task`; preemption; deadlock; E0211, E0438 | The §10 green-threads programs pass; `tests/programs/m91/` |
