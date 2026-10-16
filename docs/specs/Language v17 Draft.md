# Lugha v17 — Draft (delta against v16)

**Status:** milestone 91 (green threads) approved in session 201 (proposed in session 200, after discovery in sessions 195–200), and built. Milestone 92 (actors) approved in session 214 (proposed in session 213, after discussion in sessions 207–212), and built. Milestones 93 (struct update syntax) and 94 (variants as functions) approved in session 216 (proposed in session 215); 93 built in session 216, 94 in session 217.
**Base:** `Language v16 Specification.md` (milestones 67–90), frozen when milestone 91 started. `Language v17 Specification.md` starts from it and grows milestone by milestone; it is what `lughac spec` prints.

**Theme:** concurrency without async/await. Every function stays an ordinary function: a green thread pauses only where it waits (`join`, a channel, `select`, `sleep`), and the scheduler preempts long loops on its own, so nothing is marked `async` and nothing is awaited: no function colouring. Milestone 91 brings green threads and channels. Actors, which build on them (a green thread owning a channel as its mailbox), follow in a later v17 milestone.

This draft lists only what changes, in the order of the v16 sections. Everything not mentioned keeps its v16 meaning.

**Compatibility.** Every valid v16 program stays valid unless it uses `spawn`, `select` or (from milestone 92) `actor` as a name (keywords now). A module's own `Task` or `Chan` still means its own, as for `Ref`. `after` and `default` are keywords only at the start of a `select` arm.

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

## Decisions behind milestone 92 (discussion, sessions 207–212)

The user's choices:

14. **An `actor` declaration**, not a library of channels and message enums: the compiler writes the mailbox, the message loop and the reply plumbing.
    ```
    public actor Counter {
        count: i64,
        fun new(start: i64): Self = Self { count: start };
        fun add(mut self, n: i64) { self.count += n; }
        fun get(self): i64 = self.count;
    }
    ```
    Fields and methods are declared together in the actor's body. An actor runs on its own green thread and handles its messages one at a time, so its state is never touched by two calls at once.
15. **Calls:** calling a method on a handle sends a message. A method that returns a value makes the caller wait for the reply, a copy of the result; a method that returns nothing is queued and the caller goes on (Erlang's call and cast). Messages from one caller are handled in the order sent; from several, in the order they arrive. Two actors waiting on each other is the existing deadlock panic.
16. **Stopping:** every handle has a built-in `stop()`, a message like any other: what is queued before it is still handled, then the actor ends. `join(): Option<string>` waits until the actor has ended. Calling a method on an ended actor panics in the caller (`actor `Counter` has stopped`). When `main` returns, every actor ends.
17. **Panics stay in the actor:** a panic inside an actor's method prints its message and ends only that actor. A caller waiting for a reply, and any later value-returning call, panics in the caller (`actor `Counter` panicked`); later void calls panic the same way. `join()` gives `None` for an actor that stopped, `Some(message)` for one that panicked, so a supervisor can be written by hand. Plain `spawn f()` threads keep decision 4: a panic there ends the program.
18. **Actors share nothing:** a `Ref`, or any type that holds or may hold one, in an actor's fields, method parameters or method results is E0439. Function values (a closure may have captured a `Ref`) and interface values (the value inside may hold one) count as holding one. Channels and actor handles may cross, since they are messaging. This keeps a later multi-core step free of data races.

Mine, approved by the user (session 212):

19. **Starting:** `spawn Counter { count: 0 }` or `spawn Counter.new(10)` (an associated function returning `Self`) starts an actor and gives its handle. An actor's state is never a value outside its actor: a literal or `Self`-returning associated call of an actor type anywhere but right after `spawn` is E0439.
20. **The handle's type is the actor's name:** outside the actor, `Counter` means the handle (`fun log(c: Counter)`, `Counter[]`). Handles are shared like channels, aren't `Eq` (`a.is(b)` compares them) and print as `<actor Counter>`. Inside the actor's methods, `self` is the state and `self.helper()` an ordinary call.
21. **Private fields:** reading or assigning an actor's field from outside is E0439; methods are the way in. A method named `stop`, `join` or `is` is E0439.
22. **An unbounded mailbox:** void calls never wait, so the queue grows as needed. A bounded mailbox with waiting senders is later work.
23. **Not in this milestone:** generic actors (`actor Cache<T>` is E0439), actors satisfying interfaces, built-in supervisors, several OS threads.

## Decisions behind milestones 93 and 94 (discovery, session 215)

The two language prerequisites of the kivuli UI framework (`../kivuli/KICKOFF.md`, Elm architecture): struct update syntax and variants as function values (the user's choices: Rust's rules for `..base`; every variant with payloads is a function value, `Some`/`Ok`/`Err` included).

24. **`S { f: e, …, ..base }`:** a struct literal may end with `..base`, an expression of exactly the literal's struct type (type arguments included). Every field not written is taken from `base`.
25. **Evaluation order:** the written fields first, left to right, then `base`, once. Fields taken from `base` follow the normal copy rules (a place's array is shared until written; a local `base` is moved on its last use).
26. **Errors:** a `base` of another type is E0440 (the next unused code when built); `..` anywhere but last, or twice, is E0201; every field written as well as `..base` is a warning, W0104.
27. **Visibility** as for any literal: outside the struct's module every field must be visible, written or not (E0313).
28. **Not:** structural updates from another type; `..` in patterns; tuples (they have no literal syntax).
29. **Success:** milestone 93, tag `m93`. Tests: parser, checker (E0440, W0104, E0313, generic inference), `tests/programs` (order, copies, moves, nesting, generics); byte-identical IR without `..`.
30. **Variants as values:** a variant with payloads, written without a call, is a value of type `fun(P1, …): E` — `Msg.Named` is `fun(string): Msg` — and so are `Some`, `Ok` and `Err`. A variant without payloads stays a value of its enum.
31. **Generic enums:** a variant of a generic enum needs an expected function type to fix its type arguments, as a generic function does (E0412 otherwise); a mismatch with the expected type is E0403.
32. **Names:** variant values resolve as variants do today — `E.V`, `m.E.V`, `m.v.E.V`, through named imports of the enum — with the enum's visibility.
33. **Lowering:** the value is `{ lugha_ref_<Enum>.<Variant>, null }`; the wrapper takes the environment first and builds the variant (indirect slots included), emitted once. It can't collide with a method's wrapper: a method may not share a variant's name (E0310).
34. **Success:** milestone 94, tag `m94`. Tests: checker, `tests/programs` (`map(Shape.Circle)`, `map(Some)`, stored and called later, recursive enums, `Ok`/`Err`), one wrapper per variant; byte-identical IR without variant values.

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

For milestone 92: **§2** keyword `actor`; **§3** `actor_decl = [ visibility ] "actor" IDENT "{" { field "," | fun_decl } "}"`; **§4** actor handles, calls, `stop`/`join`/`is`, private fields, the share-nothing rule; **§5** call and cast; **§6** actors among green threads, panics in actors; **§7** an actor's state lives in its green thread, the mailbox in the GC heap; **§9** lowering (the actor loop, message handlers `lugha_actor_<Actor>.<method>`), runtime `lugha_rt_actor_*`, E0439; **§10** the actors programs; **§11** row 92.

## Milestones

| # | Milestone | Spec subset | Done when |
| --- | --- | --- | --- |
| 91 | Green threads (v17) | `spawn`, `Task`, `Chan`, `select`, `std.task`; preemption; deadlock; E0211, E0438 | The §10 green-threads programs pass; `tests/programs/m91/` |
| 92 | Actors (v17) | `actor` declarations, `spawn` of an actor, call and cast, `stop`, `join`, panics isolated to the actor, share-nothing; E0439 | The §10 actors programs pass; `tests/programs/m92/` |
| 93 | Struct update syntax (v17) | `S { f: e, ..base }`, Rust's rules; E0440, W0104 |
| 94 | Variants as functions (v17) | `Msg.Named`, `Some`, `Ok`, `Err` as function values |
