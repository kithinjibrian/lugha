# Lugha v17 — Draft (delta against v16)

**Status:** milestone 91 (green threads) approved in session 201 (proposed in session 200, after discovery in sessions 195–200), and built. Milestone 92 (actors) approved in session 214 (proposed in session 213, after discussion in sessions 207–212), and built. Milestones 93 (struct update syntax) and 94 (variants as functions) approved in session 216 (proposed in session 215); 93 built in session 216, 94 in session 217. Milestones 95–97 (formatter hugging, inference from siblings, packages and tests) approved in session 220 (proposed in session 219); 95 built in session 220, 96 in session 221, 97 in session 222. Milestone 98 (module doc comments, PRP-130) proposed in session 224, approved and built in session 225. Milestone 99 (package entry modules, PRP-131) proposed in session 227, approved and built in session 228. Milestone 100 (glob re-exports, PRP-132) proposed in session 232, approved and built in session 233. Milestone 101 (maps and sets, PRP-133) proposed in session 243, approved and built in session 244. Milestone 102 (`out` parameters, PRP-134) proposed in session 248, approved and built in session 249. Milestones 103–105 (`ptr`/`mut`, arrays and unions, C memory; PRP-135–137) proposed in session 250, approved and built in session 251. Milestone 106 (nullable handle fields, plain-data unions; PRP-138) proposed in session 252, approved and built in session 253. Milestone 107 (the unit type; PRP-139) proposed in session 254, approved and built in session 255. Milestone 108 (float bits; PRP-140) proposed in session 258, approved and built in session 259. Milestone 109 (global constants; PRP-141) proposed in session 260, approved and built in session 261.
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

## Decisions behind milestones 95–97 (discovery, session 219)

Fixes for what building kivuli (a package with tests and nested views) turned up in lughac (the user: "fix the rest"; the user's choice: `skip(reason)`; the rest mine, for review).

35. **Hugging:** a call that doesn't fit on its line, whose last argument is an array literal, a closure or a struct literal and whose other arguments fit on the first line, prints as `f(a, [` / one element per line / `])` (the closure's body, the struct's fields likewise); nested calls hug the same way.
36. **The header first:** an expression body breaks inside the expression before the function's parameter list breaks; the parameters break only when the header alone doesn't fit.
37. **Unchanged:** what fits on one line; the formatter's self-check (a failed hug falls back to today's layout).
38. **Success:** milestone 95, tag `m95`. kivuli's views format as nested hugged calls; `fmt_corpus` idempotent.
39. **Inference from siblings:** a call to a generic function whose type parameters appear only in its result is checked after the other arguments of the call it is passed to, expecting what they fixed — as literals and `None` already are.
40. **The same** for the operands of `assert_eq`/`assert_ne` and of `==`/`!=`: the operand that can't be typed alone is checked second, expecting the other's type.
41. **Unchanged:** programs that already checked keep their types; no inference across statements.
42. **Success:** milestone 96, tag `m96`. `assert_eq(text("hi"), expected)` checks.
43. **A package's file is its module:** a file given to `lughac test` or `lughac check` inside a package's `source` folder is built as that package's module (`N.path`) from a hidden entry, not as a main file: cycles through it are allowed, its tests are named `N.path.name`, and no `main` is needed.
44. **`check` without `main`:** `lughac check` reports E0303 only for a file that has no `main` and must be run (never under `check` for a file without `main`, which is checked as a library); `build` and `run` still need `main`.
45. **`test/`, `tests/`, `examples/` belong to their package:** a program whose manifest is its package's own and whose file is under that manifest's folder sees the package's package-level items, as its modules do; `private` items stay private.
46. **One lock per package:** `lugha.lock` lives beside the `lugha.toml` governing the build (beside the main file only when there is none); a lock beside a main file is read once if the manifest's folder has none, then written beside the manifest.
47. **`skip(reason)`:** an intrinsic for tests; the test stops and is reported `skipped (reason)`, counted apart (`N tests: P passed, F failed, S skipped`), `"skipped"` in `--json`, skipped in VS Code's Testing view; exit 0 unless something failed. Outside a test it panics.
48. **Skips never hide failures:** a test that failed before calling `skip` failed; an `exit` with the skip code but no `skip` call failed.
49. **The mechanism:** the test process exits with code 77 after reporting the reason to the runner on its captured output; the runner accepts a skip only with both.
50. **Success:** milestone 97, tag `m97`. Tests through `lughac` for each part; kivuli's own layout simplified.

## Decisions behind milestone 99 (session 227, approved in session 228)

A package's front door (the user asked whether libraries should have an entry like `lib.la` or one declared in the manifest; PRP-131).

51. **The entry is `lib.la`** at the package's module root (its `source` folder, or its folder without one). No manifest key.
52. **`import N;`** binds the entry as `N` (also `as`, `{ X, y }`, `public import N;`); module path `N`, symbols `lugha_fn_N/f`. Without a `lib.la`, `import N;` stays E0312, its help pointing to `lib.la` or `import N.<module>;`.
53. **One name:** `import N.lib;` is E0312 ("the entry is imported as `import N;`"); a `lib/` folder's modules (`N.lib.x`) are unaffected.
54. **Unchanged:** `import N.module;` and visibility; the entry is an ordinary module.
55. **Everywhere a package is:** the implicit package (`examples/`, `test/`) can `import N;`; `lughac test`/`check` take `src/lib.la` as the module `N`; no entry for `std`.
56. **`lughac init --lib`** writes `src/lib.la` and an example with `import N;`; libraries from older `init` (`src/N.la`) keep working as `N.N`.
57. **Success:** milestone 99, tag `m99`.

## Decisions behind milestone 100 (discovery, session 232; approved in session 233)

Glob re-exports (kivuli / lugha-gtk: an entry flattening its C-bindings module; PRP-132). Q1–Q6 answered by the user; 64–66 follow from them, for review.

58. **Syntax:** `{ * }` in an import's braces; `*` stands alone (`{ *, X }` is E0201).
59. **Only re-exports:** `{ * }` needs `public`; `import raw { * };` is E0201.
60. **Coverage:** every `public` item of the module (functions, externs, extern types, structs, enums, interfaces); package-level and `private` items skipped.
61. **Chained:** the module's own named re-exports and globs come along; whole-module re-exports stay members; glob cycles add each item once.
62. **Clashes:** E0314 at the glob, naming the name and both sources, unless the same item; no silent shadowing.
63. **One import per module:** `public import raw;` with `public import raw { * };` stays E0314; the glob also binds `raw` in its file.
64. **The same items:** a glob-re-exported name is the declaration itself.
65. **Editor and formatter:** hover, definition, completion, references and rename reach the declaration; `lughac fmt` keeps `{ * }`.
66. **Success:** milestone 100, tag `m100`.

## Decisions behind milestone 101 (discovery, session 243; approved in session 244)

Maps and sets (the user: "let's work on map and set"; literal syntax and Q1–Q6 answered by the user; PRP-133). In short: `#{k: v}` and `#{x}` literals; `Map<K, V>` and `Set<T>` built in, the iterator type renamed `Mapped`; values like arrays; keys any `Eq` type without floats (E0441), hashed structurally; insertion order; the API of PRP-133 decisions 73–75; `==` regardless of order; `for … of mut` refused (E0442); W0105 for equal literal keys; written in Lugha in the prelude over a `$hash` built-in. Full list: PRP-133 decisions 67–80.

## Decisions behind milestone 102 (discovery, session 248; approved in session 249)

`out` parameters on externs (lugha-wasmtime's need list, items 1, 2 and part of 6; PRP-134). Q1–Q4 answered by the user.

81. **Declaration:** `out name: T` in an `extern fun`'s parameters (C's `T *`); contextual keyword; on a Lugha function's parameter E0201.
82. **Call site:** `out place`; a missing or misplaced `out` is E0443.
83. **The place:** any assignable place (assignment rules); C gets a zeroed slot, and its value is stored into the place after the call.
84. **Types:** integers, `bool`, floats, handles (NULL panics), `Option` of a handle (NULL → `None`), extern structs; others E0409.
85. **Order:** arguments left to right; write-backs after the call, left to right.
86. **Scope:** only `out`; `ptr` and in-out `mut` next.
87. **Success:** milestone 102, tag `m102`.

## Decisions behind milestones 103–105 (session 250; approved in session 251)

The rest of lugha-wasmtime's C-boundary needs (the user: "let's knock them out"). Details in PRP-135, PRP-136 and PRP-137.

88–92. **`ptr` and `mut` parameters** (milestone 103): `ptr e` passes a copy's address (`const T *`); `mut place` passes the place's value's address and writes it back; modes written at the call as at the declaration (E0443).
93–99. **Arrays and unions** (milestone 104): `T[]` extern parameters pass a pointer to the elements for the call, `mut` arrays made unique and written in place; `extern union` with C's layout, one-field literals (E0444), not `==` or printable.
100–106. **C memory and pointer callbacks** (milestone 105): `CPtr`, `c_read`/`c_write` (copy-only; the user's security sign-off required); callback parameters `T[] n` and `mut T[] n`.

## Decisions behind milestone 106 (session 252; approved in session 253)

lugha-wasmtime's probe found no way to start a NULL-able pointer field; session 251 found unions making pointers out of numbers. Details in PRP-138.

107–113. **Nullable handle fields and plain-data unions** (milestone 106): `Option<H>` as an extern-struct field (NULL is `None`); union fields only plain data (no handles, `Option<H>` or `CPtr`, at any depth: E0437); spec §8 states `c_read`/`c_write` as the program's responsibility; tag `m106`.

## Decisions behind milestone 107 (session 254; approved in session 255)

The user: "add unit type" — a real type and value, and a warning for binding one. Details in PRP-139.

114–122. **The unit type** (milestone 107): `()` is a type and its one value and replaces `void` (omitted return types mean `: ()`); it works in generics, variables, fields and arrays, is `Eq`/`Ord`/hashable/printable, takes no memory; E0407 withdrawn; W0106 for `let x = e` binding a `()` that isn't the literal (silenced by `let x: () = e`); not across C (E0409/E0437); tag `m107`.

## Decisions behind milestone 110 (sessions 264–265; approved in session 266)

The user: "wanna build a std http lib?" — sockets with non-blocking I/O first, HTTP/1.1 next, TLS later. Details in PRP-142.

141–152. **`std.net`** (milestone 110): TCP `Listener`/`Conn` in Lugha over `$net.*` built-ins; non-blocking sockets parked in the scheduler through `epoll`, with timeouts; `Result<…, string>` errors; explicit `close()`; blocking name lookup; no UDP, Unix sockets or non-blocking files; tag `m110`. Needs the user's security sign-off.

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
| 95 | Formatter hugging (v17) | trailing array/closure/struct-literal arguments hugged; headers kept |
| 96 | Inference from siblings (v17) | result-only generic calls typed by their siblings; `assert_eq`, `==` |
| 97 | Packages and tests (v17) | package files as modules in `test`/`check`, `test/` inside the package, one lock per package, `skip(reason)` |
| 98 | Module doc comments (v17) | a file's top `/** */` (before a blank line or an `import`) documents its module; hover on module names; std modules documented (PRP-130, approved in session 225) |
| 99 | Package entry modules (v17) | `import N;` imports the package's `lib.la`; `init --lib` writes one (PRP-131, approved in session 228) |
| 100 | Glob re-exports (v17) | `public import m { * };` re-exports every `public` item of `m` (PRP-132, approved in session 233) |
| 101 | Maps and sets (v17) | `Map<K, V>`, `Set<T>`, `#{ … }` literals, iteration, printing, `==`; E0441, E0442, W0105 (PRP-133, approved in session 244) |
| 102 | `out` parameters (v17) | `out x` on extern parameters and call arguments; E0443 (PRP-134, approved in session 249) |
| 103 | `ptr` and `mut` parameters (v17) | `ptr e` and `mut place` on extern parameters and arguments (PRP-135, approved in session 251) |
| 104 | Arrays and unions at the C boundary (v17) | `T[]` extern parameters, `mut` arrays, `extern union`, E0444 (PRP-136, approved in session 251) |
| 105 | C memory and pointer callbacks (v17) | `CPtr`, `c_read`/`c_write`, callback `T[] n` parameters (PRP-137, approved in session 251) |
