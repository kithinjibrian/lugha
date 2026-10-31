# Lugha v17 — Draft (delta against v16)

**Status:** milestone 91 (green threads) approved in session 201 (proposed in session 200, after discovery in sessions 195–200), and built. Milestone 92 (actors) approved in session 214 (proposed in session 213, after discussion in sessions 207–212), and built. Milestones 93 (struct update syntax) and 94 (variants as functions) approved in session 216 (proposed in session 215); 93 built in session 216, 94 in session 217. Milestones 95–97 (formatter hugging, inference from siblings, packages and tests) approved in session 220 (proposed in session 219); 95 built in session 220, 96 in session 221, 97 in session 222. Milestone 98 (module doc comments, PRP-130) proposed in session 224, approved and built in session 225. Milestone 99 (package entry modules, PRP-131) proposed in session 227, approved and built in session 228. Milestone 100 (glob re-exports, PRP-132) proposed in session 232, approved and built in session 233. Milestone 101 (maps and sets, PRP-133) proposed in session 243, approved and built in session 244. Milestone 102 (`out` parameters, PRP-134) proposed in session 248, approved and built in session 249. Milestones 103–105 (`ptr`/`mut`, arrays and unions, C memory; PRP-135–137) proposed in session 250, approved and built in session 251. Milestone 106 (nullable handle fields, plain-data unions; PRP-138) proposed in session 252, approved and built in session 253. Milestone 107 (the unit type; PRP-139) proposed in session 254, approved and built in session 255. Milestone 108 (float bits; PRP-140) proposed in session 258, approved and built in session 259. Milestone 109 (global constants; PRP-141) proposed in session 260, approved and built in session 261. Milestone 110 (`std.net`; PRP-142) proposed in session 265, approved and built in session 266. Milestone 111 (`std.http`; PRP-143) proposed in session 267, approved and built in session 268. Milestone 112 (TLS; PRP-144) proposed in session 269, approved and built in session 270. Milestone 113 (`std.crypto`; PRP-145) proposed in session 272, approved and built in session 273. Milestone 114 (`std.toml`; PRP-146) proposed in session 275, approved and built in session 276. Milestone 115 (`fs.rename`, `fs.set_mode`, `flush`, `std.time`; PRP-147) proposed in session 277, approved and built in session 278. Milestone 116 (`fs.sync`; PRP-148) proposed in session 279, approved and built in session 280. Milestone 117 (octal literals; PRP-149) proposed in session 281, approved and built in session 282. Milestone 118 (`and_then`; PRP-150) proposed in session 281, approved in 282, built in session 283.
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

## Decisions behind milestone 111 (session 267; approved in session 268)

HTTP/1.1 on `std.net`, written in Lugha. Details in PRP-143.

153–166. **`std.http`** (milestone 111): `serve(addr, handler)` with keep-alive, one green thread per connection, strict limits (8 KiB line and headers, 100 headers, 1 MiB body, 30 s) and smuggling-safe framing; `Request`/`Response` with `Map` headers; a client for `http://` (`get`, `post`, `request`); a handler's panic ends the program (documented); no TLS, HTTP/2, streaming, redirects or cookies; tag `m111`. Needs the user's security sign-off.

## Decisions behind milestone 112 (session 269; approved in session 270)

`https://`: TLS through the system's OpenSSL 3. Details in PRP-144.

167–180. **TLS** (milestone 112): `std.tls` (`connect`, `connect_with(opts)`, `listen(addr, cert, key)`, `Conn`/`Listener` shaped like `std.net`'s) and `https://` in `std.http` (`serve_tls`, `request_tls`); OpenSSL linked only into programs that load `std.tls` (E0332 when it's missing); always-on verification with the system roots and host names, `ca_file` to trust more, TLS 1.2+; non-blocking through `wait_fd`; no mutual TLS, pinning or verification bypass; tag `m112`. Needs the user's security review.

## Decisions behind milestone 113 (session 272; approved in session 273)

The user's brief: hashing and signatures for Rapidkoi, over the OpenSSL `std.tls` already links. Details in PRP-145.

181–197. **`std.crypto`** (milestone 113): `sha256` and a streaming `Sha256` (its OpenSSL state freed by a GC finalizer), Ed25519 keypair/public/sign/verify with raw 32/32/64-byte keys and signatures, `random_bytes` (≤ 1 MiB), constant-time `equal`; `CryptoError { BadLength, OpenSsl }`; nothing panics on input; C-side secrets wiped, Lugha-side copies documented as not; OpenSSL linked only into programs that load `std.crypto` or `std.tls`; no encryption, key exchange, key derivation or other algorithms; tag `m113`. Needs the user's security review before merging.

## Decisions behind milestone 114 (session 275; approved in session 276)

The user's brief: reading configuration files, first Rapidkoi's `rapidkoi.toml`. Details in PRP-146.

198–214. **`std.toml`** (milestone 114): a read-only TOML 1.0 parser in Lugha — `parse(text): Result<Map<string, Value>, TomlError>`, `Value { Text, Integer, Float, Boolean, OffsetDateTime, LocalDateTime, LocalDate, LocalTime, Array, Table }`, tables in file order, every definition rule, `TomlError { kind, detail, line, column }`, limits on size (1 MiB), depth (100) and values (1 000 000), overflow-checked integers, `get(table, "a.b")` and `as_*` helpers; tested against toml-test v2.2.0's TOML 1.0 cases (MIT, vendored); no writing, TOML 1.1, struct mapping or file reading; tag `m114`.

## Decisions behind milestone 115 (session 277; approved in session 278)

The user's brief: keeping data on disk safely and a key file private. Details in PRP-147.

215–224. **`fs.rename`, `fs.set_mode`, `flush()`, `std.time`** (milestone 115): atomic `rename(2)` (cross-device is `Err`), `chmod(2)` with modes 0–4095 (no octal literals: `0b110_000_000` for 0600), a `flush()` built-in for stdout, and `std.time.now_ms`/`monotonic_ms`; nothing panics; no locking, other permission APIs, time zones or formatting; tag `m115`.

## Decisions behind milestone 116 (session 279; approved in session 280)

The user: "fs.sync". Details in PRP-148.

225–230. **`fs.sync`** (milestone 116): `fsync(2)` on a file or folder, so a written file (`sync` it) and a rename (`sync` its folder) survive a power cut; `Err` with the system's message; no `write_atomic` helper yet; tag `m116`.

## Decisions behind milestones 117–118 (session 281; approved in session 282)

The user's follow-ups. Details in PRP-149 and PRP-150.

231–236. **Octal literals** (milestone 117): `0o600`, underscores between digits; a leading zero (`0600`) warns W0107 and stays decimal; tag `m117`.

237–240. **`and_then`** (milestone 118): on `Option` (`fun(T): Option<U>`) and `Result` (`fun(T): Result<U, E>`), in the prelude; tag `m118`.

## Decisions behind milestone 119 (session 284; approved and built session 285)

The user's brief: Rapidkoi's bundle fetch needs `Limits` and a test CA in one call. Details in PRP-151.

241–244. **`http.request_tls_with`** (milestone 119): `Limits` as given plus `tls.Options` (`ca_file` trusted, a non-zero `timeout_ms` overriding); tag `m119`.

## Decisions behind milestone 120 (session 286; approved and built session 287)

The user's brief: Rapidkoi derives each app's Ed25519 seed from one master secret. Details in PRP-152.

245–251. **`crypto.hkdf_sha256`** (milestone 120): RFC 5869 through OpenSSL's `EVP_KDF` "HKDF", lengths 1–8160 (`BadLength` otherwise), the C copies wiped; tag `m120` after the user's review.

## Decisions behind milestone 121 (session 289; approved and built session 290)

The user's brief: Rapidkoi's PRP-013 refuses names pointing to the local network without DNS rebinding. Details in PRP-153.

252–261. **`net.resolve` and `tls.Options.address`** (milestone 121): every address a name resolves to; a connection to a chosen IP address that still verifies the name, in `std.tls` and `std.http`; tag `m121`.

## Decisions behind milestone 122 (session 292; approved and built session 293)

The user's brief: "add a concat method to arrays"; discovery chose the operator. Details in PRP-154.

262–268. **Joining arrays** (milestone 122): `xs + ys` on two arrays of one type is a new array (E0404 otherwise), lowered to the prelude's `$concat`; `T[]` satisfies `Add`, and a `T: Add` instance at an array type joins; `+=` joins arrays and strings; no `concat` method, no `xs + element`; tag `m122`.

## Decisions behind milestone 123 (approved and built session 294)

The user's brief: allow a struct field and a method of the same name. Rust (checked with rustc 1.99.0) accepts every pair but silently prefers the method over a function field and the variant over an associated function; the user chose a principle instead. Details in PRP-155.

269–275. **Shared member names** (milestone 123): two members may share a name only when every use shows which one is meant — a field and a method (`p.name`, `p.name()`) unless the field holds a function; a variant and a `self` method (`Shape.Circle(…)`, `s.Circle()`) but not an associated function; E0310 for the rest, reworded; tag `m123`.

## Decisions behind milestone 124 (session 295; approved and built session 296)

The user's report: "lughac test is slow" — kivuli's 46 test files were 46 builds of the whole package. Details in PRP-156.

276–284. **Shared test builds** (milestone 124): the test files of one folder under a `lugha.toml` are one program of several roots (`modules::load_all`), each the module named by its stem; checked together, or each alone to find a broken file; one executable; `--filter` builds only matching files; a panicking constant fails every test of its executable; tag `m124`. Grouping narrowed from "by package" to "by package and folder" in session 296 (the user's choice).
## Decisions behind milestone 125 (session 297; approved and built session 298)

The user's report: "lughac test is slow"; measured in sessions 296–297: 90% of `llc -O0` time is the DAG instruction selector, because functions returning structs (and whole-value struct operations) can't use the fast one. Details in PRP-157.

285–292. **Struct results through a pointer** (milestone 125): every Lugha function returning a struct, enum, `Option`, tuple or function value takes a hidden first `ptr sret(T)` and returns `void`, done as one pass over the lowered module (`codegen/sret.rs`); calls pass an entry-block slot; C-facing functions and calls into C unchanged; measured before and after; tag `m125`. Whole-value loads, stores, `insertvalue` and `extractvalue` inside bodies are the next step.

## Decisions behind milestone 126 (session 299; approved session 300, extended and built session 301)

Step 2 of aggregates through memory. Details in PRP-158. Splitting alone measured no gain on kivuli (its big types have 300–608 leaves); the user extended the PRP (decisions 304–306).

293–306. **Struct values split into fields** (milestone 126): every struct value of ≤ 64 leaves becomes its leaves (`codegen/split.rs`, `split_leaves.rs`), struct parameters too (`split_params.rs`); bigger structs copied by `memcpy` and read field by field when those are their only uses; `bool` results `zeroext`; leftover declarations deleted (`rebuild.rs`, one approved `unsafe`); tag `m126`.

## Decisions behind milestone 127 (session 304; approved and built session 305)

After milestones 124–126, `cc` was most of a kivuli build: lugha-gtk's five `[link]` C files (~2.8 s) and the runtime. Details in PRP-159.

307–316. **The C object cache** (milestone 127): `[link]` C files' and the runtime's objects kept in `<cache root>/objects/<key>/`, reused when source, flags, compiler, lughac version, object and every `cc -MD` header are unchanged (exact comparison, FNV-1a fingerprints, no new crate); never a build failure; no eviction; tag `m127`.

## Decisions behind milestone 128 (session 306; approved and built session 307)

Rapidkoi ships its signed new-tab bundle inside its binary, and its icons as text; until now only by pasting bytes into source. Details in PRP-160.

317–326. **Included files** (milestone 128): `include_bytes(path): u8[]` and `include_str(path): string`, a string-literal path relative to the naming file, constant expressions usable anywhere; the file inside the package's folder after links are resolved, a regular file of at most 16 MiB (E0447), `include_str` UTF-8 (E0448); read once by the checker (so `check` and the language server report errors), the bytes handed to codegen; private read-only globals, the `u8[]` one born shared, and in programs with includes a copy site's shared mark of an already-shared array goes to a sink word; hover shows the size, go-to-definition opens the file; the names reserved (E0302); tag `m128`.

### `std.json` (milestone 137)

JSON for programs behind `std.http`, after discovery in session 327. Details in PRP-169.

327–343. **`std.json`** (milestone 137): strict RFC 8259 `parse`/`parse_with` to `Value { Null, Bool, Integer(i64), Float(f64), Text, Array, Object(Map) }` (exact integers, keys in order); duplicate keys, lone surrogates, a BOM and numbers past `f64` refused; `JsonError { kind, detail, line, column }`; limits on size (1 MiB), depth (100) and values (1 000 000); `stringify`/`pretty` returning `Result`, `NotFinite` for NaN and infinities; `get`, `at`, `as_*`, `is_null`; tested against JSONTestSuite (MIT, vendored); no struct mapping, JSON5, streaming or `std.http` change; tag `m137`.

### JSON in `std.http` (milestone 138)

After discovery in session 329, revised in session 330. Details in PRP-170.

344–351. **JSON in `std.http`** (milestone 138): `Request.json()`/`Response.json()` parse a body with `json.parse`, no `Content-Type` check, non-UTF-8 a `Syntax` error; `http.json(status, v: json.Value)` replaces `json(status, text)`, a 500 `text/plain` reply for NaN or an infinity; `post_json(url, v): Result<Response, string>`; no `request_json` or `_with`/`_tls` variants; tag `m138`.

### JSON and structs (milestones 139–140)

After discovery (PRP-171): JSON for any data type, the per-type code written by the compiler.

352–360. **`json.encode`** (milestone 139): `encode<T: json.Json>(x: T): Value`; `json.Json` a built-in bound met structurally (E0450 naming the path otherwise); externally tagged enums, `None`/`Some` as `null`/the value, maps with string or integer keys, all fields; `u64` past `i64.MAX` a `Float`; walkers per struct, enum and tuple (`lugha_json_enc_<type>`) and Lugha helpers in `std.$json_derive`; tag `m139`.

361–368. **`json.decode`** (milestone 140): `decode<T: json.Json>(v: Value): Result<T, DecodeError>`, `T` from context; unknown keys ignored, a missing `Option` field `None`, exact numbers (a `Float` never an integer), `DecodeError { path, detail }` printing as its message; walkers per struct, enum and tuple (`lugha_json_dec_<type>`) and helpers in `std.$json_from`; `?` passes its expected type to its operand (`Result<T, _>` or `Option<T>`), added during the build (the user, PRP-171 amendment); tag `m140`.

### Derive blocks (milestone 141)

After discovery (PRP-172): Lugha meets metaprogramming needs with targeted features (DECISIONS.md 208); following a type's shape comes first.

**Derive blocks** (milestone 141): `$derive[<params>] I for X { items }`, written only in bundled sources and only in interface `I`'s own module, gives `I`'s items to one type (`i64`, `T[]`, `Option<T>`, `Map<K, V>`) or to every `struct`, `enum` or `tuple` whose parts meet `I`; a type meets a derivable interface through the block that covers it (E0450's paths for `Json`, E0419 otherwise). The field intrinsics `$derive.fields`, `build`, `variant`, `variants` and `build_variant` call a generic function once per field, unrolled per type. `json.encode`/`json.decode` and `json.Json` move onto blocks in `std.$json_value`, byte for byte; the compiler's JSON walkers and `std.$json_derive` are removed. Programs see no change: `derive` stays a name. Opening derive blocks to every module is a later PRP; tag `m141`.

**Derive blocks for every module** (milestone 142, PRP-173): `derive` is a contextual word starting a block in any module, for an interface that module declares (E0452 elsewhere or for overlapping blocks); the intrinsics are written `derive.fields(…)` and so on (E0453 when misused); a struct's or enum's own method with an item's exact signature replaces that item, per item, and one writing every item needs no block; a type that doesn't meet a derivable interface is E0451 with a path. `json.Json`'s items become `to_json` and `from_json`, so a type customises its JSON (breaking for a type that already has methods of those names); tag `m142`.

**Hashing on derive blocks** (milestone 143, PRP-174): `$hash.value` and its per-type IR give way to the prelude's derivable `$Hash` (`prelude/hash.la`), which `Eq` (and `Num`) brings with it, so `K: Eq` stays all a map needs; `$hash.text(s, h)` stays built in for strings' bytes. Derive blocks gain `derive.fold(x, init, f)` (no allocation), `derive.index(x)` and a fourth kind, `derive … for handle`, with `derive.address(x)`. Maps and sets hash order-independently, fixing equal sets built in different orders being two keys; tag `m143`.

**`==` through `Eq`** (milestone 144, PRP-175): `Eq` becomes the prelude's derivable interface with `eq` and `hash` (`prelude/eq.la`, replacing milestone 143's `$Hash`); `==` on anything but numbers, `bool`, `char`, `string` and handles calls it; a struct or enum writing both `eq` and `hash` decides its own equality (reversing v15's "`==` stays structural"), E0454 for one alone; a type that isn't `Eq` is E0451 with a path at `==` and at `T: Eq`; derive blocks gain `derive.all(a, b, f)`; `codegen/equality.rs`'s per-type helpers go; tag `m144`.
**Calendar dates and the local offset** (milestone 145, PRP-176, at Rapidkoi's request): `std.time` gains `Civil`, `TimeError`, `civil(ms, offset_s)`, `from_civil(c, offset_s)` (pure, proleptic Gregorian, any `i64` moment) and `local_offset(ms)` (`localtime_r`'s `tm_gmtoff`, through a new `$os.local_offset` and `lugha_rt_local_offset`); offsets in seconds; tag `m145`.
**File streams** (milestone 146, PRP-177): `std.fs` gains `File` from `open`, `create` and `append`, with `read(max)`, `read_line()`, `write(bytes)`, `write_text(s)` and `close()`; unbuffered writes, modes fixed at opening, no seeking; through `$os.file_*` and `runtime/file.c`; tag `m146`.
**Constants computed by pure code** (milestone 147, PRP-178): a `const`'s value is any pure expression — calls, `if`, `match`, blocks, closures, interpolation — with purity found through calls at real type arguments (no printing, I/O intrinsics, `$os`, `extern`, `spawn` or `Chan.new`); E0445 names the path; dependencies and E0446 follow calls; still computed at startup, never while compiling (decision 212); tag `m147`.
**Inference through result methods** (milestone 148, PRP-179): milestone 96's rule widens from "the method returns the receiver's own type" to "the method's result mentions the receiver type's parameters"; the expected type matched against the result fixes the receiver's, the rest left open. Covers `unwrap`/`expect` on `Option` and `Result` (`let p: Person = json.decode(v).unwrap();`) and a program's own methods; such a call waits for its siblings too; tag `m148`.
**Handing over arguments** (milestone 149, PRP-180): liveness finds parameters a function keeps (stored on their last use, or handed on to a keeping parameter; fixed point over calls), call arguments on their last use (and `x` inside the value of `x = …`, loops or not), and walks methods and derive items too; a keeping function also compiles as `lugha_fn_<symbol>.$own`, its kept parameters owned, called only by direct calls that share what they still need; every other path calls the ordinary instance; tag `m149`.
**Inference through iterator chains** (milestone 150, PRP-181): an element hint (`Checker::element_hints`, by receiver id) from `collect`, `collect_set` and `collect_map`'s expected type, passed back by `filter`, `take`, `skip`, `take_while` and `skip_while`, read only by the helper `map`, whose `U` it fixes before the closure is checked; not an expected type, which would convert the receiver to `Iter`; tag `m150`.
**`defer`** (milestone 151, PRP-182; design note `docs/decisions/defer.md`): `defer e;`, `defer place op value;` (PRP-183, the inner statement an `Expr` or `Assign`) or `defer { … }`, block-scoped, the whole statement run at exit, last first, on every exit but a panic (no unwinding; an actor's panic leaks what its defers would close); a tail or `return` value computed (and copied) first; E0455 for leaving a deferred statement, E0456 for one with a value; no `errdefer`; `defer` a keyword; lowered inline at each exit (`codegen/defer.rs`); tag `m151`.
**`loop` and labels** (milestone 152, PRP-185, from the benchmark suite's `done` flags and the `while true` known issue): `loop { … }` a block-like expression, `never` when no `break` targets it (so it counts as returning), otherwise its breaks' type (expected type first, else the first value); `break [label] [value];`, `continue [label];`; Go-style labels (`name:`) on `loop`, `while`, `while let` and `for`, apart from variables (after `break` a name is the label only when an enclosing loop has it); no labelled blocks; labels don't cross closures; `break value` only out of a `loop` (E0514); E0512 unknown label, E0513 a label reused inside its loop; `while true` unchanged, E0503's help points to `loop`; `loop` a keyword; tag `m152`.
**`fs.size`** (milestone 153, PRP-186, from Rapidkoi's request to refuse a large file before reading it): `fs.size(path): Result<i64, string>`, `stat(2)`'s `st_size` for a regular file, links followed, nothing opened; a folder `Err("Is a directory")`, another kind `Err("not a regular file")`, else the system's message; `$os.file_size` through the `file_` lowering. Not a `metadata` struct, modification times or `lstat` yet.
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
