## FEATURE: Actors — `actor Name { fields, methods }`, started with `spawn`, called through a handle (value methods wait for the reply, void methods are queued), stopped with `stop()`, a panic isolated to its actor, sharing nothing; milestone 92 of v17.

**Status:** approved 2026-10-17 ("yes", session 214); built in session 214. Proposed in session 213, after discussion in sessions 207–212.
**Milestone:** 92 (tag `m92` at merge).
**Spec:** `Language v17 Draft.md` decisions 14–23 (14–18 the user's choices; 19–23 mine, approved in session 212).

**Decisions (the user's choices):**
- An `actor` keyword, not a library of channels and enums.
- Value methods wait for the reply; void methods are queued (call and cast).
- A built-in `stop()` message, `join()`, everything ends with `main`.
- A panic ends only its actor; waiting and later calls panic in the caller; `join(): Option<string>` reports it; supervision written by hand for now.
- Share nothing: no `Ref` (or anything that may hold one) across an actor's boundary.
- Approved defaults: start only through `spawn` (literal or `Self`-returning associated function); the actor's name is the handle type outside; private fields; built-in `stop`/`join`/`is`; unbounded mailbox; no generic actors yet.
- No open entries in DECISIONS.md apply.

**Why:** state owned by one green thread, reached only by messages; no channel plumbing by hand; failures contained; the ground for supervisors and several cores later.

## OBJECTIVE
```
public actor Counter {
    count: i64,
    fun new(start: i64): Self = Self { count: start };
    fun add(mut self, n: i64) { self.count += n; }
    fun get(self): i64 = self.count;
    fun divide(mut self, by: i64) { self.count = self.count / by; }
}

fun main() {
    let c = spawn Counter.new(10);
    c.add(5);                  // queued; main goes on
    println(c.get());          // waits: 15
    c.divide(0);               // the actor panics; main goes on
    println(c.join());         // Some("division by zero …")
    let d = spawn Counter { count: 0 };
    d.stop();
    println(d.join());         // None
}
```

## CONTEXT
- **Starting state:** milestone 91: green threads on one OS thread (`runtime/tasks.c`, `spawn.c`, `chans.c`), `spawn call`, `Task<T>`, channels, `select`; every runtime panic calls `exit(101)` directly.
- **Ending state:**
  - **Syntax:** keyword `actor`; `Item::Actor(ActorDecl { visibility, name, fields, methods, span })`; E0201 for anything but fields and `fun`s in the body; type parameters after the name parse and are rejected by the checker (E0439). Formatter, s-expressions, partial trees, doc comments on actors and their methods.
  - **Checker** (`check/actors.rs`, new; `check/errors/v17.rs`): the actor's state is a struct (its fields) with the methods as an `impl` (reusing struct and method checking; `Self` and `self` are the state). Outside the actor, the name resolves to `Type::Actor(key)`, the handle. `spawn` of an actor literal or of a `Self`-returning associated call gives the handle; method calls on a handle type-check against the methods (receivers ignored: a `mut self` method needs no `let mut` handle), plus built-in `stop()`, `join(): Option<string>`, `is(other)`. E0439 for: a field read or written through a handle; an actor literal or state-returning call not right after `spawn`; a method named `stop`, `join` or `is`; type parameters; a `Ref`, function or interface type (or one holding them) in a field, parameter or result. Handles not `Eq`; print `<actor Counter>`; visibility and module paths as for structs (`m.Counter`).
  - **Codegen** (`codegen/actors.rs`, new): the state as a named struct; methods as ordinary methods `lugha_fn_Counter.add`. Per method, a handler `lugha_actor_Counter.add(state, args, result)` reading the arguments from a GC block in element layout and storing the result. A call on a handle copies its arguments into a fresh block and calls `lugha_rt_actor_call(handle, handler, args, size)` (waits; returns the result's address) or `lugha_rt_actor_cast(handle, handler, args)`. `spawn` copies the state to the GC heap and calls `lugha_rt_actor_spawn(state, name)`. `uses_tasks` covers actors.
  - **Runtime** (`runtime/actors.c`, new; `lugha_rt.c`, `stack.c`, `tasks.c` touched): an actor is a green thread running the message loop over an unbounded queue (GC nodes); a call queues `{handler, args, result, waiter}` and blocks; `stop` is a message with no handler. **One panic path:** every runtime panic (`lugha_rt_panic`, bounds, slices, overflow, channels, `unwrap`…) goes through `panic_end(message)`: in an actor, it prints `panic: …`, records the message, marks the actor ended, makes every waiting caller (and later callers) panic with ``actor `Counter` panicked``, and abandons the actor's green thread (no unwinding: Lugha has no destructors); anywhere else, `exit(101)` as today. A stack overflow inside an actor still ends the program (the signal handler can't switch threads safely) — recorded as a limitation.
  - **Spec v17:** §2–§11 per the draft's milestone 92 list; §10 actors programs; E0439 row; `docs/errors/E0439.md`.
- **Related existing code:** `src/parser/{item,decl}.rs`, `src/check/{structs,methods,method_calls,tasks,bounds,levels}.rs`, `src/codegen/{structs,methods,tasks,show}.rs`, `runtime/{lugha_rt,tasks,spawn,chans}.c`, `src/formatter/items.rs`, `src/driver/lsp/{hover,completion}.rs`.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- Test-first; new code in new files; every file under 500 lines (runtime fragments under 300).
- Every existing panic message and exit code unchanged outside actors (the whole e2e corpus is the check).
- VS Code: `actor` highlighted and an `actor` snippet, as a follow-up commit in the same branch (upkeep, as session 205).
### Must NOT Do
- No supervisors or restart policies, no links/monitors, no actors on several OS threads, no bounded mailboxes, no generic actors, no actors as interface values, no `async`/`await`.
- No unwinding machinery (`setjmp`/`longjmp`, C++ exceptions): an actor's panic abandons its green thread.
- No change to how plain `spawn f()` threads panic.
### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- **E0439** (new, checker): field access through a handle; actor state outside `spawn`; reserved method names; generic actors; a `Ref`/function/interface (or a type holding one) across the boundary.
- **E0201**: anything else in an actor's body.
- Existing: E0415 (no such method on a handle), E0405/E0403 (arguments), E0404/E0419 (`==` on handles).
- Runtime: in the caller, ``panic: actor `Counter` has stopped`` and ``panic: actor `Counter` panicked`` (with the call's location); in the actor, the original panic message, then the actor ends; the deadlock panic covers actors waiting on each other.

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new crate. The runtime's panic path changes for every program: tested by the whole e2e corpus (every panic case keeps its stderr and exit code).

## TESTS TO WRITE
- [ ] Lexer/parser: `actor` keyword; actor with fields and methods; `public actor`; doc comments; E0201 for an `enum` inside; formatter round-trip.
- [ ] Checker: handle typing (`spawn Counter {…}`, `spawn Counter.new(1)`, `Counter` as a parameter type, `Counter[]`); call results (value vs void); `stop`, `join`, `is`; every E0439 case; E0415 for an unknown method; not `Eq`; module-qualified actors.
- [ ] Runtime harness: call/cast ordering, `stop` after queued messages, a call to a stopped actor panics in the caller, an actor's panic wakes its waiting callers with a panic, `join` returns the message, a plain thread's panic still exits 101, deadlock between two actors.
- [ ] e2e `tests/programs/m92/`: the OBJECTIVE; a bank account actor shared by several green threads; actors calling actors; a hand-written supervisor restarting a crashing worker; a logger actor fed by casts; a call after `stop`; an actor's `unwrap` panic isolated; a channel passed into an actor; 1000 actors; every E0439 rejection; IR of programs without actors unchanged.
- [ ] `tests/explain.rs`: E0439's page.
- [ ] Language server: hover on a handle (`Counter`, an actor), completion of a handle's methods (user's plus `stop`/`join`/`is`).

## ROLLBACK PLAN
- Branch `prp-123-actors` in a worktree, merged through a PR; abandoning it means not merging.

## ACCEPTANCE CRITERIA
- [ ] The OBJECTIVE prints `15`, the panic message on stderr, `Some("…")`, `None`, exit 0, at `-O0` and `-O2`.
- [ ] Every test above written first and passing; all existing tests pass.
- [ ] `cargo clippy --all-targets -- -D warnings` and `cargo fmt --check` pass
- [ ] Every `pub` item has a doc comment
- [ ] CHANGELOG.md updated

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cd editors/vscode && npm test`
