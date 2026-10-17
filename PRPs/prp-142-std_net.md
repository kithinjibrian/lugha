## FEATURE: `std.net` — TCP sockets with non-blocking I/O in the green-thread scheduler; milestone 110 (v17).

**Status:** approved 2026-10-18 (session 266, with the security sign-off); proposed in session 265, after discovery in sessions 264–265 (the user: "wanna build a std http lib?"; plan: sockets → HTTP/1.1 → TLS; Q1 "let's start" — sockets with non-blocking I/O first; Q2 "cool" — the API below). **Needs the user's security sign-off** (new network surface in `runtime/`).

## OBJECTIVE
```lugha
import std.net;

fun serve(conn: net.Conn) {
    match conn.read(4096) {
        Ok(bytes) => { let _ = conn.write(bytes); }
        Err(e) => eprintln(e),
    }
    conn.close();
}

fun main() {
    let listener = net.listen("127.0.0.1:0").unwrap();
    println(listener.addr());                 // "127.0.0.1:41234"
    while true {
        match listener.accept() {             // pauses only this green thread
            Ok(conn) => { spawn serve(conn); }
            Err(e) => eprintln(e),
        }
    }
}
```
Thousands of connections are served on the one OS thread: a task waiting to accept, read, write or connect is parked until the socket is ready, and the others keep running.

## CONTEXT
- Today the runtime has no sockets, and every I/O call blocks the one OS thread all green threads share (v17 draft decision 11; spec §11 lists non-blocking I/O as out of scope).
- The scheduler (`runtime/tasks.c`): a ready queue, sleepers ordered by deadline, `schedule()` that `nanosleep`s until the next sleeper and panics with "deadlock" when nothing can run.
- `std` modules are Lugha over `$os.*` built-ins (`std.fs`, `std.process`, `std.task`): `src/std/*.la`, `check/float_ops.rs` (`os_signature`), `codegen/os.rs`, runtime fragments in `RUNTIME_PARTS`.
- Related: `runtime/{tasks,spawn,chans}.c`, `src/link/mod.rs`, `src/std/task.la`, spec §7 (green threads), §11.

## DECISIONS (draft decisions 141–152)
141. **Scope** (Q1): TCP sockets and the scheduler's non-blocking I/O for them. HTTP (milestone 111) and TLS (later, with its own security review) build on this. Stdin, files and processes stay blocking.
142. **API** (Q2), `src/std/net.la`:
     - `net.listen(addr: string): Result<Listener, string>` — `SO_REUSEADDR`, backlog 128; port `0` picks a free one.
     - `l.accept(): Result<Conn, string>`, `l.addr(): string` (the bound address), `l.close()`.
     - `net.connect(addr: string): Result<Conn, string>`.
     - `c.read(max: i64): Result<u8[], string>` — up to `max` bytes (1 … 16 MiB, else `Err`); `[]` at end of stream.
     - `c.write(bytes: u8[]): Result<(), string>` — all of them, or `Err`.
     - `c.set_timeout(ms: i64)` — a read, write or accept waiting longer is `Err("timed out")`; `0` (the default) waits forever.
     - `c.peer(): string`, `c.close()`.
143. **Values:** `Listener` and `Conn` are std structs holding the socket in a shared box (`Ref`), so every copy sees `close()`; a closed socket's calls are `Err("closed")`; `close()` twice is harmless.
144. **Addresses:** `"host:port"`, `"[v6]:port"`; a malformed one is `Err` naming it. Names resolve with `getaddrinfo`, which blocks the OS thread for the lookup (stated in the spec; a resolver thread is later).
145. **Errors:** `Err` with the system's message (`strerror`), as `std.fs`; no panics for network failures.
146. **Scheduler:** the runtime keeps one `epoll` set. A waiting call registers its socket (one-shot) and parks its fiber; `schedule()`, when nothing is ready, `epoll_wait`s until a socket is ready or the next sleeper is due. A timeout parks the fiber as a sleeper too, and whichever comes first wakes it. The deadlock panic fires only when no fiber is ready, sleeping or waiting on a socket. `main` alone (no `spawn`) works the same way.
147. **Sockets are non-blocking** (`SOCK_NONBLOCK | SOCK_CLOEXEC`); writes use `MSG_NOSIGNAL` (no `SIGPIPE`); `EINTR` retries.
148. **Not:** UDP, Unix sockets, socket options beyond the timeout, non-blocking stdin/files/processes, `select` arms on sockets, a resolver thread.
149. **The GC never closes sockets:** an unclosed one stays open until exit (documented).
150. **Built-ins:** `$net.listen`, `accept`, `connect`, `read`, `write`, `close`, `addr`, `peer` — bundled sources only, like `$os.*`; runtime `lugha_rt_net_*` in a new fragment `runtime/net.c`; `tasks.c` gains the `epoll` wait.
151. **Spec:** a `std.net` section; §7 (the scheduler waits on sockets); §11: non-blocking I/O for sockets leaves the out-of-scope list (the rest stays).
152. **Success:** milestone 110, tag `m110`; the HTTP PRP follows.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- `runtime/net.c`: socket, bind/listen, accept4, connect (non-blocking, awaited with `SO_ERROR`), recv/send loops parking on `EAGAIN`, close, getsockname/getpeername formatting, getaddrinfo; every buffer bounded; results through out-slots as `$os` built-ins do.
- `runtime/tasks.c`: `epoll` set, `lugha_rt_task_wait_fd(fd, events, deadline)`, the `schedule()` change, the deadlock condition.
- Checker/codegen: `$net.*` signatures and lowering (as `$os.*`).
- `src/std/net.la` with doc comments; lazy loading as other std modules.
- Spec, CHANGELOG, `lughac explain` unaffected (no new codes).
### Must NOT Do
- Decision 148; any shell, file or process effect from `net.c`; reading more than asked; keeping Lugha pointers in C beyond a call.
### New dependencies
- none (libc sockets and `epoll`, Linux).

## ERROR HANDLING REQUIREMENTS
- Every failure is `Err(message)`; `read`'s `max` outside 1 … 16 MiB is `Err`; a malformed address is `Err`; a closed socket is `Err("closed")`; a timeout is `Err("timed out")`.
- Runtime invariants (an `epoll` failure itself) panic with a message, as the scheduler does today.

## SECURITY CONSIDERATIONS
- Network bytes are untrusted: `read` never returns more than `max`, which is capped; no parsing happens here (HTTP's PRP adds limits for its parser).
- No shell, no files, no processes; `getaddrinfo` only on the address string given; `SOCK_CLOEXEC` so started processes don't inherit sockets; `MSG_NOSIGNAL` so a closed peer can't kill the program.
- Listening on `0.0.0.0` exposes the program to the network: the docs recommend `127.0.0.1` for local use; nothing listens unless the program asks.
- Needs the user's sign-off before building.

## TESTS TO WRITE
- [ ] Checker: `$net.*` only in bundled sources (E0101 in a program), `std.net` types and method signatures.
- [ ] `tests/net.rs` (Lugha programs through `lughac run`, loopback only): echo server and client in one program (`spawn`, port 0, `addr()`); 200 clients at once while a ticking task keeps printing (proves reads don't block the thread); `read` returning `[]` at end of stream; `set_timeout` → `Err("timed out")`; connecting to a closed port → `Err` with the system's message; calls after `close()` → `Err("closed")`; a malformed address and `read(0)` → `Err`; IPv6 `[::1]` when the host has it; `main` alone accepting one client.
- [ ] `tests/tasks_runtime.rs`: the deadlock panic still fires with no sockets waited on; a fiber waiting on a socket isn't a deadlock.

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] The OBJECTIVE echo server serves many concurrent clients from one OS thread; narrow tests (`net`, `tasks_runtime`, `lazy_std`, lib), fmt, clippy; CHANGELOG.

## VALIDATION
- `cargo test --test net --test tasks_runtime --test lazy_std`, `cargo test --lib -- net`, fmt, clippy
