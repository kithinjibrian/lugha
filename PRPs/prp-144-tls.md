## FEATURE: TLS — `std.tls` over system OpenSSL 3, and `https://` in `std.http`; milestone 112 (v17).

**Status:** proposed 2026-10-18 (session 269), after discovery in the same session (the user: "okay do https"; Q1 "yeah": system OpenSSL 3, linked only into programs that use TLS; Q2 "ok": always verify, `ca_file` as the only way to trust more). **Needs the user's security review** (cryptography; a new linked C library).

## OBJECTIVE
```lugha
import std.http;
import std.tls;

let res = http.get("https://example.com/")?;                 // verified against the system's roots
let opts = tls.Options { ca_file: "my-ca.pem", ..tls.options() };
let conn = tls.connect_with("internal.lan:8443", opts)?;      // a self-signed or private CA
http.serve_tls("127.0.0.1:8443", "cert.pem", "key.pem", handle)?;
```
Encrypted connections that behave like `std.net`'s: waits park only their green thread, failures are `Err`, certificates are always checked.

## CONTEXT
- `std.net` (milestone 110): non-blocking sockets parked through `epoll` (`runtime/net.c`, `wait_fd`); `std.http` (milestone 111) reads and writes through a `net.Conn`.
- Every program is linked with the runtime source compiled alongside it (`src/link/mod.rs`: `cc … lugha_rt.c -lgc -lm`). OpenSSL headers must not be needed by programs that don't use TLS.
- Related: `src/link/mod.rs` (`RUNTIME_SOURCE`, `link_with`, `LinkError`), `src/driver/{build,pipeline}.rs`, `src/std/{net,http,http_wire}.la`, CI (`libssl-dev`).

## DECISIONS (draft decisions 167–180)
167. **Library** (Q1): the system's OpenSSL 3 (`libssl-dev`, Apache-2.0); no crates, no vendored source.
168. **Linking only when used** (Q1): a program whose modules include `std.tls` (directly, or through `std.http`'s `https://` support, which imports it) gets the TLS runtime fragment `runtime/tls.c` appended to its runtime source and `-lssl -lcrypto` on the link line; every other program builds exactly as before. `check` and the language server never link. A missing OpenSSL (the header or library) is **E0332** (new) at the program's `import`, with help: install `libssl-dev`.
169. **Verification** (Q2): always — the certificate chain against the system's default trust store (`SSL_CTX_set_default_verify_paths`) and the host name (`SSL_set1_host`); no option turns it off. A failure is `Err` with OpenSSL's reason (`"certificate verify failed: …"`).
170. **`Options`**: `ca_file: string` (`""` by default): PEM certificates trusted in addition to the system's (a private CA or a self-signed certificate); `timeout_ms` (0: none). `tls.options()` gives the defaults.
171. **Protocol**: TLS 1.2 minimum (1.3 preferred), OpenSSL's default ciphers, SNI with the host name; IP addresses are verified as IP SANs.
172. **API** (`src/std/tls.la`): `tls.connect(addr)`, `tls.connect_with(addr, opts)`: `Result<Conn, string>`; `tls.listen(addr, cert_file, key_file): Result<Listener, string>`, `l.accept(): Result<Conn, string>` (the handshake runs on the accepting green thread, so a slow client stalls only that accept); `Conn` has `read(max)`, `write(bytes)`, `set_timeout(ms)`, `peer()`, `addr()`, `close()` (sends `close_notify`) — the same shapes and errors as `net.Conn`.
173. **`std.http`**: the client accepts `https://` (port 443 by default) and verifies; `http.request_tls(method, url, headers, body, opts)` for `ca_file`; `http.serve_tls(addr, cert, key, handler)` and `serve_tls_with(…, limits, handler)`. Internally the server and client read and write through one connection type that wraps either a `net.Conn` or a `tls.Conn`.
174. **Non-blocking**: OpenSSL works on the non-blocking socket; `SSL_ERROR_WANT_READ`/`WANT_WRITE` park the green thread with `wait_fd` (read or write readiness) and retry, so the handshake, reads and writes never block the OS thread.
175. **Memory**: OpenSSL objects are `malloc`ed by OpenSSL and freed by `close()`; the GC never sees them; Lugha holds an opaque handle (an `i64`, as sockets are). An unclosed connection leaks until exit, as sockets do.
176. **Not**: client certificates (mutual TLS), pinning, session resumption, ALPN/HTTP2, turning verification off, TLS without an OS socket.
177. **Errors**: every failure `Err(message)` with OpenSSL's error string; `"closed"`, `"timed out"` as `std.net`.
178. **CI and docs**: `libssl-dev` added to CI; README prerequisites mention it (only for TLS programs).
179. **Spec**: `std.tls` and the `https` parts of `std.http`; E0332's row and page; §11 unchanged (TLS was never out of scope).
180. **Success**: milestone 112, tag `m112`.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- `runtime/tls.c` (its own `#include <openssl/ssl.h>`, `<openssl/err.h>`, `<openssl/x509v3.h>`; joined after `net.c` only for TLS programs): contexts (client: default verify paths + optional CA file, TLS 1.2 minimum; server: certificate and key files), `SSL_new` on the socket, handshake/read/write/shutdown loops with `wait_fd`, error strings, `lugha_rt_tls_*` entry points.
- Link: a `TLS_SOURCE`, the condition (the program loads `std.tls`), `-lssl -lcrypto`, E0332 mapping from `cc`'s failure.
- Checker/codegen: `$os.tls_*` built-ins as `$os.net_*` are.
- `src/std/tls.la`; `std.http` changes (a connection wrapper, `https://`, `serve_tls`); file sizes within 500 lines (another `$` module if needed).
- E0332 page; spec; CHANGELOG; CI `libssl-dev`.
### Must NOT Do
- Decision 176; any verification bypass; loading keys or certificates from anywhere but the paths given.
### New dependencies
- System OpenSSL 3 (`libssl`, `libcrypto`; Apache-2.0), only for TLS programs. No crates.

## ERROR HANDLING REQUIREMENTS
- E0332 (new): OpenSSL missing when building a TLS program.
- Runtime: `Err` for handshake, verification, I/O, file-loading failures; never a panic for network input.

## SECURITY CONSIDERATIONS
- Verification can't be turned off; host names are checked; TLS 1.0/1.1 are refused.
- Key files are read by OpenSSL from the given path only; nothing is logged or printed from them.
- OpenSSL memory is outside the GC and freed on `close()`; no Lugha pointer is kept by OpenSSL beyond a call (buffers are copied).
- The `std.http` limits apply unchanged over TLS.
- Needs the user's security review before building.

## TESTS TO WRITE
- [ ] `tests/tls.rs` (certificates generated per run with the `openssl` command into a temp folder — a CA, a server certificate for `127.0.0.1`/`localhost`, a self-signed one, an expired one; skipped with a message if `openssl` is absent): a Lugha TLS echo server and client in one program trusting the test CA; `http.serve_tls` + `http.request_tls`; failures: the CA not trusted (system roots only) → `Err("…certificate verify failed…")`; wrong host name; expired certificate; a plain-TCP peer; a missing key file.
- [ ] A non-TLS program's link line has no `-lssl` (unit test on the link plan).
- [ ] E0332 when OpenSSL is "missing" (a fake `cc` failure through the plan's test seam, or a unit test of the error mapping).
- [ ] `tests/http.rs` and `tests/net.rs` still pass.

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] The OBJECTIVE program's shapes work on loopback with test certificates; `https://example.com` works when the machine has network (manual check, not a test).
- [ ] Narrow tests (`tls`, `http`, `net`, link unit tests, `explain`), fmt, clippy; CHANGELOG.

## VALIDATION
- `cargo test --test tls --test http --test net --test explain`, `cargo test --lib -- link`, fmt, clippy
