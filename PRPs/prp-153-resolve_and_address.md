## FEATURE: `net.resolve` and `tls.Options.address` — resolve a name, then connect to a chosen address; milestone 121 (v17).

**Status:** approved and built 2026-10-19 (session 290); proposed in session 289, from the user's brief: Rapidkoi's PRP-013 refuses names that point to the local network, without DNS rebinding — it resolves a name once, checks every address, then connects to the address it checked (like curl's `--resolve`).

## OBJECTIVE
```lugha
let addrs = net.resolve("example.com")?;             // ["93.184.215.14", "2606:4700::1"]
// … refuse private/loopback addresses …
let opts = tls.Options { address: addrs[0], ..tls.options() };
let res = http.request_tls_with("GET", "https://example.com/app.rkb", #{}, [], limits, opts)?;
// connected to 93.184.215.14:443; SNI, certificate check and Host: example.com
```

## CONTEXT
- `runtime/net.c` resolves with `getaddrinfo` inside `net_connect`/`net_listen` and connects to each result in turn; `runtime/tls.c` `lugha_rt_tls_connect(addr, host, ca_file, timeout_ms)` already takes the address to connect to and the name to verify separately (`std.tls` passes `host_of(addr)`).
- `std.http`'s private `exchange` builds `host:port` from the URL and calls `tls.connect_with` or `net.connect_timeout`; the `host` header is the URL's host.
- `$os.<op>` built-ins: `check/float_ops.rs` `os_signature`, `codegen/net.rs` (signature-driven), `RUNTIME_SYMBOLS`.
- Related: spec v17 `std.net`, `std.tls`, `std.http` (HTTPS bullet); `tests/net.rs`, `tests/tls.rs`.

## DECISIONS (draft decisions 252–261)
252. **`net.resolve(name: string): Result<string[], string>`**: every address `getaddrinfo` gives (`AF_UNSPEC`, `SOCK_STREAM`), as text (`inet_ntop`: `"93.184.215.14"`, `"2606:4700::1"`), in the resolver's order, each once. An IP literal gives itself (in `inet_ntop`'s canonical form). `Err` with `gai_strerror`'s reason; `Err` for `""` or a NUL byte. Nothing panics.
253. **Built-in `$os.net_resolve(name, numeric: i64): Result<string, string>`** (`lugha_rt_net_resolve`): the addresses joined by `\n`, which `std.net` splits and de-duplicates; `numeric` 1 adds `AI_NUMERICHOST` (no lookup: only an IP literal succeeds), used to check `address`.
254. **`tls.Options` gains `public address: string`**; `options()` sets `""`, which keeps today's behaviour.
255. **`address` set**: it must be an IPv4 or IPv6 literal (checked with the numeric resolve; brackets not accepted), else `Err("address must be an IP address: {address}")`. `tls.connect_with(addr, opts)` then connects to `address` at `addr`'s port (IPv6 written `[…]:port`), and still uses `addr`'s host for SNI and the certificate's host-name (or IP) check.
256. **`std.http`**: `request_tls_with` and `request_tls` pass `opts.address` to `exchange`, which honours it for `https://` (through `tls.connect_with`) and `http://` (connecting to `address` at the URL's port); the `host` header stays the URL's host. `request`, `request_with` and `get`/`post` are unchanged (no address).
257. **Compatibility**: a `tls.Options { … }` literal naming every field without `..base` must now name `address` too (Lugha has no field defaults). Rapidkoi has two (`src/fetching.la:96`, `src/requests.la:124`); `..tls.options()` or `address: ""` fixes each. Stated in the spec's compatibility line.
258. **Blocking**: `getaddrinfo` blocks the OS thread, as `net.connect` already does; noted in the docs (a non-blocking resolver is out of scope).
259. **Not**: reverse lookups, a resolver cache, SRV/MX records, choosing among addresses (the caller picks), an address list in `Options`.
260. **Docs**: the spec's `std.net` (`resolve`), `std.tls` (`Options.address`) and `std.http` HTTPS bullet, the milestones table (121), "Changes since v16", the compatibility line; CHANGELOG.
261. **Success**: milestone 121, tag `m121`.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- `runtime/net.c`: `lugha_rt_net_resolve`; `RUNTIME_SYMBOLS`; `$os.net_resolve` in the checker (codegen is signature-driven).
- `src/std/net.la`: `resolve`; `src/std/tls.la`: `address`, the check and the target address; `src/std/http.la`: `exchange` takes the address.
- Update this repo's own `tls.Options { … }` literals (`tests/tls.rs`).
### Must NOT Do
- Decision 259; changes to verification (`SSL_set1_host`/SNI stay on the host name).
### New dependencies
- none.

## BUILT (session 290)
- As planned. `std.http` checks the address for `http://` with its own private `pinned` (`std.tls`'s `at` is private). Tests: `tests/net.rs` `resolve_gives_every_address_once`, `tests/tls.rs` `an_address_is_connected_to_and_the_name_still_verified` (`wrong.example` fails with `hostname mismatch`), `tests/http.rs` `an_address_is_connected_to_with_the_url_host`.

## ERROR HANDLING REQUIREMENTS
- `resolve`: `Err` with the resolver's reason, or for `""`/NUL. `address`: `Err("address must be an IP address: …")`. Connect and TLS errors as today.

## SECURITY CONSIDERATIONS
- The point is to stop DNS rebinding: the caller checks the addresses `resolve` returns and connects to exactly one of them; the connection never resolves the name again when `address` is set.
- Verification is unchanged: the certificate must still name the URL's host, so pinning an address can't make a wrong server trusted.
- `AI_NUMERICHOST` for `address` means no lookup happens on that path.

## TESTS TO WRITE
- [ ] `tests/net.rs`: `resolve("localhost")` includes `127.0.0.1` or `::1`; `resolve("127.0.0.1")` is `["127.0.0.1"]`, `resolve("::1")` is `["::1"]`; `resolve("")` and `resolve("nothing.invalid")` are `Err`; no duplicates.
- [ ] `tests/tls.rs`: an HTTPS server on 127.0.0.1 with a certificate for localhost is reached as `https://localhost:PORT` with `address: "127.0.0.1"`; `https://wrong.example:PORT` with the same address fails the certificate check (`certificate verify failed: …`), proving no lookup of `wrong.example` and the host-name check kept; `address: "localhost"` is `Err("address must be an IP address: localhost")`.
- [ ] `tests/http.rs` (or `tls.rs`): `http://wrong.example:PORT/x` with `address: "127.0.0.1"` reaches a raw server on 127.0.0.1, which reports the request's `host: wrong.example:PORT`.

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] The tests pass; narrow tests (`net`, `tls`, `http`, `lazy_std`, `lsp_docs`, `--lib`), fmt, clippy; CHANGELOG.

## VALIDATION
- `cargo test --test net --test tls --test http --test lazy_std --test lsp_docs`, `cargo test --lib`, fmt, clippy
