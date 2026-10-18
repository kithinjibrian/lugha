## FEATURE: `http.request_tls_with` — one request with both `Limits` and `tls.Options`; milestone 119 (v17).

**Status:** proposed 2026-10-18 (session 284), from the user's brief: Rapidkoi fetches app bundles (up to 32 MB, 30 s) with `request_with`; its tests must also trust a test CA, which only `request_tls` accepts — and that one is capped at 16 MB with no `Limits`.

## OBJECTIVE
```lugha
let mut limits = http.limits();
limits.body = 33554432;           // 32 MB
limits.timeout_ms = 30000;
let opts = tls.Options { ca_file: "test-ca.pem", ..tls.options() };
let res = http.request_tls_with("GET", "https://localhost:8443/app.rkb", #{}, [], limits, opts)?;
```

## CONTEXT
- `src/std/http.la`: `request_with(method, url, headers, body, limits)` and `request_tls(method, url, headers, body, opts)` both end in the private `exchange(method, url, headers, body, limits, ca_file)`; `request_tls` fixes the body limit at 16 MiB.
- Related: spec v17 `std.http` (the HTTPS bullet), `tests/tls.rs`.

## DECISIONS (draft decisions 241–244)
241. **`request_tls_with(method: string, url: string, headers: Map<string, string>, body: u8[], limits: Limits, opts: tls.Options): Result<Response, string>`**: `limits` used as given; `opts.ca_file` trusted for `https://`; `opts.timeout_ms > 0` overrides `limits.timeout_ms`. Exactly `exchange(method, url, headers, body, limits, opts.ca_file)` after that override.
242. **Nothing else changes**: `request_tls` keeps its 16 MiB default; `http://` URLs work too (the CA file is then unused), as with `request_tls`.
243. **Docs**: the spec's `std.http` HTTPS bullet; CHANGELOG.
244. **Success**: milestone 119, tag `m119`.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- `request_tls_with` in `src/std/http.la` with doc comments; tests; docs.
### Must NOT Do
- Changes to `request_tls`'s defaults or to `std.tls`.
### New dependencies
- none.

## ERROR HANDLING REQUIREMENTS
- As `request_with`/`request_tls`: `Err` with the reason (`"response too large"`, `"timed out"`, `"certificate verify failed: …"`).

## SECURITY CONSIDERATIONS
- Verification stays on; only the trusted set grows by `ca_file`, as with `request_tls`.

## TESTS TO WRITE
- [ ] `tests/tls.rs`: a TLS server (per-run test CA) serves a body over 16 MiB: with a 32 MiB limit and the CA it arrives whole; with a smaller limit it is `Err("response too large")`; without `ca_file` it is `Err("certificate verify failed: …")`; `opts.timeout_ms` overrides a long `limits.timeout_ms` against a server that never answers (`Err("timed out")` within seconds).

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] The tests pass; narrow tests (`tls`, `http`, `lsp_docs`), fmt, clippy; CHANGELOG.

## VALIDATION
- `cargo test --test tls --test http --test lsp_docs`, fmt, clippy
