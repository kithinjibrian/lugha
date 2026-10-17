## FEATURE: `std.http` — an HTTP/1.1 server and client in Lugha over `std.net`; milestone 111 (v17).

**Status:** approved 2026-10-18 (session 268, with the security sign-off); proposed in session 267, after discovery in the same session (Q1 "yes": server and small client, `http://` only, strict limits; Q2 "accept": a handler's panic ends the program, documented). **Needs the user's security sign-off** (a parser of untrusted network input).

## OBJECTIVE
```lugha
import std.http;

fun handle(req: http.Request): http.Response {
    match (req.method, req.path) {
        ("GET", "/") => http.text(200, "hello"),
        ("POST", "/echo") => http.bytes(200, req.body),
        _ => http.text(404, "not found"),
    }
}

fun main() {
    http.serve("127.0.0.1:8080", handle).unwrap();
}

let res = http.get("http://127.0.0.1:8080/")?;   // a client
println("{res.status} {res.text()?}");
```

## CONTEXT
- Builds on `std.net` (milestone 110, PRP-142): green threads per connection, non-blocking sockets, timeouts.
- Written entirely in Lugha (`src/std/http.la`), loaded lazily like every std module; no runtime or compiler change expected.
- Related: `src/std/net.la`, `src/lib.rs` (`STD`), spec standard-library section.

## DECISIONS (draft decisions 153–166)
153. **Scope** (Q1): HTTP/1.1 server and client, `http://` only. Not: TLS, HTTP/2, streaming bodies, compression, cookies, redirects, routing beyond `match`, JSON parsing.
154. **`Request`**: `method: string`, `path: string` (before `?`, not decoded), `query: string` (after `?`, raw), `headers: Map<string, string>` (names lowercased; a repeated header's values joined with `", "`), `body: u8[]`, `peer: string`; `req.text(): Result<string, string>` (UTF-8), `req.header(name): Option<string>` (any case).
155. **`Response`**: `status: i64`, `headers: Map<string, string>`, `body: u8[]`; builders `http.text(status, s)` (`text/plain; charset=utf-8`), `http.json(status, s)` (`application/json`, the text as given), `http.bytes(status, b)` (`application/octet-stream`); `res.text()`, `res.header(name)`. The server adds `content-length` (and `connection: close` when it closes); a handler's own headers win otherwise.
156. **Server**: `http.serve(addr, handler: fun(Request): Response): Result<(), string>` listens and serves forever, one green thread per connection, keep-alive (`HTTP/1.1` default; `HTTP/1.0` closes unless `connection: keep-alive`; `connection: close` honoured). `http.serve_with(addr, limits, handler)` takes `http.Limits`.
157. **Limits** (`http.Limits`, defaults): request line 8 KiB, all headers 8 KiB, 100 headers, body 1 MiB, 30 s to read a request (also the idle keep-alive wait), 30 s per write. Over a limit: `431` (headers), `413` (body), `414` (line), `408` (timeout, when anything arrived), then close; malformed: `400` and close. `Expect: 100-continue` gets `100 Continue` before the body.
158. **Parsing** is strict: methods are tokens; versions `HTTP/1.0`/`HTTP/1.1` only (else `505`); header names are tokens, values without control characters (bare CR/LF rejected); line ends `CRLF` (a bare `LF` accepted, as RFC 9112 allows); `Content-Length` must be one decimal number (two different ones: `400`); both `Content-Length` and `Transfer-Encoding: chunked`: `400` (request smuggling); any other transfer coding: `501`. Chunked bodies: hex sizes, extensions ignored, trailers read within the header limit and dropped.
159. **Handler panics** (Q2): end the program, as any green thread's panic; documented ("return `http.text(500, …)` instead"). Containing them is a later runtime PRP; `serve` would then answer `500` with no API change.
160. **Client**: `http.get(url)`, `http.post(url, content_type, body: u8[])`, `http.request(method, url, headers, body)`: `Result<Response, string>`. URLs `http://host[:port]/path?query` only (`https://` is `Err("https isn't supported yet")`); `Host`, `content-length`, `connection: close` sent; responses read by `Content-Length`, chunked, or to the end; the same size limits (body up to 16 MiB for responses) and a 30 s timeout (`http.request_with(…, limits)`).
161. **Reason phrases** for the common statuses (`200 OK`, `404 Not Found`, …); others send `Unknown`.
162. **Errors** are `Err(message)`; nothing in `std.http` panics on input.
163. **No dates, no server header**: responses carry only what's needed.
164. **Spec**: a `std.http` section in the standard library; changes list; row 111.
165. **Security** as below; sign-off required.
166. **Success**: milestone 111, tag `m111`.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- `src/std/http.la` (doc comments), registered in `STD`; split into `http.la` and a second std module only if it passes 500 lines (std modules can import each other).
- Server loop, request parser with the limits, chunked decoding, response writer, client, URL parsing.
### Must NOT Do
- Decision 153's exclusions; reading more than the limits allow before rejecting; following redirects.
### New dependencies
- none.

## ERROR HANDLING REQUIREMENTS
- Malformed or over-limit requests: the status in 157/158, then close; the server keeps serving others.
- Client: every failure `Err(message)` (`"bad URL"`, `"https isn't supported yet"`, the network's message, `"response too large"`, `"malformed response"`).

## SECURITY CONSIDERATIONS
- Every read is bounded before parsing; the header and body limits are enforced while reading, not after.
- Request smuggling: conflicting `Content-Length`s or both framings are rejected; only `chunked` is understood.
- Header injection: values with CR/LF are rejected when parsing and when a handler sets them (a response header containing CR/LF is dropped, and the response is sent without it).
- Slow clients (slowloris): a whole request must arrive within the read timeout.
- Listening on `0.0.0.0` exposes the server: the docs recommend `127.0.0.1` for local use.
- Needs the user's sign-off.

## TESTS TO WRITE
- [ ] `tests/http.rs` (Lugha programs through `lughac run`, loopback): GET/POST round trips through `serve` and the client; keep-alive (two requests on one connection, by a raw `net` client); chunked request and response bodies; HTTP/1.0 closing; `100-continue`.
- [ ] Rejections by raw requests: oversized line (414), headers (431), body (413), malformed line (400), CR in a header (400), two `Content-Length`s (400), both framings (400), unknown coding (501), `HTTP/2.0` (505), a slow request (408 or close).
- [ ] Client errors: `https://`, bad URLs, a refused port, a response over the limit.
- [ ] A response header with CR/LF set by a handler is dropped.

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] The OBJECTIVE program serves curl-style requests and the client reads them; narrow tests (`http`, `net`, `lazy_std`, `fmt_corpus`, `lsp_docs`), fmt, clippy; CHANGELOG.

## VALIDATION
- `cargo test --test http --test net --test lazy_std --test fmt_corpus --test lsp_docs`, fmt, clippy
