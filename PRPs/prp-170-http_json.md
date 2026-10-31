## FEATURE: JSON helpers in `std.http` — `req.json()`, `resp.json()`, `http.json_reply(status, v)` and `http.post_json(url, v)` over `std.json`; milestone 138 (v17).

**Status:** proposed 2026-10-31 (session 329), after discovery in session 329 (4 questions, each answered with the recommended option): (1) read, reply and post — four names; (2) `json_reply` answers 500 when a float can't be written; (3) bodies are parsed whatever their `Content-Type`; (4) on the client, `post_json` only.

## OBJECTIVE
```lugha
import std.http;
import std.json;

fun handle(req: http.Request): http.Response {
    match req.json() {                                   // Result<json.Value, json.JsonError>
        Ok(v) => http.json_reply(200, json.Value.Object(#{"echo": v})),
        Err(e) => http.text(400, e.message()),           // "a key defined twice: a (line 1, column 10)"
    }
}

fun call(): Result<(), string> {
    let resp = http.post_json("http://127.0.0.1:8080/echo", json.Value.Array([json.Value.Integer(1)]))?;
    let v = resp.json().map_err(fun(e) = e.message())?;  // Object(#{"echo": Array([Integer(1)])})
    println(v);
    Ok(())
}
```
A server reads a JSON request and answers with JSON in one expression each, and a client posts JSON and reads the answer, with no hand-written `stringify`/`to_bytes`/`string_from_utf8` steps and nothing that can panic.

## CONTEXT
- Starting state: `std.http` (milestones 111, 112, 119, 121; PRP-143) has `Request`/`Response` with `body: u8[]`, `text()` and `header()` methods, the response builders `text`, `json(status, text)` (unchecked JSON text, unchanged here) and `bytes`, and the client `get`/`post`/`request…`. `std.json` (milestone 137, PRP-169) has `parse`, `stringify`, `Value`, `JsonError`.
- Ending state: `src/std/http.la` gains the four helpers and `import std.json;` (≈ 450 lines, under the 500-line rule); `tests/http.rs` gains their tests; docs.
- Related existing code: `src/std/http.la`, `src/std/json.la`, `tests/http.rs` (a server sent raw requests; the client against a raw Rust server).
- Laziness: std modules compile only what a program reaches, so an `std.http` program that never calls a JSON helper compiles none of `std.json`; checked by a test.
- Open decisions that must be resolved first: none.

## DECISIONS (draft decisions 344–351)
344. **`Request.json(self): Result<json.Value, json.JsonError>`** and **`Response.json(self): Result<json.Value, json.JsonError>`** parse the body with `json.parse` (its default limits: 1 MiB, depth 100). The `Content-Type` header isn't looked at: strict parsing already refuses what isn't JSON, and callers can check `header("content-type")` themselves.
345. **A body that isn't UTF-8** is `JsonError { kind: Syntax, detail: "a body that isn't UTF-8", line: 1, column: 1 }`; every other failure is `json.parse`'s own error. An empty body is `json.parse("")`'s `expected a value`.
346. **`json_reply(status: i64, v: json.Value): Response`** — `json.stringify(v)` as the body with `content-type: application/json`. When `v` holds NaN or an infinity, the reply is instead `text(500, "NaN or an infinity can't be written as JSON")`: a handler stays one expression, and the failure is visible to the client, never a panic or a silently changed value.
347. **`post_json(url: string, v: json.Value): Result<Response, string>`** — `post(url, "application/json", json.stringify(v)?)`, so `http://` and `https://` as `post` handles them; a value that can't be written is `Err` with `JsonError.message()` before any connection is made.
348. **`http.json(status, text)` is unchanged**; the new names don't replace or overload it.
349. **Out of scope**: `request_json` or JSON versions of the `_with`/`_tls` functions (callers use `request…` with `json.stringify(v)?.to_bytes()`); checking `Content-Type`; mapping bodies to structs; `Accept` negotiation; changing `std.json` or the server's limits.
350. **Docs**: the spec's `std.http` paragraph, the milestones table (138), the "Changes since v16" list; CHANGELOG; `CLAUDE.md`'s tree line for `http.la`; doc comments on the four helpers.
351. **No new dependency.** Success: milestone 138, tag `m138`.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- `src/std/http.la`: `import std.json;`, the two `json` methods, `json_reply`, `post_json`, each with a doc comment; the module doc mentions them.
- `tests/http.rs` first (TESTING RULE), failing, then the helpers.
### Must NOT Do
- Decision 349's exclusions; changing `http.json(status, text)`, `std.json`, `std.$http_wire` or any compiler or runtime file; a panic on any body or value.
### New dependencies
- none.

## ERROR HANDLING REQUIREMENTS
- `req.json()`/`resp.json()`: `json.JsonError` (decision 345 for non-UTF-8, otherwise `json.parse`'s).
- `json_reply`: never fails; a 500 text response for a non-finite float (decision 346).
- `post_json`: `Err(string)` — the `JsonError`'s message for an unwritable value, otherwise `post`'s errors unchanged.

## SECURITY CONSIDERATIONS
- Request bodies come from strangers: they are already capped by the server's `Limits.body` (1 MiB default) and parsed by `std.json` within its limits (size, depth, value count), refusing duplicate keys. Nothing new is logged; the 400 in the OBJECTIVE echoes only the parser's message, which quotes at most a key.
- No I/O beyond `post`'s, no `unsafe`, no new crate.

## TESTS TO WRITE
- [ ] Server: a handler using `req.json()` and `json_reply` answers a JSON POST with the expected body and `content-type: application/json`; a body with a duplicate key gets the handler's 400 with the error message; a non-UTF-8 body gives `a body that isn't UTF-8 (line 1, column 1)`; an empty body gives `expected a value (line 1, column 1)`; a `text/plain` body holding JSON is parsed.
- [ ] `json_reply` with a NaN nested in the value answers 500 `text/plain` with the message.
- [ ] Client: `post_json` against the raw Rust server sends `POST`, `content-type: application/json` and the compact body; `resp.json()` parses its reply; a reply that isn't JSON is the parser's error; `post_json` with an infinity is `Err` and the raw server sees no connection.
- [ ] Laziness: an `std.http` program that calls only `http.text` compiles no `std/json` function (`--emit=ir`).

## ROLLBACK PLAN
- `main`; revert the merge. Only additions to `std.http`.

## ACCEPTANCE CRITERIA
- [ ] The tests above pass; `cargo test --lib`, `--test http`, `--test json`, `--test lsp_docs`, `--test fmt_corpus`; fmt and clippy clean.
- [ ] Doc comments on the four helpers; spec, CHANGELOG, CLAUDE.md updated.

## VALIDATION
- `cargo test --lib`
- `cargo test --test http --test json --test lsp_docs --test fmt_corpus`
- `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`
