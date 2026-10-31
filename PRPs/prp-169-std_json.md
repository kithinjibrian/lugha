## FEATURE: `std.json` — a strict RFC 8259 JSON parser and writer written in Lugha, safe on hostile input; milestone 137 (v17).

**Status:** proposed 2026-10-31 (session 327), after discovery in session 327 (6 questions, each answered with the recommended option): (1) the next std gap is JSON; (2) parse and write a `Value`, no struct mapping; (3) numbers as `Integer(i64)` and `Float(f64)`; (4) strict RFC 8259 with duplicate keys an error, limits like `std.toml`'s; (5) writing returns `Result`, `Err` on NaN/±Infinity; (6) helpers `get`, `at` and `as_*` only; JSONTestSuite vendored as fixtures, no `std.http` change.

## OBJECTIVE
```lugha
import std.json;

fun run(): Result<(), json.JsonError> {
    let v = json.parse("{\"user\": {\"name\": \"ada\", \"id\": 9007199254740993}}")?;  // Result<json.Value, JsonError>
    let name = json.get(v, "user").and_then(|u| json.get(u, "name")).and_then(json.as_text);  // Some("ada")
    let id = json.get(v, "user").and_then(|u| json.get(u, "id")).and_then(json.as_i64);       // exact, past 2^53

    let reply = json.Value.Object(#{
        "ok": json.Value.Bool(true),
        "tags": json.Value.Array([json.Value.Text("a"), json.Value.Null]),
    });
    println(json.stringify(reply)?);   // {"ok":true,"tags":["a",null]}
    println(json.pretty(reply)?);      // the same, two-space indented

    match json.parse("{\"a\": 1, \"a\": 2}") {
        Err(e) => println(e.message()),  // "a key defined twice: a (line 1, column 10)"
        Ok(_) => {}
    }
    Ok(())
}

fun main() {
    match run() { Err(e) => eprintln(e.message()), Ok(_) => {} }
}
```
Programs read and answer JSON (first behind `std.http`) with the whole of RFC 8259, exactly (no lost integer digits, no silently changed floats), and nothing an attacker sends can crash or exhaust them.

## CONTEXT
- Starting state: `std.toml` (milestone 114, PRP-146) is the pattern — a std module written in Lugha, loaded lazily, split across internal `std.$…` modules under the 500-line rule, public types re-exported with `public import std.$… { * };`. `Map` keeps insertion order (milestone 101); `Option.and_then` exists (milestone 118); floats print in the shortest form that round-trips (spec §5).
- Ending state: `src/std/json.la` (public API) plus internal `src/std/json_value.la`, `src/std/json_parse.la`, `src/std/json_write.la` (more only if the 500-line rule needs them), registered in `STD` in `src/lib.rs`; `tests/json.rs`; `tests/fixtures/JSONTestSuite/`.
- Related existing code: `src/std/toml.la`, `toml_value.la`, `toml_strings.la`, `toml_scan.la`, `toml_parse.la`; `src/lib.rs` (`STD`); `tests/toml.rs` (how a suite is run); the spec's standard-library section.
- Expected: no compiler or runtime change. If one turns out needed (e.g. a float conversion the prelude lacks), stop and report before making it (SCOPE RULE).
- Open decisions that must be resolved first: none in DECISIONS.md.

## DECISIONS (draft decisions 327–343)
327. **`parse(text: string): Result<Value, JsonError>`** — any JSON value at the top (RFC 8259 allows a scalar). `parse_with(text, limits: Limits)` for other limits.
328. **`Value`**:
     ```lugha
     public enum Value {
         Null, Bool(bool), Integer(i64), Float(f64), Text(string),
         Array(Value[]), Object(Map<string, Value>),
     }
     ```
     A number with no `.`, `e` or `E` that fits `i64` is `Integer`, exactly. Any other number is `Float` — including an integer literal past `i64` (`1e400`-style magnitudes are below). `-0` is `Integer(0)`; `-0.0` is `Float(-0.0)`.
329. **Objects keep their keys in the text's order** (`Map`); writing visits them in `Map` order.
330. **Strict RFC 8259 grammar**: whitespace is only space, tab, LF, CR; no comments, trailing commas, single quotes, unquoted keys, `NaN`/`Infinity`, leading `+`, leading zeros (`01`), `.5` or `5.`; a byte-order mark is refused; nothing but whitespace may follow the value. Strings: every escape (`\" \\ \/ \b \f \n \r \t \uXXXX`), surrogate pairs joined, a lone or reversed surrogate refused, raw control characters (U+0000–U+001F) refused.
331. **A number whose magnitude overflows `f64`** (e.g. `1e400`) is an `Err` of kind `Overflow`, not `Float(inf)` — it could not be written back. Underflow to zero is accepted (`1e-400` is `Float(0.0)`).
332. **A key defined twice in one object is an `Err` of kind `Duplicate`** at the second key (parsers that disagree on duplicates are a known smuggling bug).
333. **`JsonError`**: `public struct JsonError { public kind: ErrorKind, public detail: string, public line: i64, public column: i64 }` with `e.message()` = `"{detail} (line {line}, column {column})"`, 1-based, columns counting characters. `ErrorKind` is `Syntax`, `Duplicate`, `Overflow` (decision 331), `BadValue` (a bad escape or surrogate), `Limit` (decision 335) and `NotFinite` (writing NaN or ±Infinity; line and column 0). Details name the problem plainly: `"a key defined twice: a"`, `"expected ':' after a key"`, `"a lone surrogate: \\ud800"`.
334. **Nothing panics on any input**: every index checked before use; integers accumulated with overflow checked before each multiply and add (overflow falls back to `Float`, decision 328).
335. **Limits** (`Limits`, `limits()` gives the defaults): `bytes` 1048576 (the text's size), `depth` 100 (arrays and objects nested in each other), `values` 1000000 (every value created). Depth is checked before descending, so recursion is bounded by it.
336. **Writing**: `stringify(v: Value): Result<string, JsonError>` (compact: no spaces) and `pretty(v: Value): Result<string, JsonError>` (two-space indent, `": "` after keys, one element per line, `[]` and `{}` for empty ones, no trailing newline). Only `"`, `\` and U+0000–U+001F are escaped (`\n`, `\t`, `\r`, `\b`, `\f` by name, the rest `\u00XX`); other text, including non-ASCII, is written as UTF-8. Integers in decimal; floats in the shortest form that round-trips, made valid JSON (a whole float keeps `.0`; exponents written `e` with a sign or digits as JSON allows). A non-finite float is `Err(NotFinite)`. Writing depth is not limited (a `Value` built in Lugha is already in memory), but must not recurse deeper than the value is nested.
337. **Round trip**: for every value `parse` returns, `parse(stringify(v)?)` gives back an equal value.
338. **Helpers**, each usable with `and_then`: `get(v: Value, key: string): Option<Value>` (an `Object`'s member), `at(v: Value, i: i64): Option<Value>` (an `Array`'s element; out of range or negative is `None`), `as_text`, `as_i64`, `as_f64` (an `Integer` too, widened), `as_bool`, `as_array`, `as_object`, each `fun(Value): Option<…>`, and `is_null(v: Value): bool`. No dotted paths or JSON Pointer.
339. **Out of scope**: mapping to and from user structs; JSON5, comments or other leniency; streaming or incremental parsing; reading files (callers use `read_file`); `std.http` helpers (a later PRP).
340. **Implementation**: a hand-written scanner over the text's bytes with one position, line and column; no backtracking beyond a token. Strings are UTF-8 by construction, so invalid UTF-8 never reaches the parser.
341. **The suite as fixtures**: JSONTestSuite (github.com/nst/JSONTestSuite, MIT), its `test_parsing/` cases copied unedited into `tests/fixtures/JSONTestSuite/` with its `LICENSE` and a `README` naming the commit and source. Every `y_` case must parse; every `n_` case must be `Err` (not a panic, not `Limit`), or be rejected before `parse` as invalid UTF-8; every `i_` case has its outcome pinned in `tests/json.rs` by these decisions (BOM refused, lone surrogates refused, huge exponents `Overflow`, huge integers `Float`, deep nesting `Limit` past 100).
342. **Docs**: the spec's standard-library section (`std.json`), the milestones table (137), the "Changes since v16" list; CHANGELOG; `CLAUDE.md`'s stack line and tree; doc comments on every public item (shown by the language server).
343. **No new dependency** (the fixtures are MIT test data, noted; `serde_json`, already a dependency, may compare outputs in `tests/json.rs`). Success: milestone 137, tag `m137`.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- `src/std/json.la` (`parse`, `parse_with`, `stringify`, `pretty`, `Limits`, `limits`, the helpers; re-exports the types), `src/std/json_value.la` (`std.$json_value`: `Value`, `JsonError`, `ErrorKind`, `message()`, `Limits`), `src/std/json_parse.la` (`std.$json_parse`), `src/std/json_write.la` (`std.$json_write`); each registered in `STD`; doc comments on every public item.
- `tests/fixtures/JSONTestSuite/` (cases, `LICENSE`, `README`), `tests/json.rs`.
- Tests written before the module (TESTING RULE): `tests/json.rs` first, failing.
### Must NOT Do
- Decision 339's exclusions; any compiler, runtime or `std.http` change without stopping first; recursion not bounded by the depth limit; silently accepting duplicates or non-finite floats.
### New dependencies
- none.

## ERROR HANDLING REQUIREMENTS
- `JsonError` with `ErrorKind` (decision 333) for every malformed, duplicate, overflowing, over-limit or unwritable input, with its 1-based line and column; never a panic.

## SECURITY CONSIDERATIONS
- Hostile input: the size limit checked first; depth before descending; a count of values; integers checked for overflow; escapes checked for surrogates; duplicate keys refused; no quadratic behaviour (members collected in a `Map`, strings built once from bytes).
- No I/O, no `unsafe`, no crypto, no new crate.

## TESTS TO WRITE
- [ ] JSONTestSuite: every `y_` parses; every `n_` is `Err` (not a panic, not `Limit`) or invalid UTF-8; every `i_` matches its pinned outcome.
- [ ] Values: each kind; `Integer` vs `Float` boundaries (`9223372036854775807` is `Integer`, `9223372036854775808` is `Float`, `-9223372036854775808` is `Integer`, `1.0` and `1e2` are `Float`, `-0` and `-0.0`); surrogate pairs; every escape; key order kept.
- [ ] Errors: kind, line and column for a duplicate key (the OBJECTIVE's example), a trailing comma, a missing `:`, a lone surrogate, a raw control character, `1e400` (`Overflow`), trailing text, a BOM; CRLF counted as one line end; a multi-byte character before the error counts as one column.
- [ ] Limits: 1048577 bytes is `Limit`, 1048576 bytes of valid JSON parses; depth 100 parses and 101 is `Limit` (arrays and objects); the value count past `values` is `Limit`; `parse_with` with small limits.
- [ ] Hostile: 100 000 nested `[` (refused by depth, no stack growth); a 1 MB string; a 60 000-member object and a 100 000-element array in linear time; 60 000 duplicate keys stops at the first.
- [ ] Writing: `stringify` and `pretty` of each kind and of empty arrays/objects (exact text); escapes; non-ASCII kept; floats (`1.0`, `0.1`, `1e300`, `5e-324`, `-0.0`) valid JSON that round-trips; NaN, Infinity and -Infinity, also nested, are `Err(NotFinite)`.
- [ ] Round trip: every `y_` case, `parse(stringify(v)?)` equals `v`, and `pretty` too.
- [ ] Helpers: `get`, `at` (in range, out of range, negative, wrong kind), every `as_*` and `is_null` on a matching and a non-matching value; chained with `and_then`.
- [ ] Lazy std: a program not importing `std.json` has none of its functions in the IR (`tests/lazy_std.rs` pattern).

## ROLLBACK PLAN
- `main`; revert the merge. The module is new and nothing else depends on it.

## ACCEPTANCE CRITERIA
- [ ] All of JSONTestSuite's `test_parsing` cases behave as decision 341 says.
- [ ] The OBJECTIVE program runs with the output shown.
- [ ] New tests written first and passing; narrow tests (`json`, `lazy_std`, `lsp_docs`, `fmt_corpus`, `programs` if a std-module list changes).
- [ ] `cargo clippy --all-targets -- -D warnings` and `cargo fmt --check` pass.
- [ ] Every public item has a doc comment; spec, CHANGELOG, CLAUDE.md updated.

## VALIDATION
- `cargo test --lib`
- `cargo test --test json --test lazy_std --test lsp_docs --test fmt_corpus`
- `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`
