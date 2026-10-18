## FEATURE: `std.toml` — a read-only TOML 1.0 parser written in Lugha, safe on hostile input; milestone 114 (v17).

**Status:** approved 2026-10-18 (session 276); proposed in session 275, from the user's brief (session 275).

## OBJECTIVE
```lugha
import std.toml;

let doc = toml.parse(read_file("rapidkoi.toml")?)?;           // Result<Map<string, toml.Value>, TomlError>
let name = toml.get(doc, "app.name").and_then(toml.as_text);  // Option<string>
match toml.parse("a = 1\na = 2") {
    Err(e) => println(e.message()),   // "a key defined twice: a (line 2, column 1)"
    Ok(_) => {}
}
```
Programs read configuration files — first Rapidkoi's `rapidkoi.toml` — with the whole of TOML 1.0, as Python's `tomllib` does, and nothing an attacker writes in one can crash or exhaust them.

## CONTEXT
- Written entirely in Lugha as a std module (like `std.http`), loaded lazily; no compiler or runtime change expected. Strings are UTF-8 by construction, so invalid UTF-8 never reaches the parser.
- `Map` keeps insertion order (milestone 101), so tables keep the file's order.
- Files are limited to 500 lines, so the module is split: public types in an internal `std.$toml_value`, the parser in `std.$toml_parse`, and `std.toml` re-exporting the types (`public import std.$toml_value { * };`) and wrapping the parser — the `$` modules are importable only by bundled sources (milestone 111).
- Tests use toml-test (github.com/toml-lang/toml-test, v2.2.0, MIT), the cases its `files-toml-1.0.0` names.
- Related: `src/std/{http,http_wire}.la` (the split pattern), `src/lib.rs` (`STD`), spec standard-library section.

## DECISIONS (draft decisions 198–214)
198. **`parse(text: string): Result<Map<string, Value>, TomlError>`** — a document is its root table. `parse_with(text, limits: Limits)` for other limits.
199. **`Value`** (TOML 1.0's data model):
     ```lugha
     public enum Value {
         Text(string), Integer(i64), Float(f64), Boolean(bool),
         OffsetDateTime(Date, Time, i64),  // the offset in minutes east of UTC (`Z` is 0)
         LocalDateTime(Date, Time), LocalDate(Date), LocalTime(Time),
         Array(Value[]), Table(Map<string, Value>),
     }
     public struct Date { public year: i64, public month: i64, public day: i64 }
     public struct Time { public hour: i64, public minute: i64, public second: i64, public nanosecond: i64 }
     ```
     Floats include `inf`, `-inf` and `nan`. Fractional seconds keep up to 9 digits (nanoseconds); more digits are truncated, as TOML 1.0 allows. Dates and times are checked (month 1–12, the day within its month, leap years, hour < 24, minute < 60, second ≤ 60 for a leap second), with no time-zone arithmetic.
200. **Tables keep their keys in the file's order** (`Map`).
201. **The whole of TOML 1.0**: bare, quoted and dotted keys; basic, literal and both multi-line strings with every escape (`\b \t \n \f \r \" \\ \uXXXX \UXXXXXXXX`, line-ending backslash), rejecting surrogates and code points past U+10FFFF; integers in decimal (with sign), `0x`, `0o`, `0b`, with underscores only between digits and no leading zeros; floats with fraction and/or exponent, `inf`/`nan` with signs; booleans; all four date-time forms (`T`, `t` or a space between date and time); arrays (mixed types, trailing comma, comments and newlines inside); inline tables (one line, no trailing comma); `[table]` and `[[array.of.tables]]` headers; comments; LF and CRLF line ends; control characters refused where TOML forbids them.
202. **Definition rules** (TOML 1.0 §Table, §Inline Table, §Array of Tables): a key defined twice, a table defined twice (`[a]` twice, or `[a]` after `a.b = 1` made `a` a dotted-key table), a value redefined as a table, extending an inline table or a static array (`a = []` then `[[a]]`), and dotted keys reopening a table defined by a header are errors.
203. **`TomlError`**: `public struct TomlError { public kind: ErrorKind, public detail: string, public line: i64, public column: i64 }` with `e.message()` = `"{detail} (line {line}, column {column})"`, line and column 1-based (columns count characters). `ErrorKind` is `Syntax`, `Duplicate` (a key or table defined twice), `Immutable` (extending an inline table or static array), `Overflow` (an integer past i64), `BadValue` (an invalid date, time or escape) and `Limit` (a limit passed). Details name the problem plainly: `"a key defined twice: app.name"`, `"expected '=' after a key"`, `"an integer too large for i64: 9223372036854775808"`.
204. **Nothing panics on any input**: every index is checked before use; integer parsing checks overflow before each multiply and add.
205. **Limits** (`Limits`, `limits()` gives the defaults): `bytes` 1048576 (the text's size), `depth` 100 (arrays and inline tables nested in each other, plus a key's dotted parts and a header's), `values` 1000000 (every value and table created). Depth is checked before descending, so recursion is bounded by it; a limit passed is `Limit` at the place it happened.
206. **Helpers**, so callers needn't match deeply: `get(table: Map<string, Value>, path: string): Option<Value>` follows a dotted path (`"app.name"`; quoted segments aren't parsed — keys containing dots are reached through the maps); `as_text`, `as_i64`, `as_f64` (an integer too, widened), `as_bool`, `as_array`, `as_table`, `as_date_time` (any of the four kinds, as `(Option<Date>, Option<Time>, Option<i64>)`), each `fun(Value): Option<…>`, usable with `and_then`.
207. **Read-only**: no writing or serialising; no TOML 1.1; no mapping into structs; no file reading (callers use `read_file`).
208. **Implementation**: a hand-written scanner over the text's bytes with one position, line and column; no regular expressions, no backtracking beyond a token.
209. **The suite as fixtures**: toml-test v2.2.0's TOML 1.0 cases (as listed in its `files-toml-1.0.0`), copied into `tests/fixtures/toml-test/` with its `LICENSE` (MIT) and a `README` naming the version and source. Not edited, so they can be refreshed by copying again.
210. **How the suite is run**: `tests/toml.rs` builds one Lugha decoder program once, which prints a parsed document as toml-test's tagged JSON (`{"type": "integer", "value": "42"}`), and runs it on every case; the Rust test compares with the expected JSON through `serde_json` (already a dependency), normalising as toml-test does: integers and floats by value (`nan` equal to `nan`), date-times by their parts (`Z` = `+00:00`, `t`/`T`/space alike, trailing fractional zeros ignored).
211. **Docs**: the spec's standard-library section (`std.toml`), the milestones table (114), the "Changes since v16" list; CHANGELOG; `CLAUDE.md`'s tree.
212. **Errors in the suite**: every invalid case must be an `Err`, and must not panic or hit a limit; the message isn't compared (toml-test doesn't define messages).
213. **No new dependency** (the fixtures are test data under MIT, noted).
214. **Success**: milestone 114, tag `m114`.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- `src/std/toml.la` (public API: `parse`, `parse_with`, `Limits`, `limits`, `get`, the `as_*` helpers; re-exports the types), `src/std/toml_value.la` (`std.$toml_value`: `Value`, `Date`, `Time`, `TomlError`, `ErrorKind`, `message()`), `src/std/toml_parse.la` and, if needed for the 500-line rule, `src/std/toml_scan.la` (`std.$toml_parse`, `std.$toml_scan`: the scanner, keys, values, strings, numbers, date-times, tables and the definition rules); each registered in `STD`; doc comments on every public item.
- `tests/fixtures/toml-test/` (cases, `LICENSE`, `README`), `tests/toml.rs`.
### Must NOT Do
- Decision 207; any compiler, runtime or `std.http` change; recursion not bounded by the depth limit.
### New dependencies
- none.

## ERROR HANDLING REQUIREMENTS
- `TomlError` with `ErrorKind` (decision 203) for every malformed, contradictory or over-limit input, with its 1-based line and column; never a panic.

## SECURITY CONSIDERATIONS
- Hostile input: the size limit is checked first; depth before descending; a count of values; integers checked for overflow; strings' escapes checked for surrogates and range; no recursion beyond the depth limit; no quadratic behaviour in the scanner (keys and tables are looked up in `Map`s).
- No I/O at all.

## TESTS TO WRITE
- [ ] toml-test v2.2.0, TOML 1.0 cases: every valid case parses to its expected JSON; every invalid case is `Err` (not a panic, not `Limit`).
- [ ] Limits: a text of 1048577 bytes is `Limit`; 1048576 bytes of valid TOML parses; depth 100 parses and 101 is `Limit` (arrays, inline tables, dotted keys); the value count past `values` is `Limit`; `parse_with` with small limits.
- [ ] Hostile: 100 000 nested `[` (refused by depth, no stack growth), `9223372036854775808` and `-9223372036854775809` (`Overflow`), `0x8000000000000000` (`Overflow`), a 1 MB string, a million-key table at the size limit (fast: no quadratic time), a long run of `[[a]]` headers.
- [ ] Errors: kind, line and column for a duplicate key (the brief's example), a duplicate table, extending an inline table, extending a static array, a bad escape, an invalid date, a missing `=`; CRLF counted as one line end.
- [ ] Helpers: `get` through nested tables and a missing segment; every `as_*` on a matching and a non-matching value.

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] The whole TOML 1.0 suite passes; narrow tests (`toml`, `lazy_std`, `lsp_docs`, `fmt_corpus`, `programs` for E0312's list), fmt, clippy; CHANGELOG.

## VALIDATION
- `cargo test --test toml --test lazy_std --test lsp_docs --test fmt_corpus --test programs`, fmt, clippy
