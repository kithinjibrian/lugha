## FEATURE: `json.encode(x)` and `json.decode(v)` for any data type, the per-type code written by the compiler; milestones 139 (encode) and 140 (decode), v17.

**Status:** proposed 2026-10-31, after discovery (5 questions, each answered with the recommended option): (1) built in for any type, no annotation, as printing, `==` and hashing are; (2) enums externally tagged, `Option` as `null`/the value; (3) decoding ignores unknown keys, fills a missing `Option` field with `None`, and is strict on types, with errors naming a path; (4) data types only, everything else a compile error (E0450), maps with string or integer keys; (5) one PRP, built and tagged as two milestones.

## OBJECTIVE
```lugha
import std.json;

struct Person { name: string, age: i64, email: Option<string>, tags: string[] }
enum Shape { Dot, Circle(f64), Rect(f64, f64) }

fun main() {
    let p = Person { name: "ada", age: 36, email: None, tags: ["math"] };
    println(json.stringify(json.encode(p)).unwrap());
    // {"name":"ada","age":36,"email":null,"tags":["math"]}
    println(json.stringify(json.encode([Shape.Dot, Shape.Circle(1.5), Shape.Rect(2.0, 3.0)])).unwrap());
    // ["Dot",{"Circle":1.5},{"Rect":[2.0,3.0]}]

    let v = json.parse("{\"name\": \"bob\", \"age\": 7, \"tags\": [], \"extra\": true}").unwrap();
    let back: Person = json.decode(v).unwrap();            // email missing → None; extra ignored
    let bad: Result<Person, json.DecodeError> = json.decode(json.parse("{\"name\": 1}").unwrap());
    println(bad);  // Err(name: expected a string, found an integer)
}
```
Programs turn their own structs and enums into JSON and back — an HTTP handler reads a request into a struct and answers with one — without writing a converter per type.

## CONTEXT
- Starting state: `std.json` (milestone 137) has `Value`, `parse`, `stringify`; `std.http` (138) has `req.json()` and `http.json(status, v)`. The compiler already writes per-type code for printing (`check/printing.rs`, `codegen/show.rs`: IR walkers calling Lugha `show` methods for library types such as `Map`), `==` (`codegen/equality.rs`) and hashing (`check/hashing.rs`, `codegen/hashing.rs`).
- Ending state: `src/std/json.la` gains `encode`, `decode`, `DecodeError`; a new internal `src/std/json_derive.la` (`std.$json_derive`) holds the small Lugha helpers the generated code calls; `src/check/json.rs` (the `Json` bound, E0450, instances to record); `src/codegen/json_encode.rs`, `src/codegen/json_decode.rs` (the per-type walkers); `docs/errors/E0450.md`; tests.
- Spec: the standard library section (`std.json`), §5 (where printing and `==` are defined structurally), §9 (E0450), §11 rows 139–140, "Changes since v16". Explicit type arguments don't exist (TODO), so `decode`'s type comes from context, as `None` and `#{}` do.
- Decisions it depends on: none open.

## REQUIREMENTS

### Must Do
- **`json.encode<T: Json>(x: T): json.Value`** never fails. **`json.decode<T: Json>(v: json.Value): Result<T, json.DecodeError>`**.
- **`Json` is a built-in bound** (named `json.Json` in programs, like `Eq`/`Ord` are built in): satisfied structurally. JSON-able: every integer and float type, `bool`, `char`, `string`, `()`, arrays of JSON-able types, tuples, `Map<K, V>` with `K` `string` or an integer type, `Set<T>`, `Option<T>`, structs and enums whose fields and payloads are JSON-able (recursive types included), and `json.Value`. Anything else — functions, closures, interface values, `Ref`, `Task`, `Chan`, actor handles, `extern type` handles, `CPtr`, maps with other keys — is **E0450** at the call, naming the path to the first bad part: `` `Job` can't be turned into JSON: its field `run` is a function ``.
- **Shapes** (encode writes them; decode reads them):
  | Lugha | JSON |
  |---|---|
  | integers (`u64` above `i64.MAX` as a `Float`, documented as lossy) | `Integer` |
  | `f32`, `f64` (NaN and infinities stay `Float`; `stringify` refuses them) | `Float` |
  | `bool` / `string` / `char` (a one-character string) / `()` (`null`) | as named |
  | arrays, tuples, `Set` | arrays (tuples by position, sets in their order) |
  | `Map<string, V>` / `Map<integer, V>` | objects (integer keys as decimal strings), in map order |
  | `Option<T>` | `None` → `null`, `Some(x)` → `x` (so `Some(None)` encodes as `null` and decodes as `None`: documented) |
  | struct | an object of every field, in declaration order, by its written name (all fields, as printing shows them) |
  | enum variant | no payload → `"Name"`; one → `{"Name": value}`; several → `{"Name": [a, b]}` |
  | `json.Value` | itself |
- **Decoding rules**: unknown object keys are ignored; a missing field is `None` if its type is `Option`, an error otherwise; an `Integer` fills any integer type it fits (range-checked, an error otherwise), any number fills `f32`/`f64` (`f32` rounded as `as` does), a `Float` never fills an integer type; a `char` needs a one-character string; a tuple needs an array of exactly its length; a `Set` from an array with a repeat is an error; integer map keys must parse as that integer type; an enum is a variant name string or a one-key object naming a variant with a payload of the right shape; `null` fills `()` and `None` only.
- **`DecodeError`**: `public struct DecodeError { public path: string, public detail: string }` with `message()` = `"{path}: {detail}"`, or `detail` alone at the root. Paths join fields with `.` and indexes with `[i]`, map keys as `["key"]`: `people[2].age`, `scores["ann"]`. Details: `expected a string, found an integer`, `missing field age`, `99999 doesn't fit u8`, `unknown variant Square`, `expected an array of 2, found 3`.
- **Nothing panics** in generated code on any `Value`; recursion follows the value, whose depth `json.parse` already bounds.
- **Generated code** follows printing's pattern: the checker records each type `encode`/`decode` is instantiated at (and the library instances the walkers call); codegen emits one `lugha_json_enc_<type>` / `lugha_json_dec_<type>` per type reached (symbols listed with the others in spec §8), calling `std.$json_derive`'s Lugha helpers to build and read `Value`s, maps and errors. A program that never calls them compiles none of it (lazy std).
- Milestone 139: `encode`, the `Json` bound, E0450 (its `docs/errors/` page), spec and tests; tag `m139`. Milestone 140: `decode`, `DecodeError`, spec and tests; tag `m140`.

### Must NOT Do
- Per-field options (renaming, skipping, defaults, flattening), attributes such as `@json`, other enum shapes, base64 bytes: later PRPs if wanted.
- Explicit type arguments (`json.decode<Person>(v)`): a separate language feature.
- Implicit conversions anywhere else, or changing how `Value`, `parse` or `stringify` behave.
- Encoding interface values by their dynamic type (they're E0450).

## DESIGN
- **New diagnostic E0450** (checker range): a type that isn't JSON-able where `Json` is required, with the path to the first bad part and a note for maps with other key types. Page `docs/errors/E0450.md`.
- **New symbols** `lugha_json_enc_<type>`, `lugha_json_dec_<type>` (spec §8; core decision 6's rule of disjoint prefixes).
- **Private fields** are encoded and filled like public ones (as printing shows them); a type's invariants are not re-checked by `decode`. Documented.
- Size estimate: checker ~250 lines, codegen ~600 across two files, `json_derive.la` ~200; every file under 500 lines.
- No new dependency, no `unsafe`, no runtime change.

## TESTS
- [ ] `tests/programs/m139/`: encode of every shape in the table (each integer and float type, `u64` past `i64.MAX`, `char`, `()`, tuples, sets, both map kinds, `Option` nesting, a recursive enum, a struct holding a `json.Value`, generic structs); `stringify` of a NaN field refused; E0450 for a function field, a closure, an interface value, a `Ref`, a `Map<bool, V>` (one `.stderr` each, with paths through nested structs and arrays).
- [ ] `tests/programs/m140/`: decode round-trips of every m139 value; every error detail and path (wrong type, missing field, out-of-range integer, `Float` into an integer, bad `char`, tuple length, repeated set element, bad integer key, unknown variant, malformed variant object, `null` into a non-`Option`); unknown keys ignored; missing `Option` field `None`; decode with no type in context is E0412.
- [ ] `tests/lazy_std.rs`-style: a program using only `json.parse` emits no `lugha_json_` function; one using `encode` emits no decoder.
- [ ] `tests/explain.rs` picks up E0450's page; its examples check.
- [ ] Hostile: decoding a 1 MiB, depth-100 `Value` into a recursive enum doesn't overflow the stack.

## ACCEPTANCE
- [ ] The OBJECTIVE program prints what it shows.
- [ ] `cargo test --lib`, `--test programs`, `--test json`, `--test explain`, `--test lazy_std`, `--test lsp_docs`, `--test fmt_corpus`; `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`.
- [ ] Every `pub` item documented; spec (std section, §8 symbols, §9 E0450, §11 rows), CHANGELOG, TODO and CLAUDE.md updated.
