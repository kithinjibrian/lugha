## FEATURE: Derivable interfaces in the bundled sources, with `json.encode`/`json.decode` moved onto them; milestone 141, v17.

**Status:** approved 2026-10-31 ("approved"), built the same day; proposed 2026-10-31, after discovery (4 questions, each answered with the recommended option): (1) scope: the mechanism plus JSON moved onto it, byte for byte; printing and `==` stay built in; (2) built-in types are covered by `derive <Interface> for <type> { … }` blocks that only the interface's own module may write (Rust's orphan rule, narrowed to the interface's owner), adding no method visible anywhere else; (3) structs and enums are reached through field intrinsics (`$fields`, `$build`, `$variant`, `$build_variant`) that call a generic function once per field, unrolled per type by codegen; (4) bundled sources only: programs see no change, and a later PRP opens `derive` to every module. Also settled: a program type's own `to_json`-shaped methods don't override the derived walk yet (decided with the opening PRP).

Background (the metaprogramming discussion that led here): Lugha meets each metaprogramming need with a targeted feature rather than macros or Zig-style comptime. Macros would break core decision 3 (the spec defines the language) and architecture rule 1; comptime with types as values would break decisions 33 and 41 (generic bodies checked once). Following a type's shape (serialise, compare, hash) is the most common need and the one Lugha keeps paying for in compiler code; this PRP is its first step. A DECISIONS.md entry records this (see DESIGN).

## OBJECTIVE
Nothing a program can see changes. This program prints exactly what it prints today (PRP-171's OBJECTIVE):
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
}
```
What changes is where that code comes from. JSON's per-type knowledge moves out of the compiler (`check/json.rs`'s helper tables, `codegen/json_encode.rs`, `codegen/json_decode.rs`: about 1 000 lines of Rust) into Lugha in `std`:
```lugha
// std.$json_value (sketch)
public interface Json {
    fun $to_json(self): Value;
    fun $from_json(v: Value, path: string): Result<Self, DecodeError>;
    fun $missing(path: string): Result<Self, DecodeError>;   // a struct field absent from the object
}

derive Json for i64 {
    fun $to_json(self): Value = Value.Integer(self);
    fun $from_json(v: Value, path: string): Result<i64, DecodeError> = $dec_int(v, path, i64.MIN, i64.MAX, "i64").map(fun(n) = n);
    fun $missing(path: string): Result<i64, DecodeError> = $no_field(path);
}

derive Json for Option<T> {                       // T: Json, implied
    fun $missing(path: string): Result<Option<T>, DecodeError> = Ok(None);
    …
}

derive Json for struct {                          // any struct whose fields all meet Json
    fun $to_json(self): Value = $object($fields(self, (), $enc_field));
    fun $from_json(v: Value, path: string): Result<Self, DecodeError> {
        let m = $dec_object(v, path)?;
        $build(Obj { m: m, path: path }, $dec_field)
    }
    …
}

fun $enc_field<F: Json>(_: (), name: string, x: F): Value = x.$to_json();

fun $dec_field<F: Json>(c: Obj, name: string, at: i64, of: i64): Result<F, DecodeError> {
    let path = $field_path(c.path, name);
    match c.m.get(name) {
        Some(x) => F.$from_json(x, path),
        None => F.$missing(path),
    }
}
```

## CONTEXT
- Read first: PRP-171 (what JSON does today and why); `src/check/json.rs` (412 lines: the bound, E0450, helper tables, `reach_json`/`reach_decode`); `src/codegen/json_encode.rs` (255), `src/codegen/json_decode.rs` (356); `src/std/json_derive.la`, `json_from.la`, `json_value.la`, `json.la`; default methods in the checker (`check/defaults.rs`) and codegen (spec §9, `lugha_fn_<Interface>.<method><Type>`), whose per-type instantiation derive blocks reuse; `check/satisfy.rs` (structural satisfaction).
- Creates: `src/check/derive.rs` (derive blocks: declaration checks, which block a type uses, bound satisfaction through blocks with E0450's paths), `src/check/derive_intrinsics.rs` (typing `$fields`, `$build`, `$variant`, `$build_variant`, `$variants`), `src/codegen/derive.rs` (unrolling the intrinsics per type); parser support for the `derive` item in bundled sources (`src/parser/`).
- Changes: `src/std/json_value.la` (`Json` gains its three `$` items), `json.la` (`encode` is `x.$to_json()`, `decode` is `T.$from_json(v, "")`); `json_derive.la` and `json_from.la` become the derive blocks and their helpers (split by direction or by kind if either passes 500 lines); `src/check/json.rs` shrinks to what's JSON-specific (E0450's wording, the `Json` key); `src/codegen/expr.rs`, `codegen/mod.rs`, `check/instances.rs`, `check/mod.rs` lose the `$json.encode`/`$json.decode` special cases and `json_walkers`.
- Deletes: `src/codegen/json_encode.rs`, `src/codegen/json_decode.rs`.
- Spec: §9 internals only (how `std.json` is compiled; §8's symbol table loses `lugha_json_enc_<type>`/`lugha_json_dec_<type>`, whose code is now ordinary derive-method instances named like default methods, `lugha_fn_std/$json_value/Json.$to_json<Point>`); the `std.json` text doesn't change; §11 row 141; "Changes since v16". The v17 Draft gets a "Derivable interfaces (milestone 141)" entry before the build (architecture rule 9).
- Decisions: core decision 6 (symbol prefixes: no new prefix; derive instances use the default-method form), 33 and 41 (bodies checked once: each derive block is checked once, against its `Self`), 3 (no program-visible behaviour, so the spec stays exact). None open. No dependency on decision 4: nothing runs at build time.

## REQUIREMENTS

### Must Do
- **`derive I for X { … }` items**, accepted only in bundled sources (`std`, the prelude) and only in the module that declares interface `I`. `X` is one of:
  - a built-in or library type, possibly generic: `i64`, `string`, `T[]`, `Option<T>`, `Map<K, V>`, `Set<T>`, `std.$json_value.Value`; its type parameters must meet `I` (and the type's own bounds, `K: Eq`);
  - `tuple` (any tuple whose elements meet `I`), `struct` (any struct whose fields meet `I`), `enum` (any enum whose payloads meet `I`).
  A block defines every item of `I` for `Self` = `X` (the signature with `Self` replaced, as satisfaction already compares; E0422-style mismatch is an internal error, since only bundled sources write blocks). A type uses the block for its exact type constructor first, then the `tuple`/`struct`/`enum` one.
- **A type meets a derivable interface** when its block's requirements hold, recursively (cycles are fine: a recursive type meets it if every part does). Otherwise **E0450**, unchanged in code, wording and path (`` `Job` can't be turned into JSON: its field `run` is a function ``, the map-key note). A generic parameter with the bound meets it, as today.
- **Field intrinsics** (bundled sources only; typed by the enclosing block's `Self`):
  - `$fields(x: Self, ctx: C, f): R[]` in a `struct` or `tuple` block: `f` names a generic function `fun f<F: I>(ctx: C, name: string, x: F): R`; called once per field in declaration order (tuples: names `"0"`, `"1"`, …).
  - `$build(ctx: C, f): Result<Self, E>` in a `struct` or `tuple` block: `fun f<F: I>(ctx: C, name: string, at: i64, of: i64): Result<F, E>` makes each field (`at` its position, `of` the field count); the first `Err` is returned.
  - `$variant(x: Self, ctx: C, f): (string, R[])` in an `enum` block: the variant's name and its payloads through `fun f<F: I>(ctx: C, name: string, x: F): R`.
  - `$variants(): (string, i64)[]` in an `enum` block: each variant's name and payload count, in declaration order.
  - `$build_variant(name: string, ctx: C, f): Option<Result<Self, E>>` in an `enum` block: `None` for an unknown name, else the variant built from `f` as for `$build`.
  Each block is type-checked once, with `F` known only to meet `I`; codegen expands each intrinsic per concrete `Self` into straight-line calls of `f` at each field's type.
- **JSON on top of it:** `Json` gets `$to_json`, `$from_json`, `$missing`; derive blocks for every type in PRP-171's shapes table, `Value` (itself), `tuple`, `struct`, `enum`; `Map` keys through an internal `$JsonKey` interface with blocks for `string` and each integer type. `json.encode`/`json.decode` call them. Output, `DecodeError` paths and details, and E0450 diagnostics are identical, character for character.
- **Laziness:** a program that never reaches `json.encode`/`json.decode` compiles no derive-method instance (as today, with the new symbol names).
- **Performance:** encoding and decoding a large value (a struct-heavy array of about 1 MiB of JSON) stays within 10% of today at `-O2`, measured by hand against `main` with a scratch benchmark program and recorded in the PRP's BUILT section.
- Milestone 141; tag `m141`.

### Must NOT Do
- Let programs write `derive` blocks or call the intrinsics; make `derive` a keyword in programs (it stays an identifier there); add any visible method to a built-in type. That's the next PRP.
- Let a program type's own `to_json`/`from_json` methods override the derived walk (decided with the opening PRP).
- Move printing, `==` or hashing onto derive blocks: they need no bound and cover every type; a separate decision.
- Change E0450's text, JSON's shapes or any `std.json` behaviour.
- Run any code at build time (no evaluator): expansion happens in instantiation, as monomorphisation does now.

## BUILT — milestone 141
- As planned, with these differences:
  - The block keyword is **`$derive`**, not `derive`: `$` lexes only in bundled sources, so programs can't write a block and `derive` stays a name without the parser knowing which source it reads. The field intrinsics are `$derive.fields(…)` and so on.
  - A block's bounds are **written**, not implied: `$derive<T: Json> Json for T[]`, `$derive<K: Eq + $JsonKey, V: Json> Json for Map<K, V>`. Map keys go through an internal derivable interface `$JsonKey` (blocks for `string` and the eight integer types).
  - `Json`'s third item is `$missing(path, name)`: the struct's path and the field's name, so the `Option` block answers `Ok(None)` and every other block "missing field name" exactly as before.
  - A misused block or intrinsic in a bundled source is a diagnostic (E0201 for a block, E0415 for an intrinsic, as `$json` misuse was), not an exit-2 internal error; only lughac's sources can reach either.
  - Satisfaction went into its own file, `check/derive_meets.rs` (159 lines), beside `check/derive.rs` (387) and `check/derive_intrinsics.rs` (220); codegen is `codegen/derive.rs` (391). `codegen/methods.rs` gained `call_method`, shared by ordinary and derived calls.
  - **Size, against the PRP's estimate:** the PRP guessed about 400 lines of Rust removed net. In fact about 1 060 lines of JSON-specific Rust went (`codegen/json_encode.rs`, `codegen/json_decode.rs`, most of `check/json.rs`, the `$json` hooks), and about 1 550 lines of general mechanism came in, about 190 of them unit tests: net +489 Rust. The Lugha side is net +215 (`std.$json_value` 452 lines; `std.$json_derive` deleted; `std.$json_from` trimmed to the scalar decoders and pieces).
- Byte for byte: `m139/encode`, `m139/not_data` (every E0450 path and wording), `m140/decode` and `m140/no_type` pass unchanged, at `-O0` and `-O2`.
- Performance, `-O2`, 8 000 structs with an enum, an `Option` and an array each, encoded and decoded 10 times (about 0.85 MiB of JSON a round), against `main`: encode only 0.88 s vs 1.00 s, 191 MB vs 211 MB peak; decode only 0.90 s vs 0.91 s, 90 MB vs 93 MB; both in one run 1.94 s vs 1.84 s (+5%, inside the 10%), but 267 MB vs about 170 MB peak, which the split runs show is Boehm growing its heap at different moments rather than more garbage.
- Tests: `parser/derive.rs` (3), `check/derive.rs` (4: block choice, recursive and generic types, misses, misuse), `check/derive_intrinsics.rs` (2), `check/json.rs` (E0450's wording); `tests/json.rs`'s laziness check now looks for `Json@` and `$from_json` symbols; `tests/programs/m141/derive_name.la` (`derive` and `tuple` as names, JSON of them).

## DESIGN
- **Checking once:** a derive block is checked like a set of default methods, with `Self` the block's type (a hidden type with only `I`'s items for `struct`/`enum`/`tuple` blocks, plus the intrinsics). Generic functions passed to an intrinsic are named functions (no closures, no new rank-2 types): the intrinsic, like every intrinsic, accepts an argument an ordinary function can't (spec §5, "Intrinsics").
- **Instantiation:** a derive method is instantiated per concrete `Self` exactly as a default method is per type; the checker's `Checked.instances` lists each per-field `f<F>` it reaches, so codegen still never infers types (architecture rule 3).
- **Diagnostics:** no new code. E0450's path search moves from `check/json.rs` into `check/derive.rs`, parametrised by the interface; the message text for `Json` stays in `check/json.rs`. A bundled-source misuse (a block outside its interface's module, an intrinsic outside a block) is an internal error with exit 2, since only lughac's own sources can reach it.
- **DECISIONS.md:** a new entry, "Metaprogramming: a targeted feature per need (PRP-172)": shape → derivable interfaces; earlier work → `const` calling pure functions (later); platform → `@cfg` on items (later, with portability stage C); wrapping signatures → wait; new syntax → none. Rules out macros (decision 3, rule 1) and comptime with types as values (decisions 33, 41). Lists as open for the next PRP: opening `derive` to programs, and whether a type's own methods override a derived interface.
- **Sizes:** `check/derive.rs` ~350, `check/derive_intrinsics.rs` ~250, `codegen/derive.rs` ~300, parser ~60; the JSON derive blocks ~450 Lugha across two files. Net Rust removed ≈ 400 lines. Every file under 500.
- No new dependency, no `unsafe`, no runtime change, no external input.

## TESTS
- [ ] `tests/programs/m139/`, `m140/` unchanged and passing (the proof: same stdout, same E0450 and E0412 stderr), and at `-O2`.
- [ ] `tests/json.rs`: the laziness checks updated to the new symbol names (a parse-only program has no `$to_json` instance; an encode-only one no `$from_json`).
- [ ] By hand: the 1 MiB encode/decode timing against `main`, recorded in BUILT.
- [ ] Unit tests in `check/derive.rs`: block choice (exact constructor beats `struct`), satisfaction through nested generics and recursion, E0450 paths through a tuple, an enum payload and a map key; a block outside its interface's module and an intrinsic outside a block are internal errors.
- [ ] Unit tests in `check/derive_intrinsics.rs`: each intrinsic's result type; `f` whose bound isn't the block's interface rejected; `$variants()` order and counts.
- [ ] A program using `derive` as a name (`let derive = 1;`, `fun derive()`) still compiles (`tests/programs/m141/derive_name.la`).

## ACCEPTANCE
- [ ] The OBJECTIVE program prints what it shows; `m139`/`m140` pass byte for byte.
- [ ] `codegen/json_encode.rs` and `codegen/json_decode.rs` are gone; no `$json.encode`/`$json.decode` special case remains in the checker or codegen.
- [ ] `cargo test --lib`, `--test programs`, `--test json`, `--test lazy_std`, `--test explain`, `--test http`; `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`.
- [ ] Every `pub` item documented; DECISIONS.md entry added; spec (§8, §9, §11 row 141, "Changes since v16"), the v17 Draft, CHANGELOG and TODO updated.
