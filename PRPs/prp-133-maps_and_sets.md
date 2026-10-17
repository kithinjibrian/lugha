## FEATURE: Maps and sets — `Map<K, V>` and `Set<T>` with `#{ … }` literals, built in; milestone 101 (v17).

**Status:** proposed in session 243, after a discovery of seven questions answered by the user (literal syntax, Q1–Q6).

## OBJECTIVE
Lugha programs get the two collections every program reaches for, with a literal that reads like the data:

```lugha
let mut ages = #{"ann": 31, "bo": 40};   // Map<string, i64>
ages["cy"] = 25;
let seen = #{1, 2, 3};                   // Set<i64>
for (name, age) of ages { println("{name} is {age}"); }
println(ages);                           // #{"ann": 31, "bo": 40, "cy": 25}
```

They are values like arrays, keyed by any `Eq` type but floats, visited in insertion order, and written in Lugha in the prelude over one new compiler built-in, `$hash`.

## CONTEXT
- Starting state: no map or set; arrays, tuples, `Option`, `Ref<T>`. The prelude's iterator adapter type returned by `it.map(f)` is named `Map<T, U>` (`src/prelude/iter.la`). `{` starts a block; `#` is not a token. Equality is structural and compiler-generated (`lugha_eq_*`, `codegen/equality.rs`); arrays are copy-on-write values (`codegen/cow.rs`).
- Ending state: lexer `#{`; parser map and set literals; checker types `Map<K, V>`/`Set<T>` as prelude generic structs with literals, indexing, `for … of`, printing and `==`; codegen `$hash` per type (`lugha_hash_*`); `src/prelude/maps.la`; iterator type renamed `Mapped`; spec v17 §2–§11; new codes E0441, E0442, W0105.
- Related existing code: `src/prelude/{core,iter}.la`, `src/check/{array,access,iterators,printing,bounds,literal}.rs`, `src/codegen/{equality,show,array,cow}.rs`, `src/parser/{primary,tuples}.rs`, `src/formatter/exprs.rs`, `src/driver/lsp/{completion,members}` (members of the new types come from the prelude like other helpers).
- Open decisions: none.

## DECISIONS (session 243; draft decisions 67–80)
67. **Literal:** `#{ k: v, … }` is a map, `#{ x, … }` a set; the first entry decides, mixing is E0201; a trailing comma is allowed. `#{}` is empty: typed by its expected type like `[]` (E0412 with none). `{` alone stays a block. (User.)
68. **Names (Q1):** `Map<K, V>` and `Set<T>` are built in (no import), like `Option`. The iterator type `Map<T, U>` becomes `Mapped<T, U>`; a program naming it must rename it (the only incompatibility). A module's own `Map` or `Set` still means its own, as for `Ref`, `Task`, `Chan`.
69. **Values (Q2):** assignment and passing copy, copy-on-write as arrays; changes need `let mut` (E0501) or `mut self`; a last use moves. Sharing goes through `Ref<Map<K, V>>`.
70. **Keys (Q3):** `K` (and a set's `T`) must be `Eq` and contain no float: integers, `bool`, `char`, `string`, and structs, enums, tuples, arrays and `Option`s of those. A float anywhere in a key type is E0441 ("floats can't be keys: `NaN != NaN`"); a non-`Eq` key (`Ref`, functions, interfaces, actors) is E0419 as for `==`. Hashing is structural and matches `==`; there is no `Hash` interface.
71. **Order (Q4):** iteration, `keys()`, `values()`, printing and `iter()` follow insertion order; replacing a value keeps its key's place; removing then inserting moves the key to the end.
72. **Equality:** `==` on maps compares entries regardless of order (`#{"a": 1, "b": 2} == #{"b": 2, "a": 1}`); likewise sets. A map is `Eq` when `V` is.
73. **Map API (Q5):** `m.len`; `m[k]` (panics `no key "x" in the map` when missing, located like an index panic); `m[k] = v` inserts or replaces (place rules as for `xs[i] = v`); `m.get(k): Option<V>`; `m.has(k): bool`; `m.remove(k): Option<V>`; `m.clear()`; `m.keys(): K[]`; `m.values(): V[]`; `for (k, v) of m`; `m.iter(): Iter<(K, V)>`.
74. **Set API (Q5):** `s.len`; `s.has(x)`; `s.insert(x): bool` (true when new); `s.remove(x): bool`; `s.clear()`; `for x of s`; `s.iter(): Iter<T>`; `a.union(b)`, `a.intersect(b)`, `a.minus(b)` (new sets, in `a`'s order then `b`'s new ones).
75. **Collecting (Q5):** `it.collect_map()` on an `Iter<(K, V)>` and `it.collect_set()` on an `Iter<T>` (later duplicates: last value wins / ignored).
76. **Iterating while changing:** `for … of m` over a map or set may not change it (E0507, as for arrays); `for … of mut` is not offered for maps and sets (E0442: "iterate `m.keys()` and assign `m[k]`").
77. **Printing:** `#{"ann": 31, "bo": 40}`, `#{1, 2, 3}`, `#{}` — the literal form, strings quoted as inside arrays.
78. **Duplicate literal keys:** two equal literal keys in one `#{…}` is W0105; at run time the last wins.
79. **Implementation (Q6):** `src/prelude/maps.la`: a compact table — `entries` in insertion order (removed ones marked, compacted when half are), `slots: i64[]` open addressing with linear probing at load ≤ 2/3 — over arrays, so copy-on-write and the collector need nothing new. One built-in, `$hash(x): u64` (bundled sources only), lowered per key type to `lugha_hash_<type>` (FNV-1a over the same structure `lugha_eq_<type>` compares; strings by bytes). Literals, `m[k]`, `for … of`, printing and `==` are the checker's and codegen's sugar over prelude calls.
80. **Success:** milestone 101, tag `m101`; the §10 programs gain a word-count program (map) and a dedupe program (set).

## IMPLEMENTATION REQUIREMENTS

### Must Do (in this order, each step test-first)
1. Rename the prelude iterator type `Map` → `Mapped` (spec, prelude, `iter_helpers`, tests); full `programs` run.
2. `$hash` built-in: checker (bundled only), codegen `lugha_hash_<type>` for every `Eq` type without floats; unit and codegen tests (equal values hash equal; strings, tuples, nested structs, enums with payloads).
3. `src/prelude/maps.la`: `Map`/`Set` structs and methods per decisions 71–75, tested through Lugha programs.
4. Lexer `#{`; parser map/set literals (AST `MapLiteral`, `SetLiteral`), s-expressions, formatter (`#{"a": 1, "b": 2}`, broken one entry per line like arrays).
5. Checker: built-in names (decision 68), literal typing and E0412 for `#{}`, key rules (E0441, E0419), indexing and assignment places, `for … of` (E0507, E0442), printing, `==` (decision 72), W0105; members for completion.
6. Codegen: literals → prelude calls; `m[k]`, `m[k] = v`; iteration; printing `lugha_show_*`; equality through the prelude's `eq`.
7. Spec v17 §2 (`#`), §3 grammar, §4 types and keys, §5 statements, §9 printing, §10 programs, row 101, changes list; draft; error pages E0441, E0442, W0105; CLAUDE.md tree; VS Code grammar for `#{` (extension version bump).

### Must NOT Do
- A user-written `Hash` interface or custom hashers (decision 70).
- Shared (reference) maps (decision 69).
- Sorted maps, multimaps, or `for … of mut` over maps (out of scope).
- Changes to the C runtime: everything is Lugha and generated code.

### New dependencies
- none

## ERROR HANDLING REQUIREMENTS
- E0201 (existing): mixing map and set entries; a malformed `#{`.
- E0412 (existing): `#{}` with no expected type.
- E0419 (existing): a non-`Eq` key type.
- E0441 (new): a float in a key or set element type.
- E0442 (new): `for … of mut` over a map or set.
- E0501, E0507 (existing): changing a map without `mut`; changing it while iterating.
- W0105 (new): equal literal keys in one literal.
- Runtime panic: `m[k]` with a missing key.

## SECURITY CONSIDERATIONS
- None beyond the usual: no new runtime C, no I/O. Hashing is not cryptographic and is documented as such (no hash-flooding defence; a seed is a later decision).

## TESTS TO WRITE
- [ ] Rename: `programs` and prelude tests pass with `Mapped`; a program naming `Map` as the iterator type now reports E0305-style errors pointing at the rename (CHANGELOG).
- [ ] `$hash`: equal values hash equal across strings, tuples, structs, enums with payloads, arrays, `Option`; not offered outside bundled sources (E0301).
- [ ] Prelude maps: insert/replace/remove/has/get/len, growth past many entries, removal compaction, insertion order kept, replacing keeps place.
- [ ] Parser: map, set, empty, trailing comma, nested, mixing (E0201); formatter round-trips and breaks long literals.
- [ ] Checker: literal typing, `#{}` with and without expected type, E0441 (`Set<f64>`, `Map<(i64, f64), i64>`), E0419 (`Set<Ref<i64>>`), E0501, E0507, E0442, W0105, built-in names shadowed by a module's own `Map`.
- [ ] `tests/programs/m101/`: word count, dedupe, nested maps, `m[k]` panic (`.stderr`, exit 101), equality regardless of order, printing, set algebra, collect_map/collect_set, copies independent after change.
- [ ] Spec §10 programs (word count, dedupe) through `tests/spec.rs`.
- [ ] LSP: completion after `m.` lists the map's methods; hover on a literal shows `Map<string, i64>`.
- [ ] VS Code grammar test for `#{`.

## ROLLBACK PLAN
- Branch to return to: `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] The word-count and dedupe programs run; every decision above has a test.
- [ ] Targeted tests (`--lib`, `programs`, `spec`, `fmt`, `fmt_corpus`, `lsp_completion`, `lsp_hover`, `explain`, extension `npm test`), fmt, clippy pass; the full suite once at the end (codegen and prelude change broadly); CHANGELOG updated.

## VALIDATION
- `cargo test` (full, once at the end), `cd editors/vscode && npm test`, `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`
