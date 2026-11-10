## FEATURE: Hashing written in Lugha on derive blocks, part of `Eq`, with `derive.fold`, `derive.index`, `derive … for handle` and `derive.address`; milestone 143, v17.

**Status:** approved 2026-10-31 ("build"); proposed 2026-10-31, after discovery (4 questions, each answered with the recommended option): (1) order: hashing first, then `==`, printing last; (2) a new intrinsic `derive.fold(x, init, f)` threads a value through the fields, nothing allocated; (3) `derive.index(x)` gives an enum value's variant position; (4) hashing lives inside `Eq`: `K: Eq` brings the prelude's `$Hash`, so programs keep writing `K: Eq`. The user's standing guidance from this discovery: breaking changes are fine when they make the design better (none is needed here).

## OBJECTIVE
Nothing a program writes changes, and one bug goes away:
```lugha
fun main() {
    let mut sets: Set<Set<i64>> = #{};
    for n in 0..200 {
        let mut a: Set<i64> = #{};
        let mut b: Set<i64> = #{};
        for i in 0..n {
            a.insert(i);
            b.insert(n - 1 - i);        // the same numbers, the other way round
        }
        sets.insert(a);
        sets.insert(b);                 // equal to `a`: must not be added
    }
    println(sets.len);                  // 200 (today: 394, measured on main)
}
```
What changes is where hashing comes from: `codegen/hashing.rs` (218 lines) and `check/hashing.rs` (49) are replaced by Lugha in the prelude:
```lugha
// src/prelude/hash.la (sketch)
interface $Hash { fun $hash(self, h: u64): u64; }

derive $Hash for i64 { fun $hash(self, h: u64): u64 = $mix(h, self as u64); }
derive $Hash for string { fun $hash(self, h: u64): u64 = $hash.text(self, h); }
derive<T: $Hash> $Hash for T[] { fun $hash(self, h: u64): u64 { let mut s = h; for x of self { s = x.$hash(s); } s } }
derive $Hash for struct { fun $hash(self, h: u64): u64 = derive.fold(self, h, $part); }
derive $Hash for enum {
    fun $hash(self, h: u64): u64 = derive.fold(self, $mix(h, derive.index(self) as u64), $part);
}
derive $Hash for handle { fun $hash(self, h: u64): u64 = $mix(h, derive.address(self)); }
/** Equal maps hash equal whatever their order: entries combine by wrapping sum. */
derive<K: Eq, V: Eq> $Hash for Map<K, V> { … }

fun $part<F: $Hash>(h: u64, name: string, x: F): u64 = x.$hash(h);
/** FNV-1a's step: xor in, multiply by its prime. */
fun $mix(h: u64, bits: u64): u64 = (h ^ bits).wrapping_mul(1099511628211);
```

## CONTEXT
- Read first: PRP-172 and PRP-173 (derive blocks, their BUILT notes); `src/codegen/hashing.rs`, `src/check/hashing.rs`; `src/prelude/maps.la` (`$map_find`, `$set_of`, the `same` methods); `check/bounds.rs` (`eq_ok`), `check/derive*.rs`, `codegen/derive.rs`.
- Creates: `src/prelude/hash.la` (`$Hash`, its blocks, `$mix`, `$part`). Changes: `src/prelude/maps.la` (`k.$hash(OFFSET)` instead of `$hash.value(k)`); `src/check/derive.rs` (`Covers::Handle`), `derive_intrinsics.rs` (`fold`, `index`, `address`), `derive_meets.rs`; the bound machinery so a parameter bounded by `Eq` also has `$Hash`'s items (`check/satisfy.rs`'s `bound_entry`, `instances.rs`'s `bound_target`); `src/codegen/derive.rs` (the three intrinsics); `src/check/hashing.rs` and `codegen/hashing.rs` shrink to `$hash.text` (a string's bytes into the state, kept built in for speed) or go.
- Spec: §4 (derive blocks: `for handle`, `derive.fold`, `derive.index`, `derive.address`; maps hash what `==` compares, in any order), §5 "Intrinsics", §9 (hashing's lowering, symbols), §11 row 143, "Changes since v16". The v17 Draft gets the milestone before the build.
- Decisions: 208 and 209 (derive blocks), core 6 (no new symbol prefix: `lugha_fn_$prelude/$Hash@…`), 33/41 (checked once). None open.

## REQUIREMENTS

### Must Do
- **`$Hash`**, a derivable prelude interface (bundled-only name), `fun $hash(self, h: u64): u64` folding a value into FNV-1a state `h`. Blocks cover every `Eq` type: the integers, `f32`/`f64` (by their bits, as today), `bool`, `char`, `string`, `()`, arrays, `Option`, tuples, structs and enums (each variant's position mixed in before its payloads), handles, `Map` and `Set`.
- **`Eq` implies `$Hash`:** wherever a type parameter is bounded by `Eq` (or `Num`, which gives `==`), it also has `$Hash`'s items; every concrete `Eq` type meets `$Hash` through its block. Programs see nothing: they can't name `$Hash` or `$hash`, and `K: Eq` stays all a map needs.
- **Maps and sets hash what `==` compares:** their blocks combine each entry's (or element's) hash, from a fixed start, by wrapping sum, so equal maps and sets hash equal whatever order they were built in. This fixes `Set<Set<T>>` and maps keyed by maps or sets holding equal keys twice.
- **New intrinsics** (programs may use them too, section 4):
  - `derive.fold(x, init, f)` in a struct, tuple or enum block: `f(acc: A, name: string, field: F): A` once per field (an enum's: per payload of the variant at hand, named by the variant), threading `acc` from `init`; the result is the last `acc`. Nothing is allocated.
  - `derive.index(x)` in an enum block: the variant's position, an `i64`.
  - `derive … for handle`: a fourth kind, covering every `extern type` handle (and `CPtr`); in it `derive.address(x)` is the handle's address, a `u64`.
  - E0453 for each used wrongly, as for the other intrinsics.
- **`$hash.value` goes:** `maps.la` calls `k.$hash(OFFSET)` (FNV-1a's offset basis); `$hash.text(s, h)` stays a built-in for strings' bytes.
- **Performance:** a map-heavy benchmark (string, integer and struct keys; inserts and lookups) stays within 10% of `main` at `-O2`; recorded in BUILT.
- Milestone 143; tag `m143`.

### Must NOT Do
- Move `==` or printing (the next PRPs).
- Let programs name `$Hash` or override a type's hash: it must agree with `==`, which no program can change.
- Change hash values' visible effects: maps and sets iterate in insertion order, so none exist (the values themselves may change).

## BUILT — milestone 143
- As planned. `prelude/hash.la` (132 lines) is the prelude's seventh file, after `maps.la`, so no earlier prelude line (or panic location) moves. `check/hashing.rs` and `codegen/hashing.rs` keep only `$hash.text` (43 and 62 lines, from 49 and 218).
- `Eq` ⇒ `$Hash` is `derive::implies_hash` + `hash_bound`, consulted in `satisfy::bound_entry` and `instances::bound_target`; `Num` and `Float` count too, since they give `==`.
- `fold`'s `f` must return `init`'s type (E0453 otherwise); `fold` over an `Option` or enum switches on the tag like `variant`.
- Measured, `-O2`, 5 rounds of 100 000 inserts and lookups each into maps keyed by `i64`, `string`, a struct, and a set of enums, against `main`: 0.90 s vs 0.85 s on average (+5%), peak memory level (110–128 MB vs 118–123 MB). The bug reproduced on `main` with 200 sets inserted twice in opposite orders: 394 entries; now 200.
- Tests: `tests/programs/m143/` (`hash.la`: sets of sets, maps of maps, struct, tuple, recursive enum, every integer width, `char`, `Option`, array and `CPtr` keys, same at `-O2`; `fold.la`: a program's own interface over `fold`, `index` and a `for handle` block; `bad_fold.la`: E0453 three ways); unit tests for `Eq` ⇒ `$Hash` (and no hashing compiled without maps), a handle block, `fold`'s typing. E0453's page and help now list the new intrinsics (`m142/bad_fields.stderr` regenerated).

## DESIGN
- `derive.fold` lowers like `derive.fields`, unrolled per type, but threads one SSA value through the calls instead of storing results; for an enum it switches on the tag as `variant` does. `derive.index` is the tag; `derive.address` a `ptrtoint`.
- `Covers::Handle` matches `Type::Handle(_)`; its label is `handle`.
- The `Eq` ⇒ `$Hash` link is one place in the checker: a bound lookup (`bound_entry`) that finds no item consults `$Hash` when the parameter has `eq` (or `Num`), and `bound_target` resolves it through `$Hash`'s blocks.
- Sizes: `prelude/hash.la` ~120 lines; checker +120, codegen +150, −250 from the hashing files; all under 500.
- No new dependency, no `unsafe`, no runtime change.

## TESTS
- [ ] `tests/programs/m143/hash.la`: the OBJECTIVE; maps keyed by strings, integers of every width, `char`, tuples, nested structs, a recursive enum, `Option`, arrays, sets of sets and maps of maps in different orders, handles (a `CPtr`); lookups find what was inserted; same at `-O2`.
- [ ] `tests/programs/m143/`: programs using `derive.fold`, `derive.index` and a `for handle` block of their own; E0453 for `fold` outside a block, `index` in a struct block, `address` outside a handle block.
- [ ] Unit tests: `Eq` ⇒ `$Hash` through a bound; the new intrinsics' types; `Covers::Handle` matching.
- [ ] `tests/lazy_std.rs`/codegen: a program without maps compiles no `$Hash` instance.
- [ ] m100-and-later map/set programs unchanged (`--test programs`).

## ACCEPTANCE
- [ ] The OBJECTIVE prints `200`.
- [ ] `cargo test --lib`, `--test programs`, `--test lazy_std`, `--test explain`, `--test json`, `--test fmt_corpus`; `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`.
- [ ] Every `pub` item documented; DECISIONS.md, spec (§4, §5, §9, §11 row 143, "Changes since v16"), the v17 Draft, CHANGELOG (the set fix under Fixed), TODO and CLAUDE.md updated.
