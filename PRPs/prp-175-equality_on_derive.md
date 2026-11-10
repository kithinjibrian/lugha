## FEATURE: `==` written in Lugha on derive blocks: `Eq` a public prelude interface with `eq` and `hash`, a type's own pair overriding both, `derive.all`, E0451 for what isn't `Eq`; milestone 144, v17.

**Status:** approved 2026-10-31 ("build"); proposed 2026-10-31, after discovery (3 questions, each answered with the recommended option): (1) programs may define their own equality: `Eq` gets public items `eq` and `hash`, and a type writing one must write the other, so maps stay correct (this reverses the v15 rule that `==` is always structural); (2) a new intrinsic `derive.all(a, b, f)` compares two values field by field and stops at the first difference; (3) a type that isn't `Eq` is E0451 with the path to the part at fault, at `==` and at a `T: Eq` bound alike.

## OBJECTIVE
```lugha
struct User { id: i64, name: string, seen: i64 }

impl User {
    /** Users are the same user when their ids are. */
    fun eq(self, other: User): bool = self.id == other.id;
    fun hash(self, h: u64): u64 = self.id.hash(h);
}

struct Point { x: i64, y: i64 }

fun main() {
    let a = User { id: 7, name: "ada", seen: 1 };
    let b = User { id: 7, name: "Ada", seen: 9 };
    println(a == b);                                  // true: own `eq`
    let mut names: Map<User, string> = #{};
    names.insert(a, "first");
    names.insert(b, "second");                        // the same key
    println(names.len);                               // 1
    println(Point { x: 1, y: 2 } == Point { x: 1, y: 2 });   // true: structural, as always
    println([Some(1), None] != [Some(1), Some(2)]);          // true
}
```
And a type that isn't `Eq` says why:
```text
error[E0451]: `Job` doesn't meet `Eq`: `run` is `fun(i64): i64`, which no `derive Eq` block covers
```

## CONTEXT
- Read first: PRP-172–174 and their BUILT notes; `src/codegen/equality.rs` (309 lines: `lugha_eq_<type>` helpers), the `==` paths in `check/ops.rs`/`check/operators.rs` and `check/bounds.rs` (`eq_ok`), `check/maps.rs` (`EQUAL`, the `same` methods' instances), `src/prelude/hash.la` and `maps.la`; spec §4 "User-defined order" (`==` stays structural: this PRP changes that sentence).
- Changes: `src/prelude/hash.la` becomes `src/prelude/eq.la`: `public interface Eq { fun eq(self, other: Self): bool; fun hash(self, h: u64): u64; }` and its blocks (numbers, `bool`, `char`, `string`, `()`, arrays, `Option`, `Map`, `Set` — their `same` moves here — structs, enums, tuples, handles). `$Hash` and the `Eq` ⇒ `$Hash` link go: `Eq` has `hash` itself; `Num`/`Float` still give `==`, so they imply `Eq`. The checker's built-in `Eq` flag (`Bounds::eq`) becomes the interface, and `==`/`!=` on a type other than a number, `bool`, `char` or `string` is a call of `Eq.eq` (`!=` its negation). `check/derive*.rs` (`derive.all`; the own-pair rule), `codegen/derive.rs`, `codegen/equality.rs` (shrinks to the inline scalar and string comparisons, or goes), `check/maps.rs` (`EQUAL` and `same_instances` go).
- Spec: §4 (`Eq` as a prelude interface; who meets it; own `eq`/`hash`; `derive.all`; "User-defined order" no longer says `==` is always structural), §5 (`==`, `!=`), §9 (equality's lowering, E0451 at `==`, E0454), `Map`/`Set` (equality and hashing from `Eq`), §11 row 144, "Changes since v16". The v17 Draft before the build. DECISIONS: amend core decision 35 (deep equality) and record the reversal of v15's "`==` stays structural".
- Decisions: 208–210, 35 (amended), 6 (no new prefix: `lugha_fn_$prelude/Eq@…`; `lugha_eq_` goes from §8's table if unused). None open.

## REQUIREMENTS

### Must Do
- **`Eq`** is the prelude's derivable interface `Eq { fun eq(self, other: Self): bool; fun hash(self, h: u64): u64; }`, still a reserved name (E0302), still written `T: Eq`. It is met by exactly the types that are `Eq` today, through its blocks: numbers, `bool`, `char`, `string`, `()`, arrays, `Option`, tuples, structs and enums whose parts are `Eq`, handles, `Map` (keys and values `Eq`) and `Set`. Functions, interface values, `Ref`, `Task`, `Chan`, actor handles and structs holding a union are not.
- **`a == b`** on numbers, `bool`, `char` and `string` stays a built-in comparison (inline, as now). On any other type it is `a.eq(b)` through `Eq`, and `a != b` is `!a.eq(b)`. Through `T: Eq`, `a == b` is `a.eq(b)`.
- **A type's own equality:** a struct or enum whose `impl` has `fun eq(self, other: Self): bool` and `fun hash(self, h: u64): u64` (exactly these shapes) uses them for `==`, `!=`, maps and sets, wherever it appears (as a field of another type too). One of the pair without the other is **E0454** (new): `` `User` has its own `eq` but no `hash`: equal values must hash equal ``. A method named `eq` or `hash` of another shape is E0422 where the type must be `Eq`.
- **`hash`** is callable through `T: Eq` (`x.hash(h)`) and on the concrete types whose blocks or own methods give it, as `eq` is; the starting state for a whole value is FNV-1a's offset basis, `14695981039346656037`, which the prelude's maps use.
- **`derive.all(a, b, f)`** in struct, tuple and enum blocks: `true` if `f(name, x, y): bool` holds for every pair of corresponding fields (for an enum: `false` when the variants differ, else every payload pair), stopping at the first `false`; nothing allocated. E0453 when misused.
- **Not `Eq`:** `==`/`!=` on such a type, and a `T: Eq` bound given one, are **E0451** with the path ("`run` is `fun(i64): i64`"); E0404 stays for mismatched operand types (`1 == "a"`) and E0419 for the other built-in bounds.
- **Maps and sets:** `same` and `$hash` move into their `Eq` blocks; equality is order-independent as before, and hashing as since milestone 143.
- **Performance:** a struct-, enum- and array-heavy `==` benchmark and the milestone 143 map benchmark stay within 10% of `main` at `-O2`; recorded in BUILT.
- Milestone 144; tag `m144`.

### Must NOT Do
- Change ordering: `<` still uses the built-in order or `cmp`; `Ord` still doesn't imply `Eq`.
- Make interface values, functions or boxes `Eq`.
- Move printing (the next PRP).

## BUILT — milestone 144
- As planned, with these differences:
  - `Bounds::eq` stays: `T: Eq` remains the checker's flag, which `implies_eq` turns into the `Eq` interface (milestone 143's mechanism), instead of moving `Eq` into `Bounds::interfaces`. Same behaviour, far fewer touched files.
  - The prelude may declare `Eq` (`collect_interfaces` exempts it, as `collect_enums` already did); programs still get E0302.
  - Handles stay inline in codegen (address compare) though they have an `Eq` block (for `hash`, and `eq` through a bound).
  - `Option` has no block of its own: the `enum` block covers it (`Derives::find` maps `Type::Option` to `Covers::Enum`).
  - The codegen test helper `ir()` and `tests/codegen.rs`'s harness now check and lower with the prelude, as every program does (`==` on a struct needs it); the library's single-file `check::check` says so in its doc.
  - `codegen/derive.rs` reached 673 lines; with the user's approval its two-value and folding walkers (`eq_call`, `all`, `fold`, `index_or_address`, `address_of`, `fields_at`) moved to `codegen/derive_pairs.rs` (288; `derive.rs` 404).
- **Performance**, `-O2`, 27 million `==` on structs (string and array fields), enums and `Option`s: the first version took ~0.55 s against `main`'s 0.27 s. Three fixes, each found in the optimised IR: (1) the array block compared `self[i].eq(other[i])`, and the loop hoister treated the method call as a possible write, cloning a shared `self` on every comparison — it compares with `!=` now; (2) prelude and std instances are now `Internal`, so LLVM inlines them as it did the old helpers; (3) `derive.all` reads both values in place (`address_of`, `fields_at`) instead of copying them again, and `Option` goes through the `enum` block instead of nested `match`es. Final: best/median 0.25/0.26 s vs 0.26/0.27 s on `main`; the map benchmark 0.84/0.87 s vs 0.88/0.89 s. Optimised `main` is 556 instructions vs 573.
- Tests: `tests/programs/m144/` (`eq.la`: the OBJECTIVE, own `eq`/`hash` as a field and as map keys, structural `==` on every kind, maps and sets in different orders, handles, NaN, `hash` on values; `all.la`: a program's own `derive.all`; `not_eq.la`, `half_pair.la`, `bad_eq.la`), the same at `-O2`; unit tests for the pair rule, `Eq` through a bound, `derive.all`'s typing; codegen equality tests rewritten for `Eq` instances; E0404/E0419 → E0451 in three checker tests (`fun_values`, `iface_calls`, `refs`). No earlier `tests/programs` case changed.

## DESIGN
- **Breaking** (fine, per the user's standing guidance): a struct or enum that already has a method `eq` or `hash` now either takes over `==` (with the right shapes, both present), or is E0454 / E0422 where it must be `Eq`. Programs that wrote `x.hash(…)` meaning something else are affected the same way. Listed under **Breaking**.
- `Bounds::eq` is removed in favour of `Eq` in `Bounds::interfaces`; every `eq_ok`/`bounds.eq` reader moves to "meets `Eq`" (the derive satisfaction check), which gives E0451's path for free.
- `derive.all` lowers like `fold` with an early exit: one block per field, branching to `false` on the first failure; for enums it compares tags first.
- The own-pair rule is a check in `derive_meets.rs` (`own_item` for both items) plus E0454's builder and page.
- Sizes: `prelude/eq.la` ~190 lines (replacing `hash.la`), checker ±200 across the bound and operator files, codegen −250. Every file under 500.
- No new dependency, no `unsafe`, no runtime change.

## TESTS
- [ ] `tests/programs/m144/eq.la`: the OBJECTIVE; structural `==` on every `Eq` kind (nested structs, recursive enums, tuples, arrays, `Option`, maps and sets in different orders, handles); own `eq`/`hash` as a field of another struct and as map keys; `!=`; same output at `-O2`.
- [ ] `tests/programs/m144/`: `not_eq.la` (E0451 at `==` and at a bound, with paths), `half_pair.la` (E0454 both ways), `bad_eq.la` (E0422 for an `eq` of another shape), `all.la` (a program's own interface using `derive.all`).
- [ ] Every earlier `tests/programs` case unchanged except expected E0404/E0419 → E0451 where a type isn't `Eq` (each change listed in BUILT).
- [ ] Unit tests: `Eq` through a bound (`eq` and `hash`), the own-pair rule, `derive.all`'s typing, E0451 at `==`.
- [ ] `tests/explain.rs` for E0454; `tests/lsp_hover.rs` shows `Eq`'s items.

## ACCEPTANCE
- [ ] The OBJECTIVE prints `true`, `1`, `true`, `true`; the E0451 example reads as shown.
- [ ] `cargo test --lib`, `--test programs`, `--test spec`, `--test explain`, `--test json`, `--test lazy_std`, `--test fmt_corpus`, `--test lsp_hover`; `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`.
- [ ] Every `pub` item documented; DECISIONS.md (core 35 amended, a new entry), spec, v17 Draft, CHANGELOG (**Breaking**), TODO and CLAUDE.md updated.
