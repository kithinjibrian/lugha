# Lugha v15 — Draft (delta against v14)

**Status:** milestone 51 approved 2026-10-14 — session 105 (proposed in session 104), and built. Milestone 52 (operator overloading) approved in session 107 (proposed in session 106), and built. Milestone 53 (the std library and `std.math`) approved in session 111 (proposed in session 110), and built. Milestone 54 (`std.random`) approved in session 114 (proposed in session 113), with `uniform(): f64`, and built. Milestone 55 (copy-on-write arrays) approved in session 117 (proposed in session 116), and built. Milestone 51 is planned here. The rest of v15 is planned in `TODO.md` (directories, processes, environment variables, Unicode follow-ups); it gets milestones in this draft once its discovery resumes.
**Base:** `Language v14 Specification.md`, complete (milestone 50). It stays the source of truth, and what `lughac spec` prints, until v15's first milestone starts `Language v15 Specification.md`. That file then grows milestone by milestone, and v14 is frozen.

This draft lists only what changes, in the order of the v14 sections. Everything not mentioned keeps its v14 meaning.

**Compatibility.** Every valid v14 program and package stays valid:
- **`name` and `source` are optional.** A package without them behaves as before.
- **The upward lookup** only finds a manifest with `[package] name`, which no v14 manifest has.
- **A manifest path such as `".."`,** used from a main file given with a folder (`lughac run example/main.la`), now resolves; in v14 it failed with E0322 (a bug).

## Decisions behind this draft (discovery, session 104)

1. **Where it goes:** v15's first milestone (M51), ahead of the system-access milestones.
2. **`source` is explicit:**
   - `[package] source = "src"` makes `src/array.la` the package's module `array`, so users write `import glowworm.array;`;
   - without `source`, modules start at the package's folder, as in v14.
3. **`[package] name` is optional and checked:** a manifest listing the package under another key is an error (E0327). A short local name comes from an import alias (`import glowworm.array as ga;`, v14).
4. **Programs inside a package use it by name:**
   - a main file with no `lugha.toml` beside it looks upward for the nearest one;
   - if that manifest has `[package] name`, the program may import that package by its name, and the manifest's `[packages]` are its packages too;
   - so `glowworm/examples/basics.la` writes `import glowworm.array;` with no manifest of its own;
   - a `lugha.toml` beside the main file wins, as in v14.
5. **The file argument stays required:** `lughac run examples/basics.la`. A default entry point may come later.
6. **One file, one module:** a file reached under two module names (`cli.args` and `mytool.cli.args`) is an error (E0328).
7. **The lookup is bounded:**
   - it stops at the first folder holding a `lugha.toml`, a `.git` or that is `$HOME`;
   - so a manifest in a shared folder above a repository or a home folder, such as `/tmp`, is never used.

**Choices made while writing this draft** (each settles a gap the answers left open):
- **`name`:** a Lugha identifier, as manifest keys are (E0321 otherwise). The root program may declare one: when its main file sits in that folder, that program is the package itself, so `import mytool.x;` and `import x;` reach the same file (E0328 if both are used).
- **`source`:**
  - a relative path with `/`, with no `..`, not absolute, and not empty (E0321);
  - a missing folder is E0322;
  - it applies wherever the folder is used as a package: through a path entry, a git entry, or the upward lookup;
  - a program's own modules always resolve from its main file's folder.
- **The upward lookup:**
  - a folder holding a `.git` *and* a `lugha.toml` uses that manifest, so a repository's own root manifest is found;
  - the main file's own folder counts as the first step, which is the v14 rule;
  - a found manifest without `[package] name` gives no implicit package, but its `[packages]` still apply, as a manifest beside the main file would.
- **E0320 still applies:** a folder or `.la` file beside the main file named like the implicit package clashes, as for a listed one.
- **Manifest paths:**
  - paths are resolved against the manifest's folder as given, before `.`/`..` are folded;
  - a folder that folds to empty is the current folder;
  - so `example/..` is `.`, which fixes the v14 bug.

## Decisions behind milestone 52 (discovery, session 106)

The goal, from the user: this program runs.
```
struct Vec2 { x: f64, y: f64 }
impl Vec2 {
    fun add(self, o: Vec2): Vec2 = Vec2 { x: self.x + o.x, y: self.y + o.y };
    fun mul(self, o: Vec2): Vec2 = Vec2 { x: self.x * o.x, y: self.y * o.y };
}
fun main() {
    let a = Vec2 { x: 1.0, y: 2.0 };
    let b = Vec2 { x: 3.0, y: 4.0 };
    println(a + b);        // Vec2 { x: 4.0, y: 6.0 }
    println(a * b);        // Vec2 { x: 3.0, y: 8.0 }
}
```
8. **Operators:**
   - `+ - * / %` and unary `-` map to the methods `add`, `sub`, `mul`, `div`, `rem` and `neg`;
   - compound assignment follows (`a += b` is `a = a + b`);
   - `==`, `<`, the bit operators and indexing are not overloadable.
9. **Operand types:**
   - `a * b` is `a.mul(b)` with whatever parameter and result types `mul` declares, so `fun mul(self, k: f64): Vec2` gives `v * 2.0`;
   - one method per operator per type;
   - the left operand chooses, so `2.0 * v` is an error.
10. **Opt-in is structural:** a method with an operator's name and shape enables the operator:
    - a plain `self` receiver, not `mut self`;
    - exactly one parameter, or none for `neg`;
    - a result.

    No declaration is needed, as for interfaces. `fun add(mut self, x: i64)` (a set's `add`) never becomes `+`.
11. **Generic code uses operators through prelude bounds:**
    - the prelude declares `interface Add { fun add(self, other: Self): Self; }`, and likewise `Sub`, `Mul`, `Div`, `Rem` and `Neg` (`fun neg(self): Self;`);
    - `Num` is the bound for all of the arithmetic;
    - the numbers satisfy all of them as built-ins, `string` satisfies `Add`, and user types satisfy the single-operator ones structurally;
    - `fun add<T: Add>(a: T, b: T): T = a + b;` works for numbers, strings and `Vec2`.
12. **`impl` blocks take bounds, in place:**
    - written `impl NdArray<T: Num> { … }`, with the same bounds as a function's parameters;
    - every method in the block sees them;
    - a type may have several `impl` blocks that differ in bounds;
    - a bounded method used on an instance that misses its bound is E0419.
13. **`0` and `1` are `T` where a `T: Num` is expected** (`let mut s: T = 0;`); each instance makes them its own type. Other literals stay errors. `Num` is for the built-in numbers only.

**Choices made while writing milestone 52** (each settles a gap the answers left open):
- **Built-in operands keep their meaning:**
  - an operator between numbers, or `+` between strings, is the built-in one;
  - an operand of a struct or enum type looks for the method;
  - `string` has no `sub`, so `"a" - "b"` stays E0404.
- **Missing methods:**
  - `a + b` with no fitting `add` is E0404;
  - when a method by that name exists but has the wrong shape, the help names the shape needed;
  - an argument of the wrong type is E0403 at `b`, as in a call.
- **Through a bound:**
  - `T: Add` allows `a + b`, and also `a.add(b)`;
  - each instance calls the concrete type's method, or uses the built-in operator for a number or a string, so `add<i64>` compiles to a plain checked `add` with no call.
- **Built-ins under bounds:**
  - **What satisfies them:** an interface value satisfies the operator interfaces only through its own items. A number, or a `string` for `Add`, satisfies them only as built-ins: they still have no methods to call directly (`5.add(3)` is E0415).
  - **Overflow:** with numbers, operators keep their overflow and division-by-zero panics.
  - **Shadowing:** the prelude's `Add`, `Num` and the rest are visible everywhere unless a module declares the same name, like `Iter` (v6), so existing programs with a type named `Add` stay valid.
- **`Num` and literals:**
  - `Num` declares the five binary operators and `neg`;
  - `T: Num` also allows the literals `0` and `1` as `T`, `<` (every number is `Ord`) and `==`;
  - casts to and from `T` stay errors.
- **`impl` bounds:**
  - an `impl` without bounds keeps v14's meaning;
  - two methods with one name in two `impl` blocks of a type are E0310, whatever their bounds;
  - a bound on an `impl` of a non-generic type is E0315.

## Decisions behind milestone 53 (discovery, session 110)

14. **A standard library of bundled modules:**
    - `std.<module>` modules are written in Lugha (`src/std/`), embedded in `lughac` like the prelude, and loaded only when a file imports one, so they cost nothing otherwise;
    - **no opt-out** (session 109);
    - the prelude stays small, and new library features go in std modules.
15. **The first milestone is the machinery plus `std.math`.** `std.fs`, `std.env` and `std.process` follow as their own milestones; `std.process` needs a security review.
16. **`std` is reserved:**
    - a manifest entry named `std` is E0321, and a folder `std/` or file `std.la` beside the main file is E0320, so `import std.math;` always means the bundled module;
    - `import std.nope;` is E0312, with help listing the std modules;
    - otherwise std follows the normal rules: `public` items, `import std.math as m;`, `import std.math { sqrt };`.
17. **Constants are functions:** `math.pi()` and `math.e()`. Top-level constants are a later language feature.
18. **Float functions are generic over a new built-in `Float` bound:**
    - `Float` is satisfied by `f32` and `f64`, and implies everything `Num` allows;
    - `sqrt`, `pow`, `exp`, `ln`, `log10`, `sin`, `cos`, `tan`, `atan2`, `floor`, `ceil` and `round` are `<T: Float>(…): T`;
    - `abs` is `<T: Num>`, and `min`, `max` and `clamp` are `<T: Ord>`, written in Lugha.

**Choices made while writing milestone 53:**
- **The module's API:**
  ```
  public fun pi(): f64        public fun e(): f64
  public fun sqrt<T: Float>(x: T): T        public fun pow<T: Float>(x: T, y: T): T
  public fun exp<T: Float>(x: T): T         public fun ln<T: Float>(x: T): T
  public fun log10<T: Float>(x: T): T
  public fun sin<T: Float>(x: T): T         public fun cos<T: Float>(x: T): T
  public fun tan<T: Float>(x: T): T         public fun atan2<T: Float>(y: T, x: T): T
  public fun floor<T: Float>(x: T): T       public fun ceil<T: Float>(x: T): T
  public fun round<T: Float>(x: T): T       // halves away from zero
  public fun abs<T: Num>(x: T): T           // panics for the minimum integer, as `-x` does
  public fun min<T: Ord>(a: T, b: T): T     public fun max<T: Ord>(a: T, b: T): T
  public fun clamp<T: Ord>(x: T, lo: T, hi: T): T   // panics if lo > hi
  ```
- **The float functions are compiler built-ins** that `std.math` wraps. A std source may call the built-ins `$float.sqrt(x)` and so on. A name starting with `$` lexes only in bundled sources (the prelude and std), so a program can never name one. Each instance lowers to LLVM's float operation at its width (`llvm.sqrt.f64`, `llvm.sqrt.f32`), with no call into libm.
- **The results are IEEE's:** `sqrt(-1.0)` is `NaN` and `ln(0.0)` is `-inf`, printed as v7 prints them; no panics.
- **`Float`:**
  - it is in the prelude next to `Num`, visible unless a module declares the name;
  - a non-float argument is E0419 (help: "only `f32` and `f64` are `Float`");
  - `T: Float` allows `0` and `1` as `T`, like `Num`; other literals stay E0401, so a generic float body writes `let half: T = 1; half / (half + half)`, or takes values as parameters.
- **Where std modules come from:** the loader maps an import whose first segment is `std` to an embedded source, `src/std/<module>.la`, and gives the module path `std.<module>`, so symbols read `lugha_fn_std/math/sqrt<f64>`. Diagnostics inside std name the file `<std>/math.la`.

## Decisions behind milestone 54 (discovery, session 113)

19. **`std.random` uses xoshiro256\*\*,** seeded through splitmix64. It is not cryptographically secure, and the spec says so.
20. **Seeding:**
    - `random.seeded(seed: u64)` gives a repeatable stream;
    - `random.from_os()` seeds from the operating system's entropy (`getrandom`), through a new runtime function;
    - there is no global generator: a program makes an `Rng` and passes it around.
21. **The API**, methods of `Rng` (all `mut self`):
    - `next_u64()`, `uniform()`, `below(n)`, `between(lo, hi)`;
    - `normal(mean, std)`, `chance(p)`;
    - `choice(xs)`, an `Option` element;
    - `shuffle(xs)`, which returns a shuffled copy, because arrays are values.

**Choices made while writing milestone 54:**
- **`uniform(): f64`, not `uniform<T: Float>()`.** With no arguments, `T` couldn't be inferred, so `rng.uniform()` alone would be E0412. `f32` users write `rng.uniform() as f32`. `normal<T: Float>(mean: T, std: T): T` stays generic, since its arguments decide `T`.
- **Ranges:**
  - `below(n)` is in `0..n`, and `between(lo, hi)` in `lo..hi`, half-open like `for i in lo..hi`;
  - both are unbiased, by rejection;
  - `n <= 0` or `lo >= hi` panics.
- **`uniform()`** is in `[0, 1)`, with 53 random bits. `chance(p)` is `uniform() < p`. `normal` uses Box–Muller.
- **`choice(xs)`** is `None` for an empty array.
- **`shuffle(xs)`** is a Fisher–Yates shuffle, every order equally likely.
- **Built-ins:**
  - `$os.seed()` gives 64 bits of OS entropy. It calls the new runtime function `lugha_rt_os_seed` (`getrandom`, else `/dev/urandom`), which panics (`no randomness available from the operating system`) if both fail;
  - `$float.from_f64(x)` converts to the float type its context expects.

  Both can be named only in bundled sources.
- **Reproducibility:** for one seed, the sequence is the same on every platform and at `-O0` and `-O2`, and stays the same across compiler versions unless a spec change says otherwise.

## Decisions behind milestone 55 (discovery, session 116)

Measured in firefly's IR: the copies that cost are `let` bindings that only read, such as `let g = self.grads[i];` (18 deep-copy calls in `Graph.back` alone). Function arguments and receivers are already never copied (§4).

22. **Arrays become copy-on-write.**
    - A copy site no longer copies an array. It marks the array *shared* and shares it.
    - The first write to a shared array copies it first. The copy is unshared, so later writes are free.
    - The flag is sticky: an object once shared stays marked, so the other holder pays one more copy if it ever writes, and no reference counts are needed (Boehm keeps none).
    - Programs behave exactly as before: every value is still independent. This reverses §12's "cost of array copies" decision (eager deep copies), which assumed copy-on-write needed reference counts.
23. **Arrays passed to `extern` functions are unshared first** (copied if shared), so C code may write to them without breaking value semantics.
24. **Success:**
    - every existing test passes unchanged;
    - new tests cover writing to shared arrays (directly, nested, in structs, through `push`/`pop`/`swap`/`for … of mut`/`mut self`, to C);
    - firefly's and glowworm's timings are reported before and after. Speed is a goal, not a gate.

**Choices made while writing milestone 55:**
- **The flag is the top bit of the header's `cap` word,** so the header stays `{ i64 len; i64 cap }` (16 bytes) and capacity uses 62 bits. Everything that reads `cap` (`push`, `lugha_rt_array_grow`) masks it.
- **Sharing at a copy site:**
  - an array: mark it, keep the pointer;
  - a struct (or interface box, closure capture) holding arrays: copy the struct's bytes and mark each array reachable without passing through another array.
  
  Arrays inside arrays are marked when their outer array is cloned, since that is when a second pointer to them appears.
- **Cloning before a write** (`lugha_own_<type>`, one helper per array type, like the copy helpers): a shallow copy of the elements, with each array reachable from an element marked shared. The new array is unshared and full (`cap == len`).
- **Writes reach their array through its slot, after the value is computed.**
  - For `place = value` and `place op= value`, the place's index expressions run and are bounds-checked first, as §5 requires. Then the value is computed. Then, from the root outward, each array on the path is made unique (cloned and stored back if shared), and the element address is formed.
  - The same applies to `push`, `pop`, `clear`, `swap`, a `mut self` receiver's writes, and `for … of mut`.
  - So an array shared during the right-hand side (`xs[0] += { let c = xs; 1 }`) is never written through a stale pointer.

**Found while building milestone 55 (session 117):**
- **Decision 23 needs nothing:** §8 already keeps arrays from crossing the C boundary (only numbers, `bool` and strings do), so C never sees one.
- **No `lugha_own_<type>` helpers:** the uniqueness test is inline (load `cap`, test the sign bit, a cold branch to the clone); the clone reuses the `lugha_copy_<type>` helpers, which now copy one level and mark the arrays inside.
- **`swap` plans both places before committing either,** and **`for x of mut it` over an iterator re-reaches `it` before every `next`:** otherwise a copy made in between (an index of the second place, or the loop body) would share the array the first address points into.
- **Interface `mut self` calls reach their receiver through its place,** so the boxed value's arrays are owned like any other.
- **`for x of mut xs`:** `x` names the element place `xs[k]`, not a raw pointer. Reading `x` loads the current element, and writing `x` makes `xs` unique first. Behaviour is as §5 specifies.
- **Strings** are unchanged (immutable, shared). Enum payloads were already shared and never written.
- **IR changes for every program that copies or writes arrays,** so the byte-identical-IR regression check doesn't apply to this milestone. Program output must be identical.

## §2–§5 — no changes for milestone 51

## §6 Functions and program structure — changes

- **Packages:**
  - `[package]` gains `name = "ident"` and `source = "path"`, both optional and at most once (E0321);
  - E0327 when a listing's key differs from the declared name;
  - `source` moves the package's module root.
- **Loading:**
  - the upward lookup for the program's manifest (decisions 4 and 7), and the implicit package;
  - E0328 for a file loaded under two module names.

## §9 Compilation model — changes

- Module paths and symbols are unchanged: with `source = "src"`, `src/array.la` of package `glowworm` is module `glowworm.array` and its function `zeros` is `lugha_fn_glowworm/array/zeros`.

**New error codes.** The numbers are proposed, and become final when their milestone's PRP is approved.

| Code | Meaning | Examples |
| --- | --- | --- |
| E0327 | A package listed under another name than it declares | `gw = { path = "../glowworm" }` when glowworm's `[package]` says `name = "glowworm"` |
| E0328 | One file loaded as two modules | `src/main.la` importing both `cli.args` and `mytool.cli.args` |

## §10 Example programs — additions (v15 acceptance tests)

- **M55:** a program printing the results of copies followed by writes — through every write path — showing every copy independent (the same output as v14), and a timing note for firefly's GPT.
- **M54:** a seeded `Rng` printing a fixed sequence (the reproducibility test): `uniform`, `below`, `between`, `normal`, `chance`, `choice`, `shuffle`, and a `from_os()` generator whose values are only checked for range.
- **M53:** `import std.math;` with `math.sqrt(2.0)`, `math.pow`, trigonometry, `math.floor`/`ceil`/`round`, `math.abs(-3)`, `math.max(2, 7)`, `math.clamp`, `math.pi()`, an `f32` instance, and a generic `hypot<T: Float>`.
- **M52:** the Vec2 program above, plus a generic `add<T: Add>` over `i64`, `f64`, `string` and `Vec2`, `v * 2.0`, unary `-`, `+=`, and a generic `sum<T: Num>`.
- **M51:** a directory case: a package with `name` and `source = "src"`, and an `examples/` program with no manifest that imports it by name, run from the package's parent folder (`lughac run glowworm/examples/basics.la`).

## §11 Milestones — additions

| # | Milestone | Adds |
| --- | --- | --- |
| 51 | Package layout | `[package] name` (E0327), `source`, the upward lookup and implicit package, E0328, the `..` path fix |
| 52 | Operator overloading | `+ - * / %` and unary `-` through `add`/`sub`/`mul`/`div`/`rem`/`neg`; prelude `Add`…`Neg` and `Num` bounds; bounds on `impl` blocks; `0`/`1` as `T: Num` |
| 53 | The std library and `std.math` | bundled `std.<module>` modules, `std` reserved, the `Float` bound, `std.math` (float functions via LLVM, `abs`/`min`/`max`/`clamp`, `pi`/`e`) |
| 54 | `std.random` | xoshiro256\*\* `Rng`, `seeded`/`from_os`, `uniform`, `below`, `between`, `normal`, `chance`, `choice`, `shuffle` |
| 55 | Copy-on-write arrays | copy sites share and mark arrays; the first write to a shared array clones it; writes reach arrays through their slots after the value is computed; arrays unshared before `extern` calls |

Later v15 milestones (directories, processes, environment variables, Unicode follow-ups) are added when their discovery resumes; processes need a security review.

**Out of scope for v15** (for later): a default entry point (`lughac run` without a file), workspaces of several packages, `name` being required.

## §12 Open questions — additions

- [ ] **A default entry point** (v15). `[package] main = "src/main.la"` and `lughac run` with no file could come later.
- [ ] **Requiring `name`** (v15). A later version could require it once packages have adopted it.
