# Lugha v15 — Draft (delta against v14)

**Status:** milestone 51 approved 2026-10-14 — session 105 (proposed in session 104), and built. Milestone 52 (operator overloading) approved in session 107 (proposed in session 106), and built. Milestone 53 (the std library and `std.math`) approved in session 111 (proposed in session 110), and built. Milestone 54 (`std.random`) approved in session 114 (proposed in session 113), with `uniform(): f64`, and built. Milestone 55 (copy-on-write arrays) approved in session 117 (proposed in session 116), and built. Milestone 56 (owning arrays once per loop) approved in session 119 (proposed in session 118), and built. Milestone 57 (nested `mut self` calls) proposed in session 120 and built in session 122, and milestones 58 (numbers as `T`, built in session 123) and 59 (user-defined `Ord`, built in session 124) in session 121; all three approved in session 122. Milestone 60 (lazy std modules) approved in session 126 (proposed in session 125), and built. Milestones 61–66 (the rest of the v15 TODO) proposed in sessions 127–128 and approved in session 129 (PRP-095's approval is the user's security review); 61 built in session 129, 62 in session 130, 63 in session 131, 64 in session 132, 65 in session 133, 66 in session 134. Milestone 51 is planned here. The rest of v15 is planned in `TODO.md` (directories, processes, environment variables, Unicode follow-ups); it gets milestones in this draft once its discovery resumes.
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

## Decisions behind milestone 56 (discovery, session 118)

Measured after milestone 55: a write tests the shared flag of every array on its path, so loops that only write slow down (DECISION-010's histogram 140 → 217 ms, matmul 143 → 188 ms, where the test also stops vectorization).

25. **The compiler hoists ownership out of loops.** Before a loop, codegen makes unique each array the body writes through, when the body can't share that array again; inside the loop, those writes skip the flag test. No language change: every program prints what it does today.
26. **"Can't share" is a strict syntactic whitelist, per root variable.** In the loop (its condition, header and body, nested loops included), the root may appear only:
    - as the root of a written place: `=`, `op=`, `push`/`pop`/`clear` (assigning an array-valued place, such as `xs = ys` or `g[i] = row`, is not on the list);
    - in reads whose value holds no array: numbers, `bool`, `char`, strings, `.len`, and structs, enums and tuples without arrays.

    Anything else turns hoisting off for that root in that loop: a `let`/argument/return/capture of the root or an array inside it, a method call on it (`self` or `mut self`), `swap`, `for … of` over it, or a `for … of mut` alias of it used other than as above.
27. **Every loop kind, every nesting level:** `for … in`, `for … of`, `for … of mut`, iterator loops, `while`, `while let`. Each loop owns the paths fixed during it.
    - A path is fixed when each of its indexes is a literal or a variable the loop never assigns, with no calls; so in matmul the `i` loop owns `out` and the `j` loop owns `out[i]`.
    - A hoisted index out of bounds skips that hoist instead of panicking; the write's own bounds check still reports it, at the same place as today.
28. **Success:** milestone 56, tag `m56`. Every existing test unchanged, IR tests (no flag test in a hoisted loop; the test kept when the body copies), hazard programs; timings reported, not gated.

**Found while building milestone 56 (session 119):**
- **A method on a value without arrays** (`xs[i].abs()`, a `mut self` call on an array-free struct element) is a read or a write of that value, not a disqualifying call: it can't share an array.
- **`push` of a value holding arrays,** like assigning one, disqualifies its root: the new element may be shared.
- **A `for … of mut` variable stands for its array's root,** both when mentioned in the loop and when it is a hoisted path's root; one from an enclosing loop is never a fixed index.
- **Paths match by the root variable's slot,** so code from another function lowered mid-loop never matches.

## Decisions behind milestone 57 (discovery, session 120)

Found writing firefly: tape-style model code needs one `let` per op, because `g.tanh(g.add(a, b))` is E0508 (an argument of a `mut self` call mentions its receiver).

29. **E0508 applies only to arguments that can be the receiver's storage:** a place rooted at the receiver's root variable (also through `if`, block and `match` tails) whose type is passed by pointer (a struct, enum, `Option`, array, tuple or interface value). Anything else may mention the receiver: calls (`g.f(g.h(x))`), operators, literals, and reads of numbers, `bool`, `char` and strings (`g.scale(x, g.lr)`). `v.add(v)`, `v.add(v.inner)`, `g.set(g.values[0])` and `v.add(if c { v } else { w })` stay E0508.
30. **A `mut self` call runs like an assignment** (§5, milestone 55): the receiver place's index expressions first, bounds-checked; then the arguments, left to right; then the receiver's place is reached (its arrays made unique) and the method runs. Inner calls on the same receiver finish before the outer one starts, and it sees their effects.
31. **E0510 for a `mut self` receiver only when an index lies between:** a change made by the arguments to something that strictly contains the receiver is allowed when no array index lies on the receiver's path below the changed place (`self.g.f(self.norm(x))`, `a.b.f(a.reset())`); `gs[i].f(gs.clear())` and `self.layers[i].f(self.reset())` stay E0510, since the array could shrink after the bounds check. Assignment and `swap` keep their E0510.
32. **Success:** milestone 57, tag `m57`; checker tests for every allowed and still-rejected case, programs whose output equals the one-`let`-per-op form, a §10 program; then firefly's `gpt.la` `layer` rewritten in the nested style, with the same losses (committed, not pushed).

## Decisions behind milestone 58 (discovery, session 121)

Found writing glowworm and firefly: only `0` and `1` can be a `T: Num`, so generic code counts up by `1` to make a number (`arange`, `linspace`, `mean`).

33. **`as` casts with type parameters:** `e as T` and `x as U`, where `T` or `x`'s type is a type parameter bounded by `Num` or `Float`, are valid when the other side is numeric (or another such parameter). Each instance casts by the §4 rules for its types (truncating and saturating silently, as every `as` does). A type parameter without a numeric bound stays E0408.
34. **Literals as `T`:** any integer literal may take a type `T: Num` (or `T: Float`), and any float literal a type `T: Float`, as `0` and `1` already do. A float literal where `T` is only `Num` is E0401.
35. **Per-instance range check:** an integer literal that doesn't fit an instance's type (`300` with `T = u8`; `-1` with `T = u32`) is E0402 at the literal, naming the instance (`in big<u8>`). The checker reports it, from its instance list.
36. **Success:** milestone 58, tag `m58`; checker tests, a program at several instances, a §10 program; glowworm's `arange`/`linspace`/`mean` and firefly's `mean` simplified with the same output (committed, not pushed).

## Decisions behind milestone 59 (discovery, session 121)

37. **A type with `fun cmp(self, other: Self): Ordering` is `Ord`:** a prelude `enum Ordering { Less, Equal, Greater }` (written `Ordering.Less`, like any enum). `<`, `<=`, `>` and `>=` on such a type call `cmp`, and it satisfies `T: Ord`. `==` stays structural; `Ord` still doesn't imply `Eq`. The numbers, `string` and `char` keep their built-in comparisons and get no `cmp` method (as with `add`, milestone 52). A `cmp` of another shape doesn't make a type `Ord` (E0419 with help naming the shape; E0404 for `<`).
38. **Tuples compare through `cmp`:** a tuple is `Ord` when every element is, and compares element by element, calling `cmp` for user types.
39. **`xs.sort()`:** a prelude helper for `T: Ord` arrays, a stable sort returning a new array, next to `sort_by`. A module's own `Ordering` still means its own, as for `Iter`.
40. **Success:** milestone 59, tag `m59`; checker tests (shapes, interface values still not `Ord`), programs (`<` on structs, `math.min`/`max`/`clamp`, `it.min()`, tuples, `sort()`), a §10 program. No library changes.

## Decisions behind milestone 60 (discovery, session 125)

Found in session 114: importing a std module compiles every non-generic function and method in it, so a program using `random.seeded` and `r.below` also gets `from_os`, `uniform`, `between`, `chance`, `math.pi` and `math.e` in its IR. The prelude compiles only what is reached.

41. **Std compiles like the prelude:** every body of an imported std module is still checked, and only the functions and methods a program reaches are compiled. Reached means called directly, through a bound or an interface table, used as a function value, or reached from another reached body.
42. **Std only:** a program's own modules and its packages still compile everything.
43. **Success:** milestone 60, tag `m60`, with no language change. IR tests (unused std functions absent; std reached through generics, interface values and function values present), every program's output unchanged, and the IR size of the std programs before and after.

## Decisions behind milestones 61–66 (discovery, session 127)

The user asked to finish the v15 TODO in one approval. Each item stays its own PRP and milestone (the scope rule), built back to back in this order.

44. **Milestone 61, one `iter_sum`:** the ten `iter_sum_<type>` prelude copies and the checker's per-type table become one `fun iter_sum<T: Num>(it: Iter<T>): T`. `["a"].iter().sum()` stays E0415; only IR names change (`iter_sum<i64>`). No language change.
45. **Milestone 62, `parse_i64` and `parse_u64` in Lugha:** prelude functions with the same results (an optional `-` for `i64`, digits only, `None` on overflow or anything else). `lugha_rt_parse_i64` and `lugha_rt_parse_u64` are removed.
46. **Milestone 62, one UTF-8 decoder:** a prelude `utf8_step` decides how many bytes the scalar at a position takes (0 if invalid). `s.chars()` and `string_from_utf8` both use it, so they can't disagree. A bundled-only built-in `$string.from_bytes` turns checked bytes into a string, and `lugha_rt_utf8_check` is removed. Same results and messages.
47. **Milestone 63, widths in characters:** a format width (`{name:<8}`, `{x:^9}`) counts Unicode scalar values, not bytes, and a fill may be any `char`. Classification and case stay Latin-1: the user ruled fuller tables out of v15, keeping §11's "full Unicode tables" exclusion.
48. **Milestone 64, `std.env`:** `env.get(name): Option<string>` (`None` when unset or not UTF-8) and `env.current_dir(): Result<string, string>`; read-only, with no setting and no changing directory.
49. **Milestone 65, `std.fs`:**
    - `list_dir(path): Result<string[], string>` gives names sorted by bytes, without `.` and `..`;
    - `make_dir`, `make_dirs` (with parents), `remove_file` and `remove_dir` (empty only) return `Result<i64, string>` with `Ok(0)`;
    - `exists`, `is_dir` and `is_file` return `bool`.

    No recursive delete. Errors carry the system's message, as `read_file`'s do.
50. **Milestone 66, `std.process` API:**
    - `run(cmd, args): Result<Output, string>` captures, with `Output { code: i32, stdout: string, stderr: string }`;
    - `run_with(cmd, args, input)` also writes `input` to the child's stdin;
    - `call(cmd, args): Result<i32, string>` lets output through to the terminal.

    `Err` only when the program can't start; any exit code is `Ok`.
51. **Milestone 66, how programs start** (the user's security choices):
    - **No shell:** `posix_spawnp` with the argument list (no splitting, globbing or quoting); a `cmd` without `/` is found on `PATH`, one with `/` is a path; a NUL in `cmd` or an argument is an `Err`.
    - **Inherited state:** the child inherits the environment unchanged; the runtime's own descriptors are close-on-exec, so it gets only 0, 1 and 2.
    - **Results:** stdout and stderr are drained together (no pipe deadlock); output that isn't valid UTF-8 is an `Err`; death by signal `n` is code `128 + n`.
    - **Limits:** no timeout and no output limit in v15.

    The user's approval of PRP-095 is the security review.
52. **Success, each milestone:** tests first, every earlier program unchanged, a tag (`m61` … `m66`); new std modules get §10 programs, while 61–63 change no language (63 changes only padding of non-ASCII text).

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

- **M64–M66:** a §10 program each for `std.env`, `std.fs` (in a scratch folder) and `std.process` (running `echo` and `sort` found on `PATH`, with captured output, stdin and exit codes).
- **M61–M63:** no new §10 program; M63 updates the format program's expectations only if it pads non-ASCII text.
- **M60:** no new §10 program (no language change).
- **M59:** a leaderboard: a struct with `cmp`, sorted with `sort()`, its best with `it.min()`, ties broken through a tuple.
- **M58:** a generic `mean`, `arange` and `scale` at `i64`, `f32` and `f64`, using `as T` and literals as `T`.
- **M57:** a tape-style program with nested calls on one receiver (`g.mul(g.add(a, b), g.add(a, b))`), printing the same as the one-`let`-per-op form.
- **M56:** no new §10 program (no language change); `tests/programs/m56/` hazards whose output equals v14's.
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
| 56 | Owning arrays once per loop | arrays a loop writes through, and can't share, are made unique before the loop; its writes skip the flag test |
| 57 | Nested `mut self` calls | E0508 only for arguments that can be the receiver's storage; `mut self` calls run their arguments before reaching the receiver; E0510 for receivers only across an index |
| 58 | Numbers as `T` | `as` casts with `T: Num`/`Float`; integer literals as `T: Num`, float literals as `T: Float`; E0402 per instance |
| 59 | User-defined `Ord` | `cmp(self, other: Self): Ordering`, the prelude `Ordering`, `<` through `cmp`, tuples, `xs.sort()` |
| 60 | Lazy std modules | an imported std module's functions and methods compiled only when reached, as the prelude's |
| 61 | One `iter_sum` | `iter_sum<T: Num>` replaces the per-type copies |
| 62 | C → Lugha | `parse_i64`/`parse_u64` and one UTF-8 decoder in the prelude; `$string.from_bytes` |
| 63 | Widths in characters | format widths count chars; any `char` fill |
| 64 | `std.env` | `get`, `current_dir` |
| 65 | `std.fs` | `list_dir`, `make_dir(s)`, `remove_file`, `remove_dir`, `exists`, `is_dir`, `is_file` |
| 66 | `std.process` | `run`, `run_with`, `call`; no shell, `PATH` lookup, inherited environment |

Later v15 milestones (directories, processes, environment variables, Unicode follow-ups) are added when their discovery resumes; processes need a security review.

**Out of scope for v15** (for later): a default entry point (`lughac run` without a file), workspaces of several packages, `name` being required.

## §12 Open questions — additions

- [ ] **A default entry point** (v15). `[package] main = "src/main.la"` and `lughac run` with no file could come later.
- [ ] **Requiring `name`** (v15). A later version could require it once packages have adopted it.
