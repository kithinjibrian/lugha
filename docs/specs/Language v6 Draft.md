# Lugha v6 — Draft (delta against v5)

**Status:** approved 2026-10-13 — session 63
**Base:** `Language v5 Specification.md`, complete (milestones 20–22). It stays the source of truth, and what `lughac spec` prints, until v6's first milestone starts `Language v6 Specification.md`. That file then grows milestone by milestone, and v5 is frozen, as earlier versions were.

This draft lists only what changes, in the order of the v5 sections. Everything not mentioned keeps its v5 meaning. Every valid v5 program stays valid in v6, because everything v6 adds was an error before:
- `interface I<T>` and `T: I<i64>` were syntax errors;
- a written `Self` was E0305;
- converting one interface value to another was E0403 (E0424 through `?`);
- `for … of` on anything but an array was E0411.

A module that declares its own `Iter` keeps using it (§4).

## Decisions behind this draft (discovery, session 63)

1. **Headline: generic interfaces and `Self`.** This finishes the interface work of v5. Bitwise operators, more integer types, packages and the `match` additions wait (§11).
2. **An interface whose signatures use `Self` is a bound only.** `fun less(self, other: Self): bool` is fine through `T: Ordered`. Using such an interface as a value type is a new error, E0427: two `Ordered` values could hold different types, so `a.less(b)` couldn't be dispatched safely. A `Self` return (`fun copy(self): Self`) follows the same rule.
3. **Generic interfaces work wherever plain ones do,** as bounds and as value types (`Source<i64>[]`). In `fun collect<T, S: Source<T>>(s: S): T[]`, the `T` is inferred from the concrete type's methods. Satisfaction is structural and a type has one method per name, so a type satisfies `Source<X>` for at most one `X`.
4. **One interface value converts to another** when the source interface declares every item of the target the same way. This is checked at compile time and never fails at run time. A type pattern still sees the concrete type inside. `?` converts interface errors the same way.
5. **Iterators:** the prelude declares `interface Iter<T> { fun next(mut self): Option<T>; }`, and `for x of e` loops over any `e` that satisfies it.

**Choices made while writing this draft** (each settles a gap the answers left open):
- **`for … of` iterates a copy** of the iterator, moved on last use, so the loop never changes `e` itself (as for every value in Lugha).
- **A module's own `Iter` hides the prelude's** in that module. `for … of` goes by structure, so it works either way.
- **An interface's type parameters may have bounds** (`interface Sorted<T: Ord>`), checked at each use (E0419), as for structs.
- **Inside an interface, `Self` always means the implementing type,** even in a module that declares a type named `Self`.
- **Conversion between interfaces needs every item, defaulted ones included.** Each method then behaves the same after the conversion as before it.

Later (the out-of-scope list in §11): `Self` outside interfaces, and default methods on concrete types (v5 §12).

## §2 Lexical structure — changes

None. `Self` is an identifier, not a keyword; §4 gives it its meaning inside an interface.

## §3 Grammar — changes

```ebnf
interface_decl = "interface" IDENT [ type_params ] "{" { iface_item } "}" ;
bound          = "Eq" | "Ord" | IDENT [ "." IDENT ] [ "<" type { "," type } [ "," ] ">" ] ;
```

- **Generic interfaces:** `interface Source<T> { … }`, with type parameters exactly as on a struct.
- **Bounds with arguments:** `S: Source<T>`, `S: io.Source<string>`.
- **Types:** `Source<i64>` and `Self` are written with the existing `base_type` rule.

## §4 Type system — changes

**Generic interfaces.**
- **Declaring:** `interface Source<T> { fun next(mut self): Option<T>; }`.
  - The interface's type parameters can appear in its items' signatures and default bodies.
  - Items still have no type parameters of their own.
  - A parameter's bounds are checked wherever the interface is used with arguments (E0419).
- **Writing one:** an interface with type parameters is always written with its arguments: `Source<i64>`, `T: Source<string>`. A bare `Source`, or the wrong number of arguments, is E0418.
- **Satisfying one:** a type satisfies `Source<X>` if it satisfies the interface with `X` substituted (v5's rule).
- **As a bound:** `fun collect<T, S: Source<T>>(src: S): T[]`. A bound's arguments may mention the function's other type parameters.
- **Inference:** a type parameter that appears only inside a bound's arguments (the `T` above) is read off the concrete type's methods. `collect(Count { … })` gives `S = Count`, then `T = i64` from `Count.next`. When no argument fixes the parameter and no method does either, it is E0412.
- **As a value type:** `let s: Source<i64> = Count { n: 0, end: 3 };`, following v5's interface-value rules. Its table is `lugha_vt_Count.Source<i64>` (§9).

**`Self`.**
- **Where:** in an interface's signatures and default bodies, `Self` is the type that satisfies the interface. It can be written anywhere a type can: `other: Self`, `: Self`, `Self[]`, `Option<Self>`.
- **Outside an interface,** `Self` is still an unknown type (E0305).
- **Satisfying:** a type `C` satisfies an item that mentions `Self` when its method has `C` in those places. For example, `Money` satisfies `fun less(self, other: Self): bool` with `fun less(self, other: Money): bool`. For a generic type, `Self` is `Stack<T>` with its arguments.
- **Through a bound:** with `T: Ordered`, `a.less(b)` needs `b: T`, and `max` returns a `T`. An associated function returning `Self` is called as `T.zero()` (v2's `T.make(…)` rule).
- **In a default body:** `Self` can now be written (v5 made it E0305). It names the hidden type parameter: `fun max(self, other: Self): Self = if self.less(other) { other } else { self };`.
- **Not a value type:** an interface any of whose items mention `Self` outside the receiver is a bound only. Using it as a type is **E0427**, naming the item. This sits alongside v5's E0425 for associated functions.
- **An interface value as a type argument** satisfies an interface that uses `Self` when its own items match with `Self` replaced by that interface. This is v5's rule, unchanged.

**Converting between interfaces** (amends v5's "Not between interfaces").
- **When:** where an interface `I` is expected, a value of interface type `J` converts when `J` declares every item of `I` (defaulted ones included) with the same name, receiver and signature, after substituting type arguments. Otherwise it is E0422, naming what `J` lacks. Being able to show that every concrete type inside a `J` satisfies `I` isn't enough: `J` must declare the items itself.
- **What it keeps:**
  - the value inside, still the same concrete type, so a type pattern on the `I` still sees `Dog`;
  - each method's behaviour: a default `J` uses stays the method `I` calls.
- **Not inside other types,** as in v5: a `Pet[]` is not a `Named[]` (E0403).
- **`?`** (amends v5): an error of interface type `J` converts to an interface `F` under the same rule.

**Iterators.**
- **The prelude** declares `interface Iter<T> { fun next(mut self): Option<T>; }`.
  - `Iter` is visible in every module, like `Result`.
  - A module that declares its own `Iter` uses its own (so v5 programs stay valid).
- **`for x of e`**, where `e` is not an array:
  - **When it applies:** `e`'s type satisfies the prelude's `Iter<T>` for some `T`, or `e` is an `Iter<T>` value.
  - **How it runs:** `e` is evaluated once into a hidden mutable copy (moved on last use). The loop then calls `next` on that copy and runs the body with `x` bound to each `Some` payload, until `next` returns `None`.
  - **`x`** is an immutable binding, as for arrays.
  - **`break` and `continue`** work as usual.
  - **`e` itself is never changed,** so looping twice over the same variable starts from the beginning both times.
  - **Anything else** is still E0411 (not iterable).
- **Arrays keep their own loop** (v0). They have no `next`, so they never satisfy `Iter`.

## §5 Expressions and statements — changes

- **`for … of`** gains the iterator form (§4). It desugars to:

```
{
    let mut $it = e;
    while true {
        match $it.next() {
            Some(x) => { body }
            None => { break; }
        }
    }
}
```

## §7 Memory model — changes

None. Generic interface values are the same two words. Converting between interfaces keeps the box (copied as v5's conversions are) and changes only the table.

## §9 Compilation model — changes

- **Symbols:**
  - **Generic interfaces** print with their arguments: `lugha_vt_Count.Source<i64>`, `lugha_copy_…` mangled as for generic structs.
  - **A default method of a generic interface** compiles per (type, interface arguments): `lugha_fn_Source<i64>.skip<Count>`.
- **Converting `J` to `I`:**
  - **The table it uses:** for each concrete type `C` converted to `J` anywhere in the program, the compiler emits a table for `I` whose method slots are the functions `C`'s `J` table uses for those items. Its identity and copy slots are the same as in `C`'s `J` table.
  - **How a conversion finds it:** each table for `J` gains one slot per interface that `J` values are converted to, pointing at that table. A conversion loads that slot and pairs it with a copy of the box, so it costs one load at run time.
  - **Chains** (`J` to `I` to `K`) close over these slots.
  - **No extra symbols** when nothing is converted between interfaces.
- **Iterator loops** lower as the desugaring in §5: a direct call when `e`'s type is concrete, and a dynamic call when `e` is an `Iter<T>` value.
- **The runtime** needs nothing new.

**New error code.** The number is proposed, and becomes final when its milestone's PRP is approved.

| Code | Error | Example |
| --- | --- | --- |
| E0427 | An interface that uses `Self` used as a type | `let xs: Ordered[] = …;` where `Ordered` declares `fun less(self, other: Self): bool;` |

**Reused codes:**
- **E0305:** `Self` outside an interface.
- **E0412:** a type argument that neither the arguments nor the methods fix.
- **E0418:** `Source` without its arguments.
- **E0419:** an interface argument that breaks the parameter's bound.
- **E0422:** a conversion between interfaces whose source lacks an item.
- **E0411:** `for … of` on something that is neither an array nor an iterator.
- **E0424:** `?` whose error neither matches nor converts.

## §10 Example programs — additions (v6 acceptance tests)

**Generic interfaces** (milestone 23). Prints `3` then `a-b` then `11` on three lines, exits with 0.

```
interface Source<T> {
    fun next(mut self): Option<T>;
}

struct Count { n: i64, end: i64 }

impl Count {
    fun next(mut self): Option<i64> {
        if self.n == self.end {
            return None;
        }
        self.n += 1;
        Some(self.n)
    }
}

struct Words { xs: string[], at: i64 }

impl Words {
    fun next(mut self): Option<string> {
        if self.at == self.xs.len {
            return None;
        }
        self.at += 1;
        Some(self.xs[self.at - 1])
    }
}

fun collect<T, S: Source<T>>(src: S): T[] {
    let mut s = src;
    let mut out: T[] = [];
    let mut more = true;
    while more {
        match s.next() {
            Some(x) => { out.push(x); }
            None => { more = false; }
        }
    }
    out
}

fun main() {
    println(collect(Count { n: 0, end: 3 }).len);
    println(collect(Words { xs: ["a", "b"], at: 0 }).join("-"));
    let mut s: Source<i64> = Count { n: 10, end: 12 };
    println(s.next().unwrap_or(0));
}
```

**Iterators** (milestone 24). Prints `1`, `2`, `3`, `0`, `88` on five lines, exits with 0.

```
struct Count { n: i64, end: i64 }

impl Count {
    fun next(mut self): Option<i64> {
        if self.n == self.end {
            return None;
        }
        self.n += 1;
        Some(self.n)
    }
}

struct Fib { a: i64, b: i64, left: i64 }

impl Fib {
    fun next(mut self): Option<i64> {
        if self.left == 0 {
            return None;
        }
        self.left -= 1;
        let a = self.a;
        self.a = self.b;
        self.b += a;
        Some(a)
    }
}

fun total(it: Iter<i64>): i64 {
    let mut sum = 0;
    for n of it {
        sum += n;
    }
    sum
}

fun main() {
    let c = Count { n: 0, end: 3 };
    for n of c {
        println(n);
    }
    println(c.n);
    println(total(Fib { a: 0, b: 1, left: 10 }));
}
```

**`Self`** (milestone 25). Prints `1999` then `2.0` on two lines, exits with 0.

```
interface Ordered {
    fun less(self, other: Self): bool;
    fun max(self, other: Self): Self = if self.less(other) { other } else { self };
}

struct Money { cents: i64 }

impl Money {
    fun less(self, other: Money): bool = self.cents < other.cents;
}

struct Version { major: i64, minor: i64 }

impl Version {
    fun less(self, other: Version): bool =
        self.major < other.major || (self.major == other.major && self.minor < other.minor);
}

fun largest<T: Ordered>(xs: T[]): T {
    let mut best = xs[0];
    for x of xs {
        best = best.max(x);
    }
    best
}

fun main() {
    println(largest([Money { cents: 250 }, Money { cents: 1999 }, Money { cents: 5 }]).cents);
    let v = largest([Version { major: 1, minor: 9 }, Version { major: 2, minor: 0 }, Version { major: 1, minor: 10 }]);
    println("{v.major}.{v.minor}");
}
```

**Converting between interfaces** (milestone 26). Prints `good dog, rex` then `woof` then `hello, tom` then `meow` on four lines, exits with 0.

```
interface Named {
    fun name(self): string;
}

interface Pet {
    fun name(self): string;
    fun sound(self): string;
}

struct Dog { called: string }

impl Dog {
    fun name(self): string = self.called;
    fun sound(self): string = "woof";
}

struct Cat { called: string }

impl Cat {
    fun name(self): string = self.called;
    fun sound(self): string = "meow";
}

fun greet(n: Named): string = match n {
    d: Dog => "good dog, {d.called}",
    _ => "hello, {n.name()}",
};

fun main() {
    let pets: Pet[] = [Dog { called: "rex" }, Cat { called: "tom" }];
    for p of pets {
        println(greet(p));
        println(p.sound());
    }
}
```

## §11 Milestones — additions

| # | Milestone | Adds | Done when |
| --- | --- | --- | --- |
| 23 | Generic interfaces | `interface I<T>`, bounds with arguments, inference through bounds, generic interface values | The generic-interfaces program passes |
| 24 | Iterators | `Iter<T>` in the prelude, `for … of` over iterators | The iterators program passes |
| 25 | `Self` | `Self` in interface signatures and default bodies, bounds-only interfaces (E0427) | The `Self` program passes |
| 26 | Converting between interfaces | `J` to `I` where `J` declares `I`'s items, and through `?` | The converting-between-interfaces program passes, and the v6 spec is complete |

**Order:**
- **M23 first:** everything else in v6 builds on interfaces having parameters.
- **M24 next:** iterators are the everyday use of generic interfaces, and they need nothing more.
- **M25:** `Self` is independent of iterators, but it shares the satisfaction changes that M23 makes.
- **M26 last:** conversion between interfaces extends v5's tables, which by then include generic interfaces.

**Out of scope for v6** (for later, roughly in order):
- `Self` outside interfaces (in `impl` blocks), and default methods called on concrete types;
- packages with `public`;
- bitwise operators, `u32`/`u64`/`f32`, wrapping arithmetic;
- or-patterns and `mut` bindings in `match`, and `unwrap()`;
- iterator helpers (`map`, `filter`, `collect` on `Iter<T>`), and strings as iterators;
- binary I/O, directories, streams and processes;
- shared references and closures that change their captures;
- explicit type arguments and `.len()`.

## §12 Open questions — additions

- [ ] **Iterating a copy:** `for n of c` leaves `c` unchanged, which suits values but means a half-used iterator can't be resumed after a `break`. If programs need that, a later version could add `for n of mut c`.
- [ ] **One `X` per type:** a type satisfies `Source<X>` for at most one `X`, because it has one `next`. That's fine for iterators, but it rules out an interface such as `From<T>`, implemented once per `T`. Lifting it would need methods overloaded by type, which Lugha doesn't have.
