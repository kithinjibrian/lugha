# Lugha v5 — Draft (delta against v4)

**Status:** approved 2026-10-13 — session 57
**Base:** `Language v4 Specification.md`, complete (milestones 17–19, plus v4.1). It stays the source of truth, and what `lughac spec` prints, until v5's first milestone starts `Language v5 Specification.md`. That file then grows milestone by milestone, and v4 is frozen, as earlier versions were.

This draft lists only what changes, in the order of the v4 sections. Everything not mentioned keeps its v4 meaning. Every valid v4 program stays valid in v5: everything v5 adds was an error before (an interface used as a type was E0305, a method body in an interface and a `:` in a pattern were syntax errors).

## Decisions behind this draft (discovery, session 57)

1. **Headline: interface values.** An interface can be the type of a value, so one array can hold circles and squares, and a call goes to the concrete type's method at run time. v5 is about interfaces only; bitwise operators, more integer types, packages and the `match` additions wait.
2. **Conversion is implicit, but only at a written interface type.** Where the expected type is an interface (an annotation, a parameter, a field, an element, a payload, a return type), a value of a type that satisfies it is converted. This is the one implicit conversion in Lugha (§4 names it). `let c = Circle { r: 1.0 };` stays a `Circle`.
3. **Interface values are values.** A `Shape` holds its own copy of what's inside; assigning one copies it, like a struct. A `mut self` method called through a `let mut s: Shape` changes `s`'s copy only.
4. **Type patterns get the concrete value back:** `c: Circle => …` in a `match` on an interface value. The set of types is open, so such a `match` needs a `_` arm.
5. **Default methods:** an interface method may have a body, which a type gets unless it has the method itself.
6. **`?` converts errors into an interface:** in a function returning `Result<T, Error>`, `?` on a `Result<U, ParseError>` wraps the `ParseError` as an `Error`. This resolves v4's open question on error conversion.
7. **No `==` on interface values,** as for functions. Match on the concrete types to compare.

Later (the out-of-scope list in §11): `Self`, generic interfaces, converting one interface value to another interface.

## §2 Lexical structure — changes

None. No new keywords or tokens.

## §3 Grammar — changes

```ebnf
iface_item   = "fun" IDENT [ "(" params ")" ] [ ":" type ] ( ";" | "=" expr ";" | block ) ;
pattern      = … | ( IDENT | "_" ) ":" type ;          (* `c: Circle`, `_: Square` *)
```

- **Types:** an interface's name (`Shape`, `shapes.Shape`) is a type wherever a type can be written: `Shape[]`, `Option<Shape>`, fields, payloads, parameters, return types, function types and type arguments. The grammar of types doesn't change; this is a rule of §4.
- **Default methods:** an interface method may end in `= expr;` or a block instead of `;`.
- **Type patterns:** `name: Type` or `_: Type`, anywhere a pattern may appear, including nested (`Some(c: Circle)`).

## §4 Type system — changes

**Interface values.**
- **The type:** an interface `I` is a type when every item it declares is a method (has a `self` or `mut self` receiver). An interface with an associated function can still be a bound, but used as a type it is E0425, with help naming the associated function.
- **Conversion** (the one implicit conversion): where the expected type is exactly `I`, a value of a type that satisfies `I` (v4's rule, checked where the conversion happens, with that place's visibility) becomes an `I`. A type that doesn't satisfy it is E0422, naming each missing or mismatched item.
  - **Where:** wherever an expected type reaches a value: `let s: Shape = c;`, arguments, struct-literal fields, array elements (`let xs: Shape[] = [c, sq];`), payloads (`Some(c)` where `Option<Shape>` is expected), `Ok`/`Err`, assignments to a place of type `I`, `return` and tail values.
  - **Not inside other types:** a `Circle[]` is not a `Shape[]`, and an `Option<Circle>` is not an `Option<Shape>` (E0403). Convert the elements one by one, for example with `map`.
  - **A type parameter** `T` whose bounds make it satisfy `I` converts too; each instance converts its concrete type.
  - **An `I` value** is already an `I`. A value of another interface type `J` does not convert to `I` (E0403), even if `J` declares the same items; that waits (§11).
  - **No expected type, no conversion:** `let s = c;` keeps `c`'s type, and `[c, sq]` with no expected type is E0403 (the elements disagree), with help to annotate the array.
- **Calls:** `s.area()` calls the concrete type's method. The arguments and result are checked against the interface's signature. A `mut self` method needs a mutable place, with the v1 rules (E0501, E0502, E0508). Fields of the value inside aren't reachable (E0306); use a type pattern.
- **Copies:** an interface value holds its own copy of the value inside. Assigning, passing or storing one copies that value, deep, as for a struct; move on last use applies as it does to every type.
- **Equality and order:** none (E0404, with help to match on the concrete types). So an interface type doesn't satisfy `Eq` or `Ord` (E0419), nor does a struct, enum, array or `Option` that holds one.
- **As a bound's argument:** `I` satisfies the interface `I` itself, so `total_area<Shape>(shapes)` works and calls through the value. It satisfies another interface only if `I` declares all of that interface's items the same way (v4's rule, applied to `I`'s declared items).
- **Printing:** `print`, `println`, `eprint`, `eprintln` and interpolation don't accept interface values (E0403), as for structs.

**Default methods.**
- **Declaring:** `fun describe(self): string = "area {self.area()}";` inside an interface. The body is checked once, with `self` typed as a type parameter bounded by the interface, so it can call the interface's other items, and nothing else on `self`.
- **Which types get it:** a type satisfies the interface if it has every item without a body, as before. For an item with a body, the type's own method is used when it has one with that name. That method must have the same signature, or it is E0422. Otherwise the default is used.
- **Where it can be called:** through an interface value (`s.describe()`) and through a bound (`x.describe()` with `T: Shape`). On a concrete type, a default method doesn't exist: `c.describe()` on a `Circle` without its own `describe` is E0415, with help that it is an interface default. A type never declares the interfaces it satisfies, so the method can't be found from the type alone.

**Type patterns.**
- **`name: T` matches** when the interface value holds a `T`, and binds the value inside, as a `T`, to `name` (immutable, copied when stored, like any `match` binding). `_: T` matches without binding.
- **Only on interface values:** a type pattern on any other type is E0426. So is a pattern whose `T` is an interface, or a type that doesn't satisfy the matched interface (it could never match), or a generic type written without its type arguments.
- **Guards and nesting** work as for every pattern: `c: Circle if c.r > 1.0 =>`, `Some(c: Circle) =>`.
- **Exhaustiveness:** the set of types is open, so a `match` on an interface value needs a `_` arm (E0417, with `_` as the witness). A later arm that an earlier unguarded arm covers is W0102, as before (`c: Circle` twice).

**The `?` operator** (amends v4). On `Result<T, E>`, the enclosing function or closure must return `Result<U, F>` where `F` is `E`, or `F` is an interface that `E` satisfies, in which case the error is converted. Anything else is still E0424, with help to `map_err` it.

## §5 Expressions and statements — changes

- **Method calls** on interface values are §4's calls. Nothing else changes.

## §7 Memory model — changes

- **An interface value is two words:** a pointer to a dispatch table for its (concrete type, interface) pair, and a pointer to a GC-allocated copy of the value inside (`lugha_rt_alloc`). Primitives are boxed the same way, so every interface value has one layout.
- **The dispatch table** holds the concrete type's identity (for type patterns), a copy function (for §4's copies) and one entry per method, in the interface's declaration order.

## §8 C interop — changes

- **Interface values can't cross the C boundary** (E0409), like every aggregate.

## §9 Compilation model — changes

- **Symbols** (amends §8's naming): a dispatch table is `lugha_vt_<Type>.<Interface>`, with types and interfaces printed as in other symbols (`lugha_vt_Circle.Shape`, `lugha_vt_shapes/Circle.shapes/Shape`). Each method entry is a wrapper `lugha_dyn_<Type>.<method>` that takes the boxed pointer and calls the method. A default method compiles once per type that uses it, like a generic method: `lugha_fn_<Interface>.<method><Type>`.
- **Conversion** copies the value into a new box and pairs it with the table; a value about to be moved isn't copied first.
- **Calls** load the entry from the table and call it indirectly, as function values do.
- **Type patterns** compare the table's identity with the pattern type's.
- **Tables are emitted only for the pairs a program converts,** recorded by the checker, so programs without interface values give byte-identical IR.
- **The runtime** needs nothing new.

**New error codes.** The numbers are proposed and become final when their milestone's PRP is approved.

| Code | Error | Example |
| --- | --- | --- |
| E0425 | An interface with associated functions used as a type | `let x: Factory = f;` where `Factory` declares `fun make(): Factory;` |
| E0426 | A type pattern that can't apply | `c: Circle` on an `i64`; `s: Shape` (an interface); `r: Rect` when `Rect` doesn't satisfy the interface |

**Reused codes:**
- E0305 stays for unknown type names; an interface used as a type is no longer an error.
- E0422 now also reports a conversion whose value doesn't satisfy the interface, and a type's method that conflicts with a default.
- E0403: `Circle[]` where `Shape[]` is expected; one interface to another; an interface value printed.
- E0404 and E0419: `==` and `Eq` on interface values.
- E0415: a default method called on a concrete type.
- E0417: a `match` on an interface value without `_`.
- E0424: `?` whose error neither matches nor converts.

## §10 Example programs — additions (v5 acceptance tests)

**Interface values** (milestone 20). Prints `16.0` then `1.0` then `circle` on three lines, exits with 0.

```
interface Shape {
    fun area(self): f64;
    fun name(self): string;
    fun grow(mut self, k: f64);
}

struct Circle { r: f64 }

impl Circle {
    fun area(self): f64 = 3.0 * self.r * self.r;
    fun name(self): string = "circle";
    fun grow(mut self, k: f64) { self.r *= k; }
}

struct Square { side: f64 }

impl Square {
    fun area(self): f64 = self.side * self.side;
    fun name(self): string = "square";
    fun grow(mut self, k: f64) { self.side *= k; }
}

fun total(shapes: Shape[]): f64 {
    let mut sum = 0.0;
    for s of shapes {
        sum += s.area();
    }
    sum
}

fun main() {
    let c = Circle { r: 1.0 };
    let mut shapes: Shape[] = [c, Square { side: 2.0 }];
    shapes[0].grow(2.0);
    println(total(shapes));
    println(c.r);
    let first = shapes[0];
    println(first.name());
}
```

**Type patterns** (milestone 21). Prints `big circle` then `circle of radius 1.5` then `a square` on three lines, exits with 0.

```
interface Shape {
    fun area(self): f64;
}

struct Circle { r: f64 }

impl Circle {
    fun area(self): f64 = 3.0 * self.r * self.r;
}

struct Square { side: f64 }

impl Square {
    fun area(self): f64 = self.side * self.side;
}

fun describe(s: Shape): string = match s {
    c: Circle if c.r > 10.0 => "big circle",
    c: Circle => "circle of radius {c.r}",
    _: Square => "a square",
    _ => "some shape",
};

fun main() {
    let shapes: Shape[] = [Circle { r: 20.0 }, Circle { r: 1.5 }, Square { side: 2.0 }];
    for s of shapes {
        println(describe(s));
    }
}
```

**Errors as interfaces** (milestone 22). Prints `42` then `error: not a number: x` then `range error: 500 is out of range` on three lines, exits with 0.

```
interface Error {
    fun message(self): string;
    fun report(self): string = "error: {self.message()}";
}

struct ParseError { text: string }

impl ParseError {
    fun message(self): string = "not a number: {self.text}";
}

struct RangeError { n: i64 }

impl RangeError {
    fun message(self): string = "{self.n} is out of range";
    fun report(self): string = "range error: {self.message()}";
}

fun parse(s: string): Result<i64, ParseError> = parse_i64(s).ok_or(ParseError { text: s });

fun check(n: i64): Result<i64, RangeError> =
    if n < 100 { Ok(n) } else { Err(RangeError { n: n }) };

fun read(s: string): Result<i64, Error> {
    let n = parse(s)?;
    let m = check(n)?;
    Ok(m * 2)
}

fun main() {
    for s of ["21", "x", "500"] {
        match read(s) {
            Ok(n) => println(n),
            Err(e) => println(e.report()),
        }
    }
}
```

## §11 Milestones — additions

| # | Milestone | Adds | Done when |
| --- | --- | --- | --- |
| 20 | Interface values | interfaces as types, conversion at an expected interface type, dynamic calls, copies, `I` as a type argument (E0425) | The interface-values program passes |
| 21 | Type patterns | `name: T` and `_: T` in `match`, exhaustiveness with `_` (E0426) | The type-patterns program passes |
| 22 | Default methods and error conversion | interface methods with bodies; `?` converting into an interface error | The errors-as-interfaces program passes, and the v5 spec is complete |

**Order:**
- **M20 first:** the values themselves; everything else uses them.
- **M21 next:** getting the concrete value back is the escape hatch real programs need.
- **M22 last:** default methods and `?` conversion make interfaces pleasant for errors, the main use the discovery found.

**Out of scope for v5** (for later, roughly in order):
- `Self` in interfaces, generic interfaces, and converting one interface value to another;
- packages with `public`;
- bitwise operators, `u32`/`u64`/`f32`, wrapping arithmetic;
- or-patterns and `mut` bindings in `match`, and `unwrap()`;
- binary I/O, directories, streams and processes;
- shared references and closures that change their captures;
- explicit type arguments and `.len()`.

## §12 Open questions — additions

- [ ] **Boxing primitives:** an `i64` inside an interface value is boxed like a struct, so every interface value has one layout. Small values could be stored inline later, as an optimisation that changes nothing visible.
- [ ] **Default methods on concrete types:** `c.describe()` is E0415 because a type never names its interfaces. If that proves annoying, a later version could allow it when exactly one visible interface supplies the default.
- [ ] **The spec's stale out-of-scope list:** v4's §11 still shows the v1 "Out of scope for v1" list. `Language v5 Specification.md` replaces it with the list above.
