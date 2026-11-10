# Lugha v10 — Draft (delta against v9)

**Status:** approved 2026-10-14 — session 81 (proposed in session 80). The user asked for milestones 34–36 "in one go".
**Base:** `Language v9 Specification.md`, complete (milestone 33). It stays the source of truth, and what `lughac spec` prints, until v10's first milestone starts `Language v10 Specification.md`. That file then grows milestone by milestone, and v9 is frozen.

This draft lists only what changes, in the order of the v9 sections. Everything not mentioned keeps its v9 meaning. A valid v9 program stays valid in v10 unless it uses `i8`, `i16` or `u16` as a name: they become type keywords, as `u32` did in v7. Everything else v10 adds was an error before:
- `Self` outside an interface was E0305;
- a default method called on a concrete type, `MIN`/`MAX` on a type, and the new integer methods were E0415;
- `iter()`, `bytes()` and the iterator helpers were E0415, unless a type declares a method by that name, which still wins;
- `for … of` over a string was E0411, and the new names `range` and `parse_u64` were E0301.

## Decisions behind this draft (discovery, session 80)

1. **Headline: three independent milestones.** `Self` outside interfaces, more integer types, and iterator helpers.
2. **Iterator helpers are lazy.** `it.map(f)` returns a new iterator, and nothing runs until a loop or a consumer (`sum`, `collect`, …) pulls values, so chains work on endless iterators.
3. **The focused helper set:**
   - adapters `map`, `filter`, `take`, `skip`, `enumerate`;
   - consumers `collect`, `count`, `fold`, `any`, `all`, `find`, `sum`.
4. **Sources:** `xs.iter()` on arrays; `s.bytes()` on strings, and `for b of s`; `range(a, b)`.
5. **`Self` fully, in `impl` blocks:** as a type, in struct literals, in associated calls and in enum variants. Default methods become callable on concrete types.
6. **Integers:**
   - the keyword types `i8`, `i16` and `u16`;
   - `checked_*` and `saturating_*` methods;
   - rotations;
   - `parse_u64`;
   - `MIN` and `MAX` on every integer type.

**Choices made while writing this draft** (each settles a gap the answers left open):
- **Adapters are prelude structs.** `Map<T, U>`, `Filter<T>`, `Take<T>`, `Skip<T>`, `Enumerate<T>`, plus the sources `ArrayIter<T>`, `Bytes` and `Range`. Each holds its source as an `Iter<T>` value. Like `Iter`, each name is visible everywhere unless a module declares the name itself. Programs rarely write them, since a variable can be typed `Iter<i64>` instead.
- **`enumerate` gives `Indexed<T> { i: i64, value: T }`,** a prelude struct, because Lugha has no tuples.
- **Helpers apply wherever `for … of` does,** to any value with `next`, and to `Iter<T>` values and type parameters bound by `Iter<T>`. A type's own method of the same name wins. Arrays keep their eager v3 helpers: `xs.map(f)` is still an array, and `xs.iter().map(f)` is the lazy one.
- **Helpers take their receiver by copy,** as `for … of` does: `let m = it.map(f);` leaves `it` unchanged.
- **`take(n)` and `skip(n)` take an `i64`.** A negative `n` takes or skips nothing.
- **`sum()` is defined on iterators of every number type** and gives that type. It panics on overflow, as `+` does.
- **`Self` lives only in `impl` blocks and interfaces.** In a struct or enum declaration (`struct Node { kids: Self[] }`) it is still E0305. In `impl Stack<T>`, `Self` is `Stack<T>`.
- **A default method on a concrete type** is found among the interfaces visible where the call is: those declared in the calling module, in a module it imports, or in the prelude, that the type satisfies and that give the method a default. The type's own method wins. Exactly one such interface: the call uses its default. None: E0415, as before. More than one: a new error, E0430, naming two of them.
- **`T.MIN` and `T.MAX`** exist only for the integer types. Any other name after a type keyword, and `f64.MAX`, is E0415.
- **Rotations take an `i64` count,** used modulo the width (`n` mod 16 for a `u16`, so a negative count rotates the other way), and never panic.
- **`checked_div` gives `None`** for a zero divisor and for `MIN / -1`.
- **`parse_u64(s)`** follows `parse_i64`'s rules (§4) without a sign, and gives `None` above `u64.MAX`.
- **Indexes stay `i64`:** unsigned indexes wait (§12).

## §2 Lexical structure — changes

- **New keywords:** `i8`, `i16`, `u16`, as type names, like `i32`.

## §3 Grammar — changes

```ebnf
type      = … | "i8" | "i16" | "u16" ;
primary   = … | int_type "." IDENT ;        (* i16.MAX *)
int_type  = "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" ;
```

- **`Self`** is already a type name in interfaces. Inside an `impl` block it can also start a struct literal (`Self { x: 0 }`) and an associated call or variant (`Self.new()`, `Self.Red`), wherever a type's name can.

## §4 Type system — changes

**`Self` in `impl` blocks** (milestone 34).
- In an `impl T { … }` block, in signatures and bodies, `Self` is `T` with its type parameters: `Point`, or `Stack<T>` in `impl Stack<T>`.
- It is allowed wherever `T` would be:
  - types: `other: Self`, `: Self`, `Self[]`, `Option<Self>`;
  - struct literals: `Self { x: 0, y: 0 }`;
  - associated calls: `Self.origin()`;
  - variants: `Self.Red`.
- Elsewhere outside interfaces it is still E0305.

**Default methods on concrete types** (milestone 34). `c.describe()` on a concrete type without its own `describe` calls the default from the one visible interface that supplies it and that the type satisfies (choices above). It is checked as a call through that interface, with `Self` as the type. More than one candidate is E0430.

**More integer types** (milestone 35).

| Type | Width | Range |
| --- | --- | --- |
| `i8` | 8 bits, signed | −128 … 127 |
| `i16` | 16 bits, signed | −32768 … 32767 |
| `u16` | 16 bits, unsigned | 0 … 65535 |

- **Same rules as the other integers:** literals take them from the expected type, they cast, print, compare, overflow-panic and work with the bit operators.
- **Methods:** they have v7's `wrapping_*` and counting methods.
- **Externs:** in an extern they are C's `int8_t`, `int16_t` and `uint16_t`.

**Integer methods and constants** (milestone 35), on every integer type `I`:

| Expression | Gives | Meaning |
| --- | --- | --- |
| `I.MIN`, `I.MAX` | `I` | the smallest and largest value |
| `a.checked_add(b)`, `checked_sub`, `checked_mul`, `checked_div` | `Option<I>` | `None` where the operator would panic |
| `a.saturating_add(b)`, `saturating_sub`, `saturating_mul` | `I` | the result clamped to `I.MIN … I.MAX` |
| `a.rotate_left(n)`, `a.rotate_right(n)` | `I` | the bits rotated by `n` (an `i64`) modulo the width |

`b` has `a`'s type, as for v7's methods.

**`parse_u64(s)`** (milestone 35) gives `Option<u64>`.

**Iterator sources** (milestone 36).

| Expression | Gives | Yields |
| --- | --- | --- |
| `xs.iter()` | `ArrayIter<T>` | the elements of a copy of `xs`, in order |
| `s.bytes()` | `Bytes` | the bytes of `s`, as `u8` |
| `range(a, b)` | `Range` | `a`, `a + 1`, … up to `b − 1` (`i64`; nothing if `a >= b`) |

`for b of s` over a string loops over its bytes, as `for b of s.bytes()` does.

**Iterator helpers** (milestone 36), on any iterator `it` that yields `T`:

| Helper | Gives | Meaning |
| --- | --- | --- |
| `it.map(f)`, `f: fun(T): U` | `Map<T, U>` (yields `U`) | `f` of each value |
| `it.filter(f)`, `f: fun(T): bool` | `Filter<T>` | the values where `f` is true |
| `it.take(n)` | `Take<T>` | the first `n` values, then stops pulling |
| `it.skip(n)` | `Skip<T>` | all values after the first `n` |
| `it.enumerate()` | `Enumerate<T>` (yields `Indexed<T>`) | each value with its index from 0 |
| `it.collect()` | `T[]` | every value, in order |
| `it.count()` | `i64` | how many values |
| `it.fold(init, f)`, `f: fun(A, T): A` | `A` | values folded from `init` |
| `it.any(f)`, `it.all(f)` | `bool` | stops at the first value that decides |
| `it.find(f)` | `Option<T>` | the first value where `f` is true |
| `it.sum()` | `T` | the total, for number types only (E0415 otherwise) |

- **Argument types:** closure arguments are typed from the receiver, as for the array helpers.
- **Adapters are iterators themselves,** so they chain, and they work in `for … of`.

## §5 Expressions and statements — changes

- **`for b of s`** with `s` a `string` binds `b: u8`.

## §9 Compilation model — changes

- **`Self`** is resolved by the checker. Codegen sees only concrete types, so it needs no change.
- **Default methods on concrete types** call the existing default instance `lugha_fn_<Interface>.<method><Type>`.
- **The new integer types** are LLVM `i8` and `i16`. The methods lower to:
  - checked: `llvm.*.with.overflow`;
  - saturating: `llvm.*.sat`;
  - rotations: `llvm.fshl` and `llvm.fshr`;
  - `MIN`/`MAX`: constants.
- **`parse_u64`** calls a new runtime function, `lugha_rt_parse_u64`.
- **Iterator sources and helpers** are Lugha in the prelude: `it.map(f)` calls the prelude's `iter_map` with `it` converted to `Iter<T>`, as `xs.map(f)` calls `map`. `for b of s` lowers like an array loop over the string's bytes.
- **Like every prelude generic,** they are compiled only when reached, so programs using none of this give byte-identical IR.

**New error codes.** The numbers are proposed, and become final when their milestone's PRP is approved.

| Code | Error | Example |
| --- | --- | --- |
| E0430 | A default method that two visible interfaces supply | `c.describe()` where `Named` and `Shape` both default `describe` and `Circle` satisfies both |

**Reused codes:**
- **E0305:** `Self` outside interfaces and `impl` blocks.
- **E0415:** an unknown name after a type (`i64.TOP`, `f64.MAX`); `sum()` over non-numbers; a default method no visible interface supplies.
- **E0403:** a wrong argument type (`range(0, 1.5)`, `take(true)`).

## §10 Example programs — additions (v10 acceptance tests)

**`Self` in `impl` blocks** (milestone 34). Prints `(3, 4)`, `this is (3, 4)`, `green` on three lines, exits with 0.

```
interface Describe {
    fun name(self): string;
    fun describe(self): string = "this is {self.name()}";
}

struct Point { x: i64, y: i64 }

impl Point {
    fun origin(): Self = Self { x: 0, y: 0 };
    fun plus(self, other: Self): Self = Self { x: self.x + other.x, y: self.y + other.y };
    fun unit(): Self = Self.origin().plus(Self { x: 1, y: 1 });
    fun name(self): string = "({self.x}, {self.y})";
}

enum Light { Red, Green }

impl Light {
    fun toggle(self): Self = match self {
        Red => Self.Green,
        Green => Self.Red,
    };
}

fun main() {
    let p = Point.unit().plus(Point { x: 2, y: 3 });
    println(p.name());
    println(p.describe());
    match Light.Red.toggle() {
        Green => println("green"),
        Red => println("red"),
    }
}
```

**More integers** (milestone 35). Prints `32767`, `-1`, `32767`, `255`, `3`, `true`, `18446744073709551615` on seven lines, exits with 0.

```
fun main() {
    let a: i16 = i16.MAX;
    println(a);
    println(a.checked_add(1).unwrap_or(-1));
    println(a.saturating_add(100));
    let b: u8 = 200;
    println(b.saturating_mul(2));
    let c: u16 = 0b1000000000000001;
    println(c.rotate_left(1));
    let d: i8 = i8.MIN;
    println(d.checked_div(-1).is_none());
    println(parse_u64("18446744073709551615").unwrap());
}
```

**Iterator helpers** (milestone 36). Prints `16,4,36`, `5050`, `63`, `0=104;1=101;2=121;`, `3`, `true`, `5`, `31` on eight lines, exits with 0.

```
fun even(n: i64): bool = n % 2 == 0;

fun main() {
    let xs = [3, 1, 4, 1, 5, 9, 2, 6];
    let words = xs.iter().filter(even).map(fun(n) = "{n * n}").collect();
    println(words.join(","));
    println(range(1, 101).sum());
    println(range(10, 1000000000).filter(fun(n) = n % 7 == 0).take(3).sum());
    for p of "hey".bytes().enumerate() {
        print("{p.i}={p.value};");
    }
    println("");
    let mut vowels = 0;
    for b of "banana" {
        if b == 97 {
            vowels += 1;
        }
    }
    println(vowels);
    println(xs.iter().any(fun(n) = n > 8));
    println(xs.iter().find(fun(n) = n > 4).unwrap());
    println(xs.iter().fold(0, fun(a, n) = a + n));
}
```

## §11 Milestones — additions

| # | Milestone | Adds | Done when |
| --- | --- | --- | --- |
| 34 | `Self` everywhere | `Self` in `impl` blocks; default methods on concrete types (E0430) | The `Self`-in-`impl` program passes |
| 35 | More integers | `i8`, `i16`, `u16`; `checked_*`, `saturating_*`, rotations, `MIN`/`MAX`; `parse_u64` | The more-integers program passes |
| 36 | Iterator helpers | `iter`, `bytes`, `range`, `for b of s`; the adapters and consumers | The iterator-helpers program passes, and the v10 spec is complete |

**Order:**
- **M34 first:** the smallest. Its default-method rule also settles a v5 open question.
- **M35:** independent, and spread across the number tables.
- **M36 last:** the largest. It leans on generic interface values (v6) and closures (v3).

**Out of scope for v10** (for later, roughly in order):
- `zip`, `chain`, `take_while`, `skip_while`, `flat_map`, `min`/`max`, `position`, and a pair type;
- `for n of mut c`, and unsigned indexes;
- `Self` in struct and enum declarations;
- package versions, downloading, re-exports and a `--package` flag;
- a `Display`-like interface, and binary I/O, directories, streams and processes;
- shared references and closures that change their captures;
- explicit type arguments and `.len()`.

## §12 Open questions — additions and changes

- [x] **Default methods on concrete types** (v5): decided. They are allowed when exactly one visible interface supplies the default (E0430 otherwise).
- [ ] **`xs.iter()` copies the array.** It follows the copy rules, so iterating a variable still used later costs a copy; move on last use avoids it otherwise. A borrowing iterator would need shared references.
- [ ] **Unicode text.** `bytes()` is the only way to iterate a string; iterating characters waits for a decision on text encoding.
