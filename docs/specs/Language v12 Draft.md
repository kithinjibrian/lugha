# Lugha v12 — Draft (delta against v11)

**Status:** approved 2026-10-14 — session 88 (proposed in session 87). The user asked for milestones 39–40 "in one go".
**Base:** `Language v11 Specification.md`, complete (milestone 38). It stays the source of truth, and what `lughac spec` prints, until v12's first milestone starts `Language v12 Specification.md`. That file then grows milestone by milestone, and v11 is frozen.

This draft lists only what changes, in the order of the v11 sections. Everything not mentioned keeps its v11 meaning.

**Compatibility.** Every valid v11 program stays valid in v12, because everything v12 adds was an error before: printing, interpolating or `to_string` of a struct, enum, array, `Option`, interface value, function value or type parameter was E0403. Two things change for existing programs:
- **A panic message:** `unwrap()` on an `Err` now also prints the error.
- **The prelude's new name `Display`:** a module that declares something named `Display` keeps its own, as with `Iter`.

## Decisions behind this draft (discovery, session 87)

1. **Headline: every value prints.** `print`, `println`, `to_string` and `"{x}"` accept any type.
2. **A type's own text comes from `show`.** The prelude declares `interface Display { fun show(self): string; }`. A type with a matching method prints through it. Any other struct, enum, array or `Option` gets a built-in form: `Point { x: 1, y: 2 }`, `Circle(2.0)`, `[1, 2, 3]`, `Some(5)`, `None`.
3. **Strings are quoted inside the built-in form, and only there.** `println("ann")` still prints `ann`, but `Person { name: "ann" }` quotes it and escapes `"`, `\`, newlines and tabs. A `show`'s text is never quoted.
4. **The reach is everything:**
   - a type parameter prints in generic code without a bound;
   - an interface value prints as its value;
   - a function value prints as `<fun>`;
   - `unwrap` on `Err` shows the error;
   - `Display` is an ordinary interface, so it works as a bound and as a type.

**Choices made while writing this draft** (each settles a gap the answers left open):
- **Which `show` counts:** a struct's or enum's own method `fun show(self): string`, whatever its visibility, since printing is the type's own behaviour, not a use of its name. A method named `show` with any other signature is not used, and the built-in form applies; it is no error, so v11 programs with such methods stay valid.
- **The built-in form:**
  - `Name { field: value, … }` for a struct, fields in declaration order, `Name {}` without fields;
  - `Variant(a, b)` or `Variant` for an enum value, without the enum's name;
  - `[a, b]` for an array, `[]` when empty;
  - `Some(x)` and `None`;
  - `Ok(x)` and `Err(e)` for `Result`, which is an ordinary prelude enum.
  - Each part prints by these same rules, so a field that has `show` uses it.
  - Numbers, `bool`, quoted strings and `<fun>` appear as themselves.
  - Generic types print without their arguments: `Stack { items: [1] }`.
  - A type from another module prints with its bare name: `Circle`, not `shapes.Circle`. The text is for people, and the name is what they wrote.
- **Recursion:** a recursive enum prints to its full depth, like any value. Printing a self-referential structure can't loop, since Lugha values are trees.
- **Escapes in quoted strings:** `\"`, `\\`, `\n`, `\t`, `\r`, and `\u{XX}` for other bytes below 0x20. Other bytes are copied as they are.
- **Interface values** print by their concrete type's rules. The print site compares the value's type identity (`lugha_type_<T>`, v5) with each type the program converts to that interface. Dispatch tables don't change.
- **`unwrap` on `Err(e)`** panics with `unwrap on Err: ` followed by `e` printed by these rules. `expect(msg)` keeps exactly `msg`.
- **`Display` in the prelude** is visible everywhere like `Iter` (unless a module declares the name). It needs no special rules: `T: Display` lets generic code call `x.show()`, and `Display[]` holds values of any type with a `show`.

## §4 Type system — changes

- **Printing:** every type can be printed, interpolated and passed to `to_string`; the E0403 for structs, enums, arrays, `Option`, interface values, function values and type parameters is gone.
- **`Display`:** `interface Display { fun show(self): string; }`, declared in the prelude.

## §5 Expressions and statements — changes

**How values print** (milestone 39, interface and function values in 40):

| Value | Prints as |
| --- | --- |
| a number, `bool` | as in v11 |
| a `string`, on its own | its bytes, as in v11 |
| a `string` inside a built-in form | quoted, with escapes: `"a\"b"` |
| a struct or enum with `fun show(self): string` | what `show` returns, unquoted |
| another struct | `Point { x: 1, y: 2 }` |
| another enum value | `Circle(2.0)`, `Empty` |
| an array | `[1, 2, 3]` |
| `Option` | `Some(5)`, `None` |
| an interface value | as its concrete value |
| a function value | `<fun>` |

`"{x}"` and `to_string(x)` give the same text that `print(x)` writes.

## §9 Compilation model — changes

- **Text helpers:** a type that is printed (or reached inside a printed value) gets one generated function, `lugha_show_<type>(value) -> string`, named like the copy helpers (`lugha_show_5Point`, `lugha_show_i64_arr`), emitted once and only when used. It calls the type's `show`, or builds the built-in form with the runtime's string functions.
- **Runtime:** a new `lugha_rt_str_quote(s)` returns `s` quoted and escaped.
- **Interface values:** a print site lowers to a chain of identity comparisons over the types converted to that interface, each calling that type's helper.
- **`unwrap` on `Err`** builds its panic message with the error's helper.
- **Programs that print only numbers, `bool` and strings,** and never `unwrap` an `Err`, give byte-identical IR.

**New error codes:** none. E0403 now applies to fewer things.

## §10 Example programs — additions (v12 acceptance tests)

**Printing values** (milestone 39). Prints these seven lines and exits with 0:

```
Point { x: 1, y: 2 }
Circle(1.5)
[Some(1), None]
Team { name: "red", members: ["ann", "bo"] }
$5.25
wallet: Wallet { cash: $5.25 }
Stack { items: [3] }
```

```
struct Point { x: i64, y: i64 }

enum Shape { Circle(f64), Empty }

struct Team { name: string, members: string[] }

struct Money { cents: i64 }

impl Money {
    fun show(self): string = "${self.cents / 100}.{self.cents % 100}";
}

struct Wallet { cash: Money }

struct Stack<T> { items: T[] }

fun dump<T>(x: T) {
    println(x);
}

fun main() {
    println(Point { x: 1, y: 2 });
    println(Shape.Circle(1.5));
    println([Some(1), None]);
    println(Team { name: "red", members: ["ann", "bo"] });
    let m = Money { cents: 525 };
    println(m);
    println("wallet: {Wallet { cash: m }}");
    dump(Stack { items: [3] });
}
```

**Printing anything** (milestone 40). Prints `Circle { r: 2.0 }`, `square 3`, `<fun>`, then panics with `unwrap on Err: Bad { code: 7 }`, exit 101.

```
interface Shape { fun area(self): f64; }

struct Circle { r: f64 }
impl Circle { fun area(self): f64 = 3.0 * self.r * self.r; }

struct Square { side: i64 }
impl Square {
    fun area(self): f64 = (self.side * self.side) as f64;
    fun show(self): string = "square {self.side}";
}

struct Bad { code: i64 }

fun main() {
    let shapes: Shape[] = [Circle { r: 2.0 }, Square { side: 3 }];
    for s of shapes {
        println(s);
    }
    println(fun(x: i64): i64 = x);
    let r: Result<i64, Bad> = Err(Bad { code: 7 });
    println(r.unwrap());
}
```

## §11 Milestones — additions

| # | Milestone | Adds | Done when |
| --- | --- | --- | --- |
| 39 | Printing values | `Display` and `show`; the built-in form for structs, enums, arrays, `Option`; quoting; generic `T` | The printing-values program passes |
| 40 | Printing anything | interface values, function values, `unwrap` on `Err` | The printing-anything program passes, and the v12 spec is complete |

**Order:**
- **M39 first:** the helpers and the runtime's quoting.
- **M40 second:** it builds on M39's helpers, adding the identity chain and the panic message.

**Out of scope for v12** (for later, roughly in order):
- format specifiers (`{x:.2}`, padding, hex);
- a separate debug form next to `show` (Rust's `{:?}`);
- parsing values back from text;
- downloading packages, whole-module re-exports and import aliases;
- `zip`/`chain` and the other iterator helpers; `for n of mut c`; unsigned indexes; `Self` in declarations; Unicode text; binary I/O.

## §12 Open questions — additions and changes

- [x] **Printing errors** (v9). Decided (v12): `unwrap on Err` prints the error.
- [ ] **A `show` with the wrong signature** is silently ignored, for compatibility. A later warning could point out `fun show(self): i64`.
- [ ] **Module paths in printed names:** a type prints with its bare name, so two modules' `Point`s look the same. A debug form could add the path.
