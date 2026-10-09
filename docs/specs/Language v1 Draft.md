# Lugha v1 — Draft (delta against v0)

**Status:** draft for review — session 24, 2026-10-09
**Base:** `Language v0 Specification.md`, which stays the source of truth (and what `lughac spec` prints) until v1 is implemented. Each v1 milestone folds its sections into a full v1 specification.

This draft lists only what changes, in the order of the v0 sections. Everything not mentioned keeps its v0 meaning, and every valid v0 program stays valid in v1 unless it uses one of the new keywords as an identifier.

## Decisions behind this draft (discovery, session 24)

1. **Scope:**
   - v1 adds **enums and `match`**, **growable arrays** and **methods**.
   - It adds a built-in **`Option<T>`** and **recursive enums**.
   - User generics, modules and closures wait for v2.
2. **Form:** a delta draft; v0 stays authoritative until v1 ships.
3. **Enums:** positional payloads, built through the enum name with `.` (`Shape.Circle(2.0)`). There is no `::` path syntax. Two rules make `.` unambiguous and forgiving (added after review):
   - A local may not be named like a type (E0311), so `Shape.Circle` always means the type `Shape`.
   - `::` is lexed only to report E0207 "write `Shape.Circle`", for code written with Rust habits.
4. **`match`:**
   - It is an expression, with flat patterns: a variant with names or `_` for its payloads, an integer, `bool` or string literal, or `_`.
   - It must be exhaustive. There are no guards and no nested patterns.
5. **Recursion:** an enum may contain itself. The compiler stores such values on the heap implicitly; there is no `Box`.
6. **Optional values:** a built-in generic `Option<T>` with `Some(T)` and `None`. `Option` is the only type that takes a type argument in v1.
7. **Growable arrays:** there is no new list type. `T[]` itself gains `push`, `pop` and `clear`.
8. **Methods:**
   - They live in `impl` blocks, with `self` or `mut self` receivers.
   - Functions without `self` are called on the type (`Point.origin()`).

---

## §1 Overview — changes

- **Non-goals for v1** become: generics (other than the built-in `Option<T>`), modules, closures, nested patterns and guards, and a self-hosted compiler.
- **Goals** gain: sum types with exhaustive matching, so a missing case is a compile error; and sequences that can grow.

## §2 Lexical structure — changes

- **Keywords** gain `enum`, `match`, `impl`, `self`.

  ```
  fun  extern  struct  enum  impl  let  mut  if  else  while  for  in  of  match
  return  break  continue  true  false  as  self
  i32  i64  u8  f64  bool  string
  ```

- **Operators and punctuation** gain `=>`. In type position, `<` and `>` also delimit `Option`'s type argument; they are the existing tokens.
- **`::` is lexed as a token only so the parser can reject it helpfully.** Any use of it is E0207 "`::` is not Lugha syntax", with help "write `Shape.Circle`". The parser recovers as if `.` had been written, so later errors are still found.
- `Option`, `Some` and `None` are **not keywords**. They are built-in global names, reserved like the intrinsics (§6), so a program can't define them.

## §3 Grammar — changes

```ebnf
item         = fun_decl | extern_decl | struct_decl | enum_decl | impl_decl ;

enum_decl    = "enum" IDENT "{" [ variant { "," variant } [ "," ] ] "}" ;
variant      = IDENT [ "(" type { "," type } [ "," ] ")" ] ;

impl_decl    = "impl" IDENT "{" { method } "}" ;
method       = "fun" IDENT "(" [ receiver [ "," params ] | params ] ")" [ ":" type ] fun_body ;
receiver     = [ "mut" ] "self" ;

base_type    = "i32" | "i64" | "u8" | "f64" | "bool" | "string"
             | "Option" "<" type ">"
             | IDENT ;

block_like   = if_expr | match_expr | block ;
match_expr   = "match" cond "{" [ arm { arm_sep arm } [ "," ] ] "}" ;
arm          = pattern "=>" expr ;
arm_sep      = "," | (* nothing, after an arm whose expr is a block *) ;
pattern      = "_"
             | IDENT [ "(" pat_slot { "," pat_slot } [ "," ] ")" ]
             | [ "-" ] INT | STRING | "true" | "false" ;
pat_slot     = IDENT | "_" ;

primary      = … | "self" ;
```

- **Paths and method calls need no new grammar.** `Shape.Circle(2.0)`, `Point.origin()`, `xs.push(4)` and `p.dist(q)` already parse as a member followed by a call. The checker decides what they mean (§4, §6).
- **The `match` scrutinee** is a `cond`, parsed in no-struct-literal mode for the same reason as `if` (§3).
- **A `match` is block-like.** As a statement it needs no `;`, and a non-`void` `match` statement whose value is discarded is E0505, exactly like `if`.
- **Patterns:**
  - A bare `IDENT` must name a variant of the scrutinee's enum; there are no top-level binding patterns, so use `_`.
  - Payload slots bind names, or ignore a payload with `_`.

## §4 Type system — changes

**New types.**

| Type | Meaning | LLVM representation (§7) |
| --- | --- | --- |
| `E` (enum) | One of the declared variants, with its payloads | A tagged value `{ i32 tag, payload }`; a pointer to a heap node if `E` is recursive |
| `Option<T>` | `Some(T)` or `None` | As a non-recursive enum with those two variants |

**Enums.**
- **Declaration:** at top level, in any order. A variant name is unique within its enum (E0309).
- **Construction:** `E.Variant(a, b)` or `E.Variant` for a payload-less variant. Each payload expects its declared type, and the count must match (E0416).
- **`Option`:** values are written `Some(x)` and `None` without a prefix. `Some(x)` has type `Option<T>` where `x : T`. `None` needs an expected type, exactly like `[]`; otherwise it is E0412 "cannot infer the type of `None`".
- **No operators:** there is no `==` or other operator on enums or `Option` (E0404). Compare with `match`.
- **Recursion:** an enum may contain itself, directly or through structs or other enums. Such a **recursive enum** is stored behind a pointer to a GC-heap node, so it has a finite size; the program can't observe the pointer.
- **E0307 narrowed:** it now applies only to a struct cycle that passes through no enum and no array.

**`match`.**
- **Arms:** the arms' types follow `if`/`else` (E0403 on a mismatch). An arm that diverges fits any type.
- **Pattern kinds:** variant patterns need an enum or `Option` scrutinee; literal patterns need a scrutinee of the literal's type (E0403).
- **Exhaustiveness:** every variant must be covered, or the match must end with `_`. Otherwise it is **E0417** "non-exhaustive match: `Rect` not covered", listing every missing variant. A literal match always needs a final `_`.
- **Unreachable arms:** an arm after `_`, or a variant already covered, is **W0102** "unreachable match arm" (a warning).
- **Payload bindings:** immutable and **borrowed**, like `for … of` variables. They are read in place, and returning or storing one copies it (§4 copy table, row `return p`).

**Growable arrays.** `T[]` has a length that can change through three built-in methods:

| Method | Effect |
| --- | --- |
| `xs.push(v)` | Appends `v`; `xs.len` grows by one |
| `xs.pop()` | Removes and returns the last element as `Some(x)`, or `None` if `xs` is empty |
| `xs.clear()` | Removes every element; `xs.len` becomes 0 |

- **Mutable receiver:** all three need `xs` to be a place with a `let mut` root (E0501), not inside a string (E0506). Calling them on an array being iterated by `for … of` is E0507.
- **Values:** `push` stores its value with the same copy rule as assignment (copied from a place; moved on last use). `pop` moves the element out.
- **Unchanged:** `xs[i]` is still bounds-checked against the current `.len`, and value semantics are unchanged; copying an array copies its current elements.

**Methods.**
- **Where they live:** `impl T { … }` attaches functions to a struct or enum `T` declared anywhere in the file. A type may have several `impl` blocks.
- **Names:** method names are unique per type, and may not equal a field name of a struct (**E0310**).
- **`self` receivers:** a method with `self` is called as `x.m(args)`.
  - `self` is an immutable, borrowed parameter of type `T` (§4 copy rules apply as for any parameter).
  - With `mut self`, the call needs `x` to be a place with a `let mut` root (E0501). Assignments to `self`'s fields and elements change the caller's place in place, and `mut self` may be reassigned whole.
- **Associated functions:** a function without a receiver is called as `T.f(args)`.
- **Errors:** an unknown method or associated function is **E0415** "no method `m` on `T`". This is E0410's counterpart for calls.
- **Name resolution for `T.f`:** a local may never be named like a type. `let`, parameters, `for` variables and `match` bindings named after a struct, an enum or `Option` are **E0311** "`Shape` is the name of a type", with help "rename the variable". So a name before `.` is either a type, meaning a variant or associated function, or a local, meaning a field or method of its value; it is never ambiguous.
- **Built-ins:** arrays have the built-in methods above and no others, and no `impl` may target a built-in type (E0305).

**Copy table (§4) — additions.**

| Situation | Copied? | Why |
| --- | --- | --- |
| `xs.push(v)` where `v` is a place | Yes (or moved on last use) | The array gets its own element |
| A `match` payload binding | No | Read in place, like a `for … of` variable |
| Passing `x` as `self` | No | Receivers are passed like any other argument |
| A `mut self` call | No | The method edits the caller's place directly |

## §5 Expressions and statements — changes

- **`match` evaluation:** it evaluates its scrutinee once, then tests the arms from top to bottom and runs the first that matches.
- **Method calls:** a call `x.m(a, b)` evaluates `x`, then `a`, then `b`. For a `mut self` call, the place `x` is evaluated once, before the arguments, like an assignment's place.
- **`push` order:** `xs.push(v)` evaluates the place `xs`, then `v`.
- **`for x of xs`:** the length is still read once, and the array can't change during the loop (E0507).
- **No new panics:** `pop` on an empty array returns `None`, and an out-of-range index after a `pop` panics as before.

## §6 Functions and program structure — changes

- **Items:** a program may contain `enum` and `impl` items.
- **Pass 1** also collects every enum, its variants, and every `impl` block's method signatures.
- **Pass 2** resolves enum payload types and method parameter types. Recursive enums are found here (§4).
- **Shared namespace:** functions, structs, enums, intrinsics and the built-ins `Option`, `Some` and `None` share one global namespace (E0302).
- **Methods are not in it:** `impl` methods live in their type's namespace, so `fun dist()` and `Point.dist` can coexist.
- **Symbols:** methods and associated functions are emitted as `lugha_fn_<Type>.<name>` (for example `lugha_fn_Point.dist`).
  - `.` can't appear in a Lugha identifier, so these can't collide with `lugha_fn_<name>` or C symbols.
  - The `lugha_fn_` prefix rule (§8) is unchanged.

## §7 Memory model — changes

- **Enums:** a non-recursive enum is a stack value `{ i32 tag; payload }`. The payload area has the size and alignment of the largest variant's payloads laid out as a struct, with natural alignment. Copying it is a load and store, plus deep copies for payloads that hold arrays or recursive enums.
- **Recursive enums:** a value is a pointer to a GC-heap node `{ i32 tag; payload }` from `lugha_rt_alloc`. Copying deep-copies the node graph through a generated `lugha_copy_<mangled>` helper, length-prefixed like structs. Move on last use (PRP-018) applies.
- **Arrays:** the heap object becomes `{ i64 len; i64 cap; T elems[cap] }`, still held by one pointer.
  - **Growth:** `push` beyond `cap` allocates a new object with doubled capacity (at least 4), copies the elements, and stores the new pointer into the array's place. Each array has exactly one owner (§7), so no other pointer can see the old object.
  - **Literal and copied arrays** have `cap == len`.
  - **Strings** keep their v0 layout.

## §8 C interop — changes

- Enums and `Option` can't cross the C boundary (E0409), like structs and arrays.

## §9 Compilation model — changes

**Lowering notes.**
- **`match` on an enum:** an LLVM `switch` on the tag.
  - **On integers:** a `switch` on the value.
  - **On strings:** a chain of `lugha_rt_str_eq` calls.
  - **Payload bindings:** loads from the payload area at constant offsets (`build_struct_gep`).
- **Recursive enums:** a pointer load first, then the same.
- **Growing arrays:** `push` and `pop` are inline header updates, plus a runtime call to grow: a new `lugha_rt_array_grow(ptr, elem_size)`, which returns the new pointer.
- **Methods:** ordinary functions whose first parameter is the receiver. `self` is passed like a struct argument (a pointer to the caller's storage, §7). `mut self` passes a pointer to the caller's place itself.

**New error codes** (numbers proposed; final when each milestone's PRP is approved):

| Code | Error | Example |
| --- | --- | --- |
| E0309 | Variant declared twice in one enum | `enum E { A, A }` |
| E0310 | Method declared twice for a type, or named like a field | `impl P { fun x(self) {} }` where `P` has field `x` |
| E0311 | A local named like a type | `let Shape = 3;`, `fun f(Point: i64)` |
| E0207 | `::` used as a path separator | `Shape::Circle(1.0)` — help: write `Shape.Circle(1.0)` |
| E0415 | No such method, associated function or variant | `p.size()`, `Shape.Circel(1.0)` |
| E0416 | Wrong number of payloads in a constructor or pattern | `Shape.Rect(1.0)`, `Circle(a, b) => …` |
| E0417 | Non-exhaustive `match` | `match s { Circle(r) => r }` with more variants |
| W0102 | Unreachable `match` arm (warning) | An arm after `_` |

**Reused codes:** E0302, E0305, E0403, E0404, E0409, E0412 (now also `None`), E0501, E0505, E0506, E0507.

## §10 Example programs — additions (v1 acceptance tests)

**Shapes** (milestone 6). Prints `12.56636` then `6.0` then `0.0` on three lines, exits with 0.

```
enum Shape {
    Circle(f64),
    Rect(f64, f64),
    Empty,
}

fun area(s: Shape): f64 = match s {
    Circle(r) => 3.14159 * r * r,
    Rect(w, h) => w * h,
    Empty => 0.0,
};

fun main() {
    let shapes = [Shape.Circle(2.0), Shape.Rect(2.0, 3.0), Shape.Empty];
    for s of shapes {
        println(area(s));
    }
}
```

**A linked list** (milestone 6). Prints `6`, exits with 0.

```
enum List {
    Nil,
    Cons(i64, List),
}

fun sum(l: List): i64 = match l {
    Nil => 0,
    Cons(x, rest) => x + sum(rest),
};

fun main() {
    let l = List.Cons(1, List.Cons(2, List.Cons(3, List.Nil)));
    println(sum(l));
}
```

**Methods** (milestone 7). Prints `5.0` then `6.0` on two lines, exits with 0.

```
struct Vec2 { x: f64, y: f64 }

impl Vec2 {
    fun new(x: f64, y: f64): Vec2 = Vec2 { x: x, y: y };
    fun len_sq(self): f64 = self.x * self.x + self.y * self.y;
    fun scale(mut self, k: f64) {
        self.x *= k;
        self.y *= k;
    }
}

fun main() {
    let mut v = Vec2.new(1.0, 2.0);
    println(v.len_sq());
    v.scale(2.0);
    println(v.x + v.y);
}
```

**A stack** (milestone 8). Prints `3` then `2` then `empty` on three lines, exits with 0.

```
fun main() {
    let mut stack: i64[] = [];
    stack.push(1);
    stack.push(2);
    stack.push(3);
    match stack.pop() {
        Some(x) => println(x),
        None => println("empty"),
    }
    println(stack.len);
    stack.clear();
    match stack.pop() {
        Some(x) => println(x),
        None => println("empty"),
    }
}
```

A rejected program for milestone 6: the checker reports E0417 at the `match`, listing `Empty`:

```
enum Shape { Circle(f64), Empty }

fun area(s: Shape): f64 = match s {
    Circle(r) => r * r,
};

fun main() {}
```

## §11 Milestones — additions

| # | Milestone | Spec subset | Done when |
| --- | --- | --- | --- |
| 6 | Enums | `enum`, construction, `match` with flat patterns, exhaustiveness (E0417, W0102), `Option<T>`, recursive enums on the heap, enum copies | The shapes and linked-list programs pass, and the rejected program reports E0417 |
| 7 | Methods | `impl`, `self`/`mut self`, associated functions, method calls, E0310/E0415, `lugha_fn_<Type>.<name>` symbols | The methods program passes |
| 8 | Growable arrays | The `{ len, cap, elems }` layout, `push`/`pop`/`clear`, `lugha_rt_array_grow`, E0507 for growth during `for … of` | The stack program passes; `lughac spec` prints the full v1 specification |

**Order:** methods come before growable arrays, because `xs.push(v)` uses the method-call machinery, and `pop` needs `Option` from milestone 6.

**Out of scope for v1** (for v2, roughly in order): user generics, modules and imports, closures, nested patterns and guards, `==` on enums and structs, string interpolation, bitwise operators, more integer and float types, and a swap or move operation for buffers (DECISION-010).

## §12 Open questions — additions

- [ ] **Literal patterns on `f64`.** They are excluded: float equality in patterns invites mistakes. Revisit if needed.
- [ ] **`match` on `Option<T>` payloads holding arrays.** Bindings are borrowed; does any program need `mut` bindings? Leaning no for v1.
- [ ] **Array growth factor.** Doubling with a minimum of 4 is proposed. Measure on v1 programs before freezing it.
- [ ] **`impl` on enums for `Option`.** Methods on `Option<T>` (for example `unwrap_or`) would need generic methods. Excluded from v1; `match` covers it.
