# Lugha v13 — Draft (delta against v12)

**Status:** proposed 2026-10-14 — session 90
**Base:** `Language v12 Specification.md`, complete (milestone 40). It stays the source of truth, and what `lughac spec` prints, until v13's first milestone starts `Language v13 Specification.md`. That file then grows milestone by milestone, and v12 is frozen.

This draft lists only what changes, in the order of the v12 sections. Everything not mentioned keeps its v12 meaning.

**Compatibility.** Every valid v12 program stays valid in v13, because everything v13 adds was an error before:
- `Self` in a struct or enum declaration was E0305;
- `for x of mut e` was a parse error;
- the new helper names were E0415 (a type's own method by that name still wins);
- a `:` at the top level of an interpolation was a parse error.

The prelude gains the name `Pair`, visible unless a module declares its own, as with `Range`.

## Decisions behind this draft (discovery, session 90)

1. **Headline: four conveniences**, one milestone each:
   - `Self` in declarations;
   - `for … of mut`;
   - more iterator helpers;
   - format specifiers.
2. **Format specifiers are Rust's core:** `[[fill]align][#][0][width][.precision][kind]` after a `:` in an interpolation. There is no `format()` function.
3. **The iterator helpers are the full set, and `zip` yields a prelude `Pair<A, B> { first, second }`:** `zip`, `chain`, `take_while`, `skip_while`, `flat_map`, `min`, `max`, `position`.
4. **`for x of mut e`:**
   - on an iterator, it advances `e` itself, so a later loop resumes it;
   - on an array, it makes `x` a mutable alias of each element.

**Choices made while writing this draft** (each settles a gap the answers left open):
- **`Self` in declarations:**
  - **Where:** in a struct's field types and an enum's payload types, `Self` is the type being declared, with its parameters: `struct Node<T> { value: T, kids: Self[] }`, `enum List { Cons(i64, Self), Nil }`.
  - **Recursion:** the usual rules apply, so a struct holding `Self` directly is still E0307.
  - **Interfaces and `impl`s** are unchanged: `Self` already means their type there, type patterns included (`c: Self`).
- **`for x of mut e` on an iterator:**
  - `e` must be a place rooted in a `let mut` local (E0501 otherwise), and is the iterator `next` is called on, not a copy;
  - the body may not assign to `e` or call its `mut self` methods (E0507), since the loop is using it.
- **`for x of mut xs` on an array:**
  - `xs` must be a place rooted in a `let mut` local (E0501);
  - `x` is the element itself: `x = v`, `x += 1`, `x.field = v` and `mut self` calls on `x` change `xs[i]`;
  - the body may not assign to `xs` itself or grow or shrink it (E0507), but may read it;
  - on a string it is E0506, since strings never change.
- **`flat_map(f)`:** `f` returns an iterator (`fun(T): I` with `I` yielding `U`); for an array, return `xs.iter()`. It yields every value of each result in turn.
- **`min()`/`max()`** need `T: Ord` (E0419 otherwise), and give the first smallest or largest value, or `None` for an empty iterator.
- **`position(f)`** counts from 0, like `enumerate`.
- **`chain(b)`:** `b` must yield the same type (E0403 otherwise).
- **`zip(b)`** may pair different types: `Pair<A, B>`. `Pair` is a prelude struct with public fields, so it prints as `Pair { first: 1, second: "a" }` and can be built by programs.
- **Format specifiers:**
  - **Where the spec starts:** the specifier follows the first `:` at the interpolation's top level, outside `()`, `[]`, `{}` and strings. An expression that needs such a `:` (an `if let c: T = …` inside an interpolation) goes in parentheses.
  - **Fill** is one ASCII character other than `{` or `}`, and needs an alignment after it. **Alignment** is `<` (left), `>` (right) or `^` (centre, extra space on the right). Without one, numbers align right and everything else left.
  - **Width** is decimal, at most 1024, and counts bytes. Text longer than the width is never cut.
  - **`0`** pads numbers with zeros after the sign or `0x`. It is ignored when an alignment is given, and on anything but numbers it is E0431.
  - **`.precision`** applies to `f32`/`f64` only: that many decimals, rounded half to even as C's `printf` does.
  - **`kind`** is `x`, `X`, `b` or `o`, for integers only. A negative value prints its two's complement at the type's width (`{-1 as i8:x}` gives `ff`). `#` adds `0x`, `0b` or `0o` and is allowed only with a kind.
  - **Everything else** is E0431: an unknown kind, precision on an integer, a kind on a float, `#` without a kind, a width over 1024, a non-ASCII fill, or an empty spec after `:`.
  - **Other values:** for anything but numbers, only fill, alignment and width apply, to the value's text: `{p:>20}` right-aligns a struct's text.

## §3 Grammar — changes

```ebnf
for_stmt     = "for" IDENT ( "in" range | "of" [ "mut" ] expr ) block ;
interpolated = STRING_START expr [ ":" FORMAT ] { STRING_MID expr [ ":" FORMAT ] } STRING_END ;
FORMAT       = [ [ fill ] align ] [ "#" ] [ "0" ] [ width ] [ "." precision ] [ kind ] ;
align        = "<" | ">" | "^" ;
kind         = "x" | "X" | "b" | "o" ;
```

The lexer splits the specifier off the interpolation; it is not an expression.

## §4 Type system — changes

- **`Self`** in struct and enum declarations (above).
- **`Pair<A, B>`:** `struct Pair<A, B> { public first: A, public second: B }` in the prelude.

## §5 Expressions and statements — changes

- **`for x of mut e`** (above).
- **Iterator helpers**, added to v10's table:

| Helper | Gives | Meaning |
| --- | --- | --- |
| `a.zip(b)` | yields `Pair<A, B>` | the values side by side, stopping at the shorter |
| `a.chain(b)` | yields `T` | `a`'s values, then `b`'s |
| `it.take_while(f)` | yields `T` | values while `f` holds, then stops |
| `it.skip_while(f)` | yields `T` | values from the first where `f` fails |
| `it.flat_map(f)`, `f: fun(T): I` | yields `U` | each value's iterator, flattened |
| `it.min()`, `it.max()` | `Option<T>` | the first smallest / largest (`T: Ord`) |
| `it.position(f)` | `Option<i64>` | the index of the first value `f` holds for |

- **Format specifiers:** `"{x:spec}"` (above). `"{pi:.2}"` gives `3.14`, `"{n:>6}"` gives `    42`, `"{n:05}"` gives `-0042` for −42, `"{n:#x}"` gives `0xff`, and `"{name:*^9}"` gives `***ann***`.

## §9 Compilation model — changes

- **`Self` in declarations** is replaced by the parser, as in `impl` blocks (v10).
- **`for x of mut e`:**
  - over an iterator, it calls `next` on `e`'s own address;
  - over an array, it binds `x` to the element's address, as a place.
- **The new helpers** are Lugha in the prelude, compiled only when reached.
- **Format specifiers** lower to new runtime functions:
  - `lugha_rt_fmt_int(value, bits, signed, spec…)` for integers;
  - `lugha_rt_fmt_float(value, f32, spec…)` for floats;
  - `lugha_rt_pad(text, spec…)` for other values.

  Each takes the fill, alignment, width, precision, kind and flags as plain arguments.
- **Programs using none of this** give byte-identical IR.

**New error code.** The number is proposed, and becomes final when its milestone's PRP is approved.

| Code | Error | Example |
| --- | --- | --- |
| E0431 | A bad format specifier | `"{n:.2}"` with `n: i64`; `"{x:q}"`; `"{f:x}"`; `"{n:#}"`; `"{s:05}"`; `"{n:99999}"` |

**Reused codes:**
- **E0501:** `for … of mut` over something not rooted in a `let mut`.
- **E0506:** `for … of mut` over a string.
- **E0507:** changing what a `for … of mut` loop is using.
- **E0419:** `min`/`max` on a type that isn't `Ord`.

## §10 Example programs — additions (v13 acceptance tests)

**`Self` in declarations** (milestone 41). Prints `3` then `Cons(1, Cons(2, Nil))`, exits with 0.

```
struct Tree<T> { value: T, kids: Self[] }

impl Tree<T> {
    fun size(self): i64 {
        let mut n = 1;
        for k of self.kids {
            n += k.size();
        }
        n
    }
}

enum List { Cons(i64, Self), Nil }

fun main() {
    let t = Tree { value: 1, kids: [Tree { value: 2, kids: [] }, Tree { value: 3, kids: [] }] };
    println(t.size());
    println(List.Cons(1, List.Cons(2, List.Nil)));
}
```

**`for … of mut`** (milestone 42). Prints `[2, 4, 6]`, `a b`, `c d`, exits with 0.

```
fun main() {
    let mut xs = [1, 2, 3];
    for x of mut xs {
        x *= 2;
    }
    println(xs);
    let mut words = ["a", "b", "--", "c", "d"].iter();
    let mut first = "";
    for w of mut words {
        if w == "--" {
            break;
        }
        first = first + w + " ";
    }
    println(first.trim());
    let mut rest = "";
    for w of mut words {
        rest = rest + w + " ";
    }
    println(rest.trim());
}
```

**More iterator helpers** (milestone 43). Prints `[Pair { first: 1, second: "a" }, Pair { first: 2, second: "b" }]`, `[1, 2, 3, 4]`, `[1, 2]`, `[3, 1]`, `[0, 0, 1, 0, 1, 2]`, `Some(1)`, `Some(9)`, `Some(2)`, exits with 0.

```
fun main() {
    let xs = [1, 2, 3, 1, 9];
    println(xs.iter().zip(["a", "b"].iter()).collect());
    println([1, 2].iter().chain([3, 4].iter()).collect());
    println(xs.iter().take_while(fun(n) = n < 3).collect());
    println(xs.iter().skip_while(fun(n) = n < 3).take(2).collect());
    println(range(1, 4).flat_map(fun(n) = range(0, n)).collect());
    println(xs.iter().min());
    println(xs.iter().max());
    println(xs.iter().position(fun(n) = n == 3));
}
```

**Format specifiers** (milestone 44). Prints `3.14`, `|    42|`, `|42    |`, `-0042`, `0xff`, `***ann***`, `11111111`, exits with 0.

```
fun main() {
    let pi = 3.14159;
    let n = 42;
    println("{pi:.2}");
    println("|{n:6}|");
    println("|{n:<6}|");
    println("{-42:05}");
    println("{255:#x}");
    println("{"ann":*^9}");
    println("{-1 as i8:b}");
}
```

## §11 Milestones — additions

| # | Milestone | Adds | Done when |
| --- | --- | --- | --- |
| 41 | `Self` in declarations | `Self` in struct fields and enum payloads | The `Self`-in-declarations program passes |
| 42 | `for … of mut` | iterators advanced in place; array elements as mutable aliases | The `for … of mut` program passes |
| 43 | More iterator helpers | `zip` (`Pair`), `chain`, `take_while`, `skip_while`, `flat_map`, `min`, `max`, `position` | The more-helpers program passes |
| 44 | Format specifiers | `"{x:spec}"`, E0431 | The format-specifiers program passes, and the v13 spec is complete |

**Order:** smallest first. M41 is a parser change; M42 touches the loops; M43 is mostly prelude; M44 touches the lexer, checker, codegen and runtime.

**Out of scope for v13** (for later, roughly in order):
- a `format()` function, a debug form, and parsing values from text;
- tuples;
- downloading packages, whole-module re-exports and import aliases;
- unsigned indexes; Unicode text; binary I/O.

## §12 Open questions — additions

- [ ] **Width counts bytes:** text with multi-byte characters pads as if it were longer. This waits for a decision on Unicode text.
- [ ] **Format specifiers at run time:** a spec must be written in the string; a `format(spec, x)` function could come later.
