# Lugha v3 — Draft (delta against v2)

**Status:** approved 2026-10-13 — session 47
**Base:** `Language v2 Specification.md`, complete (milestones 9–12). It stays the source of truth, and what `lughac spec` prints, until v3 is implemented. Each v3 milestone folds its sections into a new `Language v3 Specification.md`; v2 is then frozen, as v0 and v1 were.

This draft lists only what changes, in the order of the v2 sections. Everything not mentioned keeps its v2 meaning. Every valid v2 program stays valid in v3 unless it:
- has a `{` or `}` in a string literal (write `{{` and `}}`; the compiler says so, E0111);
- defines a function named `args`, `parse_i64` or `parse_f64`, which become intrinsics (E0302).

## Decisions behind this draft (discovery, session 47)

1. **Headline:** closures. v3 adds function types, functions as values, and closures that capture variables. Packages, interface values, `Self`, bitwise operators, more integer types and the `match` additions wait for a later version.
2. **Capture by copy, read-only:**
   - A closure copies each outer variable it mentions when it is created, by the copy rules of §4 (arrays deep-copied, or moved on last use).
   - It can read its copies but never assign them (E0511).
   - The caller's variables never change through a closure.

   This keeps v2's value semantics: there is still no shared reference.
3. **`fun` everywhere:** the type is `fun(i64, i64): bool`, and the literal is `fun(a, b) = a < b` or `fun(x: i64): i64 { … }`, a declaration without a name. Parameter types may be left out when an expected function type gives them. Named functions are values too.
4. **Practical additions:** `args()` for command-line arguments, `parse_i64`/`parse_f64` returning `Option`, and string interpolation `"{name} is {age}"` in every string literal (`{{` for a brace).
5. **Higher-order helpers** are built-in array methods, next to `push`/`pop`: `map`, `filter`, `fold`, `sort_by`, `any`, `all`. Each returns a new value and never changes the receiver.

## §1 Overview — changes

- **Non-goals** drop closures; they gain packages, interface values and shared references (see §11).

## §2 Lexical structure — changes

**String interpolation.**
- **Inside a string literal,** `{` starts an interpolation, which runs to the matching `}`. `{{` and `}}` are literal braces.
- **Errors (E0111):**
  - a lone `}`, an unclosed `{`, or an empty `{}`;
  - a string literal or a raw line break inside an interpolation (keeping the lexer simple; build such strings in a variable first).
- **Lexing:** the lexer emits a string as its literal parts and interpolated token runs. `lughac build --emit=tokens` shows them.

## §3 Grammar — changes

```ebnf
base_type    = … | "fun" "(" [ type { "," type } [ "," ] ] ")" [ ":" type ] ;   (* a function type *)

primary      = … | closure | interpolated ;
closure      = "fun" "(" [ cparam { "," cparam } [ "," ] ] ")" [ ":" type ] ( block | "=" expr ) ;
cparam       = IDENT [ ":" type ] ;
interpolated = STRING_START { expr STRING_MID } expr STRING_END ;   (* "a {x} b {y} c" *)

postfix      = primary { call | index | member } ;   (* unchanged: a call may follow any postfix, `make()(3)` *)
```

- **Closure bodies:** `fun(x) = e` takes the longest expression, so `sort_by(xs, fun(a, b) = a > b)` ends at the `)`. A closure in statement position needs `;` like any expression.
- **Calling a field:** `s.f(x)` is always a method call. To call a field of function type, write `(s.f)(x)`.
- **New parse error E0209:** a closure parameter list that isn't plain names with optional types (for example `fun(self) = …`, or a pattern).

## §4 Type system — changes

**Function types.** `fun(A, B): R` is the type of a function value taking `A` and `B` and returning `R`; `fun(A)` returns nothing. Two function types are the same when their parameter and return types are.
- **Equality:** function values have no `==` (E0404), and neither does a struct, enum, array or `Option` containing one. So `T: Eq` now restricts: a function type doesn't satisfy `Eq` (E0419).
- **Order and printing:** function values have no `<`, can't be printed, and can't be cast.
- **Copying:** a function value is immutable, so copies share it, as enum payloads are shared.
- **Generics and interfaces:** a function type can be a type argument (`map<T, U>(xs: T[], f: fun(T): U)`), a field, an element or a payload. It has no methods, so it satisfies only an interface with no items.

**Functions as values.**
- **Named functions:** a non-generic function or associated function used without a call is a value of its function type: `let f = double;`, `apply(xs, Circle.make)`.
- **Generic functions:** a generic function used as a value needs an expected function type to fix its type arguments (`let m: fun(i64, i64): i64 = max;`); otherwise it is E0412.
- **Not values:** methods with a receiver (write `fun(c) = c.area()`), externs, and the intrinsics. Using one as a value is E0406, with help.

**Closures.**
- **Types:**
  - A closure's parameter types are written, or come from an expected function type in the same position. Otherwise it is E0412 "cannot infer the type of parameter `x`", with help to annotate it.
  - Its return type is written, or the expected one, or the body's type.
- **Captures:**
  - A closure captures every local, parameter or `self` of an enclosing function that its body mentions.
  - Each capture is copied when the closure is created, following the §4 copy table (moved on last use).
  - Assigning a captured variable, or one of its fields or elements, is **E0511** "can't assign `count`: closures capture by copy", with help to return a value instead.
  - A closure may declare and change its own `let mut` locals as usual.
- **No recursion:** a closure has no name, so it can't call itself. Use a named function.

**Calls.** `e(args)` calls any expression `e` of function type: a local, a parameter, a call's result, `(s.f)`. Calling a value that isn't a function remains E0406. A local of function type shadows a function of the same name, as any local does.

**Built-in array methods.** In addition to `push`, `pop` and `clear`, every `T[]` has these. Each takes `self`: the receiver is never changed, and each result is a new array.

| Method | Type | Result |
| --- | --- | --- |
| `xs.map(f)` | `f: fun(T): U` | `U[]`, `f` applied to each element in order |
| `xs.filter(f)` | `f: fun(T): bool` | `T[]`, the elements for which `f` is true, in order |
| `xs.fold(init, f)` | `init: A`, `f: fun(A, T): A` | `A`, `f(…f(f(init, xs[0]), xs[1])…)` |
| `xs.sort_by(less)` | `less: fun(T, T): bool` | `T[]`, sorted so that `less` holds between neighbours or neither way; stable |
| `xs.any(f)` / `xs.all(f)` | `f: fun(T): bool` | `bool`, stopping at the first element that decides |

A closure that panics stops the program as usual.

## §5 Expressions and statements — changes

- **Evaluation order:**
  - A closure literal evaluates to a value: its captures are copied, left to right in order of first mention, when the literal is evaluated. Its body runs only when it is called.
  - In `e(args)`, `e` is evaluated before the arguments.
- **Interpolated strings:** each `{expr}` is evaluated left to right, and its value printed as `print` would. Allowed types are numbers, `bool` and `string`. Anything else is E0403, with help to convert it first. The result is a new `string`.
- **Intrinsics:** these join the built-in names, and defining a function with one of these names is E0302.
  - `args(): string[]` returns the program's command-line arguments; `args()[0]` is the program's path.
  - `parse_i64(s: string): Option<i64>` accepts optional `-` then decimal digits, with no spaces or `_`. It returns `None` on anything else, including overflow.
  - `parse_f64(s: string): Option<f64>` accepts the float literal forms of §2, plus integers. It returns `None` otherwise.

## §6 Functions and program structure — changes

- **Function values across modules:** `m.f` without a call is a value, following visibility (E0313).
- **`main`:** unchanged. Arguments come through `args()`, not parameters.

## §7 Memory model — changes

- **A function value** is a pair `{ ptr code, ptr env }`, 16 bytes, passed and stored by value.
  - A closure's environment is one GC heap object, allocated with `lugha_rt_alloc` when the closure is created, holding its copied captures laid out like a struct.
  - A named function's value has a null environment.
- **Environments are never mutated** after creation, so sharing them is safe.

## §8 C interop — changes

- Function types can't appear in extern signatures (E0409). C can't call a Lugha closure.

## §9 Compilation model — changes

**Symbols.** Architecture rule 7 gains two prefixes:

| Item | Symbol |
| --- | --- |
| Closure body | `lugha_closure_<enclosing symbol>.<n>`, `n` counting closures in that function in source order, from 0: `lugha_closure_main.0` |
| Function used as a value | `lugha_ref_<symbol>`, a wrapper taking the hidden environment pointer and calling `lugha_fn_<symbol>` |

**Lowering notes.**
- **Calling a function value:** pass its environment pointer as a hidden first argument to its code pointer. Aggregates are passed by pointer, as for ordinary calls.
- **Monomorphization:** each instance compiles its own closures; a closure's captured types are concrete there.
- **Array methods** are emitted inline, like `push`, with bounds-free loops over the receiver. `sort_by` is a stable merge sort into a new array, allocated through `lugha_rt_alloc`.
- **Interpolated strings** lower to the same formatting the print intrinsics use, concatenated into one string.

**Runtime.** It gains:
- `lugha_rt_args()`, filled by the C `main` from `argc`/`argv`;
- `lugha_rt_parse_i64` and `lugha_rt_parse_f64`, which return a success flag plus the value;
- string formatting helpers for interpolation.

**New error codes.** The numbers are proposed; each becomes final when its milestone's PRP is approved.

| Code | Error | Example |
| --- | --- | --- |
| E0111 | Bad interpolation in a string literal | `"a } b"`, `"{x"`, `"{}"`, `"{f("s")}"` |
| E0209 | Bad closure parameter list | `fun(self) = 1` |
| E0511 | Assigning a captured variable | `let inc = fun() { count += 1; };` |

**Reused codes:**
- E0302: the new intrinsic names.
- E0313: a private function used as a value.
- E0403: interpolating a value that isn't printable.
- E0404: `==` on function values.
- E0406: a method, extern or intrinsic used as a value; calling a non-function.
- E0409: function types in externs.
- E0412: a closure parameter's type, or a generic function value, not inferred.
- E0419: a function type for a `T: Eq` parameter.

## §10 Example programs — additions (v3 acceptance tests)

**Arguments and interpolation** (milestone 13). Run as `prog 21`, prints `21 doubled is 42`; run with no number, prints `usage: prog <n>`; exits with 0.

```
fun main() {
    let a = args();
    if a.len < 2 {
        println("usage: prog <n>");
        return;
    }
    match parse_i64(a[1]) {
        Some(n) => println("{n} doubled is {n * 2}"),
        None => println("usage: prog <n>"),
    }
}
```

**Functions as values** (milestone 14). Prints `6` then `9` on two lines, exits with 0.

```
fun double(x: i64): i64 = x * 2;
fun triple(x: i64): i64 = x * 3;

fun apply(f: fun(i64): i64, x: i64): i64 = f(x);

fun main() {
    let fs = [double, triple];
    println(apply(fs[0], 3));
    println(apply(fs[1], 3));
}
```

**Closures** (milestone 15). Prints `13` then `1` then `20` on three lines, exits with 0.

```
fun adder(k: i64): fun(i64): i64 = fun(x) = x + k;

fun main() {
    let add10 = adder(10);
    println(add10(3));
    let mut count = 0;
    let show = fun(): i64 = count + 1;
    count = 5;
    println(show());
    let twice = fun(f: fun(i64): i64, x: i64): i64 = f(f(x));
    println(twice(fun(y) = y + 5, 10));
}
```

**Array helpers** (milestone 16). Prints `ann, cy` then `61` then `cy` on three lines, exits with 0.

```
struct Person { name: string, age: i64 }

fun main() {
    let people = [Person { name: "ann", age: 30 }, Person { name: "bo", age: 12 }, Person { name: "cy", age: 31 }];
    let adults = people.filter(fun(p) = p.age >= 18);
    let names = adults.map(fun(p) = p.name);
    println(names.fold("", fun(acc, n) = if acc == "" { n } else { "{acc}, {n}" }));
    println(adults.fold(0, fun(sum, p) = sum + p.age));
    println(people.sort_by(fun(a, b) = a.age > b.age)[0].name);
}
```

## §11 Milestones — additions

| # | Milestone | Adds | Done when |
| --- | --- | --- | --- |
| 13 | Input and interpolation | `args()`, `parse_i64`, `parse_f64`, string interpolation (E0111) | The arguments program passes |
| 14 | Functions as values | function types, named functions as values, calling values, no `==` on functions, `lugha_ref_*` | The functions-as-values program passes |
| 15 | Closures | closure literals, capture by copy (E0511), parameter inference, `lugha_closure_*`, closures in generic code | The closures program passes |
| 16 | Array helpers | `map`, `filter`, `fold`, `sort_by`, `any`, `all` | The array-helpers program passes, and the v3 spec is complete |

**Order:**
- **M13 first:** it is independent and small, and every later example uses interpolation.
- **M14 before M15:** function values without captures settle the type, the call and the value layout. Closures then only add environments.
- **M16 last:** the helpers are the first big user of closures.

**Out of scope for v3** (for v4, roughly in order):
- packages (a manifest, dependencies, search paths) with `public` for their API, re-exports, API-change checking;
- interface values and dynamic dispatch, `Self`, generic interfaces, default methods;
- a shared reference type, and closures that change their captures;
- bitwise operators, `u32`/`u64`/`f32`, wrapping arithmetic;
- or-patterns and `mut` bindings in `match`;
- explicit type arguments, `.len()`, selective imports, operator overloading.

## §12 Open questions — additions

- [ ] **Printing function values:** proposed, never (E0403). There's nothing useful to show.
- [ ] **Closures in interpolations:** `"{f(x)}"` is allowed (a call is an expression). A closure literal inside `{…}` would need braces inside braces; proposed, allowed only if the lexer can match nesting simply, else E0111.
- [ ] **`sort_by` with an inconsistent `less`:** the result is some order of the elements, never a crash or lost element; documented rather than checked.
- [ ] **Environment layout for empty captures:** a closure capturing nothing could reuse the null environment. Proposed: yes, as an optimization with no visible effect.
