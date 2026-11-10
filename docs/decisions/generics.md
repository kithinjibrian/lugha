# Lugha Generics and Interfaces

Design note by the project owner, adopted for v2 in session 43 (2026-10-13), with the adaptations below. Generics are milestone 11 as already approved, and interfaces become milestone 12. The amended rules are in `docs/specs/Language v2 Draft.md` (decisions 3–5 and 10, §2, §3, §4, §9, §10, §11).

## Adaptations for v2

- **Milestones:** the note says "v1, milestone 7", but Lugha's milestone 7 was methods. Generics are v2 milestone 11, exactly as the approved draft says: `Eq`/`Ord` bounds, inference, checked once, monomorphized. Interfaces are v2 milestone 12.
- **Kept from the note:**
  - interfaces as bounds only, satisfied structurally by matching methods (visibility applies);
  - checking at the definition, and monomorphization;
  - errors that suggest the missing bound.
- **`==` and `<` stay as they are:** `==` is deep on every type (milestone 9), and `Eq`/`Ord` remain built-in bounds. User types don't get operators through `eq`/`lt` methods: there is no operator overloading in v2.
- **Left out of v2:**
  - **Explicit type arguments** (`parse<i64>(s)`): inference only, and you annotate the binding.
  - **`.len` → `.len()`:** it would break every program, and it only matters once built-in types can satisfy interfaces.
  - **`Self` in interface signatures:** an open question for v3, together with generic interfaces.
  - **`impl<T> T[]` and methods on built-in types.**
  - **`comptime` and `Task<T>`:** not Lugha features.
  - **Interface values and dynamic dispatch.**
- **Syntax:**
  - `impl Pair<A, B>` (the draft's form) rather than `impl<A, B> Pair<A, B>`;
  - bounds are written on the declaration's type parameters, as the draft says.
- **Error codes:**
  - the note's E0420 is taken (generic instantiation too deep);
  - "a method no bound provides" is E0421;
  - "a type argument doesn't satisfy an interface" is E0422.

---

## Summary

Generic functions, structs and `impl` blocks take type parameters. Interfaces describe the methods a type parameter must have, and any type with those methods satisfies them, with no declaration needed. Generic code is type-checked once, at its definition, and compiled separately for each set of type arguments it is used with.

```
interface Shape {
    fun area(self): f64;
}

fun total_area<T: Shape>(shapes: T[]): f64 {
    let mut sum = 0.0;
    for s of shapes { sum += s.area(); }
    sum
}

fun largest<T: Ord>(xs: T[]): Option<T> { ... }

struct Pair<A, B> { first: A, second: B }

impl<A, B> Pair<A, B> {
    fun swap(self): Pair<B, A> = Pair { first: self.second, second: self.first };
}
```

## Principles

1. **Monomorphization.** Each distinct instantiation is compiled to its own concrete code. There is no runtime type information and no boxing, so LLVM lowering, array copy functions and the garbage collector need no changes.
2. **Checked at the definition.** A generic body may only use what its bounds provide. Mistakes in the body are reported once, in the body, not at every use.
3. **Structural interfaces.** A type satisfies an interface if it has matching methods.
4. **Interfaces are bounds only.** In v1 they constrain type parameters at compile time. There are no interface-typed values and no dynamic dispatch.
5. **Generics introduce types; comptime computes with them.** Reflection with `comptime for` works inside generic functions, and is the one place where checking happens at instantiation (v1 spec, section 5).

## Grammar additions

```ebnf
item           = fun_decl | extern_decl | struct_decl | impl_decl | interface_decl ;

fun_decl       = "fun" IDENT [ type_params ] "(" [ params ] ")" [ ":" type ] fun_body ;
struct_decl    = "struct" IDENT [ struct_params ] "{" [ field { "," field } [ "," ] ] "}" ;
impl_decl      = "impl" [ type_params ] type "{" { method } "}" ;
interface_decl = "interface" IDENT "{" { iface_item } "}" ;
iface_item     = "fun" IDENT "(" [ receiver [ "," params ] | params ] ")" [ ":" type ] ";" ;

type_params    = "<" type_param { "," type_param } ">" ;
type_param     = IDENT [ ":" IDENT { "+" IDENT } ] ;
struct_params  = "<" IDENT { "," IDENT } ">" ;          (* no bounds on structs *)

type           = ( base_type [ type_args ] | "Self" ) { "[" "]" } ;
type_args      = "<" type { "," type } ">" ;

primary        = ... | IDENT type_args ( call | struct_init ) ;
```

`interface` and `Self` become keywords.

**Nested type arguments.** `Pair<Pair<i64, i64>>` ends in `>>`. Lugha has no shift operators yet, so the lexer never produces a `>>` token today. When bitwise operators arrive, the parser must split `>>` into two `>` tokens in type position, as Rust and C++ do.

## Type parameters and bounds

- **Functions and `impl` blocks** declare type parameters with optional bounds: `fun f<T: Shape + Eq>(...)`, `impl<T: Ord> Set<T> { ... }`.
- **Struct definitions** declare type parameters without bounds. A bound on a struct would have to be repeated on every function that mentions the struct, which Rust's experience shows is mostly noise. Put the bound where it is used: on the `impl` block or function.
- **Methods on arrays** use `impl<T> T[] { ... }`. The prelude defines array methods this way.

**What a generic body can do with a `T`.** Copy it, assign it, pass it, return it, and store it in arrays and structs, under the usual value-semantics rules. Beyond that, only what the bounds provide:

| Needs | Provided by |
| --- | --- |
| A method call, `x.area()` | An interface bound declaring that method |
| An associated function, `T.zero()` | An interface bound declaring it |
| `==`, `!=` | The `Eq` bound |
| `<`, `<=`, `>`, `>=` | The `Ord` bound |

Field access, arithmetic, numeric literals and printing are not available on an unbounded `T`. `let z: T = 0;` is an error, because `0` cannot have an arbitrary type.

## Interfaces

An interface lists method signatures. It may use `Self` for the type that satisfies it, and may require associated functions (methods without a receiver).

```
interface Zero {
    fun zero(): Self;
}

interface Add {
    fun add(self, other: Self): Self;
}

fun sum<T: Zero + Add>(xs: T[]): T {
    let mut total = T.zero();
    for x of xs { total = total.add(x); }
    total
}
```

**Satisfaction.** A type `T` satisfies interface `I` if, for every item `I` declares, `T` has a method or associated function with the same name, the same receiver kind, and the same parameter and return types after replacing `Self` with `T`. A `self` requirement is met only by a `self` method, and a `mut self` requirement only by a `mut self` method. A method counts only where it is visible (see the visibility note).

**Built-in interfaces**

| Interface | Requires | Enables | Satisfied automatically by |
| --- | --- | --- | --- |
| `Eq` | `fun eq(self, other: Self): bool` | `==`, `!=` | Primitives, `string` |
| `Ord` | `Eq`, plus `fun lt(self, other: Self): bool` | `<`, `<=`, `>`, `>=` | Primitives, `string` |

The compiler derives `!=`, `<=`, `>` and `>=` from `eq` and `lt`. User types get the operators by defining the methods; there is no other way to overload operators. `Ord` uses `lt` rather than a three-way comparison so it does not depend on enums. It can move to `cmp(self, other: Self): Ordering` once enums exist (milestone 8).

Arithmetic operators are not overloadable in v1. Generic numeric code can use interfaces like `Add` above with method syntax.

## Instantiation and inference

Type arguments are inferred, in this order:

1. **From arguments.** `total_area(circles)` gives `T = Circle` from the argument's type.
2. **From the expected type.** `let x: Option<i64> = None;` gives `T = i64` from the annotation. This extends v0's rule that numeric literals take their type from context, and applies to the same contexts: annotations, parameter types, declared return types, the other operand of an operator, and assignment targets.
3. **Explicitly**, when neither is enough: `parse<i64>(s)`.

A type argument that can't be inferred is a compile error asking for it to be written explicitly.

**Explicit type arguments and `<`.** Lugha's comparison operators are non-associative, so `a < b > (c)` is already a parse error. That means `name<...>` followed by `(` or `{` can only be a generic call or a generic struct literal, and no turbofish (`f::<i64>`) is needed. On `IDENT <`, the parser tries to read a type argument list; if it closes with `>` followed by `(` or `{`, it commits, and otherwise backtracks and parses a comparison.

One case is genuinely ambiguous: in an argument list, `g(f < a, b > (c))` could also be two comparisons. As in C#, the parser prefers the generic reading. Write `g((f < a), b > (c))` to mean the comparisons.

**Non-copyable types.** `Task<T>` cannot be passed, stored or copied, so it is never a valid type argument.

## Lowering

- **On demand.** Starting from `main` and every non-generic function, the compiler keeps a worklist of instantiations reached by calls and compiles each one once. Generic code that is never used produces no machine code, though it is still type-checked.
- **Names.** Each instantiation gets a mangled LLVM name, for example `lugha_total_area__Circle` or `lugha_Pair_swap__i64__string`.
- **Copy functions.** Each instantiated type that contains arrays gets its own deep-copy function, exactly as non-generic types do in v0.
- **Polymorphic recursion.** A generic function that calls itself with an ever-larger type, such as `T` then `Pair<T, T>`, would instantiate forever. The compiler stops after a fixed nesting depth with a compile error.
- **Code size.** Each instantiation is separate machine code, so heavily used generics grow the binary. That is the price of no runtime cost; LLVM merges identical functions at `-O2`.

## Diagnostics

Errors in a generic body point at the body, and suggest the bound that would fix them:

```
error[E0421]: `T` has no method `perimeter`
 --> src/shapes.lugha:9:21
  |
9 |     total += s.perimeter();
  |                ^^^^^^^^^ not provided by the bound `T: Shape`
  |
help: add an interface that declares `perimeter`, e.g. `T: Shape + Perimeter`
```

Unsatisfied bounds point at the use site and name each missing or mismatched method:

```
error[E0420]: `Square` does not satisfy `Shape`
  --> src/main.lugha:14:24
   |
14 |     println(total_area(squares));
   |                        ^^^^^^^ T = Square here
   |
note: `Shape` requires `fun area(self): f64`; `Square` has no method `area`
```

Error codes here are illustrative; they will be assigned within the E04xx type-error range.

## Migration from v0: `.len` becomes `.len()`

In v0, `.len` on arrays and strings is a built-in property. Interfaces are satisfied by methods, so a property can't satisfy `interface Sized { fun len(self): i64; }`. In v1, `len` becomes an ordinary prelude method on arrays and strings, so built-in and user-defined collections fit the same interfaces. The compiler recognizes the old `.len` form and reports an error with a fix-it suggesting `.len()`.

## Alternatives considered

| Alternative | Used by | Why not |
| --- | --- | --- |
| Generics through comptime type parameters | Zig | Errors appear at each instantiation, and signatures don't say what a type must support |
| Type erasure or dictionary passing | Java, Swift (by default) | Smaller binaries, but runtime cost, boxing, and runtime type information |
| Nominal traits with `impl Trait for Type` | Rust | Extra declarations; structural matching fits the TypeScript side of Lugha |
| Turbofish `f::<T>()` | Rust | Unnecessary, because non-associative comparisons remove the ambiguity |
| Bounds on struct definitions | Rust | Bounds spread to every function mentioning the struct |

## Open questions

- **Printing generic values.** A built-in `Show` interface (`fun to_string(self): string`) would let generic code print `T`. Whether the `print` intrinsics should accept any `T: Show`.
- **Generic interfaces.** Whether interfaces can take type parameters, as in `interface Into<T> { fun into(self): T; }`.
- **Interface values.** Dynamic dispatch (a `dyn Shape`-style type) for collections mixing different shape types. Not in v1.
- **Arithmetic operators.** Whether `+`, `-` and `*` should ever be overloadable through interfaces like `Add`.
- **Default type parameters.** `struct Map<K, V, H = DefaultHash>`. Probably unnecessary until there are real library types that need them.
