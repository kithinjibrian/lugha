# Lugha v2 — Draft (delta against v1.2)

**Status:** approved 2026-10-12 — session 36
**Base:** `Language v1 Specification.md` (v1.2). It stays the source of truth, and what `lughac spec` prints, until v2 is implemented. Each v2 milestone folds its sections into a new `Language v2 Specification.md`; v1 is then frozen, as v0 was.

This draft lists only what changes, in the order of the v1 sections. Everything not mentioned keeps its v1.2 meaning. Every valid v1.2 program stays valid in v2 unless it:
- uses `import`, `private` or `public` as a name; or
- defines something named `Eq` or `Ord`.

## Decisions behind this draft (discovery, session 36)

1. **Scope:** v2 adds:
   - **richer patterns:** nested patterns, `if` guards, `==` on every type;
   - **modules:** several files, `import`, `private`;
   - **generics:** generic functions, structs, enums and methods.

   Closures and shared references wait for v3.
2. **Form:** a delta draft. v1.2 stays authoritative until v2 ships.
3. **Bounds:** generic code can store, move and copy a type parameter `T`. Two built-in bounds add operators: `T: Eq` allows `==`/`!=` and `T: Ord` allows `<`, `<=`, `>`, `>=`. There are no user-defined interfaces.
4. **Type arguments are inferred, never written at a use:** they come from the arguments and the expected type, like `None` and `[]` today. When those don't decide, it's an error, and you annotate the binding. `<…>` appears only in declarations and type positions, so there's no `::<>` and no parser ambiguity with `<`.
5. **Checking:** a generic body is checked once, with its type parameters abstract. It may use only what its bounds allow, so every instantiation compiles. Codegen then monomorphizes.
6. **Equality and order:**
   - `==` compares any two values of the same type deeply: structs field by field, enums by variant and then payloads, arrays by length and then elements. So every type satisfies `Eq`.
   - `<` stays on numbers and gains strings, compared bytewise. So `Ord` is the numbers and `string`.
7. **Modules:**
   - **Files:** a file is a module, named by its path from the main file's directory (`shapes/circle.la` is `shapes.circle`).
   - **Imports:** `import shapes.circle;` binds the name `circle`, and items are always used qualified (`circle.area(c)`). There are no selective or glob imports.
   - **Cycles:** import cycles are allowed.
   - **Visibility (amended in session 40, see 9):** everything is visible to the whole program unless it is marked `private`.
8. **Milestones:** M9 patterns and equality, then M10 modules, then M11 generics.
9. **Visibility (amendment, session 40;** replaces the per-item `pub` of answer 7; rationale in `docs/decisions/visibility.md`**):**
   - **Default:** no keyword means visible to every module in the program. v2 has no packages, so the program is the whole package.
   - **`private`:** a function, extern, struct, enum, method, associated function or struct field marked `private` is visible only in its own module.
     - A struct with any `private` field can't be built with a literal outside its module, so a type can protect its invariants (`Range.new` keeps `lo <= hi`).
     - Variants share their enum's visibility.
   - **`public`:** reserved for packages (v3), where it will mean "visible to other packages". In v2 it is an error.
10. **Interfaces (amendment, session 43;** a new milestone 12 after generics; rationale in `docs/decisions/generics.md`**):**
    - **What:** an `interface` lists method signatures. It is used only as a bound, `T: Shape`, so generic code can call those methods on a `T`.
    - **Structural:** any type whose visible methods match satisfies the interface; there is no `impl Shape for`.
    - **Unchanged:** `Eq` and `Ord` stay built-in bounds, and `==` stays deep for every type. There is no operator overloading.
    - **Not in v2:** interface-typed values, `Self` in signatures, generic interfaces, explicit type arguments, and `.len()` as a method.

## §1 Overview — changes

- **Non-goals for v2:** closures and function values, shared or reference types, interface values and dynamic dispatch, packages and search paths. See §11.
- **Design principles:** unchanged. Generics are inferred and checked once. Modules use the same `.` as fields and variants, so there is still one way to write each thing.

## §2 Lexical structure — changes

- **New keywords:** `import`, `private`, `public` (`public` is reserved for v3 packages; see §6), `interface` (milestone 12).
- **Built-in names:** `Eq` and `Ord` join `Option`, `Some` and `None` as built-in names. They are reserved like the intrinsics and can't be redefined (E0302).
- **`<` and `>`:** no lexer change. In type positions they also bracket type arguments, which is unambiguous because Lugha has no `>>` operator.

## §3 Grammar — changes

```ebnf
program      = { import } { item } EOF ;
import       = "import" IDENT { "." IDENT } ";" ;
item         = [ visibility ] ( fun_decl | extern_decl | struct_decl | enum_decl | interface_decl ) | impl_decl ;
interface_decl = "interface" IDENT "{" { iface_item } "}" ;        (* milestone 12 *)
iface_item   = "fun" IDENT "(" [ receiver [ "," params ] | params ] ")" [ ":" type ] ";" ;
visibility   = "private" | "public" ;                  (* `public`: an error until v3 *)

fun_decl     = "fun" IDENT [ type_params ] "(" [ params ] ")" [ ":" type ] fun_body ;
struct_decl  = "struct" IDENT [ type_params ] "{" [ field { "," field } [ "," ] ] "}" ;
field        = [ visibility ] IDENT ":" type ;
enum_decl    = "enum" IDENT [ type_params ] "{" … "}" ;              (* variants as in v1 *)
impl_decl    = "impl" type_name [ "<" IDENT { "," IDENT } ">" ] "{" { [ visibility ] method } "}" ;
method       = "fun" IDENT [ type_params ] "(" … ")" [ ":" type ] fun_body ;
type_params  = "<" type_param { "," type_param } [ "," ] ">" ;
type_param   = IDENT [ ":" bound { "+" bound } ] ;
bound        = "Eq" | "Ord" | type_name ;         (* an interface, `m.Shape` from another module *)

type_name    = IDENT [ "." IDENT ] ;                (* module.Type *)
base_type    = "i32" | "i64" | "u8" | "f64" | "bool" | "string"
             | type_name [ "<" type { "," type } [ "," ] ">" ] ;    (* Option<T> is now an ordinary case *)

primary      = … | type_name struct_init | … ;      (* geometry.Vec2 { x: 1.0, y: 2.0 } *)

arm          = pattern [ "if" expr ] "=>" expr ;
pattern      = "_"
             | IDENT [ "(" pattern { "," pattern } [ "," ] ")" ]
             | [ "-" ] INT | STRING | "true" | "false" ;
```

- **Imports come first.** An `import` after any item is E0208.
- **Generic headers.** `type_params` follow the name, like Rust and TypeScript: `fun max<T: Ord>(a: T, b: T): T`. An `impl` names the type's parameters without bounds, `impl Stack<T> { … }`; the bounds come from the type's declaration.
- **Nested patterns.** A payload slot is now a whole `pattern`, not just a name or `_`. A bare `IDENT` in a slot is a variant if the slot's type is an enum with a variant of that name (`Some(Empty)`), and otherwise a new binding (`Some(x)`). At the top level, a bare `IDENT` must still be a variant.
- **Guards.** `pattern if expr => value`. The guard is parsed like a `cond`, in no-struct-literal mode.
- **New parse error:** E0208 (`import` after an item). E0207 still rejects `::`, which also covers `Stack::<i64>`.

## §4 Type system — changes

**Equality on every type.** `==` and `!=` accept two values of any one type:
- **Primitives and strings:** as before.
- **Structs:** field by field.
- **Enums:** same variant, then payload by payload.
- **Arrays:** same `.len`, then element by element.
- **Option:** `None == None`, and `Some(a) == Some(b)` when `a == b`.

Equality is deep and costs time proportional to the values compared. `f64` keeps IEEE semantics, so a `NaN` anywhere makes the values unequal. The operator table's `==` row becomes "two values of the same type". The v1 sentences "Structs and arrays have no `==`" and "There is no `==` or other operator on enums" are removed.

**String order.** `<`, `<=`, `>` and `>=` also accept two strings and compare them bytewise. The first differing byte decides, compared as an unsigned value, and a proper prefix is smaller (`"ab" < "abc"`). This is the order of C's `memcmp` and needs no Unicode tables.

**Generic declarations.** `fun`, `struct`, `enum` and methods can take type parameters.
- **Scope:** a type parameter is a type inside its declaration (fields, payloads, signature and body), and a method also sees its `impl`'s parameters. It may not be named like a type, a module or another parameter of the same declaration (E0316).
- **Bounds:** `T: Eq`, `T: Ord`, or both (`T: Eq + Ord`). An unknown bound is E0315. `Ord` doesn't imply `Eq`; write both if both are used.
- **What a bare `T` allows:** being stored, moved, copied, passed, returned and put in arrays, structs and enums. `==` needs `T: Eq`, and `<` needs `T: Ord`. Using either without the bound is E0404, with help suggesting the bound.
- **What `T` never allows:** arithmetic, casts, printing, `.len`, or indexing. Those need a concrete type.

**Interfaces** (milestone 12).
- **Declaring one:** `interface Shape { fun area(self): f64; fun name(self): string; }` lists methods (with a `self` or `mut self` receiver) and associated functions (without one), each ending in `;`. Their signatures use concrete types only: no type parameters and no `Self`.
- **Satisfying one:** a type satisfies `Shape` if, for every item, it has a method or associated function with the same name, the same receiver kind, and the same parameter and return types, visible where the bound is checked. No declaration is needed. Primitives, strings and arrays have no methods of their own, so they satisfy only an empty interface.
- **Using one as a bound:** `fun total<T: Shape>(xs: T[]): f64`, combined freely with `Eq` and `Ord` (`T: Shape + Eq`). In the body, `x.area()` and, for an associated item, `T.make(…)` are allowed when some bound declares them. Calling a method no bound declares is E0421, with help suggesting the bound.
- **At a use:** a type argument that doesn't satisfy an interface bound is E0422, which names each missing or mismatched method.
- **Not a type:** an interface is never the type of a value; `let s: Shape = …` is E0305, with help to use it as a bound.
- **Names and visibility:** interfaces share their module's item namespace (E0302) and follow the visibility rules of §6.

**Using a generic.**
- **Type arguments:** a generic type is written with all its arguments in type positions: `Stack<i64>`, `Option<Vec2>`, `Pair<string, f64>`. A wrong count, or a bare `Stack` in a type position, is E0418.
- **Inference:** at a call, a struct literal, a variant or an associated function, the type arguments are inferred from the argument types and the expected type, by the same unification that types `[]` and `None` today. A literal's default (`i64`, `f64`) applies only if nothing else fixes the parameter.
- **Can't infer:** a parameter that is still undecided is E0412 (extended), with help to annotate: `let s: Stack<i64> = Stack.new();`.
- **Bounds at a use:** a type argument that doesn't satisfy its bound is E0419. For example, `largest([[1]])` fails because `i64[]` is not `Ord`. Every type is `Eq`, so in v2 only `Ord` can fail.

**`Option<T>`** becomes an ordinary generic enum, predeclared as `enum Option<T> { Some(T), None }`. Its behaviour is unchanged, and v1's special cases for it are removed from the checker's description.

**Generic recursion.** A generic struct or enum may contain itself only with its own parameters unchanged: `enum List<T> { Nil, Cons(T, List<T>) }` is fine, and the indirect-slot rule applies per instantiation. Instantiation must terminate. If monomorphizing a program needs instantiations nested more than 64 deep (`f<T>` calling `f<T[]>`), it is E0420.

**Modules and types.**
- **Naming:** an imported type is named through its module: `geometry.Vec2`, `circle.Shape.Circle(1.0)`.
- **Equality:** two types are the same only if they are declared by the same item. `a.Point` and `b.Point` are different types.
- **E0311:** a local may not be named like an imported module either.

**Copy table.** Unchanged. Generic code copies exactly where the concrete code would, because each instantiation is concrete.

## §5 Expressions and statements — changes

**`match` with nested patterns and guards.**
- **Matching:** a value matches a nested pattern when its variant matches and each payload matches its slot's pattern. Literal slots compare with `==`, and `f64` literal patterns are still excluded.
- **Bindings:** they behave as in v1 (immutable, borrowed, scoped to the arm) at any depth.
- **Guards:** the guard runs after the pattern matches, with its bindings in scope. It must be `bool` (E0403). If it is false, matching continues with the next arm.
- **Exhaustiveness:** a guarded arm never counts toward exhaustiveness or toward W0102. E0417 lists the missing cases as patterns, such as `Some(Rect(_, _))`.

**Evaluation of `==`.**
- **Order:** the operands are evaluated left to right, then compared.
- **Structs and arrays:** compared in field or element order, stopping at the first difference.
- **Panics:** a comparison never panics.

**`import` and qualified names.**
- **Calls:** `m.f(args)` calls the function `f` of the imported module `m`.
- **Types:** `m.T` names its type, and `m.T.f()` / `m.T.V(…)` reach its associated functions and variants.
- **Private items:** naming a `private` item of another module is E0313. So is calling a `private` method, or reading or assigning a `private` field (including through `swap`) outside its module.
- **Struct literals:** a literal outside the struct's module needs every field to be visible. A struct with a `private` field is built through one of its functions, such as `range.Range.new(1, 5)` (E0313).
- **Whole values:** `==`, copies and passing values are unaffected, because they never name a field.

## §6 Functions and program structure — changes

**Modules.**
- **The program:** a program is the main file plus every module it imports, directly or indirectly.
- **Module names:** a module's name is its path from the main file's directory, without `.la`, with `/` written as `.`. Each segment must be an identifier.
- **Resolution:** `import a.b;` loads `<root>/a/b.la` and binds the name `b` in the importing file only. A file that doesn't exist is E0312.
- **Clashes:** importing the same module twice in one file, or two modules whose last segments clash, or a module name that clashes with one of the file's own items, is E0314.
- **The main file** is the root module, and no file may import it. `main` must be defined there, and only there.

**Item order and cycles.** Pass 1 of §6 now runs over all modules together. It collects every module's types and signatures before any body is checked. So items may be used before their declaration across files too, and import cycles are fine. There is no module initialization code to order, because there are no top-level statements.

**Names.**
- **Namespaces:** each module has its own global namespace for functions, structs, enums and externs. The intrinsics and the built-in names `Option`, `Some`, `None`, `Eq` and `Ord` are visible in every module.
- **Visibility:** everything is visible to every module in the program unless it is marked `private`, which limits it to its own module. That covers functions, externs, structs, enums, methods, associated functions and struct fields.
  - A field can be no more visible than its struct: a `private` struct's fields are module-only already.
  - Variants share their enum's visibility.
  - An `impl` must be in the module that declares its type.
  - **`public`** is reserved for packages in v3. Writing it is E0317 ("`public` needs packages (v3)"), with help to remove it, since everything is already visible to the whole program.
- **No leaking:** a declaration that isn't `private` may not mention a `private` type in its signature: its parameters, return type, a field's type, or a variant's payloads. Callers in other modules couldn't name that type. It is E0318, reported at the declaration.
- **`main`** needs no keyword.
- **Lookup order:** block scopes, parameters, the module's own items, then imported module names.

**Generic functions.** They are declared once and instantiated per distinct list of type arguments. `main` can't be generic (E0304). An `extern fun` can't be generic, which the grammar enforces.

## §7 Memory model — changes

- No change for values. Generic instances lay out exactly like the concrete types they stand for.
- Deep `==` reads both values and allocates nothing.

## §8 C interop — changes

- An `extern fun` follows the same visibility rules: visible to the whole program unless it is `private`.
- Two modules may declare the same C function only with identical signatures. Otherwise it is E0306, extended.
- Generic types can't appear in extern signatures (E0409).

## §9 Compilation model — changes

**Driver.**
- **Builds:** `lughac build|run|check main.la` loads the main file and every module it reaches. Each file is lexed and parsed once, and all are checked together.
- **Diagnostics:** each one names its file. The `"file"` JSON field is the path as given on the command line for the main file, and the module's path relative to the main file's directory for the others.
- **`--emit`:** `--emit=tokens|ast` prints every module in load order, each after a `; module <name>` line. `--emit=ir` prints the one LLVM module, as before.

**Symbols.** These are architecture rule 7, extended:

| Item | Symbol |
| --- | --- |
| Function in the root module | `lugha_fn_<name>` (unchanged, so `main` is still `lugha_fn_main`) |
| Function in module `a.b` | `lugha_fn_a/b/<name>` (PRP-034: `/` after the module path, so it can't collide with a method) |
| Method | `lugha_fn_<module/>Type.<name>`, e.g. `lugha_fn_shapes/circle/Circle.area` |
| Generic instance | the symbol above with the type arguments in source spelling, `lugha_fn_max<i64>`, `lugha_fn_Stack<string>.push` |
| Deep equality helper | `lugha_eq_<type>`, one per struct, enum or array type compared, emitted like the copy helpers |

**Runtime.** It gains `lugha_rt_str_cmp(a, b)`, which returns −1, 0 or 1 as `i32`, for string `<`. String `==` keeps `lugha_rt_str_eq`.

**Lowering notes.**
- **Monomorphization:** it happens after checking, from `main` outward. The checker records each call's type arguments in its side table, so codegen still never infers a type (architecture rule 4).
- **Nested patterns** are lowered arm by arm (PRP-032): each arm tests its pattern outside in, binds, runs its guard, and on any failure branches to the next arm. Flat, unguarded matches keep v1's `switch`.

**New error codes.** The numbers are proposed; each becomes final when its milestone's PRP is approved.

| Code | Error | Example |
| --- | --- | --- |
| E0208 | `import` after an item | `fun f() {}` then `import geometry;` |
| E0312 | Module not found | `import shapes.cirlce;` with no `shapes/cirlce.la` |
| E0313 | `private` item or field used from another module | `range.helper()`; `r.lo`; `range.Range { lo: 1, hi: 5 }` |
| E0314 | Import clash | `import a.util;` and `import b.util;` in one file |
| E0319 | The main file imported | a module writing `import main;` (PRP-033) |
| E0315 | Unknown bound | `fun f<T: Hash>(x: T)` |
| E0316 | Bad type parameter name | `fun f<T, T>()`, `struct S<i64>`, a parameter named like a struct |
| E0317 | `public` used before packages exist | `public fun f() {}` — help: remove it; everything is visible to the program |
| E0318 | A `private` type in a non-`private` signature | `private struct S {}` then `fun make(): S` |
| E0418 | Wrong number of type arguments | `Stack<i64, i64>`, a bare `Stack` as a parameter type |
| E0419 | Type argument doesn't satisfy its bound | `largest([[1]])`: `i64[]` is not `Ord` |
| E0420 | Generic instantiation too deep | `fun f<T>(x: T) { f([x]); }` |
| E0421 | A method no bound provides (milestone 12) | `s.perimeter()` in `fun f<T: Shape>(s: T)` |
| E0422 | A type argument doesn't satisfy an interface (milestone 12) | `total_area([1, 2])`: `i64` has no method `area` |

**Reused codes:**
- E0302: built-in names, imports.
- E0304: generic `main`.
- E0306: conflicting externs.
- E0311: a local named like a module.
- E0403: guards, nested pattern types.
- E0404: operator on `T` without its bound, with help.
- E0409.
- E0412: type parameter not inferred.
- E0416: payload count in a nested pattern.
- E0417: missing cases now listed as nested patterns.
- W0102.

## §10 Example programs — additions (v2 acceptance tests)

**Patterns and equality** (milestone 9). Prints `big circle`, `square`, `nothing`, `true`, `true`, `true` on six lines; exits with 0.

```
enum Shape { Circle(f64), Rect(f64, f64), Empty }

fun describe(s: Option<Shape>): string = match s {
    Some(Circle(r)) if r > 1.0 => "big circle",
    Some(Circle(_)) => "small circle",
    Some(Rect(w, h)) if w == h => "square",
    Some(Rect(_, _)) => "rect",
    Some(Empty) => "empty",
    None => "nothing",
};

fun main() {
    println(describe(Some(Shape.Circle(2.0))));
    println(describe(Some(Shape.Rect(3.0, 3.0))));
    println(describe(None));
    println([1, 2] == [1, 2]);
    println(Shape.Rect(1.0, 2.0) == Shape.Rect(1.0, 2.0));
    println("apple" < "banana");
}
```

**Two modules** (milestone 10). `main.la` and `geometry.la` in one directory. `lughac run main.la` prints `3.5`, exits with 0.

```
// geometry.la
struct Vec2 { x: f64, y: f64 }

impl Vec2 {
    fun add(self, o: Vec2): Vec2 = Vec2 { x: self.x + o.x, y: self.y + o.y };
}

fun origin(): Vec2 = Vec2 { x: 0.0, y: 0.0 };
```

```
// main.la
import geometry;

fun main() {
    let mut p = geometry.origin();
    p = p.add(geometry.Vec2 { x: 1.5, y: 2.0 });
    println(p.x + p.y);
}
```

**Generics** (milestone 11). Prints `b`, `true`, `plum` on three lines; exits with 0.

```
struct Stack<T> { items: T[] }

impl Stack<T> {
    fun new(): Stack<T> = Stack { items: [] };
    fun push(mut self, x: T) { self.items.push(x); }
    fun pop(mut self): Option<T> = self.items.pop();
}

fun contains<T: Eq>(xs: T[], x: T): bool {
    for y of xs {
        if y == x { return true; }
    }
    false
}

fun largest<T: Ord>(xs: T[]): Option<T> {
    if xs.len == 0 { return None; }
    let mut best = xs[0];
    for x of xs {
        if x > best { best = x; }
    }
    Some(best)
}

fun main() {
    let mut s: Stack<string> = Stack.new();
    s.push("a");
    s.push("b");
    match s.pop() {
        Some(x) => println(x),
        None => println("empty"),
    }
    println(contains([1, 2, 3], 2));
    match largest(["pear", "apple", "plum"]) {
        Some(w) => println(w),
        None => println("none"),
    }
}
```

**Interfaces** (milestone 12). Prints `15.0`, `9.0`, `square` on three lines; exits with 0.

```
interface Shape {
    fun area(self): f64;
    fun name(self): string;
}

struct Circle { r: f64 }

impl Circle {
    fun area(self): f64 = 3.0 * self.r * self.r;
    fun name(self): string = "circle";
}

struct Square { side: f64 }

impl Square {
    fun area(self): f64 = self.side * self.side;
    fun name(self): string = "square";
}

fun total_area<T: Shape>(shapes: T[]): f64 {
    let mut sum = 0.0;
    for s of shapes {
        sum += s.area();
    }
    sum
}

fun describe<T: Shape>(s: T): string = s.name();

fun main() {
    println(total_area([Circle { r: 1.0 }, Circle { r: 2.0 }]));
    println(total_area([Square { side: 3.0 }]));
    println(describe(Square { side: 1.0 }));
}
```

## §11 Milestones — additions

| # | Milestone | Adds | Done when |
| --- | --- | --- | --- |
| 9 | Patterns and equality | nested patterns, guards, deep `==` on every type, string `<` (`lugha_rt_str_cmp`, `lugha_eq_*`) | The patterns program passes |
| 10 | Modules | `import`, `private` (items, methods, fields), `public` reserved (E0317), no-leak rule (E0318), multi-file loading, per-file diagnostics, module-qualified symbols | The two-module program passes |
| 11 | Generics | type parameters, `Eq`/`Ord` bounds, inference, monomorphization; `Option<T>` as an ordinary generic enum | The generics program passes |
| 12 | Interfaces | `interface` declarations, interfaces as bounds, structural satisfaction (E0421, E0422) | The interfaces program passes, and the v2 spec is complete (no longer "in progress") |

**Order:**
- **M9 first:** deep `==` and string order are what `T: Eq` and `T: Ord` rely on.
- **M10 next:** modules touch the driver, name resolution and symbols but not the type system.
- **M11 next:** generics, the largest milestone, land on a stable base.
- **M12 last:** interfaces are bounds, so they need generics first.

The multi-file example needs the §10 extraction in `tests/common/` to understand several files; M10's PRP decides how.

**Out of scope for v2** (for v3, roughly in order):
- closures and function types;
- a shared reference type;
- interface values and dynamic dispatch, generic interfaces, `Self` in interface signatures, and default methods;
- operator overloading through interfaces;
- explicit type arguments at a use (`parse<i64>(s)`);
- `.len()` as a method, so built-in types can satisfy interfaces;
- packages (a manifest, dependencies, search paths) with `public` for their API, re-exports, API-change checking;
- selective imports;
- or-patterns;
- `mut` bindings in `match`;
- string interpolation, bitwise operators, `u32`/`u64`/`f32`, wrapping arithmetic;
- command-line arguments.

## §12 Open questions — additions

- [ ] **Keep `T: Eq` when every type is `Eq`?** Proposed: yes. It documents intent, and it will start to restrict once function types exist in v3.
- [x] **A bare name in a nested slot:** it is a variant if the slot's enum has one by that name, and otherwise a binding. This is type-directed, as in Rust, and a typo in a variant name silently becomes a binding. W0102 or an unused-binding warning would catch most cases. Should v2 add an "unused binding" warning? Resolved (PRP-032): yes, W0103 "unused binding".
- [x] **The instantiation depth limit:** 64. Large enough for real code, small enough to fail fast. Resolved (PRP-036): 64, for instances and for generic types alike (E0420).
- [ ] **NaN and `Ord`:** a `largest` over `f64`s that contain `NaN` gives an order-dependent answer, because every comparison with `NaN` is false. Documented rather than prevented.
- [ ] **Module root:** the main file's directory, with no search path. Shared code outside it needs packages, which are v3.
- [x] **Meaning of `private`:** decided (session 40): private to its module, not to a type's `impl` blocks. An `impl` must live in its type's module anyway.
- [ ] **From the visibility note, for v3:** re-exports, API-change checking (`lughac api-diff`), and methods on built-in types across packages all wait for packages.
- [ ] **`Self` in interfaces:** without it, an interface can't say "the same type as the receiver", as in `fun add(self, o: Self): Self` or `fun zero(): Self`. Left for v3, with generic interfaces.
- [ ] **Printing generic values:** a built-in `Show`-style interface would let generic code print a `T`. Open; the intrinsics accept only primitives and strings today.
