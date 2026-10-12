# Lugha v1 Specification

Oct 9, 2026 · @kithinji

**v1.** This is v0 plus enums, `match`, `Option`, recursive data, methods and growable arrays (milestones 6–8). Every valid v0 program is a valid v1 program unless it uses one of the new keywords as a name.

## 1. Overview

Lugha (Swahili for "language") is a small, statically typed, compiled language with Rust-flavored structure and TypeScript-flavored declarations. It compiles through LLVM to native executables and manages heap memory with the Boehm garbage collector. Source files use the `.la` extension.

v0 is a learning target: every feature in this spec should be implementable by one person in a weekend or two, and every milestone ends in a program that runs.

**Goals**

- A complete pipeline from source text to native binary: lexer, parser, type checker, LLVM codegen, linker.
- Static types with no implicit conversions, so type errors are caught before codegen.
- Memory safety for ordinary code: no null, no dangling pointers, bounds-checked indexing.
- Clear error messages with source locations.

**Non-goals for v0**

- Generics (other than the built-in `Option<T>`), modules and closures. See section 11.
- Performance tuning beyond what LLVM's optimizer gives for free.
- A self-hosted compiler or a package manager.

**Design principles**

- One way to do each thing. Fewer features, fully specified.
- Immutable by default. A binding can only be reassigned if it is declared `let mut`.
- Every construct maps directly onto LLVM IR without a runtime type system.
- Where Rust and TypeScript disagree, pick whichever is simpler to implement and parse.

**Syntax at a glance**

```
struct Point { x: f64, y: f64 }

fun dist2(a: Point, b: Point): f64 {
    let dx = a.x - b.x;
    let dy = a.y - b.y;
    dx * dx + dy * dy
}

fun main() {
    let mut total = 0;
    for i in 0..10 {
        total += i;
    }
    println(total);
}
```

## 2. Lexical structure

Source files are UTF-8. The lexer produces a flat list of tokens, each carrying a span (start and end byte offsets) for error reporting.

**Whitespace and comments.** Spaces, tabs, carriage returns and newlines separate tokens and are otherwise ignored. A line comment starts with `//` and runs to the end of the line. v0 has no block comments.

**Identifiers.** `[A-Za-z_][A-Za-z0-9_]*`, excluding keywords. Identifiers are case-sensitive.

**Keywords.**

```
fun  extern  struct  enum  impl  let  mut  if  else  while  for  in  of  match
return  break  continue  true  false  as  self
i32  i64  u8  f64  bool  string
```

Built-in type names are keywords, so they can't be shadowed by user structs. `Option`, `Some` and `None` are not keywords but built-in names, reserved like the intrinsics (section 6).

**Literals**

| Kind | Examples | Notes |
| --- | --- | --- |
| Integer | `42`, `1_000_000`, `0xFF` | Decimal or hex; `_` separators allowed between digits |
| Float | `3.14`, `2.0e-3` | Must have digits on both sides of the `.`; optional exponent |
| String | `"hello\n"` | Double-quoted; escapes `\n` `\t` `\r` `\\` `\"` `\0` |
| Boolean | `true`, `false` | Keywords |

**Number details.**

- `_` may appear only between two digits, and only in integers. `1__0`, `1_`, `0x_FF` and `1_0.5` are errors.
- The hex prefix is lowercase `0x` followed by at least one hex digit, in either case. `0XFF` and `0x` are errors.
- Leading zeros are allowed and mean nothing special: `007` is 7.
- A literal without a `.` is an integer. `2e5` is an error; write `2.0e5`.
- An exponent is `e` or `E`, an optional sign, then at least one digit.
- A `.` not followed by a digit ends an integer: `1..10` is `1` `..` `10`.
- A letter directly after a number is an error (`123abc`, `1.5x`).
- The lexer accepts integers up to 2⁶⁴−1. Whether a literal fits its actual type is checked later (section 4).
- A float literal that rounds to infinity is an error. One that underflows rounds to zero.

**String details.** A string may not contain a raw line break: write `\n`. An unclosed string is reported at the end of its line, and lexing continues on the next one. Any other UTF-8 may appear inside strings and comments. Outside them, only ASCII is allowed.

**Lexical errors.** The lexer reports every error it finds, not just the first.

| Code | Error | Example |
| --- | --- | --- |
| E0101 | Unexpected character | `@`, `#`, `é` outside a string, lone `&` |
| E0102 | Unterminated string | `"abc` at end of line or file |
| E0103 | Unknown escape sequence | `"\q"` |
| E0104 | Integer literal too large | `18446744073709551616` |
| E0105 | Misplaced `_` in a number | `1__0`, `1_`, `0x_FF`, `1_0.5` |
| E0106 | Number is missing digits | `0x`, `2.0e`, `2.0e+` |
| E0107 | Float needs a fractional part | `2e5` (help: write `2.0e5`) |
| E0108 | Invalid character in a number | `123abc`, `0XFF`, `0xFG` |
| E0109 | Float literal out of range | `1.0e999` |
| E0110 | Source is not valid UTF-8 | A Latin-1 `é` byte; reported at the first bad byte |

**Operators and punctuation.** The lexer uses longest match, so `<=` is one token, not `<` then `=`.

```
+  -  *  /  %  ==  !=  <  <=  >  >=  &&  ||  !
=  +=  -=  *=  /=
(  )  {  }  [  ]  ,  ;  :  .  ..  =>
```

`::` is lexed as a token only so the parser can reject it helpfully (E0207): Lugha writes `Shape.Circle`, not `Shape::Circle`.

## 3. Grammar

The grammar is LL(1) apart from expressions, which use Pratt parsing with the precedence table below. Parentheses around the condition of `if` and `while`, and around a `for` header, are optional: `if x > 0 {` and `if (x > 0) {` are both valid.

```ebnf
program      = { item } EOF ;
item         = fun_decl | extern_decl | struct_decl | enum_decl | impl_decl ;

fun_decl     = "fun" IDENT "(" [ params ] ")" [ ":" type ] fun_body ;
fun_body     = block | "=" expr ";" ;          (* "= e;" means "{ e }" *)
extern_decl  = "extern" "fun" IDENT "(" [ params ] ")" [ ":" type ] ";" ;
struct_decl  = "struct" IDENT "{" [ field { "," field } [ "," ] ] "}" ;
field        = IDENT ":" type ;
enum_decl    = "enum" IDENT "{" [ variant { "," variant } [ "," ] ] "}" ;
impl_decl    = "impl" IDENT "{" { method } "}" ;
method       = "fun" IDENT "(" [ receiver [ "," params ] | params ] ")" [ ":" type ] fun_body ;
receiver     = [ "mut" ] "self" ;
variant      = IDENT [ "(" type { "," type } [ "," ] ")" ] ;
params       = param { "," param } [ "," ] ;
param        = IDENT ":" type ;

type         = base_type { "[" "]" } ;
base_type    = "i32" | "i64" | "u8" | "f64" | "bool" | "string"
             | "Option" "<" type ">"
             | IDENT ;

block        = "{" { stmt } [ expr ] "}" ;   (* trailing expr = tail value *)
stmt         = var_decl | while_stmt | for_stmt
             | "return" [ expr ] ";"
             | "break" ";" | "continue" ";"
             | block_like                     (* no ";" needed *)
             | expr [ assign_op expr ] ";" ;
block_like   = if_expr | match_expr | block ;
var_decl     = "let" [ "mut" ] IDENT [ ":" type ] "=" expr ";" ;
assign_op    = "=" | "+=" | "-=" | "*=" | "/=" ;

if_expr      = "if" cond block [ "else" ( if_expr | block ) ] ;
while_stmt   = "while" cond block ;
for_stmt     = "for" ( for_head | "(" for_head ")" ) block ;
for_head     = IDENT ( "in" cond ".." cond | "of" cond ) ;
cond         = expr ;            (* parsed in no-struct-literal mode *)

match_expr   = "match" cond "{" [ arm { arm_sep arm } [ "," ] ] "}" ;
arm          = pattern "=>" expr ;
arm_sep      = "," | (* nothing, after an arm whose expr is a block *) ;
pattern      = "_"
             | IDENT [ "(" pat_slot { "," pat_slot } [ "," ] ")" ]
             | [ "-" ] INT | STRING | "true" | "false" ;
pat_slot     = IDENT | "_" ;

expr         = prefix { infix } ;            (* Pratt loop, see table *)
prefix       = ( "-" | "!" ) prefix | postfix ;   (* binds tighter than infix *)
postfix      = primary { call | index | member } ;
call         = "(" [ expr { "," expr } [ "," ] ] ")" ;
index        = "[" expr "]" ;
member       = "." IDENT ;
primary      = INT | FLOAT | STRING | "true" | "false"
             | IDENT [ struct_init ]
             | "self"
             | "(" expr ")"
             | "[" [ expr ( ";" expr | { "," expr } [ "," ] ) ] "]"
             | block_like ;
struct_init  = "{" [ IDENT ":" expr { "," IDENT ":" expr } [ "," ] ] "}" ;
infix        = binop expr | "as" type ;
```

**Operator precedence**, from lowest to highest. All binary operators are left-associative, except equality and comparison (levels 3 and 4), which are non-associative: `a < b < c` and `a == b == c` are parse errors. Write `(a == b) == c` if that is really meant.

| Level | Operators | Kind |
| --- | --- | --- |
| 1 | `\|\|` | Logical or, short-circuit |
| 2 | `&&` | Logical and, short-circuit |
| 3 | `==` `!=` | Equality |
| 4 | `<` `<=` `>` `>=` | Comparison |
| 5 | `+` `-` | Additive |
| 6 | `*` `/` `%` | Multiplicative |
| 7 | `as` | Cast |
| 8 | `-` `!` | Prefix unary |
| 9 | call, `[]`, `.` | Postfix |

**Assignment is a statement**, not an expression, so `a = b = c` and `if (x = 1)` are parse errors. The left side of an assignment must be a place expression: a variable, a field access, an index, or a chain of these (`pts[i].x`). The parser accepts any expression there; the type checker rejects non-places.

**Struct literals in conditions.** Without required parentheses, `if p == Point { x: 0.0, y: 0.0 } { ... }` is ambiguous, because the parser can't tell where the condition ends. So every `cond` is parsed in *no-struct-literal* mode, where `IDENT {` ends the expression instead of starting a literal. The restriction lifts inside every bracket pair — parentheses, brackets, call arguments and blocks — so `if p == (Point { x: 0.0, y: 0.0 }) { ... }` and `if ok { Point { x: 1.0, y: 2.0 } } else { ... }` both work. Rust uses the same rule.

**Enum values and `match`.** `Shape.Circle(2.0)` and `Shape.Empty` need no grammar of their own: they parse as a member access, followed by a call when there are payloads, and the checker decides they name a variant (section 4). A `match` scrutinee is a `cond`, parsed in no-struct-literal mode like an `if` condition. A bare `IDENT` pattern must name a variant; there are no top-level binding patterns, so use `_`.

**Block-like statements.** A statement that starts with `if`, `match` or `{` is parsed as `block_like` and needs no trailing `;`. It ends at its closing brace, so `if c { a } - 1` is a statement followed by the expression `-1`, not a subtraction. If a block's final element is an expression with no `;` after it, including a block-like one, that expression is the block's tail value (section 5). A `;` directly after a block-like statement is allowed and makes it an ordinary statement: `if c { f(); };`. A `;` on its own, where a statement should start, is an error.

**Parse errors.** The parser reports as many syntax errors as it can, skipping to the next statement, or to the next item at top level, after each one.

| Code | Error | Example |
| --- | --- | --- |
| E0201 | Unexpected token | `let x = 1 let y = 2;`, `let x = );`, a stray `;`, `let` at top level |
| E0202 | Unclosed delimiter | `(`, `[` or `{` still open at end of file |
| E0203 | Chained comparison or equality | `a < b < c`, `a == b != c` |
| E0204 | Assignment used as an expression | `a = b = c;`, `if (x = 1) {}`, `f(x = 1)` |
| E0205 | Struct literal in a condition without parentheses | `if p == Point { x: 0.0 } { }` |
| E0206 | Nesting too deep | More than 256 levels of nested expressions and blocks |
| E0207 | `::` used as a path separator | `Shape::Circle(1.0)` — help: write `Shape.Circle`; parsing continues as if `.` were written |

## 4. Type system

Lugha is statically typed with no implicit conversions. Every expression has exactly one type, computed bottom-up from its parts, with one exception: numeric literals take their type from context (see Inference).

**Types**

| Type | Description | LLVM lowering |
| --- | --- | --- |
| `i32` | 32-bit signed integer | `i32` |
| `i64` | 64-bit signed integer | `i64` |
| `u8` | 8-bit unsigned integer (a byte) | `i8` |
| `f64` | 64-bit IEEE 754 float | `double` |
| `bool` | `true` or `false` | `i1` (stored as `i8` in memory) |
| `string` | Immutable UTF-8 bytes | `ptr` to `{ i64 len, bytes, NUL }` on the GC heap (literals: constant globals) |
| `T[]` | Fixed-length array of `T` | `ptr` to `{ i64 len, T... }` on the GC heap |
| `S` (struct) | User-declared record | LLVM named struct `%S` |
| `E` (enum) | One of the declared variants, with its payloads | LLVM named struct `%E` = `{ i32 tag, payload area }` (section 7) |
| `Option<T>` | `Some(T)` or `None` | As an enum with those two variants |
| `void` | Return type only; written by omitting `: type` | `void` |

**Value semantics.** Every Lugha type behaves as a value. Assigning or storing a value gives the new place its own independent copy, so changing one variable never changes another. Strings are immutable, so the compiler shares them freely and no program can tell. Arrays live on the heap but are copied whenever sharing could be observed (see Array copies below). Enum payloads can't be changed in place, so enum values share them freely too, and no program can tell.

```
let a = Point { x: 1.0, y: 2.0 };
let mut b = a;    // copy
b.x = 9.0;        // a.x is still 1.0

let xs = [1, 2, 3];
let mut ys = xs;  // copy
ys[0] = 9;        // xs[0] is still 1
```

There is no null. Every string and array variable holds a valid object.

**void is not a value.** `void` can't be written as a type, and no variable, parameter, field or array element can hold one. `let x = println(1);` is a type error. A `void` expression can only be used as a statement, as the tail of a `void` block or function, or as a branch of an `if` whose type is `void`.

**Structs.** Declared at top level with named, typed fields. A struct literal must initialize every field exactly once, in any order. Structs may contain other structs and arrays, but a struct may not contain itself directly or through other structs (it would have infinite size). Containing itself through an array is fine, because an array is a pointer: `struct Node { value: i64, kids: Node[] }` builds trees. Other recursive data, such as linked lists, uses an enum or `Option`: a struct may contain itself through either, as in `struct Item { value: i64, next: Option<Item> }`. Only a cycle through structs alone is an error (E0307).

**Enums.** Declared at top level, in any order, as a list of variants that each carry zero or more positional payloads: `enum Shape { Circle(f64), Rect(f64, f64), Empty }`. A variant name is unique within its enum (E0309). A value is built through the enum's name with `.`: `Shape.Circle(2.0)`, or `Shape.Empty` for a variant without payloads. Each payload expects its declared type, and the count must match (E0416); an unknown variant is E0415. There is no `==` or other operator on enums (E0404): compare with `match`. A payload can never be changed in place — there is no syntax for it, and `match` bindings are immutable — so copying an enum shares its payloads, and the copies that matter happen when a payload is stored: building a variant from a place copies it (`Job.Batch(xs)`), and storing a `match` binding copies it (section 4, Array copies). 

**Recursive data.** An enum may contain itself, directly or through structs, other enums or `Option`: `enum List { Nil, Cons(i64, List) }` builds a linked list, and `enum Tree { Leaf, Node(Tree, i64, Tree) }` a binary tree. Nothing has to be annotated and there is no `Box` (writing `Box<List>` is E0305 with a hint). The compiler stores each payload slot that leads back to its enum behind a hidden pointer (section 7), so the type has a finite size, values like `List.Nil` cost nothing, and — since payloads are shared — copying a list of any length takes constant time. A program can't observe the pointer: in `Cons(x, rest)`, `rest` is an ordinary `List` value.

**`Option`.** The built-in `Option<T>` has the variants `Some(T)` and `None`, written without a prefix. `Some(x)` has type `Option<T>` where `x : T`, or the expected `Option` type when there is one. `None` needs an expected type, exactly like `[]`: `let x: Option<i64> = None;` — otherwise it is E0412. `Option` is the only type that takes a type argument.

**`match`.** `match e { pattern => value, … }` is an expression. Patterns are flat: a variant with a name or `_` for each payload (`Circle(r)`, `Rect(w, _)`, `Empty`), an integer, `bool` or string literal, or `_`. Variant patterns need an enum or `Option` scrutinee and are written without the enum's name; literal patterns need a scrutinee of their type (E0403). The arms' values follow the rules of `if`/`else`: they must agree (E0403), and an arm that never finishes fits any type.

- **Exhaustiveness.** A `match` must cover every value. On an enum, either every variant has an arm or the last arm is `_`; otherwise it is E0417, listing every missing variant. A `bool` match is complete with both `true` and `false`. Integers and strings always need a final `_`.
- **Unreachable arms.** An arm after `_`, or for a variant or literal already covered, is warning W0102.
- **Bindings.** Payload names are immutable and borrowed, like `for … of` variables: storing or returning one copies it. Each is in scope in its own arm only.

**Arrays.** `T[]` has a length, read with `.len` (type `i64`), that changes only through the built-in methods below. Elements are read and written with `xs[i]`, where `i` must be `i64`. Every index is bounds-checked; an out-of-range index panics. Arrays are created two ways:

- List literal `[a, b, c]`. All elements must have the same type. An empty literal `[]` needs an expected type from context, such as `let xs: i64[] = [];`.
- Repeat literal `[value; count]`, where `count` is a runtime `i64`. Every element is a copy of `value`. `value` is evaluated once, before `count`. A negative `count` panics with `negative array length`; a `count` of 0 gives an empty array.

**Array copies.** The compiler copies an array only when it is read from an existing place and stored into another one. Fresh arrays and function arguments are never copied.

| Situation | Copied? | Why |
| --- | --- | --- |
| `let ys = xs;` or `ys = xs;` | Yes | `ys` gets its own array |
| A place used as a struct-literal field or array-literal element (`Wrap { data: xs }`) | Yes | The new struct or array gets its own copy |
| `return p;` where `p` is a parameter, a `for … of` variable, or part of one | Yes | Otherwise the caller's argument, or the iterated array, would be shared |
| `return xs;` where `xs` is a local or part of one | No | The local is about to go away, so the array simply moves |
| Passing an argument, `f(xs)` | No | Parameters are immutable, and nothing else runs during the call |
| Calling a method, `x.m()` | No | `self` is passed like any argument; a `mut self` method edits `x` itself, in place |
| A fresh value: literal, call result, string concatenation | No | Nothing else refers to it |

A function's tail value follows the same rules as `return`: a body ending in `p` copies, and one ending in `xs` moves. "From a place" looks through `if` and block tails: `let ys = if c { xs } else { [0] };` copies. When the value comes from a local that is never used again, the compiler may move it instead of copying; no program can tell the difference.

Copies are deep. Copying an `i64[][]` copies every inner array, and copying a struct copies the arrays inside it. Strings inside are shared, since they are immutable.

**Strings.** Immutable. `.len` gives the length in bytes as `i64`. `s[i]` reads one byte as `u8`, bounds-checked. `+` concatenates two strings into a new one. `==` and `!=` compare contents.

**Inference.** Local inference only.

1. A `let` without an annotation takes the type of its initializer.
2. Function parameters, return types and struct fields are always annotated.
3. Numeric literals use the expected type when there is one: the variable's annotation, the parameter type at a call, the declared return type, the other operand of a binary operator, the other bound of a range, or the target of an assignment.
4. With no expected type, integer literals are `i64` and float literals are `f64`.
5. An integer literal that doesn't fit its type is a compile error. An integer literal never becomes a float, or vice versa.
6. A unary `-` applied directly to a numeric literal is folded into the literal before the range check, so `-2147483648` is a valid `i32` and `-9223372036854775808` a valid `i64`. A negated `u8` literal is an error.

```
let a = 5;            // i64
let b: i32 = 5;       // i32
let c = b + 1;        // i32: 1 takes its type from b
let d = 2.5;          // f64
let e: f64 = 5;       // error: integer literal where f64 expected
```

**Operator typing**

| Operators | Operands | Result |
| --- | --- | --- |
| `+` `-` `*` `/` `%` | Two numbers of the same type | That type |
| `+` | Two strings | `string` |
| `<` `<=` `>` `>=` | Two numbers of the same type | `bool` |
| `==` `!=` | Two values of the same primitive type, or two strings | `bool` |
| `&&` `\|\|` | Two `bool` | `bool` |
| `!` | `bool` | `bool` |
| unary `-` | `i32`, `i64` or `f64` | Same type |

Structs and arrays have no `==` in v0. Compare their fields or elements instead.

**Casts.** `expr as T` converts between numeric types (`i32`, `i64`, `u8`, `f64`). Integer narrowing truncates. Integer to float rounds to nearest. Float to integer truncates toward zero and saturates at the target's range; NaN becomes 0. `bool` cannot be cast; write `if b { 1 } else { 0 }` instead.

**Names of types.** A local — a `let`, a parameter, a `for` variable or a `match` binding — may not be named like a struct, an enum or `Option` (E0311). So a name before `.` is always unambiguous: a type, meaning a variant or associated function follows, or a local, meaning a field or method follows.

**Methods.** An `impl T { … }` block attaches functions to a struct or enum `T` declared anywhere in the file; a type may have several `impl` blocks, and no `impl` may target a built-in type (E0305). Method names are unique per type and may not repeat a field or variant name (E0310).

- **`self` methods.** A method whose first parameter is `self` is called on a value: `v.len_sq()`. `self` is an immutable, borrowed parameter of type `T`: storing or returning it copies it, like any parameter.
- **`mut self` methods.** A method whose first parameter is `mut self` changes the caller's value in place: `v.scale(2.0)`. The receiver must be a place whose root is `let mut` (E0501), not inside a string (E0506), and not an array being iterated (E0507); it may be a variable, a field or an element (`r.min.shift(1.0)`, `ps[1].scale(2.0)`). Inside, `self`, its fields and its elements can be assigned, and `self` can be replaced whole.
- **Associated functions.** A function in an `impl` without `self` is called on the type: `Vec2.new(1.0, 2.0)`.
- **No aliasing.** An argument of a `mut self` call may not mention the receiver's root variable (E0508): in `v.add(v)` the method would see its own edits through `o`. Copy first: `let w = v; v.add(w);`.

Calling a method that doesn't exist, a `self` method on a type or an associated function on a value is E0415. Strings and `Option` have no methods; arrays have only the built-ins below.

**Growable arrays.** Every array has three built-in methods:

| Method | Effect |
| --- | --- |
| `xs.push(v)` | Appends `v`; `xs.len` grows by one |
| `xs.pop()` | Removes and returns the last element as `Some(x)`, or `None` if `xs` is empty |
| `xs.clear()` | Removes every element; `xs.len` becomes 0 |

They follow the `mut self` rules: `xs` must be a place whose root is `let mut` (E0501), not inside a string (E0506), and not an array being iterated by `for … of` (E0507). `push` stores its value like an assignment does — copied from a place, moved on last use — and reads it before the array grows, so `xs.push(xs[0])` simply appends a copy of the first element (E0508 doesn't apply). `pop` moves the element out. Indexing is still bounds-checked against the current `.len`, and copying an array copies its current elements.

**Mutability.** Bindings are immutable by default. Assigning to any place requires the variable at its root to be declared `let mut`. That covers the variable itself, a field (`p.x = 1.0`), an element (`xs[0] = 1`) and any chain of these (`shapes[i].pts[j].x = 2.0`). Because every variable owns its data, a variable declared without `mut` never changes after it is initialized.

## 5. Expressions and statements

Evaluation is strictly left to right: operands, call arguments and struct-literal fields are evaluated in source order. The only exceptions are `&&` and `||`, which skip their right operand when the left decides the result.

**Arithmetic.** Integer `+`, `-`, `*` and unary `-` panic on overflow: if the true result doesn't fit the type, the program stops with `integer overflow` instead of continuing with a wrong value. Integer `/` and `%` panic when the divisor is zero, and also for `MIN / -1`, which overflows. `%` takes the sign of the dividend, like C and Rust. Compound assignments such as `+=` are checked the same way. Float arithmetic follows IEEE 754: overflow gives an infinity and division by zero gives an infinity or NaN, never a panic. Explicit casts are the one place values wrap: narrowing with `as` truncates (section 4).

**Method calls.** `x.m(a, b)` evaluates `x`, then `a`, then `b`; `xs.push(v)` evaluates the place `xs`, then `v`. For a `mut self` call the place `x` is evaluated once, before the arguments, like an assignment's place.

**`match` evaluation.** A `match` evaluates its scrutinee once, then tests the arms from top to bottom and runs the first that matches. A variant's payloads are evaluated left to right when it is built.

**Compound assignment.** `x op= e` means `x = x op e`, except that the place `x` is evaluated once. So `xs[next()] += 1` calls `next()` once.

**Variable declarations.** Every variable must be initialized. A variable is in scope from the end of its declaration to the end of its enclosing block, so `let x = x + 1;` refers to an outer `x`. Shadowing is allowed, both in nested blocks and in the same block.

**if / else.** `if` is an expression. The condition must be `bool`. With an `else`, both branches must have the same type, which becomes the type of the whole `if`. Without an `else`, the type is `void`. An expected type flows into both branches, so `let x: i32 = if c { 1 } else { 2 };` is valid.

```
let sign = if x < 0 { -1 } else if x == 0 { 0 } else { 1 };
```

**Blocks and tail values.** A block `{ stmts tail }` runs its statements, then evaluates `tail` and yields it as the block's value. A block with no tail has type `void`. Variables declared inside a block go out of scope at its closing brace. A block-like statement in the middle of a block must have type `void`, because a value there would be silently thrown away; the checker reports it as an error.

**while.** Evaluates the condition before each iteration; stops when it is `false`.

**for over a range.** `for i in a..b { body }` runs with `i` = `a`, `a+1`, …, `b-1`. The range is half-open, and empty when `a >= b`. `a` and `b` must have the same integer type, which becomes the type of `i`. Both bounds are evaluated once, before the first iteration. `i` is a fresh immutable binding on each iteration. It desugars to:

```
{
    let __end = b;
    let mut __i = a;
    while __i < __end {
        let i = __i;
        body
        __i += 1;   // also run on continue
    }
}
```

**for over an array.** `for x of xs { body }` binds each element in order. `xs` is evaluated once and is not copied. `x` is an immutable binding to the element, so assigning to `x` is an error; write `xs[i]` with a range loop to modify elements. The body may not assign to `xs` or any part of it, which is checked at compile time, so the loop always sees the array as it was when the loop started. Strings are not iterable in v0.

**break and continue.** Apply to the innermost enclosing loop. Using either outside a loop is a compile error.

**Expression statements.** Any expression followed by `;` is a statement, and its value is discarded. Discarding the result of a non-`void` call is allowed. Adding `;` after a would-be tail expression turns it into a statement, so `{ x * x; }` is a `void` block. When a value was expected, the checker reports this as "remove this semicolon" and points at it.

**Panics.** A panic prints `panic: <message> at <file>:<line>:<col>` to stderr and exits with code 101. Panics come from integer overflow (message `integer overflow`, including `MIN / -1`), integer division by zero (`division by zero`), out-of-bounds indexing (`index out of bounds: the length is N but the index is I`, located at the `[`), a negative repeat-literal count, and the `panic(msg)` intrinsic. An arithmetic panic's location is its operator: the `+` in `x + 1`, the `+=` of a compound assignment, the `-` of a negation. They cannot be caught.

**Intrinsics.** These names are built into the compiler, not declared in source. They accept arguments that ordinary functions can't, which is why they are special.

| Intrinsic | Accepts | Effect |
| --- | --- | --- |
| `print(x)` | Any primitive or `string` | Writes `x` to stdout with no newline |
| `println(x)` | Any primitive or `string`, or nothing | Writes `x` and a newline |
| `panic(msg)` | `string` | Panics with `msg` |
| `to_string(x)` | Any primitive | Returns `x` formatted as a `string` |

Floats print as the shortest decimal that round-trips, in Lugha float-literal syntax: always a `.` with at least one digit on each side, so `2.0` prints as `2.0`, not `2`. Values with magnitude at or above `1e16`, or below `1e-5` and nonzero, use an exponent: `1.0e16`, `2.5e-7`. Infinities and NaN print as `inf`, `-inf` and `NaN`; `-0.0` prints as `-0.0`. `bool` prints as `true` or `false`. `u8` prints as a number, not a character.

## 6. Functions and program structure

A v0 program is a single `.la` file containing top-level items: functions, extern declarations and structs. There are no top-level variables or statements.

**Item order doesn't matter.** A function can call a function declared later in the file, and a struct or enum can use a struct or enum declared later. The type checker runs in two passes to support this:

1. Collect every struct name and function signature into a global table.
2. Resolve struct field and enum payload types, check for structs that contain themselves through structs alone (E0307), and mark the enum payload slots that lead back to their enum (indirect slots, section 7).
3. Check each function body against the global table.

**Names.** Functions, structs, enums, intrinsics and the built-in names `Option`, `Some` and `None` share one global namespace, and duplicates are a compile error. Methods live in their type's namespace, so a function `dist` and a method `Point.dist` can coexist. No local may be named like a type (E0311, section 4). Local variables may shadow function names. Names resolve from the innermost scope outward: block scopes, then function parameters, then globals.

**Functions.** Declared with `fun`, typed parameters, and an optional `: type` return annotation. Omitting the annotation means `void`.

**Expression bodies.** A function whose body is a single expression can be written Kotlin-style, with `=` and a semicolon instead of braces. `fun f(...): T = e;` means exactly `fun f(...): T { e }`, so every rule for blocks and tail values applies. The return type follows the same rule as block bodies: it must be written, and omitting it means `void`. This keeps signatures readable without looking at the body, and lets the checker collect every signature before it checks any body.

```
fun square(x: i64): i64 = x * x;

fun is_even(n: i64): bool = n % 2 == 0;

fun max(a: i64, b: i64): i64 = if a > b { a } else { b };

fun greet(name: string) = println("hello, " + name);

fun clamp(x: i64, lo: i64, hi: i64): i64 {
    let low = max(x, lo);
    if low > hi { hi } else { low }
}
```

- Parameters are immutable bindings. To modify one, shadow it with a mutable copy: `let mut n = n;`.
- Arguments are never copied at the call. Structs and arrays are passed by pointer, which is safe because the callee can't modify them and nothing else runs during the call. A copy happens only if the callee stores or returns the parameter (section 4).
- Recursion, including mutual recursion, is allowed.
- There are no default parameters, overloading, variadics or nested functions.

**Return checking.** A non-`void` function must produce a value on every path. Its body passes if its tail expression has the return type, or if the body *definitely returns* by these rules:

- `return e;` definitely returns.
- A block definitely returns if any statement in it does. Statements after it are unreachable and reported as a warning.
- `if/else` definitely returns if both branches do. `if` without `else` never does.
- Loops never do, even `while true`.

A `panic(...);` statement also counts as definitely returning, because it never returns. A block that definitely returns can stand in for a value of any type, so `if x < 0 { return 0; } else { x }` has type `i64`.

This rejects a few correct programs, such as a function whose only exit is a `return` inside `while true`. Add a final `panic("unreachable");` in that case. `return` stays available for early exits. In a `void` function, `return;` is allowed and falling off the end is an implicit return.

**Entry point.** Every program must define `main` as either `fun main()` or `fun main(): i32`. The process exit code is 0 for the first form and `main`'s return value for the second. The user's `main` is emitted as `lugha_fn_main`, like every Lugha function, and the compiler emits a real C `main` that initializes the runtime and calls it (see section 9). Command-line arguments are not accessible in v0.

## 7. Memory model

Value types live on the stack; strings and arrays live on a garbage-collected heap managed by the Boehm-Demers-Weiser conservative collector. Programs never free memory explicitly.

**Stack.** Every local variable and parameter gets an `alloca` in its function's entry block, including struct values. LLVM's `mem2reg` pass promotes these to registers. Struct parameters are passed as a pointer to the caller's storage (section 9); struct return values are returned by value in LLVM IR, and LLVM handles the platform calling convention.

**Enums.** An enum value is a stack value `{ i32 tag; payload area }`. Variants are numbered in declaration order (`None` is 0 and `Some` is 1). The payload area has room for the largest variant's payloads laid out as a struct with natural alignment, and the whole value is aligned to the larger of 4 and that alignment. Enum parameters are passed like structs: as a pointer to the caller's storage. Copying an enum copies its tag and payload bytes; arrays in its payloads are shared, never mutated.

**Indirect slots.** A payload slot whose type leads back to its enum through values held inline — struct fields, enum payloads or `Option`'s payload, but not arrays — is an 8-byte pointer to a copy of the value on the GC heap, allocated with `lugha_rt_alloc` when the variant is built and never mutated afterwards. Every other slot, and the enum value itself, stays inline. `List` is therefore `{ i32 tag; i64 head; ptr rest }`.

**Recursion depth.** Function calls use the native stack, which is finite. A recursion that goes too deep — for example walking a list of a million nodes recursively — crashes the program with a signal (exit code 139) instead of panicking. Walk very long lists with a loop.

**Heap objects.** All heap memory comes from one runtime function, `lugha_rt_alloc(size)`, which calls `GC_malloc`. Memory from `GC_malloc` is zeroed. Every heap object starts with a length header:

| Object | Layout | Pointer held by Lugha code |
| --- | --- | --- |
| `string` | `{ i64 len; u8 bytes[len]; u8 nul }` | Points at `len` |
| `T[]` | `{ i64 len; i64 cap; T elems[cap] }` | Points at `len` |

Elements are laid out with LLVM's natural alignment for `T`. An array's capacity is never visible to programs: literals, repeat literals and copies are full (`cap == len`), and `push` on a full array allocates a new one with double the capacity (at least 4), copies the elements, and stores the new pointer in the array's place — its only owner, so no other pointer sees the old object. String data is always followed by a NUL byte, so it can be passed to C without copying. String literals are emitted as private constant globals with the same layout, and are never freed.

Because of value semantics (section 4), every array is owned by exactly one variable, field or element, except while it is lent to a function call. Only strings are shared, and they are immutable.

**Collection.** Boehm scans the stack, registers and the GC heap for anything that looks like a pointer into the heap. Objects with no such pointer are reclaimed. Two consequences matter for v0:

- **No compiler support is needed.** The compiler doesn't emit stack maps or GC metadata. It only has to route every allocation through `lugha_rt_alloc`.
- **Pointers must stay visible.** Boehm can't see memory allocated by C's `malloc`. If an extern C function stores a Lugha string or array somewhere Boehm doesn't scan, the object may be freed while still in use. In v0, extern functions must not keep references to Lugha objects after they return.

Boehm is conservative, so an integer that happens to look like a heap address keeps that object alive. This can waste some memory but never causes incorrect behavior.

**Safety.** Ordinary v0 code cannot produce a dangling pointer, read uninitialized memory, or index out of bounds. The only route to undefined behavior is calling extern C code.

## 8. C interop

Lugha calls C functions through `extern fun` declarations, which give a C function's name and signature without a body. The compiler emits an LLVM function declaration, and the system linker resolves it.

```
extern fun puts(s: string): i32;
extern fun abs(x: i32): i32;
extern fun sqrt(x: f64): f64;
```

**Allowed types.** Extern signatures may use only the types below. Structs, arrays, enums and `Option` cannot cross the boundary.

| Lugha type | C type | Passed as |
| --- | --- | --- |
| `i32` | `int32_t` | Value |
| `i64` | `int64_t` | Value |
| `u8` | `uint8_t` | Value |
| `f64` | `double` | Value |
| `bool` | `bool` (C99) | Value, `i1` zero-extended |
| `string` (parameter only) | `const char *` | Pointer to the first data byte, 8 bytes past the header |
| return type omitted | `void` | — |

The compiler adjusts the string pointer at each call site. C code receives an ordinary NUL-terminated string. A `string` may not be an extern return type, because C strings aren't allocated by the GC and have no length header.

**Rules**

- Extern functions are called exactly like Lugha functions.
- Variadic C functions such as `printf` cannot be declared in v0. Use the `print` and `println` intrinsics instead.
- The extern name is used as the C symbol name unchanged. Lugha functions themselves are emitted with a `lugha_fn_` prefix, so they never collide with C symbols. Methods and associated functions are `lugha_fn_<Type>.<name>` (for example `lugha_fn_Point.dist`); `.` can't appear in a Lugha name, so these can't collide either.
- Runtime library functions use the `lugha_rt_` prefix. Neither prefix is a prefix of the other, so a Lugha function can never collide with a runtime function, whatever its name.
- Extern names starting with `lugha_` are reserved and rejected with an E03xx error, so an extern declaration can't collide with either.
- Extern C code must not keep a Lugha string after the call returns, and must not write through the pointer (section 7).
- C's `libc` and `libm` are always linked, so their functions can be declared directly.

## 9. Compilation model

The compiler, `lughac`, is written in Rust with inkwell. It turns one `.la` file into one native executable, in six stages. Each stage stops the pipeline if it reports an error.

1. **Lex.** Source text → tokens with spans.
2. **Parse.** Tokens → AST. Recursive descent for items and statements, Pratt parsing for expressions.
3. **Check.** Resolve names, compute a type for every expression, check every rule in sections 4–6. Output: the AST plus a side table mapping each expression to its type.
4. **Lower.** Typed AST → LLVM IR via inkwell, one LLVM function per Lugha function. Run `module.verify()` afterwards; a verifier failure is a compiler bug, not a user error.
5. **Optimize and emit.** Run LLVM's pass pipeline (`default<O0>` or `default<O2>`), then write an object file with `TargetMachine`.
6. **Link.** Invoke the system C compiler, which also compiles the runtime: `cc prog.o lugha_rt.c -lgc -lm -o prog`. The runtime source is embedded in `lughac` and written to a private temporary file for each build.

**Lowering notes.** These are the places where Lugha semantics need specific IR:

- **Overflow checks:** lower `+`, `-` and `*` to the `llvm.sadd.with.overflow`, `llvm.ssub.with.overflow` and `llvm.smul.with.overflow` intrinsics (the `u` versions for `u8`). Each returns the result plus an overflow flag; branch to a panic call when the flag is set. Unary `-x` is a checked `0 - x`. At `-O2`, LLVM removes checks it can prove never fire.
- **Division:** compare the divisor to zero (and check `MIN / -1`) before `sdiv`/`srem`; branch to a panic call on failure.
- **Bounds checks:** load the length header, compare with `icmp ult` (an unsigned compare catches negative indexes too), branch to a panic call on failure.
- **Short-circuit `&&` and `||`:** separate basic blocks joined by a `phi`.
- **if expressions:** lower each branch to its own basic block, as for statements, and merge a non-`void` result with a `phi` in the join block. A branch that definitely returns contributes no incoming edge.
- **Float-to-int casts:** the `llvm.fptosi.sat` intrinsic gives the saturating behavior in section 4.
- **Opaque pointers:** every `load`, `store` and `getelementptr` needs its element type, taken from the checker's type table.
- **Array copies:** generate one deep-copy function per type that contains arrays (for example `lugha_copy_i64_arr_arr`, or `lugha_copy_5Point_arr` — struct names are length-prefixed so they can't be confused with `_arr` or primitive names; the `lugha_copy_` prefix is disjoint from `lugha_fn_` and `lugha_rt_`), and call it at each copy site listed in section 4. For arrays of plain values, the copy is one `lugha_rt_alloc` plus `llvm.memcpy`.
- **Struct, enum and array arguments:** pass a pointer to the caller's storage rather than the value itself, since the callee can't modify it.
- **Methods:** ordinary functions whose first parameter is the receiver. A `self` receiver is passed like a struct argument; a `mut self` receiver is the address of the caller's place itself, so the method's writes land there.
- **`match`:** an LLVM `switch` on the tag for enums and on the value for integers and `bool`s; a chain of `lugha_rt_str_eq` tests for strings. Payload bindings load from the payload area at constant offsets. An exhaustive `match` without `_` ends in `unreachable`.

**Runtime library.** A small C file, `lugha_rt.c`, is compiled once and linked into every program. The compiler calls only these functions:

| Function | Purpose |
| --- | --- |
| `lugha_rt_init()` | Runs `GC_INIT()`; the C `main` calls it first, since a C macro can't be called from IR |
| `lugha_rt_alloc(size)` | Allocate zeroed GC memory |
| `lugha_rt_panic(msg, file, line, col)` | Print the panic message and `exit(101)` |
| `lugha_rt_str_concat(a, b)` | Implement string `+` |
| `lugha_rt_panic_bounds(len, index, file, line, col)` | The out-of-bounds panic, with the length and index |
| `lugha_rt_str_eq(a, b)` | Implement string `==` and `!=`; returns 1 or 0 as `i32` |
| `lugha_rt_array_grow(array, elem_size)` | A copy of a full array with double the capacity (at least 4), for `push` |
| `lugha_rt_print_*` / `lugha_rt_to_string_*` | One per primitive type (plus `lugha_rt_print_str`), for the intrinsics; `bool` and `u8` arguments are passed zero-extended to 32 bits |
| `lugha_rt_print_newline()` | The newline `println` adds |

The runtime writes through C's `stdout` stream, so output from `print`/`println` and from libc functions such as `puts` appears in program order.

The C `main` emitted by the compiler calls `lugha_rt_init()` (which runs `GC_INIT()`), then `lugha_fn_main()`, and returns its exit code.

**Command-line interface**

| Command | Effect |
| --- | --- |
| `lughac build prog.la [-o prog]` | Compile and link an executable |
| `lughac run prog.la` | Build to a temporary file and run it |
| `lughac check prog.la` | Lex, parse and type-check only; no codegen or linking (type checking from milestone 3) |
| `lughac spec` | Print the LLM-ready spec bundled with this compiler version |
| `lughac build --emit=tokens\|ast\|ir prog.la` | Print an intermediate stage and stop |
| `--diagnostics=human\|json` | Diagnostic format for `build`, `run` and `check`; `human` is the default |
| `-O0` / `-O2` | Optimization level; `-O0` is the default |

Exit codes: 0 on success, 1 when the program has errors, 2 for bad command-line usage or an internal compiler error.

**Diagnostics.** Errors print the file, line and column, the offending source line, a caret under the span with its label, and any secondary labels, in a rustc-like layout rendered by the `codespan-reporting` crate. Each diagnostic is followed by a blank line, and lines carry no trailing whitespace. The checker reports as many errors as it can find instead of stopping at the first one.

Internal errors — an unreadable file, a failed link — have no code and exit with 2. They print as `error: <message>`, with a source snippet when they have a location. A crash inside the compiler itself is reported the same way, as `error: internal compiler error: <message>` with a note asking for a bug report, and also exits with 2.

**Error codes.** Every diagnostic has a stable code, grouped by stage. A code is never reused or renumbered once released; its message wording may improve.

| Range | Stage |
| --- | --- |
| E01xx | Lexing: bad characters, unterminated strings, invalid escapes, out-of-range literals |
| E02xx | Parsing: unexpected tokens, missing delimiters |
| E03xx | Names and scopes: undefined or duplicate names, recursive structs |
| E04xx | Types: mismatches, invalid operators, bad casts, wrong argument counts |
| E05xx | Mutability and control flow: assignment to immutable bindings, missing returns, `break` outside a loop |
| W01xx | Warnings: unreachable code |

Name and type errors, reported by the checker:

| Code | Error | Example |
| --- | --- | --- |
| E0301 | Undefined name | `count + 1` with no `count` in scope; a call to an undefined function |
| E0302 | Name defined more than once, or a function named like an intrinsic | two `fun area`; `fun println()` |
| E0303 | No `main` function | |
| E0304 | `main` with the wrong signature | `fun main(argc: i32): i32` |
| E0305 | Unknown type name | `let p: Point = 1;` with no `Point` |
| E0306 | Reserved extern name | `extern fun lugha_rt_alloc(size: i64): i64;` (§8) |
| E0307 | Struct contains itself | `struct A { b: B }` with `struct B { a: A }` — hold it in an array instead (`A[]`) |
| E0308 | Field declared twice | `struct P { x: i64, x: f64 }` |
| E0309 | Variant declared twice | `enum E { A, B, A }` |
| E0310 | Method declared twice, or named like a field or variant | `impl P { fun x(self) {} }` where `P` has a field `x` |
| E0311 | A local named like a type | `let Shape = 3;`, `fun f(Point: i64)` |
| E0401 | Numeric literal of the wrong kind for its expected type | `x + 2.5` where `x` is `i32` |
| E0402 | Integer literal out of range for its type | `let b: u8 = 256;` |
| E0403 | Type mismatch | `let n: i32 = ok;` where `ok` is `bool`; `if`/`else` of different types |
| E0404 | Operator applied to invalid operand types | `true + 1`, `-u` where `u` is `u8` |
| E0405 | Wrong number of arguments | `add(1)` for `fun add(a: i64, b: i64)` |
| E0406 | Not a function, or a function used as a value | `let step = 2; step(1);`, `let f = fib;` |
| E0407 | `void` used as a value | `let x = log();` where `log` returns nothing |
| E0410 | No such field | `s.size` on a `string`; `(5).len`; `p.z` or `Point { z: 1.0 }` on a `Point` without `z` |
| E0411 | Not indexable or not iterable | `n[0]` where `n` is an `i64`; `for c of "abc"` |
| E0412 | Cannot infer an array's element type, or `None`'s type | `let xs = [];`, `let x = None;` — annotate it: `let xs: i64[] = [];` |
| E0413 | Struct literal missing fields | `Point { x: 1.0 }` |
| E0414 | Struct literal field given twice | `Point { x: 1.0, x: 2.0, y: 0.0 }` |
| E0415 | No such variant or method | `Shape.Circel(1.0)`, a `Square` pattern on a `Shape`, `p.size()`, `xs.push(1)` |
| E0416 | Wrong number of payloads | `Shape.Rect(1.0)`, `Rect(w) => …` |
| E0417 | Non-exhaustive `match` | `match s { Circle(r) => r }` with more variants; an integer `match` without `_` |
| E0409 | Type not allowed in an extern signature | `extern fun getenv(key: string): string;` — no `string` returns; no array, struct, enum or `Option` parameters or returns (§8) |
| E0408 | Invalid cast | `ready as i32` where `ready` is `bool`; `x as bool` |
| E0501 | Assignment to an immutable binding | `let total = 0; total = 5;`; assigning to a parameter or loop variable; `xs[0] = 1;` where `xs` isn't `let mut` |
| E0502 | Assignment to something that isn't a place | `(a + b) = 3;` |
| E0503 | Missing return | a non-void function whose body can end without a value (§6); a stray `;` after the result gets a "remove this semicolon" label |
| E0504 | `break` or `continue` outside a loop | |
| E0505 | Block-like statement with a discarded value | `if big { 100 } else { 1 }` followed by more statements |
| E0506 | Assignment into a string | `s[0] = 104;`, `s.len = 2;` — strings are immutable (§4) |
| E0507 | Assignment to the array being iterated | `for x of xs { xs[0] = x; }`, or a `mut self` call on it |
| E0508 | Argument aliases a `mut self` receiver | `v.add(v)` where `add` takes `mut self` |
| W0101 | Unreachable code (warning) | statements after `return`, `break` or `continue`; reported once per block |
| W0102 | Unreachable `match` arm (warning) | an arm after `_`, or a variant already covered |

**JSON diagnostics.** With `--diagnostics=json`, the compiler writes one JSON object per line to stderr, one per diagnostic, and nothing else. Lines and columns are 1-based; columns and offsets count UTF-8 bytes. `label` is the text shown under the primary span, or `null`. `labels` holds secondary spans, and `help` is an optional suggestion. Internal errors have `"code":null`, and `"span":null` when they have no location.

```json
{"severity":"error","code":"E0401","message":"float literal where i32 expected","file":"main.la","span":{"start":{"line":3,"col":17,"offset":49},"end":{"line":3,"col":20,"offset":52}},"label":"expected i32","labels":[{"span":{"start":{"line":3,"col":13,"offset":45},"end":{"line":3,"col":14,"offset":46}},"message":"this operand is i32"}],"help":null}
```

Both formats come from the same internal diagnostic records, so they always report the same errors.

**LLM-ready spec.** No model has seen Lugha in training, so the compiler ships a single Markdown file containing this spec and the section 10 programs, compact enough to paste into a prompt. `lughac spec` prints the copy matching the installed compiler, so an assistant or harness always gets the rules for the version it is targeting.

## 10. Example programs

These programs are the v0 acceptance tests: a conforming compiler builds and runs each one, and its stdout and exit code must match exactly. Every expected output ends with a newline.

**Hello world** (milestone 4). Prints `Hello, world!`, exits with 0.

```
fun main() {
    println("Hello, world!");
}
```

**Recursion** (milestone 4). Prints `832040`, exits with 0.

```
fun fib(n: i64): i64 = if n < 2 { n } else { fib(n - 1) + fib(n - 2) };

fun main(): i32 {
    println(fib(30));
    0
}
```

**Arrays, repeat literals and mutability** (milestone 5). Prints `25`, exits with 0. `is_composite` needs `mut` because its elements are assigned, just as `count` and `j` need it to be reassigned.

```
fun count_primes(limit: i64): i64 {
    let mut is_composite = [false; limit + 1];
    let mut count = 0;
    for i in 2..limit + 1 {
        if !is_composite[i] {
            count += 1;
            let mut j = i * i;
            while j <= limit {
                is_composite[j] = true;
                j += i;
            }
        }
    }
    count
}

fun main() {
    println(count_primes(100));
}
```

**Structs, for-of and casts** (milestone 5). Prints `centroid: 2.0, 1.0`, exits with 0.

```
struct Point { x: f64, y: f64 }

fun centroid(pts: Point[]): Point {
    let mut sx = 0.0;
    let mut sy = 0.0;
    for p of pts {
        sx += p.x;
        sy += p.y;
    }
    let n = pts.len as f64;
    Point { x: sx / n, y: sy / n }
}

fun main() {
    let pts = [
        Point { x: 0.0, y: 0.0 },
        Point { x: 4.0, y: 0.0 },
        Point { x: 2.0, y: 3.0 },
    ];
    let c = centroid(pts);
    println("centroid: " + to_string(c.x) + ", " + to_string(c.y));
}
```

**Calling C** (milestone 4). Prints `hello from libc` then `1.4142135623730951` on two lines, exits with 0.

```
extern fun puts(s: string): i32;
extern fun sqrt(x: f64): f64;

fun main() {
    puts("hello from libc");
    println(sqrt(2.0));
}
```

**Enums and match** (milestone 6). Prints `12.56636` then `6.0` then `0.0` on three lines, exits with 0.

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

**A rejected enum program** (milestone 6). The checker reports the missing variant, and `lughac` exits with 1:

```
enum Shape { Circle(f64), Empty }

fun area(s: Shape): f64 = match s {
    Circle(r) => r * r,
};

fun main() {}
```

```
error[E0417]: non-exhaustive match: `Empty` not covered
  --> main.la:3:27
  |
3 | fun area(s: Shape): f64 = match s {
  |                           ^^^^^^^
  |
  = help: add the missing arms, or `_ => …`
```

**A rejected program** (milestone 3). The checker must report this error with its location, and `lughac` exits with 1:

```
fun main() {
    let x: i32 = 5;
    let y = x + 2.5;
}
```

```
error[E0401]: float literal where i32 expected
  --> main.la:3:17
  |
3 |     let y = x + 2.5;
  |             -   ^^^ expected i32
  |             |
  |             this operand is i32
```

With `--diagnostics=json`, the same error is the JSON line shown in section 9.

## 11. Milestones and scope

The spec is implemented in five milestones, each ending in a program that runs. Build them in order; each one only adds to the last.

| # | Milestone | Spec subset | Done when |
| --- | --- | --- | --- |
| 1 | Expressions to a binary | Integer literals and arithmetic in `fun main(): i32`; lexer, parser, codegen, linking | `fun main(): i32 { 2 + 3 * 4 }` exits with 14 |
| 2 | Variables, control flow, functions | `let`/`let mut`, assignment, blocks with tail values, `if` expressions, `while`, `for` over ranges, optional condition parentheses, `=` function bodies, calls, recursion; integers only, all treated as `i64` | The program below exits with 55 |
| 3 | Type checker and diagnostics | All primitive types, literal inference, casts, mutability checks, return checking; error codes, `--diagnostics=json`, `lughac check`, "remove this semicolon" | The rejected program in section 10 reports E0401 in both formats |
| 4 | Runtime and C | `lugha_rt.c`, intrinsics, `extern fun`, string literals, integer overflow and division checks (panics) | Hello world and the libc example run |
| 5 | Heap data | Boehm GC, strings, arrays, structs, array copies, `for`-`of`, bounds checks, panics; `lughac spec` | Every program in section 10 passes |
| 6 | Enums (v1) | `enum`, construction, `match` with flat patterns, exhaustiveness, `Option<T>`, recursive enums | The enum programs in section 10 pass (done) |
| 7 | Methods (v1) | `impl`, `self`/`mut self`, associated functions, method calls | The methods program in section 10 passes (done) |
| 8 | Growable arrays (v1) | `push`, `pop`, `clear` on `T[]` | The stack program in section 10 passes (done: v1 complete) |

In milestones 1 and 2, every value can be treated as `i64`, so codegen can start before the type checker exists. Until milestone 4 provides `lugha_rt_panic`, integer `+`, `-` and `*` wrap, and `/` and `%` stop the program with a trap on a zero divisor or `MIN / -1`. Milestone 3 then replaces that assumption with real types. The process exit code is the low 8 bits of `main`'s result, so milestone tests use results below 256.

The milestone 2 test program. It uses `i32` throughout so it stays valid once milestone 3 adds real types:

```
fun fib(n: i32): i32 = if n < 2 { n } else { fib(n - 1) + fib(n - 2) };

fun main(): i32 {
    let n: i32 = 3;
    let mut total: i32 = 0;
    for i in 0..n {
        total += i;
    }
    fib(10) + total - n
}
```

**Out of scope for v1**, roughly in the order they're worth adding (enums, `match`, methods and growable arrays are v1, above):

1. **Truly shared data.** Graphs and other shared structures need a separate opt-in reference type; everything else in Lugha is a value.
2. **Modules and imports.** Multiple source files.
3. **Generics.** Monomorphized generic functions and structs (v1 has only the built-in `Option<T>`).
4. **Closures.** Requires capturing variables, which the GC makes easier.
5. **Richer patterns.** Nested patterns and `if` guards in `match`; `==` on enums and structs.
6. **Smaller additions.** Explicit wrapping arithmetic for hashing and random-number code (such as `wrapping_add`), string interpolation, bitwise operators, `u32`/`u64`/`f32`, variadic externs, command-line arguments.

## 12. Open questions

These choices were made provisionally to keep the spec complete. Each is easy to change before implementation starts.

- [x] **Return type syntax.** `fun f(): i32` (TypeScript) was chosen over `fun f() -> i32` (Rust) so that `:` always introduces a type.
- [x] **Cost of array copies.** Decided: eager deep copies, measured on realistic programs. Copy-on-write would rarely help, because most copied arrays are written right away, so it would copy anyway. It would also need a reference count on every array, which Boehm doesn't provide. To change an array inside a struct or another array, write through the place (`ps[i].pos[0] += 1.0`) rather than copying the element out and back in.
- [x] **Default integer type.** Decided: unannotated integer literals are `i64`. This matches `.len` and array indexes, so loop counters need no casts, and it pushes the overflow trap out to about 9.2 quintillion. Use `i32` explicitly where memory in large arrays matters.
- [x] **Integer overflow.** Decided: always trap, like Swift, rather than wrapping or Rust's debug-only checks. One behavior in every build, at a cost of a few percent. Explicit wrapping operations can come later.
- [x] **Semicolons.** Required, which keeps the parser simple. TypeScript-style automatic semicolon insertion is possible later but adds edge cases.
- [ ] **Literal patterns on `f64`.** Excluded from v1: float equality in patterns invites mistakes. Revisit if needed.
- [x] **Array growth factor.** Decided: capacity doubles, starting at 4. It was adequate on the stack program and the DECISION-010 programs; revisit only with measurements.
- [ ] **`mut` bindings in `match`.** Bindings are borrowed and immutable; no program has needed `mut` ones yet.
- [x] **Expression-bodied functions.** Decided: adopted (section 6). `fun square(x: i64): i64 = x * x;` drops the braces from one-liners. It is a small parser addition and a second way to write the same function, accepted because one-liners are common and it is pure sugar for `{ e }`.
