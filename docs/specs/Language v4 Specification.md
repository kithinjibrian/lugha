# Lugha v4 Specification

Oct 13, 2026 · @kithinji

**Status:** complete (milestones 17–19). It is built from the approved plan, `Language v4 Draft.md`. v4.1 allows string literals inside interpolations. This file describes exactly what `lughac` implements, and is what `lughac spec` prints. `Language v3 Specification.md` keeps v3 frozen.

**v3.** This is v2 plus command-line arguments and number parsing, string interpolation, functions as values, closures, and array helpers (milestones 13–16).

**Changes since v3** (milestones 17–19):
- `Result<T, E>` is predeclared, with `Ok(x)` and `Err(e)` written without a prefix (section 4).
- The postfix `?` gives a `Result`'s `Ok` value or an `Option`'s `Some` value, or returns the failure from the function at once (sections 2–4).
- Strings have `lines`, `split`, `trim`, `contains`, `starts_with`, `ends_with` and `slice`, `string[]` has `join`, and `Option` and `Result` have `unwrap_or`, `ok_or`, `ok`, `map`, `map_err` and `is_*` (section 4).
- Programs read stdin with `read_line`, read and write text files with `read_file`, `write_file` and `append_file`, write to stderr with `eprint`/`eprintln`, and stop with `exit(code)` (section 5).

Every valid v3 program is a valid v4 program unless it defines something named `Result`, `Ok` or `Err`, or a function named `read_line`, `read_file`, `write_file`, `append_file`, `eprint`, `eprintln` or `exit`.

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
import  private  public  interface
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

**String interpolation** (v3). Inside a string literal, `{` starts an interpolation that runs to its matching `}`: `"{name} is {age * 2}"`. Braces inside the expression nest, so `"{if c { 1 } else { 2 }}"` works. The expression may contain string literals, which may interpolate in turn (v4.1): `"<{parts.join("-")}>"`, `"{if ok { "yes" } else { "no" }}"`, `"{name}: {"{a}/{b}"}"`. `{{` and `}}` are literal braces. The lexer emits such a string as its literal parts (`STRING_START`, `STRING_MID`, `STRING_END`) with the expression's tokens between them, a nested string's tokens among them; escapes apply in every part. Errors (E0111): a lone `}`, a `{` never closed, or an empty `{}`. Strings stay on one line: a line break ends a string that began inside an interpolation with E0111 at that interpolation's `{`, and otherwise ends the string (E0102).

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
| E0111 | Bad interpolation in a string (v3) | `"a } b"`, `"{x"`, `"{}"` — write `{{`/`}}` for literal braces |

**Operators and punctuation.** The lexer uses longest match, so `<=` is one token, not `<` then `=`.

```
+  -  *  /  %  ==  !=  <  <=  >  >=  &&  ||  !
=  +=  -=  *=  /=
(  )  {  }  [  ]  ,  ;  :  .  ..  =>  ?
```

`::` is lexed as a token only so the parser can reject it helpfully (E0207): Lugha writes `Shape.Circle`, not `Shape::Circle`.

## 3. Grammar

The grammar is LL(1) apart from expressions, which use Pratt parsing with the precedence table below. Parentheses around the condition of `if` and `while`, and around a `for` header, are optional: `if x > 0 {` and `if (x > 0) {` are both valid.

```ebnf
program      = { import } { item } EOF ;
import       = "import" IDENT { "." IDENT } ";" ;
item         = [ visibility ] ( fun_decl | extern_decl | struct_decl | enum_decl | interface_decl ) | impl_decl ;
visibility   = "private" | "public" ;                (* `public`: E0317 until packages *)

fun_decl     = "fun" IDENT [ type_params ] "(" [ params ] ")" [ ":" type ] fun_body ;
fun_body     = block | "=" expr ";" ;          (* "= e;" means "{ e }" *)
extern_decl  = "extern" "fun" IDENT "(" [ params ] ")" [ ":" type ] ";" ;
struct_decl  = "struct" IDENT [ type_params ] "{" [ field { "," field } [ "," ] ] "}" ;
field        = [ visibility ] IDENT ":" type ;
enum_decl    = "enum" IDENT [ type_params ] "{" [ variant { "," variant } [ "," ] ] "}" ;
impl_decl    = "impl" IDENT [ "<" IDENT { "," IDENT } ">" ] "{" { [ visibility ] method } "}" ;
method       = "fun" IDENT [ type_params ] "(" [ receiver [ "," params ] | params ] ")" [ ":" type ] fun_body ;
type_params  = "<" type_param { "," type_param } [ "," ] ">" ;
type_param   = IDENT [ ":" bound { "+" bound } ] ;
bound        = "Eq" | "Ord" | IDENT [ "." IDENT ] ;   (* an interface, `m.Shape` from module `m` *)
interface_decl = "interface" IDENT "{" { iface_item } "}" ;
iface_item   = "fun" IDENT "(" [ receiver [ "," params ] | params ] ")" [ ":" type ] ";" ;
receiver     = [ "mut" ] "self" ;
variant      = IDENT [ "(" type { "," type } [ "," ] ")" ] ;
params       = param { "," param } [ "," ] ;
param        = IDENT ":" type ;

type         = base_type { "[" "]" } ;
base_type    = "i32" | "i64" | "u8" | "f64" | "bool" | "string"
             | IDENT [ "." IDENT ] [ "<" type { "," type } [ "," ] ">" ]
             | "fun" "(" [ type { "," type } [ "," ] ] ")" [ ":" type ]   (* a function type (v3) *)
             | "(" type ")" ;                  (* `(fun(i64): i64)[]` is an array of functions *)
                                           (* `m.T`, `Stack<i64>`; `Option<T>` is one such *)

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
arm          = pattern [ "if" cond ] "=>" expr ;
arm_sep      = "," | (* nothing, after an arm whose expr is a block *) ;
pattern      = "_"
             | IDENT [ "(" pattern { "," pattern } [ "," ] ")" ]
             | [ "-" ] INT | STRING | "true" | "false" ;

expr         = prefix { infix } ;            (* Pratt loop, see table *)
prefix       = ( "-" | "!" ) prefix | postfix ;   (* binds tighter than infix *)
postfix      = primary { call | index | member | "?" } ;   (* `read(p)?` (v4) *)
call         = "(" [ expr { "," expr } [ "," ] ] ")" ;
index        = "[" expr "]" ;
member       = "." IDENT ;
primary      = INT | FLOAT | STRING | interpolated | closure | "true" | "false"
             | IDENT [ "." IDENT ] [ struct_init ]  (* `m.T { … }` *)
             | "self"
             | "(" expr ")"
             | "[" [ expr ( ";" expr | { "," expr } [ "," ] ) ] "]"
             | block_like ;
interpolated = STRING_START expr { STRING_MID expr } STRING_END ;   (* "a {x} b {y} c" *)
closure      = "fun" "(" [ cparam { "," cparam } [ "," ] ] ")" [ ":" type ] ( block | "=" expr ) ;   (* v3 *)
cparam       = IDENT [ ":" type ] ;
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

**Enum values and `match`.** `Shape.Circle(2.0)` and `Shape.Empty` need no grammar of their own: they parse as a member access, followed by a call when there are payloads, and the checker decides they name a variant (section 4). A `match` scrutinee is a `cond`, parsed in no-struct-literal mode like an `if` condition. At the top level a bare `IDENT` pattern must name a variant; there are no top-level binding patterns, so use `_`. Inside a payload slot, a bare `IDENT` is a variant if the slot's type is an enum with a variant of that name (`Some(Empty)`), and otherwise a new binding (`Some(x)`). A guard after `if` is parsed like a `cond`.

**Generic headers.** Type parameters follow the name, like Rust and TypeScript: `fun max<T: Ord>(a: T, b: T): T`. An `impl` names its type's parameters without bounds, `impl Stack<T> { … }`; the bounds come from the type's declaration. Type arguments are never written at a use (section 4), so `<` after a name in an expression is always a comparison; `Stack::<i64>` is E0207.

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
| E0208 | `import` after an item | `fun f() {}` then `import geometry;` (v2) |
| E0209 | A closure parameter that isn't a plain name | `fun(self) = 1`, `fun(mut x) = x` (v3) |
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

**Enums.** Declared at top level, in any order, as a list of variants that each carry zero or more positional payloads: `enum Shape { Circle(f64), Rect(f64, f64), Empty }`. A variant name is unique within its enum (E0309). A value is built through the enum's name with `.`: `Shape.Circle(2.0)`, or `Shape.Empty` for a variant without payloads. Each payload expects its declared type, and the count must match (E0416); an unknown variant is E0415. `==` compares two enum values by variant and then payload by payload; there is no other operator on enums (E0404). A payload can never be changed in place — there is no syntax for it, and `match` bindings are immutable — so copying an enum shares its payloads, and the copies that matter happen when a payload is stored: building a variant from a place copies it (`Job.Batch(xs)`), and storing a `match` binding copies it (section 4, Array copies). 

**Recursive data.** An enum may contain itself, directly or through structs, other enums or `Option`: `enum List { Nil, Cons(i64, List) }` builds a linked list, and `enum Tree { Leaf, Node(Tree, i64, Tree) }` a binary tree. Nothing has to be annotated and there is no `Box` (writing `Box<List>` is E0305 with a hint). The compiler stores each payload slot that leads back to its enum behind a hidden pointer (section 7), so the type has a finite size, values like `List.Nil` cost nothing, and — since payloads are shared — copying a list of any length takes constant time. A program can't observe the pointer: in `Cons(x, rest)`, `rest` is an ordinary `List` value.

**`Option`.** `Option<T>` is a generic enum predeclared in every module as `enum Option<T> { Some(T), None }`. Its variants are written without a prefix. `Some(x)` has type `Option<T>` where `x : T`, or the expected `Option` type when there is one. `None` needs an expected type, exactly like `[]`: `let x: Option<i64> = None;` — otherwise it is E0412.

**`match`.** `match e { pattern => value, … }` is an expression. A pattern is `_`, an integer, `bool` or string literal, or a variant written without the enum's name. A variant's payload slots are patterns too, so they nest: `Circle(r)`, `Rect(w, _)`, `Empty`, `Some(Circle(r))`, `Pair(1, Some("a"))`. Variant patterns need an enum or `Option` value and literal patterns a value of their type, at any depth (E0403); a variant needs one slot per payload (E0416). An arm may have a guard, `pattern if cond => value`: the guard runs after the pattern matches, with its bindings in scope, and must be `bool` (E0403); if it is false, matching continues with the next arm. The arms' values follow the rules of `if`/`else`: they must agree (E0403), and an arm that never finishes fits any type.

- **Exhaustiveness.** A `match` must cover every value, counting only arms without guards. Otherwise it is E0417, which lists up to three missing cases as patterns (`Some(Rect(_, _))`, `None`) and then "and N more"; a missing variant with any payloads shows as its bare name at the top level (`Rect`). A `bool` is covered by `true` and `false`. Integers and strings, at the top level or inside a variant, always need a `_` or a binding.
- **Unreachable arms.** An arm that no value can reach past the earlier unguarded arms is warning W0102: after `_`, after the same pattern, or after arms that together cover it (`Some(_)` and `None` before `Some(Empty)`).
- **Bindings.** Payload names are immutable and borrowed, like `for … of` variables: storing or returning one copies it. Each is in scope in its own arm (guard and value) only. A binding that its arm never uses is warning W0103, which also catches a misspelled variant name that became a binding.

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
| `swap(a, b)` | No | The two places exchange their contents (section 5) |
| A fresh value: literal, call result, string concatenation | No | Nothing else refers to it |

A function's tail value follows the same rules as `return`: a body ending in `p` copies, and one ending in `xs` moves. "From a place" looks through `if` and block tails: `let ys = if c { xs } else { [0] };` copies. When the value comes from a local that is never used again, the compiler may move it instead of copying; no program can tell the difference.

Copies are deep. Copying an `i64[][]` copies every inner array, and copying a struct copies the arrays inside it. Strings inside are shared, since they are immutable.

**Strings.** Immutable. `.len` gives the length in bytes as `i64`. `s[i]` reads one byte as `u8`, bounds-checked. `+` concatenates two strings into a new one. `==` and `!=` compare contents. `<`, `<=`, `>` and `>=` compare bytewise: the first differing byte decides, compared as an unsigned value, and a proper prefix is smaller (`"ab" < "abc"`). This is the order of C's `memcmp` and needs no Unicode tables.

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
| `<` `<=` `>` `>=` | Two numbers of the same type, or two strings | `bool` |
| `==` `!=` | Two values of the same type | `bool` |
| `&&` `\|\|` | Two `bool` | `bool` |
| `!` | `bool` | `bool` |
| unary `-` | `i32`, `i64` or `f64` | Same type |

**Equality on every type.** `==` and `!=` compare deeply:
- **Primitives and strings:** as before.
- **Structs:** field by field.
- **Enums:** same variant, then payload by payload.
- **Arrays:** same `.len`, then element by element.
- **Option:** `None == None`, and `Some(a) == Some(b)` when `a == b`.

Equality costs time proportional to the values compared. `f64` keeps IEEE semantics, so a `NaN` anywhere makes the values unequal, even a value compared with itself.

**Operand types.** In `==`, `!=` and the comparisons, an operand that can't be typed on its own (a number literal, `None`, `[]`, or a list or `Some` around one) takes its type from the other operand, whichever side it is on. So `o == None`, `None == o` and `xs == []` work. If neither side can be typed alone (`None == None`), it is E0412. A number literal never takes a non-numeric type this way: `"a" == 1` is E0404.

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

**`Result`** (v4). `Result<T, E>` is predeclared in every module as `enum Result<T, E> { Ok(T), Err(E) }`, an ordinary generic enum.
- **Construction:** `Ok(x)` and `Err(e)` are written without a prefix, like `Some`. The type argument the payload doesn't give comes from the expected type (`let r: Result<i64, string> = Ok(1);`); `Ok(1)` alone is E0412.
- **Everything else** is as for any enum: `match` with `Ok(x)` and `Err(e)` patterns (exhaustiveness, E0417), `==` when `T` and `E` are `Eq`, copies and layout.
- **Names:** `Result`, `Ok` and `Err` are built-in names: declaring one is E0302, and a local named `Result` is E0311. A local named `Ok` or `Err` shadows the constructor, as one named `Some` does. Messages print the type as `Result<i64, string>`.

**The `?` operator** (v4). `e?` evaluates `e` once.
- **On `Result<T, E>`:** its type is `T`. If `e` is `Err(x)`, the enclosing function returns `Err(x)` at once. The function must return `Result<U, E>` with the same `E`; there are no conversions.
- **On `Option<T>`:** its type is `T`. If `e` is `None`, the function returns `None`. The function must return an `Option`.
- **In a closure**, it returns from the closure, whose return type must be written or expected (E0412 otherwise, as for `return`).
- **Misuse is E0424**, with help: `?` on any other type, in `main`, in a function returning something else, on an `Option` in a function returning `Result`, or with different error types.
- **Early return:** on failure nothing after the `?` runs, and a value it was part of is never stored, exactly as for `return`.

**String, `Option` and `Result` helpers** (v4). These methods each take `self`, never change it, and return a new value.

| Receiver | Method | Result |
| --- | --- | --- |
| `string` | `s.lines()` | `string[]`: split at `\n`, one trailing `\r` removed from each line, and no empty last line after a final `\n` (`"a\nb\n"` gives `["a", "b"]`, `""` gives `[]`) |
| `string` | `s.split(sep)` | `string[]`: the parts between occurrences of `sep`, empty parts kept (`"a,,b"` gives `["a", "", "b"]`); an empty `sep` panics with `split with an empty separator` |
| `string` | `s.trim()` | `string`: without leading and trailing ASCII whitespace (space, `\t`, `\n`, `\r`) |
| `string` | `s.contains(sub)`, `s.starts_with(p)`, `s.ends_with(p)` | `bool`; true for an empty argument |
| `string` | `s.slice(from, to)` | `string`: the bytes `from..to`; panics (at the call) unless `0 <= from <= to <= s.len` |
| `string[]` | `xs.join(sep)` | `string`: the elements with `sep` between them; `""` for `[]` |
| `Option<T>` | `o.unwrap_or(d)` | `T`: the value, or `d` |
| `Option<T>` | `o.ok_or(e)` | `Result<T, E>`: `Ok` with the value, or `Err(e)` |
| `Option<T>` | `o.map(f)` | `Option<U>` |
| `Option<T>` | `o.is_some()`, `o.is_none()` | `bool` |
| `Result<T, E>` | `r.unwrap_or(d)` | `T`: the `Ok` value, or `d` |
| `Result<T, E>` | `r.ok()` | `Option<T>` |
| `Result<T, E>` | `r.map(f)`, `r.map_err(f)` | `Result<U, E>`, `Result<T, F>` |
| `Result<T, E>` | `r.is_ok()`, `r.is_err()` | `bool` |

Strings are bytes: every position is a byte offset, and `slice` can cut a multi-byte character, giving a string of exactly those bytes. A closure argument is typed as for the array helpers (`o.map(fun(x) = "{x}")` is an `Option<string>`). `map` means the array, `Option` or `Result` helper according to the receiver. `parse_i64(s).ok_or("bad number")?` turns a missing value into an error and returns it. There is no `unwrap()`: handle a failure with `match`, `?` or `unwrap_or`.

**Array helpers** (v3). Every array `xs: T[]` also has these methods. Each takes `self`: the receiver never changes, and every array it returns is new.

| Method | Arguments | Result |
| --- | --- | --- |
| `xs.map(f)` | `f: fun(T): U` | `U[]`: `f` applied to each element, in order |
| `xs.filter(f)` | `f: fun(T): bool` | `T[]`: the elements for which `f` is true, in order |
| `xs.fold(init, f)` | `init: A`, `f: fun(A, T): A` | `A`: `f(…f(f(init, xs[0]), xs[1])…)`, left to right |
| `xs.sort_by(less)` | `less: fun(T, T): bool` | `T[]`: sorted so that `less` holds between neighbours or neither way, stably (equal elements keep their order) |
| `xs.any(f)` / `xs.all(f)` | `f: fun(T): bool` | `bool`, stopping at the first element that decides |

`T` comes from the receiver, and the other type arguments (`U`, `A`) are inferred as for any generic call, so a closure argument needs no written types: `xs.map(fun(x) = "{x}")` is a `string[]`, its result type coming from the closure's body. Arity and argument types are checked at the call (E0405, E0403). `f` runs once per element it is given, in order; a panic inside it stops the program as usual. If `less` is inconsistent, `sort_by` still returns every element exactly once, in some order. The helpers chain: `people.filter(fun(p) = p.age >= 18).map(fun(p) = p.name)`. Functions named `map`, `filter` and so on in a program are unaffected: the helpers exist only as array methods.

**Generic declarations.** `fun`, `struct`, `enum` and methods can take type parameters.
- **Scope:** a type parameter is a type inside its declaration (fields, payloads, signature and body), and a method also sees its `impl`'s parameters. It may not be named like a type, a module or another parameter of the same declaration, nor a method's like its `impl`'s (E0316).
- **Bounds:** `T: Eq`, `T: Ord`, interfaces (below), or several (`T: Shape + Eq`). Anything else is E0315. `Ord` doesn't imply `Eq`; write both if both are used.
- **What a bare `T` allows:** being stored, moved, copied, passed, returned and put in arrays, structs and enums. `==` needs `T: Eq`, `<` needs `T: Ord`, and a method call needs an interface bound that declares it. Using `==` or `<` without its bound is E0404, with help suggesting the bound.
- **What `T` never allows:** arithmetic, casts, printing, `.len`, or indexing. Those need a concrete type.
- **Checked once:** a generic body is checked once, with its parameters abstract, so every instance compiles.

**Using a generic.**
- **Type arguments:** a generic type is written with all its arguments in type positions: `Stack<i64>`, `Option<Vec2>`, `Pair<string, f64>`. A wrong count, or a bare `Stack` in a type position, is E0418.
- **Inference:** at a call, a struct literal, a variant or an associated function, the type arguments are inferred, never written. They come first from the expected type, then from the arguments that can be typed alone, then from the ones that can't (number literals, `None`, `[]`), so `max(x, 3)` with `x: u8` makes `3` a `u8`. A method's `impl` parameters come from its receiver: `s.push(1)` on a `Stack<string>` is E0401.
- **Can't infer:** a parameter still undecided is E0412, with help to annotate: `let s: Stack<i64> = Stack.new();`.
- **Bounds at a use:** a type argument that doesn't satisfy its bound is E0419. For example, `largest([[1]])` fails because `i64[]` is not `Ord`. Every type is `Eq`, so only `Ord` can fail: it holds for the numbers, `string`, and type parameters declared `Ord`.

**Interfaces.**
- **Declaring one:** `interface Shape { fun area(self): f64; fun name(self): string; }` lists methods (with a `self` or `mut self` receiver) and associated functions (without one), each ending in `;`. Their signatures use concrete types only: no type parameters and no `Self`. An item name appears once (E0310).
- **Satisfying one:** a type satisfies `Shape` if, for every item, it has a method or associated function with the same name, the same receiver kind, and the same parameter and return types, visible where the bound is checked. No declaration is needed. A generic type's methods count with its type arguments substituted (`Stack<i64>` meets `fun size(self): i64` through `impl Stack<T>`); a method with type parameters of its own never matches. A type parameter satisfies an interface when its own bounds declare every item the same way. Primitives, strings, arrays and `Option` have no methods of their own, so they satisfy only an interface with no items.
- **Using one as a bound:** `fun total<T: Shape>(xs: T[]): f64`, combined freely with `Eq`, `Ord` and other interfaces (`T: Shape + Eq`), or from another module (`T: shapes.Shape`). In the body, `x.area()` and, for an associated item, `T.make(…)` are allowed when some bound declares them, and are checked against its signature. Calling something no bound declares is E0421, with help suggesting a bound. Two bounds of one parameter may declare the same name only with the same signature; otherwise the bound list is E0423.
- **At a use:** a type argument that doesn't satisfy an interface bound is E0422, which names each missing or mismatched item.
- **Not a type:** an interface is never the type of a value; `let s: Shape = …` is E0305, with help to use it as a bound.
- **Names and visibility:** interfaces share their module's item namespace (E0302) and follow the visibility rules of section 6. Each instance calls the concrete type's own method, so there is no dispatch at run time.

**Function types** (v3). `fun(A, B): R` is the type of a function value taking `A` and `B` and returning `R`; `fun(A)` returns nothing. Two function types are the same when their parameter and return types are. A return type takes any `[]` after it, so `fun(i64): i64[]` returns an array, and an array of functions is written `(fun(i64): i64)[]`; messages print types the same way.
- **Equality:** function values have no `==` (E0404), and neither does a struct, enum, array or `Option` that holds one anywhere. So a function type, or a type holding one, doesn't satisfy `Eq` (E0419).
- **Order, printing and casts:** function values have no `<`, can't be printed or interpolated (E0403), and can't be cast (E0408).
- **Copying:** a function value is immutable, so a copy is the same value.
- **Generics and interfaces:** a function type can be a type argument (`apply<T>(f: fun(T): T, x: T)`), a field, an element or a payload. It has no methods, so it satisfies only an interface with no items.

**Functions as values** (v3).
- **Named functions:** a function used without a call is a value of its function type: `let f = double;`, `apply(double, 3)`, `m.f` from an imported module (visibility applies, E0313).
- **Associated functions** are values too: `let make = Point.at;`, `Circle.make`.
- **Generic functions** need an expected function type to fix their type arguments: `let m: fun(i64, i64): i64 = max;`. Otherwise it is E0412.
- **Not values:** methods with a receiver (call them on a value), externs and the intrinsics. Using one as a value is E0406, with help.

**Calls** (v3). `e(args)` calls any expression `e` of function type: a local, a parameter, an element (`fs[0](3)`), a call's result (`make()(3)`). `s.f(x)` calls method `f` if the type has one, otherwise field `f` if it holds a function; a method and a field never share a name (E0310), so this is never ambiguous. Arity (E0405) and argument types (E0403) are checked against the function type. Calling a value that isn't a function is E0406. A local of function type shadows a function of the same name, as any local does.

**Closures** (v3). `fun(x) = x + k` and `fun(x: i64): i64 { … }` are function values written inline: a function without a name. An `= e` body takes the longest expression, so `sort(xs, fun(a, b) = a > b)` ends at the `)`.
- **Parameter types** are written, or come from an expected function type in the same position (`let g: fun(i64): i64 = fun(x) = x + 1;`, or a parameter of a called function). Otherwise it is E0412, with help to write the type. In a generic call, a closure whose parameter types aren't written is typed last, so `apply(fun(x) = x + 1, 41)` makes `T` an `i64` first.
- **The return type** is written, or the expected one, or the body's type. A closure that uses `return` needs its return type written or expected (E0412).
- **Its own function:** `return` leaves the closure, and `break` and `continue` reach only loops inside it (E0504 otherwise).
- **Captures:**
  - A closure captures every local, parameter or `self` of an enclosing function that its body mentions.
  - Each is copied when the closure is created, in order of first mention, by the copy rules above; arrays are always deep-copied.
  - The closure reads its copies and never changes them: assigning a captured variable, one of its fields or elements, calling a `mut self` method or `push`/`pop`/`clear` on it, or `swap`ping it is E0511, with help to return a value instead.
  - So changing the outer variable later doesn't change the closure, and calling the closure never changes the caller's variables.
  - A closure's own `let mut` locals are ordinary.
- **No recursion:** a closure has no name, so it can't call itself; use a named function.

**Generic recursion.** A generic struct or enum may contain itself only with its own parameters unchanged: `enum List<T> { Nil, Cons(T, List<T>) }` is fine, and the indirect-slot rule (section 7) applies to each instance. Instantiation must end: a program whose instances nest more than 64 deep, such as `f<T>` calling `f<T[]>` or a `struct S<T>` holding `S<T[]>`, is E0420. Recursion at the same arguments (`count<T>` calling `count<T>`) never counts. Generic code copies exactly where concrete code would, because each instance is concrete (Array copies, above).

**Mutability.** Bindings are immutable by default. Assigning to any place requires the variable at its root to be declared `let mut`. That covers the variable itself, a field (`p.x = 1.0`), an element (`xs[0] = 1`) and any chain of these (`shapes[i].pts[j].x = 2.0`), and both places of a `swap` (section 5). Because every variable owns its data, a variable declared without `mut` never changes after it is initialized.

## 5. Expressions and statements

Evaluation is strictly left to right: operands, call arguments and struct-literal fields are evaluated in source order. The only exceptions are `&&` and `||`, which skip their right operand when the left decides the result.

**Arithmetic.** Integer `+`, `-`, `*` and unary `-` panic on overflow: if the true result doesn't fit the type, the program stops with `integer overflow` instead of continuing with a wrong value. Integer `/` and `%` panic when the divisor is zero, and also for `MIN / -1`, which overflows. `%` takes the sign of the dividend, like C and Rust. Compound assignments such as `+=` are checked the same way. Float arithmetic follows IEEE 754: overflow gives an infinity and division by zero gives an infinity or NaN, never a panic. Explicit casts are the one place values wrap: narrowing with `as` truncates (section 4).

**Method calls.** `x.m(a, b)` evaluates `x`, then `a`, then `b`; `xs.push(v)` evaluates the place `xs`, then `v`. For a `mut self` call the place `x` is evaluated once, before the arguments, like an assignment's place.

**`match` evaluation.** A `match` evaluates its scrutinee once, then tests the arms from top to bottom and runs the first that matches, outside in for nested patterns; a guard runs only once its pattern has matched. A variant's payloads are evaluated left to right when it is built.

**Compound assignment.** `x op= e` means `x = x op e`, except that the place `x` is evaluated once. So `xs[next()] += 1` calls `next()` once.

**Evaluation of `==`.** Both operands are evaluated left to right, then compared. Structs and arrays are compared in field or element order, stopping at the first difference. A comparison never panics.

**Changing what a statement writes.** An assignment, a `swap` and a `mut self` call each fix their place before the rest of the statement runs: the value, the arguments, and the place's own indexes. The rest may not change anything that contains that place (E0510), because growing, shrinking or replacing an array would leave the write aimed at an array that is no longer there. `xs[0] = { xs.push(2); 9 };`, `xs[{ xs.clear(); 0 }] = 1;` and `g[0][0] = { g[1] = [5]; 1 };` are rejected (any two indexes might be equal). Changing the place itself, a place at the same depth, or a sibling is fine, and reading is always fine: `xs[0] = { xs[1] = 5; 9 };`, `p.items[0] = { p.n += 1; 7 };`, `xs[i] = xs[i] + 1;` and `xs = { xs.push(1); xs };` are valid. Make the change in its own statement instead.

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

**Panics.** A panic prints `panic: <message> at <file>:<line>:<col>` to stderr and exits with code 101. Panics come from integer overflow (message `integer overflow`, including `MIN / -1`), integer division by zero (`division by zero`), out-of-bounds indexing (`index out of bounds: the length is N but the index is I`, located at the `[`), an out-of-range `slice` (`slice out of bounds: from F to T of a string of length L`, at the method name; v4), `split` with an empty separator (located in the prelude, `<prelude>:…`; v4), a negative repeat-literal count, too-deep recursion (`stack overflow`), and the `panic(msg)` intrinsic. An arithmetic panic's location is its operator: the `+` in `x + 1`, the `+=` of a compound assignment, the `-` of a negation. A stack overflow is the one panic without a location — it prints just `panic: stack overflow` — because the runtime can't tell which call went too deep. They cannot be caught.

**Intrinsics.** These names are built into the compiler, not declared in source. They accept arguments that ordinary functions can't, which is why they are special.

| Intrinsic | Accepts | Effect |
| --- | --- | --- |
| `print(x)` | Any primitive or `string` | Writes `x` to stdout with no newline |
| `println(x)` | Any primitive or `string`, or nothing | Writes `x` and a newline |
| `panic(msg)` | `string` | Panics with `msg` |
| `to_string(x)` | Any primitive | Returns `x` formatted as a `string` |
| `swap(a, b)` | Two places of the same type | Exchanges their contents |
| `args()` | Nothing | Returns the program's command-line arguments as a new `string[]`; `args()[0]` is the program's path (v3) |
| `parse_i64(s)` | `string` | `Some(n)` if `s` is an optional `-` followed by decimal digits and fits in `i64`, else `None`; no `+`, spaces, `_` or hex (v3) |
| `parse_f64(s)` | `string` | `Some(x)` if `s` is an optional `-`, digits, an optional `.` and digits, and an optional exponent (`e`/`E`, optional sign, digits), else `None`; `"3"` gives `3.0`, while `".5"`, `"5."`, `"inf"` and `"nan"` give `None` (v3) |
| `read_line()` | Nothing | `Option<string>`: the next line of stdin without its `\n` (and a `\r` before it); a last line without `\n` is still returned; `None` at the end of input (or on a read error). Stdout is flushed first, so a prompt shows (v4) |
| `read_file(path)` | `string` | `Result<string, string>`: the whole file, or `Err` with the system's message, such as `"No such file or directory"` (v4) |
| `write_file(path, text)` | `string`, `string` | `Result<i64, string>`: replaces the file's contents; `Ok` with the bytes written (v4) |
| `append_file(path, text)` | `string`, `string` | `Result<i64, string>`: adds to the end, creating the file if needed (v4) |
| `eprint(x)`, `eprintln(x)` | Like `print`, `println` | Writes to stderr, formatted as `print` does, after flushing stdout so a terminal shows output in program order (v4) |
| `exit(code)` | `i32` | Flushes stdout and stops the program with exit code `code`; never returns, so code after it is unreachable (W0101) (v4) |

**Files** (v4). Paths are relative to the current directory, exactly as given; a path containing a NUL byte is an `Err`. Files are read and written as bytes, with no newline translation, and a whole file is read into memory. Every failure is a value (`Err` or `None`), never a panic.

**Interpolated strings** (v3). Each `{expr}` is evaluated left to right and formatted exactly as `print` would write it (floats as below); the parts are joined into one new `string`. Only numbers, `bool` and `string` interpolate; any other value is E0403, with help to convert it first.

`swap(a, b)` exchanges what two places hold, like `let t = a; a = b; b = t;` but without copying anything: swapping two arrays exchanges them in constant time, which makes double buffering cheap (`swap(ys, buf)`). Both arguments must be places of the same type (E0502, E0403), each following the assignment rules: the root is `let mut` (E0501), not inside a string (E0506), and not an array being iterated (E0507). The places are evaluated left to right, and elements are bounds-checked, before either is written. A place can't be swapped with a part of itself (E0509): `swap(n, n.kids[0])` would store `n` inside itself. Places that might be the same are fine, since swapping a place with itself changes nothing: `swap(xs[i], xs[j])`, `swap(p.a, p.b)` and `swap(a, a)` are all valid.

Floats print as the shortest decimal that round-trips, in Lugha float-literal syntax: always a `.` with at least one digit on each side, so `2.0` prints as `2.0`, not `2`. Values with magnitude at or above `1e16`, or below `1e-5` and nonzero, use an exponent: `1.0e16`, `2.5e-7`. Infinities and NaN print as `inf`, `-inf` and `NaN`; `-0.0` prints as `-0.0`. `bool` prints as `true` or `false`. `u8` prints as a number, not a character.

## 6. Functions and program structure

A v0 program is a single `.la` file containing top-level items: functions, extern declarations and structs. There are no top-level variables or statements.

**Item order doesn't matter.** A function can call a function declared later in the file, and a struct or enum can use a struct or enum declared later. The type checker runs in two passes to support this:

1. Collect every struct name and function signature into a global table.
2. Resolve struct field and enum payload types, check for structs that contain themselves through structs alone (E0307), and mark the enum payload slots that lead back to their enum (indirect slots, section 7).
3. Check each function body against the global table.

**Modules (loading).** A program can span several files (v2, milestone 10; being built).
- **Files are modules.** A file is a module named by its path from the main file's directory, without `.la`, with `/` written as `.`: `shapes/circle.la` is `shapes.circle`.
- **Loading.** `import shapes.circle;` loads `<root>/shapes/circle.la`, where the root is the main file's directory. Each file is loaded once, however many files import it, so import cycles are fine.
- **Errors.** A missing file is E0312. These are E0314: importing the same module twice in one file, two imports whose last names clash, or an import named like one of the file's own items. Importing the main file is E0319.
- **Each file is checked for its own errors.** Invalid UTF-8 (E0110) and lexical and syntax errors in any module are reported against that module's file.
- **Order and cycles.** The passes above run over all modules together, so an item may be used before its declaration across files too.

**Using another module.** `import shapes.circle;` binds the name `circle` in that file only.
- **Calls:** `circle.area(c)` calls function `area` of the module.
- **Types:** `circle.Circle` names its type in annotations; `circle.Circle { r: 1.0 }` builds it; `circle.Circle.new()` calls an associated function; `circle.Shape.Round(c)` and `circle.Shape.Flat` build variants.
- **Methods:** a method call on a value from another module (`c.area()`) works like any method call.
- **Errors:** an unknown module name is E0301; an unknown function or type in a known module is E0301 or E0305.

**Names.** Each module has its own namespace for functions, structs, enums, interfaces and externs, so two modules may each declare `Point`; duplicates within a module are a compile error. Intrinsics and the built-in names `Option`, `Some`, `None`, `Eq` and `Ord` are visible in every module, and no program may declare them (E0302). Methods live in their type's namespace, so a function `dist` and a method `Point.dist` can coexist. No local may be named like a type or an imported module (E0311, section 4). Local variables may shadow function names. Names resolve from the innermost scope outward: block scopes, then function parameters, then the module's own items, then imported module names. In messages a type from another module is shown with its full module path (`shapes.circle.Circle`); a main-file type keeps its bare name. `main` belongs in the main file (E0304 elsewhere).

**Visibility.** Everything is visible to every module of the program unless it is marked `private`, which limits it to its own module. That covers functions, externs, structs, enums, methods, associated functions and struct fields.
- **Private items** (E0313): naming another module's `private` item, calling its `private` method, or reading or assigning its `private` field (including through `swap` and `mut self`) is an error.
- **Struct literals:** a literal outside the struct's module needs every field to be visible, so a struct with a `private` field is built through one of its functions (`range.Range.new(1, 5)`). This is how a type protects an invariant such as `lo <= hi`. `==`, copies and passing values are unaffected: they never name a field.
- **Fields and variants:** a field can be no more visible than its struct; variants share their enum's visibility.
- **`public`** is reserved for packages (v3), where it will mean "visible to other packages". Writing it is E0317.
- **No leaking** (E0318): a declaration that isn't `private` may not mention a `private` type in its signature (parameters, return type, a field's type, a variant's payloads), looking inside arrays and `Option`, since other modules couldn't name that type.

**Functions.** Declared with `fun`, typed parameters, and an optional `: type` return annotation. Omitting the annotation means `void`.

**Generic functions.** A generic function is declared once and compiled once for each distinct list of type arguments the program uses (section 9). One that nothing calls is checked but not compiled. An `extern fun` can't be generic, which the grammar enforces.

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

**Entry point.** Every program must define `main` as either `fun main()` or `fun main(): i32`, without type parameters (E0304). The process exit code is 0 for the first form and `main`'s return value for the second. The user's `main` is emitted as `lugha_fn_main`, like every Lugha function, and the compiler emits a real C `main` that initializes the runtime and calls it (see section 9). Command-line arguments are not accessible in v0.

## 7. Memory model

Value types live on the stack; strings and arrays live on a garbage-collected heap managed by the Boehm-Demers-Weiser conservative collector. Programs never free memory explicitly.

**Stack.** Every local variable and parameter gets an `alloca` in its function's entry block, including struct values. LLVM's `mem2reg` pass promotes these to registers. Struct parameters are passed as a pointer to the caller's storage (section 9); struct return values are returned by value in LLVM IR, and LLVM handles the platform calling convention.

**Enums.** An enum value is a stack value `{ i32 tag; payload area }`. Variants are numbered in declaration order (`None` is 0 and `Some` is 1). The payload area has room for the largest variant's payloads laid out as a struct with natural alignment, and the whole value is aligned to the larger of 4 and that alignment. Enum parameters are passed like structs: as a pointer to the caller's storage. Copying an enum copies its tag and payload bytes; arrays in its payloads are shared, never mutated.

**Indirect slots.** A payload slot whose type leads back to its enum through values held inline — struct fields, enum payloads or `Option`'s payload, but not arrays — is an 8-byte pointer to a copy of the value on the GC heap, allocated with `lugha_rt_alloc` when the variant is built and never mutated afterwards. Every other slot, and the enum value itself, stays inline. `List` is therefore `{ i32 tag; i64 head; ptr rest }`.

**Function values** (v3) are a pair `{ ptr code, ptr env }`, 16 bytes with alignment 8, passed and stored by value. A named function's value has a null environment, and its code is the function's `lugha_ref_` wrapper (section 9).

**Closure environments** (v3). A closure's environment is one GC object, allocated with `lugha_rt_alloc` when the closure is created and laid out as a struct of its captures. It is never written afterwards, so copies of the closure share it safely. A closure that captures nothing has a null environment.

**Generic instances** lay out exactly like the concrete types they stand for: `Stack<i64>` is laid out like a struct with one `i64[]` field, and `List<bool>` gets its own indirect slots.

**Recursion depth.** Function calls use the native stack, which is finite. A recursion that goes too deep — for example walking a list of a million nodes recursively — panics with `stack overflow` (exit 101). The runtime detects it when the stack's guard area is hit, so ordinary calls cost nothing. Walk very long lists with a loop.

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

**Allowed types.** Extern signatures may use only the types below. Structs, arrays, enums, `Option` and function values, generic or not, cannot cross the boundary: C can't call a Lugha function.

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
- Items of a module other than the main file put the module's path before their name, joined with `/`: function `area` of module `shapes.circle` is `lugha_fn_shapes/circle/area`, and its method `Circle.area` is `lugha_fn_shapes/circle/Circle.area`. A name contains neither `/` nor `.`, so a module function (`lugha_fn_a/b/c`) can't collide with a method (`lugha_fn_a/b.c`, method `c` of type `b` in module `a`).
- A generic instance adds its type arguments as they print, with module paths joined by `/`: `lugha_fn_max<i64>`, `lugha_fn_a/b/max<a/b/Point>`. A method's go on its type, then its own: `lugha_fn_Stack<string>.push`, `lugha_fn_Stack<string>.tag<f64>`. `<` can't appear in a Lugha name, so instances can't collide with anything else.
- A function used as a value (v3) is called through `lugha_ref_<symbol>`, a wrapper that takes an environment pointer first and calls `lugha_fn_<symbol>`: `lugha_ref_double`, `lugha_ref_max<i64>`, `lugha_ref_Point.at`. It is emitted once, when first needed.
- A closure's body (v3) is `lugha_closure_<enclosing>.<n>`: the enclosing function's symbol without its prefix, then the closure's number in that function from 0 (`lugha_closure_main.0`, `lugha_closure_main.0.0` for a closure inside it, `lugha_closure_max<i64>.0` in an instance). Its environment comes first, as for every function value's code.
- The array helpers (v3) are generic functions of a prelude written in Lugha, bundled with the compiler and checked with every program, under the module path `$prelude`, which no program can name: `lugha_fn_$prelude/map<Person, string>`. Each is compiled only at the types a program uses.
- An extern follows the visibility rules of section 6, and the same C function may be declared in several modules only with identical signatures (E0306); it is one symbol.
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
- **Deep equality:** generate one `lugha_eq_<type>(T, T) -> i1` per struct, enum, `Option` or array type compared, with the copy helpers' type names (`lugha_eq_1L_opt` for `Option<L>`). Numbers and `bool` compare inline, and strings through `lugha_rt_str_eq`.
- **Struct, enum and array arguments:** pass a pointer to the caller's storage rather than the value itself, since the callee can't modify it.
- **`?`** (v4): the operand goes in a slot and its tag picks the branch. The failing branch builds the function's own `Err(e)` (or `None`) from the payload and returns it; the passing branch loads the `Ok`/`Some` payload.
- **Text and option helpers** (v4) are prelude functions (`str_trim`, `option_map`, `result_map_err`, …) chosen by the receiver's type; `slice` is built in and calls `lugha_rt_str_slice`. A prelude function without type parameters is compiled only when user code reaches it, as generic helpers are.
- **`Result`** (v4) is declared in the prelude, so it is the generic-enum machinery throughout; its LLVM type and helpers are named as it prints (`%"Result<i64, string>"`).
- **Array helpers** (v3): `xs.map(f)` calls the prelude's `map` instance with `xs` first. The prelude is Lugha source like any other (`sort_by` is a bottom-up merge sort between two buffers), so it needs no special lowering; `--emit=tokens|ast` leaves it out, and an error inside it is reported as an internal compiler error.
- **Calling a function value** (v3): extract its code and environment, and call the code indirectly with the environment as a hidden first argument, then the arguments as for any call (aggregates by pointer).
- **Monomorphization:** after checking, the checker lists every generic function and method instance reachable from the non-generic code, at concrete type arguments, and every concrete type those instances use (E0420 when they nest too deep). Codegen compiles each instance from its one body by substituting the type arguments into the types the checker recorded; it still never infers a type. Generic structs and enums get one LLVM type per instance, named as they print (`%"Stack<i64>"`), and their own copy and equality helpers (`lugha_copy_10Stack<i64>`).
- **Methods:** ordinary functions whose first parameter is the receiver. A `self` receiver is passed like a struct argument; a `mut self` receiver is the address of the caller's place itself, so the method's writes land there.
- **`match`:** an LLVM `switch` on the tag for enums and on the value for integers and `bool`s; a chain of `lugha_rt_str_eq` tests for strings. Payload bindings load from the payload area at constant offsets. An exhaustive `match` without `_` ends in `unreachable`. A `match` with a nested pattern or a guard is lowered arm by arm instead: each arm tests its pattern outside in (loading indirect payloads through their pointer), binds, runs its guard, and on any failure branches to the next arm.

**Runtime library.** A small C file, `lugha_rt.c`, is compiled once and linked into every program. The compiler calls only these functions:

| Function | Purpose |
| --- | --- |
| `lugha_rt_init(argc, argv)` | Keeps `argc`/`argv` for `args()`, runs `GC_INIT()`, then installs the stack-overflow handler (an alternate signal stack and a `SIGSEGV` handler that panics only for faults at the stack's end); the C `main` calls it first, since a C macro can't be called from IR |
| `lugha_rt_alloc(size)` | Allocate zeroed GC memory |
| `lugha_rt_panic(msg, file, line, col)` | Print the panic message and `exit(101)` |
| `lugha_rt_str_concat(a, b)` | Implement string `+` |
| `lugha_rt_panic_bounds(len, index, file, line, col)` | The out-of-bounds panic, with the length and index |
| `lugha_rt_str_eq(a, b)` | Implement string `==` and `!=`; returns 1 or 0 as `i32` |
| `lugha_rt_str_cmp(a, b)` | Implement string `<`, `<=`, `>`, `>=`; returns −1, 0 or 1 as `i32` (v2) |
| `lugha_rt_array_grow(array, elem_size)` | A copy of a full array with double the capacity (at least 4), for `push` |
| `lugha_rt_print_*` / `lugha_rt_to_string_*` | One per primitive type (plus `lugha_rt_print_str`), for the intrinsics; `bool` and `u8` arguments are passed zero-extended to 32 bits |
| `lugha_rt_print_newline()` | The newline `println` adds |
| `lugha_rt_args()` | A new `string[]` of the program's arguments (v3) |
| `lugha_rt_parse_i64(s, out)` / `lugha_rt_parse_f64(s, out)` | Parse `s` into `*out`; return 1, or 0 for `None` (v3) |
| `lugha_rt_str_slice(s, from, to, file, line, col)` | `s.slice(from, to)`: a new string of those bytes, or the bounds panic (v4) |
| `lugha_rt_read_line(out)` | `read_line()`: 1 and the line, or 0 at the end of input (v4) |
| `lugha_rt_read_file(path, out, error)` / `lugha_rt_write_file(path, text, append, count, error)` | The file intrinsics: 1 and the value, or 0 and the C library's `strerror` message (v4) |
| `lugha_rt_eprint(text, newline)` / `lugha_rt_exit(code)` | `eprint`/`eprintln` (values formatted first) and `exit`, each flushing stdout first (v4) |

The runtime writes through C's `stdout` stream, so output from `print`/`println` and from libc functions such as `puts` appears in program order.

The C `main(argc, argv)` emitted by the compiler calls `lugha_rt_init(argc, argv)` (which runs `GC_INIT()`), then `lugha_fn_main()`, and returns its exit code. An interpolated string lowers to the `lugha_rt_to_string_*` functions and `lugha_rt_str_concat`.

**Command-line interface**

| Command | Effect |
| --- | --- |
| `lughac build prog.la [-o prog]` | Compile and link an executable |
| `lughac run prog.la [-- args…]` | Build to a temporary file and run it, passing `args` to the program (v3) |
| `lughac check prog.la` | Lex, parse and type-check only; no codegen or linking (type checking from milestone 3) |
| `lughac spec` | Print the LLM-ready spec bundled with this compiler version |
| `lughac build --emit=tokens\|ast\|ir prog.la` | Print an intermediate stage and stop; with several modules, `tokens` and `ast` print each after a `; module <name>` line (`main` for the main file) |
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
| E0304 | `main` with the wrong signature, generic, or outside the main file | `fun main(argc: i32): i32`; `fun main<T>()`; a `main` in a module (v2) |
| E0305 | Unknown type name, or an interface used as a type | `let p: Point = 1;` with no `Point`; `let s: Shape = c;` for an interface `Shape` |
| E0306 | Reserved extern name, or one C function declared with two signatures | `extern fun lugha_rt_alloc(size: i64): i64;` (§8) |
| E0307 | Struct contains itself | `struct A { b: B }` with `struct B { a: A }` — hold it in an array instead (`A[]`) |
| E0308 | Field declared twice | `struct P { x: i64, x: f64 }` |
| E0309 | Variant declared twice | `enum E { A, B, A }` |
| E0310 | Method declared twice, or named like a field or variant | `impl P { fun x(self) {} }` where `P` has a field `x` |
| E0311 | A local named like a type or an imported module | `let Shape = 3;`, `fun f(Point: i64)` |
| E0312 | Module not found | `import shapes.cirlce;` with no `shapes/cirlce.la` (v2) |
| E0313 | Another module's `private` item, method or field | `range.helper()`; `r.lo`; `range.Range { lo: 1, hi: 5 }` (v2) |
| E0314 | Import clash | `import a.util;` and `import b.util;` in one file; an import named like an item (v2) |
| E0315 | Unknown bound | `fun f<T: Hash>(x: T)`; a struct as a bound (v2) |
| E0316 | Bad type parameter name | `fun f<T, T>()`, a parameter named like a struct (v2) |
| E0317 | `public` before packages exist | `public fun f() {}` (v2) |
| E0318 | A `private` type in a non-`private` signature | `private struct S {}` then `fun make(): S` (v2) |
| E0319 | The main file imported | a module writing `import main;` (v2) |
| E0401 | Numeric literal of the wrong kind for its expected type | `x + 2.5` where `x` is `i32` |
| E0402 | Integer literal out of range for its type | `let b: u8 = 256;` |
| E0403 | Type mismatch | `let n: i32 = ok;` where `ok` is `bool`; `if`/`else` of different types |
| E0404 | Operator applied to invalid operand types | `true + 1`, `-u` where `u` is `u8`; `a == b` on a `T` without `Eq` (with help to declare it) |
| E0405 | Wrong number of arguments | `add(1)` for `fun add(a: i64, b: i64)` |
| E0406 | Not a function, or something that can't be a value | `let step = 2; step(1);`; `let a = Circle.area;` (a method), `let p = println;`, an extern (v3) |
| E0407 | `void` used as a value | `let x = log();` where `log` returns nothing |
| E0410 | No such field | `s.size` on a `string`; `(5).len`; `p.z` or `Point { z: 1.0 }` on a `Point` without `z` |
| E0411 | Not indexable or not iterable | `n[0]` where `n` is an `i64`; `for c of "abc"` |
| E0412 | Cannot infer an array's element type, `None`'s type, or a type argument | `let xs = [];`, `let x = None;`, `let s = Stack.new();`, `let m = max;` — annotate it: `let xs: i64[] = [];` |
| E0413 | Struct literal missing fields | `Point { x: 1.0 }` |
| E0414 | Struct literal field given twice | `Point { x: 1.0, x: 2.0, y: 0.0 }` |
| E0415 | No such variant or method | `Shape.Circel(1.0)`, a `Square` pattern on a `Shape`, `p.size()`, `xs.push(1)` |
| E0416 | Wrong number of payloads | `Shape.Rect(1.0)`, `Rect(w) => …` |
| E0417 | Non-exhaustive `match` | `match s { Circle(r) => r }` with more variants; an integer `match` without `_` |
| E0418 | Wrong number of type arguments | `Stack<i64, i64>`, a bare `Stack` as a parameter type (v2) |
| E0419 | Type argument doesn't satisfy its bound | `largest([[1]])`: `i64[]` is not `Ord` (v2) |
| E0420 | Generic instantiation too deep | `fun f<T>(x: T) { f([x]); }`; `struct S<T> { xs: S<T[]>[] }` (v2) |
| E0421 | A method no bound declares | `s.perimeter()` in `fun f<T: Shape>(s: T)` (v2) |
| E0422 | A type argument doesn't satisfy an interface | `total_area([1, 2])`: `i64` has no method `area` (v2) |
| E0423 | Two bounds declare one item differently | `T: Sized + Measured` where both declare `size` with different types (v2) |
| E0424 | `?` where it can't return | `?` on an `i64`; in `main`; on an `Option` in a function returning `Result`; different error types (v4) |
| E0409 | Type not allowed in an extern signature | `extern fun getenv(key: string): string;` — no `string` returns; no array, struct, enum, `Option` or function parameters or returns (§8) |
| E0408 | Invalid cast | `ready as i32` where `ready` is `bool`; `x as bool` |
| E0501 | Assignment to an immutable binding | `let total = 0; total = 5;`; assigning to a parameter or loop variable; `xs[0] = 1;` where `xs` isn't `let mut` |
| E0502 | Assignment to something that isn't a place | `(a + b) = 3;` |
| E0503 | Missing return | a non-void function whose body can end without a value (§6); a stray `;` after the result gets a "remove this semicolon" label |
| E0504 | `break` or `continue` outside a loop | |
| E0505 | Block-like statement with a discarded value | `if big { 100 } else { 1 }` followed by more statements |
| E0506 | Assignment into a string | `s[0] = 104;`, `s.len = 2;` — strings are immutable (§4) |
| E0507 | Assignment to the array being iterated | `for x of xs { xs[0] = x; }`, or a `mut self` call or `swap` on it |
| E0508 | Argument aliases a `mut self` receiver | `v.add(v)` where `add` takes `mut self` |
| E0509 | A place swapped with a part of itself | `swap(n, n.kids[0])` |
| E0510 | A statement changes what contains the place it writes | `xs[0] = { xs.push(2); 9 };` (§5) |
| E0511 | Changing a variable a closure captured | `let inc = fun() { count += 1; };` (v3) |
| W0101 | Unreachable code (warning) | statements after `return`, `break` or `continue`; reported once per block |
| W0102 | Unreachable `match` arm (warning) | an arm after `_`, or a variant already covered |
| W0103 | Unused `match` binding (warning) | `Some(Emtpy) => 0`, a misspelled variant that became a binding |

**JSON diagnostics.** With `--diagnostics=json`, the compiler writes one JSON object per line to stderr, one per diagnostic, and nothing else. Lines and columns are 1-based; columns and offsets count UTF-8 bytes. `label` is the text shown under the primary span, or `null`. `labels` holds secondary spans, and `help` is an optional suggestion. Internal errors have `"code":null`, and `"span":null` when they have no location. `"file"` is the file of the primary span: the main file as given on the command line, or a module's path relative to the main file's directory (`shapes/circle.la`); lines, columns and offsets are within that file. A label in a different file carries its own `"file"` key, which appears only then.

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

**Patterns and equality** (milestone 9). Prints `big circle` then `square` then `nothing` then `true` then `true` then `true` on six lines, exits with 0.

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

**Two modules** (milestone 10). Prints `3.5`, exits with 0. The two files sit in one directory, and `lughac run main.la` builds both.

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

**Generics** (milestone 11). Prints `b` then `true` then `plum` on three lines, exits with 0.

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

**Interfaces** (milestone 12). Prints `15.0` then `9.0` then `square` on three lines, exits with 0.

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

**Arguments and interpolation** (milestone 13). Run as `prog 21`. Prints `21 doubled is 42`, exits with 0.

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

**Results and `?`** (milestone 17). Prints `3` then `3 is odd` on two lines, exits with 0.

```
fun half(n: i64): Result<i64, string> = if n % 2 == 0 { Ok(n / 2) } else { Err("{n} is odd") };

fun quarter(n: i64): Result<i64, string> {
    let h = half(n)?;
    half(h)
}

fun main() {
    match quarter(12) {
        Ok(q) => println(q),
        Err(e) => println(e),
    }
    match quarter(6) {
        Ok(q) => println(q),
        Err(e) => println(e),
    }
}
```

**Text helpers** (milestone 18). Prints `42` then `a-b-c` then `2` on three lines, exits with 0.

```
fun main() {
    let text = "name, age\nann, 30\n\nbo, 12\n";
    let mut total = 0;
    for line of text.lines() {
        let parts = line.split(",").map(fun(p) = p.trim());
        if parts.len < 2 || parts[0] == "name" {
            continue;
        }
        total += parse_i64(parts[1]).unwrap_or(0);
    }
    println(total);
    println(["a", "b", "c"].join("-"));
    println("  hi  ".trim().len);
}
```

**Reading input** (milestone 19). Run with `5`, `7` and `x` on three lines of stdin. Prints `12` then `skipped 1` on two lines, exits with 0.

```
fun main() {
    let mut total = 0;
    let mut skipped = 0;
    while true {
        match read_line() {
            Some(line) => match parse_i64(line.trim()) {
                Some(n) => { total += n; }
                None => { skipped += 1; }
            },
            None => { break; }
        }
    }
    println(total);
    println("skipped {skipped}");
}
```

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
| 9 | Patterns and equality (v2) | deep `==` on every type, string order, nested patterns, guards, W0103 | The patterns program in section 10 passes (done) |
| 10 | Modules (v2) | `import`, per-module names, `m.f`/`m.T`, `private` (E0313), `public` reserved (E0317), no leaking (E0318), module symbols | The two-module program in section 10 passes (done) |
| 11 | Generics (v2) | type parameters, `Eq`/`Ord` bounds, inference, monomorphization, instance symbols, E0420; `Option<T>` as a predeclared generic enum | The generics program in section 10 passes (done) |
| 12 | Interfaces (v2) | `interface` declarations, interfaces as bounds, structural satisfaction (E0421, E0422, E0423) | The interfaces program in section 10 passes (done: v2 complete) |
| 13 | Input and interpolation (v3) | `args()`, `parse_i64`, `parse_f64`, string interpolation (E0111), `lughac run … -- args` | The arguments program in section 10 passes (done) |
| 14 | Functions as values (v3) | function types, named and associated functions as values, calling values, no `==` on functions, `lugha_ref_*` | The functions-as-values program in section 10 passes (done) |
| 15 | Closures (v3) | closure literals, capture by copy (E0511), parameter inference, `lugha_closure_*`, closures in generic code (E0209) | The closures program in section 10 passes (done) |
| 16 | Array helpers (v3) | `map`, `filter`, `fold`, `sort_by`, `any`, `all`, from a Lugha prelude | The array-helpers program in section 10 passes (done: v3 complete) |
| 17 | Results and `?` (v4) | `Result<T, E>` (in the prelude), `Ok`/`Err`, the `?` operator (E0424) | The results program in section 10 passes (done) |
| 18 | Text and option helpers (v4) | string helpers, `join`, `Option`/`Result` helpers, built-in `slice` | The text-helpers program in section 10 passes (done) |
| 19 | Input and output (v4) | `read_line`, `read_file`, `write_file`, `append_file`, `eprint`/`eprintln`, `exit` | The reading-input program in section 10 passes (done: v4 complete) |

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
- [x] **Swapping buffers.** Decided (v1.1): the `swap(a, b)` intrinsic. Double buffering with `ys = buf;` copies the whole buffer each time, because `buf` is still in use; copy-on-write and move on last use can't avoid that copy (DECISION-010), but `swap(ys, buf)` exchanges the two arrays without copying.
- [x] **Array growth factor.** Decided: capacity doubles, starting at 4. It was adequate on the stack program and the DECISION-010 programs; revisit only with measurements.
- [x] **Instantiation depth.** Decided (v2): 64, large enough for real code and small enough to fail fast (E0420).
- [ ] **`mut` bindings in `match`.** Bindings are borrowed and immutable; no program has needed `mut` ones yet.
- [x] **Expression-bodied functions.** Decided: adopted (section 6). `fun square(x: i64): i64 = x * x;` drops the braces from one-liners. It is a small parser addition and a second way to write the same function, accepted because one-liners are common and it is pure sugar for `{ e }`.
