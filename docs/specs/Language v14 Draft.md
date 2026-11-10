# Lugha v14 — Draft (delta against v13)

**Status:** approved 2026-10-14 — session 96 (proposed in session 95). The user asked for milestones 45–49 "in one go"; milestone 50 waits for a security review of its PRP.
**Base:** `Language v13 Specification.md`, complete (milestone 44). It stays the source of truth, and what `lughac spec` prints, until v14's first milestone starts `Language v14 Specification.md`. That file then grows milestone by milestone, and v13 is frozen.

This draft lists only what changes, in the order of the v13 sections. Everything not mentioned keeps its v13 meaning.

**Compatibility.** v14 breaks v13 programs in these ways, each chosen in discovery or needed by a choice:
- **`zip` yields tuples, and `Pair` is gone.** `p.first`/`p.second` become `p.0`/`p.1`, and a program's own `Pair` is unaffected.
- **`char` is a type keyword,** as `u32` became one in v7.
- **New intrinsic names, which a program may no longer declare (E0302):** `format`, `read_bytes`, `write_bytes`, `append_bytes`, `read_stdin_bytes`, `string_from_utf8` and `char_from`.
- **A string literal right after `format(`** is a template, read without interpolation.

Everything else v14 adds was an error before.

## Decisions behind this draft (discovery, session 95)

1. **Headline: seven features in six milestones:**
   - binary I/O with any-integer indexes;
   - Unicode `char`;
   - tuples;
   - `format` templates;
   - import aliases and whole-module re-exports;
   - downloaded packages with a lock file.
2. **`format` is a template:** `format("{} and {:>5}", a, b)` returns a `string`, with `{}` and `{:spec}` (v13 specifiers) filled from the arguments in order.
3. **Tuples are full:**
   - types `(A, B, …)`, values `(a, b)` and access `t.0`;
   - patterns in `match`, `if let`, `while let`, `let`, `let … else` and `for`;
   - equality, order and printing.

   `zip` yields `(A, B)`, and `Pair` is removed.
4. **Imports get `as` and whole-module re-exports:**
   - `import a.b as c;`;
   - `import a.b { X as Y };`;
   - `public import a.b;` / `public import a.b as v;`, giving three-part names `m.v.X`.
5. **Packages can come from git:**
   - `geo = { git = "https://…", version = "1.2" }`, fetched with the system's `git` into a cache;
   - pinned by commit in `lugha.lock`;
   - `lughac update` and `--offline`.
6. **Unicode is a `char` type:**
   - `'a'` literals;
   - `s.chars()`, `s.char_count()`;
   - `c as u32`, `char_from(n)`;
   - ASCII and Latin-1 classification and case.
7. **Bytes:**
   - `read_bytes`, `write_bytes`, `append_bytes`, `read_stdin_bytes`;
   - `s.to_bytes()`, and `string_from_utf8` checked;
   - indexes of any integer type.

**Choices made while writing this draft** (each settles a gap the answers left open):
- **`format`:**
  - **The template:** it must be a string literal (E0432 otherwise). The lexer reads a string literal that directly follows `format` `(` as a template, with no interpolation; `{{` and `}}` are literal braces.
  - **Placeholders:** `{}` or `{:spec}`. The number of placeholders must equal the number of further arguments (E0432), and each spec must fit its argument (E0431, as in v13).
  - **Arguments:** each may be any value `print` takes. `format` is the language's only variadic function.
- **Tuples:**
  - **Size:** a tuple has two or more elements. `(x)` is just `x`; `()` is not a value.
  - **Access:** `t.0`, `t.1`, …, an index past the end is E0410, and `t.0.1` works (the parser splits the float-like `0.1`).
  - **Changing:** a tuple is a value like a struct, so `t.0 = v` changes a `let mut` tuple.
  - **Patterns:**
    - a tuple pattern `(p, q)` nests like any pattern;
    - `let (x, y) = e;` and `for (i, w) of …` need irrefutable patterns: bindings, `_`, tuples of those, and `mut` bindings. Anything else is E0433, with help to use `let … else`;
    - `let mut (x, y)` is not a thing: write `let (mut x, mut y)`.
  - **Comparison:** `==` compares element-wise; `<` is lexicographic and needs every element `Ord`.
  - **Printing:** `(1, "a")`, with strings quoted inside, as in v12's built-in forms.
  - **Representation:** a tuple type is an LLVM named struct `%"(i64, string)"`, and symbols spell it the same way (`lugha_fn_max<(i64, i64)>`).
  - **`enumerate`** still yields `Indexed<T>`; only `zip` changes.
- **Imports:**
  - **Clashes:** an alias name is checked for clashes like any import name (E0314). `import a.util as u1; import b.util as u2;` is how two `util`s coexist.
  - **Item aliases:** `X as Y` binds only `Y`; a re-export `public import m { X as Y }` re-exports it as `Y`.
  - **Whole-module re-exports:** `public import a.b [as v];` makes the module a member of the importing module, visible to every package. `m.v.X` names `v`'s item `X`, for types, calls, struct literals, associated calls and variants (`m.v.T.f()`, `m.v.E.V`). Only `public` items can be reached that way (E0313). Chains (`m.v.w.X`) are not followed: a module re-exported in a re-exported module is reached through its own import.
- **Downloaded packages:**
  - **The manifest entry:** `{ git = "URL", version = "R" }`. `path` and `git` are exclusive, `git` needs `version`, and the URL must start `https://` or `file://` (E0321).
  - **Cache:** `$LUGHA_CACHE`, else `$XDG_CACHE_HOME/lugha`, else `$HOME/.cache/lugha`. A package lives in `git/<host-and-path sanitised>/<commit>`.
  - **Resolving:**
    - lughac runs `git ls-remote --tags URL` and picks the highest `vMAJOR.MINOR.PATCH` tag meeting the requirement;
    - then `git clone --depth 1 --branch <tag> URL <dir>` into a temporary folder in the cache, then `git rev-parse HEAD`, then renames the folder into place.
    - `git` runs with an argument list (never a shell), `GIT_TERMINAL_PROMPT=0` and no hooks (`-c core.hooksPath=/dev/null`).
  - **`lugha.lock`:** written next to the main file in the manifest subset (a `[locked]` table of `name = { git = "…", version = "1.4.0", commit = "<40 hex>" }`). When it exists, builds use exactly its commits: a cached folder is used as is, a missing one is cloned at that tag and its `HEAD` must equal the locked commit (E0326 otherwise).
  - **Commands:** `lughac update [prog.la]` re-resolves every git package and rewrites the lock. `--offline` on `build`, `run` and `check` refuses to run `git` and fails (E0325) when something isn't cached.
  - **Failures:** no `git`, a failed command, or no matching tag is E0325, at the entry's line, with `git`'s message.
  - **What never happens:** lughac never runs code from a package while building; built programs run it as they run any code.
- **`char`:**
  - **Values:** a Unicode scalar value (U+0000–U+D7FF, U+E000–U+10FFFF), 32 bits (LLVM `i32`).
  - **Literals:** `'a'`, `'é'`, `'\n'`, `'\t'`, `'\\'`, `'\''`, `'\u{1F600}'`. An empty literal, two characters, a bad escape or a non-scalar code is E0112.
  - **Operators:** `==` and `<` compare code points. No arithmetic.
  - **Casts:** `c as u32` (and any integer type, truncating as casts do); `n as char` from an integer gives U+FFFD for a non-scalar value, as casts never fail. `char_from(n: u32): Option<char>` checks instead.
  - **Strings:**
    - `s.chars()` (a `Chars` iterator, Lugha in the prelude, decoding UTF-8 and yielding U+FFFD for each invalid byte);
    - `s.char_count()`;
    - `to_string(c)` and `"{c}"` encode UTF-8.
  - **Printing:** `print(c)` writes the character; inside a built-in form it is quoted, `'a'`.
  - **Classification and case:** `c.is_alphabetic()`, `is_digit()` (`0`–`9`), `is_whitespace()`, `is_upper()`, `is_lower()`, `to_upper()` and `to_lower()` cover ASCII and Latin-1 (U+0000–U+00FF) only; other characters are neither letters nor cased.
  - **Formatting:** width still counts bytes.
- **Bytes and indexes:**
  - `read_bytes(path): Result<u8[], string>`, `write_bytes(path, data: u8[]): Result<i64, string>` (bytes written), `append_bytes` likewise, `read_stdin_bytes(): u8[]` (the rest of stdin);
  - `s.to_bytes(): u8[]`;
  - `string_from_utf8(b: u8[]): Result<string, string>`: `Err("invalid UTF-8 at byte 3")`;
  - **indexes:** `xs[i]` and `s[i]` take any integer type. A value below 0 or at least `.len` panics with the index as written (`index -1 out of bounds for length 3`; a `u64` above `i64.MAX` is out of bounds).

## §2 Lexical structure — changes

- **Keyword:** `char`.
- **Char literals:** `'…'`, with E0112.
- **A string literal directly after `format` `(`** is a template: one `Str` token, with no interpolation.

## §3 Grammar — changes

```ebnf
type         = … | "char" | "(" type "," type { "," type } [ "," ] ")" ;
primary      = … | CHAR | "(" expr "," expr { "," expr } [ "," ] ")" ;
member       = "." ( IDENT | INT ) ;                            (* t.0 *)
alternative  = … | "(" pattern "," pattern { "," pattern } [ "," ] ")" ;
var_decl     = … | "let" "(" pattern { "," pattern } ")" [ ":" type ] "=" expr ";" ;
for_head     = ( IDENT | tuple_pattern ) ( "in" … | "of" … ) ;
import       = [ "public" ] "import" path [ "as" IDENT ] [ "{" item { "," item } "}" ] ";" ;
item         = IDENT [ "as" IDENT ] ;
qualified    = IDENT "." IDENT [ "." IDENT ] ;                  (* m.v.X through a re-exported module *)
```

## §4–§6 — changes

As described above, by milestone.

## §9 Compilation model — changes

- **Tuples:** named structs `%"(A, B)"`, with copy, equality and show helpers like structs.
- **`char`:** `i32`. The runtime gains `lugha_rt_print_char` and `lugha_rt_to_string_char`.
- **Bytes:** `lugha_rt_read_bytes`, `lugha_rt_write_bytes`, `lugha_rt_read_stdin_bytes`, `lugha_rt_str_to_bytes`, `lugha_rt_utf8_check`.
- **Indexes:** an index is sign- or zero-extended to `i64` before the bounds check.
- **`format`:** lowers to the v13 format calls and `lugha_rt_str_concat`.
- **Packages:** resolved by the loader before parsing; nothing reaches codegen.
- **Programs using none of this** give byte-identical IR.

**New error codes.** The numbers are proposed, and become final when their milestone's PRP is approved.

| Code | Error | Example |
| --- | --- | --- |
| E0112 | A bad `char` literal | `''`, `'ab'`, `'\q'`, `'\u{D800}'` |
| E0325 | A package that can't be fetched | no `git`; a URL that fails; no tag meeting `version`; `--offline` with nothing cached |
| E0326 | A package that doesn't match `lugha.lock` | the locked commit isn't what the tag now gives |
| E0432 | A bad `format` template | a non-literal template; 2 placeholders for 3 arguments; `{x}` in a template |
| E0433 | A refutable pattern where it must always match | `let (1, x) = t;`, `for (Some(a), b) of …` |

## §10 Example programs — additions (v14 acceptance tests)

Each milestone's PRP writes its §10 program with exact output.
- **M45:** a program that writes, reads back and decodes bytes, and indexes with `u8`/`u32`.
- **M46:** counting characters and case-changing a Latin-1 string.
- **M47:**
  - `divmod` returning `(i64, i64)`;
  - `let (q, r) = …`;
  - `for (i, w) of xs.iter().zip(ws.iter())`;
  - printing tuples.
- **M48:** `println(format("{} has {:>4} items", name, n))`.
- **M49:** a directory case with `import … as`, `{ X as Y }` and `public import m;` used as `shapes.vec.Vec2`.
- **M50:** a directory case with a `file://` git repository built in the test, locked, then built `--offline`.

## §11 Milestones — additions

| # | Milestone | Adds |
| --- | --- | --- |
| 45 | Bytes and indexes | binary file I/O, `to_bytes`, `string_from_utf8`, `read_stdin_bytes`, any-integer indexes |
| 46 | Unicode text | `char`, `'a'`, `chars()`, `char_count()`, casts, `char_from`, classification and case (E0112) |
| 47 | Tuples | tuple types, values, `t.0`, patterns in `let`/`for`/`match`/`if let`, `zip` → tuples, `Pair` removed (E0433) |
| 48 | `format` | template `format(…)` (E0432) |
| 49 | Import aliases and re-exports | `as` on modules and items, `public import m;`, `m.v.X` |
| 50 | Downloaded packages | git entries, cache, `lugha.lock`, `lughac update`, `--offline` (E0325, E0326) |

**Order:** independent features first, smallest to largest. M50 is last, and its PRP waits for your security review of fetching and running `git`.

**Out of scope for v14** (for later):
- a package registry, tarballs and checksums;
- full Unicode tables and grapheme clusters;
- `char`-counting widths;
- positional `{0}` and named `{name}` placeholders;
- one-element tuples;
- streams, directories and processes.

## §12 Open questions — additions

- [ ] **`enumerate` and tuples:** `enumerate` still yields `Indexed<T>`; a later version could yield `(i64, T)` for symmetry with `zip`.
- [ ] **Git packages and `extern`:** a downloaded package can declare `extern` C functions. A later version could list a package's externs at `lughac update`, so users see what a package can call.
