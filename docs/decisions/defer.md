# `defer`: cleanup at scope exit

**Decided and built:** decision 214, PRP-182, milestone 151 (every recommendation below was taken, except that `defer` is a reserved keyword, not a contextual word). Design note, 2026-10-31, written for the session that implements it. It records the problem, what other languages do, and a recommended design. Nothing here is decided yet: the discovery (one question at a time, `PRPs/DISCOVERY.md`) settles each choice, then a PRP. DECISIONS.md lists it under OPEN.

## The problem

Nothing closes a resource a program forgets. The collector never closes a `File` (`std.fs`, milestone 146), a `net.Conn` or `net.Listener` (milestone 110), or a TLS connection (milestone 112): an unclosed one stays open until the program exits (spec §5). A short program loses nothing (writes are unbuffered, the OS closes everything at exit). A long-running one (a server, a loop over many files) runs out of descriptors, after which `fs.open`, `net.connect` and `accept` return `Err("Too many open files")`; an unclosed socket also never sends end of file, so the peer waits for its own timeout. The usual way to forget is an early exit, which `?` makes easy:

```lugha
for path in paths {
    let f = fs.open(path)?;
    out.push(f.read_line()?);   // an Err returns here; f is never closed
    f.close();
}
```

The only finalizer in the runtime frees a `std.crypto` hasher's OpenSSL context (`runtime/crypto.c`): memory only. Files and sockets are deliberately not closed by the collector (Boehm is conservative: a finalizer may run late or never, and closing a socket at an unpredictable moment changes what the peer sees).

## Why not what Rust does

Rust closes a file when its single owner goes out of scope (`Drop`); ownership is tracked statically, so cleanup is automatic. Lugha can't copy it:
- **Copies are shared handles:** `let g = f;` gives two equal copies of one stream. Neither is the owner, so no scope end is safe to close at.
- **No ownership in the language:** Lugha has one copy semantics and no shared references (DECISIONS v3). Milestone 149's liveness knows last uses, but as an optimisation that errs towards copying; behaviour can't hang on it.
- **No reference counts:** "close when the last copy dies" is reference counting (Swift's ARC); DECISION-010 ruled it out, Boehm making it unneeded elsewhere.

Rust-style cleanup would need move-only handles (`let g = f;` moves): a second kind of value, reversing "every value copies". `defer` gives most of the benefit for one statement.

## What other languages do

| Approach | Languages | Lessons for Lugha |
|---|---|---|
| Block-scoped `defer` | Zig, Swift, Odin, Jai, Nim; C (WG14 `defer` Technical Specification, Meneide; reportedly in GCC and Clang) | Runs at the end of its block on every exit, last first. Swift forbids leaving a `defer` body (no `return`/`throw` inside: you may already be leaving because of an error). Zig adds `errdefer` (only on an error exit), for undoing half-done work. The newest independent design (C's) chose block scope. |
| Function-scoped `defer` | Go | The documented pitfalls: in a loop every file stays open until the function returns ("too many open files"; the fix is a function per iteration); `defer f.Close()` silently drops the error (CodeQL `go/unhandled-writable-file-close`, `errcheck`); arguments evaluated at the `defer`, the call made later. |
| Destructors with ownership | Rust, C++ | Automatic, at a known point. Rust's docs: dropping a `File` ignores all errors (`Drop` must never fail); call `sync_all` to detect them. Even here close errors aren't reported. |
| Scoped blocks over a protocol | Python `with`, C# `using` (C# 8: `using var x = …;` disposes at scope end), Java try-with-resources | Cleanup tied to a construct and an interface (`__exit__`, `Dispose`, `AutoCloseable`). Python's `__exit__` sees the exception and may suppress it; C#'s `Dispose` sees nothing. |
| Finalizers | Java (deprecated for removal, JEP 421, JDK 18), Lugha's collector | Unreliable, a security risk, slow; Java points to try-with-resources and `Cleaner`. Confirms not closing files from the collector. |
| Linear types | Austral, Haskell `linear-base` | Forgetting `close` is a compile error (every linear value consumed on every path). The strongest guarantee, at the cost of a second kind of value with use-exactly-once rules. |

## Recommended design

```lugha
fun first_lines(paths: string[]): Result<string[], string> {
    let mut out: string[] = [];
    for path in paths {
        let f = fs.open(path)?;
        defer f.close();                // at the end of this iteration's block
        out.push(f.read_line()?);       // an Err here still closes f
    }
    Ok(out)
}
```

Each choice below is a discovery question; the recommendation comes first.

1. **Scope: the enclosing block** (Zig, Swift, Odin, C), not the function (Go's loop pitfall).
2. **What runs: the whole statement, at exit, with the values then** (Zig, Swift, C). Not Go's "arguments now, call later". For handles it makes no difference (every copy is the same stream); for plain values it is the one rule to learn.
3. **Order and exits:** several defers in a block run last first; on every way out of the block: its end, `return`, `?`, `break`, `continue`. A `return` value or block tail is computed first, then the defers run (`defer f.close(); f.read_line()` reads, then closes). A deferred statement can't leave its own body (`return`, `?`, `break`, `continue` inside it: a compile error), as in Swift.
4. **Panics don't run defers:** a panic ends the process, the OS closes every descriptor, and each `@test` already runs in its own process. No unwinding is added. Written in the spec, not implemented.
5. **Results: a deferred statement must have type `()`.** No language reports close errors from automatic cleanup (Rust's `Drop` ignores them, recommending `sync_all`). Lugha already fits: `close()` on `File`, `net.Listener`, `net.Conn` and the TLS types returns `()` (closing twice is fine), and durability has `fs.sync` (milestone 116). A deferred `Result` would be a new error code (or E0401-like mismatch) asking to handle it, so nothing is dropped silently, unlike Go.
6. **`errdefer`: not now.** With `?` and value semantics nothing half-built stays visible; revisit if a need appears.
7. **Grammar:** `defer <expression>;` or `defer { … }` (a block, for several steps), a statement. A `defer` inside a deferred block is allowed and runs at that block's end, as in Zig and Swift. Keyword or contextual word: recommend contextual at the start of a statement, as `derive` is (milestone 142), so programs using `defer` as a name keep working.

## Implementation notes (for sizing the PRP)

- **Lexer/parser** (`src/lexer/`, `src/parser/`): `defer` recognised at the start of a statement (or a keyword, per choice 7); `StmtKind::Defer(Expr or Block)`; E02xx for a malformed one.
- **Checker** (`src/check/`): the body checked as a statement of type `()`; exits inside it rejected (new E04xx with a page in `docs/errors/`); a deferred `Result` rejected. Liveness (`check/liveness/`, milestone 149): a deferred statement's mentions count as uses at the block's end, so a local it reads (`f`) is never moved away earlier. Its position in source is before later statements, but it runs after them: `positions` and `last` must treat its mentions as at the block's end.
- **Codegen** (`src/codegen/`): the bulk of the work. Each block keeps a stack of its defers; every exit lowers the pending ones, innermost block first, last first: the block's end (`block_inner`), `return` (`stmt.rs`), `?` (`results.rs::try_value`), `break`/`continue` (`stmt.rs::jump`, through every block up to the loop's), a function's tail (computed first, then the defers). Loop hoisting (`hoist_scan.rs`) must see deferred writes. Closures and `spawn` bodies are functions of their own: their defers run when they return.
- **Formatter** (`src/formatter/`), **language server** (keyword completion, hover), **VS Code grammar** (`editors/vscode/`: the keyword in the TextMate grammar; bump the extension version).
- **Spec:** §2 (keyword), §3 (grammar), §6 (statements: the exit rule, panics), §9 (lowering), §11 row, "Changes since v16"; the v17 Draft first.
- **Tests:** `tests/programs/mNNN/`: files closed on every exit kind (end, `return`, `?`, `break`, `continue`, nested blocks, loops), order of several defers, a tail computed before the defers, values read at exit; rejected programs for each error; the descriptor count staying flat across 10 000 opened files in a loop (`/proc/self/fd` on Linux).

## Sources

- Zig: [errdefer conditional cleanup](https://www.luisllamas.es/en/zig-errdefer-conditional-cleanup/), [errdefer scoped to the parent block](https://ziggit.dev/t/errdefer-scoped-to-the-parent-block/8346), [redefer proposal](https://ziggit.dev/t/proposal-a-new-redefer-keyword-for-function-scoped-deferred-execution/14190)
- Go: [How to use defer correctly](https://oneuptime.com/blog/post/2026-01-23-go-defer/view), ["Too many open files" with defer](https://forum.golangbridge.org/t/too-many-open-files-opened-files-not-getting-closed-with-defer-file-close/7836), [CodeQL go/unhandled-writable-file-close](https://codeql.github.com/codeql-query-help/go/go-unhandled-writable-file-close/), [Understanding defer in Go](https://www.digitalocean.com/community/tutorials/understanding-defer-in-go)
- C: [WG14 document log](https://open-std.org/JTC1/SC22/WG14/www/wg14_document_log), [C2Y](https://en.wikipedia.org/wiki/C2Y_(C_standard_revision)), [thephd.dev](https://thephd.dev)
- Swift: [What is defer in Swift?](https://www.donnywals.com/what-is-defer-in-swift/), [async calls in defer bodies](https://forums.swift.org/t/support-async-calls-in-defer-bodies/81790)
- Rust: [File sync docs change](https://cdn05042023.gitlink.org.cn/dnrops/rust/commit/b24d2ad300c4864e6fdb88c872365c559d38b241), [RFC: Close traits](https://github.com/rust-lang/rfcs/pull/2677/files), [LWN discussion](https://lwn.net/Articles/1009302)
- Java: [JEP 421](https://openjdk.java.net/jeps/421), [Clean Cleaner](https://inside.java/2022/05/25/clean-cleaner/)
- Linear types: [Austral's linear type checker](https://borretti.me/article/how-australs-linear-type-checker-works), [linear-base Simple FileIO](https://hackage-content.haskell.org/package/linear-base-0.5.0/docs/Simple-FileIO.html)
- C#: [using statement](https://learn.microsoft.com/en-US/dotnet/csharp/language-reference/statements/using); [PHP RFC discussion on block scoping](https://discourse.thephp.foundation/t/php-dev-rfc-discussion-use-construct-block-scoping/4678?page=2)
- Odin and others: [C3 design blog](https://c3.handmade.network/blog/4), [Odin grammar](https://odin-lang.org/spec/grammar/), [Odin overview](https://odin-lang.org/docs/overview)
