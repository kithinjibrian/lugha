<p align="center">
  <img src="editors/vscode/icons/lugha.svg" alt="The Lugha icon: a baobab tree in front of the sun" width="128">
</p>

<h1 align="center">Lugha</h1>

<p align="center"><b>A language that doesn't get in your way, but still has your back.</b></p>

---

*Lugha* is the Swahili word for "language". It is a small programming language I am building. You
write your program in a file that ends in `.la`. Then `lughac`, the Lugha compiler, turns it into a
program your computer can run on its own.

```lugha
fun fib(n: i64): i64 =
    if n < 2 { n } else { fib(n - 1) + fib(n - 2) };

fun main(): i32 {
    println(fib(30));
    0
}
```

```console
$ lughac run fib.la
832040
```

## How it started

The first version of Lugha could do one thing. It could work out a sum inside `main`, like this:

```lugha
fun main(): i32 { 2 + 3 * 4 }
```

The program did the maths and quit with the answer, 14. That was all.

Then I added one small piece at a time: variables and loops, then structs and arrays, then enums,
modules, generics, closures and much more. Lugha is now on its seventeenth version.

Today it is big enough to build [Rapidkoi](https://github.com/kithinjibrian/rapidkoi), a browser for
apps. In Rapidkoi, apps don't decide what they take from you. You decide what they get. Rapidkoi is
written in Lugha.

<p align="center">
  <img src="https://raw.githubusercontent.com/kithinjibrian/rapidkoi/main/docs/screenshot.png" alt="Rapidkoi's new tab" width="720">
</p>

## What it is like

### Every value is your own

When you give a value a new name, you get your own copy. Changing the copy never changes the
original.

```lugha
struct Point { x: i64, y: i64 }

fun main(): i32 {
    let a = Point { x: 1, y: 2 };
    let mut b = a;
    b.x = 9;
    println(a);
    println(b);
    0
}
```

```console
Point { x: 1, y: 2 }
Point { x: 9, y: 2 }
```

Copies are cheap. Two copies of an array share the same memory until one of them changes. When you
really do want two names for one thing, you put it in a box called `Ref`, and the type shows it.

### Mistakes are caught early

`lughac` checks your program before it runs. When something is wrong, it says where, and often how
to fix it:

```console
$ lughac check push.la
error[E0501]: cannot call `mut self` method `push` on `xs`: it is not mutable
  --> push.la:3:5
  |
2 |     let xs = [1, 2, 3];
  |         -- declared here
3 |     xs.push(4);
  |     ^^
  |
  = help: make it mutable: `let mut xs`

For more about E0501, run `lughac explain E0501`.
```

Some mistakes can only show up while the program runs. Reading past the end of an array is one. A
number that grows too big to fit is another. Lugha never lets these slip by quietly. It stops the
program and tells you what happened and where:

```console
panic: index out of bounds: the length is 3 but the index is 5 at oob.la:3:15
```

### Doing many things at once

`spawn` runs a function next to the rest of your program. An *actor* is a value that answers
messages one at a time, so two parts of your program can never change it at the same moment.

```lugha
actor Counter {
    count: i64,
    fun add(mut self, n: i64) {
        self.count += n;
    }
    fun get(self): i64 = self.count;
}

fun work(c: Counter, n: i64) {
    c.add(n);
}

fun main() {
    let c = spawn Counter { count: 0 };
    let tasks = [spawn work(c, 1), spawn work(c, 2), spawn work(c, 3)];
    for t of tasks {
        t.join();
    }
    println("count = {c.get()}");
}
```

```console
count = 6
```

### Using C libraries

A lot of useful code is written in C. Lugha can call it directly. You write one line saying what
the C function looks like, and list the library in your project's `lugha.toml`. That is how
Rapidkoi draws its windows with GTK and keeps its data in SQLite.

## Things to know before you start

- **It is young.** Lugha changes as I learn what works. A new version can break programs written
  for the last one. There are only a few libraries so far.
- **It runs on Linux.** It works best on x86-64 computers. It can also build programs for ARM64.
- **It has a garbage collector.** Lugha frees memory for you. This is easier, but it uses more
  memory than freeing it by hand.
- **The safety checks are always on.** Every sum and every array index is checked, even in a fast
  (`-O2`) build. This costs a little speed.

## Try it

The easiest way to try Lugha is the [playground](https://lugha-lang.org/playground). You write a
program in your browser, press Run, and see what it prints. Nothing to install.

If you are new to Lugha, the [lessons](https://lugha-lang.org/learn) go one small idea at a time,
and the [examples](https://lugha-lang.org/examples) show whole programs you can open in the
playground.

## The `lughac` command

| Command | What it does |
| --- | --- |
| `lughac init [folder] [--lib]` | Start a new program, or a library |
| `lughac run prog.la` | Build a program and run it |
| `lughac build prog.la [-O2]` | Build a program you can share |
| `lughac check prog.la` | Look for mistakes without building |
| `lughac test` | Run the tests (functions marked `@test`) |
| `lughac fmt` | Lay out your code in the standard style |
| `lughac explain E0501` | Explain an error, with an example and the fix |
| `lughac spec` | Print the full description of the language |

The [language specification](docs/specs/Language%20v17%20Specification.md) describes every rule,
with example programs. `lughac spec` prints the copy that matches your compiler.

## Editors

The VS Code extension in [`editors/vscode/`](editors/vscode) shows mistakes as you type. It also
completes names, jumps to where things are defined, renames, formats, and runs your tests.

```sh
cd editors/vscode && npm ci && npm run package
code --install-extension lugha-*.vsix
```

Other editors, such as Neovim, Helix and Zed, can start `lughac lsp`. In Neovim 0.11 or later:

```lua
vim.filetype.add({ extension = { la = "lugha" } })
vim.lsp.config("lugha", {
  cmd = { "lughac", "lsp" },
  filetypes = { "lugha" },
  root_markers = { "lugha.toml", ".git" },
})
vim.lsp.enable("lugha")
```

## Working on lughac

To build `lughac` on your own computer, you need Linux, Rust, LLVM 21 (`llvm-21-dev`) and a C
compiler.

```sh
cargo install --path .
cargo test        # needs llvm-21-dev, a C compiler and libssl-dev
cargo fmt --check && cargo clippy --all-targets -- -D warnings
```

The test programs live in [`tests/programs/`](tests/programs), one folder for each feature. A
test is a `.la` file with the output it should print next to it. To add one, add the files.

Each feature starts as a short plan in [`PRPs/`](PRPs). [`DECISIONS.md`](DECISIONS.md) records
each design choice and why it was made, and [`docs/CODE_STYLE.md`](docs/CODE_STYLE.md) says how the
code is written. [`TODO.md`](TODO.md) lists what comes next.

## License

[GPL-3.0](LICENSE).
