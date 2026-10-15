# Lugha v16 — Draft (delta against v15)

**Status:** milestone 67 (shared boxes, `Ref<T>`) approved in session 136 (proposed in session 135), and built. Milestone 68 (changing the working directory) proposed in session 137.
**Base:** `Language v15 Specification.md`, complete (milestones 51–66). It stays the source of truth, and what `lughac spec` prints, until v16's first milestone starts `Language v16 Specification.md`. That file then grows milestone by milestone, and v15 is frozen.

This draft lists only what changes, in the order of the v15 sections. Everything not mentioned keeps its v15 meaning.

**Compatibility.** Every valid v15 program stays valid, unless it declares its own type named `Ref`. A module's own `Ref` still means its own, as for `Iter` and `Ordering`.

## Decisions behind milestone 67 (discovery, session 135)

Found while writing firefly and oyster, and discussed in session 135: without any shared reference, a closure can't keep state (E0511), and linked or graph-shaped data must use index handles. The user chose a small, explicit shared box over classes or borrowed references, and to open v16 with it.

1. **`Ref<T>` is a shared box:** one GC object holding a `T`. Copying a `Ref` (assigning, passing, storing, capturing) copies the handle, never the box, so every copy sees the same contents. Every other type keeps value semantics; sharing appears only where a `Ref` is written.
2. **Copy in, copy out:**
   - `Ref.new(v: T): Ref<T>` makes a box;
   - `r.get(): T` gives a copy of the contents;
   - `r.set(v: T)` replaces them;
   - `r.update(f: fun(T): T)` is `r.set(f(r.get()))`;
   - `r.is(other: Ref<T>): bool` tells whether both are the same box.

   Nothing reaches inside a box in place: no `r.value.x = 1`, no field or index access through a `Ref`. So copy-on-write (milestone 55) and loop ownership (milestone 56) stay sound unchanged: a box is one holder of whatever it contains.
3. **Not `Eq`:**
   - `r == s` is E0404, with help: `r.is(s)` for the same box, or `r.get() == s.get()` for the same contents;
   - a struct, enum, tuple, array or `Option` holding a `Ref` isn't `Eq` either (E0419 for `T: Eq`), as for interface values.

   **Printing:** a box prints as `Ref(<contents>)`. A box already being printed (a cycle) prints as `Ref(…)`, so printing always ends.
4. **Changing a box needs no `let mut`:** `set` and `update` take plain `self`, because they change the box, not the handle. `let r = Ref.new(0); r.set(1);` is valid, and a closure may `set` or `update` a `Ref` it captured; E0511 still forbids changing the captured variable itself. `let mut r` is needed only to point `r` at another box (`r = Ref.new(5)`).
5. **Success:** milestone 67, tag `m67`. v15 is declared complete and frozen; `Language v16 Specification.md` starts and `lughac spec` prints it. Tests cover:
   - checker rules (not `Eq`, the `let` rules, generics, recursion through `Ref`);
   - programs: a stateful closure, a doubly linked list, `Ref` in structs, arrays and interface values, values taken out with `get` never changing when the box does, and cyclic printing;
   - a §10 program.

   No library changes.

## Decisions behind milestone 68 (discovery, session 137)

Found building oyster, a shell in Lugha: `cd` was impossible, because v15's `std.env` was read-only (session 127, decision 48) and `std.process` takes no directory.

6. **`env.set_current_dir(path): Result<i64, string>`** changes the program's working directory (`chdir`): `Ok(0)`, or `Err` with the system's message (a missing folder, a file, no permission); a NUL byte in `path` is `Err("path contains a NUL byte")`. It is process-wide, so relative paths in the file intrinsics and `std.fs` resolve against it, and every program `std.process` starts afterwards runs there. Environment variables stay read-only.
7. **Shell conveniences are the program's job:** `~`, `cd` alone and `cd -` are oyster's, not the language's.
8. **Success:** milestone 68, tag `m68`. Tests: `current_dir`, a relative `read_file`, `std.fs` and a child process all see the new directory; `Err` for a missing folder, a file and a NUL. Then oyster's `cd` (`cd dir`, `cd` to `$HOME`, `cd -`, `~`), left uncommitted with the rest of oyster.

## §4 Type system — changes

- **`Ref<T>`**, predeclared like `Option`, for any `T`. Recursion through a `Ref` is allowed, as through an array: `struct Node { value: i64, next: Option<Ref<Node>> }` needs no indirect slot.
- **Equality:** decision 3.
- **Copies:** the Array copies table gains a row: a `Ref` is never copied, only its handle.

## §7 Memory model — changes

- **A `Ref` is a pointer** to one GC object holding its contents, laid out as the contents would be in an array element.
- **Two kinds of shared object:** boxes are the only objects two Lugha values may share and change (strings are shared but immutable). The ownership sentence (copy-on-write) reads: every array has one holder, a box counting as one.

## §9 Compilation model — changes

- **`Ref.new`:** an allocation through `lugha_rt_alloc` and a store.
- **`get`:** a load and a copy site (copy-on-write marks arrays).
- **`set`:** a store following the normal store rules.
- **`is`:** a pointer comparison.
- **Printing:** `lugha_show_Ref<T>` keeps a stack of boxes being printed.

## §10 Example programs — additions

- **M67:** a counter closure over a `Ref`, a two-node cycle printed with `Ref(…)`, and `get` giving a copy that later `set`s don't change.

## §11 Milestones — additions

| # | Milestone | Scope |
| --- | --- | --- |
| 67 | Shared boxes | `Ref<T>`: `new`, `get`, `set`, `update`, `is`; not `Eq`; cyclic printing |
| 68 | Changing directory | `env.set_current_dir(path)` |

## §12 Open questions — additions

- [ ] **Editing a box in place** (v16). `r.with(fun(mut x: T) { … })` could avoid the copy `update` makes of big contents, if the closure can be kept from reaching the same box.
- [ ] **A bounds-checked byte buffer** (v16, session 135): the safe answer to pointer-style code over raw bytes.
