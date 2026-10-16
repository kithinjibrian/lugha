## FEATURE: `lugha-gtk` — a GTK 4 binding package in its own repository, with Lugha-style wrappers (widgets as handle types with methods, closures for signals).

**Status:** approved 2026-10-16 ("yes build all three", session 190); proposed in session 189 with PRP-119 and PRP-120 as one batch.
**Milestone:** none in lughac (it changes no compiler code); the package is tagged `v0.1.0` in its own repository.
**Spec:** none changed. Uses spec §6 (packages, `[link]`), §8 (handles, callbacks, strings from C — milestones 87–89).

**Decisions (discovery, session 189; the user's choices):**
- its own repository, `lugha-gtk`, used as a git package; built locally in `/home/kithinji/Documents/kithinji/lugha-gtk`, and the GitHub repository created and pushed only when the user says so;
- Lugha-style wrappers: handle types with methods and closures for signals; the raw externs `public` in one module for anything not wrapped.
- Decided by me, for review: package name `gtk`, `source = "src"`, `[link] pkg-config = ["gtk4"]`; license GPL-3.0-only like lugha; first set: Application, Window, Box (row/column), Button, Label, Entry, a widget conversion (`b.widget()`), `on_click`, `on_change`, `on_activate`.
- No open entries in DECISIONS.md apply.

## OBJECTIVE
```
import gtk.app;
import gtk.widgets { Window, Box, Button, Label };

fun main(): i32 {
    app.run("org.lugha.counter", fun(a: app.Application) {
        let window = Window.new(a, "Counter", 320, 160);
        let label = Label.new("0");
        let button = Button.new("add one");
        let n = Ref.new(0);
        button.on_click(fun() {
            n.update(fun(x) = x + 1);
            label.set_text("{n.get()}");
        });
        window.set_child(Box.column([label.widget(), button.widget()]));
        window.present();
    })
}
```

## CONTEXT
- **Starting state:** milestone 88's GTK counter (scratchpad) works with hand-written externs; glowworm (`/home/kithinji/Documents/kithinji/glowworm`) is the layout model (`[package]` with `source = "src"`, programs in `example/`). The GTK development files and `pkg-config` aren't installed on this machine.
- **Ending state** (`/home/kithinji/Documents/kithinji/lugha-gtk`, a git repository with its own history):
  - `lugha.toml`: `[package] name = "gtk"`, `version = "0.1.0"`, `source = "src"`; `[link] pkg-config = ["gtk4"]`.
  - `src/raw.la`: `public extern type`s (`GtkApplication`, `GtkWidget`) and `public` externs, renamed `g_signal_connect_data` per signal shape.
  - `src/app.la`: `Application` (a struct around the handle), `run(id, on_activate): i32`.
  - `src/widgets.la`: `Widget`, `Window`, `Box` (`row`, `column`, `append`), `Button` (`new`, `set_label`, `on_click`), `Label` (`new`, `set_text`, `text`), `Entry` (`new`, `text`, `set_text`, `on_change`, `on_activate`), each with `widget()`.
  - `examples/counter.la`, `examples/greeter.la` (an entry and a label: strings both ways).
  - `README.md` (install `libgtk-4-dev` and `pkgconf`; usage as a git package), `LICENSE` (GPL-3.0-only).
  - **lughac repo:** README (a pointer to lugha-gtk), TODO, CHANGELOG, MEMORY, CONTEXT. No compiler change: anything missing in lughac found while building is stopped and reported (SCOPE RULE).
- **Related existing code:** `tests/programs` multi-file cases (for module layout), glowworm.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- Every module checks with `lughac check` (needs no GTK); every example builds once `libgtk-4-dev` and `pkgconf` are installed.
- Wrappers hold the handle in a struct (`struct Button { w: raw.GtkWidget }`) so each widget kind is its own type.
### Must NOT Do
- No GitHub repository, push or tag without the user's go-ahead (outward-facing).
- No compiler change in this PRP.
### New dependencies
- None for lughac. The package needs GTK 4 (LGPL-2.1+, compatible with GPL-3.0) at build time.

## ERROR HANDLING REQUIREMENTS
- GTK's own failures surface as GTK prints them; the wrappers add no error types in 0.1.0.

## SECURITY CONSIDERATIONS
- The package's externs are its trust point (spec §7); each is checked against GTK 4's headers by hand and listed in the README's coverage table.

## TESTS TO WRITE
- [ ] `lughac check` on every example (CI-able without GTK).
- [ ] `lughac build` on every example with GTK installed (manual until the user installs `libgtk-4-dev`).
- [ ] Manual: run both examples; the counter counts, the greeter echoes the entry.

## ROLLBACK PLAN
- Delete the local `lugha-gtk` folder; nothing in lughac depends on it.

## ACCEPTANCE CRITERIA
- [ ] The objective program builds against the installed GTK and runs (the user's run).
- [ ] The lughac repo's suite unchanged and passing.

## VALIDATION
- `lughac check examples/*.la`, `lughac build examples/counter.la` (in lugha-gtk)
