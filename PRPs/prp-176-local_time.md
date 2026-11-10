## FEATURE: Calendar dates and the system's local offset in `std.time`: `Civil`, `civil`, `from_civil`, `local_offset`, `TimeError`; milestone 145, v17.

**Status:** approved 2026-10-31 ("build"); proposed 2026-10-31, from Rapidkoi's request (DECISION-086 there), after a short discovery (2 questions, each answered with the recommended option): (1) offsets are in seconds, as the system gives them (`tm_gmtoff`), so historical offsets that aren't whole minutes round-trip; (2) `from_civil` ignores the `weekday` it is given (the date decides it).

## OBJECTIVE
```lugha
import std.time;

fun main() {
    let now = time.now_ms();
    let offset = time.local_offset(now)?;               // seconds east of UTC, daylight saving included
    let c = time.civil(now, offset)?;
    println("{c.hour:02}:{c.minute:02}");                // 14:32, in the person's own zone
    let d = time.civil(1798761600000, 0)?;              // 2027-01-01T00:00Z
    println("{d.year}-{d.month:02}-{d.day:02} weekday {d.weekday}");   // 2027-01-01 weekday 5 (Friday)
    let back = time.from_civil(d, 0)?;                   // 1798761600000
    let bad = time.from_civil(time.Civil { year: 2026, month: 2, day: 31, weekday: 0, hour: 0, minute: 0, second: 0, ms: 0 }, 0);
    println(bad);                                        // Err(no day 31 in month 2 of 2026)
}
```
Any program showing a moment to a person can now say when it is in their zone.

## CONTEXT
- Read first: `src/std/time.la` (`now_ms` = `$os.wall_ms()`), how `$os` built-ins are typed (`check/float_ops.rs`, `os_signature`) and lowered (`codegen`'s `os_call`), `runtime/random.c` (`lugha_rt_now_ms`, the clocks), `src/std/toml_scan.la` (`days_in`).
- Changes: `src/std/time.la` (`Civil`, `TimeError`, `civil`, `from_civil`, `local_offset`, the calendar arithmetic in Lugha); one built-in `$os.local_offset(s): i64` plus a success flag, as other fallible `$os` calls report failure; `runtime/` (a `lugha_rt_local_offset(seconds, ok)` beside the clocks: `localtime_r`, `tm_gmtoff`); spec `std.time`, §9's runtime table, §11 row 145, "Changes since v16"; the v17 Draft.
- Decisions: none open. No dependency: `localtime_r` is libc, as the clocks already use it. Not security-sensitive (no process, git, `cc`/`pkg-config`, credentials); the only outside input is the system's zone (`TZ`, `/etc/localtime`), read by libc.

## REQUIREMENTS

### Must Do
- **`public struct Civil { year, month, day, weekday, hour, minute, second, ms }`**, every field `public` and `i64`: month 1–12, day 1–31, weekday 1 (Monday) to 7 (Sunday, ISO 8601), hour 0–23, minute 0–59, second 0–59, ms 0–999; years on the proleptic Gregorian calendar, before 1 too (year 0 is 1 BC, astronomical numbering).
- **`civil(ms: i64, offset_s: i64): Result<Civil, TimeError>`**: the moment `ms` (milliseconds since 1970-01-01T00:00Z, any `i64`, negative ones included) as seen at `offset_s` seconds east of UTC. Pure arithmetic (Howard Hinnant's `civil_from_days`), never overflowing: an offset beyond ±26 hours (93 600 s) is the only error.
- **`from_civil(c: Civil, offset_s: i64): Result<i64, TimeError>`**: the moment, inverse of `civil`; `c.weekday` is ignored. A field out of its range (month 13, 31 February, hour 24, second 60, ms 1000), an offset beyond ±26 hours, or a moment past `i64` is an error naming the problem: `no day 31 in month 2 of 2026`, `hour 24 isn't 0 to 23`.
- **`local_offset(ms: i64): Result<i64, TimeError>`**: the system zone's offset from UTC at that moment, in seconds, through `localtime_r` (`TZ` if set, else `/etc/localtime`; UTC when neither says), so daylight saving is included. A moment the system can't represent (`time_t` or `localtime_r` failing) is an error.
- **`public struct TimeError { public message: string }`** with `message()`, printing as its message.
- Nothing panics on any input, `i64.MIN` and `i64.MAX` included.
- Milestone 145; tag `m145`.

### Must NOT Do
- Formatting by locale, parsing dates from text, named zones other than the system's, a shipped zone database (Rapidkoi formats its own short strings).
- Change `now_ms` or `monotonic_ms`.

## BUILT — milestone 145
- As planned. `$os.local_offset(seconds): Result<i64, string>` goes through the generic `net_call` path (like `rename`), calling `lugha_rt_local_offset(seconds, out, error)` in `runtime/random.c` beside the clocks; registered in `link::RUNTIME_SYMBOLS`.
- `from_civil` folds the time of day less the offset into whole days before multiplying, and multiplies one day fewer before 1970, so every moment that fits `i64` comes back without an intermediate overflow (`civil` of `i64.MIN` and of `i64.MAX` at +26 h both round-trip).
- `local_offset(i64.MAX)` is `Ok` on glibc (it represents that year), so it isn't an error case.
- Tests: `tests/programs/m145/time.la` (round trips at 0, −1 ms, leap days, offsets −12 h, +5:30, +0:19:32, `i64.MIN`, `i64.MAX`, `i64.MAX` at +26 h; year 1, year 0, 1900; every error), same at `-O2`; `local.la` under `TZ=EST5EDT,M3.2.0,M11.1.0` (−18000 in January, −14400 in July); a unit test for the built-in's signature.

## DESIGN
- Days from civil and back after Hinnant (`days_from_civil`, `civil_from_days`), floor division throughout; the milliseconds split into days and the time of day before the offset is added, with the carry handled in days, so no step overflows for any `i64` moment. `from_civil` builds the moment with checked arithmetic (`checked_mul`/`checked_add`), an overflow being the "past `i64`" error.
- The runtime function takes whole seconds (`ms` floored), returns the offset and sets a flag; `std.time` turns a failure into `TimeError`.
- Sizes: `std/time.la` ~170 lines, runtime +20, checker/codegen +15.

## TESTS
- [ ] `tests/programs/m145/time.la`: `civil` and `from_civil` round trips across leap years (2000, 2024, 1900 not), month ends, negative moments (1969-12-31T23:59:59.999Z), year 1 and year 0, `i64.MIN`/`i64.MAX`, offsets −12 h, +5:30, +0:19:32; every `from_civil` error; weekdays of known dates.
- [ ] `tests/programs/m145/local.la` with `TZ=EST5EDT,M3.2.0,M11.1.0` (a POSIX rule, no zone database needed): −18000 s in January, −14400 s in July; with `TZ=UTC`: 0.
- [ ] Unit tests for the new `$os` built-in's typing.

## ACCEPTANCE
- [ ] The OBJECTIVE prints its lines (the first depending on the clock and zone).
- [ ] `cargo test --lib`, `--test programs`, `--test spec`, `--test lsp_docs`; `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`.
- [ ] Every `pub` item documented; spec, v17 Draft, CHANGELOG, TODO updated.
