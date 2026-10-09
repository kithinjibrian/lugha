# MEMORY.md — lugha

Records resolved architectural decisions and current project state.
Read this at the start of every session before writing any code.
Open questions live in `DECISIONS.md`, not here.

---

## ARCHITECTURAL DECISIONS

### 1. Language and license

**Decision:** The project is written in Rust and licensed GPL-3.0.

**Why:** Established by the initial commit (`Cargo` `.gitignore`, GPL-3.0 `LICENSE`).

**Rules out:** Dependencies with GPL-incompatible licenses.

---

### 2. Return-based error handling

**Decision:** Expected failures return `Result<T, E>` with typed error enums; panics are reserved for bugs. See CLAUDE.md → ERROR HANDLING.

**Why:** Makes every failure path visible in the type signature and impossible to ignore silently.

**Rules out:** `unwrap()` on external input, `Option` as an error signal, stringly-typed errors.

---

## CURRENT PROJECT STATE

### Fully Working
- AI context system scaffolded from `setup.md`

### In Progress
- Nothing

### Not Started
- Crate initialisation (blocked on DECISION-001)
- All features

---

## NEXT SESSION START POINT

Read `DECISIONS.md` and resolve DECISION-001 (what lugha is and how the crate is laid out) with the user. Then work through `TODO.md`.
