/*
 * print.c — part of lugha_rt.c: formatting and the printing behind the
 * `print`, `println` and `to_string` intrinsics (spec §5).
 *
 * A fragment, not compiled on its own: lughac's link.rs joins it after the
 * core (lugha_rt.c) and stack.c, relying on the core's includes, types and
 * `FORMAT_CAP`.
 */

/* Appends `text` to `out` at `*used`, never past `cap - 1` bytes. */
static void put(char *out, size_t cap, size_t *used, const char *text, size_t n) {
    if (*used + n >= cap) {
        n = cap - 1 - *used;
    }
    memcpy(out + *used, text, n);
    *used += n;
    out[*used] = '\0';
}

/*
 * Spec §5: the shortest decimal that round-trips, in Lugha float syntax —
 * always a `.` with a digit on each side, and an exponent (`1.0e16`,
 * `2.5e-7`) when the magnitude is >= 1e16 or below 1e-5. With `single`
 * (v7 `f32`), "round-trips" means reads back as the same float.
 */
static void format_float(double x, bool single, char *out, size_t cap) {
    if (isnan(x)) {
        snprintf(out, cap, "NaN");
        return;
    }
    if (isinf(x)) {
        snprintf(out, cap, x < 0 ? "-inf" : "inf");
        return;
    }
    if (x == 0.0) {
        snprintf(out, cap, signbit(x) ? "-0.0" : "0.0");
        return;
    }
    // The fewest significant digits whose %e form reads back as exactly x.
    char sci[FORMAT_CAP];
    for (int precision = 0; precision <= 16; precision++) {
        snprintf(sci, sizeof sci, "%.*e", precision, x);
        if (single ? (double)strtof(sci, NULL) == x : strtod(sci, NULL) == x) {
            break;
        }
    }
    // Split "-d.ddde+XX" into sign, digits and exponent.
    const char *s = sci;
    bool negative = *s == '-';
    if (negative) {
        s++;
    }
    char digits[FORMAT_CAP];
    size_t n = 0;
    for (; *s != '\0' && *s != 'e'; s++) {
        if (*s != '.') {
            digits[n++] = *s;
        }
    }
    int exponent = atoi(s + 1);

    size_t used = 0;
    out[0] = '\0';
    if (negative) {
        put(out, cap, &used, "-", 1);
    }
    double magnitude = fabs(x);
    if (magnitude >= 1e16 || magnitude < 1e-5) {
        put(out, cap, &used, digits, 1);
        put(out, cap, &used, ".", 1);
        if (n > 1) {
            put(out, cap, &used, digits + 1, n - 1);
        } else {
            put(out, cap, &used, "0", 1);
        }
        char tail[16];
        int len = snprintf(tail, sizeof tail, "e%d", exponent);
        put(out, cap, &used, tail, (size_t)len);
    } else if (exponent >= 0) {
        size_t whole = (size_t)exponent + 1;
        for (size_t i = 0; i < whole; i++) {
            put(out, cap, &used, i < n ? &digits[i] : "0", 1);
        }
        put(out, cap, &used, ".", 1);
        if (n > whole) {
            put(out, cap, &used, digits + whole, n - whole);
        } else {
            put(out, cap, &used, "0", 1);
        }
    } else {
        put(out, cap, &used, "0.", 2);
        for (int i = 0; i < -exponent - 1; i++) {
            put(out, cap, &used, "0", 1);
        }
        put(out, cap, &used, digits, n);
    }
}

static LughaString *make_string(const char *text) {
    size_t len = strlen(text);
    LughaString *s = lugha_rt_alloc((int64_t)(sizeof(LughaString) + len + 1));
    s->len = (int64_t)len;
    memcpy(s->bytes, text, len + 1);
    return s;
}

void lugha_rt_print_i32(int32_t value) {
    printf("%" PRId32, value);
}

void lugha_rt_print_i64(int64_t value) {
    printf("%" PRId64, value);
}

void lugha_rt_print_u8(uint32_t value) {
    printf("%" PRIu32, value & 0xFFu);
}

void lugha_rt_print_f64(double value) {
    char text[FORMAT_CAP];
    format_float(value, false, text, sizeof text);
    fputs(text, stdout);
}

void lugha_rt_print_u64(uint64_t value) {
    printf("%" PRIu64, value);
}

void lugha_rt_print_f32(float value) {
    char text[FORMAT_CAP];
    format_float((double)value, true, text, sizeof text);
    fputs(text, stdout);
}

void lugha_rt_print_bool(int32_t value) {
    fputs(value ? "true" : "false", stdout);
}

void lugha_rt_print_str(const LughaString *value) {
    // Exactly `len` bytes: embedded NULs and `%` print as themselves.
    fwrite(value->bytes, 1, (size_t)value->len, stdout);
}

void lugha_rt_print_newline(void) {
    fputc('\n', stdout);
}

LughaString *lugha_rt_to_string_i32(int32_t value) {
    char text[FORMAT_CAP];
    snprintf(text, sizeof text, "%" PRId32, value);
    return make_string(text);
}

LughaString *lugha_rt_to_string_i64(int64_t value) {
    char text[FORMAT_CAP];
    snprintf(text, sizeof text, "%" PRId64, value);
    return make_string(text);
}

LughaString *lugha_rt_to_string_u8(uint32_t value) {
    char text[FORMAT_CAP];
    snprintf(text, sizeof text, "%" PRIu32, value & 0xFFu);
    return make_string(text);
}

LughaString *lugha_rt_to_string_f64(double value) {
    char text[FORMAT_CAP];
    format_float(value, false, text, sizeof text);
    return make_string(text);
}

LughaString *lugha_rt_to_string_u64(uint64_t value) {
    char text[FORMAT_CAP];
    snprintf(text, sizeof text, "%" PRIu64, value);
    return make_string(text);
}

LughaString *lugha_rt_to_string_f32(float value) {
    char text[FORMAT_CAP];
    format_float((double)value, true, text, sizeof text);
    return make_string(text);
}

LughaString *lugha_rt_to_string_bool(int32_t value) {
    return make_string(value ? "true" : "false");
}

/* The escape for byte `c` inside a quoted string (v12), or NULL when it is
 * copied as itself; bytes below 0x20 without a short form use `\u{XX}`. */
static const char *escape_of(unsigned char c, char *buf) {
    switch (c) {
    case '"': return "\\\"";
    case '\\': return "\\\\";
    case '\n': return "\\n";
    case '\t': return "\\t";
    case '\r': return "\\r";
    default:
        if (c < 0x20) {
            snprintf(buf, 8, "\\u{%02x}", c);
            return buf;
        }
        return NULL;
    }
}

/* `s` between double quotes with `"`, `\` and control bytes escaped, as
 * strings appear inside a printed struct, enum or array (v12). The length
 * is counted first, so the result is allocated once. */
LughaString *lugha_rt_str_quote(const LughaString *s) {
    char buf[8];
    int64_t len = 2;
    for (int64_t i = 0; i < s->len; i++) {
        const char *e = escape_of((unsigned char)s->bytes[i], buf);
        len += e ? (int64_t)strlen(e) : 1;
    }
    LughaString *out = lugha_rt_alloc((int64_t)sizeof(LughaString) + len + 1);
    out->len = len;
    char *at = out->bytes;
    *at++ = '"';
    for (int64_t i = 0; i < s->len; i++) {
        const char *e = escape_of((unsigned char)s->bytes[i], buf);
        if (e) {
            size_t n = strlen(e);
            memcpy(at, e, n);
            at += n;
        } else {
            *at++ = s->bytes[i];
        }
    }
    *at++ = '"';
    *at = '\0';
    return out;
}

/* Boxes being printed, innermost last (v16, milestone 67): printing a box
 * that is already on the stack would never end, so it prints as `Ref(…)`.
 * The stack lives in GC memory, which Boehm scans through this root. */
static void **show_stack = NULL;
static int64_t show_len = 0;
static int64_t show_cap = 0;

/* 1 after pushing `box`, or 0 if it is already being printed. */
int32_t lugha_rt_show_enter(void *box) {
    for (int64_t i = 0; i < show_len; i++) {
        if (show_stack[i] == box) {
            return 0;
        }
    }
    if (show_len == show_cap) {
        int64_t cap = show_cap == 0 ? 16 : show_cap * 2;
        void **bigger = lugha_rt_alloc(cap * (int64_t)sizeof(void *));
        if (show_len > 0) {
            memcpy(bigger, show_stack, (size_t)show_len * sizeof(void *));
        }
        show_stack = bigger;
        show_cap = cap;
    }
    show_stack[show_len++] = box;
    return 1;
}

/* Pops the box `lugha_rt_show_enter` pushed last. */
void lugha_rt_show_leave(void) {
    if (show_len > 0) {
        show_len--;
    }
}
