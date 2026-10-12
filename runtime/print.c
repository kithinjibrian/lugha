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
 * `2.5e-7`) when the magnitude is >= 1e16 or below 1e-5.
 */
static void format_f64(double x, char *out, size_t cap) {
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
        if (strtod(sci, NULL) == x) {
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
    format_f64(value, text, sizeof text);
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
    format_f64(value, text, sizeof text);
    return make_string(text);
}

LughaString *lugha_rt_to_string_bool(int32_t value) {
    return make_string(value ? "true" : "false");
}
