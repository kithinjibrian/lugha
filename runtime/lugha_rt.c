/*
 * lugha_rt.c — the Lugha runtime library (spec §7, §9).
 *
 * Linked into every program. Provides heap allocation through the Boehm
 * collector, panics, and the printing and formatting behind the `print`,
 * `println` and `to_string` intrinsics. All output goes through C's `stdout`
 * so it interleaves with libc calls in program order (spec §9).
 *
 * ABI notes: `bool` and `u8` arguments arrive zero-extended to 32 bits, so
 * no function depends on how a C compiler extends narrower parameters.
 */

#include <gc.h>
#include <inttypes.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* A Lugha string: length, then the bytes and a terminating NUL (spec §7). */
typedef struct {
    int64_t len;
    char bytes[];
} LughaString;

void lugha_rt_init(void);
void *lugha_rt_alloc(int64_t size);
void lugha_rt_panic(const LughaString *msg, const char *file, int64_t line, int64_t col);
void lugha_rt_panic_bounds(int64_t len, int64_t index, const char *file, int64_t line, int64_t col);
LughaString *lugha_rt_str_concat(const LughaString *a, const LughaString *b);
int32_t lugha_rt_str_eq(const LughaString *a, const LughaString *b);
void lugha_rt_print_i32(int32_t value);
void lugha_rt_print_i64(int64_t value);
void lugha_rt_print_u8(uint32_t value);
void lugha_rt_print_f64(double value);
void lugha_rt_print_bool(int32_t value);
void lugha_rt_print_str(const LughaString *value);
void lugha_rt_print_newline(void);
LughaString *lugha_rt_to_string_i32(int32_t value);
LughaString *lugha_rt_to_string_i64(int64_t value);
LughaString *lugha_rt_to_string_u8(uint32_t value);
LughaString *lugha_rt_to_string_f64(double value);
LughaString *lugha_rt_to_string_bool(int32_t value);

/* Room for any formatted primitive: the longest is an f64 in fixed notation
 * just below 1e16 with 17 significant digits, or "-9223372036854775808". */
enum { FORMAT_CAP = 64 };

/* Called by the C `main` before the program starts (spec §6). */
void lugha_rt_init(void) {
    GC_INIT();
}

static void die(const char *message) {
    fflush(stdout);
    fputs(message, stderr);
    exit(101);
}

void *lugha_rt_alloc(int64_t size) {
    void *memory = GC_malloc((size_t)size);
    if (memory == NULL) {
        die("panic: out of memory\n");
    }
    return memory;
}

void lugha_rt_panic(const LughaString *msg, const char *file, int64_t line, int64_t col) {
    // Flush first so everything the program printed appears before the panic.
    fflush(stdout);
    fputs("panic: ", stderr);
    // fwrite, never printf: the message is program data, not a format string.
    fwrite(msg->bytes, 1, (size_t)msg->len, stderr);
    fprintf(stderr, " at %s:%" PRId64 ":%" PRId64 "\n", file, line, col);
    exit(101);
}

/* Out-of-range indexing (spec §5, §9); `index` may be negative. */
void lugha_rt_panic_bounds(int64_t len, int64_t index, const char *file, int64_t line, int64_t col) {
    fflush(stdout);
    fprintf(stderr,
            "panic: index out of bounds: the length is %" PRId64 " but the index is %" PRId64
            " at %s:%" PRId64 ":%" PRId64 "\n",
            len, index, file, line, col);
    exit(101);
}

/* `a + b` for strings: a new string holding both (spec §4). */
LughaString *lugha_rt_str_concat(const LughaString *a, const LughaString *b) {
    int64_t len = a->len + b->len;
    LughaString *s = lugha_rt_alloc((int64_t)sizeof(LughaString) + len + 1);
    s->len = len;
    memcpy(s->bytes, a->bytes, (size_t)a->len);
    memcpy(s->bytes + a->len, b->bytes, (size_t)b->len);
    s->bytes[len] = '\0';
    return s;
}

/* `a == b` for strings compares contents (spec §4); 1 or 0. */
int32_t lugha_rt_str_eq(const LughaString *a, const LughaString *b) {
    return a->len == b->len && memcmp(a->bytes, b->bytes, (size_t)a->len) == 0;
}

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
