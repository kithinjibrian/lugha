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
 *
 * Platform: Linux with glibc (pthread_getattr_np for the stack bounds).
 *
 * Source layout: this core comes first (it enables GNU extensions and holds
 * every include, type and declaration), then stack.c (the stack overflow
 * handler), then print.c (formatting and printing), then input.c (arguments
 * and parsing, v3). lughac joins the four, in that order, into the single
 * lugha_rt.c it compiles with each program.
 */

#define _GNU_SOURCE

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <gc.h>
#include <gc/gc_mark.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <spawn.h>
#include <sys/mman.h>
#include <sys/random.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <inttypes.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* A Lugha string: length, then the bytes and a terminating NUL (spec §7). */
typedef struct {
    int64_t len;
    char bytes[];
} LughaString;

/* A Lugha array: length, capacity, then `cap` elements (v1 §7). */
typedef struct {
    int64_t len;
    int64_t cap;
    unsigned char elems[];
} LughaArray;

void lugha_rt_init(int32_t argc, char **argv);
LughaArray *lugha_rt_args(void);
int32_t lugha_rt_parse_f64(const LughaString *s, double *out);
int32_t lugha_rt_read_line(LughaString **out);
int32_t lugha_rt_read_file(const LughaString *path, LughaString **out, LughaString **error);
int32_t lugha_rt_write_file(const LughaString *path, const LughaString *text, int32_t append,
                            int64_t *count, LughaString **error);
void lugha_rt_eprint(const LughaString *text, int32_t newline);
void lugha_rt_exit(int32_t code);
void *lugha_rt_alloc(int64_t size);
void lugha_rt_panic(const LughaString *msg, const char *file, int64_t line, int64_t col);
void lugha_rt_panic_bounds(int64_t len, int64_t index, const char *file, int64_t line, int64_t col);
LughaString *lugha_rt_str_concat(const LughaString *a, const LughaString *b);
LughaString *lugha_rt_str_slice(const LughaString *s, int64_t from, int64_t to, const char *file,
                                int64_t line, int64_t col);
LughaArray *lugha_rt_array_grow(const LughaArray *array, int64_t elem_size);
int32_t lugha_rt_str_eq(const LughaString *a, const LughaString *b);
int32_t lugha_rt_str_cmp(const LughaString *a, const LughaString *b);
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
static void install_stack_overflow_handler(void);

/* A scalar value's UTF-8 (`chars.c`), also used by `format.c`'s padding,
 * which comes earlier in the joined file (v15). */
static int utf8_encode(uint32_t code, char *out);

/* Ends the program after a panic, or only the running actor (v17,
 * actors.c); `text` is what followed "panic: ". */
static void panic_end(const char *text, size_t len);

/* The C `main`'s arguments, for `args()` (v3). */
static int32_t program_argc;
static char **program_argv;

void lugha_rt_init(int32_t argc, char **argv) {
    program_argc = argc;
    program_argv = argv;
    GC_INIT();
    /* After GC_INIT: Boehm may use signals while it starts up. */
    install_stack_overflow_handler();
}

static void die(const char *message) {
    fflush(stdout);
    fputs(message, stderr);
    exit(101);
}

/* Prints `panic: <text>` (program data, so never a format string), then
 * ends the program or the running actor. */
static void panic_with(const char *text, size_t len) {
    fflush(stdout);
    fputs("panic: ", stderr);
    fwrite(text, 1, len, stderr);
    fputc('\n', stderr);
    panic_end(text, len);
}

/* `panic_with` for a message built from a format. */
__attribute__((format(printf, 1, 2))) static void panic_format(const char *format, ...) {
    char *text = NULL;
    va_list args;
    va_start(args, format);
    int len = vasprintf(&text, format, args);
    va_end(args);
    if (len < 0) {
        die("panic: out of memory\n");
    }
    panic_with(text, (size_t)len);
}

void *lugha_rt_alloc(int64_t size) {
    void *memory = GC_malloc((size_t)size);
    if (memory == NULL) {
        die("panic: out of memory\n");
    }
    return memory;
}

/* A copy of `array` with room for at least one more element: capacity
 * doubles, starting at 4. The old object is left to the GC; its single
 * owner switches to the copy (v1 §7). */
LughaArray *lugha_rt_array_grow(const LughaArray *array, int64_t elem_size) {
    /* The top bit of `cap` is the shared flag (v15). Arrays are made unique
     * before they grow, so it should be clear, but a set flag would read as
     * a negative capacity and shrink the array: mask it. */
    int64_t old_cap = array->cap & INT64_MAX;
    int64_t cap = old_cap < 2 ? 4 : old_cap * 2;
    /* `()` elements take no bytes (v17): only the length grows. */
    if (elem_size > 0 && cap > (INT64_MAX - (int64_t)sizeof(LughaArray)) / elem_size) {
        die("panic: out of memory\n");
    }
    LughaArray *grown = lugha_rt_alloc((int64_t)sizeof(LughaArray) + cap * elem_size);
    grown->len = array->len;
    grown->cap = cap;
    memcpy(grown->elems, array->elems, (size_t)(array->len * elem_size));
    return grown;
}

void lugha_rt_panic(const LughaString *msg, const char *file, int64_t line, int64_t col) {
    char *where = NULL;
    int tail = asprintf(&where, " at %s:%" PRId64 ":%" PRId64, file, line, col);
    char *text = tail < 0 ? NULL : malloc((size_t)msg->len + (size_t)tail);
    if (text == NULL) {
        die("panic: out of memory\n");
    }
    // memcpy, never printf: the message is program data, not a format string.
    memcpy(text, msg->bytes, (size_t)msg->len);
    memcpy(text + msg->len, where, (size_t)tail);
    panic_with(text, (size_t)msg->len + (size_t)tail);
}

/* Out-of-range indexing (spec §5, §9); `index` may be negative. */
void lugha_rt_panic_bounds(int64_t len, int64_t index, const char *file, int64_t line, int64_t col) {
    panic_format("index out of bounds: the length is %" PRId64 " but the index is %" PRId64
                 " at %s:%" PRId64 ":%" PRId64,
                 len, index, file, line, col);
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

/* `s.slice(from, to)` (v4): a new string of the bytes `from..to`. Panics at
 * the caller's location unless 0 <= from <= to <= len, so no byte outside
 * `s` is ever read. */
LughaString *lugha_rt_str_slice(const LughaString *s, int64_t from, int64_t to, const char *file,
                                int64_t line, int64_t col) {
    if (from < 0 || from > to || to > s->len) {
        panic_format("slice out of bounds: from %" PRId64 " to %" PRId64
                     " of a string of length %" PRId64 " at %s:%" PRId64 ":%" PRId64,
                     from, to, s->len, file, line, col);
    }
    int64_t len = to - from;
    LughaString *out = lugha_rt_alloc((int64_t)sizeof(LughaString) + len + 1);
    out->len = len;
    memcpy(out->bytes, s->bytes + from, (size_t)len);
    out->bytes[len] = '\0';
    return out;
}

/* `a == b` for strings compares contents (spec §4); 1 or 0. */
int32_t lugha_rt_str_eq(const LughaString *a, const LughaString *b) {
    return a->len == b->len && memcmp(a->bytes, b->bytes, (size_t)a->len) == 0;
}

/* `a < b` and friends for strings (v2 draft §4): bytewise, as unsigned
 * values, and a proper prefix is smaller. -1, 0 or 1. Reads only `len` bytes
 * of each, never relying on the terminating NUL. */
int32_t lugha_rt_str_cmp(const LughaString *a, const LughaString *b) {
    size_t shorter = (size_t)(a->len < b->len ? a->len : b->len);
    int order = memcmp(a->bytes, b->bytes, shorter);
    if (order != 0) {
        return order < 0 ? -1 : 1;
    }
    return a->len < b->len ? -1 : a->len > b->len ? 1 : 0;
}
