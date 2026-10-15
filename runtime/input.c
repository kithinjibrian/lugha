
/* ---- input.c: command-line arguments and parsing text (v3 draft §5) ---- */

/* `args()`: a fresh `string[]` of the program's arguments on each call, so
 * no program can share or change the runtime's copy. */
LughaArray *lugha_rt_args(void) {
    int64_t count = program_argc < 0 ? 0 : program_argc;
    LughaArray *array =
        lugha_rt_alloc((int64_t)sizeof(LughaArray) + count * (int64_t)sizeof(LughaString *));
    array->len = count;
    array->cap = count;
    LughaString **elems = (LughaString **)(void *)array->elems;
    for (int64_t i = 0; i < count; i++) {
        size_t len = strlen(program_argv[i]);
        LughaString *s = lugha_rt_alloc((int64_t)sizeof(LughaString) + (int64_t)len + 1);
        s->len = (int64_t)len;
        memcpy(s->bytes, program_argv[i], len + 1);
        elems[i] = s;
    }
    return array;
}

/* Skips decimal digits from `*i`; true if there was at least one. */
static bool digits(const LughaString *s, int64_t *i) {
    int64_t start = *i;
    while (*i < s->len && s->bytes[*i] >= '0' && s->bytes[*i] <= '9') {
        (*i)++;
    }
    return *i > start;
}

/* `parse_f64(s)`: `-`? digits (`.` digits)? ([eE] [+-]? digits)? and nothing
 * else, checked here first because `strtod` also takes spaces, hex, `inf`
 * and `nan`. Lugha strings end in a NUL, so `strtod` can read the bytes. */
int32_t lugha_rt_parse_f64(const LughaString *s, double *out) {
    int64_t i = 0;
    if (i < s->len && s->bytes[i] == '-') {
        i++;
    }
    if (!digits(s, &i)) {
        return 0;
    }
    if (i < s->len && s->bytes[i] == '.') {
        i++;
        if (!digits(s, &i)) {
            return 0;
        }
    }
    if (i < s->len && (s->bytes[i] == 'e' || s->bytes[i] == 'E')) {
        i++;
        if (i < s->len && (s->bytes[i] == '+' || s->bytes[i] == '-')) {
            i++;
        }
        if (!digits(s, &i)) {
            return 0;
        }
    }
    if (i != s->len) {
        return 0;
    }
    *out = strtod(s->bytes, NULL);
    return 1;
}

/* ---- I/O (v4 draft §5): stdin lines, text files, stderr and exit ---- */

/* A new Lugha string holding the `len` bytes at `bytes`. */
static LughaString *string_of(const char *bytes, int64_t len) {
    LughaString *s = lugha_rt_alloc((int64_t)sizeof(LughaString) + len + 1);
    s->len = len;
    memcpy(s->bytes, bytes, (size_t)len);
    s->bytes[len] = '\0';
    return s;
}

/* The C library's message for the error `code`, as a Lugha string. */
static LughaString *error_text(int code) {
    const char *message = strerror(code);
    return string_of(message, (int64_t)strlen(message));
}

/* A path for the C library: NUL-terminated already, but one with a NUL
 * inside would name a different file, so it is refused. */
static bool path_ok(const LughaString *path, LughaString **error) {
    if (memchr(path->bytes, '\0', (size_t)path->len) != NULL) {
        *error = string_of("path contains a NUL byte", 24);
        return false;
    }
    return true;
}

/* `read_line()`: 1 and the next stdin line without its `\n` (and a `\r`
 * before it) in `*out`, or 0 at the end of input or on a read error.
 * Stdout is flushed first, so a prompt written with `print` shows. */
int32_t lugha_rt_read_line(LughaString **out) {
    fflush(stdout);
    char *line = NULL;
    size_t cap = 0;
    ssize_t n = getline(&line, &cap, stdin);
    if (n < 0) {
        free(line);
        return 0;
    }
    if (n > 0 && line[n - 1] == '\n') {
        n--;
    }
    if (n > 0 && line[n - 1] == '\r') {
        n--;
    }
    *out = string_of(line, n);
    free(line);
    return 1;
}

/* `read_file(path)`: 1 and the whole file in `*out`, or 0 and the system's
 * message in `*error`. Reads until the end, so pipes and growing files work. */
int32_t lugha_rt_read_file(const LughaString *path, LughaString **out, LughaString **error) {
    if (!path_ok(path, error)) {
        return 0;
    }
    FILE *file = fopen(path->bytes, "rb");
    if (file == NULL) {
        *error = error_text(errno);
        return 0;
    }
    size_t len = 0;
    size_t cap = 4096;
    char *buffer = malloc(cap);
    if (buffer == NULL) {
        die("panic: out of memory\n");
    }
    size_t got;
    while ((got = fread(buffer + len, 1, cap - len, file)) > 0) {
        len += got;
        if (len == cap) {
            cap *= 2;
            char *bigger = realloc(buffer, cap);
            if (bigger == NULL) {
                die("panic: out of memory\n");
            }
            buffer = bigger;
        }
    }
    int failed = ferror(file) ? errno : 0;
    fclose(file);
    if (failed != 0) {
        free(buffer);
        *error = error_text(failed);
        return 0;
    }
    *out = string_of(buffer, (int64_t)len);
    free(buffer);
    return 1;
}

/* `write_file(path, text)` (or `append_file` when `append` is 1): 1 and the
 * bytes written in `*count`, or 0 and the system's message in `*error`. */
int32_t lugha_rt_write_file(const LughaString *path, const LughaString *text, int32_t append,
                            int64_t *count, LughaString **error) {
    if (!path_ok(path, error)) {
        return 0;
    }
    FILE *file = fopen(path->bytes, append ? "ab" : "wb");
    if (file == NULL) {
        *error = error_text(errno);
        return 0;
    }
    size_t written = fwrite(text->bytes, 1, (size_t)text->len, file);
    int failed = written != (size_t)text->len ? errno : 0;
    if (fclose(file) != 0 && failed == 0) {
        failed = errno;
    }
    if (failed != 0) {
        *error = error_text(failed);
        return 0;
    }
    *count = text->len;
    return 1;
}

/* `eprint(x)` / `eprintln(x)`, with `x` already formatted: stdout is
 * flushed first so a terminal shows output in program order. */
void lugha_rt_eprint(const LughaString *text, int32_t newline) {
    fflush(stdout);
    fwrite(text->bytes, 1, (size_t)text->len, stderr);
    if (newline) {
        fputc('\n', stderr);
    }
}

/* `exit(code)`: stdout flushed, then the process ends with `code`. */
void lugha_rt_exit(int32_t code) {
    fflush(stdout);
    exit(code);
}
