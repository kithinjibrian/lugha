
/* Bytes (v14 draft §5, milestone 45): binary files, stdin as bytes, and
 * conversions between `string` and `u8[]`. Text never holds invalid UTF-8:
 * the prelude's `text_from_utf8` checks bytes before
 * `lugha_rt_str_from_bytes` turns them into a string (v15). */

/* A new `u8[]` holding a copy of `len` bytes at `data`. */
static LughaArray *bytes_of(const void *data, int64_t len) {
    LughaArray *array = lugha_rt_alloc((int64_t)sizeof(LughaArray) + len);
    array->len = len;
    array->cap = len;
    if (len > 0) {
        memcpy(array->elems, data, (size_t)len);
    }
    return array;
}

/* `read_bytes(path)`: 1 and the file's bytes in `*out`, or 0 and the
 * system's message in `*error`. */
int32_t lugha_rt_read_bytes(const LughaString *path, LughaArray **out, LughaString **error) {
    LughaString *text = NULL;
    if (!lugha_rt_read_file(path, &text, error)) {
        return 0;
    }
    *out = bytes_of(text->bytes, text->len);
    return 1;
}

/* `write_bytes(path, data)` (or `append_bytes` when `append` is 1). */
int32_t lugha_rt_write_bytes(const LughaString *path, const LughaArray *data, int32_t append,
                             int64_t *count, LughaString **error) {
    LughaString *text = string_of((const char *)data->elems, data->len);
    return lugha_rt_write_file(path, text, append, count, error);
}

/* `read_stdin_bytes()`: everything left on stdin. */
LughaArray *lugha_rt_read_stdin_bytes(void) {
    size_t len = 0;
    size_t cap = 4096;
    char *buffer = malloc(cap);
    if (buffer == NULL) {
        die("panic: out of memory\n");
    }
    size_t got;
    while ((got = fread(buffer + len, 1, cap - len, stdin)) > 0) {
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
    LughaArray *bytes = bytes_of(buffer, (int64_t)len);
    free(buffer);
    return bytes;
}

/* `s.to_bytes()`: the string's bytes as a new `u8[]`. */
LughaArray *lugha_rt_str_to_bytes(const LughaString *s) {
    return bytes_of(s->bytes, s->len);
}

/* `$string.from_bytes(bytes)` (v15, milestone 62): bytes the prelude has
 * already checked as UTF-8, copied into a new string. */
LughaString *lugha_rt_str_from_bytes(const LughaArray *bytes) {
    return string_of((const char *)bytes->elems, bytes->len);
}
