
/* Bytes (v14 draft §5, milestone 45): binary files, stdin as bytes, and
 * conversions between `string` and `u8[]`. Text never holds invalid UTF-8:
 * `lugha_rt_utf8_check` is the only way from bytes to a string. */

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

/* The length of the UTF-8 sequence at `b[i]`, or 0 if it is invalid there:
 * no overlong forms, no surrogates, nothing above U+10FFFF, no truncation. */
static int64_t utf8_step(const unsigned char *b, int64_t i, int64_t len) {
    unsigned char c = b[i];
    if (c < 0x80) {
        return 1;
    }
    int64_t n;
    uint32_t min;
    uint32_t cp;
    if (c >= 0xC2 && c <= 0xDF) {
        n = 2, min = 0x80, cp = c & 0x1Fu;
    } else if (c >= 0xE0 && c <= 0xEF) {
        n = 3, min = 0x800, cp = c & 0x0Fu;
    } else if (c >= 0xF0 && c <= 0xF4) {
        n = 4, min = 0x10000, cp = c & 0x07u;
    } else {
        return 0;
    }
    if (i + n > len) {
        return 0;
    }
    for (int64_t k = 1; k < n; k++) {
        if ((b[i + k] & 0xC0u) != 0x80u) {
            return 0;
        }
        cp = (cp << 6) | (b[i + k] & 0x3Fu);
    }
    if (cp < min || cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF)) {
        return 0;
    }
    return n;
}

/* `string_from_utf8(bytes)`: 1 and a string of exactly those bytes, or 0
 * and `invalid UTF-8 at byte N` for the first bad sequence. */
int32_t lugha_rt_utf8_check(const LughaArray *bytes, LughaString **out, LughaString **error) {
    int64_t i = 0;
    while (i < bytes->len) {
        int64_t step = utf8_step(bytes->elems, i, bytes->len);
        if (step == 0) {
            char text[64];
            snprintf(text, sizeof text, "invalid UTF-8 at byte %" PRId64, i);
            *error = string_of(text, (int64_t)strlen(text));
            return 0;
        }
        i += step;
    }
    *out = string_of((const char *)bytes->elems, bytes->len);
    return 1;
}
