
/*
 * cstr.c — text from C (v16, milestone 89). C's NUL-terminated text is
 * copied into a new Lugha string; C keeps and frees its own memory. Each
 * invalid UTF-8 sequence becomes U+FFFD and the copy moves one byte on,
 * exactly as the prelude's `utf8_decode` and `Chars.next` decide.
 */

/* The length of the valid UTF-8 sequence at `p` with `avail` bytes left,
 * or 0: no overlong forms, no surrogates, nothing above U+10FFFF, no
 * truncation (the prelude's `utf8_decode`, in C). */
static int64_t utf8_length(const unsigned char *p, int64_t avail) {
    uint32_t b0 = p[0];
    if (b0 < 128) {
        return 1;
    }
    int64_t n = 0;
    uint32_t code = 0;
    uint32_t min = 0;
    if (b0 >= 194 && b0 <= 223) {
        n = 2;
        code = b0 & 31;
        min = 128;
    } else if (b0 >= 224 && b0 <= 239) {
        n = 3;
        code = b0 & 15;
        min = 2048;
    } else if (b0 >= 240 && b0 <= 244) {
        n = 4;
        code = b0 & 7;
        min = 65536;
    }
    if (n == 0 || n > avail) {
        return 0;
    }
    for (int64_t k = 1; k < n; k++) {
        if ((p[k] & 192) != 128) {
            return 0;
        }
        code = (code << 6) | (p[k] & 63);
    }
    if (code < min || code > 1114111 || (code >= 55296 && code <= 57343)) {
        return 0;
    }
    return n;
}

/* A copy of C's `text` as a Lugha string. `NULL` gives `NULL` when the
 * declared type is `Option<string>` (`nullable`), and panics otherwise. */
LughaString *lugha_rt_str_from_c(const char *text, int32_t nullable) {
    if (text == NULL) {
        if (nullable) {
            return NULL;
        }
        const char text[] = "C gave NULL for a `string`; declare it `Option<string>`";
        panic_with(text, sizeof text - 1);
    }
    const unsigned char *p = (const unsigned char *)text;
    int64_t len = (int64_t)strlen(text);
    /* First the copy's length: three bytes for each replacement. */
    int64_t out = 0;
    for (int64_t i = 0; i < len;) {
        int64_t n = utf8_length(p + i, len - i);
        out += n > 0 ? n : 3;
        i += n > 0 ? n : 1;
    }
    LughaString *s = lugha_rt_alloc((int64_t)sizeof(LughaString) + out + 1);
    s->len = out;
    int64_t at = 0;
    for (int64_t i = 0; i < len;) {
        int64_t n = utf8_length(p + i, len - i);
        if (n > 0) {
            memcpy(s->bytes + at, p + i, (size_t)n);
            at += n;
            i += n;
        } else {
            /* U+FFFD, the replacement character. */
            s->bytes[at++] = (char)0xEF;
            s->bytes[at++] = (char)0xBF;
            s->bytes[at++] = (char)0xBD;
            i += 1;
        }
    }
    s->bytes[out] = '\0';
    return s;
}
