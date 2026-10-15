
/* Format specifiers (v13 draft §9): `{x:spec}` in an interpolation. The
 * compiler checks every specifier, so these trust their arguments: `fill`
 * is a Unicode scalar value, `align` 0 (left), 1 (right) or 2 (centre),
 * `width` and `precision` at most 1024. Widths count characters, not
 * bytes (v15, milestone 63). */

/* `len` bytes of `text`, UTF-8, padded with `fill` to `width` characters,
 * never cut: a character is a byte that isn't a continuation byte. */
static LughaString *pad_bytes(const char *text, int64_t len, int32_t fill, int32_t align,
                              int64_t width) {
    int64_t chars = 0;
    for (int64_t i = 0; i < len; i++) {
        chars += ((unsigned char)text[i] & 0xC0u) != 0x80u;
    }
    int64_t gap = chars < width ? width - chars : 0;
    int64_t left = align == 1 ? gap : (align == 2 ? gap / 2 : 0);
    char unit[4];
    int64_t step = utf8_encode((uint32_t)fill, unit);
    /* `gap` is at most 1024 and `step` at most 4, so this can't overflow. */
    int64_t total = len + gap * step;
    LughaString *out = lugha_rt_alloc((int64_t)sizeof(LughaString) + total + 1);
    out->len = total;
    char *at = out->bytes;
    for (int64_t k = 0; k < left; k++, at += step) {
        memcpy(at, unit, (size_t)step);
    }
    memcpy(at, text, (size_t)len);
    at += len;
    for (int64_t k = left; k < gap; k++, at += step) {
        memcpy(at, unit, (size_t)step);
    }
    out->bytes[total] = '\0';
    return out;
}

/* A number's text, zero-padded after its sign and `0x`-style prefix when
 * `zero` and no explicit fill applies, else padded like any text. */
static LughaString *pad_number(const char *text, int32_t fill, int32_t align, int64_t width,
                               int32_t zero) {
    int64_t len = (int64_t)strlen(text);
    if (!zero || len >= width) {
        return pad_bytes(text, len, fill, align, width);
    }
    int64_t lead = (text[0] == '-') ? 1 : 0;
    if (text[lead] == '0' && (text[lead + 1] == 'x' || text[lead + 1] == 'b' ||
                              text[lead + 1] == 'o')) {
        lead += 2;
    }
    LughaString *out = lugha_rt_alloc((int64_t)sizeof(LughaString) + width + 1);
    out->len = width;
    memcpy(out->bytes, text, (size_t)lead);
    memset(out->bytes + lead, '0', (size_t)(width - len));
    memcpy(out->bytes + lead + (width - len), text + lead, (size_t)(len - lead));
    out->bytes[width] = '\0';
    return out;
}

/* An integer of `bits` bits, `value` its sign- or zero-extended form.
 * `kind`: 0 decimal, 1 hex, 2 upper hex, 3 binary, 4 octal, the latter in
 * two's complement at `bits`; `alternate` adds `0x`, `0b` or `0o`. */
LughaString *lugha_rt_fmt_int(int64_t value, int32_t bits, int32_t is_signed, int32_t kind,
                              int32_t alternate, int32_t fill, int32_t align, int64_t width,
                              int32_t zero) {
    char text[80];
    uint64_t mask = bits >= 64 ? UINT64_MAX : ((UINT64_C(1) << bits) - 1);
    uint64_t u = (uint64_t)value & mask;
    const char *prefix = "";
    if (alternate) {
        prefix = kind == 3 ? "0b" : (kind == 4 ? "0o" : "0x");
    }
    switch (kind) {
    case 0:
        if (is_signed) {
            snprintf(text, sizeof text, "%" PRId64, value);
        } else {
            snprintf(text, sizeof text, "%" PRIu64, u);
        }
        break;
    case 1:
        snprintf(text, sizeof text, "%s%" PRIx64, prefix, u);
        break;
    case 2:
        snprintf(text, sizeof text, "%s%" PRIX64, prefix, u);
        break;
    case 4:
        snprintf(text, sizeof text, "%s%" PRIo64, prefix, u);
        break;
    default: {
        /* Binary: the digits from the highest 1 (or a single 0). */
        size_t at = strlen(prefix);
        memcpy(text, prefix, at);
        int top = 0;
        for (int b = 63; b >= 0; b--) {
            if ((u >> b) & 1u) {
                top = b;
                break;
            }
        }
        for (int b = top; b >= 0; b--) {
            text[at++] = (char)('0' + ((u >> b) & 1u));
        }
        text[at] = '\0';
        break;
    }
    }
    return pad_number(text, fill, align, width, zero);
}

/* A float with `precision` decimals (`%.*f`), or its shortest text when
 * `precision` is negative. The text is sized by a first `snprintf`. */
LughaString *lugha_rt_fmt_float(double value, int32_t is_f32, int32_t precision, int32_t fill,
                                int32_t align, int64_t width, int32_t zero) {
    if (precision < 0) {
        char text[FORMAT_CAP];
        format_float(value, is_f32 != 0, text, sizeof text);
        return pad_number(text, fill, align, width, zero);
    }
    int needed = snprintf(NULL, 0, "%.*f", precision, value);
    if (needed < 0) {
        needed = 0;
    }
    char *text = lugha_rt_alloc((int64_t)needed + 1);
    snprintf(text, (size_t)needed + 1, "%.*f", precision, value);
    return pad_number(text, fill, align, width, zero);
}

/* Any other value's text, padded (`{p:>20}`). */
LughaString *lugha_rt_pad(const LughaString *s, int32_t fill, int32_t align, int64_t width) {
    return pad_bytes(s->bytes, s->len, fill, align, width);
}
