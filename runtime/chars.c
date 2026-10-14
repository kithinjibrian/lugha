
/* Characters (v14 draft §5, milestone 46): a `char` is a Unicode scalar
 * value, written as UTF-8 when printed or turned into text. */

/* `code` as UTF-8 into `out` (room for 4 bytes); the byte count. */
static int utf8_encode(uint32_t code, char *out) {
    if (code < 0x80) {
        out[0] = (char)code;
        return 1;
    }
    if (code < 0x800) {
        out[0] = (char)(0xC0 | (code >> 6));
        out[1] = (char)(0x80 | (code & 0x3F));
        return 2;
    }
    if (code < 0x10000) {
        out[0] = (char)(0xE0 | (code >> 12));
        out[1] = (char)(0x80 | ((code >> 6) & 0x3F));
        out[2] = (char)(0x80 | (code & 0x3F));
        return 3;
    }
    out[0] = (char)(0xF0 | (code >> 18));
    out[1] = (char)(0x80 | ((code >> 12) & 0x3F));
    out[2] = (char)(0x80 | ((code >> 6) & 0x3F));
    out[3] = (char)(0x80 | (code & 0x3F));
    return 4;
}

void lugha_rt_print_char(uint32_t code) {
    char text[4];
    fwrite(text, 1, (size_t)utf8_encode(code, text), stdout);
}

LughaString *lugha_rt_to_string_char(uint32_t code) {
    char text[4];
    return string_of(text, utf8_encode(code, text));
}

/* `'a'`, as a `char` appears inside a printed struct, enum or array:
 * quotes, with `'`, `\` and control characters escaped. */
LughaString *lugha_rt_char_quote(uint32_t code) {
    char text[16];
    int len;
    switch (code) {
    case '\'': len = snprintf(text, sizeof text, "'\\''"); break;
    case '\\': len = snprintf(text, sizeof text, "'\\\\'"); break;
    case '\n': len = snprintf(text, sizeof text, "'\\n'"); break;
    case '\t': len = snprintf(text, sizeof text, "'\\t'"); break;
    case '\r': len = snprintf(text, sizeof text, "'\\r'"); break;
    case 0: len = snprintf(text, sizeof text, "'\\0'"); break;
    default:
        if (code < 0x20) {
            len = snprintf(text, sizeof text, "'\\u{%02x}'", code);
        } else {
            text[0] = '\'';
            len = 1 + utf8_encode(code, text + 1);
            text[len++] = '\'';
        }
    }
    return string_of(text, len);
}
