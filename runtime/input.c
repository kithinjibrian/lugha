
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

/* `parse_i64(s)`: an optional `-`, then one or more decimal digits and
 * nothing else; 1 with `*out` set, or 0 (spec: `None`) on anything else,
 * including overflow. Accumulates negatively so `INT64_MIN` fits. */
int32_t lugha_rt_parse_i64(const LughaString *s, int64_t *out) {
    int64_t i = 0;
    bool negative = s->len > 0 && s->bytes[0] == '-';
    if (negative) {
        i = 1;
    }
    if (i == s->len) {
        return 0;
    }
    int64_t value = 0;
    for (; i < s->len; i++) {
        char c = s->bytes[i];
        if (c < '0' || c > '9') {
            return 0;
        }
        int64_t digit = c - '0';
        if (value < (INT64_MIN + digit) / 10) {
            return 0;
        }
        value = value * 10 - digit;
    }
    if (!negative) {
        if (value == INT64_MIN) {
            return 0;
        }
        value = -value;
    }
    *out = value;
    return 1;
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
