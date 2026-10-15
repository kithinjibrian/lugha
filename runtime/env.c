/*
 * env.c — part of lugha_rt.c: `std.env`'s built-ins (v15, milestone 64).
 * Values are returned as bytes; `std.env` checks them as UTF-8 in Lugha.
 * Nothing here changes the environment or the working directory.
 *
 * A fragment, not compiled on its own: lughac's link.rs joins it after the
 * core (lugha_rt.c), whose includes (`<stdlib.h>`, `<unistd.h>`,
 * `<errno.h>`, `<string.h>`) it relies on, and after bytes.c (`bytes_of`)
 * and input.c (`string_of`).
 */

/* `$os.env(name)`: 1 and the variable's bytes, or 0 when it is unset or
 * `name` holds a NUL byte (which would cut the name short). The value is
 * copied at once, so later changes to the environment can't affect it. */
int32_t lugha_rt_env(const LughaString *name, LughaArray **out) {
    if (memchr(name->bytes, 0, (size_t)name->len) != NULL) {
        return 0;
    }
    const char *value = getenv(name->bytes);
    if (value == NULL) {
        return 0;
    }
    *out = bytes_of(value, (int64_t)strlen(value));
    return 1;
}

/* `$os.cwd()`: 1 and the current directory's bytes, or 0 and the system's
 * message. The buffer doubles until the path fits. */
int32_t lugha_rt_cwd(LughaArray **out, LughaString **error) {
    size_t cap = 256;
    for (;;) {
        char *buffer = lugha_rt_alloc((int64_t)cap);
        if (getcwd(buffer, cap) != NULL) {
            *out = bytes_of(buffer, (int64_t)strlen(buffer));
            return 1;
        }
        if (errno != ERANGE || cap > ((size_t)1 << 30)) {
            const char *message = strerror(errno);
            *error = string_of(message, (int64_t)strlen(message));
            return 0;
        }
        cap *= 2;
    }
}
