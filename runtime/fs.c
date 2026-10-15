/*
 * fs.c — part of lugha_rt.c: `std.fs`'s built-ins (v15, milestone 65).
 * Names come back as bytes; `std.fs` checks them as UTF-8 and sorts them
 * in Lugha. Nothing here removes more than one file or one empty folder.
 *
 * A fragment, not compiled on its own: lughac's link.rs joins it after the
 * core (lugha_rt.c), whose includes (`<dirent.h>`, `<sys/stat.h>`,
 * `<errno.h>`, `<string.h>`) it relies on, and after bytes.c (`bytes_of`)
 * and input.c (`string_of`).
 */

/* Fails with `message`: 0, and `*error` set. */
static int32_t fs_fail(LughaString **error, const char *message) {
    *error = string_of(message, (int64_t)strlen(message));
    return 0;
}

/* Whether `path` holds a NUL byte, which would cut it short in C. */
static bool fs_has_nul(const LughaString *path) {
    return memchr(path->bytes, 0, (size_t)path->len) != NULL;
}

/* `$os.list_dir(path)`: 1 and every entry's name as bytes, without `.` and
 * `..`, in the order the system gives them; or 0 and the system's message.
 * The array of names grows by doubling. */
int32_t lugha_rt_list_dir(const LughaString *path, LughaArray **out, LughaString **error) {
    if (fs_has_nul(path)) {
        return fs_fail(error, "path contains a NUL byte");
    }
    DIR *dir = opendir(path->bytes);
    if (dir == NULL) {
        return fs_fail(error, strerror(errno));
    }
    int64_t len = 0;
    int64_t cap = 16;
    LughaArray **names = lugha_rt_alloc(cap * (int64_t)sizeof(LughaArray *));
    for (;;) {
        errno = 0;
        struct dirent *entry = readdir(dir);
        if (entry == NULL) {
            break;
        }
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }
        if (len == cap) {
            if (cap > INT64_MAX / 2 / (int64_t)sizeof(LughaArray *)) {
                die("panic: out of memory\n");
            }
            LughaArray **bigger = lugha_rt_alloc(2 * cap * (int64_t)sizeof(LughaArray *));
            memcpy(bigger, names, (size_t)len * sizeof(LughaArray *));
            names = bigger;
            cap *= 2;
        }
        names[len++] = bytes_of(entry->d_name, (int64_t)strlen(entry->d_name));
    }
    int failed = errno;
    closedir(dir);
    if (failed != 0) {
        return fs_fail(error, strerror(failed));
    }
    LughaArray *array = lugha_rt_alloc((int64_t)sizeof(LughaArray) + len * (int64_t)sizeof(LughaArray *));
    array->len = len;
    array->cap = len;
    memcpy(array->elems, names, (size_t)len * sizeof(LughaArray *));
    *out = array;
    return 1;
}

/* `$os.make_dir(path)`: one folder, mode 0777 less the umask. */
int32_t lugha_rt_make_dir(const LughaString *path, LughaString **error) {
    if (fs_has_nul(path)) {
        return fs_fail(error, "path contains a NUL byte");
    }
    return mkdir(path->bytes, 0777) == 0 ? 1 : fs_fail(error, strerror(errno));
}

/* `$os.remove_file(path)`: `unlink`, which refuses a folder (`EISDIR`). */
int32_t lugha_rt_remove_file(const LughaString *path, LughaString **error) {
    if (fs_has_nul(path)) {
        return fs_fail(error, "path contains a NUL byte");
    }
    return unlink(path->bytes) == 0 ? 1 : fs_fail(error, strerror(errno));
}

/* `$os.remove_dir(path)`: `rmdir`, which refuses a non-empty folder. */
int32_t lugha_rt_remove_dir(const LughaString *path, LughaString **error) {
    if (fs_has_nul(path)) {
        return fs_fail(error, "path contains a NUL byte");
    }
    return rmdir(path->bytes) == 0 ? 1 : fs_fail(error, strerror(errno));
}

/* `$os.stat_kind(path)`: 0 when nothing is there (or `stat` fails), 1 for
 * a regular file, 2 for a folder, 3 for anything else; links followed. */
int64_t lugha_rt_stat_kind(const LughaString *path) {
    struct stat info;
    if (fs_has_nul(path) || stat(path->bytes, &info) != 0) {
        return 0;
    }
    if (S_ISREG(info.st_mode)) {
        return 1;
    }
    return S_ISDIR(info.st_mode) ? 2 : 3;
}
