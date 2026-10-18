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

/* `$os.rename(from, to)` (v17, milestone 115): `rename(2)`, which replaces
 * `to` atomically on one file system and refuses a move across devices. */
int32_t lugha_rt_rename(const LughaString *from, const LughaString *to, LughaString **error) {
    if (fs_has_nul(from) || fs_has_nul(to)) {
        return fs_fail(error, "path contains a NUL byte");
    }
    return rename(from->bytes, to->bytes) == 0 ? 1 : fs_fail(error, strerror(errno));
}

/* `$os.chmod(path, mode)` (v17, milestone 115): `chmod(2)`, links followed;
 * `mode` only the permission, setuid, setgid and sticky bits. */
int32_t lugha_rt_chmod(const LughaString *path, int64_t mode, LughaString **error) {
    if (fs_has_nul(path)) {
        return fs_fail(error, "path contains a NUL byte");
    }
    if (mode < 0 || mode > 07777) {
        return fs_fail(error, "mode must be from 0 to 4095 (0o7777)");
    }
    return chmod(path->bytes, (mode_t)mode) == 0 ? 1 : fs_fail(error, strerror(errno));
}

/* `$os.sync(path)` (v17, milestone 116): `fsync(2)` on a file or folder, so
 * its data (or, for a folder, its entries: a rename) reaches the disk. Opened
 * read-only and closed on every path; `EINTR` retries. */
int32_t lugha_rt_sync(const LughaString *path, LughaString **error) {
    if (fs_has_nul(path)) {
        return fs_fail(error, "path contains a NUL byte");
    }
    int fd = open(path->bytes, O_RDONLY | O_CLOEXEC);
    if (fd < 0) {
        return fs_fail(error, strerror(errno));
    }
    int result;
    do {
        result = fsync(fd);
    } while (result != 0 && errno == EINTR);
    int saved = errno;
    close(fd);
    return result == 0 ? 1 : fs_fail(error, strerror(saved));
}
