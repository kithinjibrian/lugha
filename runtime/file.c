/*
 * file.c — part of lugha_rt.c: `std.fs`'s file streams (v17, milestone 146).
 * A `File` is a descriptor; `std.fs` keeps its mode, its read buffer and
 * whether it is closed, so these functions only open, read, write and close.
 * They block the OS thread while they run (disk files are quick; a FIFO
 * stalls the other green threads, as `std.fs` documents).
 *
 * A fragment, not compiled on its own: lughac's link.rs joins it after the
 * core (lugha_rt.c), whose includes (`<fcntl.h>`, `<unistd.h>`, `<errno.h>`,
 * `<string.h>`) it relies on, and after fs.c (`fs_fail`, `fs_has_nul`) and
 * bytes.c (`bytes_of`).
 */

/* The most one `read` asks for, as for sockets: 16 MiB. */
#define FILE_MAX_READ 16777216

/* `$os.file_open(path, mode)`: 1 and the descriptor, or 0 and the system's
 * message. Mode 0 reads, 1 creates or truncates, 2 appends (creating); every
 * descriptor is close-on-exec, and created files get 0666 minus the umask,
 * as `write_file`'s do. */
int32_t lugha_rt_file_open(const LughaString *path, int64_t mode, int64_t *out,
                           LughaString **error) {
    if (fs_has_nul(path)) {
        return fs_fail(error, "path contains a NUL byte");
    }
    int flags = O_CLOEXEC;
    if (mode == 0) {
        flags |= O_RDONLY;
    } else if (mode == 1) {
        flags |= O_WRONLY | O_CREAT | O_TRUNC;
    } else {
        flags |= O_WRONLY | O_CREAT | O_APPEND;
    }
    int fd;
    do {
        fd = open(path->bytes, flags, 0666);
    } while (fd < 0 && errno == EINTR);
    if (fd < 0) {
        return fs_fail(error, strerror(errno));
    }
    *out = fd;
    return 1;
}

/* `$os.file_read(fd, max)`: 1 and up to `max` bytes (none at the end of the
 * file), or 0 and a message. An interrupted read is retried. */
int32_t lugha_rt_file_read(int64_t fd, int64_t max, LughaArray **out, LughaString **error) {
    if (max < 1 || max > FILE_MAX_READ) {
        return fs_fail(error, "max must be from 1 to 16777216");
    }
    char *buffer = lugha_rt_alloc(max);
    for (;;) {
        ssize_t n = read((int)fd, buffer, (size_t)max);
        if (n >= 0) {
            *out = bytes_of(buffer, (int64_t)n);
            return 1;
        }
        if (errno != EINTR) {
            return fs_fail(error, strerror(errno));
        }
    }
}

/* `$os.file_write(fd, bytes)`: 1 once every byte is written, or 0 and the
 * system's message; short and interrupted writes go on where they stopped. */
int32_t lugha_rt_file_write(int64_t fd, const LughaArray *bytes, LughaString **error) {
    int64_t done = 0;
    while (done < bytes->len) {
        ssize_t n = write((int)fd, bytes->elems + done, (size_t)(bytes->len - done));
        if (n >= 0) {
            done += n;
        } else if (errno != EINTR) {
            return fs_fail(error, strerror(errno));
        }
    }
    return 1;
}

/* `$os.file_close(fd)`. A failing `close` is ignored: the descriptor is gone
 * either way (Linux frees it even on EINTR), and nothing is buffered here
 * that could be lost. */
void lugha_rt_file_close(int64_t fd) {
    close((int)fd);
}
