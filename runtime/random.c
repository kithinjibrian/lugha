/*
 * random.c — part of lugha_rt.c: a seed from the operating system for
 * `std.random`'s `from_os()` (v15, milestone 54). Only the seed comes from
 * here; the generator itself is Lugha code and is not cryptographically
 * secure.
 *
 * A fragment, not compiled on its own: lughac's link.rs joins it after the
 * core (lugha_rt.c), whose includes (`<sys/random.h>`, `<fcntl.h>`) and
 * `die` it relies on.
 */

/* 64 random bits from the kernel: getrandom(2), else /dev/urandom. A
 * system with neither gives no randomness, which is a panic. */
uint64_t lugha_rt_os_seed(void) {
    uint64_t seed = 0;
    ssize_t got;
    do {
        got = getrandom(&seed, sizeof seed, 0);
    } while (got < 0 && errno == EINTR);
    if (got == (ssize_t)sizeof seed) {
        return seed;
    }
    int fd = open("/dev/urandom", O_RDONLY | O_CLOEXEC);
    if (fd >= 0) {
        ssize_t read_bytes = read(fd, &seed, sizeof seed);
        close(fd);
        if (read_bytes == (ssize_t)sizeof seed) {
            return seed;
        }
    }
    die("panic: no randomness available from the operating system\n");
    return 0;
}

/* `$os.now_ms()` (v17, milestone 111): a monotonic clock in milliseconds,
 * for deadlines; never the time of day. */
int64_t lugha_rt_now_ms(void) {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return (int64_t)t.tv_sec * 1000 + t.tv_nsec / 1000000;
}

/* `$os.wall_ms()` (v17, milestone 115): milliseconds since 1970-01-01 UTC,
 * from the system clock, which can be set (so this can jump). */
int64_t lugha_rt_wall_ms(void) {
    struct timespec t;
    clock_gettime(CLOCK_REALTIME, &t);
    return (int64_t)t.tv_sec * 1000 + t.tv_nsec / 1000000;
}

/* `$os.local_offset(seconds)` (v17, milestone 145): the system zone's
 * offset from UTC, in seconds east, at that moment (`TZ`, else
 * `/etc/localtime`, through `localtime_r`), daylight saving included.
 * A moment `time_t` or the C library can't represent is an error. */
int32_t lugha_rt_local_offset(int64_t seconds, int64_t *out, LughaString **error) {
    time_t t = (time_t)seconds;
    struct tm parts;
    if ((int64_t)t != seconds || localtime_r(&t, &parts) == NULL) {
        const char *message = "the system can't give a local time for that moment";
        *error = string_of(message, (int64_t)strlen(message));
        return 0;
    }
    *out = (int64_t)parts.tm_gmtoff;
    return 1;
}
