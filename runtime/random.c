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
