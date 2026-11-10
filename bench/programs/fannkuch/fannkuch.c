#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
    (void)argc;
    long n = atol(argv[1]);
    long *perm1 = calloc(n, sizeof(long)), *perm = calloc(n, sizeof(long)), *count = calloc(n, sizeof(long));
    if (!perm1 || !perm || !count) abort();
    for (long i = 0; i < n; i++) perm1[i] = i;
    long r = n, perm_count = 0, max_flips = 0, checksum = 0;
    for (;;) {
        while (r != 1) { count[r - 1] = r; r--; }
        for (long i = 0; i < n; i++) perm[i] = perm1[i];
        long flips = 0;
        for (long k = perm[0]; k != 0; k = perm[0]) {
            for (long i = 0, j = k; i < j; i++, j--) {
                long t = perm[i]; perm[i] = perm[j]; perm[j] = t;
            }
            flips++;
        }
        if (flips > max_flips) max_flips = flips;
        checksum += perm_count % 2 == 0 ? flips : -flips;
        for (;;) {
            if (r == n) {
                printf("%ld %ld\n", checksum, max_flips);
                return 0;
            }
            long first = perm1[0];
            for (long i = 0; i < r; i++) perm1[i] = perm1[i + 1];
            perm1[r] = first;
            if (--count[r] > 0) break;
            r++;
        }
        perm_count++;
    }
}
