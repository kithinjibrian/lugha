#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
    (void)argc;
    long n = atol(argv[1]);
    double size = (double)n;
    long inside = 0;
    for (long y = 0; y < n; y++) {
        double ci = 2.0 * (double)y / size - 1.0;
        for (long x = 0; x < n; x++) {
            double cr = 2.0 * (double)x / size - 1.5;
            double zr = 0.0, zi = 0.0, tr = 0.0, ti = 0.0;
            for (int i = 0; i < 50 && tr + ti <= 4.0; i++) {
                zi = 2.0 * zr * zi + ci;
                zr = tr - ti + cr;
                tr = zr * zr;
                ti = zi * zi;
            }
            if (tr + ti <= 4.0) inside++;
        }
    }
    printf("%ld\n", inside);
    return 0;
}
