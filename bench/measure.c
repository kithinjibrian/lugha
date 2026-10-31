/* measure: runs a benchmark and records its wall time and peak memory
   (PRP-184). Usage: measure <result file> <program> [args...]

   bench/run.py can't measure peak memory itself: Linux keeps the larger of
   the old and new peak RSS across exec, so a child started from Python
   reports Python's own size (~18 MB) when it's smaller. This launcher is
   tiny, so its children report their own peak. It writes "<seconds> <KB>"
   to the result file and exits with the program's exit status (128 plus
   the signal number if it was killed). */

#define _GNU_SOURCE
#include <stdio.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

int main(int argc, char **argv) {
    if (argc < 3) {
        fprintf(stderr, "usage: measure <result file> <program> [args...]\n");
        return 2;
    }
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);
    pid_t pid = fork();
    if (pid < 0) {
        perror("fork");
        return 2;
    }
    if (pid == 0) {
        execvp(argv[2], argv + 2);
        perror(argv[2]);
        _exit(127);
    }
    int status;
    struct rusage usage;
    if (wait4(pid, &status, 0, &usage) < 0) {
        perror("wait4");
        return 2;
    }
    clock_gettime(CLOCK_MONOTONIC, &end);
    double seconds = (double)(end.tv_sec - start.tv_sec) + (double)(end.tv_nsec - start.tv_nsec) / 1e9;
    FILE *out = fopen(argv[1], "w");
    if (!out) {
        perror(argv[1]);
        return 2;
    }
    fprintf(out, "%.6f %ld\n", seconds, usage.ru_maxrss);
    fclose(out);
    if (WIFSIGNALED(status)) return 128 + WTERMSIG(status);
    return WEXITSTATUS(status);
}
