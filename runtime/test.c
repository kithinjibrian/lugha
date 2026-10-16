
/*
 * test.c — `lughac test` (v16, milestone 84). A test executable holds every
 * test of one file; its `main` runs only the one named by the LUGHA_TEST
 * environment variable, so each test gets a process of its own.
 */

/* Whether `name` is the test to run: 1 or 0. */
int32_t lugha_rt_test_is(const char *name) {
    const char *wanted = getenv("LUGHA_TEST");
    return wanted != NULL && strcmp(wanted, name) == 0;
}

/* No test has the wanted name: say so, and exit with 2 (lughac's own error). */
int32_t lugha_rt_test_unknown(void) {
    const char *wanted = getenv("LUGHA_TEST");
    fprintf(stderr, "error: no test named `%s`\n", wanted != NULL ? wanted : "");
    return 2;
}

/* `skip(reason)` (v17, milestone 97): end this test as skipped. The runner
 * counts it skipped only with both the reason line and exit code 77 (the
 * automake convention), so a plain `exit(77)` stays a failure. Outside a
 * test it is a panic. */
void lugha_rt_test_skip(const LughaString *reason, const char *file, int64_t line, int64_t col) {
    if (getenv("LUGHA_TEST") == NULL) {
        static const char text[] = "skip outside a test";
        lugha_rt_panic(string_of(text, (int64_t)(sizeof text - 1)), file, line, col);
    }
    fflush(stdout);
    fputs("lugha-skip: ", stderr);
    fwrite(reason->bytes, 1, (size_t)reason->len, stderr);
    fputc('\n', stderr);
    fflush(stderr);
    exit(77);
}
