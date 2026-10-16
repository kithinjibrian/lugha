
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
