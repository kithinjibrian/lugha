
/*
 * callbacks.c — closures handed to C (v16, milestone 88). A closure passed
 * where C takes user data is copied into memory Boehm scans but never
 * collects, so it and its captures live while C may call it: until the
 * program exits, or until C calls lugha_rt_cb_release.
 */

/* A copy of the function value { code, env } that C can hold. */
void *lugha_rt_cb_pin(void *code, void *env) {
    void **pinned = GC_MALLOC_UNCOLLECTABLE(2 * sizeof(void *));
    if (pinned == NULL) {
        die("panic: out of memory\n");
    }
    pinned[0] = code;
    pinned[1] = env;
    return pinned;
}

/* C dropped the callback: its closure may be collected again. */
void lugha_rt_cb_release(void *pinned) {
    GC_FREE(pinned);
}
