//! The green-thread runtime (v17, milestone 91) through a C harness built
//! against `lugha_rt.c`: scheduling order, what a switch keeps, sleeping,
//! channels, `select`, the collector scanning suspended stacks, preemption,
//! and the panics (deadlock, closed channels, stack overflow).

use std::path::PathBuf;
use std::process::Command;
use std::sync::OnceLock;

/// The harness: each case is one argument (`harness join`).
const HARNESS: &str = r#"#include <fenv.h>
#include <gc.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void lugha_rt_init(int32_t argc, char **argv);
void *lugha_rt_alloc(int64_t size);
void *lugha_rt_task_spawn(void (*entry)(void *, void *, void *), void *code, void *env, int64_t size);
void *lugha_rt_task_join(void *task);
void lugha_rt_task_yield(void);
void lugha_rt_task_sleep(int64_t ms);
void lugha_rt_task_preempt(void);
extern int64_t lugha_rt_task_budget;
void *lugha_rt_chan_new(int64_t cap, int64_t elem, const char *file, int64_t line, int64_t col);
void lugha_rt_chan_send(void *c, const void *value, const char *file, int64_t line, int64_t col);
int32_t lugha_rt_chan_recv(void *c, void *out);
int32_t lugha_rt_chan_try_recv(void *c, void *out);
void lugha_rt_chan_close(void *c, const char *file, int64_t line, int64_t col);
typedef struct { void *chan; int64_t kind; void *data; int32_t ok; } Case;
int64_t lugha_rt_task_select(Case *cases, int64_t n, int64_t timeout_ms, int32_t has_default,
                             const char *file, int64_t line, int64_t col);

typedef int64_t (*Fn)(void *env);
static void entry(void *code, void *env, void *result) {
    *(int64_t *)result = ((Fn)code)(env);
}
static void *spawn(Fn f, void *env) { return lugha_rt_task_spawn(entry, (void *)f, env, 8); }
static int64_t join(void *t) { return *(int64_t *)lugha_rt_task_join(t); }

static int64_t square(void *env) { int64_t n = (int64_t)(intptr_t)env; return n * n; }
static int64_t say(void *env) {
    const char *name = env;
    for (int i = 0; i < 3; i++) { printf("%s%d\n", name, i); lugha_rt_task_yield(); }
    return 0;
}
static int64_t rounding(void *env) {
    (void)env;
    fesetround(FE_UPWARD);
    lugha_rt_task_yield();
    return fegetround() == FE_UPWARD;
}
static int64_t sleeper(void *env) {
    int64_t ms = (int64_t)(intptr_t)env;
    lugha_rt_task_sleep(ms);
    printf("slept %ld\n", (long)ms);
    return 0;
}
static int64_t producer(void *env) {
    for (int64_t i = 0; i < 5; i++) lugha_rt_chan_send(env, &i, "p", 1, 1);
    lugha_rt_chan_close(env, "p", 2, 1);
    return 0;
}
static int64_t ponger(void *env) {
    void **chans = env;
    int64_t v;
    while (lugha_rt_chan_recv(chans[0], &v)) { v += 1; lugha_rt_chan_send(chans[1], &v, "x", 1, 1); }
    return 0;
}
static int64_t recurse(int64_t n) { volatile char pad[256]; pad[0] = (char)n; return n == 0 ? pad[0] : recurse(n - 1) + pad[0]; }
static int64_t deep(void *env) { (void)env; return recurse(100000000); }
static int64_t keeper(void *env) {
    (void)env;
    int64_t **cell = lugha_rt_alloc(sizeof(int64_t *) * 64);
    for (int i = 0; i < 64; i++) { cell[i] = lugha_rt_alloc(1024); *cell[i] = i; }
    lugha_rt_task_yield();
    int64_t sum = 0;
    for (int i = 0; i < 64; i++) sum += *cell[i];
    return sum;
}
static int64_t churn(void *env) {
    (void)env;
    for (int i = 0; i < 2000; i++) memset(lugha_rt_alloc(4096), 0xff, 4096);
    GC_gcollect();
    return 0;
}
static int64_t spin(void *env) {
    volatile int64_t *flag = env;
    while (!*flag) { if (--lugha_rt_task_budget <= 0) lugha_rt_task_preempt(); }
    return 1;
}
static int64_t setter(void *env) { *(volatile int64_t *)env = 1; return 0; }
static int64_t counter(void *env) { (void)env; return 1; }

int main(int argc, char **argv) {
    lugha_rt_init(argc, argv);
    const char *test = argv[1];
    if (!strcmp(test, "join")) {
        void *a = spawn(square, (void *)3), *b = spawn(square, (void *)4);
        printf("%ld\n", (long)(join(a) + join(b)));
        printf("%ld\n", (long)join(a));
    } else if (!strcmp(test, "fifo")) {
        void *a = spawn(say, "a"), *b = spawn(say, "b");
        join(a); join(b);
    } else if (!strcmp(test, "fpu")) {
        void *t = spawn(rounding, NULL);
        lugha_rt_task_yield();
        printf("main %d\n", fegetround() == FE_TONEAREST);
        printf("task %ld\n", (long)join(t));
    } else if (!strcmp(test, "sleep")) {
        void *a = spawn(sleeper, (void *)30), *b = spawn(sleeper, (void *)10), *c = spawn(sleeper, (void *)20);
        join(a); join(b); join(c);
    } else if (!strcmp(test, "buffered")) {
        void *c = lugha_rt_chan_new(2, 8, "m", 1, 1);
        spawn(producer, c);
        int64_t v;
        while (lugha_rt_chan_recv(c, &v)) printf("%ld\n", (long)v);
        printf("closed\n");
    } else if (!strcmp(test, "unbuffered")) {
        void *chans[2] = {lugha_rt_chan_new(0, 8, "m", 1, 1), lugha_rt_chan_new(0, 8, "m", 1, 1)};
        void *t = spawn(ponger, chans);
        int64_t v = 0;
        for (int i = 0; i < 3; i++) { lugha_rt_chan_send(chans[0], &v, "m", 1, 1); lugha_rt_chan_recv(chans[1], &v); }
        lugha_rt_chan_close(chans[0], "m", 1, 1);
        join(t);
        printf("%ld\n", (long)v);
    } else if (!strcmp(test, "select")) {
        void *a = lugha_rt_chan_new(0, 8, "m", 1, 1), *b = lugha_rt_chan_new(1, 8, "m", 1, 1);
        int64_t x = 0, y = 7;
        Case cases[2] = {{a, 0, &x, 0}, {b, 1, &y, 0}};
        printf("%ld\n", (long)lugha_rt_task_select(cases, 2, -1, 0, "m", 1, 1));
        printf("%ld\n", (long)lugha_rt_task_select(cases, 1, 20, 0, "m", 1, 1));
        printf("%ld\n", (long)lugha_rt_task_select(cases, 1, -1, 1, "m", 1, 1));
        spawn(producer, a);
        printf("%ld %ld\n", (long)lugha_rt_task_select(cases, 1, -1, 0, "m", 1, 1), (long)x);
        int64_t z;
        printf("try %d\n", lugha_rt_chan_try_recv(b, &z));
        printf("got %ld\n", (long)z);
        printf("try %d\n", lugha_rt_chan_try_recv(b, &z));
    } else if (!strcmp(test, "deadlock")) {
        void *c = lugha_rt_chan_new(0, 8, "m", 1, 1);
        int64_t v;
        printf("before\n");
        lugha_rt_chan_recv(c, &v);
    } else if (!strcmp(test, "closed")) {
        void *c = lugha_rt_chan_new(1, 8, "m", 1, 1);
        lugha_rt_chan_close(c, "m", 1, 1);
        int64_t v = 1;
        lugha_rt_chan_send(c, &v, "file.la", 3, 9);
    } else if (!strcmp(test, "overflow")) {
        join(spawn(deep, NULL));
    } else if (!strcmp(test, "gc")) {
        void *k = spawn(keeper, NULL), *c = spawn(churn, NULL);
        join(c);
        printf("%ld\n", (long)join(k));
    } else if (!strcmp(test, "preempt")) {
        int64_t flag = 0;
        void *s = spawn(spin, &flag);
        spawn(setter, &flag);
        printf("%ld\n", (long)join(s));
    } else if (!strcmp(test, "many")) {
        enum { N = 10000 };
        static void *ts[N];
        for (int i = 0; i < N; i++) ts[i] = spawn(counter, NULL);
        int64_t sum = 0;
        for (int i = 0; i < N; i++) sum += join(ts[i]);
        printf("%ld\n", (long)sum);
    }
    return 0;
}
"#;

/// Builds the runtime and the harness once for every test.
fn harness() -> &'static PathBuf {
    static BUILT: OnceLock<PathBuf> = OnceLock::new();
    BUILT.get_or_init(|| {
        let dir = std::env::temp_dir().join(format!("lugha-tasks-{}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("temp dir is writable");
        std::fs::write(dir.join("lugha_rt.c"), lugha::link::RUNTIME_SOURCE).expect("writable");
        std::fs::write(dir.join("harness.c"), HARNESS).expect("writable");
        let exe = dir.join("harness");
        // No -Werror: `runtime_compiles_without_warnings` (tests/codegen.rs)
        // holds the runtime to that, and GCC's -O2 flow analysis warns on
        // code unrelated to green threads.
        let out = Command::new("cc")
            .args(["-std=c11", "-O2"])
            .arg(dir.join("harness.c"))
            .arg(dir.join("lugha_rt.c"))
            .args(["-lgc", "-lm", "-o"])
            .arg(&exe)
            .output()
            .expect("cc runs");
        assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
        exe
    })
}

/// Runs one case: its stdout, stderr and exit code.
fn run(case: &str) -> (String, String, Option<i32>) {
    let out = Command::new(harness()).arg(case).output().expect("harness runs");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code(),
    )
}

fn prints(case: &str, expected: &str) {
    let (out, err, code) = run(case);
    assert_eq!((out.as_str(), code), (expected, Some(0)), "{case}: {err}");
}

#[test]
fn join_waits_and_gives_the_result_again() {
    prints("join", "25
9
");
}

#[test]
fn ready_threads_run_in_order() {
    prints("fifo", "a0
b0
a1
b1
a2
b2
");
}

#[test]
fn a_switch_keeps_each_threads_rounding_mode() {
    prints("fpu", "main 1
task 1
");
}

#[test]
fn sleepers_wake_by_deadline() {
    prints("sleep", "slept 10
slept 20
slept 30
");
}

#[test]
fn buffered_channels_drain_after_close() {
    prints("buffered", "0
1
2
3
4
closed
");
}

#[test]
fn unbuffered_channels_hand_over() {
    prints("unbuffered", "3
");
}

#[test]
fn select_picks_ready_arms_times_out_and_defaults() {
    prints("select", "1
-2
-1
0 0
try 1
got 7
try 0
");
}

#[test]
fn suspended_stacks_keep_their_objects_alive() {
    prints("gc", "2016
");
}

#[test]
fn the_countdown_preempts_a_busy_thread() {
    prints("preempt", "1
");
}

#[test]
fn ten_thousand_threads() {
    prints("many", "10000
");
}

#[test]
fn a_deadlock_panics() {
    let (out, err, code) = run("deadlock");
    assert_eq!(out, "before
");
    assert_eq!(err, "panic: deadlock: every green thread is waiting
");
    assert_eq!(code, Some(101));
}

#[test]
fn sending_on_a_closed_channel_panics_at_the_send() {
    let (_, err, code) = run("closed");
    assert_eq!(err, "panic: send on a closed channel at file.la:3:9
");
    assert_eq!(code, Some(101));
}

#[test]
fn a_green_threads_stack_overflow_panics() {
    let (_, err, code) = run("overflow");
    assert_eq!(err, "panic: stack overflow
");
    assert_eq!(code, Some(101));
}
