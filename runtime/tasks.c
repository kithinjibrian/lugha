/*
 * tasks.c — part of lugha_rt.c: green threads, channels and `select` (v17,
 * milestone 91).
 *
 * Every green thread runs on the program's one OS thread (M:1), so nothing
 * here locks: a switch happens only inside these functions, when a thread
 * waits (`join`, a channel, `select`, `sleep`), yields, or is preempted by
 * the countdown generated code keeps in `lugha_rt_task_budget`.
 *
 * Each green thread is a `Fiber`: its own `mmap`'d stack (1 MiB, guard pages
 * below) and the stack pointer saved while it is switched out. `main` is a
 * fiber too, on the program's own stack. Fibers are allocated uncollectable,
 * so the collector scans them as roots, and `push_fiber_stacks` hands it
 * every switched-out stack; the running one is the collector's "the stack",
 * moved with `GC_set_stackbottom` on each switch.
 *
 * A `Task<T>` value is a `LughaTask`, a GC object holding the result; a
 * `Chan<T>` is a `LughaChan`. Values cross channels as `elem` bytes in
 * element layout, already copied by generated code.
 *
 * A fragment, not compiled on its own: lughac's link.rs joins it after the
 * core (lugha_rt.c), stack.c (`green_guard_low`/`high`) and random.c.
 */

/* Reserved per green thread, and the guard below it (spec §7, v17). */
#define FIBER_STACK (1024 * 1024)
#define FIBER_GUARD GUARD_WINDOW
/* Steps between preemption checks in generated code (v17 draft decision 10). */
#define TASK_BUDGET 4096
/* Stacks of finished threads kept for reuse. */
#define SPARE_STACKS 16

typedef struct Fiber Fiber;

/* A `Task<T>` (GC object): what `join` reads. */
typedef struct {
    void *result;
    int32_t done;
    /* Fibers waiting in `join`, linked through `wait_next`. */
    Fiber *joiners;
} LughaTask;

struct Fiber {
    /* Saved stack pointer while switched out. */
    void *sp;
    /* The mapping (guard pages first), or NULL for `main`. */
    char *map;
    /* The stack's high end. */
    char *top;
    /* What to run: `entry(code, env, result)`, generated per spawn site. */
    void (*entry)(void *, void *, void *);
    void *code;
    void *env;
    LughaTask *task;
    /* Run queue, and the list of every live fiber. */
    Fiber *next;
    Fiber *prev_live;
    Fiber *next_live;
    /* In a `join` wait list. */
    Fiber *wait_next;
    /* Sleepers, by deadline; `wake_at` < 0 when not sleeping. */
    Fiber *sleep_next;
    int64_t wake_at;
    /* Which wait woke it: a `select` case, or TIMED_OUT; WAITING before. */
    int64_t fired;
    /* The actor this fiber runs (v17, actors.c), or NULL. */
    void *actor;
};

enum { WAITING = -1, TIMED_OUT = -2, DEFAULT_ARM = -1 };

int64_t lugha_rt_task_budget = TASK_BUDGET;

static Fiber *current;
static Fiber main_fiber;
static Fiber *ready_head;
static Fiber *ready_tail;
static Fiber *sleepers;
static Fiber *live;
static int64_t live_count;
/* A fiber that has finished, reaped by the next one to run. */
static Fiber *zombie;
static char *spare[SPARE_STACKS];
static int spare_count;
static GC_push_other_roots_proc next_push;

void lugha_rt_task_swap(void **save, void *load);
void lugha_rt_task_boot(void);

#if defined(__x86_64__)
/* Saves the callee-saved registers, MXCSR and the x87 control word on the
 * old stack, switches stacks, and restores them from the new one (x86-64
 * System V). */
__asm__(".text\n"
        ".globl lugha_rt_task_swap\n"
        ".type lugha_rt_task_swap, @function\n"
        "lugha_rt_task_swap:\n"
        "    pushq %rbp\n"
        "    pushq %rbx\n"
        "    pushq %r12\n"
        "    pushq %r13\n"
        "    pushq %r14\n"
        "    pushq %r15\n"
        "    subq $8, %rsp\n"
        "    stmxcsr (%rsp)\n"
        "    fnstcw 4(%rsp)\n"
        "    movq %rsp, (%rdi)\n"
        "    movq %rsi, %rsp\n"
        "    ldmxcsr (%rsp)\n"
        "    fldcw 4(%rsp)\n"
        "    addq $8, %rsp\n"
        "    popq %r15\n"
        "    popq %r14\n"
        "    popq %r13\n"
        "    popq %r12\n"
        "    popq %rbx\n"
        "    popq %rbp\n"
        "    ret\n"
        ".size lugha_rt_task_swap, .-lugha_rt_task_swap\n"
        /* A new fiber's first `ret` lands here, its fiber in %r12. */
        ".globl lugha_rt_task_boot\n"
        ".type lugha_rt_task_boot, @function\n"
        "lugha_rt_task_boot:\n"
        "    movq %r12, %rdi\n"
        "    call lugha_rt_task_fiber_main\n"
        "    ud2\n"
        ".size lugha_rt_task_boot, .-lugha_rt_task_boot\n");
#elif defined(__aarch64__)
/* The same for AArch64 (v17, milestone 133): AAPCS64's callee-saved
 * registers — x19–x28, the frame pointer x29, the link register x30 and
 * d8–d15 (the low halves of v8–v15) — and FPCR, the rounding mode, in a
 * 176-byte frame, so `sp` stays 16-byte aligned as AAPCS64 requires. */
__asm__(".text\n"
        ".globl lugha_rt_task_swap\n"
        ".type lugha_rt_task_swap, %function\n"
        "lugha_rt_task_swap:\n"
        "    sub sp, sp, #176\n"
        "    stp x19, x20, [sp, #0]\n"
        "    stp x21, x22, [sp, #16]\n"
        "    stp x23, x24, [sp, #32]\n"
        "    stp x25, x26, [sp, #48]\n"
        "    stp x27, x28, [sp, #64]\n"
        "    stp x29, x30, [sp, #80]\n"
        "    stp d8, d9, [sp, #96]\n"
        "    stp d10, d11, [sp, #112]\n"
        "    stp d12, d13, [sp, #128]\n"
        "    stp d14, d15, [sp, #144]\n"
        "    mrs x9, fpcr\n"
        "    str x9, [sp, #160]\n"
        "    mov x9, sp\n"
        "    str x9, [x0]\n"
        "    mov sp, x1\n"
        "    ldr x9, [sp, #160]\n"
        "    msr fpcr, x9\n"
        "    ldp x19, x20, [sp, #0]\n"
        "    ldp x21, x22, [sp, #16]\n"
        "    ldp x23, x24, [sp, #32]\n"
        "    ldp x25, x26, [sp, #48]\n"
        "    ldp x27, x28, [sp, #64]\n"
        "    ldp x29, x30, [sp, #80]\n"
        "    ldp d8, d9, [sp, #96]\n"
        "    ldp d10, d11, [sp, #112]\n"
        "    ldp d12, d13, [sp, #128]\n"
        "    ldp d14, d15, [sp, #144]\n"
        "    add sp, sp, #176\n"
        "    ret\n"
        ".size lugha_rt_task_swap, .-lugha_rt_task_swap\n"
        /* A new fiber's first `ret` lands here, its fiber in x19. */
        ".globl lugha_rt_task_boot\n"
        ".type lugha_rt_task_boot, %function\n"
        "lugha_rt_task_boot:\n"
        "    mov x0, x19\n"
        "    bl lugha_rt_task_fiber_main\n"
        "    brk #0\n"
        ".size lugha_rt_task_boot, .-lugha_rt_task_boot\n");
#else
#error "green threads need a stack switch for this architecture (x86-64 and AArch64 have one)"
#endif

static int64_t now_ns(void) {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return (int64_t)t.tv_sec * 1000000000 + t.tv_nsec;
}

/* Hands the collector every switched-out stack: from its saved stack
 * pointer, where its registers are, to its top. */
static void GC_CALLBACK push_fiber_stacks(void) {
    for (Fiber *f = live; f != NULL; f = f->next_live) {
        if (f != current && f->sp != NULL) {
            GC_push_all_eager(f->sp, f->top);
        }
    }
    if (next_push != NULL) {
        next_push();
    }
}

/* `main` becomes a fiber the first time anything here runs. */
static void ensure_started(void) {
    if (current != NULL) {
        return;
    }
    struct GC_stack_base base;
    GC_get_my_stackbottom(&base);
    main_fiber.top = base.mem_base;
    main_fiber.wake_at = -1;
    main_fiber.fired = 0;
    current = &main_fiber;
    live = &main_fiber;
    live_count = 1;
    next_push = GC_get_push_other_roots();
    GC_set_push_other_roots(push_fiber_stacks);
}

static void make_ready(Fiber *f) {
    f->next = NULL;
    if (ready_tail == NULL) {
        ready_head = f;
    } else {
        ready_tail->next = f;
    }
    ready_tail = f;
}

static Fiber *pop_ready(void) {
    Fiber *f = ready_head;
    if (f != NULL) {
        ready_head = f->next;
        if (ready_head == NULL) {
            ready_tail = NULL;
        }
    }
    return f;
}

static void unsleep(Fiber *f) {
    if (f->wake_at < 0) {
        return;
    }
    for (Fiber **at = &sleepers; *at != NULL; at = &(*at)->sleep_next) {
        if (*at == f) {
            *at = f->sleep_next;
            break;
        }
    }
    f->wake_at = -1;
}

static void add_sleeper(Fiber *f, int64_t deadline) {
    f->wake_at = deadline;
    Fiber **at = &sleepers;
    while (*at != NULL && (*at)->wake_at <= deadline) {
        at = &(*at)->sleep_next;
    }
    f->sleep_next = *at;
    *at = f;
}

/* Wakes a waiting fiber for `why` (a case index, or TIMED_OUT). */
static void wake(Fiber *f, int64_t why) {
    f->fired = why;
    unsleep(f);
    make_ready(f);
}

static void wake_due_sleepers(void) {
    if (sleepers == NULL) {
        return;
    }
    int64_t now = now_ns();
    while (sleepers != NULL && sleepers->wake_at <= now) {
        Fiber *f = sleepers;
        sleepers = f->sleep_next;
        f->wake_at = -1;
        f->fired = TIMED_OUT;
        make_ready(f);
    }
}

/* Gives a finished fiber's stack back once another one is running on its own. */
static void reap(void) {
    Fiber *dead = zombie;
    if (dead == NULL) {
        return;
    }
    zombie = NULL;
    if (spare_count < SPARE_STACKS) {
        spare[spare_count++] = dead->map;
    } else {
        munmap(dead->map, FIBER_GUARD + FIBER_STACK);
    }
    GC_FREE(dead);
}

static void *set_bottom(void *top) {
    struct GC_stack_base base = {.mem_base = top};
    GC_set_stackbottom(NULL, &base);
    return NULL;
}

static void switch_to(Fiber *next) {
    Fiber *prev = current;
    if (next == prev) {
        return;
    }
    current = next;
    GC_call_with_alloc_lock(set_bottom, next->top);
    green_guard_low = next->map == NULL ? 0 : (uintptr_t)next->map;
    green_guard_high = next->map == NULL ? 0 : (uintptr_t)next->map + FIBER_GUARD;
    lugha_rt_task_swap(&prev->sp, next->sp);
    /* Back on `prev`'s stack: whoever ran before may have finished. */
    reap();
}

/* Nothing is ready: waits for a socket or the next sleeper; wakes threads
 * whose sockets are ready without waiting (net.c, v17). */
static void idle_wait(void);
static void poll_ready_sockets(void);

static void deadlock(void) {
    die("panic: deadlock: every green thread is waiting\n");
}

/* Runs the next ready fiber; the current one has already queued itself, or
 * is waiting. Sleeps the OS thread when only sleepers remain. */
static void schedule(void) {
    for (;;) {
        wake_due_sleepers();
        Fiber *next = pop_ready();
        if (next != NULL) {
            switch_to(next);
            return;
        }
        idle_wait();
    }
}

/* Blocks the current fiber until something wakes it; what did. */
static int64_t block(void) {
    current->fired = WAITING;
    schedule();
    return current->fired;
}
