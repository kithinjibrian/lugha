/*
 * spawn.c — part of lugha_rt.c: starting, joining and pausing green threads
 * (v17, milestone 91): `spawn`, `join`, `task.yield_now()`, `task.sleep(ms)`
 * and the preemption check generated code calls.
 *
 * A fragment, not compiled on its own: lughac's link.rs joins it right after
 * tasks.c, whose fibers and scheduler it uses.
 */

static char *new_stack(void) {
    if (spare_count > 0) {
        return spare[--spare_count];
    }
    char *map = mmap(NULL, FIBER_GUARD + FIBER_STACK, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE | MAP_STACK, -1, 0);
    if (map == MAP_FAILED) {
        die("panic: out of memory\n");
    }
    if (mprotect(map, FIBER_GUARD, PROT_NONE) != 0) {
        die("panic: out of memory\n");
    }
    return map;
}

/* Where a new fiber starts: runs its call, stores the result, wakes its
 * joiners, and never comes back. */
static void fiber_exit(Fiber *self);

/* The fiber `lugha_rt_task_spawn` made last, for actors.c to mark. */
static Fiber *last_spawned;

__attribute__((used, visibility("hidden"))) void lugha_rt_task_fiber_main(Fiber *self) {
    reap();
    self->entry(self->code, self->env, self->task->result);
    fiber_exit(self);
}

/* Finishes the running fiber: its task is done, its joiners wake, and the
 * next fiber runs; never returns. An actor's panic ends its fiber here too. */
static void fiber_exit(Fiber *self) {
    LughaTask *task = self->task;
    task->done = 1;
    for (Fiber *j = task->joiners; j != NULL;) {
        Fiber *after = j->wait_next;
        wake(j, 0);
        j = after;
    }
    task->joiners = NULL;
    if (self->prev_live != NULL) {
        self->prev_live->next_live = self->next_live;
    } else {
        live = self->next_live;
    }
    if (self->next_live != NULL) {
        self->next_live->prev_live = self->prev_live;
    }
    live_count--;
    zombie = self;
    schedule();
    die("panic: internal error: a finished green thread ran again\n");
}

/* `spawn` (v17): a new green thread running `entry(code, env, result)`,
 * queued behind the ready ones; the caller keeps running. */
LughaTask *lugha_rt_task_spawn(void (*entry)(void *, void *, void *), void *code, void *env,
                               int64_t result_size) {
    ensure_started();
    LughaTask *task = lugha_rt_alloc((int64_t)sizeof(LughaTask));
    task->result = lugha_rt_alloc(result_size < 1 ? 1 : result_size);
    Fiber *f = GC_MALLOC_UNCOLLECTABLE(sizeof(Fiber));
    if (f == NULL) {
        die("panic: out of memory\n");
    }
    memset(f, 0, sizeof *f);
    f->map = new_stack();
    f->top = f->map + FIBER_GUARD + FIBER_STACK;
    f->entry = entry;
    f->code = code;
    f->env = env;
    f->task = task;
    f->wake_at = -1;
#if defined(__x86_64__)
    /* The first switch pops these: MXCSR and x87 defaults, r15…rbp, then
     * returns into lugha_rt_task_boot with %rsp 16-aligned for its call. */
    uint64_t *frame = (uint64_t *)(f->top - 16) - 8;
    frame[0] = 0x1f80 | ((uint64_t)0x037f << 32);
    frame[1] = 0;                    /* r15 */
    frame[2] = 0;                    /* r14 */
    frame[3] = 0;                    /* r13 */
    frame[4] = (uint64_t)(uintptr_t)f; /* r12 */
    frame[5] = 0;                    /* rbx */
    frame[6] = 0;                    /* rbp */
    frame[7] = (uint64_t)(uintptr_t)lugha_rt_task_boot;
#else
    /* AArch64 (milestone 133): the switch's 176-byte frame, all zero (FPCR 0
     * is round-to-nearest, no traps) but x19 = the fiber and x30 = boot, the
     * address its `ret` takes; `sp` ends at `top`, 16-byte aligned. */
    uint64_t *frame = (uint64_t *)(f->top - 176);
    memset(frame, 0, 176);
    frame[0] = (uint64_t)(uintptr_t)f;                   /* x19 */
    frame[11] = (uint64_t)(uintptr_t)lugha_rt_task_boot; /* x30 */
#endif
    f->sp = frame;
    f->next_live = live;
    live->prev_live = f;
    live = f;
    live_count++;
    make_ready(f);
    last_spawned = f;
    return task;
}

/* `t.join()`: waits until the task's call has finished; its result. */
void *lugha_rt_task_join(LughaTask *task) {
    ensure_started();
    if (!task->done) {
        current->wait_next = task->joiners;
        task->joiners = current;
        block();
    }
    return task->result;
}

/* `task.yield_now()`: lets every ready thread run first. */
void lugha_rt_task_yield(void) {
    ensure_started();
    wake_due_sleepers();
    poll_ready_sockets();
    if (ready_head == NULL) {
        return;
    }
    make_ready(current);
    schedule();
}

/* `task.sleep(ms)`: pauses this thread; others run. */
void lugha_rt_task_sleep(int64_t ms) {
    ensure_started();
    if (ms <= 0) {
        lugha_rt_task_yield();
        return;
    }
    int64_t ns = ms > INT64_MAX / 1000000 ? INT64_MAX / 2 : ms * 1000000;
    add_sleeper(current, now_ns() + ns);
    block();
}

/* Called by generated code when its countdown reaches zero. */
void lugha_rt_task_preempt(void) {
    lugha_rt_task_budget = TASK_BUDGET;
    if (live_count > 1) {
        lugha_rt_task_yield();
    }
}
