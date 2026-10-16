/*
 * actors.c — part of lugha_rt.c: actors (v17, milestone 92).
 *
 * An actor is a green thread running `actor_loop`: it takes messages from
 * its mailbox, an unbounded queue, one at a time, and runs each message's
 * handler (generated per method) on its state. A call queues a message and
 * waits for its outcome; a cast queues one and goes on; `stop` is a message
 * with no handler. A panic inside an actor (`panic_end`) ends only that
 * actor: its waiting callers panic in turn, `join` reports the message, and
 * its green thread is abandoned — Lugha has no destructors to run.
 *
 * A fragment, not compiled on its own: lughac's link.rs joins it last,
 * after tasks.c and spawn.c, whose fibers and scheduler it uses.
 */

enum { RUNNING, STOPPED, PANICKED };

typedef void (*Handler)(void *state, void *args, void *result);

/* One queued call (GC object). */
typedef struct Message {
    /* NULL for `stop`. */
    Handler handler;
    void *args;
    void *result;
    /* The caller waiting for the outcome, NULL for a cast. */
    Fiber *waiter;
    int32_t *outcome;
    struct Message *next;
} Message;

/* An actor handle's object (GC object). */
typedef struct {
    void *state;
    const char *name;
    int32_t status;
    /* The panic's text once PANICKED. */
    LughaString *panic;
    Message *head;
    Message *tail;
    /* The actor's fiber while it waits for mail. */
    Fiber *idle;
    /* Fibers waiting in `join`, linked through `wait_next`. */
    Fiber *joiners;
    /* The message being handled, so a panic can still answer its caller
     * (a handler may wait, letting other actors run meanwhile). */
    Message *handling;
} LughaActor;

static void actor_panic(const LughaActor *a, const char *what, const char *file, int64_t line,
                        int64_t col) {
    panic_format("actor `%s` %s at %s:%" PRId64 ":%" PRId64, a->name, what, file, line, col);
}

/* Settles every message still queued: their callers learn `outcome`. */
static void settle_mail(LughaActor *a, int32_t outcome) {
    for (Message *m = a->head; m != NULL; m = m->next) {
        if (m->waiter != NULL) {
            *m->outcome = outcome;
            wake(m->waiter, 0);
        }
    }
    a->head = a->tail = NULL;
}

static void wake_joiners(LughaActor *a) {
    for (Fiber *j = a->joiners; j != NULL;) {
        Fiber *after = j->wait_next;
        wake(j, 0);
        j = after;
    }
    a->joiners = NULL;
}

static void actor_loop(void *code, void *env, void *result) {
    (void)code;
    (void)result;
    LughaActor *a = env;
    for (;;) {
        while (a->head == NULL) {
            a->idle = current;
            block();
        }
        Message *m = a->head;
        a->head = m->next;
        if (a->head == NULL) {
            a->tail = NULL;
        }
        if (m->handler == NULL) {
            break;
        }
        a->handling = m;
        m->handler(a->state, m->args, m->result);
        a->handling = NULL;
        if (m->waiter != NULL) {
            *m->outcome = RUNNING;
            wake(m->waiter, 0);
        }
    }
    a->status = STOPPED;
    settle_mail(a, STOPPED);
    wake_joiners(a);
}

static void panic_end(const char *text, size_t len) {
    LughaActor *a = current == NULL ? NULL : current->actor;
    if (a == NULL) {
        exit(101);
    }
    LughaString *message = lugha_rt_alloc((int64_t)sizeof(LughaString) + (int64_t)len + 1);
    message->len = (int64_t)len;
    memcpy(message->bytes, text, len);
    message->bytes[len] = '\0';
    a->panic = message;
    a->status = PANICKED;
    Message *m = a->handling;
    a->handling = NULL;
    if (m != NULL && m->waiter != NULL) {
        *m->outcome = PANICKED;
        wake(m->waiter, 0);
    }
    settle_mail(a, PANICKED);
    wake_joiners(a);
    fiber_exit(current);
}

/* `spawn Counter { … }` (v17): an actor holding `state` (GC memory). */
LughaActor *lugha_rt_actor_spawn(void *state, const char *name) {
    LughaActor *a = lugha_rt_alloc((int64_t)sizeof(LughaActor));
    a->state = state;
    a->name = name;
    lugha_rt_task_spawn(actor_loop, NULL, a, 1);
    last_spawned->actor = a;
    return a;
}

static void post(LughaActor *a, Message *m) {
    m->next = NULL;
    if (a->tail == NULL) {
        a->head = m;
    } else {
        a->tail->next = m;
    }
    a->tail = m;
    if (a->idle != NULL) {
        Fiber *f = a->idle;
        a->idle = NULL;
        wake(f, 0);
    }
}

static void check_alive(const LughaActor *a, const char *file, int64_t line, int64_t col) {
    if (a->status == STOPPED) {
        actor_panic(a, "has stopped", file, line, col);
    }
    if (a->status == PANICKED) {
        actor_panic(a, "panicked", file, line, col);
    }
}

/* A call: queues `handler(state, args, result)` and waits for it; the
 * result's address (`size` bytes of GC memory). */
void *lugha_rt_actor_call(LughaActor *a, Handler handler, void *args, int64_t size,
                          const char *file, int64_t line, int64_t col) {
    ensure_started();
    check_alive(a, file, line, col);
    int32_t outcome = RUNNING;
    Message *m = lugha_rt_alloc((int64_t)sizeof(Message));
    m->handler = handler;
    m->args = args;
    m->result = lugha_rt_alloc(size < 1 ? 1 : size);
    m->waiter = current;
    m->outcome = &outcome;
    post(a, m);
    block();
    if (outcome == STOPPED) {
        actor_panic(a, "has stopped", file, line, col);
    }
    if (outcome == PANICKED) {
        actor_panic(a, "panicked", file, line, col);
    }
    return m->result;
}

/* A cast: queues `handler(state, args, …)` and goes on. */
void lugha_rt_actor_cast(LughaActor *a, Handler handler, void *args, const char *file,
                         int64_t line, int64_t col) {
    ensure_started();
    check_alive(a, file, line, col);
    Message *m = lugha_rt_alloc((int64_t)sizeof(Message));
    m->handler = handler;
    m->args = args;
    post(a, m);
}

/* `a.stop()`: a message like any other, after what is queued. */
void lugha_rt_actor_stop(LughaActor *a, const char *file, int64_t line, int64_t col) {
    ensure_started();
    check_alive(a, file, line, col);
    Message *m = lugha_rt_alloc((int64_t)sizeof(Message));
    post(a, m);
}

/* `a.join()`: waits until the actor has ended; its panic's text, or NULL
 * when it stopped. */
LughaString *lugha_rt_actor_join(LughaActor *a) {
    ensure_started();
    if (a->status == RUNNING) {
        current->wait_next = a->joiners;
        a->joiners = current;
        block();
    }
    return a->panic;
}
