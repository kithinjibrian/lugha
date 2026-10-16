/*
 * chans.c — part of lugha_rt.c: channels and `select` (v17, milestone 91).
 *
 * `Chan.new()` is unbuffered: a send waits for a receiver. `Chan.buffered(n)`
 * holds up to `n` values in a ring. Waiting senders and receivers queue as
 * `Waiter`s on their own stacks; a `select` queues one per arm, and the
 * first arm to fire wins (`dequeue` drops the rest).
 *
 * A fragment, not compiled on its own: lughac's link.rs joins it right after
 * tasks.c, whose types and scheduler (`block`, `wake`, `add_sleeper`) it uses.
 */

typedef struct Waiter Waiter;

/* One fiber waiting on one channel: in its `recvq` or `sendq`. Lives on the
 * waiting fiber's stack, which stays put while it waits. */
struct Waiter {
    Fiber *fiber;
    /* The value to send, or where a received one goes. */
    void *data;
    /* Receivers: 1 for a value, 0 for closed. Senders: 0 when closed. */
    int32_t *ok;
    /* The `select` case, or 0. */
    int64_t index;
    Waiter *next;
};

/* A `Chan<T>` (GC object). */
typedef struct {
    int64_t cap;
    int64_t elem;
    int64_t len;
    int64_t head;
    unsigned char *buf;
    int32_t closed;
    Waiter *recvq;
    Waiter *sendq;
} LughaChan;

/* One `select` arm handed over by generated code (v17). */
typedef struct {
    LughaChan *chan;
    /* 0 receive, 1 send. */
    int64_t kind;
    void *data;
    int32_t ok;
} LughaCase;

/* `select`'s pick: pseudo-random, the same on every run. */
static uint64_t pick_state = 0x9e3779b97f4a7c15u;

static void chan_panic(const char *what, const char *file, int64_t line, int64_t col) {
    fflush(stdout);
    fprintf(stderr, "panic: %s at %s:%" PRId64 ":%" PRId64 "\n", what, file, line, col);
    exit(101);
}

/* `Chan.new()` and `Chan.buffered(n)`. */
LughaChan *lugha_rt_chan_new(int64_t cap, int64_t elem, const char *file, int64_t line,
                             int64_t col) {
    if (cap < 0) {
        chan_panic("negative channel buffer", file, line, col);
    }
    LughaChan *c = lugha_rt_alloc((int64_t)sizeof(LughaChan));
    c->cap = cap;
    c->elem = elem < 1 ? 1 : elem;
    if (cap > 0) {
        if (cap > INT64_MAX / c->elem) {
            die("panic: out of memory\n");
        }
        c->buf = lugha_rt_alloc(cap * c->elem);
    }
    return c;
}

static void enqueue(Waiter **queue, Waiter *w) {
    w->next = NULL;
    while (*queue != NULL) {
        queue = &(*queue)->next;
    }
    *queue = w;
}

/* The first waiter whose fiber still waits; waiters of a `select` that
 * another case already woke are dropped on the way. */
static Waiter *dequeue(Waiter **queue) {
    while (*queue != NULL) {
        Waiter *w = *queue;
        *queue = w->next;
        if (w->fiber->fired == WAITING) {
            return w;
        }
    }
    return NULL;
}

static void unqueue(Waiter **queue, Fiber *f) {
    while (*queue != NULL) {
        if ((*queue)->fiber == f) {
            *queue = (*queue)->next;
        } else {
            queue = &(*queue)->next;
        }
    }
}

static bool can_send(LughaChan *c) {
    if (c->closed || c->len < c->cap) {
        return true;
    }
    for (Waiter *w = c->recvq; w != NULL; w = w->next) {
        if (w->fiber->fired == WAITING) {
            return true;
        }
    }
    return false;
}

static bool can_recv(LughaChan *c) {
    if (c->closed || c->len > 0) {
        return true;
    }
    for (Waiter *w = c->sendq; w != NULL; w = w->next) {
        if (w->fiber->fired == WAITING) {
            return true;
        }
    }
    return false;
}

/* Sends without waiting; the caller made sure it can. */
static void send_now(LughaChan *c, const void *value, const char *file, int64_t line,
                     int64_t col) {
    if (c->closed) {
        chan_panic("send on a closed channel", file, line, col);
    }
    Waiter *r = dequeue(&c->recvq);
    if (r != NULL) {
        memcpy(r->data, value, (size_t)c->elem);
        *r->ok = 1;
        wake(r->fiber, r->index);
        return;
    }
    memcpy(c->buf + ((c->head + c->len) % c->cap) * c->elem, value, (size_t)c->elem);
    c->len++;
}

/* Receives without waiting; the caller made sure it can. 1 for a value,
 * 0 when closed and empty. */
static int32_t recv_now(LughaChan *c, void *out) {
    if (c->len > 0) {
        memcpy(out, c->buf + c->head * c->elem, (size_t)c->elem);
        c->head = (c->head + 1) % c->cap;
        c->len--;
        /* A waiting sender's value takes the freed place. */
        Waiter *s = dequeue(&c->sendq);
        if (s != NULL) {
            memcpy(c->buf + ((c->head + c->len) % c->cap) * c->elem, s->data, (size_t)c->elem);
            c->len++;
            *s->ok = 1;
            wake(s->fiber, s->index);
        }
        return 1;
    }
    Waiter *s = dequeue(&c->sendq);
    if (s != NULL) {
        memcpy(out, s->data, (size_t)c->elem);
        *s->ok = 1;
        wake(s->fiber, s->index);
        return 1;
    }
    memset(out, 0, (size_t)c->elem);
    return 0;
}

/* `c.send(v)`: `value` points at the copy in element layout. */
void lugha_rt_chan_send(LughaChan *c, const void *value, const char *file, int64_t line,
                        int64_t col) {
    ensure_started();
    if (can_send(c)) {
        send_now(c, value, file, line, col);
        return;
    }
    int32_t ok = 1;
    Waiter w = {.fiber = current, .data = (void *)value, .ok = &ok, .index = 0};
    enqueue(&c->sendq, &w);
    block();
    if (!ok) {
        chan_panic("send on a closed channel", file, line, col);
    }
}

/* `c.recv()`: 1 and the value in `out`, or 0 once closed and empty. */
int32_t lugha_rt_chan_recv(LughaChan *c, void *out) {
    ensure_started();
    if (can_recv(c)) {
        return recv_now(c, out);
    }
    int32_t ok = 0;
    Waiter w = {.fiber = current, .data = out, .ok = &ok, .index = 0};
    enqueue(&c->recvq, &w);
    block();
    return ok;
}

/* `c.try_recv()`: never waits. */
int32_t lugha_rt_chan_try_recv(LughaChan *c, void *out) {
    ensure_started();
    if (can_recv(c)) {
        return recv_now(c, out);
    }
    memset(out, 0, (size_t)c->elem);
    return 0;
}

/* `c.close()`: receivers get `None`, waiting senders panic. */
void lugha_rt_chan_close(LughaChan *c, const char *file, int64_t line, int64_t col) {
    ensure_started();
    if (c->closed) {
        chan_panic("close of a closed channel", file, line, col);
    }
    c->closed = 1;
    for (Waiter *r = dequeue(&c->recvq); r != NULL; r = dequeue(&c->recvq)) {
        memset(r->data, 0, (size_t)c->elem);
        *r->ok = 0;
        wake(r->fiber, r->index);
    }
    for (Waiter *s = dequeue(&c->sendq); s != NULL; s = dequeue(&c->sendq)) {
        *s->ok = 0;
        wake(s->fiber, s->index);
    }
}

static uint64_t pick(uint64_t n) {
    pick_state ^= pick_state << 13;
    pick_state ^= pick_state >> 7;
    pick_state ^= pick_state << 17;
    return pick_state % n;
}

/* `select` (v17): the case that ran, DEFAULT_ARM (-1) for `default`, or
 * TIMED_OUT (-2) for `after`. `timeout_ms` < 0 means no `after`. A ready
 * case is picked at random; otherwise the thread waits on all of them. */
int64_t lugha_rt_task_select(LughaCase *cases, int64_t n, int64_t timeout_ms, int32_t has_default,
                             const char *file, int64_t line, int64_t col) {
    ensure_started();
    int64_t ready = 0;
    for (int64_t i = 0; i < n; i++) {
        LughaChan *c = cases[i].chan;
        if (cases[i].kind == 0 ? can_recv(c) : can_send(c)) {
            ready++;
        }
    }
    if (ready > 0) {
        int64_t chosen = (int64_t)pick((uint64_t)ready);
        for (int64_t i = 0; i < n; i++) {
            LughaChan *c = cases[i].chan;
            if (!(cases[i].kind == 0 ? can_recv(c) : can_send(c))) {
                continue;
            }
            if (chosen-- > 0) {
                continue;
            }
            if (cases[i].kind == 0) {
                cases[i].ok = recv_now(c, cases[i].data);
            } else {
                send_now(c, cases[i].data, file, line, col);
            }
            return i;
        }
    }
    if (has_default) {
        return DEFAULT_ARM;
    }
    if (n > 64) {
        die("panic: a select with more than 64 arms\n");
    }
    Waiter waiters[64];
    for (int64_t i = 0; i < n; i++) {
        cases[i].ok = cases[i].kind == 0 ? 0 : 1;
        waiters[i] = (Waiter){.fiber = current, .data = cases[i].data, .ok = &cases[i].ok,
                              .index = i};
        enqueue(cases[i].kind == 0 ? &cases[i].chan->recvq : &cases[i].chan->sendq, &waiters[i]);
    }
    if (timeout_ms >= 0) {
        int64_t ns = timeout_ms > INT64_MAX / 4000000 ? INT64_MAX / 4 : timeout_ms * 1000000;
        add_sleeper(current, now_ns() + ns);
    }
    int64_t fired = block();
    for (int64_t i = 0; i < n; i++) {
        unqueue(&cases[i].chan->recvq, current);
        unqueue(&cases[i].chan->sendq, current);
    }
    if (fired >= 0 && cases[fired].kind == 1 && !cases[fired].ok) {
        chan_panic("send on a closed channel", file, line, col);
    }
    return fired;
}
