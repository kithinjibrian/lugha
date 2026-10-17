/*
 * net.c — part of lugha_rt.c: `std.net`'s built-ins (v17, milestone 110).
 * TCP sockets, all non-blocking and close-on-exec. A call that would block
 * parks its green thread on one `epoll` set (`wait_fd`) and the others run;
 * the scheduler waits on that set when nothing is ready (`idle_wait`).
 * Every failure comes back as 0 and the system's message, never a panic.
 *
 * A fragment, not compiled on its own: joined after tasks.c (the scheduler
 * it extends), bytes.c (`bytes_of`) and input.c (`string_of`).
 */

enum { NET_MAX_READ = 16 * 1024 * 1024, NET_BACKLOG = 128, NET_EVENTS = 64 };

static int net_epoll = -1;
/* Green threads parked on a socket. */
static int64_t net_waiting;

static int32_t net_error(LughaString **error, const char *message) {
    *error = string_of(message, (int64_t)strlen(message));
    return 0;
}

/* Wakes every thread whose socket is ready; waits up to `wait_ns` (< 0
 * forever) for the first. A thread already woken by its timeout is skipped. */
static void poll_sockets(int64_t wait_ns) {
    int ms = -1;
    if (wait_ns >= 0) {
        int64_t rounded = (wait_ns + 999999) / 1000000;
        ms = rounded > INT32_MAX ? INT32_MAX : (int)rounded;
    }
    struct epoll_event events[NET_EVENTS];
    int n = epoll_wait(net_epoll, events, NET_EVENTS, ms);
    for (int i = 0; i < n; i++) {
        Fiber *f = events[i].data.ptr;
        if (f != NULL && f->fired == WAITING) {
            wake(f, 0);
        }
    }
}

static void poll_ready_sockets(void) {
    if (net_waiting > 0) {
        poll_sockets(0);
    }
}

static void idle_wait(void) {
    if (sleepers == NULL && net_waiting == 0) {
        deadlock();
    }
    int64_t wait = sleepers == NULL ? -1 : sleepers->wake_at - now_ns();
    if (net_waiting > 0) {
        poll_sockets(wait < 0 && sleepers != NULL ? 0 : wait);
    } else if (wait > 0) {
        struct timespec t = {.tv_sec = wait / 1000000000, .tv_nsec = wait % 1000000000};
        while (nanosleep(&t, &t) != 0 && errno == EINTR) {
        }
    }
}

/* Parks this thread until `fd` is ready for `events`: 1, or 0 when
 * `deadline` (monotonic ns; < 0 for none) passes first. */
static int wait_fd(int fd, uint32_t events, int64_t deadline) {
    ensure_started();
    if (net_epoll < 0) {
        net_epoll = epoll_create1(EPOLL_CLOEXEC);
        if (net_epoll < 0) {
            die("panic: epoll_create1 failed\n");
        }
    }
    struct epoll_event ev = {.events = events | EPOLLONESHOT, .data = {.ptr = current}};
    if (epoll_ctl(net_epoll, EPOLL_CTL_MOD, fd, &ev) != 0 &&
        epoll_ctl(net_epoll, EPOLL_CTL_ADD, fd, &ev) != 0) {
        die("panic: epoll_ctl failed\n");
    }
    if (deadline >= 0) {
        add_sleeper(current, deadline);
    }
    net_waiting++;
    int64_t why = block();
    net_waiting--;
    if (why == TIMED_OUT) {
        /* Disarmed, so a late event can't wake this thread again. */
        struct epoll_event off = {.events = 0, .data = {.ptr = NULL}};
        epoll_ctl(net_epoll, EPOLL_CTL_MOD, fd, &off);
        return 0;
    }
    return 1;
}

static int64_t net_deadline(int64_t timeout_ms) {
    if (timeout_ms <= 0) {
        return -1;
    }
    int64_t ns = timeout_ms > INT64_MAX / 2000000 ? INT64_MAX / 2 : timeout_ms * 1000000;
    return now_ns() + ns;
}

/* `addr` ("host:port", "[v6]:port") resolved; 0 and a message when it
 * can't be. The list is freed by the caller. */
static const char *net_resolve(const LughaString *addr, int passive, struct addrinfo **out) {
    if (memchr(addr->bytes, 0, (size_t)addr->len) != NULL) {
        return "address contains a NUL byte";
    }
    const char *colon = memrchr(addr->bytes, ':', (size_t)addr->len);
    if (colon == NULL || colon == addr->bytes || colon[1] == 0) {
        return "address must be host:port";
    }
    size_t host_len = (size_t)(colon - addr->bytes);
    const char *host = addr->bytes;
    if (host[0] == '[') {
        if (host_len < 3 || host[host_len - 1] != ']') {
            return "address must be host:port";
        }
        host++;
        host_len -= 2;
    }
    char *name = lugha_rt_alloc((int64_t)host_len + 1);
    memcpy(name, host, host_len);
    name[host_len] = 0;
    const char *port = colon + 1;
    for (const char *p = port; *p != 0; p++) {
        if (*p < '0' || *p > '9' || p - port > 5) {
            return "port must be a number from 0 to 65535";
        }
    }
    if (atol(port) > 65535) {
        return "port must be a number from 0 to 65535";
    }
    struct addrinfo hints = {.ai_family = AF_UNSPEC, .ai_socktype = SOCK_STREAM};
    hints.ai_flags = passive ? AI_PASSIVE : 0;
    int code = getaddrinfo(name, port, &hints, out);
    return code == 0 ? NULL : gai_strerror(code);
}

static int net_socket(const struct addrinfo *a) {
    return socket(a->ai_family, a->ai_socktype | SOCK_NONBLOCK | SOCK_CLOEXEC, a->ai_protocol);
}

/* `$os.net_listen(addr)`: 1 and a listening socket, or 0 and a message. */
int32_t lugha_rt_net_listen(const LughaString *addr, int64_t *fd, LughaString **error) {
    struct addrinfo *list = NULL;
    const char *problem = net_resolve(addr, 1, &list);
    if (problem != NULL) {
        return net_error(error, problem);
    }
    int saved = 0;
    for (struct addrinfo *a = list; a != NULL; a = a->ai_next) {
        int s = net_socket(a);
        int one = 1;
        if (s >= 0 && setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one) == 0 &&
            bind(s, a->ai_addr, a->ai_addrlen) == 0 && listen(s, NET_BACKLOG) == 0) {
            freeaddrinfo(list);
            *fd = s;
            return 1;
        }
        saved = errno;
        if (s >= 0) {
            close(s);
        }
    }
    freeaddrinfo(list);
    return net_error(error, strerror(saved));
}

/* `$os.net_connect(addr, timeout_ms)`: 1 and a connected socket, or 0 and
 * a message; tries each address the name resolves to in turn. */
int32_t lugha_rt_net_connect(const LughaString *addr, int64_t timeout_ms, int64_t *fd,
                             LughaString **error) {
    struct addrinfo *list = NULL;
    const char *problem = net_resolve(addr, 0, &list);
    if (problem != NULL) {
        return net_error(error, problem);
    }
    int64_t deadline = net_deadline(timeout_ms);
    const char *message = "no address to connect to";
    for (struct addrinfo *a = list; a != NULL; a = a->ai_next) {
        int s = net_socket(a);
        if (s < 0) {
            message = strerror(errno);
            continue;
        }
        int err = connect(s, a->ai_addr, a->ai_addrlen) == 0 ? 0 : errno;
        if (err == EINPROGRESS) {
            socklen_t len = sizeof err;
            if (!wait_fd(s, EPOLLOUT, deadline)) {
                err = ETIMEDOUT;
            } else if (getsockopt(s, SOL_SOCKET, SO_ERROR, &err, &len) != 0) {
                err = errno;
            }
        }
        if (err == 0) {
            freeaddrinfo(list);
            *fd = s;
            return 1;
        }
        message = err == ETIMEDOUT ? "timed out" : strerror(err);
        close(s);
    }
    freeaddrinfo(list);
    return net_error(error, message);
}

/* `$os.net_accept(fd, timeout_ms)`: 1 and the next connection, or 0 and a message. */
int32_t lugha_rt_net_accept(int64_t fd, int64_t timeout_ms, int64_t *conn, LughaString **error) {
    int64_t deadline = net_deadline(timeout_ms);
    for (;;) {
        int c = accept4((int)fd, NULL, NULL, SOCK_NONBLOCK | SOCK_CLOEXEC);
        if (c >= 0) {
            *conn = c;
            return 1;
        }
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            if (!wait_fd((int)fd, EPOLLIN, deadline)) {
                return net_error(error, "timed out");
            }
        } else if (errno != EINTR && errno != ECONNABORTED) {
            return net_error(error, strerror(errno));
        }
    }
}

/* `$os.net_read(fd, max, timeout_ms)`: 1 and up to `max` bytes (none at the
 * end of the stream), or 0 and a message. */
int32_t lugha_rt_net_read(int64_t fd, int64_t max, int64_t timeout_ms, LughaArray **out,
                          LughaString **error) {
    if (max < 1 || max > NET_MAX_READ) {
        return net_error(error, "read size must be from 1 to 16777216 bytes");
    }
    int64_t deadline = net_deadline(timeout_ms);
    char *buffer = lugha_rt_alloc(max);
    for (;;) {
        ssize_t n = recv((int)fd, buffer, (size_t)max, 0);
        if (n >= 0) {
            *out = bytes_of(buffer, (int64_t)n);
            return 1;
        }
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            if (!wait_fd((int)fd, EPOLLIN, deadline)) {
                return net_error(error, "timed out");
            }
        } else if (errno != EINTR) {
            return net_error(error, strerror(errno));
        }
    }
}

/* `$os.net_write(fd, bytes, timeout_ms)`: 1 once every byte is sent, or 0
 * and a message. `MSG_NOSIGNAL`: a closed peer is an error, not SIGPIPE. */
int32_t lugha_rt_net_write(int64_t fd, const LughaArray *bytes, int64_t timeout_ms,
                           LughaString **error) {
    int64_t deadline = net_deadline(timeout_ms);
    int64_t sent = 0;
    while (sent < bytes->len) {
        ssize_t n = send((int)fd, bytes->elems + sent, (size_t)(bytes->len - sent), MSG_NOSIGNAL);
        if (n >= 0) {
            sent += n;
        } else if (errno == EAGAIN || errno == EWOULDBLOCK) {
            if (!wait_fd((int)fd, EPOLLOUT, deadline)) {
                return net_error(error, "timed out");
            }
        } else if (errno != EINTR) {
            return net_error(error, strerror(errno));
        }
    }
    return 1;
}

/* `$os.net_close(fd)`: closing also takes it out of the `epoll` set. */
void lugha_rt_net_close(int64_t fd) {
    close((int)fd);
}

/* `$os.net_name(fd, peer)`: 1 and "host:port" of this end (or the peer's),
 * "[v6]:port" for IPv6; or 0 and a message. */
int32_t lugha_rt_net_name(int64_t fd, int64_t peer, LughaString **out, LughaString **error) {
    struct sockaddr_storage at;
    socklen_t len = sizeof at;
    int got = peer ? getpeername((int)fd, (struct sockaddr *)&at, &len)
                   : getsockname((int)fd, (struct sockaddr *)&at, &len);
    if (got != 0) {
        return net_error(error, strerror(errno));
    }
    char host[INET6_ADDRSTRLEN];
    char text[INET6_ADDRSTRLEN + 16];
    int port;
    if (at.ss_family == AF_INET6) {
        struct sockaddr_in6 *v6 = (struct sockaddr_in6 *)&at;
        inet_ntop(AF_INET6, &v6->sin6_addr, host, sizeof host);
        port = ntohs(v6->sin6_port);
        snprintf(text, sizeof text, "[%s]:%d", host, port);
    } else {
        struct sockaddr_in *v4 = (struct sockaddr_in *)&at;
        inet_ntop(AF_INET, &v4->sin_addr, host, sizeof host);
        port = ntohs(v4->sin_port);
        snprintf(text, sizeof text, "%s:%d", host, port);
    }
    *out = string_of(text, (int64_t)strlen(text));
    return 1;
}
