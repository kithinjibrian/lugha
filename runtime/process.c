/*
 * process.c — part of lugha_rt.c: `std.process`'s one built-in (v15,
 * milestone 66), reviewed by the user as PRP-095's security design.
 *
 * - No shell: `posix_spawnp` gets the argument list as given, so no text is
 *   ever split, globbed or interpreted. A command without `/` is found on
 *   `PATH`; one with `/` is a path. A NUL byte anywhere is refused.
 * - The child gets this program's environment unchanged and only fds 0, 1
 *   and 2: the pipes are made close-on-exec, and so is every descriptor the
 *   runtime opens elsewhere (`fopen` mode "e", `O_CLOEXEC`).
 * - stdin, stdout and stderr are served together by `poll`, so a child that
 *   fills one pipe while this side waits on another can't deadlock.
 *   `SIGPIPE` is blocked while writing; a child that closes its stdin early
 *   just ends the input.
 * - Death by signal n is code 128 + n. There is no timeout and no output
 *   limit.
 *
 * A fragment, not compiled on its own: lughac's link.rs joins it after the
 * core (lugha_rt.c), whose includes (`<spawn.h>`, `<poll.h>`,
 * `<sys/wait.h>`, `<signal.h>`, `<fcntl.h>`, `<time.h>`) it relies on, and
 * after bytes.c (`bytes_of`) and input.c (`string_of`).
 */

extern char **environ;

/* A growing byte buffer for one of the child's outputs. */
typedef struct {
    char *bytes;
    int64_t len;
    int64_t cap;
} Captured;

/* Fails with `message` (and, when `cmd` is given, `cmd: ` before it). */
static int32_t spawn_fail(LughaString **error, const char *cmd, const char *message) {
    char text[512];
    if (cmd != NULL) {
        snprintf(text, sizeof text, "%s: %s", cmd, message);
    } else {
        snprintf(text, sizeof text, "%s", message);
    }
    *error = string_of(text, (int64_t)strlen(text));
    return 0;
}

/* Reads what `fd` has into `into`; false at end of input or on an error. */
static bool drain(int fd, Captured *into) {
    if (into->len == into->cap) {
        if (into->cap > INT64_MAX / 2) {
            die("panic: out of memory\n");
        }
        int64_t cap = into->cap * 2;
        char *bigger = lugha_rt_alloc(cap);
        memcpy(bigger, into->bytes, (size_t)into->len);
        into->bytes = bigger;
        into->cap = cap;
    }
    ssize_t got = read(fd, into->bytes + into->len, (size_t)(into->cap - into->len));
    if (got > 0) {
        into->len += got;
        return true;
    }
    return got < 0 && (errno == EINTR || errno == EAGAIN);
}

/* `$os.spawn(cmd, args, input, mode)`: runs `cmd` with `args` and waits.
 * `mode` 0 shares this program's stdin, stdout and stderr; 1 captures
 * stdout and stderr with an empty stdin; 2 also writes `input` to stdin.
 * 1 with `*code` and the outputs' bytes, or 0 and `*error`. */
int32_t lugha_rt_spawn(const LughaString *cmd, const LughaArray *args, const LughaString *input,
                       int32_t mode, int64_t *code, LughaArray **out, LughaArray **err,
                       LughaString **error) {
    const LughaString **list = (const LughaString **)args->elems;
    if (memchr(cmd->bytes, 0, (size_t)cmd->len) != NULL) {
        return spawn_fail(error, NULL, "command contains a NUL byte");
    }
    for (int64_t i = 0; i < args->len; i++) {
        if (memchr(list[i]->bytes, 0, (size_t)list[i]->len) != NULL) {
            char text[64];
            snprintf(text, sizeof text, "args[%" PRId64 "] contains a NUL byte", i);
            return spawn_fail(error, NULL, text);
        }
    }
    /* argv: the command, the arguments, NULL. Lugha strings end in NUL. */
    char **argv = lugha_rt_alloc((args->len + 2) * (int64_t)sizeof(char *));
    argv[0] = (char *)cmd->bytes;
    for (int64_t i = 0; i < args->len; i++) {
        argv[i + 1] = (char *)list[i]->bytes;
    }
    argv[args->len + 1] = NULL;

    bool capture = mode != 0;
    int in_pipe[2] = {-1, -1}, out_pipe[2] = {-1, -1}, err_pipe[2] = {-1, -1};
    posix_spawn_file_actions_t actions;
    posix_spawn_file_actions_init(&actions);
    if (capture) {
        if (pipe2(out_pipe, O_CLOEXEC) != 0 || pipe2(err_pipe, O_CLOEXEC) != 0 ||
            (mode == 2 && pipe2(in_pipe, O_CLOEXEC) != 0)) {
            int failed = errno;
            for (int k = 0; k < 2; k++) {
                if (out_pipe[k] >= 0) close(out_pipe[k]);
                if (err_pipe[k] >= 0) close(err_pipe[k]);
                if (in_pipe[k] >= 0) close(in_pipe[k]);
            }
            posix_spawn_file_actions_destroy(&actions);
            return spawn_fail(error, cmd->bytes, strerror(failed));
        }
        if (mode == 2) {
            posix_spawn_file_actions_adddup2(&actions, in_pipe[0], 0);
        } else {
            posix_spawn_file_actions_addopen(&actions, 0, "/dev/null", O_RDONLY, 0);
        }
        posix_spawn_file_actions_adddup2(&actions, out_pipe[1], 1);
        posix_spawn_file_actions_adddup2(&actions, err_pipe[1], 2);
    }
    /* What this program printed so far comes before what the child prints. */
    fflush(NULL);
    pid_t pid;
    int spawned = posix_spawnp(&pid, cmd->bytes, &actions, NULL, argv, environ);
    posix_spawn_file_actions_destroy(&actions);
    if (capture) {
        close(out_pipe[1]);
        close(err_pipe[1]);
        if (mode == 2) {
            close(in_pipe[0]);
        }
    }
    if (spawned != 0) {
        if (capture) {
            close(out_pipe[0]);
            close(err_pipe[0]);
            if (mode == 2) {
                close(in_pipe[1]);
            }
        }
        return spawn_fail(error, cmd->bytes, strerror(spawned));
    }

    Captured stdout_bytes = {lugha_rt_alloc(4096), 0, 4096};
    Captured stderr_bytes = {lugha_rt_alloc(4096), 0, 4096};
    if (capture) {
        /* Writes may fail with EPIPE; block SIGPIPE so that can't kill us. */
        sigset_t pipe_signal, before;
        sigemptyset(&pipe_signal);
        sigaddset(&pipe_signal, SIGPIPE);
        pthread_sigmask(SIG_BLOCK, &pipe_signal, &before);
        int64_t written = 0;
        int in_fd = mode == 2 ? in_pipe[1] : -1;
        if (in_fd >= 0) {
            fcntl(in_fd, F_SETFL, fcntl(in_fd, F_GETFL) | O_NONBLOCK);
            if (input->len == 0) {
                close(in_fd);
                in_fd = -1;
            }
        }
        int out_fd = out_pipe[0], err_fd = err_pipe[0];
        while (out_fd >= 0 || err_fd >= 0 || in_fd >= 0) {
            struct pollfd fds[3];
            nfds_t n = 0;
            int at_out = -1, at_err = -1, at_in = -1;
            if (out_fd >= 0) { at_out = (int)n; fds[n++] = (struct pollfd){out_fd, POLLIN, 0}; }
            if (err_fd >= 0) { at_err = (int)n; fds[n++] = (struct pollfd){err_fd, POLLIN, 0}; }
            if (in_fd >= 0) { at_in = (int)n; fds[n++] = (struct pollfd){in_fd, POLLOUT, 0}; }
            if (poll(fds, n, -1) < 0) {
                if (errno == EINTR) {
                    continue;
                }
                break;
            }
            if (at_out >= 0 && fds[at_out].revents != 0 && !drain(out_fd, &stdout_bytes)) {
                close(out_fd);
                out_fd = -1;
            }
            if (at_err >= 0 && fds[at_err].revents != 0 && !drain(err_fd, &stderr_bytes)) {
                close(err_fd);
                err_fd = -1;
            }
            if (at_in >= 0 && fds[at_in].revents != 0) {
                ssize_t put = write(in_fd, input->bytes + written, (size_t)(input->len - written));
                if (put > 0) {
                    written += put;
                }
                bool more = put >= 0 || errno == EINTR || errno == EAGAIN;
                if (!more || written == input->len) {
                    close(in_fd);
                    in_fd = -1;
                }
            }
        }
        /* A SIGPIPE raised while blocked is still pending: take it before
         * unblocking, so it can't end this program. */
        sigset_t pending;
        sigpending(&pending);
        if (sigismember(&pending, SIGPIPE)) {
            struct timespec now = {0, 0};
            sigtimedwait(&pipe_signal, NULL, &now);
        }
        pthread_sigmask(SIG_SETMASK, &before, NULL);
    }
    int status = 0;
    while (waitpid(pid, &status, 0) < 0) {
        if (errno != EINTR) {
            return spawn_fail(error, cmd->bytes, strerror(errno));
        }
    }
    if (WIFEXITED(status)) {
        *code = WEXITSTATUS(status);
    } else if (WIFSIGNALED(status)) {
        *code = 128 + WTERMSIG(status);
    } else {
        *code = 0;
    }
    *out = bytes_of(stdout_bytes.bytes, stdout_bytes.len);
    *err = bytes_of(stderr_bytes.bytes, stderr_bytes.len);
    return 1;
}
