/*
 * tls.c — `std.tls`'s built-ins (v17, milestone 112): TLS over net.c's
 * non-blocking sockets, through the system's OpenSSL 3. Appended to the
 * runtime only for programs that load `std.tls` (they link -lssl -lcrypto);
 * every other program never sees these headers.
 *
 * Certificates are always verified: the chain against the system's roots
 * (plus a `ca_file` the program names) and the host name. TLS 1.2 is the
 * oldest version spoken. When OpenSSL wants to read or write, the green
 * thread parks with `wait_fd` and the others run.
 *
 * OpenSSL's objects live outside the collector: `malloc`ed here and by
 * OpenSSL, freed by close. Lugha holds them as opaque `i64` handles.
 */
#include <openssl/err.h>
#include <openssl/ssl.h>
#include <openssl/x509v3.h>

typedef struct {
    SSL *ssl;
    int fd;
} TlsConn;

typedef struct {
    SSL_CTX *ctx;
    int fd;
} TlsListener;

/* OpenSSL's reason for the last failure, or `fallback`. */
static int32_t tls_error(LughaString **error, const char *fallback) {
    unsigned long code = ERR_get_error();
    char text[256];
    if (code != 0) {
        ERR_error_string_n(code, text, sizeof text);
        ERR_clear_error();
        return net_error(error, text);
    }
    return net_error(error, fallback);
}

/* A C string of `s`; `NULL` when it holds a NUL byte. */
static const char *tls_path(const LughaString *s) {
    return memchr(s->bytes, 0, (size_t)s->len) == NULL ? s->bytes : NULL;
}

/* OpenSSL writes with plain `send`, so a peer that hung up would raise
 * SIGPIPE: it is blocked around each OpenSSL call, and one raised during the
 * call is taken back, so the call just fails (as net.c's `MSG_NOSIGNAL`). */
static void tls_quiet(sigset_t *old) {
    sigset_t pipe;
    sigemptyset(&pipe);
    sigaddset(&pipe, SIGPIPE);
    pthread_sigmask(SIG_BLOCK, &pipe, old);
}

static void tls_loud(const sigset_t *old) {
    sigset_t pipe;
    sigemptyset(&pipe);
    sigaddset(&pipe, SIGPIPE);
    struct timespec now = {0, 0};
    while (sigtimedwait(&pipe, NULL, &now) > 0) {
    }
    pthread_sigmask(SIG_SETMASK, old, NULL);
}

/* Drives `step` (a handshake, read, write or shutdown) until it is done or
 * fails: 1 done (`*n` its result), 0 failed with the message set. */
static int tls_drive(TlsConn *c, int64_t deadline, int (*step)(SSL *, void *, int), void *buf,
                     int len, int *n, LughaString **error) {
    for (;;) {
        ERR_clear_error();
        sigset_t old;
        tls_quiet(&old);
        int r = step(c->ssl, buf, len);
        tls_loud(&old);
        if (r > 0) {
            *n = r;
            return 1;
        }
        int why = SSL_get_error(c->ssl, r);
        if (why == SSL_ERROR_WANT_READ || why == SSL_ERROR_WANT_WRITE) {
            if (!wait_fd(c->fd, why == SSL_ERROR_WANT_READ ? EPOLLIN : EPOLLOUT, deadline)) {
                net_error(error, "timed out");
                return 0;
            }
        } else if (why == SSL_ERROR_ZERO_RETURN) {
            *n = 0;
            return 1;
        } else {
            long verify = SSL_get_verify_result(c->ssl);
            if (verify != X509_V_OK) {
                char text[300];
                snprintf(text, sizeof text, "certificate verify failed: %s",
                         X509_verify_cert_error_string(verify));
                ERR_clear_error();
                net_error(error, text);
            } else if (why == SSL_ERROR_SYSCALL && errno != 0) {
                net_error(error, strerror(errno));
            } else {
                tls_error(error, "TLS failed");
            }
            return 0;
        }
    }
}

static int tls_handshake_connect(SSL *s, void *buf, int len) {
    (void)buf;
    (void)len;
    return SSL_connect(s);
}

static int tls_handshake_accept(SSL *s, void *buf, int len) {
    (void)buf;
    (void)len;
    return SSL_accept(s);
}

static int tls_read_step(SSL *s, void *buf, int len) {
    return SSL_read(s, buf, len);
}

static int tls_write_step(SSL *s, void *buf, int len) {
    return SSL_write(s, buf, len);
}

/* A connection on socket `fd` with `ctx`; the context's reference is the connection's. */
static TlsConn *tls_wrap(SSL_CTX *ctx, int fd) {
    SSL *ssl = SSL_new(ctx);
    SSL_CTX_free(ctx);
    if (ssl == NULL || SSL_set_fd(ssl, fd) != 1) {
        SSL_free(ssl);
        return NULL;
    }
    TlsConn *c = malloc(sizeof *c);
    if (c == NULL) {
        die("panic: out of memory\n");
    }
    c->ssl = ssl;
    c->fd = fd;
    return c;
}

static void tls_free(TlsConn *c) {
    SSL_free(c->ssl);
    close(c->fd);
    free(c);
}

/* `$os.tls_connect(addr, host, ca_file, timeout_ms)`: 1 and a verified
 * connection's handle, or 0 and the reason. `host` is what the certificate
 * must name: a host name, or an IP address. */
int32_t lugha_rt_tls_connect(const LughaString *addr, const LughaString *host,
                             const LughaString *ca_file, int64_t timeout_ms, int64_t *handle,
                             LughaString **error) {
    const char *name = tls_path(host);
    const char *ca = tls_path(ca_file);
    if (name == NULL || ca == NULL) {
        return net_error(error, "host or CA file contains a NUL byte");
    }
    SSL_CTX *ctx = SSL_CTX_new(TLS_client_method());
    if (ctx == NULL) {
        return tls_error(error, "TLS unavailable");
    }
    SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);
    SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
    /* A peer closing without close_notify reads as the end of the stream. */
    SSL_CTX_set_options(ctx, SSL_OP_IGNORE_UNEXPECTED_EOF);
    if (SSL_CTX_set_default_verify_paths(ctx) != 1 ||
        (ca[0] != 0 && SSL_CTX_load_verify_locations(ctx, ca, NULL) != 1)) {
        SSL_CTX_free(ctx);
        return tls_error(error, "can't load trusted certificates");
    }
    int64_t fd;
    if (!lugha_rt_net_connect(addr, timeout_ms, &fd, error)) {
        SSL_CTX_free(ctx);
        return 0;
    }
    TlsConn *c = tls_wrap(ctx, (int)fd);
    if (c == NULL) {
        close((int)fd);
        return tls_error(error, "TLS unavailable");
    }
    unsigned char ip[16];
    int is_ip = inet_pton(AF_INET, name, ip) == 1 || inet_pton(AF_INET6, name, ip) == 1;
    int named = is_ip ? X509_VERIFY_PARAM_set1_ip_asc(SSL_get0_param(c->ssl), name)
                      : SSL_set_tlsext_host_name(c->ssl, name) == 1 && SSL_set1_host(c->ssl, name);
    if (!named) {
        tls_free(c);
        return tls_error(error, "bad host name");
    }
    int n;
    if (!tls_drive(c, net_deadline(timeout_ms), tls_handshake_connect, NULL, 0, &n, error)) {
        tls_free(c);
        return 0;
    }
    *handle = (int64_t)(intptr_t)c;
    return 1;
}

/* `$os.tls_listen(addr, cert_file, key_file)`: 1 and a listener's handle,
 * or 0 and the reason. */
int32_t lugha_rt_tls_listen(const LughaString *addr, const LughaString *cert,
                            const LughaString *key, int64_t *handle, LughaString **error) {
    const char *cert_path = tls_path(cert);
    const char *key_path = tls_path(key);
    if (cert_path == NULL || key_path == NULL) {
        return net_error(error, "certificate or key path contains a NUL byte");
    }
    SSL_CTX *ctx = SSL_CTX_new(TLS_server_method());
    if (ctx == NULL) {
        return tls_error(error, "TLS unavailable");
    }
    SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);
    if (SSL_CTX_use_certificate_chain_file(ctx, cert_path) != 1 ||
        SSL_CTX_use_PrivateKey_file(ctx, key_path, SSL_FILETYPE_PEM) != 1 ||
        SSL_CTX_check_private_key(ctx) != 1) {
        SSL_CTX_free(ctx);
        return tls_error(error, "can't load the certificate or key");
    }
    int64_t fd;
    if (!lugha_rt_net_listen(addr, &fd, error)) {
        SSL_CTX_free(ctx);
        return 0;
    }
    TlsListener *l = malloc(sizeof *l);
    if (l == NULL) {
        die("panic: out of memory\n");
    }
    l->ctx = ctx;
    l->fd = (int)fd;
    *handle = (int64_t)(intptr_t)l;
    return 1;
}

/* `$os.tls_accept(listener, timeout_ms)`: the next connection, after its
 * handshake (which only this green thread waits for). */
int32_t lugha_rt_tls_accept(int64_t listener, int64_t timeout_ms, int64_t *handle,
                            LughaString **error) {
    TlsListener *l = (TlsListener *)(intptr_t)listener;
    int64_t fd;
    if (!lugha_rt_net_accept(l->fd, timeout_ms, &fd, error)) {
        return 0;
    }
    SSL_CTX_up_ref(l->ctx);
    TlsConn *c = tls_wrap(l->ctx, (int)fd);
    if (c == NULL) {
        close((int)fd);
        return tls_error(error, "TLS unavailable");
    }
    int n;
    if (!tls_drive(c, net_deadline(timeout_ms), tls_handshake_accept, NULL, 0, &n, error)) {
        tls_free(c);
        return 0;
    }
    *handle = (int64_t)(intptr_t)c;
    return 1;
}

/* `$os.tls_read(conn, max, timeout_ms)`: up to `max` bytes; none at the end. */
int32_t lugha_rt_tls_read(int64_t conn, int64_t max, int64_t timeout_ms, LughaArray **out,
                          LughaString **error) {
    if (max < 1 || max > NET_MAX_READ) {
        return net_error(error, "read size must be from 1 to 16777216 bytes");
    }
    char *buffer = lugha_rt_alloc(max);
    int n;
    if (!tls_drive((TlsConn *)(intptr_t)conn, net_deadline(timeout_ms), tls_read_step, buffer,
                   (int)max, &n, error)) {
        return 0;
    }
    *out = bytes_of(buffer, n);
    return 1;
}

/* `$os.tls_write(conn, bytes, timeout_ms)`: every byte, or the reason. */
int32_t lugha_rt_tls_write(int64_t conn, const LughaArray *bytes, int64_t timeout_ms,
                           LughaString **error) {
    int64_t deadline = net_deadline(timeout_ms);
    int64_t sent = 0;
    while (sent < bytes->len) {
        int64_t left = bytes->len - sent;
        int chunk = left > INT32_MAX ? INT32_MAX : (int)left;
        int n;
        if (!tls_drive((TlsConn *)(intptr_t)conn, deadline, tls_write_step,
                       (void *)(bytes->elems + sent), chunk, &n, error)) {
            return 0;
        }
        sent += n;
    }
    return 1;
}

/* `$os.tls_fd(conn)`: the socket, for `addr()` and `peer()`. */
int64_t lugha_rt_tls_fd(int64_t conn) {
    return ((TlsConn *)(intptr_t)conn)->fd;
}

/* `$os.tls_close(conn)`: says `close_notify` without waiting for the reply. */
void lugha_rt_tls_close(int64_t conn) {
    TlsConn *c = (TlsConn *)(intptr_t)conn;
    sigset_t old;
    tls_quiet(&old);
    SSL_shutdown(c->ssl);
    tls_loud(&old);
    ERR_clear_error();
    tls_free(c);
}

/* `$os.tls_close_listener(listener)`. */
void lugha_rt_tls_close_listener(int64_t listener) {
    TlsListener *l = (TlsListener *)(intptr_t)listener;
    close(l->fd);
    SSL_CTX_free(l->ctx);
    free(l);
}

/* `$os.tls_listener_fd(listener)`: the socket, for `addr()`. */
int64_t lugha_rt_tls_listener_fd(int64_t listener) {
    return ((TlsListener *)(intptr_t)listener)->fd;
}
