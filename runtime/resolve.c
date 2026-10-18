/*
 * resolve.c — part of lugha_rt.c: `std.net`'s name lookup (v17, milestone
 * 121). `getaddrinfo` blocks the OS thread while it runs, as `net_connect`'s
 * lookup does. Every failure comes back as 0 and the resolver's message.
 *
 * A fragment, not compiled on its own: joined after net.c (`net_error`).
 */

/* `$os.net_resolve(name, numeric)`: every address `name` resolves to, as
 * text joined by newlines, in the resolver's order (`std.net` drops
 * repeats), or 0 and the resolver's reason. `numeric` allows IP literals
 * only (`AI_NUMERICHOST`: no lookup). */
int32_t lugha_rt_net_resolve(const LughaString *name, int64_t numeric, LughaString **out,
                             LughaString **error) {
    if (name->len == 0) {
        return net_error(error, "no name to resolve");
    }
    if (memchr(name->bytes, 0, (size_t)name->len) != NULL) {
        return net_error(error, "name contains a NUL byte");
    }
    char *host = lugha_rt_alloc(name->len + 1);
    memcpy(host, name->bytes, (size_t)name->len);
    host[name->len] = 0;
    // One socket type, so each address comes once per family, not per protocol.
    struct addrinfo hints = {.ai_family = AF_UNSPEC, .ai_socktype = SOCK_STREAM};
    hints.ai_flags = numeric ? AI_NUMERICHOST : 0;
    struct addrinfo *list = NULL;
    int code = getaddrinfo(host, NULL, &hints, &list);
    if (code != 0) {
        return net_error(error, gai_strerror(code));
    }
    size_t cap = 64, len = 0;
    char *text = lugha_rt_alloc((int64_t)cap);
    for (struct addrinfo *a = list; a != NULL; a = a->ai_next) {
        char one[INET6_ADDRSTRLEN];
        const void *at = a->ai_family == AF_INET6
                             ? (const void *)&((struct sockaddr_in6 *)a->ai_addr)->sin6_addr
                             : (const void *)&((struct sockaddr_in *)a->ai_addr)->sin_addr;
        if ((a->ai_family != AF_INET && a->ai_family != AF_INET6) ||
            inet_ntop(a->ai_family, at, one, sizeof one) == NULL) {
            continue;
        }
        size_t n = strlen(one);
        if (len + n + 2 > cap) {
            cap = (len + n + 2) * 2;
            char *bigger = lugha_rt_alloc((int64_t)cap);
            memcpy(bigger, text, len);
            text = bigger;
        }
        if (len > 0) {
            text[len++] = '\n';
        }
        memcpy(text + len, one, n);
        len += n;
    }
    freeaddrinfo(list);
    if (len == 0) {
        return net_error(error, "no address for the name");
    }
    *out = string_of(text, (int64_t)len);
    return 1;
}
