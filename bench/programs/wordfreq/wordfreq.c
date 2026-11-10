#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* C has no map, so this is a plain open-addressing table hashed with
   FNV-1a, growing at half full. Words point into the text. */
typedef struct { const char *w; size_t len; long count; } Entry;

static int64_t state = 42;
static int64_t next(void) {
    state = (state * 1103515245 + 12345) % 2147483648;
    return state >> 16;
}

static Entry *table;
static size_t cap = 1024, used = 0;

static uint64_t hash(const char *w, size_t len) {
    uint64_t h = 1469598103934665603ULL;
    for (size_t i = 0; i < len; i++) { h ^= (unsigned char)w[i]; h *= 1099511628211ULL; }
    return h;
}

static Entry *slot(Entry *t, size_t c, const char *w, size_t len) {
    size_t i = hash(w, len) & (c - 1);
    while (t[i].w && !(t[i].len == len && memcmp(t[i].w, w, len) == 0)) i = (i + 1) & (c - 1);
    return &t[i];
}

static void add(const char *w, size_t len) {
    Entry *e = slot(table, cap, w, len);
    if (e->w) { e->count++; return; }
    *e = (Entry){w, len, 1};
    if (++used * 2 > cap) {
        size_t ncap = cap * 2;
        Entry *nt = calloc(ncap, sizeof(Entry));
        if (!nt) abort();
        for (size_t i = 0; i < cap; i++)
            if (table[i].w) *slot(nt, ncap, table[i].w, table[i].len) = table[i];
        free(table);
        table = nt;
        cap = ncap;
    }
}

static int by_count(const void *pa, const void *pb) {
    const Entry *a = pa, *b = pb;
    if (a->count != b->count) return a->count > b->count ? -1 : 1;
    size_t m = a->len < b->len ? a->len : b->len;
    int c = memcmp(a->w, b->w, m);
    return c ? c : (a->len > b->len) - (a->len < b->len);
}

int main(int argc, char **argv) {
    (void)argc;
    long n = atol(argv[1]);
    char *vocab[1000];
    size_t vlen[1000];
    for (int v = 0; v < 1000; v++) {
        vlen[v] = 3 + next() % 8;
        vocab[v] = malloc(vlen[v]);
        if (!vocab[v]) abort();
        for (size_t i = 0; i < vlen[v]; i++) vocab[v][i] = (char)(97 + next() % 26);
    }
    size_t tcap = 1024, tlen = 0;
    char *text = malloc(tcap);
    if (!text) abort();
    for (long k = 0; k < n; k++) {
        int v = (int)(next() % 1000);
        while (tlen + vlen[v] + 1 > tcap) { tcap *= 2; text = realloc(text, tcap); if (!text) abort(); }
        if (k > 0) text[tlen++] = ' ';
        memcpy(text + tlen, vocab[v], vlen[v]);
        tlen += vlen[v];
    }
    table = calloc(cap, sizeof(Entry));
    if (!table) abort();
    size_t start = 0;
    for (size_t i = 0; i <= tlen; i++) {
        if (i == tlen || text[i] == ' ') { add(text + start, i - start); start = i + 1; }
    }
    Entry *entries = malloc(used * sizeof(Entry));
    if (!entries) abort();
    size_t m = 0;
    for (size_t i = 0; i < cap; i++) if (table[i].w) entries[m++] = table[i];
    qsort(entries, m, sizeof(Entry), by_count);
    printf("%zu\n", m);
    for (int i = 0; i < 10; i++) printf("%.*s %ld\n", (int)entries[i].len, entries[i].w, entries[i].count);
    return 0;
}
