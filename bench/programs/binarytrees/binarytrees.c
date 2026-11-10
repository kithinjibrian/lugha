#include <stdio.h>
#include <stdlib.h>

/* Every node is its own malloc, freed after each check, like a plain
   C program would: no pools or arenas. */
typedef struct Node { struct Node *l, *r; } Node;

static Node *make(long depth) {
    Node *n = malloc(sizeof(Node));
    if (!n) abort();
    if (depth == 0) {
        n->l = n->r = NULL;
    } else {
        n->l = make(depth - 1);
        n->r = make(depth - 1);
    }
    return n;
}

static long check(const Node *n) { return n ? 1 + check(n->l) + check(n->r) : 0; }

static void release(Node *n) {
    if (!n) return;
    release(n->l);
    release(n->r);
    free(n);
}

int main(int argc, char **argv) {
    (void)argc;
    long max_depth = atol(argv[1]);
    Node *stretch = make(max_depth + 1);
    printf("stretch %ld %ld\n", max_depth + 1, check(stretch));
    release(stretch);
    Node *long_lived = make(max_depth);
    for (long depth = 4; depth <= max_depth; depth += 2) {
        long iterations = 1L << (max_depth - depth + 4);
        long sum = 0;
        for (long i = 0; i < iterations; i++) {
            Node *t = make(depth);
            sum += check(t);
            release(t);
        }
        printf("%ld %ld %ld\n", iterations, depth, sum);
    }
    printf("long %ld %ld\n", max_depth, check(long_lived));
    release(long_lived);
    return 0;
}
