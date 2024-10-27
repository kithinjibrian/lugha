#ifndef HMAP_H
#define HMAP_H

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct hmap hmap_t;
typedef struct hmap_enum hmap_enum_t;

typedef void (*hmap_vfree)(void *in);
typedef void (*hmap_kfree)(void *in);
typedef void *(*hmap_kcopy)(void *in);
typedef void *(*hmap_vcopy)(void *in);
typedef unsigned int (*hmap_hash)(void *in);
typedef bool (*hmap_keq)(void *a, void *b);

typedef struct
{
    hmap_kcopy key_copy;
    hmap_kfree key_free;
    hmap_vcopy val_copy;
    hmap_vfree val_free;
} hmap_cbs;

static inline uint32_t str_hash(void *in)
{
    const char *key = in;
    uint32_t hash = 2166136261u;
    while (*key)
    {
        hash ^= (uint32_t)(unsigned char)(*key++);
        hash *= 16777619u;
    }
    return hash;
}

static inline bool str_eq(void *a, void *b)
{
    const char *key_a = a;
    const char *key_b = b;

    while (*key_a && *key_b && *key_a == *key_b)
    {
        key_a++;
        key_b++;
    }

    return *key_a == *key_b;
}

static inline unsigned int int_hash(void *a)
{
    return *(int *)a;
}

// Equality function for integers
static inline int int_equals(void *a, void *b)
{
    return *(int *)a == *(int *)b;
}

void hmap_destroy(hmap_t *ht);
hmap_t *hmap_create(hmap_hash h_fun, hmap_keq keq, hmap_cbs *cbs);

void hmap_remove(hmap_t *ht, void *key);
void hmap_insert(hmap_t *ht, void *key, void *val);

void *hmap_get_direct(hmap_t *ht, void *key);
bool hmap_get(hmap_t *ht, void *key, void **val);

void hmap_enum_destroy(hmap_enum_t *he);
hmap_enum_t *hmap_enum_create(hmap_t *ht);
bool hmap_enum_next(hmap_enum_t *he, void **key, void **val);

hmap_t *hmap_union(hmap_t *ht1, hmap_t *ht2);
hmap_t *hmap_difference(hmap_t *ht1, hmap_t *ht2);

#endif /* HMAP_H */