#ifndef HSET_H
#define HSET_H

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#include "hmap.h"

typedef hmap_keq hset_keq;
typedef hmap_hash hset_hash;

typedef struct hset hset_t;
typedef struct hset_enum hset_enum_t;

typedef void (*hset_free)(void *in);
typedef void *(*hset_copy)(void *in);

typedef struct
{
    hset_copy copy;
    hset_free free;
} hset_cbs;

void hset_destroy(hset_t *ht);
bool hset_has(hset_t *ht, void *value);
void hset_insert(hset_t *ht, void *value);
void hset_remove(hset_t *ht, void *value);
hset_t *hset_create(hset_hash hash_fun, hset_keq keq, hset_cbs *cbs);

void hset_enum_destroy(hset_enum_t *he);
hset_enum_t *hset_enum_create(hset_t *ht);
bool hset_enum_next(hset_enum_t *he, void **value);

hset_t *hset_union(hset_t *ht1, hset_t *ht2);
hset_t *hset_difference(hset_t *ht1, hset_t *ht2);

#endif /* HSET_H */