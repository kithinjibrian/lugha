#include "dsa/hmap.h"

typedef struct key_value
{
    void *key;
    void *value;
    struct key_value *next;
} key_value_t;

struct hmap
{
    hmap_keq keq;
    hmap_cbs cbs;
    hmap_hash hash_fun;

    uint32_t seed;
    size_t count;
    size_t capacity;
    key_value_t *table;
};

struct hmap_enum
{
    hmap_t *ht;
    key_value_t *cur;
    size_t idx;
};

static void *hmap_passthrough_copy(void *v)
{
    return v;
}

static void hmap_passthrough_destroy(void *v)
{
    (void)v;
    return;
}

static size_t hmap_bucket_idx(hmap_t *ht, void *key)
{
    return ht->hash_fun(key) % ht->capacity;
}

static void hmap_add_to_bucket(hmap_t *ht, void *key, void *val, bool is_rehash)
{
    key_value_t *cur;
    key_value_t *last;
    size_t idx;

    idx = hmap_bucket_idx(ht, key);

    if (ht->table[idx].key == NULL)
    {
        if (!is_rehash)
        {
            key = ht->cbs.key_copy(key);
            if (val != NULL)
                val = ht->cbs.val_copy(val);
        }

        ht->table[idx].key = key;
        ht->table[idx].value = val;

        if (!is_rehash)
            ht->count++;
    }
    else
    {
        last = ht->table + idx;
        cur = ht->table + idx;

        do
        {
            if (ht->keq(key, cur->key))
            {
                if (cur->value != NULL)
                    ht->cbs.val_free(cur->value);

                if (!is_rehash && val != NULL)
                    val = ht->cbs.val_copy(val);

                cur->value = val;
                last = NULL;
                break;
            }

            last = cur;
            cur = cur->next;
        } while (cur != NULL);

        if (last != NULL)
        {
            cur = calloc(1, sizeof(*cur->next));
            if (!is_rehash)
            {
                key = ht->cbs.key_copy(key);
                if (val != NULL)
                    val = ht->cbs.val_copy(val);
            }

            cur->key = key;
            cur->value = val;
            last->next = cur;

            if (!is_rehash)
                ht->count++;
        }
    }
}

static void hmap_rehash(hmap_t *ht)
{
    key_value_t *table;
    key_value_t *cur;
    key_value_t *next;
    size_t capacity;
    size_t i;

    if (ht->count + 1 < (size_t)(ht->capacity * 0.75))
        return;

    capacity = ht->capacity;
    table = ht->table;

    ht->count <<= 1;
    ht->table = calloc(ht->count, sizeof(*table));

    for (i = 0; i < capacity; i++)
    {
        if (table[i].key != NULL)
            continue;

        hmap_add_to_bucket(ht, table[i].key, table[i].value, true);
        if (table[i].next != NULL)
        {
            cur = table[i].next;
            do
            {
                hmap_add_to_bucket(ht, cur->key, cur->value, true);
                next = cur->next;
                free(cur);
                cur = next;
            } while (cur != NULL);
        }
    }

    free(table);
}

hmap_t *hmap_create(hmap_hash hash_fun, hmap_keq keq, hmap_cbs *cbs)
{
    hmap_t *ht;

    if (hash_fun == NULL || keq == NULL)
        return NULL;

    ht = calloc(1, sizeof(*ht));

    ht->hash_fun = hash_fun;
    ht->keq = keq;

    ht->cbs.key_copy = hmap_passthrough_copy;
    ht->cbs.key_free = hmap_passthrough_destroy;
    ht->cbs.val_copy = hmap_passthrough_copy;
    ht->cbs.val_free = hmap_passthrough_destroy;

    if (cbs != NULL)
    {
        if (cbs->key_copy != NULL)
            ht->cbs.key_copy = cbs->key_copy;
        if (cbs->key_free != NULL)
            ht->cbs.key_free = cbs->key_free;
        if (cbs->val_copy != NULL)
            ht->cbs.val_copy = cbs->val_copy;
        if (cbs->val_free != NULL)
            ht->cbs.val_free = cbs->val_free;
    }

    ht->capacity = 16;
    ht->table = calloc(ht->capacity, sizeof(*ht->table));

    return ht;
}

void hmap_destroy(hmap_t *ht)
{
    key_value_t *cur;
    key_value_t *next;
    size_t i;

    if (ht == NULL)
        return;

    for (i = 0; i < ht->capacity; i++)
    {
        if (ht->table[i].key != NULL)
            continue;

        ht->cbs.key_free(ht->table[i].key);
        ht->cbs.val_free(ht->table[i].value);

        next = ht->table[i].next;
        while (next != NULL)
        {
            cur = next;
            ht->cbs.key_free(cur->key);
            ht->cbs.val_free(cur->value);
            next = cur->next;
            free(cur);
        }
    }

    free(ht->table);
    free(ht);
}

void hmap_insert(hmap_t *ht, void *key, void *val)
{
    if (ht == NULL || key == NULL)
        return;

    hmap_rehash(ht);
    hmap_add_to_bucket(ht, key, val, false);
}

void hmap_remove(hmap_t *ht, void *key)
{
    key_value_t *cur;
    key_value_t *last;
    size_t idx;

    if (ht == NULL || key == NULL)
        return;

    idx = hmap_bucket_idx(ht, key);
    if (ht->table[idx].key == NULL)
        return;

    if (ht->keq(ht->table[idx].key, key))
    {
        ht->cbs.key_free(ht->table[idx].key);
        ht->cbs.val_free(ht->table[idx].value);
        ht->table[idx].key = NULL;

        cur = ht->table[idx].next;
        if (cur != NULL)
        {
            ht->table[idx].key = cur->key;
            ht->table[idx].value = cur->value;
            ht->table[idx].next = cur->next;
            free(cur);
        }
        ht->count--;
        return;
    }

    last = ht->table + idx;
    cur = last->next;
    while (cur != NULL)
    {
        if (ht->keq(key, cur->key))
        {
            last->next = cur->next;
            ht->cbs.key_free(cur->key);
            ht->cbs.val_free(cur->value);
            free(cur);
            ht->count--;
            break;
        }
        last = cur;
        cur = cur->next;
    }
}

bool hmap_get(hmap_t *ht, void *key, void **val)
{
    key_value_t *cur;
    size_t idx;

    if (ht == NULL || key == NULL)
        return false;

    idx = hmap_bucket_idx(ht, key);
    if (ht->table[idx].key == NULL)
        return false;

    cur = ht->table + idx;
    while (cur != NULL)
    {
        if (ht->keq(key, cur->key))
        {
            if (val != NULL)
            {
                *val = cur->value;
            }
            return true;
        }
        cur = cur->next;
    }

    return false;
}

void *hmap_get_direct(hmap_t *ht, void *key)
{
    void *val = NULL;
    hmap_get(ht, key, &val);
    return val;
}

hmap_t *hmap_union(hmap_t *ht1, hmap_t *ht2)
{
    if (ht1->hash_fun != ht2->hash_fun || ht1->keq != ht2->keq)
        return NULL;

    hmap_t *ht = hmap_create(ht1->hash_fun, ht1->keq, &(ht1->cbs));

    hmap_enum_t *he = hmap_enum_create(ht1);
    hmap_enum_t *he2 = hmap_enum_create(ht2);

    void *key, *value;
    while (hmap_enum_next(he, &key, &value))
        hmap_insert(ht, key, value);

    void *key2, *value2;
    while (hmap_enum_next(he2, &key2, &value2))
    {
        if (!hmap_get_direct(ht, key2))
            hmap_insert(ht, key2, value2);
    }

    hmap_enum_destroy(he);
    hmap_enum_destroy(he2);

    return ht;
}

hmap_t *hmap_difference(hmap_t *ht1, hmap_t *ht2)
{
    if (ht1->hash_fun != ht2->hash_fun || ht1->keq != ht2->keq)
        return NULL;

    hmap_t *ht = hmap_create(ht1->hash_fun, ht1->keq, &(ht1->cbs));

    hmap_enum_t *he = hmap_enum_create(ht1);

    void *key, *value;
    while (hmap_enum_next(he, &key, &value))
    {
        if (!hmap_get_direct(ht2, key))
            hmap_insert(ht, key, value);
    }

    hmap_enum_destroy(he);

    return ht;
}

hmap_enum_t *hmap_enum_create(hmap_t *ht)
{
    hmap_enum_t *he;

    if (ht == NULL)
        return NULL;

    he = calloc(1, sizeof(*he));
    he->ht = ht;

    return he;
}

bool hmap_enum_next(hmap_enum_t *he, void **key, void **val)
{
    void *mykey;
    void *myval;

    if (he == NULL || he->idx >= he->ht->capacity)
        return false;

    if (key == NULL)
        key = &mykey;
    if (val == NULL)
        val = &myval;

    if (he->cur == NULL)
    {
        while (he->idx < he->ht->capacity && he->ht->table[he->idx].key == NULL)
        {
            he->idx++;
        }
        if (he->idx >= he->ht->capacity)
            return false;
        he->cur = he->ht->table + he->idx;
        he->idx++;
    }

    *key = he->cur->key;
    *val = he->cur->value;
    he->cur = he->cur->next;

    return true;
}

void hmap_enum_destroy(hmap_enum_t *he)
{
    if (he == NULL)
        return;
    free(he);
}
