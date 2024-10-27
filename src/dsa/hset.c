#include "dsa/hset.h"

typedef struct value
{
    void *value;
    struct value *next;
} value_t;

struct hset
{
    hset_cbs cbs;
    hset_keq keq;
    hset_hash hash_fun;

    size_t count;
    size_t capacity;
    value_t *table;
};

struct hset_enum
{
    hset_t *ht;
    size_t idx;
    value_t *cur;
};

static void *hset_passthrough_copy(void *v)
{
    return v;
}

static void hset_passthrough_destroy(void *v)
{
    (void)v;
    return;
}

static size_t hset_bucket_idx(hset_t *ht, void *value)
{
    return ht->hash_fun(value) % ht->capacity;
}

static void hset_add_to_bucket(hset_t *ht, void *value, bool is_rehash)
{
    value_t *cur;
    value_t *last;
    size_t idx;

    idx = hset_bucket_idx(ht, value);

    if (ht->table[idx].value == NULL)
    {
        if (!is_rehash)
        {
            value = ht->cbs.copy(value);
        }

        ht->table[idx].value = value;

        if (!is_rehash)
            ht->count++;
    }
    else
    {
        last = ht->table + idx;
        cur = ht->table + idx;

        do
        {
            if (ht->keq(value, cur->value))
                return; // Value already exists, do nothing

            last = cur;
            cur = cur->next;
        } while (cur != NULL);

        if (last != NULL)
        {
            cur = calloc(1, sizeof(*cur->next));
            if (!is_rehash)
            {
                value = ht->cbs.copy(value);
            }

            cur->value = value;
            last->next = cur;

            if (!is_rehash)
                ht->count++;
        }
    }
}

static void hset_rehash(hset_t *ht)
{
    value_t *table;
    value_t *cur;
    value_t *next;
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
        if (table[i].value != NULL)
            continue;

        hset_add_to_bucket(ht, table[i].value, true);
        if (table[i].next != NULL)
        {
            cur = table[i].next;
            do
            {
                hset_add_to_bucket(ht, cur->value, true);
                next = cur->next;
                free(cur);
                cur = next;
            } while (cur != NULL);
        }
    }

    free(table);
}

hset_t *hset_create(hset_hash hash_fun, hset_keq keq, hset_cbs *cbs)
{
    hset_t *ht;

    if (hash_fun == NULL || keq == NULL)
        return NULL;

    ht = calloc(1, sizeof(*ht));
    ht->hash_fun = hash_fun;
    ht->keq = keq;

    ht->cbs.copy = hset_passthrough_copy;
    ht->cbs.free = hset_passthrough_destroy;

    if (cbs != NULL)
    {
        if (cbs->copy != NULL)
            ht->cbs.copy = cbs->copy;
        if (cbs->free != NULL)
            ht->cbs.free = cbs->free;
    }

    ht->capacity = 16;

    ht->table = calloc(ht->capacity, sizeof(*ht->table));

    return ht;
}

void hset_destroy(hset_t *ht)
{
    value_t *cur;
    value_t *next;
    size_t i;

    if (ht == NULL)
        return;

    for (i = 0; i < ht->capacity; i++)
    {
        if (ht->table[i].value != NULL)
            continue;

        ht->cbs.free(ht->table[i].value);

        next = ht->table[i].next;
        while (next != NULL)
        {
            cur = next;
            ht->cbs.free(cur->value);
            next = cur->next;
            free(cur);
        }
    }

    free(ht->table);
    free(ht);
}

void hset_insert(hset_t *ht, void *value)
{
    if (ht == NULL || value == NULL)
        return;

    hset_rehash(ht);
    hset_add_to_bucket(ht, value, false);
}

void hset_remove(hset_t *ht, void *value)
{
    value_t *cur;
    value_t *last;
    size_t idx;

    if (ht == NULL || value == NULL)
        return;

    idx = hset_bucket_idx(ht, value);
    if (ht->table[idx].value == NULL)
        return;

    if (ht->keq(ht->table[idx].value, value))
    {
        ht->cbs.free(ht->table[idx].value);
        ht->table[idx].value = NULL;

        cur = ht->table[idx].next;
        if (cur != NULL)
        {
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
        if (ht->keq(value, cur->value))
        {
            last->next = cur->next;
            ht->cbs.free(cur->value);
            free(cur);
            ht->count--;
            break;
        }
        last = cur;
        cur = cur->next;
    }
}

bool hset_has(hset_t *ht, void *value)
{
    value_t *cur;
    size_t idx;

    if (ht == NULL || value == NULL)
        return false;

    idx = hset_bucket_idx(ht, value);
    if (ht->table[idx].value == NULL)
        return false;

    cur = ht->table + idx;
    while (cur != NULL)
    {
        if (ht->keq(value, cur->value))
            return true;

        cur = cur->next;
    }

    return false;
}

hset_t *hset_union(hset_t *ht1, hset_t *ht2)
{
    if (ht1->hash_fun != ht2->hash_fun || ht1->keq != ht2->keq)
        return NULL;

    hset_t *ht = hset_create(ht1->hash_fun, ht1->keq, &(ht1->cbs));

    hset_enum_t *he = hset_enum_create(ht1);
    hset_enum_t *he2 = hset_enum_create(ht2);

    void *value;
    while (hset_enum_next(he, &value))
        hset_insert(ht, value);

    void *value2;
    while (hset_enum_next(he2, &value2))
    {
        if (!hset_has(ht, value2))
            hset_insert(ht, value2);
    }

    hset_enum_destroy(he);
    hset_enum_destroy(he2);

    return ht;
}

hset_t *hset_difference(hset_t *ht1, hset_t *ht2)
{
    if (ht1->hash_fun != ht2->hash_fun || ht1->keq != ht2->keq)
        return NULL;

    hset_t *ht = hset_create(ht1->hash_fun, ht1->keq, &(ht1->cbs));

    hset_enum_t *he = hset_enum_create(ht1);

    void *value;
    while (hset_enum_next(he, &value))
    {
        if (!hset_has(ht2, value))
            hset_insert(ht, value);
    }

    hset_enum_destroy(he);

    return ht;
}

hset_enum_t *hset_enum_create(hset_t *ht)
{
    hset_enum_t *he;

    if (ht == NULL)
        return NULL;

    he = calloc(1, sizeof(*he));
    he->ht = ht;

    return he;
}

bool hset_enum_next(hset_enum_t *he, void **value)
{
    void *myvalue;

    if (he == NULL || he->idx >= he->ht->capacity)
        return false;

    if (value == NULL)
        value = &myvalue;

    if (he->cur == NULL)
    {
        while (he->idx < he->ht->capacity && he->ht->table[he->idx].value == NULL)
        {
            he->idx++;
        }
        if (he->idx >= he->ht->capacity)
            return false;
        he->cur = he->ht->table + he->idx;
        he->idx++;
    }

    *value = he->cur->value;
    he->cur = he->cur->next;

    return true;
}

void hset_enum_destroy(hset_enum_t *he)
{
    if (he == NULL)
        return;
    free(he);
}
