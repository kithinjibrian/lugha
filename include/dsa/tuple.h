#ifndef PARRAY_H
#define PARRAY_H

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define ENTRY(tag, data, size) (&(entry_t){tag, data, size})

typedef struct _entry
{
#define N_STRING 0
#define N_INTEGER 1
#define N_POINTER 2
    int tag;
    void *data;
    size_t size;
} entry_t;

static inline entry_t *entry_data(int tag, void *data, size_t size)
{
    entry_t *entry = (entry_t *)malloc(sizeof(entry_t));
    entry->tag = tag;
    entry->size = size;

    entry->data = malloc(size);
    memcpy(entry->data, data, size);

    return entry;
}

typedef struct tuple
{
    size_t size;
    size_t count;
    size_t index;
    entry_t **tuple;
} tuple_t;

tuple_t *new_tuple(size_t size);
void tuple_free(tuple_t *tuple);
void tuple_clear(tuple_t *tuple);
entry_t *tuple_pop(tuple_t *tuple);
void tuple_reverse(tuple_t *tuple);
entry_t *tuple_shift(tuple_t *tuple);
size_t tuple_length(const tuple_t *tuple);
tuple_t *tuple_clone(const tuple_t *tuple);
void tuple_push(tuple_t *tuple, entry_t *entry);
void tuple_delete(tuple_t *tuple, size_t index);
void tuple_unshift(tuple_t *tuple, entry_t *entry);
void tuple_resize(tuple_t *tuple, size_t new_size);
entry_t *tuple_at(const tuple_t *tuple, size_t index);
void tuple_swap(tuple_t *tuple, size_t index1, size_t index2);
void tuple_replace(tuple_t *tuple, size_t index, entry_t *entry);
void tuple_for_each(const tuple_t *tuple, void (*action)(entry_t *));
tuple_t *tuple_filter(const tuple_t *tuple, int (*filter)(entry_t *));
tuple_t *tuple_find_all(const tuple_t *tuple, int (*match)(entry_t *));
int tuple_contains(const tuple_t *tuple, const void *value, size_t size);
entry_t *tuple_find_first(const tuple_t *tuple, int (*match)(entry_t *));
tuple_t *tuple_map(const tuple_t *tuple, entry_t *(*transform)(entry_t *));
entry_t *tuple_find(const tuple_t *tuple, const void *value, size_t size);
tuple_t *tuple_clone(const tuple_t *tuple);

char *entry_to_string(const entry_t *datum);
char *tuple_stringify(
    const tuple_t *tuple,
    const char *delim,
    char *(*stringify)(const entry_t *));

#endif /* PARRAY_H */