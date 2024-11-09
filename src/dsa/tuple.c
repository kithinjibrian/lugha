#include "dsa/tuple.h"

tuple_t *new_tuple(size_t size)
{
    tuple_t *tuple = (tuple_t *)malloc(sizeof(tuple_t));
    tuple->size = size;
    tuple->count = 0;
    tuple->index = 0;
    tuple->tuple = (entry_t **)malloc(sizeof(entry_t *) * size);

    memset(tuple->tuple, 0, sizeof(entry_t *) * size);

    return tuple;
}

void tuple_push(tuple_t *tuple, entry_t *entry)
{
    if (tuple == NULL || entry == NULL)
        return;

    if (tuple->count >= tuple->size)
    {
        tuple->size *= 2;
        tuple->tuple = (entry_t **)realloc(tuple->tuple, sizeof(entry_t *) * tuple->size);
    }

    tuple->tuple[tuple->count++] = entry;
}

entry_t *tuple_pop(tuple_t *tuple)
{
    if (tuple == NULL || tuple->count == 0)
        return NULL;

    entry_t *entry = tuple->tuple[--tuple->count];

    if (entry->data != NULL)
        free(entry->data);

    return entry;
}

void tuple_replace(tuple_t *tuple, size_t index, entry_t *entry)
{
    if (tuple == NULL || entry == NULL || index >= tuple->count)
        return;

    entry_t *old_datum = tuple->tuple[index];
    if (old_datum->data != NULL)
    {
        free(old_datum->data);
    }
    free(old_datum);

    tuple->tuple[index] = entry;
}

entry_t *tuple_shift(tuple_t *tuple)
{
    if (tuple == NULL || tuple->count == 0)
        return NULL;

    entry_t *entry = tuple->tuple[0];

    for (size_t i = 1; i < tuple->count; ++i)
    {
        tuple->tuple[i - 1] = tuple->tuple[i];
    }

    --tuple->count;

    if (entry->data != NULL)
        free(entry->data);

    return entry;
}

void tuple_unshift(tuple_t *tuple, entry_t *entry)
{
    if (tuple == NULL || entry == NULL)
        return;

    if (tuple->count >= tuple->size)
    {
        tuple->size = (tuple->size == 0) ? 2 : tuple->size * 2;
        tuple->tuple = (entry_t **)realloc(tuple->tuple, sizeof(entry_t *) * tuple->size);

        if (tuple->tuple == NULL)
        {
            perror("Failed to resize tuple array");
            exit(EXIT_FAILURE);
        }
    }

    for (size_t i = tuple->count; i > 0; --i)
    {
        tuple->tuple[i] = tuple->tuple[i - 1];
    }

    tuple->tuple[0] = entry;

    tuple->count++;
}

void tuple_delete(tuple_t *tuple, size_t index)
{
    if (tuple == NULL || index >= tuple->count)
        return;

    entry_t *entry = tuple->tuple[index];
    if (entry->data != NULL)
    {
        free(entry->data);
    }
    free(entry);

    for (size_t i = index + 1; i < tuple->count; ++i)
    {
        tuple->tuple[i - 1] = tuple->tuple[i];
    }

    --tuple->count;
}

void tuple_resize(tuple_t *tuple, size_t new_size)
{
    if (tuple == NULL || new_size < tuple->count)
        return;

    tuple->size = new_size;
    tuple->tuple = (entry_t **)realloc(tuple->tuple, sizeof(entry_t *) * tuple->size);

    if (tuple->tuple == NULL)
    {
        perror("Failed to resize tuple array");
        exit(EXIT_FAILURE);
    }
}

entry_t *tuple_at(const tuple_t *tuple, size_t index)
{
    if (tuple == NULL || index >= tuple->count)
        return NULL;

    return tuple->tuple[index];
}

size_t tuple_length(const tuple_t *tuple)
{
    return tuple->count;
}

entry_t *tuple_find(const tuple_t *tuple, const void *value, size_t size)
{
    if (tuple == NULL || value == NULL)
        return NULL;

    for (size_t i = 0; i < tuple->count; ++i)
    {
        entry_t *entry = tuple->tuple[i];
        if (entry->size == size && memcmp(entry->data, value, size) == 0)
        {
            return entry;
        }
    }
    return NULL;
}

int tuple_contains(const tuple_t *tuple, const void *value, size_t size)
{
    return tuple_find(tuple, value, size) != NULL;
}

void tuple_swap(tuple_t *tuple, size_t index1, size_t index2)
{
    if (tuple == NULL || index1 >= tuple->count || index2 >= tuple->count)
        return;

    entry_t *temp = tuple->tuple[index1];
    tuple->tuple[index1] = tuple->tuple[index2];
    tuple->tuple[index2] = temp;
}

void tuple_reverse(tuple_t *tuple)
{
    if (tuple == NULL)
        return;

    size_t left = 0, right = tuple->count - 1;
    while (left < right)
    {
        tuple_swap(tuple, left, right);
        ++left;
        --right;
    }
}

void tuple_for_each(const tuple_t *tuple, void (*action)(entry_t *))
{
    if (tuple == NULL || action == NULL)
        return;
    for (size_t i = 0; i < tuple->count; ++i)
    {
        action(tuple->tuple[i]);
    }
}

entry_t *tuple_find_first(const tuple_t *tuple, int (*match)(entry_t *))
{
    if (tuple == NULL || match == NULL)
        return NULL;
    for (size_t i = 0; i < tuple->count; ++i)
    {
        if (match(tuple->tuple[i]))
        {
            return tuple->tuple[i];
        }
    }
    return NULL;
}

tuple_t *tuple_find_all(const tuple_t *tuple, int (*match)(entry_t *))
{
    if (tuple == NULL || match == NULL)
        return NULL;

    tuple_t *result = malloc(sizeof(tuple_t));
    result->size = tuple->size;
    result->count = 0;
    result->index = 0;
    result->tuple = malloc(sizeof(entry_t *) * tuple->size);

    for (size_t i = 0; i < tuple->count; ++i)
    {
        if (match(tuple->tuple[i]))
        {
            tuple_push(result, tuple->tuple[i]);
        }
    }

    return result;
}

tuple_t *tuple_map(const tuple_t *tuple, entry_t *(*transform)(entry_t *))
{
    if (tuple == NULL || transform == NULL)
        return NULL;

    tuple_t *result = malloc(sizeof(tuple_t));
    result->size = tuple->size;
    result->count = 0;
    result->index = 0;
    result->tuple = malloc(sizeof(entry_t *) * tuple->size);

    for (size_t i = 0; i < tuple->count; ++i)
    {
        entry_t *new_datum = transform(tuple->tuple[i]);
        tuple_push(result, new_datum);
    }

    return result;
}

tuple_t *tuple_filter(const tuple_t *tuple, int (*filter)(entry_t *))
{
    if (tuple == NULL || filter == NULL)
        return NULL;

    tuple_t *result = malloc(sizeof(tuple_t));
    result->size = tuple->size;
    result->count = 0;
    result->index = 0;
    result->tuple = malloc(sizeof(entry_t *) * tuple->size);

    for (size_t i = 0; i < tuple->count; ++i)
    {
        if (filter(tuple->tuple[i]))
        {
            tuple_push(result, tuple->tuple[i]);
        }
    }

    return result;
}

tuple_t *tuple_clone(const tuple_t *tuple)
{
    if (tuple == NULL)
        return NULL;

    tuple_t *new_tuple = malloc(sizeof(tuple_t));
    new_tuple->size = tuple->size;
    new_tuple->count = tuple->count;
    new_tuple->index = tuple->index;
    new_tuple->tuple = malloc(sizeof(entry_t *) * new_tuple->size);

    // Clone each datum
    for (size_t i = 0; i < tuple->count; ++i)
    {
        entry_t *original = tuple->tuple[i];
        entry_t *new_datum = malloc(sizeof(entry_t));

        new_datum->tag = original->tag;
        new_datum->size = original->size;
        new_datum->data = malloc(original->size);

        memcpy(new_datum->data, original->data, original->size);
        new_tuple->tuple[i] = new_datum;
    }

    return new_tuple;
}

char *entry_to_string(const entry_t *datum)
{
    if (datum == NULL)
        return strdup("null");

    char buffer[128];
    switch (datum->tag)
    {
    case N_INTEGER:
        snprintf(buffer, sizeof(buffer), "%d", *((int *)datum->data));
        break;
    case N_STRING:
        snprintf(buffer, sizeof(buffer), "%s", (char *)datum->data);
        break;
    default:
        return NULL;
    }

    return strdup(buffer);
}

char *tuple_stringify(
    const tuple_t *tuple,
    const char *delim,
    char *(*stringify)(const entry_t *))
{
    if (tuple == NULL)
        return strdup("null");

    if (tuple->count == 0)
        return strdup("[]");

    size_t buffer_size = 256;
    char *result = malloc(buffer_size);
    result[0] = '\0';

    for (size_t i = 0; i < tuple->count; ++i)
    {
        char *entry_str = stringify(tuple->tuple[i]);
        if (entry_str == NULL)
            entry_str = strdup("null");

        size_t needed_size = strlen(result) + strlen(entry_str) + strlen(delim) + 3;
        if (needed_size > buffer_size)
        {
            buffer_size *= 2;
            result = realloc(result, buffer_size);
        }

        strcat(result, entry_str);
        free(entry_str);

        if (i < tuple->count - 1)
        {
            strcat(result, delim);
        }
    }

    return result;
}

void tuple_free(tuple_t *tuple)
{
    if (!tuple)
        return;

    for (size_t i = 0; i < tuple->count; ++i)
    {
        entry_t *entry = tuple->tuple[i];

        if (entry->data != NULL)
        {
            free(entry->data);
        }

        free(entry);
    }

    free(tuple->tuple);
    free(tuple);
}