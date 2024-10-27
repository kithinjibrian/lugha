#ifndef ARRAY_H
#define ARRAY_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct array
{

    size_t size;    // size of each element
    size_t count;   // number of elements
    size_t index;   // index for tracking the current element
    size_t length;  // length of the array
    uint8_t *array; // array buffer
} array_t;

#define new_array(T) array_create(sizeof(T));

array_t *array_create(size_t size);

bool array_push(array_t *array, void *data);

void __array_free__(array_t **array);

#define array_free(array) __array_free__(&array);

static inline void array_free2(array_t *array)
{
    if (array == NULL)
        return;

    free(array->array);
    free(array);
}

static inline void *array_at(array_t *array, size_t pos)
{
    if (array == NULL || pos >= array->length)
        return NULL;

    return (void *)(array->array + pos * array->size);
}

static inline bool array_set(array_t *array, size_t index, void *data)
{
    if (array == NULL || data == NULL || index >= array->count)
        return false;

    memcpy(array->array + index * array->size, data, array->size);

    return true;
}

static inline void *array_current(array_t *array)
{
    return (void *)(array->array + array->index * array->size);
}

static inline bool array_next(array_t *array)
{
    if (array->index > array->length - 1)
    {
        return false;
    }

    array->index++;

    return true;
}

static inline bool array_prev(array_t *array)
{
    if (array->index == 0)
        return false;

    array->index--;

    return true;
}

static inline void array_rewind(array_t *array)
{
    array->index = 0;
}

#define array_for_each(pos, array) \
    for (pos = (array)->index; pos < (array)->count; pos++, array_next(array))

#endif