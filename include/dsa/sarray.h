#ifndef SLIDE_ARRAY_H
#define SLIDE_ARRAY_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct sarray
{
    uint32_t total_size; // Total size of the allocated buffer
    uint32_t used_size;  // Amount of buffer currently used
    uint32_t len;        // Number of elements stored
    uint8_t *data;       // Pointer to the contiguous data buffer
} sarray_t;

static inline sarray_t *sarray_init(uint32_t initial_size)
{
    sarray_t *arr = (sarray_t *)malloc(sizeof(sarray_t));
    if (!arr)
        return NULL; // Error handling

    arr->total_size = initial_size;
    arr->used_size = 0;
    arr->len = 0;
    arr->data = (uint8_t *)malloc(initial_size);
    if (!arr->data)
    {
        free(arr);
        return NULL;
    }

    memset(arr->data, 0, initial_size);

    return arr;
}

static inline void sarray_free(sarray_t *arr)
{
    if (arr)
    {
        free(arr->data);
        free(arr);
    }
}

int sarray_push(sarray_t *arr, void *element, size_t elem_size);

#endif /* SLIDE_ARRAY_H */