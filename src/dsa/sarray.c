#include "dsa/sarray.h"

int sarray_push(sarray_t *arr, void *element, size_t elem_size)
{
    // Check if there is enough space; if not, resize the buffer
    if (arr->used_size + elem_size > arr->total_size)
    {
        uint32_t new_size = arr->total_size * 2 + elem_size;
        uint8_t *new_data = (uint8_t *)realloc(arr->data, new_size);
        if (!new_data)
            return -1; // Error handling for realloc failure

        arr->data = new_data;
        arr->total_size = new_size;
    }

    // Copy the element into the next available spot in the buffer
    memcpy(arr->data + arr->used_size, element, elem_size);
    arr->used_size += elem_size;
    arr->len += 1;

    return 0;
}