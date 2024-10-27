#include "dsa/array.h"

array_t *array_create(size_t size)
{
    array_t *array = (array_t *)malloc(sizeof(array_t));

    if (array == NULL)
        return NULL;

    // Start with an initial capacity of 4 elements
    size_t initial_capacity = 4 * size;
    array->array = (uint8_t *)malloc(initial_capacity);

    if (array->array == NULL)
    {
        free(array);
        return NULL;
    }

    array->size = size;
    array->length = 4;
    array->index = 0;
    array->count = 0;

    return array;
}

bool array_push(array_t *array, void *data)
{
    if (array == NULL || data == NULL)
        return false;

    if (array->count >= array->length)
    {
        array->length *= 2;
        size_t new_length = array->length * array->size;
        array->array = (uint8_t *)realloc(array->array, new_length);

        if (array->array == NULL)
            return false;
    }

    memcpy(array->array + array->count * array->size, data, array->size);
    array->count++;

    return true;
}

bool array_insert(array_t *array, size_t index, void *data)
{
    if (array == NULL || data == NULL || index > array->count)
        return false;

    if (array->count >= array->length)
    {
        array->length *= 2;
        size_t new_length = array->length * array->size;
        array->array = (uint8_t *)realloc(array->array, new_length);

        if (array->array == NULL)
            return false;
    }

    // Move elements to create space at `pos`
    memmove(
        array->array + (index + 1) * array->size,
        array->array + index * array->size,
        (array->count - index) * array->size);

    // Insert new data at `pos`
    memcpy(array->array + index * array->size, data, array->size);
    array->count++;

    return true;
}

bool array_pop(array_t *array, void *out_data)
{
    if (array == NULL || array->count == 0)
        return false;

    // Get the data at the last element
    if (out_data != NULL)
    {
        memcpy(out_data, array->array + (array->count - 1) * array->size, array->size);
    }

    array->count--;

    // Shrink the array if it's less than 1/4 full
    if (array->count > 0 && array->count <= array->length / 4)
    {
        array->length /= 2;
        size_t new_length = array->length * array->size;
        array->array = (uint8_t *)realloc(array->array, new_length);

        if (array->array == NULL)
            return false;
    }

    return true;
}

void __array_free__(array_t **array)
{
    if (array == NULL || *array == NULL)
        return;

    free((*array)->array);
    free(*array);

    *array = NULL;
}
