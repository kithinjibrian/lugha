#ifndef BITMAP_H
#define BITMAP_H

#include <stdint.h>

/**
 * Set bit at index in bitmap of size 8 bits
 * @param bitmap bitmap
 * @param index index in bitmap
 */
static inline void bit_set_v8(uint8_t bitmap[], int index)
{
    bitmap[index / 8] |= (1 << (index % 8));
}

/**
 * Get bit at index in bitmap of size 8 bits
 * @param bitmap bitmap
 * @param index index in bitmap
 *
 * @return the bit at index
 */
static inline int bit_get_v8(uint8_t bitmap[], int index)
{
    return (bitmap[index / 8] >> (index % 8)) & 1;
}

/**
 * Clear bit at index in bitmap of size 8 bits
 * @param bitmap bitmap
 * @param index index in bitmap
 */
static inline void bit_clear_v8(uint8_t bitmap[], int index)
{
    bitmap[index / 8] &= ~(1 << (index % 8));
}

/**
 * Flip bit at index in bitmap of size 8 bits
 * @param bitmap bitmap
 * @param index index in bitmap
 */
static inline void bit_flip_v8(uint8_t bitmap[], int index)
{
    bitmap[index / 8] ^= (1 << (index % 8));
}

/**
 * Set bit at index in bitmap of size 32 bits
 * @param bitmap bitmap
 * @param index index in bitmap
 */
static inline void bit_set_v32(uint32_t bitmap[], int index)
{
    bitmap[index / 32] |= (1 << (index % 32));
}

/**
 * Get bit at index in bitmap of size 32 bits
 * @param bitmap bitmap
 * @param index index in bitmap
 *
 * @return the bit at index
 */
static inline int bit_get_v32(uint32_t bitmap[], int index)
{
    return (bitmap[index / 32] >> (index % 32)) & 1;
}

/**
 * Clear bit at index in bitmap of size 32 bits
 * @param bitmap bitmap
 * @param index index in bitmap
 */
static inline void bit_clear_v32(uint32_t bitmap[], int index)
{
    bitmap[index / 32] &= ~(1 << (index % 32));
}

/**
 * Flip bit at index in bitmap of size 32 bits
 * @param bitmap bitmap
 * @param index index in bitmap
 */
static inline void bit_flip_v32(uint32_t bitmap[], int index)
{
    bitmap[index / 32] ^= (1 << (index % 32));
}

#endif