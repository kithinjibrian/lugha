#ifndef ARENA_H
#define ARENA_H

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#define DEFINE_ALLOC(_name, _arena)         \
    void *_name(size_t size)                \
    {                                       \
        if (!(_arena)->head)                \
            arena_init((_arena), 1024);     \
        return arena_alloc((_arena), size); \
    }

#define DEFINE_FREE(_name, _arena) \
    void _name()                   \
    {                              \
        arena_free(_arena);        \
        (_arena)->head = NULL;     \
    }

#define BLOCK_SIZE 1024

struct memory_block;

typedef struct memory_arena
{
    struct memory_block *head;
} memory_arena_t;

void arena_init(memory_arena_t *arena, uint32_t size);
void *arena_alloc(memory_arena_t *arena, uint32_t size);
void arena_reset(memory_arena_t *arena);
void arena_free(memory_arena_t *arena);

#endif /* ARENA_H */