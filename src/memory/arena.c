#include "memory/arena.h"

typedef struct memory_block
{
    struct memory_block *next;
    uint8_t *memory;
    uint32_t used;
    uint32_t size;
} memory_block_t;

memory_block_t *new_block(uint32_t size)
{
    memory_block_t *block = (memory_block_t *)malloc(sizeof(memory_block_t));
    block->next = NULL;
    block->memory = (uint8_t *)calloc(1, size);
    block->used = 0;
    block->size = size;
    return block;
}

void arena_init(memory_arena_t *arena, uint32_t size)
{
    if (size < BLOCK_SIZE)
    {
        size = BLOCK_SIZE;
    }

    arena->head = new_block(size);
}

void *arena_alloc(memory_arena_t *arena, uint32_t size)
{
    memory_block_t *current = arena->head;

    // Find a block with enough free space
    while (current)
    {
        if (current->used + size <= current->size)
        {
            // Allocate memory from this block
            void *result = current->memory + current->used;
            current->used += size;
            return result;
        }
        // Move to the next block if there's not enough space in this one
        if (!current->next)
            break;
        current = current->next;
    }

    // No block has enough space, so allocate a new block
    size_t block_size = size > BLOCK_SIZE ? size : BLOCK_SIZE;
    memory_block_t *new_blk = new_block(block_size);

    // Link the new block into the region and allocate memory from it
    current->next = new_blk;
    void *result = new_blk->memory;
    new_blk->used = size;

    return result;
}

void arena_reset(memory_arena_t *arena)
{
    memory_block_t *current = arena->head;
    while (current)
    {
        current->used = 0; // Reset all memory blocks to 0 usage
        current = current->next;
    }
}

void arena_free(memory_arena_t *arena)
{
    memory_block_t *current = arena->head;
    while (current)
    {
        memory_block_t *next = current->next;
        free(current->memory); // Free the memory block
        free(current);         // Free the block structure
        current = next;
    }
    arena->head = NULL;
}