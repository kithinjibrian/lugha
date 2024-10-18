#include "string/str.h"

char *strdup_a(memory_arena_t *arena, char *s)
{
    size_t len = strlen(s);
    char *str = arena_alloc(arena, len + 1);
    memcpy(str, s, len + 1);
    return str;
}