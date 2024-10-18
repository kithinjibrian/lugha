#ifndef STRING_H
#define STRING_H

#include <string.h>
#include "memory/arena.h"

#define DEFINE_STRDUP(_name, _arena) \
    char *_name(char *s)             \
    {                                \
        return strdup_a(_arena, s);  \
    }

char *strdup_a(memory_arena_t *arena, char *s);

#endif /* STRING_H */