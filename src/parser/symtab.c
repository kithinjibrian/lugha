#include "parser/symtab.h"

symtab_t *symtab = NULL;
memory_arena_t symtab_arena = {NULL};

DEFINE_FREE(symtab_free, &symtab_arena);
DEFINE_ALLOC(symtab_alloc, &symtab_arena);
DEFINE_STRDUP(symtab_strdup, &symtab_arena);

size_t hash(char *key)
{
    size_t hash = 2166136261u;
    while (*key)
    {
        hash ^= (unsigned char)(*key++);
        hash *= 16777619u;
    }
    return hash;
}

static inline symtab_t *create_symtab()
{
    symtab_t *new_symtab = (symtab_t *)symtab_alloc(sizeof(symtab_t));
    memset(new_symtab->table, 0, sizeof(new_symtab->table));

    new_symtab->prev = symtab;
    new_symtab->no_vars = 0;

    symtab = new_symtab;

    return new_symtab;
}

void enter_scope(char *name)
{
    symtab_t *new_symtab = create_symtab();
    new_symtab->name = name;
}

void exit_scope()
{
    symtab = symtab->prev;
}

ref_t *create_ref()
{
    ref_t *ref = (ref_t *)symtab_alloc(sizeof(ref_t));
    set_loc(ref);
    ref->flags = 0;
    ref->filename = symtab_strdup(filename_g);
    ref->next = NULL;
    return ref;
}

void insert_ref(symbol_t *symbol)
{
    ref_t *ref = create_ref();

    ref_t *tmp = symbol->ref_list;
    while (tmp->next)
    {
        tmp = tmp->next;
    }

    tmp->next = ref;
}

symbol_t *create_symbol(char *name, void *data_type)
{
    symbol_t *symbol = (symbol_t *)symtab_alloc(sizeof(symbol_t));
    symbol->ref_list = create_ref();
    symbol->name = symtab_strdup(name);
    symbol->data_type = data_type;
    symbol->scheme = NULL;
    symbol->type = SYM_VARIABLE;
    symbol->no_params = 0;
    symbol->used = false;
    symbol->init = false;
    symbol->is_const = false;
    symbol->index = symtab->no_vars++;
    symbol->symtab = symtab;

    return symbol;
}

symbol_t *insert_symbol(char *name)
{
    symbol_t *symbol = create_symbol(name, NULL);

    size_t h = hash(name) % HASH_SIZE;

    if (symtab->table[h] == NULL)
    {
        symtab->table[h] = symbol;
    }
    else
    {
        symbol_t *tmp = symtab->table[h];
        while (tmp->next)
        {
            if (strcmp(tmp->name, name) == 0)
                return NULL;

            tmp = tmp->next;
        }

        if (strcmp(tmp->name, name) == 0)
            return NULL;

        tmp->next = symbol;
    }

    return symbol;
}

static symbol_t *_lookup_symbol(symtab_t *symtab, char *name)
{
    size_t h = hash(name) % HASH_SIZE;
    symbol_t *symbol = symtab->table[h];

    while (symbol)
    {

        if (strcmp(symbol->name, name) == 0)
        {
            return symbol;
        }
        symbol = symbol->next;
    }
    return NULL;
}

symbol_t *lookup_symbol(char *name)
{
    symtab_t *current = symtab;

    while (current)
    {
        symbol_t *symbol = _lookup_symbol(current, name);
        if (symbol)
        {
            insert_ref(symbol);
            return symbol;
        }
        current = current->prev;
    }
    return NULL;
}