#include "parser/module.h"

static size_t hash(char *key)
{
    size_t hash = 2166136261u;
    while (*key)
    {
        hash ^= (unsigned char)(*key++);
        hash *= 16777619u;
    }
    return hash;
}

scope_t *new_scope(module_t *module, char *name)
{
    scope_t *scope = (scope_t *)mod_alloc(module, sizeof(scope_t));
    memset(scope->table, 0, sizeof(scope->table));

    scope->name = mod_strdup(module, name);
    scope->abs_path = get_abs_path(module, name);
    scope->no_vars = 0;
    scope->module = module;
    scope->parent = NULL;
    scope->sibling = NULL;
    scope->children = NULL;

    return scope;
}

void scope_enter(module_t *module, char *name)
{
    scope_t *scope = new_scope(module, name);
    scope->parent = module->scope;

    if (module->scope != NULL)
    {
        if (module->scope->children == NULL)
        {
            module->scope->children = scope;
        }
        else
        {
            scope_t *sibling = module->scope->children;
            while (sibling->sibling != NULL)
            {
                sibling = sibling->sibling;
            }
            sibling->sibling = scope;
        }
    }

    module->scope = scope;
}

void scope_exit(module_t *module)
{
    module->scope = module->scope->parent;
}

symbol_t *new_symbol(module_t *module, char *name)
{
    symbol_t *symbol = (symbol_t *)mod_alloc(module, sizeof(symbol_t));
    symbol->name = mod_strdup(module, name);
    symbol->module = module;
    symbol->next = NULL;
    symbol->tag = SYM_VARIABLE;
    symbol->access = ACCESS_PRIVATE;

    symbol->abs_path = get_abs_path(module, name);

    return symbol;
}

symbol_t *symbol_insert(module_t *module, char *name)
{
    symbol_t *symbol = new_symbol(module, name);

    size_t h = hash(name) % HASH_SIZE;

    if (module->scope->table[h] == NULL)
    {
        module->scope->table[h] = symbol;
    }
    else
    {
        symbol_t *tmp = module->scope->table[h];

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

static symbol_t *_get_symbol(scope_t *scope, char *name)
{
    size_t h = hash(name) % HASH_SIZE;
    symbol_t *symbol = scope->table[h];

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

symbol_t *get_symbol(module_t *module, char *name)
{
    (void)name;

    if (module->scope == NULL)
        return NULL;

    scope_t *current = module->scope;
    while (current != NULL)
    {
        symbol_t *symbol = _get_symbol(current, name);
        if (symbol)
        {
            return symbol;
        }
        current = current->parent;
    }

    return NULL;
}

void _scope_print(scope_t *scope, int level)
{
    if (!scope)
        return;

    for (int i = 0; i < level; i++)
    {
        printf("  ");
    }

    printf("scope (%s, %s)\n", scope->name, scope->abs_path->symbol_path);

    for (int i = 0; i < HASH_SIZE; i++)
    {
        if (scope->table[i] != NULL)
        {
            symbol_t *s = scope->table[i];

            while (s != NULL)
            {
                for (int i = 0; i < level; i++)
                {
                    printf("  ");
                }
                printf("-> %s, %s\n", s->name, s->abs_path->symbol_path);
                s = s->next;
            }
        }
    }

    scope_t *child = scope->children;
    while (child != NULL)
    {
        _scope_print(child, level + 1);
        child = child->sibling;
    }
}

void scope_print(module_t *module)
{
    _scope_print(module->scope_root, 0);
}