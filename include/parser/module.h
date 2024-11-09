#ifndef MODULE_H
#define MODULE_H

#include "dsa/hmap.h"
#include "file/path.h"
#include "parser/ast.h"
#include "dsa/sarray.h"
#include "string/str.h"
#include "memory/arena.h"

#define HASH_SIZE 100

extern FILE *yyin;
extern int yyparse();

typedef struct node_type node_type_t;

typedef struct abs_path
{
    char **segments;
    size_t no_segments;
    char *symbol_path;
} abs_path_t;

typedef struct symbol
{
    char *name;
    abs_path_t *abs_path;
    int index; /* its index in the current scope */
    bool is_exported;
    enum
    {
        SYM_STRUCT,
        SYM_VARIABLE,
        SYM_FUNCTION,
        SYM_PARAMETER,
        SYM_MODULE
    } tag;
    enum
    {
        ACCESS_PUBLIC,
        ACCESS_PRIVATE
    } access;
    node_type_t *type;     /* its data type */
    struct module *module; // modules
    union
    {
        struct // functions
        {
            int no_params; /* number of parameters if it is a function */
        };
        struct // variables
        {
            bool init;     /* is it initialized */
            bool used;     /* is it used */
            bool is_const; /* is it a constant */
            bool is_this;  /* is it a 'this' */
        };
    };
    struct ref *ref_list;
    struct symbol *next;
} symbol_t;

typedef struct scope
{
    char *name;
    abs_path_t *abs_path;
    int no_vars;
    struct module *module;
    struct scope *parent;
    struct scope *children;
    struct scope *sibling;
    symbol_t *table[HASH_SIZE];
} scope_t;

typedef struct module
{
    char *name;
    bool is_dir;
    hmap_t *api;
    bool is_inline;
    abs_path_t *abs_path;
    struct module *parent;
    struct module *children;
    struct module *sibling;
    scope_t *scope_root, *scope;
    struct
    {
        void *ctx;
        path_t *path;
        ptree_t *ptree;
        sarray_t *buffers[10];
        memory_arena_t arena;
    };
} module_t;

char *mod_strdup(module_t *module, char *str);
void *mod_alloc(module_t *module, size_t size);

static inline void symbol_path(module_t *module, abs_path_t *abs_path)
{
    size_t len = 0;
    for (size_t i = 0; i < abs_path->no_segments; i++)
    {
        len += strlen(abs_path->segments[i]);
    }

    len += (abs_path->no_segments - 1) * 2;

    abs_path->symbol_path = (char *)mod_alloc(module, len + 1);

    for (size_t i = 0; i < abs_path->no_segments; i++)
    {
        strcat(abs_path->symbol_path, abs_path->segments[i]);
        if (i < abs_path->no_segments - 1)
            strcat(abs_path->symbol_path, "::");
    }
}

/*** */
void print_mod();
int module_parse(path_t *path);

symbol_t *symbol_insert(module_t *module, char *name);
symbol_t *symbol_lookup(module_t *module, abs_path_t *path);

#define module_leave(module) _module_leave(&(module))

void _module_leave(module_t **module);
module_t *module_enter(module_t *current, char *name);

///////
void scope_exit(module_t *module);
scope_t *new_scope(module_t *module, char *name);
void scope_enter(module_t *module, char *name);

void scope_print(module_t *module);
void _scope_print(scope_t *scope, int level);

symbol_t *get_symbol(module_t *module, char *name);

abs_path_t *get_abs_path(module_t *module, char *name);
abs_path_t *abs_path_dup(module_t *module, abs_path_t *path);

symbol_t *symbol_dup(module_t *module, symbol_t *symbol);

symbol_t *symbol_export(module_t *module, symbol_t *symbol);

module_t *get_module_child(module_t *module, char *name);

path_t *open_module(module_t *module, char *name);

#endif /* MODULE_H */