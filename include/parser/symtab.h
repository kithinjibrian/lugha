#ifndef SYMTAB_H
#define SYMTAB_H

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#include "ptree.h"
#include "bison.tab.h"
#include "string/str.h"
#include "memory/arena.h"

#define HASH_SIZE 100

typedef struct ref
{
    int flags;
    YYLTYPE loc;
    char *filename;
    struct ref *next;
} ref_t;

typedef struct symbol
{
    char *name;
    enum
    {
        SYM_VARIABLE,
        SYM_FUNCTION,
        SYM_PARAMETER
    } type;
    void *data_type; /* its data type */
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
        };
    };
    int index; /* its index in the current scope */
    struct ref *ref_list;
    struct symbol *next;
} symbol_t;

typedef struct symtab
{
    char *name;
    int no_vars;
    struct symtab *prev;
    symbol_t *table[HASH_SIZE];
} symtab_t;

extern YYLTYPE yylloc;
extern char *filename_g;

void exit_scope();
void enter_scope(char *name);
symbol_t *lookup_symbol(char *name);
symbol_t *insert_symbol(char *name);

void symtab_free();

#endif /* SYMTAB_H */