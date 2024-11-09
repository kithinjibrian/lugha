#ifndef AST_H
#define AST_H

#include "macro.h"
#include "ptree.h"
#include "parser/module.h"

typedef struct module module_t;

typedef struct proc_ast
{
    PROLOGUE;
    void (*exit)(void);
    void *(*init)(module_t *module, ptree_t *ptree);
} proc_ast_t;

extern const proc_ast_t node_ast;

#endif /* AST_H */