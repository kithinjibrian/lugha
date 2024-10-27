#ifndef AST_H
#define AST_H

#include "ptree.h"

typedef struct proc_ast
{
    PROLOGUE;
    void *(*init)(ptree_t *ptree);
    void (*exit)(void);
} proc_ast_t;

extern const proc_ast_t node_ast;

#endif /* AST_H */