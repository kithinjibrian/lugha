#ifndef TYPES_H
#define TYPES_H

#include <stdlib.h>

#include "ast.h"
#include "visitor.h"
#include "dsa/list.h"
#include "dsa/hmap.h"
#include "dsa/hset.h"
#include "dsa/array.h"
#include "parser/ptree.h"
#include "parser/error.h"

typedef node_type_t type_t;

void *type_alloc(size_t size);

#define CREATE_TYPE_CON(_ast, _var, _name)                \
    type_t *_var = type_alloc(sizeof(type_t));            \
    memcpy(&_var->base, &_ast->base, sizeof(node_ast_t)); \
    _var->tag = TYPE_CON;                                 \
    _var->con.name = type_strdup(_name);                  \
    _var->con.count = 0;

static char *fresh(int count)
{
    char *name = type_alloc(count_digits(count) + 2);
    sprintf(name, "t%d", count);
    return name;
}

#define CREATE_TYPE_VAR(_ast, _var, _var_id)              \
    type_t *_var = type_alloc(sizeof(type_t));            \
    memcpy(&_var->base, &_ast->base, sizeof(node_ast_t)); \
    _var->tag = TYPE_VAR;                                 \
    _var->var.name = fresh(_var_id++);

#endif /* TYPES_H */