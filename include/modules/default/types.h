#ifndef TYPES_H
#define TYPES_H

#include <stdlib.h>

#include "ast.h"
#include "visitor.h"
#include "dsa/list.h"
#include "parser/ptree.h"
#include "parser/error.h"

typedef node_type_t type_t;

#define CREATE_TYPE(_var, _type)                         \
    type_t *_var = type_alloc(sizeof(type_t));           \
    memcpy(&_var->base, &ast->base, sizeof(node_ast_t)); \
    _var->data_type = _type;

#endif /* TYPES_H */