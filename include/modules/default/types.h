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

typedef hmap_t subst_t;
typedef node_type_t type_t;

typedef struct scheme
{
    hset_t *set;
    type_t *type;
} scheme_t;

/**
 *  A type is denoted by τ(lowercase tau)
 *
 * τ ::=
 *   | α
 *   | C τ1... τn
 *   | τ1 -> τ2
 *
 * NOTE : The function constructor can be represented
 * by a type constructor.*/
typedef struct constraint
{
    enum
    {
        EQUALITY_CON, // τ1 ≡ τ2
        EXPLICIT_CON, // τ1 ≤M τ2
        IMPLICIT_CON  // τ ≤ σ
    } type;
    union
    {
        struct
        {
            type_t *left;
            type_t *right;
        } eq;
        struct
        {
            type_t *antecedent;
            type_t *consequent;
            hset_t *vars;
        } imp;
        struct
        {
            type_t *instance_type;
            struct scheme *scheme;
        } exp;
    };
} constraint_t;

typedef struct type_ctx
{
    array_t *constraints;
} type_ctx_t;

#define type_ctx_cast(visitor) ((struct type_ctx *)((visitor)->ctx))

#define constraints(visitor) type_ctx_cast(visitor)->constraints

static inline uint32_t tvar_hash(void *in)
{
    char *s = ((type_t *)in)->var.name;
    return str_hash(s);
}

static inline bool tvar_eq(void *a, void *b)
{
    char *s = ((type_t *)a)->var.name;
    char *d = ((type_t *)b)->var.name;

    return str_eq(s, d);
}

static inline hset_t *type_set()
{
    return hset_create(tvar_hash, tvar_eq, NULL);
}

static inline hmap_t *type_map()
{
    return hmap_create(tvar_hash, tvar_eq, NULL);
}

char *type_strdup(char *s);
void *type_alloc(size_t size);

type_t *type_dup(type_t *type);
bool type_eq(type_t *a, type_t *b);
void _type_str(type_t *type, bool nl);

#define type_str(type) _type_str(type, true)

type_t *_new_int(node_ast_t *ast);
type_t *_new_str(node_ast_t *ast);
type_t *_new_bool(node_ast_t *ast);
type_t *_new_array(node_ast_t *ast);
type_t *_new_type_var(node_ast_t *ast);
type_t *_new_fun(node_ast_t *ast, int n, type_t **types, type_t *return_type);

#define new_tint(ast) _new_int(node_ast_cast(ast))
#define new_tbool(ast) _new_bool(node_ast_cast(ast))
#define new_tstring(ast) _new_str(node_ast_cast(ast))
#define new_tarray(ast) _new_array(node_ast_cast(ast))
#define new_ttype_var(ast) _new_type_var(node_ast_cast(ast))
#define new_tfun(ast, n, types, return_type) _new_fun(node_ast_cast(ast), n, types, return_type)

subst_t *compose(subst_t *a, subst_t *b);
type_t *apply(subst_t *subst, type_t *type);

subst_t *solve(array_t *constraints);

void constraint_str(constraint_t *constraint);
void add_eq_constraint(type_t *left, type_t *right);

void scheme_str(scheme_t *scheme);
type_t *instantiate(scheme_t *scheme);
scheme_t *generalize(array_t *constraints, type_t *type);

#endif /* TYPES_H */