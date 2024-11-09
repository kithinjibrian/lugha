#ifndef TYPES_H
#define TYPES_H

#include <stdlib.h>

#include "ast.h"
#include "macro.h"
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

typedef struct type_class
{
    char *name;
} type_class_t;

typedef struct constraint
{
    enum
    {
        EQUALITY_CON, // τ1 ≡ τ2
        EXPLICIT_CON, // τ1 ≤M τ2
        IMPLICIT_CON, // τ ≤ σ
        TYPECLASS_CON
    } type;
    union
    {
        struct
        {
            type_t *type;
            type_class_t *type_class;
        } tc;
        struct
        {
            type_t *left;
            type_t *right;
        } eq;
        struct
        {
            type_t *antecedent;
            type_t *consequent;
            array_t *M;
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
    array_t *M; // monomorphic set
    array_t *constraints;
} type_ctx_t;

#define type_ctx_cast(visitor) ((struct type_ctx *)((visitor)->ctx))

#define M(visitor) type_ctx_cast(visitor)->M
#define constraints(visitor) type_ctx_cast(visitor)->constraints

static inline void m_add(type_t *tvar)
{
    if (M(&type_visitor) == NULL)
        M(&type_visitor) = new_array(type_t);

    array_push(M(&type_visitor), tvar);
}

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

bool type_eq(type_t *a, type_t *b);
void _type_str(type_t *type, bool nl);
type_t *type_dup(module_t *module, type_t *type);

#define type_str(type) _type_str(type, true)

type_t *_new_int(module_t *module, node_ast_t *ast);
type_t *_new_str(module_t *module, node_ast_t *ast);
type_t *_new_bool(module_t *module, node_ast_t *ast);
type_t *_new_array(module_t *module, node_ast_t *ast);
type_t *_new_type_var(module_t *module, node_ast_t *ast);
type_t *_new_trec(module_t *module, node_ast_t *ast, int size, char *name);
type_t *_new_adt(module_t *module, node_ast_t *ast, char *name, int n, type_t **types);
type_t *_new_fun(module_t *module, node_ast_t *ast, int n, type_t **types, type_t *return_type);

#define new_tint(module, ast) _new_int(module, node_ast_cast(ast))
#define new_tbool(module, ast) _new_bool(module, node_ast_cast(ast))
#define new_tstring(module, ast) _new_str(module, node_ast_cast(ast))
#define new_tarray(module, ast) _new_array(module, node_ast_cast(ast))
#define new_ttype_var(module, ast) _new_type_var(module, node_ast_cast(ast))
#define new_trec(module, ast, size, name) _new_trec(module, node_ast_cast(ast), size, name)
#define new_tfun(module, ast, n, types, return_type) _new_fun(module, node_ast_cast(ast), n, types, return_type)

void trec_add(module_t *module, type_t *type, char *label, type_t *value);
type_t *trec_get(type_t *type, char *label);

subst_t *compose(module_t *module, subst_t *a, subst_t *b);
type_t *apply(module_t *module, subst_t *subst, type_t *type);

subst_t *solve(module_t *module, array_t *cnst);
subst_t *unify(module_t *module, type_t *a, type_t *b);

void constraint_str(constraint_t *constraint);
void add_eq_constraint(type_t *left, type_t *right);
void add_tc_constraint(type_class_t *tc, type_t *type);
void add_imp_constraint(type_t *antecedent, type_t *consequent, array_t *M);

void scheme_str(scheme_t *scheme);
type_t *instantiate(scheme_t *scheme);
scheme_t *generalize(array_t *constraints, type_t *type);

hset_t *tvs_type(type_t *type);

#endif /* TYPES_H */