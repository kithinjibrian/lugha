#include "modules/default/types.h"

size_t type_var_id = 0;
memory_arena_t type_arena_g = {NULL};

#define type_ctx_cast(visitor) ((type_ctx_t *)((visitor)->ctx))

#define constraints(visitor) type_ctx_cast(visitor)->constraints

DEFINE_ALLOC(type_alloc, &type_arena_g);
DEFINE_STRDUP(type_strdup, &type_arena_g);

typedef struct constraint
{
    type_t *left;
    type_t *right;
} constraint_t;

typedef struct scheme
{
    hset_t *set;
    type_t *type;
} scheme_t;

typedef struct d_value
{
    enum
    {
        TTYPE,
        TSCHEME,
        TCONSTRAINT,
    } tag;
    union
    {
        type_t *type;
        scheme_t *scheme;
        constraint_t *constraint;
    };
} d_value_t;

typedef struct type_ctx
{
    array_t *constraints;
} type_ctx_t;

static inline uint32_t tvar_hash(void *in)
{
    tvar_t *s = (tvar_t *)in;
    return str_hash(s->name);
}

static inline bool tvar_eq(void *a, void *b)
{
    tvar_t *s = (tvar_t *)a;
    tvar_t *d = (tvar_t *)b;

    return str_eq(s->name, d->name);
}

static inline hset_t *type_set()
{
    return hset_create(tvar_hash, tvar_eq, NULL);
}

static hset_t *tvs_type(type_t *type)
{
    if (type->tag == TYPE_VAR)
    {
        hset_t *set = type_set();
        hset_insert(set, &(type->var));
        return set;
    }
    else if (type->tag == TYPE_CON)
    {
        hset_t *result = type_set();
        for (int i = 0; i < type->con.count; i++)
        {
            hset_t *subset = tvs(type->con.types[i]);
            hset_t *new_result = hset_union(result, subset);

            result = new_result;
        }

        return result;
    }

    return NULL;
}

static hset_t *tvs_scheme(scheme_t *scheme)
{
    hset_t *set = tvs_type(scheme->type);
    return set_difference(set, scheme->set);
}

static hset_t *tvs_constraint(constraint_t *constraint)
{
    hset_t *tvs1 = tvs_type(constraint->left);
    hset_t *tvs2 = tvs_type(constraint->right);
    hset_t *result = set_union(tvs1, tvs2);

    return result;
}

static hset_t *tvs(d_value_t *val)
{
    switch (val->tag)
    {
    case TTYPE:
        return tvs_type(val->type);
    case TSCHEME:
        return tvs_scheme(val->scheme);
    case TCONSTRAINT:
        return tvs_constraint(val->constraint);
    default:
        return NULL;
    }

    return NULL;
}

static scheme_t generalize(type_t *type)
{
    return (scheme_t){0};
}

static void add_constraint(type_t *left, type_t *right)
{
    if (constraints(&type_visitor) == NULL)
    {
        constraints(&type_visitor) = new_array(constraint_t);
    }

    constraint_t constraint = {
        .left = left,
        .right = right,
    };

    array_push(constraints(&type_visitor), &constraint);
}

define_visitor(type_function_dec, node_function_dec_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(type_function_expression, node_function_expression_t)
{
    node_parameter_list_t *p = (node_parameter_list_t *)ast->parameters;

    type_t **params = NULL;
    if (p != NULL)
        params = ast->parameters->accept(ast->parameters, visitor);

    type_t *body = ast->block->accept(ast->block, visitor);

    CREATE_TYPE_CON(ast, type, "->")
    type->con.count = p == NULL ? 1 : p->params_no + 1;
    type->con.types = (type_t **)type_alloc(sizeof(type_t *) * type->con.count);

    for (int i = 0; i < type->con.count; i++)
    {
        if (p == NULL)
            type->con.types[i] = params[i];
        else
        {

            if (i != type->con.count - 1)
                type->con.types[i] = params[i];
            else
                type->con.types[i] = body;
        }
    }

    return type;
}

define_visitor(type_parameter_list, node_parameter_list_t)
{
    type_t **type = (type_t **)type_alloc(sizeof(type_t *) * ast->params_no);

    for (int i = 0; i < ast->params_no; i++)
    {
        type[i] = ast->parameters[i]->accept(ast->parameters[i], visitor);
    }

    return type;
}

define_visitor(type_function_call, node_function_call_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(type_variable, node_variable_t)
{
    type_t *exp = ast->expression->accept(ast->expression, visitor);

    scheme_t scheme = generalize(exp);

    (void)scheme;

    return NULL;
}

define_visitor(type_parameter, node_parameter_t)
{
    return ast->identifer->accept(ast->identifer, visitor);
}

define_visitor(type_binary, node_binary_t)
{
    type_t *left = (type_t *)(ast->left->accept(ast->left, visitor));
    type_t *right = (type_t *)(ast->right->accept(ast->right, visitor));

    if (ast->op == OP_PLUS || ast->op == OP_MINUS ||
        ast->op == OP_MULT || ast->op == OP_DIV)
    {
        /**   Expects both operands to be integers
         *    We can constrain the left to be an integer and since earlier
         *    we added a constraint that the right operand should be equal to the left operand
         *
         *    operator (OP_PLUS, OP_MINUS, OP_MULT, OP_DIV) has type integer -> integer -> integer
         *    That is, it takes two integers and returns an integer
         */
        CREATE_TYPE_CON(ast, expected, "int")
        add_constraint(left, expected);
        add_constraint(right, expected);

        // return type (This operators return an integer as a result)
        return expected;
    }
    else if (ast->op == OP_IS_EQUAL || ast->op == OP_NOT_EQUAL ||
             ast->op == OP_LESS || ast->op == OP_LESS_EQUAL ||
             ast->op == OP_GREATER || ast->op == OP_GREATER_EQUAL)
    {
        CREATE_TYPE_CON(ast, expected, "int")
        add_constraint(left, expected);
        add_constraint(right, expected);

        // return type (This operators return a boolean as a result)
        return expected;
    }
    else if (ast->op == OP_AND || ast->op == OP_OR)
    {
        CREATE_TYPE_CON(ast, expected, "bool")
        add_constraint(left, expected);
        add_constraint(right, expected);

        // return type (This operators return a boolean as a result)
        return expected;
    }
    else if (ast->op == OP_ASSIGN)
    {
        add_constraint(left, right);
        return left;
    }

    return NULL;
}

/**
 *      Γ ⊢ cond : Bool       Γ ⊢ expr1 : τ       Γ ⊢ expr2 : τ
 *   ---------------------------------------------------------
 *          Γ ⊢ (if cond then expr1 else expr2) : τ
 *
 * This rule reads:
 *  - Under the type environment Γ,
 *      - The condition cond must have type Bool.
 *      - The expression expr1 must have some type τ.
 *      - The expression expr2 must also have type τ.
 *  - Therefore, the entire if-else expression has type τ.
 */
define_visitor(type_ternary, node_ternary_t)
{
    (void)ast;
    (void)visitor;

    type_t *cond = (type_t *)(ast->condition->accept(ast->condition, visitor));

    //  Γ ⊢ cond : Bool
    CREATE_TYPE_CON(ast, expected, "bool")
    add_constraint(cond, expected);

    type_t *then_block = (type_t *)(ast->then_block->accept(ast->then_block, visitor));
    type_t *else_block = (type_t *)(ast->else_block->accept(ast->else_block, visitor));

    // Γ ⊢ expr1 : τ       Γ ⊢ expr2 : τ
    add_constraint(then_block, else_block);

    // Γ ⊢ (if cond then expr1 else expr2) : τ
    return then_block;
}

define_visitor(type_symbol, node_symbol_t)
{
    (void)ast;
    (void)visitor;

    if (ast->symbol->data_type == NULL)
    {
        CREATE_TYPE_VAR(ast, type, type_var_id);
        return type;
    }

    return ast->symbol->data_type;
}

define_visitor(type_array_access, node_array_access_t)
{
    (void)ast;
    (void)visitor;

    CREATE_TYPE_CON(ast, type, "[]")

    return type;
}

define_visitor(type_array, node_array_t)
{
    (void)visitor;
    (void)ast;
    CREATE_TYPE_CON(ast, type, "[]")
    return type;
}

define_visitor(type_number, node_number_t)
{
    (void)ast;
    (void)visitor;

    CREATE_TYPE_CON(ast, type, "int")

    return type;
}

define_visitor(type_string, node_string_t)
{
    (void)ast;
    (void)visitor;

    CREATE_TYPE_CON(ast, type, "string")

    return type;
}

define_visitor(type_bool, node_bool_t)
{
    (void)ast;
    (void)visitor;

    CREATE_TYPE_CON(ast, type, "bool")

    return type;
}

define_visitor(type_identifier, node_identifier_t)
{
    if (ast->type)
    {
        void *type = ast->type->accept(ast->type, visitor);
        ast->symbol->data_type = (void *)type;
        return type;
    }

    CREATE_TYPE_VAR(ast, type, type_var_id);

    ast->symbol->data_type = (void *)type;

    return type;
}

define_visitor(type_type, node_type_t)
{
    (void)ast;
    (void)visitor;

    return ast;
}

void type_init()
{
}

bool occurs_check(tvar_t *var, type_t *type)
{
    if (type->tag == TYPE_VAR)
    {
        return strcmp(var->name, type->var.name) == 0;
    }

    for (int i = 0; i < type->con.count; i++)
    {
        if (occurs_check(var, type->con.types[i]))
        {
            return true;
        }
    }
    return false;
}

hmap_t *unify(type_t *a, type_t *b)
{
    if (a->tag == TYPE_VAR &&
        b->tag == TYPE_VAR &&
        strcmp(a->var.name, b->var.name) == 0)
    {
        return hmap_create(tvar_hash, tvar_eq, NULL);
    }

    return NULL;
}

void type_exit(node_visitor_t *visitor)
{
    (void)visitor;

    if (constraints(&type_visitor) == NULL)
        goto cleanup;

    size_t i;
    array_for_each(i, constraints(&type_visitor))
    {
        constraint_t *constraint = array_at(constraints(&type_visitor), i);
        unify(constraint->left, constraint->right);
    }

cleanup:
    arena_free(&type_arena_g);
}

type_ctx_t type_ctx = {
    .constraints = NULL,
};

node_visitor_t type_visitor = {
    .fullname = "Type Checker",
    .ctx = &type_ctx,
    .init = type_init,
    .exit = type_exit,
    .entry = def_entry,
    .leave = def_leave,
    .eof_fun = def_eof,
    .type_fun = type_type,
    .bool_fun = type_bool,
    .block_fun = def_block,
    .array_fun = type_array,
    .return_fun = def_return,
    .symbol_fun = type_symbol,
    .number_fun = type_number,
    .string_fun = type_string,
    .ternary_fun = type_ternary,
    .variable_fun = type_variable,
    .arguments_fun = def_arguments,
    .parameter_fun = type_parameter,
    .expression_fun = def_expression,
    .identifier_fun = type_identifier,
    .binary_expression_fun = type_binary,
    .array_access_fun = type_array_access,
    .function_dec_fun = type_function_dec,
    .statements_fun = def_source_elements,
    .variable_list_fun = def_variable_list,
    .function_call_fun = type_function_call,
    .parameter_list_fun = type_parameter_list,
    .variable_statement_fun = def_variable_statement,
    .function_expression_fun = type_function_expression,
    .expression_statement_fun = def_expression_statement,
};