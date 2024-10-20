#include "modules/default/types.h"

memory_arena_t type_arena_g = {NULL};

#define type_ctx_cast(visitor) ((type_ctx_t *)((visitor)->ctx))

#define constraint(visitor) type_ctx_cast(visitor)->constraint

DEFINE_ALLOC(type_alloc, &type_arena_g);
DEFINE_STRDUP(type_strdup, &type_arena_g);

const char *type_name_g[] = {
    "void",
    "tuple",
    "array",
    "string",
    "boolean",
    "number",
    "function",
    "unknown",
};

typedef enum
{
    CT_EQUAL,
} constraint_type_e;

typedef struct constraint
{
    constraint_type_e op;
    union
    {
        struct // equality constraints
        {
            type_t *left;
            type_t *right;
        };
    };
} constraint_t;

typedef struct constraint_list
{
    struct constraint constraint;
    struct constraint_list *next;
} constraint_list_t;

typedef struct type_ctx
{
    constraint_list_t *constraint;
} type_ctx_t;

static constraint_list_t *new_constraint(constraint_type_e op, type_t *left, type_t *right)
{
    constraint_list_t *c = (constraint_list_t *)type_alloc(sizeof(constraint_list_t));
    c->constraint.op = op;
    c->constraint.left = left;
    c->constraint.right = right;
    c->next = NULL;
    return c;
}

static void add_constraint(constraint_type_e op, type_t *left, type_t *right)
{
    constraint_list_t *c = new_constraint(op, left, right);

    if (!constraint(&type_visitor))
    {
        constraint(&type_visitor) = c;
        return;
    }

    constraint_list_t *c2 = constraint(&type_visitor);

    while (c2->next)
        c2 = c2->next;

    c2->next = c;
}

define_visitor(type_function_dec, node_function_dec_t)
{
    CREATE_TYPE(type, TYPE_FUNCTION)

    node_parameter_list_t *p = (node_parameter_list_t *)(ast->parameters);
    type->nparams = p->params_no;
    type->params = NULL;

    ast->symbol->data_type = type;

    if (ast->parameters)
        type->params = ast->parameters->accept(ast->parameters, visitor);

    ast->block->accept(ast->block, visitor);

    return type;
}

define_visitor(type_function_expression, node_function_expression_t)
{
    CREATE_TYPE(type, TYPE_FUNCTION)

    node_parameter_list_t *p = (node_parameter_list_t *)(ast->parameters);
    type->nparams = p->params_no;
    type->params = NULL;

    if (ast->parameters)
        type->params = ast->parameters->accept(ast->parameters, visitor);

    type->return_type = ast->block->accept(ast->block, visitor);

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
    CREATE_TYPE(type, TYPE_FUNCTION)
    type_t *t_exp = ((type_t *)ast->expression->accept(ast->expression, visitor));
    add_constraint(CT_EQUAL, t_exp, type);

    node_arguments_t *args = (node_arguments_t *)(ast->arguments);

    if (t_exp->data_type != TYPE_FUNCTION)
    {
        error(ast->base.loc,
              ERROR_SEMANTIC,
              "Type '%s' is not callable.",
              type_name_g[t_exp->data_type]);

        return NULL;
    }

    if (t_exp->nparams != args->nargs)
    {
        error(ast->base.loc,
              ERROR_SEMANTIC,
              "Expected %d arguments, got %d.",
              t_exp->nparams,
              args->nargs);

        return NULL;
    }

    for (int i = 0; i < t_exp->nparams; i++)
    {
        type_t *t_arg = (type_t *)(args->args[i]->accept(args->args[i], visitor));
        add_constraint(CT_EQUAL, t_exp->params[i], t_arg);
    }

    return t_exp->return_type;
}

define_visitor(type_variable, node_variable_t)
{
    type_t *an = (type_t *)(ast->identifer->accept(ast->identifer, visitor));
    type_t *type = (type_t *)(ast->expression->accept(ast->expression, visitor));

    if (an->data_type == TYPE_UNKNOWN && type->data_type != TYPE_UNKNOWN)
    {
        add_constraint(CT_EQUAL, type, type);
        ((node_identifier_t *)(ast->identifer))->symbol->data_type = type;
    }
    else
        add_constraint(CT_EQUAL, an, type);

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

    CREATE_TYPE(type, TYPE_UNKNOWN)

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
        CREATE_TYPE(expected, TYPE_INTEGER)
        add_constraint(CT_EQUAL, left, expected);
        add_constraint(CT_EQUAL, right, expected);

        // return type (This operators return an integer as a result)
        type->data_type = TYPE_INTEGER;
    }
    else if (ast->op == OP_IS_EQUAL || ast->op == OP_NOT_EQUAL ||
             ast->op == OP_LESS || ast->op == OP_LESS_EQUAL ||
             ast->op == OP_GREATER || ast->op == OP_GREATER_EQUAL)
    {
        CREATE_TYPE(expected, TYPE_INTEGER)
        add_constraint(CT_EQUAL, left, expected);
        add_constraint(CT_EQUAL, right, expected);

        // return type (This operators return a boolean as a result)
        type->data_type = TYPE_BOOLEAN;
    }
    else if (ast->op == OP_AND || ast->op == OP_OR)
    {
        CREATE_TYPE(expected, TYPE_BOOLEAN)
        add_constraint(CT_EQUAL, left, expected);
        add_constraint(CT_EQUAL, right, expected);

        // return type (This operators return a boolean as a result)
        type->data_type = TYPE_BOOLEAN;
    }
    else if (ast->op == OP_ASSIGN)
    {
        add_constraint(CT_EQUAL, left, right);
        type->data_type = left->data_type;
    }

    return type;
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
    CREATE_TYPE(expected, TYPE_BOOLEAN)
    add_constraint(CT_EQUAL, cond, expected);

    type_t *then_block = (type_t *)(ast->then_block->accept(ast->then_block, visitor));
    type_t *else_block = (type_t *)(ast->else_block->accept(ast->else_block, visitor));

    // Γ ⊢ expr1 : τ       Γ ⊢ expr2 : τ
    add_constraint(CT_EQUAL, then_block, else_block);

    // Γ ⊢ (if cond then expr1 else expr2) : τ
    return then_block;
}

define_visitor(type_symbol, node_symbol_t)
{
    (void)ast;
    (void)visitor;

    if (ast->symbol->data_type == NULL)
    {
        CREATE_TYPE(type, TYPE_UNKNOWN)
        return type;
    }

    return ast->symbol->data_type;
}

define_visitor(type_array, node_array_t)
{
    CREATE_TYPE(type, TYPE_ARRAY)
    type->length = ast->nch;
    type->elements = (type_t **)type_alloc(sizeof(type_t *) * ast->nch);

    for (int i = 0; i < ast->nch; i++)
    {
        type->elements[i] = ast->elements[i]->accept(ast->elements[i], visitor);
    }

    return type;
}

define_visitor(type_number, node_number_t)
{
    (void)ast;
    (void)visitor;

    CREATE_TYPE(type, TYPE_INTEGER)

    return type;
}

define_visitor(type_string, node_string_t)
{
    (void)ast;
    (void)visitor;

    CREATE_TYPE(type, TYPE_STRING)

    return type;
}

define_visitor(type_bool, node_bool_t)
{
    (void)ast;
    (void)visitor;

    CREATE_TYPE(type, TYPE_BOOLEAN)

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

    CREATE_TYPE(type, TYPE_UNKNOWN)

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

void type_exit(node_visitor_t *visitor)
{
    constraint_list_t *c = constraint(visitor);

    while (c)
    {
        if (c->constraint.left->data_type == TYPE_UNKNOWN)
        {
            *c->constraint.left = *c->constraint.right;
        }
        else if (c->constraint.right->data_type == TYPE_UNKNOWN)
        {
            *c->constraint.right = *c->constraint.left;
        }
        else if (c->constraint.left->data_type != c->constraint.right->data_type)
        {
            error(
                c->constraint.right->base.loc,
                ERROR_TYPE,
                "Incompatible types: %s and %s",
                type_name_g[c->constraint.left->data_type],
                type_name_g[c->constraint.right->data_type]);
        }

        printf(
            "Constraint: %s -> %s\n",
            type_name_g[c->constraint.left->data_type],
            type_name_g[c->constraint.right->data_type]);

        c = c->next;
    }

    arena_free(&type_arena_g);
}

type_ctx_t type_ctx = {
    .constraint = NULL,
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
    .function_dec_fun = type_function_dec,
    .statements_fun = def_source_elements,
    .variable_list_fun = def_variable_list,
    .function_call_fun = type_function_call,
    .parameter_list_fun = type_parameter_list,
    .variable_statement_fun = def_variable_statement,
    .function_expression_fun = type_function_expression,
    .expression_statement_fun = def_expression_statement,
};