#include "modules/default/types.h"

memory_arena_t type_arena_g = {NULL};

#define type_ctx_cast(visitor) ((type_ctx_t *)((visitor)->ctx))

#define constraint(visitor) type_ctx_cast(visitor)->constraint

DEFINE_ALLOC(type_alloc, &type_arena_g);
DEFINE_STRDUP(type_strdup, &type_arena_g);

typedef struct constraint
{
    YYLTYPE loc;
    op_type_e op;
    data_type_e left;
    data_type_e right;
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

static constraint_list_t *new_constraint(node_ast_t *ast, op_type_e op, data_type_e left, data_type_e right)
{
    constraint_list_t *c = (constraint_list_t *)malloc(sizeof(constraint_list_t));
    memcpy(&(c->constraint.loc), &(ast->loc), sizeof(YYLTYPE));
    c->constraint.op = op;
    c->constraint.left = left;
    c->constraint.right = right;
    c->next = NULL;
    return c;
}

static void add_constraint(node_ast_t *ast, op_type_e op, data_type_e left, data_type_e right)
{
    constraint_list_t *c = new_constraint(ast, op, left, right);
    c->next = constraint(&type_visitor);
    constraint(&type_visitor) = c;
}

define_visitor(type_variable, node_variable_t)
{
    data_type_e an = cast_vtoi(data_type_e, ast->identifer->accept(ast->identifer, visitor));
    data_type_e type = TYPE_UNKNOWN;

    if (ast->expression)
        type = cast_vtoi(data_type_e, ast->expression->accept(ast->expression, visitor));

    if (an == TYPE_UNKNOWN && type != TYPE_UNKNOWN)
        add_constraint(node_ast_cast(ast), OP_ASSIGN, type, type);
    else
        add_constraint(node_ast_cast(ast), OP_ASSIGN, an, type);

    return NULL;
}

define_visitor(type_binary, node_binary_t)
{
    data_type_e left = cast_vtoi(data_type_e, ast->left->accept(ast->left, visitor));
    data_type_e right = cast_vtoi(data_type_e, ast->right->accept(ast->right, visitor));

    add_constraint(node_ast_cast(ast), ast->op, left, right);

    if (ast->op == OP_PLUS || ast->op == OP_MINUS ||
        ast->op == OP_MULT || ast->op == OP_DIV)
    {
        add_constraint(node_ast_cast(ast), ast->op, left, TYPE_INTEGER);
        return cast_itov(TYPE_INTEGER);
    }
    else if (ast->op == OP_IS_EQUAL || ast->op == OP_NOT_EQUAL ||
             ast->op == OP_LESS || ast->op == OP_LESS_EQUAL ||
             ast->op == OP_GREATER || ast->op == OP_GREATER_EQUAL)
    {
        add_constraint(node_ast_cast(ast), ast->op, left, TYPE_INTEGER);
        return cast_itov(TYPE_BOOLEAN);
    }

    return NULL;
}

define_visitor(type_symbol, node_symbol_t)
{
    (void)ast;
    (void)visitor;

    return cast_itov(TYPE_UNKNOWN);
}

define_visitor(type_number, node_number_t)
{
    (void)ast;
    (void)visitor;

    return cast_itov(TYPE_INTEGER);
}

define_visitor(type_string, node_string_t)
{
    (void)ast;
    (void)visitor;

    return cast_itov(TYPE_STRING);
}

define_visitor(type_bool, node_bool_t)
{
    (void)ast;
    (void)visitor;

    return cast_itov(TYPE_BOOLEAN);
}

define_visitor(type_identifier, node_identifier_t)
{
    if (ast->type)
        return ast->type->accept(ast->type, visitor);

    return cast_itov(TYPE_UNKNOWN);
}

define_visitor(type_type, node_type_t)
{
    (void)ast;
    (void)visitor;

    return cast_itov(ast->data_type);
}

void type_init()
{
}

const char *type_name[] = {
    "void",
    "tuple",
    "array",
    "string",
    "boolean",
    "number",
    "function",
    "unknown",
};

void type_exit(node_visitor_t *visitor)
{
    constraint_list_t *c = constraint(visitor);

    while (c)
    {

        if (c->constraint.left == TYPE_UNKNOWN)
        {
            c->constraint.left = c->constraint.right;
        }
        else if (c->constraint.right == TYPE_UNKNOWN)
        {
            c->constraint.right = c->constraint.left;
        }
        else if (c->constraint.left != c->constraint.right)
        {
            error(
                c->constraint.loc,
                ERROR_TYPE,
                "Incompatible types: %s and %s",
                type_name[c->constraint.left],
                type_name[c->constraint.right]);
        }

        printf("Constraint: %s -> %s\n", type_name[c->constraint.left], type_name[c->constraint.right]);
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
    .return_fun = def_return,
    .symbol_fun = type_symbol,
    .number_fun = type_number,
    .string_fun = type_string,
    .variable_fun = type_variable,
    .arguments_fun = def_arguments,
    .parameter_fun = def_parameter,
    .expression_fun = def_expression,
    .identifier_fun = type_identifier,
    .function_dec_fun = def_function_dec,
    .binary_expression_fun = type_binary,
    .statements_fun = def_source_elements,
    .function_call_fun = def_function_call,
    .variable_list_fun = def_variable_list,
    .parameter_list_fun = def_parameter_list,
    .variable_statement_fun = def_variable_statement,
    .expression_statement_fun = def_expression_statement,
};