#include "modules/default/types.h"

memory_arena_t type_arena_g = {NULL};

DEFINE_ALLOC(type_alloc, &type_arena_g);
DEFINE_STRDUP(type_strdup, &type_arena_g);

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
        params = (type_t **)(ast->parameters->accept(ast->parameters, visitor));

    type_t *return_type = NULL;
    if (ast->return_type)
        return_type = ast->return_type->accept(ast->return_type, visitor);

    type_t *body = ast->block->accept(ast->block, visitor);

    type_t *type = new_tfun(ast, p->params_no, params, body);

    if (return_type)
        add_eq_constraint(return_type, body);

    return type;
}

define_visitor(type_parameter_list, node_parameter_list_t)
{
    type_t **type = (type_t **)type_alloc(sizeof(type_t *) * ast->params_no);

    for (int i = 0; i < ast->params_no; i++)
    {
        type[i] = ast->parameters[i]->accept(ast->parameters[i], visitor);

        node_parameter_t *param = (node_parameter_t *)(ast->parameters[i]);
        node_identifier_t *iden = (node_identifier_t *)(param->identifer);

        scheme_t *scheme = type_alloc(sizeof(scheme_t));
        scheme->set = type_set();
        scheme->type = type[i];

        iden->symbol->scheme = scheme;
    }

    return type;
}

define_visitor(type_function_call, node_function_call_t)
{
    node_arguments_t *a = (node_arguments_t *)ast->arguments;

    type_t **args = NULL;

    type_t *exp = (type_t *)(ast->expression->accept(ast->expression, visitor));

    if (ast->arguments)
        args = (type_t **)(ast->arguments->accept(ast->arguments, visitor));

    type_t *return_type = new_ttype_var(ast);

    type_t *type = new_tfun(ast, a->nargs, args, return_type);

    add_eq_constraint(type, exp);

    return return_type;
}

define_visitor(type_arguments, node_arguments_t)
{
    type_t **type = (type_t **)type_alloc(sizeof(type_t *) * ast->nargs);

    for (int i = 0; i < ast->nargs; i++)
    {
        type[i] = ast->args[i]->accept(ast->args[i], visitor);
    }

    return type;
}

define_visitor(type_variable, node_variable_t)
{
    node_identifier_t *identifer = (node_identifier_t *)ast->identifer;

    type_t *exp = NULL;

    type_t *type = (type_t *)(ast->identifer->accept(ast->identifer, visitor));

    if (ast->expression)
        exp = (type_t *)(ast->expression->accept(ast->expression, visitor));

    if (exp == NULL)
        return type;
    else
        identifer->symbol->data_type = exp;

    if (type->tag == TYPE_CON)
    {
        add_eq_constraint(type, exp);
    }

    scheme_t *scheme = generalize(constraints(visitor), exp);

    identifer->symbol->scheme = scheme;

    return exp;
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
        add_eq_constraint(left, new_tint(ast));
        add_eq_constraint(right, new_tint(ast));
        return new_tint(ast);
    }
    else if (ast->op == OP_IS_EQUAL || ast->op == OP_NOT_EQUAL ||
             ast->op == OP_LESS || ast->op == OP_LESS_EQUAL ||
             ast->op == OP_GREATER || ast->op == OP_GREATER_EQUAL)
    {
        add_eq_constraint(left, new_tint(ast));
        add_eq_constraint(right, new_tint(ast));
        return new_tbool(ast);
    }
    else if (ast->op == OP_AND || ast->op == OP_OR)
    {
        add_eq_constraint(left, new_tbool(ast));
        add_eq_constraint(right, new_tbool(ast));
        return new_tbool(ast);
    }
    else if (ast->op == OP_ASSIGN)
    {
        add_eq_constraint(left, right);
        return left;
    }

    return NULL;
}

define_visitor(type_ternary, node_ternary_t)
{
    (void)ast;
    (void)visitor;

    type_t *cond = (type_t *)(ast->condition->accept(ast->condition, visitor));

    add_eq_constraint(cond, new_tbool(ast));

    type_t *then_block = (type_t *)(ast->then_block->accept(ast->then_block, visitor));
    type_t *else_block = (type_t *)(ast->else_block->accept(ast->else_block, visitor));

    add_eq_constraint(then_block, else_block);

    return then_block;
}

define_visitor(type_symbol, node_symbol_t)
{
    (void)visitor;

    if (ast->symbol->scheme)
    {
        type_t *t = instantiate(ast->symbol->scheme);

        return t;
    }

    if (ast->symbol->data_type == NULL)
    {
        return new_ttype_var(ast);
    }

    return ast->symbol->data_type;
}

define_visitor(type_array_access, node_array_access_t)
{
    (void)ast;
    (void)visitor;

    return new_tarray(ast);
}

define_visitor(type_array, node_array_t)
{
    (void)visitor;
    (void)ast;
    return new_tarray(ast);
}

define_visitor(type_number, node_number_t)
{
    (void)ast;
    (void)visitor;
    return new_tint(ast);
}

define_visitor(type_string, node_string_t)
{
    (void)ast;
    (void)visitor;
    return new_tstring(ast);
}

define_visitor(type_bool, node_bool_t)
{
    (void)ast;
    (void)visitor;
    return new_tbool(ast);
}

define_visitor(type_identifier, node_identifier_t)
{
    if (ast->type)
    {
        void *type = ast->type->accept(ast->type, visitor);
        ast->symbol->data_type = (void *)type;
        return type;
    }

    ast->symbol->data_type = new_ttype_var(ast);

    return ast->symbol->data_type;
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
    (void)visitor;

    //  solve(constraints(&type_visitor));

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
    .arguments_fun = type_arguments,
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