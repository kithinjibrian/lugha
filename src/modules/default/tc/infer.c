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

    type_t *body = ast->block->accept(ast->block, visitor);

    type_t *type = new_tfun(ast, p->params_no, params, body);

    for (int i = 0; i < HASH_SIZE; i++)
    {
        if (ast->base.symtab->table[i] != NULL)
        {
            symbol_t *s = ast->base.symtab->table[i];
            while (s != NULL)
            {
                for (int j = 0; j < p->params_no; j++)
                {
                    node_identifier_t *iden = (node_identifier_t *)(((node_parameter_t *)(p->parameters[j]))->identifer);
                    if (strcmp(s->name, iden->name) == 0)
                    {
                        type_t *t = (type_t *)(iden->symbol->data_type);
                        add_eq_constraint(t, params[j]);
                    }
                }
                s = s->next;
            }
        }
    }

    return type;
}

define_visitor(type_parameter_list, node_parameter_list_t)
{
    (void)visitor;
    type_t **type = (type_t **)type_alloc(sizeof(type_t *) * ast->params_no);

    for (int i = 0; i < ast->params_no; i++)
    {
        // type[i] = ast->parameters[i]->accept(ast->parameters[i], visitor);

        type[i] = new_ttype_var(ast);

        m_add(type[i]);
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

    add_eq_constraint(exp, type);

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
        identifer->symbol->data_type = type;

    add_eq_constraint(exp, type);

    // TODO: insert implicit constraint here

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
        type_t *return_type = new_ttype_var(ast);

        add_eq_constraint(left, new_tint(ast));
        add_eq_constraint(right, new_tint(ast));
        add_eq_constraint(return_type, new_tint(ast));

        return return_type;
    }
    else if (ast->op == OP_IS_EQUAL || ast->op == OP_NOT_EQUAL ||
             ast->op == OP_LESS || ast->op == OP_LESS_EQUAL ||
             ast->op == OP_GREATER || ast->op == OP_GREATER_EQUAL)
    {
        type_t *return_type = new_ttype_var(ast);

        add_eq_constraint(left, new_tint(ast));
        add_eq_constraint(right, new_tint(ast));
        add_eq_constraint(return_type, new_tbool(ast));

        return return_type;
    }
    else if (ast->op == OP_AND || ast->op == OP_OR)
    {
        type_t *return_type = new_ttype_var(ast);

        add_eq_constraint(left, new_tbool(ast));
        add_eq_constraint(right, new_tbool(ast));
        add_eq_constraint(return_type, new_tbool(ast));

        return return_type;
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

    if (ast->symbol->data_type == NULL)
    {
        ast->symbol->data_type = new_ttype_var(ast);
    }

    // TODO: insert explicit constraint here

    return ast->symbol->data_type;
}

define_visitor(type_object, node_object_t)
{
    (void)ast;
    (void)visitor;

    type_t *trec = new_trec(ast, ast->nch, ast->symbol->name);

    for (int i = 0; i < ast->nch; i++)
    {
        node_keyvalue_t *kv = (node_keyvalue_t *)(ast->fields[i]);
        type_t *value = (node_type_t *)(kv->value->accept(kv->value, visitor));

        trec_add(trec, ((node_word_t *)(kv->key))->name, value);
    }

    add_eq_constraint(trec, ast->symbol->data_type);

    return trec;
}

define_visitor(type_keyvalue, node_keyvalue_t)
{
    (void)ast;
    (void)visitor;
    return ast->value->accept(ast->value, visitor);
}

define_visitor(type_struct, node_struct_t)
{
    (void)ast;
    (void)visitor;

    type_t *trec = new_trec(ast, ast->nch, ast->symbol->name);

    for (int i = 0; i < ast->nch; i++)
    {
        node_identifier_t *iden = (node_identifier_t *)(ast->fields[i]);
        node_type_t *type = (node_type_t *)(iden->type->accept(iden->type, visitor));

        trec_add(trec, iden->name, type);
    }

    ast->symbol->data_type = trec;

    return trec;
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
        node_type_t *type = (node_type_t *)(ast->type->accept(ast->type, visitor));

        type->label = type_strdup(ast->symbol->name);

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

    solve(constraints(visitor));

    // for (size_t i = 0; i < M(visitor)->count; i++)
    // {
    //     type_t *c = array_at(M(visitor), i);
    //     type_str(c);
    // }

    arena_free(&type_arena_g);
}

type_ctx_t type_ctx = {
    .M = NULL,
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
    .class_fun = def_class,
    .array_fun = type_array,
    .return_fun = def_return,
    .object_fun = type_object,
    .symbol_fun = type_symbol,
    .struct_fun = type_struct,
    .number_fun = type_number,
    .string_fun = type_string,
    .ternary_fun = type_ternary,
    .keyvalue_fun = type_keyvalue,
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