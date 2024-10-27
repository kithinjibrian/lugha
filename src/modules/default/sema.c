#include "modules/default/sema.h"

define_visitor(sema_binary, node_binary_t)
{
    (void)visitor;

    if (ast->op == OP_ASSIGN)
    {
        if (ast->left->type == NODE_SYMBOL)
        {
            symbol_t *symbol = ((node_symbol_t *)ast->left)->symbol;

            if (symbol && symbol->is_const)
            {
                if (symbol->type == SYM_VARIABLE)
                    error(ast->base.loc,
                          ERROR_SEMANTIC,
                          "Assigning to a constant variable '%s'.",
                          symbol->name);
                else if (symbol->type == SYM_PARAMETER)
                {
                    error(ast->base.loc,
                          ERROR_SEMANTIC,
                          "Assigning to a constant parameter '%s'.\n"
                          "*** Parameters are constant by default. ***\n"
                          "Use 'mut %s' instead to make the parameter mutable.",
                          symbol->name, symbol->name);
                }

                return NULL;
            }
        }
    }

    ast->left->accept(ast->left, visitor);
    ast->right->accept(ast->right, visitor);

    return NULL;
}

define_visitor(sema_variable, node_variable_t)
{
    (void)visitor;
    ast->identifer->accept(ast->identifer, visitor);

    if (ast->expression)
        ast->expression->accept(ast->expression, visitor);
    else
    {
        error(ast->base.loc,
              ERROR_SEMANTIC,
              "Variable '%s' is not initialized.",
              ((node_identifier_t *)(ast->identifer))->name);
    }

    return NULL;
}

define_visitor(sema_parameter, node_parameter_t)
{
    (void)visitor;
    (void)ast;

    if (ast->expression)
        error(ast->identifer->loc,
              ERROR_SEMANTIC,
              "A parameter '%s' can't have an initializer.",
              ((node_identifier_t *)(ast->identifer))->name);

    return NULL;
}

define_visitor(sema_identifier, node_identifier_t)
{
    (void)visitor;

    if (ast->symbol == NULL)
    {
        error(ast->base.loc,
              ERROR_SEMANTIC,
              "Variable '%s' already defined.",
              ast->name);
    }

    return NULL;
}

define_visitor(sema_symbol, node_symbol_t)
{
    (void)visitor;

    if (ast->symbol == NULL)
        error(
            ast->base.loc,
            ERROR_SEMANTIC,
            "Symbol '%s' is not declared.",
            ast->name);

    return (void *)ast;
}

define_visitor(sema_function_call, node_function_call_t)
{
    (void)visitor;
    (void)ast;

    void *ret = ast->expression->accept(ast->expression, visitor);

    if (ret)
    {
        node_ast_t *node = node_ast_cast(ret);
        if (node->type == NODE_SYMBOL)
        {
            symbol_t *symbol = ((node_symbol_t *)node)->symbol;
            if (symbol->type == SYM_FUNCTION)
            {
                int args = ((node_arguments_t *)(ast->arguments))->nargs;
                if (args < symbol->no_params)
                    error(ast->base.loc,
                          ERROR_SEMANTIC,
                          "Too few arguments for function '%s'.",
                          symbol->name);
            }
        }
    }

    ast->arguments->accept(ast->arguments, visitor);

    return NULL;
}

define_visitor(sema_arguments, node_arguments_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(sema_entry, node_ast_t)
{
    (void)visitor;
    (void)ast;

    //   printf("Entering node: %s\n", ast_type_str_g[ast->type]);

    return NULL;
}

define_visitor(sema_leave, node_ast_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

void sema_init()
{
}

void sema_exit(node_visitor_t *visitor)
{
    (void)visitor;
}

node_visitor_t sema_visitor = {
    .fullname = "Semantic Analyzer",
    .shortname = "sema",
    .author = "Kithinji Brian",
    .doc = "Semantic Analyzer module",
    .init = sema_init,
    .exit = sema_exit,
    .entry = sema_entry,
    .leave = sema_leave,
    .eof_fun = def_eof,
    .bool_fun = def_bool,
    .block_fun = def_block,
    .array_fun = def_array,
    .number_fun = def_number,
    .string_fun = def_string,
    .return_fun = def_return,
    .symbol_fun = sema_symbol,
    .ternary_fun = def_ternary,
    .variable_fun = sema_variable,
    .parameter_fun = sema_parameter,
    .arguments_fun = sema_arguments,
    .identifier_fun = sema_identifier,
    .binary_expression_fun = sema_binary,
    .array_access_fun = def_array_access,
    .function_dec_fun = def_function_dec,
    .statements_fun = def_source_elements,
    .variable_list_fun = def_variable_list,
    .function_call_fun = sema_function_call,
    .parameter_list_fun = def_parameter_list,
    .variable_statement_fun = def_variable_statement,
    .function_expression_fun = def_function_expression,
    .expression_statement_fun = def_expression_statement,
};