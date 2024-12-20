#include "modules/default/visitor.h"

define_visitor(def_import, node_import_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(def_export, node_export_t)
{
    (void)visitor;
    (void)ast;
    ast->statement->accept(module, ast->statement, visitor);
    return NULL;
}

define_visitor(def_block, node_block_t)
{
    ast->statements->accept(module, ast->statements, visitor);
    return NULL;
}

define_visitor(def_expression_statement, node_expression_t)
{
    ast->expression->accept(module, ast->expression, visitor);
    return NULL;
}

define_visitor(def_expression, node_expression_t)
{
    ast->expression->accept(module, ast->expression, visitor);
    return NULL;
}

define_visitor(def_binary_expression, node_binary_t)
{
    ast->left->accept(module, ast->left, visitor);
    ast->right->accept(module, ast->right, visitor);
    return NULL;
}

define_visitor(def_ternary, node_ternary_t)
{
    (void)visitor;
    (void)ast;
    ast->condition->accept(module, ast->condition, visitor);
    ast->then_block->accept(module, ast->then_block, visitor);
    ast->else_block->accept(module, ast->else_block, visitor);
    return NULL;
}

define_visitor(def_if_else, node_if_else_t)
{
    (void)visitor;
    (void)ast;
    ast->condition->accept(module, ast->condition, visitor);
    ast->then_block->accept(module, ast->then_block, visitor);
    if (ast->else_block)
        ast->else_block->accept(module, ast->else_block, visitor);
    return NULL;
}

define_visitor(def_source_elements, node_statements_t)
{
    for (int i = 0; i < ast->nch; i++)
    {
        ast->children[i]->accept(module, ast->children[i], visitor);
    }
    return NULL;
}

define_visitor(def_variable_statement, node_variable_statement_t)
{
    ast->statement->accept(module, ast->statement, visitor);
    return NULL;
}

define_visitor(def_variable_list, node_variable_list_t)
{
    for (int i = 0; i < ast->nvars; i++)
    {
        ast->variables[i]->accept(module, ast->variables[i], visitor);
    }
    return NULL;
}

define_visitor(def_parameter_list, node_parameter_list_t)
{
    for (int i = 0; i < ast->params_no; i++)
    {
        ast->parameters[i]->accept(module, ast->parameters[i], visitor);
    }
    return NULL;
}

define_visitor(def_variable, node_variable_t)
{
    ast->identifer->accept(module, ast->identifer, visitor);
    if (ast->expression)
        ast->expression->accept(module, ast->expression, visitor);
    return NULL;
}

define_visitor(def_parameter, node_parameter_t)
{
    ast->identifer->accept(module, ast->identifer, visitor);

    if (ast->expression)
        ast->expression->accept(module, ast->expression, visitor);

    return NULL;
}

define_visitor(def_function_dec, node_function_dec_t)
{
    if (ast->parameters)
        ast->parameters->accept(module, ast->parameters, visitor);
    ast->block->accept(module, ast->block, visitor);
    return NULL;
}

define_visitor(def_function_expression, node_function_expression_t)
{
    if (ast->parameters)
        ast->parameters->accept(module, ast->parameters, visitor);
    ast->block->accept(module, ast->block, visitor);
    return NULL;
}

define_visitor(def_function_call, node_function_call_t)
{
    ast->expression->accept(module, ast->expression, visitor);
    ast->arguments->accept(module, ast->arguments, visitor);
    return NULL;
}

define_visitor(def_arguments, node_arguments_t)
{
    for (int i = 0; i < ast->nargs; i++)
    {
        ast->args[i]->accept(module, ast->args[i], visitor);
    }
    return NULL;
}

define_visitor(def_class, node_class_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(def_struct, node_struct_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(def_methods, node_methods_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(def_object, node_object_t)
{
    (void)visitor;
    (void)ast;
    ast->sym->accept(module, ast->sym, visitor);
    return NULL;
}

define_visitor(def_module, node_module_t)
{
    ast->sources->accept(module, ast->sources, visitor);
    return NULL;
}

define_visitor(def_array_access, node_array_access_t)
{
    ast->array->accept(module, ast->array, visitor);
    ast->index->accept(module, ast->index, visitor);
    return NULL;
}

define_visitor(def_object_access, node_object_access_t)
{
    ast->object->accept(module, ast->object, visitor);
    ast->member->accept(module, ast->member, visitor);
    return NULL;
}

define_visitor(def_array, node_array_t)
{
    for (int i = 0; i < ast->nch; i++)
    {
        ast->elements[i]->accept(module, ast->elements[i], visitor);
    }
    return NULL;
}

define_visitor(def_number, node_number_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(def_bool, node_bool_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(def_symbol, node_symbol_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(def_word, node_word_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(def_string, node_string_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(def_return, node_return_t)
{
    if (ast->expression)
        ast->expression->accept(module, ast->expression, visitor);

    return NULL;
}

define_visitor(def_eof, node_ast_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(def_entry, node_ast_t)
{
    (void)visitor;
    (void)ast;
    //  printf("%s\n", ast->type_str);
    return NULL;
}

define_visitor(def_leave, node_ast_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}