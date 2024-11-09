#include "modules/default/types.h"

define_visitor(type_function_dec, node_function_dec_t)
{
    (void)visitor;
    (void)ast;
    // TODO: add constraints
    return NULL;
}

define_visitor(type_function_expression, node_function_expression_t)
{
    node_parameter_list_t *p = (node_parameter_list_t *)ast->parameters;

    type_t **params = NULL;
    if (p != NULL)
        params = (type_t **)(ast->parameters->accept(module, ast->parameters, visitor));

    type_t *body = ast->block->accept(module, ast->block, visitor);

    type_t *type = new_tfun(module, ast, p->params_no, params, body);

    // for (int i = 0; i < HASH_SIZE; i++)
    // {
    //     if (ast->base.symtab->table[i] != NULL)
    //     {
    //         _symbol_t *s = ast->base.symtab->table[i];
    //         while (s != NULL)
    //         {
    //             for (int j = 0; j < p->params_no; j++)
    //             {
    //                 node_identifier_t *iden = (node_identifier_t *)(((node_parameter_t *)(p->parameters[j]))->identifer);
    //                 if (strcmp(s->name, iden->name) == 0)
    //                 {
    //                     type_t *t = (type_t *)(iden->symbol->type);
    //                     add_eq_constraint(t, params[j]);
    //                 }
    //             }
    //             s = s->next;
    //         }
    //     }
    // }

    return type;
}

define_visitor(type_parameter_list, node_parameter_list_t)
{
    (void)visitor;
    type_t **type = (type_t **)mod_alloc(module, sizeof(type_t *) * ast->params_no);

    for (int i = 0; i < ast->params_no; i++)
    {
        // type[i] = ast->parameters[i]->accept(ast->parameters[i], visitor);

        type[i] = new_ttype_var(module, ast);

        m_add(type[i]);
    }

    return type;
}

define_visitor(type_function_call, node_function_call_t)
{
    node_arguments_t *a = (node_arguments_t *)ast->arguments;

    type_t **args = NULL;

    type_t *exp = (type_t *)(ast->expression->accept(module, ast->expression, visitor));

    // TODO: functions without arguments are crushing the program

    if (ast->arguments)
        args = (type_t **)(ast->arguments->accept(module, ast->arguments, visitor));

    type_t *return_type = new_ttype_var(module, ast);

    type_t *type = new_tfun(module, ast, a->nargs, args, return_type);

    add_eq_constraint(exp, type);

    return return_type;
}

define_visitor(type_arguments, node_arguments_t)
{
    type_t **type = (type_t **)mod_alloc(module, sizeof(type_t *) * ast->nargs);

    for (int i = 0; i < ast->nargs; i++)
    {
        type[i] = ast->args[i]->accept(module, ast->args[i], visitor);
    }

    return type;
}

define_visitor(type_variable, node_variable_t)
{
    node_identifier_t *identifer = (node_identifier_t *)ast->identifer;

    type_t *exp = NULL;

    type_t *type = (type_t *)(ast->identifer->accept(module, ast->identifer, visitor));

    if (ast->expression)
        exp = (type_t *)(ast->expression->accept(module, ast->expression, visitor));

    if (exp == NULL)
        return type;
    else
        identifer->symbol->type = type;

    add_eq_constraint(exp, type);

    // TODO: insert implicit constraint here

    return exp;
}

define_visitor(type_parameter, node_parameter_t)
{
    return ast->identifer->accept(module, ast->identifer, visitor);
}

define_visitor(type_binary, node_binary_t)
{

    type_t *left = (type_t *)(ast->left->accept(module, ast->left, visitor));
    type_t *right = (type_t *)(ast->right->accept(module, ast->right, visitor));

    type_class_t *tc = mod_alloc(module, sizeof(type_class_t));

    switch (ast->op)
    {
    case OP_PLUS:
        tc->name = mod_strdup(module, "Num");
        add_tc_constraint(tc, left);
        add_tc_constraint(tc, right);
        return left;

    default:
        break;
    }

    return NULL;
}

define_visitor(type_ternary, node_ternary_t)
{
    (void)ast;
    (void)visitor;

    type_t *cond = (type_t *)(ast->condition->accept(module, ast->condition, visitor));

    add_eq_constraint(cond, new_tbool(module, ast));

    type_t *then_block = (type_t *)(ast->then_block->accept(module, ast->then_block, visitor));
    type_t *else_block = (type_t *)(ast->else_block->accept(module, ast->else_block, visitor));

    add_eq_constraint(then_block, else_block);

    return then_block;
}

define_visitor(type_symbol, node_symbol_t)
{
    (void)visitor;

    if (ast->symbol == NULL)
        return NULL;

    if (ast->symbol->type == NULL)
    {
        ast->symbol->type = new_ttype_var(module, ast);
    }

    // TODO: insert explicit constraint here

    return ast->symbol->type;
}

define_visitor(type_object, node_object_t)
{
    (void)ast;
    (void)visitor;

    type_t *trec = new_trec(module, ast, ast->nch, ast->symbol->name);

    for (int i = 0; i < ast->nch; i++)
    {
        node_keyvalue_t *kv = (node_keyvalue_t *)(ast->fields[i]);
        type_t *value = (node_type_t *)(kv->value->accept(module, kv->value, visitor));

        trec_add(module, trec, ((node_word_t *)(kv->key))->name, value);
    }

    add_eq_constraint(trec, ast->symbol->type);

    return trec;
}

define_visitor(type_keyvalue, node_keyvalue_t)
{
    (void)ast;
    (void)visitor;
    return ast->value->accept(module, ast->value, visitor);
}

define_visitor(type_struct, node_struct_t)
{
    (void)ast;
    (void)visitor;

    type_t *trec = new_trec(module, ast, ast->nch, ast->symbol->name);

    for (int i = 0; i < ast->nch; i++)
    {
        node_identifier_t *iden = (node_identifier_t *)(ast->fields[i]);
        node_type_t *type = (node_type_t *)(iden->type->accept(module, iden->type, visitor));

        trec_add(module, trec, iden->name, type);
    }

    ast->symbol->type = trec;

    return trec;
}

define_visitor(type_object_access, node_object_access_t)
{
    (void)ast;
    (void)visitor;

    type_t *object = (node_type_t *)(ast->object->accept(module, ast->object, visitor));

    node_symbol_t *sym = (node_symbol_t *)ast->member;

    if (object->tag != TYPE_REC)
    {
        error(
            ast->base.loc,
            ERROR_TYPE,
            "Type '%s' has no member '%s'",
            object->tag == TYPE_CON ? object->con.name : object->var.name,
            sym->symbol->name);

        exit(1);
    }

    return trec_get(object, sym->symbol->name);
}

define_visitor(type_array_access, node_array_access_t)
{
    (void)ast;
    (void)visitor;

    return new_tarray(module, ast);
}

define_visitor(type_array, node_array_t)
{
    (void)visitor;
    (void)ast;
    return new_tarray(module, ast);
}

define_visitor(type_number, node_number_t)
{
    (void)ast;
    (void)visitor;
    return new_tint(module, ast);
}

define_visitor(type_string, node_string_t)
{
    (void)ast;
    (void)visitor;
    return new_tstring(module, ast);
}

define_visitor(type_bool, node_bool_t)
{
    (void)ast;
    (void)visitor;
    return new_tbool(module, ast);
}

define_visitor(type_identifier, node_identifier_t)
{
    if (ast->type)
    {
        node_type_t *type = (node_type_t *)(ast->type->accept(module, ast->type, visitor));

        type->label = mod_strdup(module, ast->symbol->name);

        ast->symbol->type = (void *)type;

        return type;
    }

    ast->symbol->type = new_ttype_var(module, ast);

    return ast->symbol->type;
}

define_visitor(type_type, node_type_t)
{
    (void)ast;
    (void)visitor;

    if (ast->tag == TYPE_REF)
    {
        return ast->symbol->type;
    }

    return ast;
}

void type_init(module_t *module)
{
    (void)module;
    printf("Running phase: Type Checker\n");
}

void type_exit(module_t *module, node_visitor_t *visitor)
{
    (void)module;
    (void)visitor;

    solve(module, constraints(visitor));

    // array_free(constraints(visitor));
    // hmap_destroy(subst);
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
    .word_fun = def_word,
    .type_fun = type_type,
    .bool_fun = type_bool,
    .block_fun = def_block,
    .class_fun = def_class,
    .module_fun = def_module,
    .array_fun = type_array,
    .export_fun = def_export,
    .return_fun = def_return,
    .import_fun = def_import,
    .object_fun = type_object,
    .symbol_fun = type_symbol,
    .struct_fun = type_struct,
    .number_fun = type_number,
    .string_fun = type_string,
    .methods_fun = def_methods,
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
    .object_access_fun = type_object_access,
    .variable_list_fun = def_variable_list,
    .function_call_fun = type_function_call,
    .parameter_list_fun = type_parameter_list,
    .variable_statement_fun = def_variable_statement,
    .function_expression_fun = type_function_expression,
    .expression_statement_fun = def_expression_statement,
};