#include "modules/js/nun.h"

#define nun_ctx_cast(visitor) ((nun_ctx_t *)((visitor)->ctx))
#define nun_ctx_ident(visitor) nun_ctx_cast(visitor)->ident
#define nun_ctx_comment(visitor) nun_ctx_cast(visitor)->comment

typedef struct nun_ctx
{
    int ident;
    bool comment;
} nun_ctx_t;

static void nwrite_code_ex(module_t *module, int n, char *code)
{
    sarray_push(module->buffers[n], code, strlen(code));
}

static void nwrite_ident_ex(module_t *module, int buffer, int ident)
{
    if (ident <= 0)
        return;

    for (int i = 0; i < ident; i++)
    {
        nwrite_code_ex(module, buffer, "    ");
    }
}

static void nwrite_code(module_t *module, char *code)
{
    nwrite_code_ex(module, 1, code);
}

static void nwrite_ident(module_t *module, int ident)
{
    nwrite_ident_ex(module, 1, ident);
}

static void nwrite_comment(module_t *module, bool comment)
{
    if (comment)
        nwrite_code(module, "// ");
}

static inline void nwrite_ops(module_t *module, op_type_e op)
{
    switch (op)
    {
    case OP_OR:
        nwrite_code(module, " || ");
        break;
    case OP_AND:
        nwrite_code(module, " && ");
        break;
    case OP_ASSIGN:
        nwrite_code(module, " = ");
        break;
    case OP_IS_EQUAL:
        nwrite_code(module, " == ");
        break;
    case OP_NOT_EQUAL:
        nwrite_code(module, " != ");
        break;
    case OP_DIV:
        nwrite_code(module, " / ");
        break;
    case OP_MOD:
        nwrite_code(module, " % ");
        break;
    case OP_PLUS:
        nwrite_code(module, " + ");
        break;
    case OP_MINUS:
        nwrite_code(module, " - ");
        break;
    case OP_MULT:
        nwrite_code(module, " * ");
        break;
    case OP_INCREMENT:
        nwrite_code(module, "++");
        break;
    case OP_DECREMENT:
        nwrite_code(module, "--");
        break;
    case OP_NOT:
        nwrite_code(module, "!");
        break;
    case OP_LESS:
        nwrite_code(module, " < ");
        break;
    case OP_GREATER:
        nwrite_code(module, " > ");
        break;
    case OP_ADD_ASSIGN:
        nwrite_code(module, " += ");
        break;
    case OP_SUB_ASSIGN:
        nwrite_code(module, " -= ");
        break;
    case OP_MULT_ASSIGN:
        nwrite_code(module, " *= ");
        break;
    case OP_DIV_ASSIGN:
        nwrite_code(module, " /= ");
        break;
    case OP_MOD_ASSIGN:
        nwrite_code(module, " %= ");
        break;
    case OP_AND_ASSIGN:
        nwrite_code(module, " &= ");
        break;
    case OP_OR_ASSIGN:
        nwrite_code(module, " |= ");
        break;
    case OP_XOR_ASSIGN:
        nwrite_code(module, " ^= ");
        break;
    case OP_SHL_ASSIGN:
        nwrite_code(module, " <<= ");
        break;
    case OP_SHR_ASSIGN:
        nwrite_code(module, " >>= ");
        break;
    default:
        break;
    }
}

static inline void nwrite_dunder(module_t *module, op_type_e op)
{
    switch (op)
    {
    case OP_OR:
        nwrite_code(module, " || ");
        break;
    case OP_AND:
        nwrite_code(module, " && ");
        break;
    case OP_ASSIGN:
        nwrite_code(module, " = ");
        break;
    case OP_NOT_EQUAL:
        nwrite_code(module, ".__neq__(");
        break;
    case OP_IS_EQUAL:
        nwrite_code(module, ".__eq__(");
        break;
    case OP_DIV:
        nwrite_code(module, ".__div__(");
        break;
    case OP_MOD:
        nwrite_code(module, ".__mod__(");
        break;
    case OP_PLUS:
        nwrite_code(module, ".__add__(");
        break;
    case OP_MINUS:
        nwrite_code(module, ".__sub__(");
        break;
    case OP_MULT:
        nwrite_code(module, ".__mult__(");
        break;
    case OP_LESS:
        nwrite_code(module, ".__less__(");
        break;
    case OP_GREATER:
        nwrite_code(module, ".__greater__(");
        break;
    case OP_INCREMENT:
        nwrite_code(module, "++");
        break;
    case OP_DECREMENT:
        nwrite_code(module, "--");
        break;
    case OP_NOT:
        nwrite_code(module, "!");
        break;
    default:
        break;
    }
}

static void nun_ctx_reset(nun_ctx_t *ctx)
{
    ctx->comment = false;
}

define_visitor(nun_import, node_import_t)
{
    (void)visitor;
    (void)ast;
    nwrite_code_ex(module, 0, "import ");
    nwrite_code_ex(module, 0, ast->name);

    nwrite_code_ex(module, 0, " from '");

    path_t *path = path_ext(ast->path, "mjs", 1, ".");
    path_delete(path, 1);

    if (ast->module->is_dir)
    {
        path_delete(path, 1);
    }

    nwrite_code_ex(module, 0, path->full_path);

    nwrite_code_ex(module, 0, "';\n");

    free_path(path);

    return NULL;
}

define_visitor(nun_export, node_export_t)
{
    (void)visitor;
    (void)ast;

    ast->statement->accept(module, ast->statement, visitor);

    if (ast->symbol->tag != SYM_MODULE)
    {
        nwrite_ident(module, nun_ctx_ident(visitor));
        nwrite_code(module, module->name);
        nwrite_code(module, ".");
        nwrite_code(module, ast->symbol->name);
        nwrite_code(module, " = ");
        nwrite_code(module, ast->symbol->name);
        nwrite_code(module, ";\n");
    }

    return NULL;
}

define_visitor(nun_expression, node_expression_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(nun_expression_statement, node_expression_t)
{
    ast->expression->accept(module, ast->expression, visitor);
    nwrite_code(module, ";\n");
    return NULL;
}

define_visitor(nun_binary_expression, node_binary_t)
{
    ast->left->accept(module, ast->left, visitor);

    if (ast->left->type == NODE_OBJECT)
    {
        nwrite_dunder(module, ast->op);
        ast->right->accept(module, ast->right, visitor);
        nwrite_code(module, ")");

        return NULL;
    }

    nwrite_ops(module, ast->op);
    ast->right->accept(module, ast->right, visitor);

    return NULL;
}

define_visitor(nun_struct, node_struct_t)
{
    (void)visitor;
    (void)ast;
    nwrite_code(module, "class ");
    nwrite_code(module, ast->symbol->name);
    nwrite_code(module, " {\n");
    nwrite_ident(module, nun_ctx_ident(visitor) + 1);
    nwrite_code(module, "constructor({ ");

    for (int i = 0; i < ast->nch; i++)
    {
        node_identifier_t *iden = (node_identifier_t *)(ast->fields[i]);
        nwrite_code(module, iden->name);
        if (i != ast->nch - 1)
            nwrite_code(module, ", ");
    }

    nwrite_code(module, " }) {\n");

    for (int i = 0; i < ast->nch; i++)
    {
        nwrite_ident(module, nun_ctx_ident(visitor) + 2);
        nwrite_code(module, "this.");
        node_identifier_t *iden = (node_identifier_t *)(ast->fields[i]);
        nwrite_code(module, iden->name);
        nwrite_code(module, " = ");
        nwrite_code(module, iden->name);
        nwrite_code(module, ";\n");
    }

    nwrite_ident(module, nun_ctx_ident(visitor) + 1);
    nwrite_code(module, "}\n");
    nwrite_ident(module, nun_ctx_ident(visitor));
    nwrite_code(module, "}\n");

    return NULL;
}

define_visitor(nun_methods, node_methods_t)
{
    (void)visitor;
    (void)ast;

    for (int i = 0; i < ast->nch; i++)
    {
        // nwrite_code(module, ast->symbol->name);
        ast->sym->accept(module, ast->sym, visitor);
        nwrite_code(module, ".prototype.");

        node_function_dec_t *fun = (node_function_dec_t *)(ast->methods[i]);

        nwrite_code(module, fun->symbol->name);
        nwrite_code(module, " = function (");

        if (fun->parameters)
        {
            node_parameter_list_t *params = (node_parameter_list_t *)(fun->parameters);

            for (int j = 0; j < params->params_no; j++)
            {
                node_parameter_t *param = (node_parameter_t *)(params->parameters[j]);
                node_identifier_t *iden = (node_identifier_t *)param->identifer;

                if (j == 0)
                {
                    iden->symbol->is_this = true;
                }
                else
                {
                    nwrite_code(module, iden->name);
                    if (j != params->params_no - 1)
                        nwrite_code(module, ", ");
                }
            }
        }

        nwrite_code(module, ") ");

        fun->block->accept(module, fun->block, visitor);

        nwrite_code(module, "\n");
    }

    return NULL;
}

define_visitor(nun_object, node_object_t)
{
    (void)visitor;
    (void)ast;

    nwrite_code(module, "new ");
    ast->sym->accept(module, ast->sym, visitor);
    nwrite_code(module, "({ ");

    for (int i = 0; i < ast->nch; i++)
    {
        node_keyvalue_t *kv = (node_keyvalue_t *)(ast->fields[i]);
        nwrite_code(module, ((node_word_t *)(kv->key))->name);
        nwrite_code(module, ": ");
        kv->value->accept(module, kv->value, visitor);
        if (i != ast->nch - 1)
            nwrite_code(module, ", ");
    }

    nwrite_code(module, " })");

    return NULL;
}

define_visitor(nun_number, node_number_t)
{
    (void)visitor;

    char str[20];
    sprintf(str, "%d", ast->num);

    nwrite_code(module, str);
    return NULL;
}

define_visitor(nun_array, node_array_t)
{
    (void)visitor;
    nwrite_code(module, "[");

    for (int i = 0; i < ast->nch; i++)
    {
        ast->elements[i]->accept(module, ast->elements[i], visitor);
        if (i != ast->nch - 1)
            nwrite_code(module, ", ");
    }

    nwrite_code(module, "]");

    return NULL;
}

define_visitor(nun_array_access, node_array_access_t)
{
    ast->array->accept(module, ast->array, visitor);
    nwrite_code(module, "[");
    ast->index->accept(module, ast->index, visitor);
    nwrite_code(module, "]");
    return NULL;
}

define_visitor(nun_object_access, node_object_access_t)
{
    ast->object->accept(module, ast->object, visitor);
    nwrite_code(module, ".");
    ast->member->accept(module, ast->member, visitor);
    return NULL;
}

define_visitor(nun_module, node_module_t)
{
    (void)visitor;
    (void)ast;

    module_t *new_module = module_enter(module, ast->name);

    nwrite_code(module, "const ");
    nwrite_code(module, ast->name);
    nwrite_code(module, " = (function () {\n");

    nun_ctx_ident(visitor)++;

    nwrite_ident(new_module, nun_ctx_ident(visitor));
    nwrite_code(new_module, "const ");
    nwrite_code(new_module, new_module->name);
    nwrite_code(new_module, " = {};\n");

    ast->sources->accept(new_module, ast->sources, visitor);

    nwrite_ident(new_module, nun_ctx_ident(visitor));

    nwrite_code(new_module, "return ");
    nwrite_code(new_module, new_module->name);
    nwrite_code(new_module, ";\n");

    nwrite_ident(new_module, nun_ctx_ident(visitor) - 1);
    nwrite_code(new_module, "})();\n");

    nwrite_ident(new_module, nun_ctx_ident(visitor) - 1);
    nwrite_code(new_module, new_module->abs_path->segments[new_module->abs_path->no_segments - 2]);
    nwrite_code(new_module, ".");
    nwrite_code(new_module, new_module->name);
    nwrite_code(new_module, " = ");
    nwrite_code(new_module, new_module->name);
    nwrite_code(new_module, ";\n");

    nun_ctx_ident(visitor)--;

    module_leave(new_module);

    return NULL;
}

define_visitor(nun_boolean, node_bool_t)
{
    (void)visitor;
    nwrite_code(module, ast->bol ? "true" : "false");
    return NULL;
}

define_visitor(nun_variable_statement, node_variable_statement_t)
{
    if (ast->is_const)
        nwrite_code(module, "const ");
    else
        nwrite_code(module, "let ");

    ast->statement->accept(module, ast->statement, visitor);
    nwrite_code(module, ";\n");
    return NULL;
}

define_visitor(nun_variable_list, node_variable_list_t)
{
    for (int i = 0; i < ast->nvars; i++)
    {
        ast->variables[i]->accept(module, ast->variables[i], visitor);
        if (i != ast->nvars - 1)
            nwrite_code(module, ", ");
    }
    return NULL;
}

define_visitor(nun_parameter_list, node_parameter_list_t)
{
    for (int i = 0; i < ast->params_no; i++)
    {
        ast->parameters[i]->accept(module, ast->parameters[i], visitor);
        if (i != ast->params_no - 1)
            nwrite_code(module, ", ");
    }
    return NULL;
}

define_visitor(nun_variable, node_variable_t)
{
    node_identifier_t *iden = (node_identifier_t *)ast->identifer;
    nwrite_code(module, iden->name);

    if (ast->expression)
    {
        nwrite_code(module, " = ");
        ast->expression->accept(module, ast->expression, visitor);
    }
    return NULL;
}

define_visitor(nun_parameter, node_parameter_t)
{
    node_identifier_t *iden = (node_identifier_t *)ast->identifer;
    nwrite_code(module, iden->name);

    if (ast->expression)
    {
        nwrite_code(module, " = ");
        ast->expression->accept(module, ast->expression, visitor);
    }

    return NULL;
}

define_visitor(nun_identifier, node_identifier_t)
{
    (void)visitor;
    nwrite_code(module, ast->name);
    return NULL;
}

define_visitor(nun_source_elements, node_statements_t)
{
    for (int i = 0; i < ast->nch; i++)
    {
        ast->children[i]->accept(module, ast->children[i], visitor);
    }

    return NULL;
}

define_visitor(nun_block, node_block_t)
{
    nun_ctx_ident(visitor)++;

    nwrite_code(module, "{\n");

    if (ast->statements)
    {
        ast->statements->accept(module, ast->statements, visitor);
    }

    nwrite_ident(module, nun_ctx_ident(visitor) - 1);

    nwrite_code(module, "}");

    nun_ctx_ident(visitor)--;

    return NULL;
}

define_visitor(nun_function_dec, node_function_dec_t)
{

    nwrite_code(module, "function ");
    nwrite_code(module, ast->symbol->name);
    nwrite_code(module, "(");

    if (ast->parameters)
        ast->parameters->accept(module, ast->parameters, visitor);

    nwrite_code(module, ") ");

    ast->block->accept(module, ast->block, visitor);

    nwrite_code(module, "\n");

    return NULL;
}

define_visitor(nun_function_expression, node_function_expression_t)
{

    nwrite_code(module, "(");

    if (ast->parameters)
        ast->parameters->accept(module, ast->parameters, visitor);

    nwrite_code(module, ") => ");

    ast->block->accept(module, ast->block, visitor);

    return NULL;
}

define_visitor(nun_call, node_function_call_t)
{
    ast->expression->accept(module, ast->expression, visitor);
    nwrite_code(module, "(");
    if (ast->arguments)
        ast->arguments->accept(module, ast->arguments, visitor);
    nwrite_code(module, ")");

    return NULL;
}

define_visitor(nun_arguments, node_arguments_t)
{
    for (int i = 0; i < ast->nargs; i++)
    {
        ast->args[i]->accept(module, ast->args[i], visitor);
        if (i != ast->nargs - 1)
            nwrite_code(module, ", ");
    }

    return NULL;
}

define_visitor(nun_symbol, node_symbol_t)
{
    (void)visitor;

    if (ast->symbol && ast->symbol->is_this)
    {
        nwrite_code(module, "this");
    }
    else
    {
        if (ast->path->no_segments > 1)
        {
            abs_path_t *path = ast->symbol->abs_path;

            for (size_t i = 0; i < path->no_segments; i++)
            {
                nwrite_code(module, path->segments[i]);
                if (i != path->no_segments - 1)
                    nwrite_code(module, ".");
            }
        }
        else
            nwrite_code(module, ast->path->segments[0]);
    }

    return NULL;
}

define_visitor(nun_string, node_string_t)
{
    (void)visitor;

    nwrite_code(module, ast->str);

    return NULL;
}

define_visitor(nun_word, node_word_t)
{
    (void)visitor;

    nwrite_code(module, ast->name);

    return NULL;
}

define_visitor(nun_return, node_return_t)
{
    nwrite_code(module, "return");

    if (ast->expression)
    {
        nwrite_code(module, " ");
        ast->expression->accept(module, ast->expression, visitor);
    }

    nwrite_code(module, ";\n");

    return NULL;
}

define_visitor(nun_eof, node_ast_t)
{
    (void)visitor;
    (void)ast;

    // nwrite_code(module, "\n");

    return NULL;
}

define_visitor(nun_break, node_ast_t)
{
    (void)visitor;
    (void)ast;
    nwrite_code(module, "break;\n");

    return NULL;
}

define_visitor(nun_if_else, node_if_else_t)
{
    if (ast->newline)
        nwrite_ident(module, nun_ctx_ident(visitor));

    nwrite_code(module, "if (");
    ast->condition->accept(module, ast->condition, visitor);
    nwrite_code(module, ") ");

    if (ast->then_block->type != NODE_BLOCK)
    {
        nwrite_code(module, "\n");
        nwrite_ident(module, 1);
    }

    ast->then_block->accept(module, ast->then_block, visitor);

    if (ast->else_block)
    {

        if (ast->else_block->type == NODE_IF_ELSE)
            ((node_if_else_t *)(ast->else_block))->newline = false;

        if (ast->then_block->type == NODE_BLOCK)
            nwrite_code(module, " ");
        else
            nwrite_ident(module, nun_ctx_ident(visitor));

        nwrite_code(module, "else ");

        if (ast->else_block->type != NODE_IF_ELSE &&
            ast->else_block->type != NODE_BLOCK)
        {
            nwrite_code(module, "\n");
            nwrite_ident(module, 1);
        }

        ast->else_block->accept(module, ast->else_block, visitor);
    }

    nwrite_code(module, "\n");

    return NULL;
}

define_visitor(nun_ternary, node_ternary_t)
{
    ast->condition->accept(module, ast->condition, visitor);
    nwrite_code(module, " ? ");
    ast->then_block->accept(module, ast->then_block, visitor);
    nwrite_code(module, " : ");
    ast->else_block->accept(module, ast->else_block, visitor);

    return NULL;
}

define_visitor(nun_do, node_do_t)
{
    nwrite_code(module, "do ");
    ast->statement->accept(module, ast->statement, visitor);
    nwrite_code(module, " while (");
    ast->expression->accept(module, ast->expression, visitor);
    nwrite_code(module, ");\n");

    return NULL;
}

define_visitor(nun_while, node_while_t)
{
    nwrite_code(module, "while (");
    ast->expression->accept(module, ast->expression, visitor);
    nwrite_code(module, ") ");

    if (ast->statement->type != NODE_BLOCK)
    {
        nwrite_code(module, "\n");
        nwrite_ident(module, nun_ctx_ident(visitor));
    }

    ast->statement->accept(module, ast->statement, visitor);
    nwrite_code(module, "\n");

    return NULL;
}

define_visitor(nun_continue, node_ast_t)
{
    (void)visitor;
    (void)ast;
    nwrite_code(module, "continue\n");

    return NULL;
}

define_visitor(nun_unary, node_unary_t)
{
    nwrite_ops(module, ast->op);
    ast->expression->accept(module, ast->expression, visitor);

    return NULL;
}

define_visitor(nun_postfix, node_postfix_t)
{
    ast->expression->accept(module, ast->expression, visitor);
    nwrite_ops(module, ast->op);

    return NULL;
}

void nun_init(module_t *module)
{
    (void)module;
    printf("Running phase: %s\n", nun_visitor.fullname);

    module->buffers[0] = sarray_init(256);  // imports
    module->buffers[1] = sarray_init(4096); // main code

    nwrite_code(module, "/**\n * ");
    nwrite_code(module, (char *)(nun_visitor.fullname));
    nwrite_code(module, "\n");
    nwrite_code(module, " * Author - ");
    nwrite_code(module, (char *)(nun_visitor.author));
    nwrite_code(module, "\n");
    nwrite_code(module, " * Version - ");
    nwrite_code(module, (char *)(nun_visitor.version));
    nwrite_code(module, "\n */ \n\n");

    nwrite_code(module, "const ");
    nwrite_code(module, module->name);
    nwrite_code(module, " = (function () {\n");
    nwrite_ident(module, nun_ctx_ident(&nun_visitor) += 1);
    nwrite_code(module, "const ");
    nwrite_code(module, module->name);
    nwrite_code(module, " = {};\n");
}

void save_js(module_t *module, node_visitor_t *visitor)
{
    (void)module;
    (void)visitor;

    path_t *path = path_ext(module->path, "mjs", 1, build_dir_g);

    nwrite_code_ex(module, 0, (char *)(module->buffers[1]->data));

    write_file(path, module->buffers[0]->data, module->buffers[0]->used_size);

    free_path(path);
}

void nun_exit(module_t *module, node_visitor_t *visitor)
{
    (void)module;

    nwrite_ident(module, nun_ctx_ident(visitor));
    nwrite_code(module, "return ");
    nwrite_code(module, module->name);
    nwrite_code(module, ";\n");
    nwrite_code(module, "})();\n\n");
    nwrite_code(module, "export default ");
    nwrite_code(module, module->name);
    nwrite_code(module, ";\n");

    save_js(module, visitor);
    nun_ctx_ident(visitor) = 0;
    nun_ctx_comment(visitor) = false;
}

void *nun_entry(module_t *module, node_visitor_t *visitor, node_ast_t *ast)
{
    (void)visitor;
    (void)module;
    (void)ast;

    // printf("Node type: %s\n", ast->type_str);

    switch (ast->type)
    {
    case NODE_DO:
    case NODE_BREAK:
    case NODE_WHILE:
    case NODE_MODULE:
    case NODE_RETURN:
    case NODE_STRUCT:
    case NODE_METHODS:
    case NODE_CONTINUE:
    case NODE_EXPRESSION:
    case NODE_FUNCTION_DEC:
    case NODE_VARIABLE_STATEMENT:
        nwrite_comment(module, nun_ctx_comment(visitor));
        nwrite_ident(module, nun_ctx_ident(visitor));
        break;

    default:
        break;
    }

    return NULL;
}

void *nun_leave(module_t *module, node_visitor_t *visitor, node_ast_t *ast)
{
    (void)visitor;
    (void)module;
    (void)ast;
    return NULL;
}

nun_ctx_t nun_ctx = {
    .comment = false,
};

node_visitor_t nun_visitor = {
    .fullname = "Nun (JavaScript code generator)",
    .shortname = "js_gen",
    .author = "Kithinji Brian",
    .doc = "Generate JavaScript code from AST",
    .version = "0.0.1",
    .code = NULL,
    .ctx = &nun_ctx,
    .init = nun_init,
    .exit = nun_exit,
    .entry = nun_entry,
    .leave = nun_leave,
    .do_fun = nun_do,
    .eof_fun = nun_eof,
    .word_fun = nun_word,
    .block_fun = nun_block,
    .break_fun = nun_break,
    .while_fun = nun_while,
    .unary_fun = nun_unary,
    .module_fun = nun_module,
    .array_fun = nun_array,
    .class_fun = def_class,
    .bool_fun = nun_boolean,
    .export_fun = nun_export,
    .object_fun = nun_object,
    .struct_fun = nun_struct,
    .number_fun = nun_number,
    .import_fun = nun_import,
    .symbol_fun = nun_symbol,
    .return_fun = nun_return,
    .string_fun = nun_string,
    .postfix_fun = nun_postfix,
    .ternary_fun = nun_ternary,
    .methods_fun = nun_methods,
    .if_else_fun = nun_if_else,
    .variable_fun = nun_variable,
    .continue_fun = nun_continue,
    .function_call_fun = nun_call,
    .arguments_fun = nun_arguments,
    .parameter_fun = nun_parameter,
    .expression_fun = nun_expression,
    .identifier_fun = nun_identifier,
    .array_access_fun = nun_array_access,
    .function_dec_fun = nun_function_dec,
    .statements_fun = nun_source_elements,
    .object_access_fun = nun_object_access,
    .variable_list_fun = nun_variable_list,
    .parameter_list_fun = nun_parameter_list,
    .binary_expression_fun = nun_binary_expression,
    .variable_statement_fun = nun_variable_statement,
    .function_expression_fun = nun_function_expression,
    .expression_statement_fun = nun_expression_statement,
};