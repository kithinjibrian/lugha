#include "modules/lugha/lugha.h"

#define c_ctx_cast(visitor) ((c_ctx_t *)((visitor)->ctx))
#define c_ctx_ident(visitor) c_ctx_cast(visitor)->ident
#define c_ctx_comment(visitor) c_ctx_cast(visitor)->comment

typedef struct c_ctx
{
    int ident;
    bool comment;
} c_ctx_t;

static void cwrite_code(char *code)
{
    sarray_push(c_visitor.code, code, strlen(code));
}

static void cwrite_ident(int ident)
{
    if (ident <= 0)
        return;

    for (int i = 0; i < ident; i++)
    {
        cwrite_code("    ");
    }
}

static void cwrite_comment(bool comment)
{
    if (comment)
        cwrite_code("// ");
}

static inline void cwrite_ops(op_type_e op)
{
    switch (op)
    {
    case OP_OR:
        cwrite_code(" || ");
        break;
    case OP_AND:
        cwrite_code(" && ");
        break;
    case OP_ASSIGN:
        cwrite_code(" = ");
        break;
    case OP_IS_EQUAL:
        cwrite_code(" == ");
        break;
    case OP_DIV:
        cwrite_code(" / ");
        break;
    case OP_MOD:
        cwrite_code(" % ");
        break;
    case OP_PLUS:
        cwrite_code(" + ");
        break;
    case OP_MINUS:
        cwrite_code(" - ");
        break;
    case OP_MULT:
        cwrite_code(" * ");
        break;
    case OP_INCREMENT:
        cwrite_code("++");
        break;
    case OP_DECREMENT:
        cwrite_code("--");
        break;
    case OP_NOT:
        cwrite_code("!");
        break;
    case OP_LESS:
        cwrite_code(" < ");
        break;
    case OP_GREATER:
        cwrite_code(" > ");
        break;
    default:
        break;
    }
}

static void c_ctx_reset(c_ctx_t *ctx)
{
    ctx->comment = false;
}

int cc_entry = 0;

define_visitor(c_expression, node_expression_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(c_expression_statement, node_expression_t)
{
    ast->expression->accept(module, ast->expression, visitor);
    cwrite_code(";\n");
    return NULL;
}

define_visitor(c_binary_expression, node_binary_t)
{
    ast->left->accept(module, ast->left, visitor);
    cwrite_ops(ast->op);
    ast->right->accept(module, ast->right, visitor);
    return NULL;
}

define_visitor(c_number, node_number_t)
{
    (void)visitor;

    char str[20];
    sprintf(str, "%d", ast->num);

    cwrite_code(str);
    return NULL;
}

define_visitor(c_array, node_array_t)
{
    (void)visitor;
    cwrite_code("[");

    for (int i = 0; i < ast->nch; i++)
    {
        ast->elements[i]->accept(module, ast->elements[i], visitor);
        if (i != ast->nch - 1)
            cwrite_code(", ");
    }

    cwrite_code("]");

    return NULL;
}

define_visitor(c_array_access, node_array_access_t)
{
    ast->array->accept(module, ast->array, visitor);
    cwrite_code("[");
    ast->index->accept(module, ast->index, visitor);
    cwrite_code("]");
    return NULL;
}

define_visitor(c_boolean, node_bool_t)
{
    (void)visitor;
    cwrite_code(ast->bol ? "true" : "false");
    return NULL;
}

define_visitor(c_variable_statement, node_variable_statement_t)
{
    if (ast->is_const)
        cwrite_code("const ");
    else
        cwrite_code("let ");

    ast->statement->accept(module, ast->statement, visitor);
    cwrite_code(";\n");
    return NULL;
}

define_visitor(c_variable_list, node_variable_list_t)
{
    for (int i = 0; i < ast->nvars; i++)
    {
        ast->variables[i]->accept(module, ast->variables[i], visitor);
        if (i != ast->nvars - 1)
            cwrite_code(", ");
    }
    return NULL;
}

define_visitor(c_parameter_list, node_parameter_list_t)
{
    for (int i = 0; i < ast->params_no; i++)
    {
        ast->parameters[i]->accept(module, ast->parameters[i], visitor);
        if (i != ast->params_no - 1)
            cwrite_code(", ");
    }
    return NULL;
}

define_visitor(c_variable, node_variable_t)
{
    node_identifier_t *iden = (node_identifier_t *)ast->identifer;
    cwrite_code(iden->name);

    if (ast->expression)
    {
        cwrite_code(" = ");
        ast->expression->accept(module, ast->expression, visitor);
    }
    return NULL;
}

define_visitor(c_parameter, node_parameter_t)
{
    node_identifier_t *iden = (node_identifier_t *)ast->identifer;
    cwrite_code(iden->name);

    if (ast->expression)
    {
        cwrite_code(" = ");
        ast->expression->accept(module, ast->expression, visitor);
    }

    return NULL;
}

define_visitor(c_identifier, node_identifier_t)
{
    (void)visitor;
    cwrite_code(ast->name);
    return NULL;
}

define_visitor(c_source_elements, node_statements_t)
{
    if (!cc_entry)
    {
        cwrite_code("/**\n * ");
        cwrite_code((char *)(c_visitor.fullname));
        cwrite_code("\n");
        cwrite_code(" * Author - ");
        cwrite_code((char *)(c_visitor.author));
        cwrite_code("\n");
        cwrite_code(" * Version - ");
        cwrite_code((char *)(c_visitor.version));
        cwrite_code("\n */ \n\n");

        cc_entry = true;
    }

    for (int i = 0; i < ast->nch; i++)
    {
        ast->children[i]->accept(module, ast->children[i], visitor);
    }
    return NULL;
}

define_visitor(c_block, node_block_t)
{
    c_ctx_ident(visitor)++;

    cwrite_code("{\n");

    if (ast->statements)
    {
        ast->statements->accept(module, ast->statements, visitor);
    }

    cwrite_ident(c_ctx_ident(visitor) - 1);

    cwrite_code("}");

    c_ctx_ident(visitor)--;

    return NULL;
}

define_visitor(c_function_dec, node_function_dec_t)
{

    cwrite_code("fun ");
    cwrite_code(ast->symbol->name);
    cwrite_code("(");

    if (ast->parameters)
        ast->parameters->accept(module, ast->parameters, visitor);

    cwrite_code(") ");

    ast->block->accept(module, ast->block, visitor);

    cwrite_code("\n\n");

    return NULL;
}

define_visitor(c_function_expression, node_function_expression_t)
{

    cwrite_code("fun (");

    if (ast->parameters)
        ast->parameters->accept(module, ast->parameters, visitor);

    cwrite_code(") -> ");

    ast->block->accept(module, ast->block, visitor);

    return NULL;
}

define_visitor(c_call, node_function_call_t)
{
    ast->expression->accept(module, ast->expression, visitor);
    cwrite_code("(");
    if (ast->arguments)
        ast->arguments->accept(module, ast->arguments, visitor);
    cwrite_code(")");

    return NULL;
}

define_visitor(c_arguments, node_arguments_t)
{
    for (int i = 0; i < ast->nargs; i++)
    {
        ast->args[i]->accept(module, ast->args[i], visitor);
        if (i != ast->nargs - 1)
            cwrite_code(", ");
    }

    return NULL;
}

define_visitor(c_symbol, node_symbol_t)
{
    (void)visitor;

    cwrite_code(ast->symbol->name);

    return NULL;
}

define_visitor(c_string, node_string_t)
{
    (void)visitor;

    cwrite_code(ast->str);

    return NULL;
}

define_visitor(c_return, node_return_t)
{
    cwrite_code("return");

    if (ast->expression)
    {
        cwrite_code(" ");
        ast->expression->accept(module, ast->expression, visitor);
    }

    cwrite_code(";\n");

    return NULL;
}

define_visitor(c_eof, node_ast_t)
{
    (void)visitor;
    (void)ast;

    cwrite_code("\n");

    return NULL;
}

define_visitor(c_break, node_ast_t)
{
    (void)visitor;
    (void)ast;
    cwrite_code("break;\n");

    return NULL;
}

define_visitor(c_if_else, node_if_else_t)
{
    if (ast->newline)
        cwrite_ident(c_ctx_ident(visitor));

    cwrite_code("if (");
    ast->condition->accept(module, ast->condition, visitor);
    cwrite_code(") ");

    if (ast->then_block->type != NODE_BLOCK)
    {
        cwrite_code("\n");
        cwrite_ident(1);
    }

    ast->then_block->accept(module, ast->then_block, visitor);

    if (ast->else_block)
    {

        if (ast->else_block->type == NODE_IF_ELSE)
            ((node_if_else_t *)(ast->else_block))->newline = false;

        if (ast->then_block->type == NODE_BLOCK)
            cwrite_code(" ");
        else
            cwrite_ident(c_ctx_ident(visitor));

        cwrite_code("else ");

        if (ast->else_block->type != NODE_IF_ELSE &&
            ast->else_block->type != NODE_BLOCK)
        {
            cwrite_code("\n");
            cwrite_ident(1);
        }

        ast->else_block->accept(module, ast->else_block, visitor);
    }

    cwrite_code("\n");

    return NULL;
}

define_visitor(c_ternary, node_ternary_t)
{
    ast->condition->accept(module, ast->condition, visitor);
    cwrite_code(" ? ");
    ast->then_block->accept(module, ast->then_block, visitor);
    cwrite_code(" : ");
    ast->else_block->accept(module, ast->else_block, visitor);

    return NULL;
}

define_visitor(c_do, node_do_t)
{
    cwrite_code("do ");
    ast->statement->accept(module, ast->statement, visitor);
    cwrite_code(" while (");
    ast->expression->accept(module, ast->expression, visitor);
    cwrite_code(");\n");

    return NULL;
}

define_visitor(c_while, node_while_t)
{
    cwrite_code("while (");
    ast->expression->accept(module, ast->expression, visitor);
    cwrite_code(") ");
    ast->statement->accept(module, ast->statement, visitor);
    cwrite_code("\n");

    return NULL;
}

define_visitor(c_continue, node_ast_t)
{
    (void)visitor;
    (void)ast;
    cwrite_code("continue\n");

    return NULL;
}

define_visitor(c_unary, node_unary_t)
{
    cwrite_ops(ast->op);
    ast->expression->accept(module, ast->expression, visitor);

    return NULL;
}

define_visitor(c_postfix, node_postfix_t)
{
    ast->expression->accept(module, ast->expression, visitor);
    cwrite_ops(ast->op);

    return NULL;
}

void c_init(module_t *module)
{
    (void)module;
    c_visitor.code = sarray_init(500);
}

void save_c(node_visitor_t *visitor)
{
    sarray_free(visitor->code);
}

void c_exit(module_t *module, node_visitor_t *visitor)
{
    (void)module;
    save_c(visitor);
}

void *c_entry(module_t *module, node_visitor_t *visitor, node_ast_t *ast)
{
    (void)visitor;
    (void)module;
    (void)ast;

    //  printf("[ERROR] node type: %s\n", ast_type_str_g[ast->type]);

    switch (ast->type)
    {
    case NODE_DO:
    case NODE_BREAK:
    case NODE_WHILE:
    case NODE_RETURN:
    case NODE_CONTINUE:
    case NODE_EXPRESSION:
    case NODE_VARIABLE_STATEMENT:
        cwrite_comment(c_ctx_comment(visitor));
        cwrite_ident(c_ctx_ident(visitor));
        break;

    default:
        break;
    }
    return NULL;
}

void *c_leave(module_t *module, node_visitor_t *visitor, node_ast_t *ast)
{
    (void)visitor;
    (void)module;
    (void)ast;
    return NULL;
}

c_ctx_t c_ctx = {
    .comment = false,
};

node_visitor_t c_visitor = {
    .fullname = "Poseidon (C lang code generator)",
    .shortname = "c_gen",
    .author = "Kithinji Brian",
    .doc = "Generate C lang code from AST",
    .version = "0.0.1",
    .code = NULL,
    .ctx = &c_ctx,
    .init = c_init,
    .exit = c_exit,
    .entry = c_entry,
    .leave = c_leave,
    .do_fun = c_do,
    .eof_fun = c_eof,
    .block_fun = c_block,
    .break_fun = c_break,
    .while_fun = c_while,
    .unary_fun = c_unary,
    .array_fun = c_array,
    .bool_fun = c_boolean,
    .number_fun = c_number,
    .symbol_fun = c_symbol,
    .class_fun = def_class,
    .return_fun = c_return,
    .string_fun = c_string,
    .struct_fun = def_struct,
    .export_fun = def_export,
    .object_fun = def_object,
    .postfix_fun = c_postfix,
    .ternary_fun = c_ternary,
    .if_else_fun = c_if_else,
    .variable_fun = c_variable,
    .continue_fun = c_continue,
    .function_call_fun = c_call,
    .arguments_fun = c_arguments,
    .parameter_fun = c_parameter,
    .expression_fun = c_expression,
    .identifier_fun = c_identifier,
    .array_access_fun = c_array_access,
    .function_dec_fun = c_function_dec,
    .statements_fun = c_source_elements,
    .variable_list_fun = c_variable_list,
    .parameter_list_fun = c_parameter_list,
    .object_access_fun = def_object_access,
    .binary_expression_fun = c_binary_expression,
    .variable_statement_fun = c_variable_statement,
    .function_expression_fun = c_function_expression,
    .expression_statement_fun = c_expression_statement,
};