#include "modules/js/nansense.h"

#define nan_ctx_cast(visitor) ((nan_ctx_t *)((visitor)->ctx))
#define nan_ctx_ident(visitor) nan_ctx_cast(visitor)->ident
#define nan_ctx_comment(visitor) nan_ctx_cast(visitor)->comment

typedef struct nan_ctx
{
    int ident;
    bool comment;
} nan_ctx_t;

static void nwrite_code(char *code)
{
    sarray_push(nan_visitor.code, code, strlen(code));
}

static void nwrite_ident(int ident)
{
    if (ident <= 0)
        return;

    for (int i = 0; i < ident; i++)
    {
        nwrite_code("    ");
    }
}

static void nwrite_comment(bool comment)
{
    if (comment)
        nwrite_code("// ");
}

static inline void nwrite_ops(op_type_e op)
{
    switch (op)
    {
    case OP_OR:
        nwrite_code(" || ");
        break;
    case OP_AND:
        nwrite_code(" && ");
        break;
    case OP_ASSIGN:
        nwrite_code(" = ");
        break;
    case OP_IS_EQUAL:
        nwrite_code(" == ");
        break;
    case OP_DIV:
        nwrite_code(" / ");
        break;
    case OP_MOD:
        nwrite_code(" % ");
        break;
    case OP_PLUS:
        nwrite_code(" + ");
        break;
    case OP_MINUS:
        nwrite_code(" - ");
        break;
    case OP_MULT:
        nwrite_code(" * ");
        break;
    case OP_INCREMENT:
        nwrite_code("++");
        break;
    case OP_DECREMENT:
        nwrite_code("--");
        break;
    case OP_NOT:
        nwrite_code("!");
        break;
    case OP_LESS:
        nwrite_code(" < ");
        break;
    case OP_GREATER:
        nwrite_code(" > ");
        break;
    default:
        break;
    }
}

static void nan_ctx_reset(nan_ctx_t *ctx)
{
    ctx->comment = false;
}

int n_entry = 0;

define_visitor(nan_expression, node_expression_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(nan_expression_statement, node_expression_t)
{
    ast->expression->accept(ast->expression, visitor);
    nwrite_code(";\n");
    return NULL;
}

define_visitor(nan_binary_expression, node_binary_t)
{
    ast->left->accept(ast->left, visitor);
    nwrite_ops(ast->op);
    ast->right->accept(ast->right, visitor);
    return NULL;
}

define_visitor(nan_number, node_number_t)
{
    (void)visitor;

    char str[20];
    sprintf(str, "%d", ast->num);

    nwrite_code(str);
    return NULL;
}

define_visitor(nan_array, node_array_t)
{
    (void)visitor;
    nwrite_code("[");

    for (int i = 0; i < ast->nch; i++)
    {
        ast->elements[i]->accept(ast->elements[i], visitor);
        if (i != ast->nch - 1)
            nwrite_code(", ");
    }

    nwrite_code("]");

    return NULL;
}

define_visitor(nan_array_access, node_array_access_t)
{
    ast->array->accept(ast->array, visitor);
    nwrite_code("[");
    ast->index->accept(ast->index, visitor);
    nwrite_code("]");
    return NULL;
}

define_visitor(nan_boolean, node_bool_t)
{
    (void)visitor;
    nwrite_code(ast->bol ? "true" : "false");
    return NULL;
}

define_visitor(nan_variable_statement, node_variable_statement_t)
{
    if (ast->is_const)
        nwrite_code("const ");
    else
        nwrite_code("let ");

    ast->statement->accept(ast->statement, visitor);
    nwrite_code(";\n");
    return NULL;
}

define_visitor(nan_variable_list, node_variable_list_t)
{
    for (int i = 0; i < ast->nvars; i++)
    {
        ast->variables[i]->accept(ast->variables[i], visitor);
        if (i != ast->nvars - 1)
            nwrite_code(", ");
    }
    return NULL;
}

define_visitor(nan_parameter_list, node_parameter_list_t)
{
    for (int i = 0; i < ast->params_no; i++)
    {
        ast->parameters[i]->accept(ast->parameters[i], visitor);
        if (i != ast->params_no - 1)
            nwrite_code(", ");
    }
    return NULL;
}

define_visitor(nan_variable, node_variable_t)
{
    node_identifier_t *iden = (node_identifier_t *)ast->identifer;
    nwrite_code(iden->name);

    if (ast->expression)
    {
        nwrite_code(" = ");
        ast->expression->accept(ast->expression, visitor);
    }
    return NULL;
}

define_visitor(nan_parameter, node_parameter_t)
{
    node_identifier_t *iden = (node_identifier_t *)ast->identifer;
    nwrite_code(iden->name);

    if (ast->expression)
    {
        nwrite_code(" = ");
        ast->expression->accept(ast->expression, visitor);
    }

    return NULL;
}

define_visitor(nan_identifier, node_identifier_t)
{
    (void)visitor;
    nwrite_code(ast->name);
    return NULL;
}

define_visitor(nan_source_elements, node_statements_t)
{
    if (!n_entry)
    {
        nwrite_code("/**\n * ");
        nwrite_code((char *)(nan_visitor.fullname));
        nwrite_code("\n");
        nwrite_code(" * Author - ");
        nwrite_code((char *)(nan_visitor.author));
        nwrite_code("\n");
        nwrite_code(" * Version - ");
        nwrite_code((char *)(nan_visitor.version));
        nwrite_code("\n */ \n\n");

        n_entry = true;
    }

    for (int i = 0; i < ast->nch; i++)
    {
        ast->children[i]->accept(ast->children[i], visitor);
    }
    return NULL;
}

define_visitor(nan_block, node_block_t)
{
    nan_ctx_ident(visitor)++;

    nwrite_code("{\n");

    if (ast->statements)
    {
        ast->statements->accept(ast->statements, visitor);
    }

    nwrite_ident(nan_ctx_ident(visitor) - 1);

    nwrite_code("}");

    nan_ctx_ident(visitor)--;

    return NULL;
}

define_visitor(nan_function_dec, node_function_dec_t)
{

    nwrite_code("function ");
    nwrite_code(ast->name);
    nwrite_code("(");

    if (ast->parameters)
        ast->parameters->accept(ast->parameters, visitor);

    nwrite_code(") ");

    ast->block->accept(ast->block, visitor);

    nwrite_code("\n\n");

    return NULL;
}

define_visitor(nan_function_expression, node_function_expression_t)
{

    nwrite_code("(");

    if (ast->parameters)
        ast->parameters->accept(ast->parameters, visitor);

    nwrite_code(") => ");

    ast->block->accept(ast->block, visitor);

    return NULL;
}

define_visitor(nan_call, node_function_call_t)
{
    ast->expression->accept(ast->expression, visitor);
    nwrite_code("(");
    if (ast->arguments)
        ast->arguments->accept(ast->arguments, visitor);
    nwrite_code(")");

    return NULL;
}

define_visitor(nan_arguments, node_arguments_t)
{
    for (int i = 0; i < ast->nargs; i++)
    {
        ast->args[i]->accept(ast->args[i], visitor);
        if (i != ast->nargs - 1)
            nwrite_code(", ");
    }

    return NULL;
}

define_visitor(nan_symbol, node_symbol_t)
{
    (void)visitor;

    nwrite_code(ast->name);

    return NULL;
}

define_visitor(nan_string, node_string_t)
{
    (void)visitor;

    nwrite_code(ast->str);

    return NULL;
}

define_visitor(nan_return, node_return_t)
{
    nwrite_code("return");

    if (ast->expression)
    {
        nwrite_code(" ");
        ast->expression->accept(ast->expression, visitor);
    }

    nwrite_code(";\n");

    return NULL;
}

define_visitor(nan_eof, node_ast_t)
{
    (void)visitor;
    (void)ast;

    nwrite_code("\n");

    return NULL;
}

define_visitor(nan_break, node_ast_t)
{
    (void)visitor;
    (void)ast;
    nwrite_code("break;\n");

    return NULL;
}

define_visitor(nan_if_else, node_if_else_t)
{
    if (ast->newline)
        nwrite_ident(nan_ctx_ident(visitor));

    nwrite_code("if (");
    ast->condition->accept(ast->condition, visitor);
    nwrite_code(") ");

    if (ast->then_block->type != NODE_BLOCK)
    {
        nwrite_code("\n");
        nwrite_ident(1);
    }

    ast->then_block->accept(ast->then_block, visitor);

    if (ast->else_block)
    {

        if (ast->else_block->type == NODE_IF_ELSE)
            ((node_if_else_t *)(ast->else_block))->newline = false;

        if (ast->then_block->type == NODE_BLOCK)
            nwrite_code(" ");
        else
            nwrite_ident(nan_ctx_ident(visitor));

        nwrite_code("else ");

        if (ast->else_block->type != NODE_IF_ELSE &&
            ast->else_block->type != NODE_BLOCK)
        {
            nwrite_code("\n");
            nwrite_ident(1);
        }

        ast->else_block->accept(ast->else_block, visitor);
    }

    nwrite_code("\n");

    return NULL;
}

define_visitor(nan_ternary, node_ternary_t)
{
    ast->condition->accept(ast->condition, visitor);
    nwrite_code(" ? ");
    ast->then_block->accept(ast->then_block, visitor);
    nwrite_code(" : ");
    ast->else_block->accept(ast->else_block, visitor);

    return NULL;
}

define_visitor(nan_do, node_do_t)
{
    nwrite_code("do ");
    ast->statement->accept(ast->statement, visitor);
    nwrite_code(" while (");
    ast->expression->accept(ast->expression, visitor);
    nwrite_code(");\n");

    return NULL;
}

define_visitor(nan_while, node_while_t)
{
    nwrite_code("while (");
    ast->expression->accept(ast->expression, visitor);
    nwrite_code(") ");
    ast->statement->accept(ast->statement, visitor);
    nwrite_code("\n");

    return NULL;
}

define_visitor(nan_continue, node_ast_t)
{
    (void)visitor;
    (void)ast;
    nwrite_code("continue\n");

    return NULL;
}

define_visitor(nan_unary, node_unary_t)
{
    nwrite_ops(ast->op);
    ast->expression->accept(ast->expression, visitor);

    return NULL;
}

define_visitor(nan_postfix, node_postfix_t)
{
    ast->expression->accept(ast->expression, visitor);
    nwrite_ops(ast->op);

    return NULL;
}

void nan_init(void)
{
    nan_visitor.code = sarray_init(500);
}

void save_js(node_visitor_t *visitor)
{
    uint8_t *code = visitor->code->data;
    char *token;
    token = strtok(filename_g, ".");

    if (token)
    {
        char *fn = malloc(strlen(token) + 5);
        strcpy(fn, token);
        strcat(fn, ".js");

        FILE *file = fopen(fn, "w");

        if (!file)
            return;

        fwrite(code, 1, visitor->code->used_size, file);
        fclose(file);

        free(fn);
    }

    sarray_free(visitor->code);
}

void nan_exit(node_visitor_t *visitor)
{
    save_js(visitor);
}

void *nan_entry(node_visitor_t *visitor, node_ast_t *ast)
{
    (void)visitor;
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
        nwrite_comment(nan_ctx_comment(visitor));
        nwrite_ident(nan_ctx_ident(visitor));
        break;

    default:
        break;
    }
    return NULL;
}

void *nan_leave(node_visitor_t *visitor, node_ast_t *ast)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

nan_ctx_t nan_ctx = {
    .comment = false,
};

node_visitor_t nan_visitor = {
    .fullname = "NaNsense (JavaScript code generator)",
    .shortname = "js_gen",
    .author = "Kithinji Brian",
    .doc = "Generate JavaScript code from AST",
    .version = "0.0.1",
    .code = NULL,
    .ctx = &nan_ctx,
    .init = nan_init,
    .exit = nan_exit,
    .entry = nan_entry,
    .leave = nan_leave,
    .do_fun = nan_do,
    .eof_fun = nan_eof,
    .block_fun = nan_block,
    .break_fun = nan_break,
    .while_fun = nan_while,
    .unary_fun = nan_unary,
    .array_fun = nan_array,
    .bool_fun = nan_boolean,
    .number_fun = nan_number,
    .symbol_fun = nan_symbol,
    .return_fun = nan_return,
    .string_fun = nan_string,
    .postfix_fun = nan_postfix,
    .ternary_fun = nan_ternary,
    .if_else_fun = nan_if_else,
    .variable_fun = nan_variable,
    .continue_fun = nan_continue,
    .function_call_fun = nan_call,
    .arguments_fun = nan_arguments,
    .parameter_fun = nan_parameter,
    .expression_fun = nan_expression,
    .identifier_fun = nan_identifier,
    .array_access_fun = nan_array_access,
    .function_dec_fun = nan_function_dec,
    .statements_fun = nan_source_elements,
    .variable_list_fun = nan_variable_list,
    .parameter_list_fun = nan_parameter_list,
    .binary_expression_fun = nan_binary_expression,
    .variable_statement_fun = nan_variable_statement,
    .function_expression_fun = nan_function_expression,
    .expression_statement_fun = nan_expression_statement,
};