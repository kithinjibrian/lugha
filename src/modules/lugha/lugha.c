#include "modules/lugha/lugha.h"

#define lugha_ctx_cast(visitor) ((lugha_ctx_t *)((visitor)->ctx))
#define lugha_ctx_ident(visitor) lugha_ctx_cast(visitor)->ident
#define lugha_ctx_comment(visitor) lugha_ctx_cast(visitor)->comment

typedef struct lugha_ctx
{
    int ident;
    bool comment;
} lugha_ctx_t;

static void lwrite_code(char *code)
{
    sarray_push(lugha_visitor.code, code, strlen(code));
}

static void lwrite_ident(int ident)
{
    if (ident <= 0)
        return;

    for (int i = 0; i < ident; i++)
    {
        lwrite_code("    ");
    }
}

static void lwrite_comment(bool comment)
{
    if (comment)
        lwrite_code("// ");
}

static inline void nwrite_ops(op_type_e op)
{
    switch (op)
    {
    case OP_OR:
        lwrite_code(" || ");
        break;
    case OP_AND:
        lwrite_code(" && ");
        break;
    case OP_ASSIGN:
        lwrite_code(" = ");
        break;
    case OP_IS_EQUAL:
        lwrite_code(" == ");
        break;
    case OP_DIV:
        lwrite_code(" / ");
        break;
    case OP_MOD:
        lwrite_code(" % ");
        break;
    case OP_PLUS:
        lwrite_code(" + ");
        break;
    case OP_MINUS:
        lwrite_code(" - ");
        break;
    case OP_MULT:
        lwrite_code(" * ");
        break;
    case OP_INCREMENT:
        lwrite_code("++");
        break;
    case OP_DECREMENT:
        lwrite_code("--");
        break;
    case OP_NOT:
        lwrite_code("!");
        break;
    case OP_LESS:
        lwrite_code(" < ");
        break;
    case OP_GREATER:
        lwrite_code(" > ");
        break;
    default:
        break;
    }
}

static void lugha_ctx_reset(lugha_ctx_t *ctx)
{
    ctx->comment = false;
}

int l_entry = 0;

define_visitor(lugha_expression, node_expression_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(lugha_expression_statement, node_expression_t)
{
    ast->expression->accept(ast->expression, visitor);
    lwrite_code(";\n");
    return NULL;
}

define_visitor(lugha_binary_expression, node_binary_t)
{
    ast->left->accept(ast->left, visitor);
    nwrite_ops(ast->op);
    ast->right->accept(ast->right, visitor);
    return NULL;
}

define_visitor(lugha_number, node_number_t)
{
    (void)visitor;

    char str[20];
    sprintf(str, "%d", ast->num);

    lwrite_code(str);
    return NULL;
}

define_visitor(lugha_array, node_array_t)
{
    (void)visitor;
    lwrite_code("[");

    for (int i = 0; i < ast->nch; i++)
    {
        ast->elements[i]->accept(ast->elements[i], visitor);
        if (i != ast->nch - 1)
            lwrite_code(", ");
    }

    lwrite_code("]");

    return NULL;
}

define_visitor(lugha_array_access, node_array_access_t)
{
    ast->array->accept(ast->array, visitor);
    lwrite_code("[");
    ast->index->accept(ast->index, visitor);
    lwrite_code("]");
    return NULL;
}

define_visitor(lugha_boolean, node_bool_t)
{
    (void)visitor;
    lwrite_code(ast->bol ? "true" : "false");
    return NULL;
}

define_visitor(lugha_variable_statement, node_variable_statement_t)
{
    if (ast->is_const)
        lwrite_code("const ");
    else
        lwrite_code("let ");

    ast->statement->accept(ast->statement, visitor);
    lwrite_code(";\n");
    return NULL;
}

define_visitor(lugha_variable_list, node_variable_list_t)
{
    for (int i = 0; i < ast->nvars; i++)
    {
        ast->variables[i]->accept(ast->variables[i], visitor);
        if (i != ast->nvars - 1)
            lwrite_code(", ");
    }
    return NULL;
}

define_visitor(lugha_parameter_list, node_parameter_list_t)
{
    for (int i = 0; i < ast->params_no; i++)
    {
        ast->parameters[i]->accept(ast->parameters[i], visitor);
        if (i != ast->params_no - 1)
            lwrite_code(", ");
    }
    return NULL;
}

define_visitor(lugha_variable, node_variable_t)
{
    node_identifier_t *iden = (node_identifier_t *)ast->identifer;
    lwrite_code(iden->name);

    if (ast->expression)
    {
        lwrite_code(" = ");
        ast->expression->accept(ast->expression, visitor);
    }
    return NULL;
}

define_visitor(lugha_parameter, node_parameter_t)
{
    node_identifier_t *iden = (node_identifier_t *)ast->identifer;
    lwrite_code(iden->name);

    if (ast->expression)
    {
        lwrite_code(" = ");
        ast->expression->accept(ast->expression, visitor);
    }

    return NULL;
}

define_visitor(lugha_identifier, node_identifier_t)
{
    (void)visitor;
    lwrite_code(ast->name);
    return NULL;
}

define_visitor(lugha_source_elements, node_statements_t)
{
    if (!l_entry)
    {
        lwrite_code("/**\n * ");
        lwrite_code((char *)(lugha_visitor.fullname));
        lwrite_code("\n");
        lwrite_code(" * Author - ");
        lwrite_code((char *)(lugha_visitor.author));
        lwrite_code("\n");
        lwrite_code(" * Version - ");
        lwrite_code((char *)(lugha_visitor.version));
        lwrite_code("\n */ \n\n");

        l_entry = true;
    }

    for (int i = 0; i < ast->nch; i++)
    {
        ast->children[i]->accept(ast->children[i], visitor);
    }
    return NULL;
}

define_visitor(lugha_block, node_block_t)
{
    lugha_ctx_ident(visitor)++;

    lwrite_code("{\n");

    if (ast->statements)
    {
        ast->statements->accept(ast->statements, visitor);
    }

    lwrite_ident(lugha_ctx_ident(visitor) - 1);

    lwrite_code("}");

    lugha_ctx_ident(visitor)--;

    return NULL;
}

define_visitor(lugha_function_dec, node_function_dec_t)
{

    lwrite_code("fun ");
    lwrite_code(ast->name);
    lwrite_code("(");

    if (ast->parameters)
        ast->parameters->accept(ast->parameters, visitor);

    lwrite_code(") ");

    ast->block->accept(ast->block, visitor);

    lwrite_code("\n\n");

    return NULL;
}

define_visitor(lugha_function_expression, node_function_expression_t)
{

    lwrite_code("fun (");

    if (ast->parameters)
        ast->parameters->accept(ast->parameters, visitor);

    lwrite_code(") -> ");

    ast->block->accept(ast->block, visitor);

    return NULL;
}

define_visitor(lugha_call, node_function_call_t)
{
    ast->expression->accept(ast->expression, visitor);
    lwrite_code("(");
    if (ast->arguments)
        ast->arguments->accept(ast->arguments, visitor);
    lwrite_code(")");

    return NULL;
}

define_visitor(lugha_arguments, node_arguments_t)
{
    for (int i = 0; i < ast->nargs; i++)
    {
        ast->args[i]->accept(ast->args[i], visitor);
        if (i != ast->nargs - 1)
            lwrite_code(", ");
    }

    return NULL;
}

define_visitor(lugha_symbol, node_symbol_t)
{
    (void)visitor;

    lwrite_code(ast->name);

    return NULL;
}

define_visitor(lugha_string, node_string_t)
{
    (void)visitor;

    lwrite_code(ast->str);

    return NULL;
}

define_visitor(lugha_return, node_return_t)
{
    lwrite_code("return");

    if (ast->expression)
    {
        lwrite_code(" ");
        ast->expression->accept(ast->expression, visitor);
    }

    lwrite_code(";\n");

    return NULL;
}

define_visitor(lugha_eof, node_ast_t)
{
    (void)visitor;
    (void)ast;

    lwrite_code("\n");

    return NULL;
}

define_visitor(lugha_break, node_ast_t)
{
    (void)visitor;
    (void)ast;
    lwrite_code("break;\n");

    return NULL;
}

define_visitor(lugha_if_else, node_if_else_t)
{
    if (ast->newline)
        lwrite_ident(lugha_ctx_ident(visitor));

    lwrite_code("if (");
    ast->condition->accept(ast->condition, visitor);
    lwrite_code(") ");

    if (ast->then_block->type != NODE_BLOCK)
    {
        lwrite_code("\n");
        lwrite_ident(1);
    }

    ast->then_block->accept(ast->then_block, visitor);

    if (ast->else_block)
    {

        if (ast->else_block->type == NODE_IF_ELSE)
            ((node_if_else_t *)(ast->else_block))->newline = false;

        if (ast->then_block->type == NODE_BLOCK)
            lwrite_code(" ");
        else
            lwrite_ident(lugha_ctx_ident(visitor));

        lwrite_code("else ");

        if (ast->else_block->type != NODE_IF_ELSE &&
            ast->else_block->type != NODE_BLOCK)
        {
            lwrite_code("\n");
            lwrite_ident(1);
        }

        ast->else_block->accept(ast->else_block, visitor);
    }

    lwrite_code("\n");

    return NULL;
}

define_visitor(lugha_ternary, node_ternary_t)
{
    ast->condition->accept(ast->condition, visitor);
    lwrite_code(" ? ");
    ast->then_block->accept(ast->then_block, visitor);
    lwrite_code(" : ");
    ast->else_block->accept(ast->else_block, visitor);

    return NULL;
}

define_visitor(lugha_do, node_do_t)
{
    lwrite_code("do ");
    ast->statement->accept(ast->statement, visitor);
    lwrite_code(" while (");
    ast->expression->accept(ast->expression, visitor);
    lwrite_code(");\n");

    return NULL;
}

define_visitor(lugha_while, node_while_t)
{
    lwrite_code("while (");
    ast->expression->accept(ast->expression, visitor);
    lwrite_code(") ");
    ast->statement->accept(ast->statement, visitor);
    lwrite_code("\n");

    return NULL;
}

define_visitor(lugha_continue, node_ast_t)
{
    (void)visitor;
    (void)ast;
    lwrite_code("continue\n");

    return NULL;
}

define_visitor(lugha_unary, node_unary_t)
{
    nwrite_ops(ast->op);
    ast->expression->accept(ast->expression, visitor);

    return NULL;
}

define_visitor(lugha_postfix, node_postfix_t)
{
    ast->expression->accept(ast->expression, visitor);
    nwrite_ops(ast->op);

    return NULL;
}

void lugha_init(void)
{
    lugha_visitor.code = sarray_init(500);
}

void save_lugha(node_visitor_t *visitor)
{
    uint8_t *code = visitor->code->data;
    char *token;
    token = strtok(filename_g, ".");

    if (token)
    {
        char *fn = malloc(strlen(token) + 5);
        strcpy(fn, token);
        strcat(fn, ".lg");

        FILE *file = fopen(fn, "w");

        if (!file)
            return;

        fwrite(code, 1, visitor->code->used_size, file);
        fclose(file);

        free(fn);
    }

    sarray_free(visitor->code);
}

void lugha_exit(node_visitor_t *visitor)
{
    save_lugha(visitor);
}

void *lugha_entry(node_visitor_t *visitor, node_ast_t *ast)
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
        lwrite_comment(lugha_ctx_comment(visitor));
        lwrite_ident(lugha_ctx_ident(visitor));
        break;

    default:
        break;
    }
    return NULL;
}

void *lugha_leave(node_visitor_t *visitor, node_ast_t *ast)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

lugha_ctx_t lugha_ctx = {
    .comment = false,
};

node_visitor_t lugha_visitor = {
    .fullname = "Lugha (Lugha code generator)",
    .shortname = "la_gen",
    .author = "Kithinji Brian",
    .doc = "Generate Lugha code from AST",
    .version = "0.0.1",
    .code = NULL,
    .ctx = &lugha_ctx,
    .init = lugha_init,
    .exit = lugha_exit,
    .entry = lugha_entry,
    .leave = lugha_leave,
    .do_fun = lugha_do,
    .eof_fun = lugha_eof,
    .block_fun = lugha_block,
    .break_fun = lugha_break,
    .while_fun = lugha_while,
    .unary_fun = lugha_unary,
    .array_fun = lugha_array,
    .bool_fun = lugha_boolean,
    .number_fun = lugha_number,
    .symbol_fun = lugha_symbol,
    .return_fun = lugha_return,
    .string_fun = lugha_string,
    .postfix_fun = lugha_postfix,
    .ternary_fun = lugha_ternary,
    .if_else_fun = lugha_if_else,
    .variable_fun = lugha_variable,
    .continue_fun = lugha_continue,
    .function_call_fun = lugha_call,
    .arguments_fun = lugha_arguments,
    .parameter_fun = lugha_parameter,
    .expression_fun = lugha_expression,
    .identifier_fun = lugha_identifier,
    .array_access_fun = lugha_array_access,
    .function_dec_fun = lugha_function_dec,
    .statements_fun = lugha_source_elements,
    .variable_list_fun = lugha_variable_list,
    .parameter_list_fun = lugha_parameter_list,
    .binary_expression_fun = lugha_binary_expression,
    .variable_statement_fun = lugha_variable_statement,
    .function_expression_fun = lugha_function_expression,
    .expression_statement_fun = lugha_expression_statement,
};