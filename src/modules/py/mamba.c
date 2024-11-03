#include "modules/py/mamba.h"

#define mamba_ctx_cast(visitor) ((mamba_ctx_t *)((visitor)->ctx))
#define mamba_ctx_ident(visitor) mamba_ctx_cast(visitor)->ident
#define mamba_ctx_comment(visitor) mamba_ctx_cast(visitor)->comment

typedef struct mamba_ctx
{
    int ident;
    bool comment;
} mamba_ctx_t;

int entry = 0;

static void mwrite_code(char *code)
{
    sarray_push(mamba_visitor.code, code, strlen(code));
}

static void mwrite_ident(int ident)
{
    for (int i = 0; i < ident; i++)
    {
        mwrite_code("    ");
    }
}

static void mwrite_comment(bool comment)
{
    if (comment)
        mwrite_code("# ");
}

static void mamba_ctx_reset(mamba_ctx_t *ctx)
{
    ctx->comment = false;
}

static inline void write_ops(op_type_e op)
{
    switch (op)
    {
    case OP_OR:
        mwrite_code(" || ");
        break;
    case OP_AND:
        mwrite_code(" && ");
        break;
    case OP_ASSIGN:
        mwrite_code(" = ");
        break;
    case OP_IS_EQUAL:
        mwrite_code(" == ");
        break;
    case OP_DIV:
        mwrite_code(" / ");
        break;
    case OP_MOD:
        mwrite_code(" % ");
        break;
    case OP_PLUS:
        mwrite_code(" + ");
        break;
    case OP_MINUS:
        mwrite_code(" - ");
        break;
    case OP_MULT:
        mwrite_code(" * ");
        break;
    case OP_INCREMENT:
        mwrite_code(" += 1");
        break;
    case OP_DECREMENT:
        mwrite_code(" -= 1");
        break;
    case OP_NOT:
        mwrite_code("not ");
        break;
    case OP_LESS:
        mwrite_code(" < ");
        break;
    case OP_GREATER:
        mwrite_code(" > ");
        break;
    default:
        break;
    }
}

void mamba_error(node_ast_t *ast, char *message)
{
    mwrite_code(" # CODE GEN ERROR: ");
    mwrite_code(message);
    mwrite_code("\n");
    error(ast->loc, ERROR_CODE_GEN, message);

    mamba_ctx_comment(&mamba_visitor) = true;
}

void mamba_init(void)
{
    mamba_visitor.code = sarray_init(500);
}

define_visitor(mamba_expression, node_expression_t)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

define_visitor(mamba_expression_statement, node_expression_t)
{
    ast->expression->accept(ast->expression, visitor);
    mwrite_code("\n");
    return NULL;
}

define_visitor(mamba_binary_expression, node_binary_t)
{
    ast->left->accept(ast->left, visitor);
    write_ops(ast->op);
    ast->right->accept(ast->right, visitor);
    return NULL;
}

define_visitor(mamba_number, node_number_t)
{
    (void)visitor;

    char str[20];
    sprintf(str, "%d", ast->num);

    mwrite_code(str);
    return NULL;
}

define_visitor(mamba_array, node_array_t)
{
    (void)visitor;
    mwrite_code("[");

    for (int i = 0; i < ast->nch; i++)
    {
        ast->elements[i]->accept(ast->elements[i], visitor);
        if (i != ast->nch - 1)
            mwrite_code(", ");
    }

    mwrite_code("]");

    return NULL;
}

define_visitor(mamba_array_access, node_array_access_t)
{
    ast->array->accept(ast->array, visitor);
    mwrite_code("[");
    ast->index->accept(ast->index, visitor);
    mwrite_code("]");
    return NULL;
}

define_visitor(mamba_boolean, node_bool_t)
{
    (void)visitor;
    mwrite_code(ast->bol ? "True" : "False");
    return NULL;
}

define_visitor(mamba_variable_statement, node_variable_statement_t)
{
    ast->statement->accept(ast->statement, visitor);
    mwrite_code("\n");
    return NULL;
}

define_visitor(mamba_variable_list, node_variable_list_t)
{
    for (int i = 0; i < ast->nvars; i++)
    {
        ast->variables[i]->accept(ast->variables[i], visitor);
        if (i != ast->nvars - 1)
            mwrite_code(", ");
    }
    return NULL;
}

define_visitor(mamba_parameter_list, node_parameter_list_t)
{
    for (int i = 0; i < ast->params_no; i++)
    {
        ast->parameters[i]->accept(ast->parameters[i], visitor);
        if (i != ast->params_no - 1)
            mwrite_code(", ");
    }
    return NULL;
}

define_visitor(mamba_variable, node_variable_t)
{
    node_identifier_t *iden = (node_identifier_t *)ast->identifer;
    mwrite_code(iden->name);

    if (ast->expression)
    {
        mwrite_code(" = ");
        ast->expression->accept(ast->expression, visitor);
    }
    return NULL;
}

define_visitor(mamba_parameter, node_parameter_t)
{
    node_identifier_t *iden = (node_identifier_t *)ast->identifer;
    mwrite_code(iden->name);

    if (ast->expression)
    {
        mwrite_code(" = ");
        ast->expression->accept(ast->expression, visitor);
    }

    return NULL;
}

define_visitor(mamba_identifier, node_identifier_t)
{
    (void)visitor;
    mwrite_code(ast->name);
    return NULL;
}

define_visitor(mamba_source_elements, node_statements_t)
{
    if (!entry)
    {
        mwrite_code("# ");
        mwrite_code((char *)(mamba_visitor.fullname));
        mwrite_code("\n");
        mwrite_code("# Author - ");
        mwrite_code((char *)(mamba_visitor.author));
        mwrite_code("\n");
        mwrite_code("# Version - ");
        mwrite_code((char *)(mamba_visitor.version));
        mwrite_code("\n\n");

        entry = true;
    }

    for (int i = 0; i < ast->nch; i++)
    {
        ast->children[i]->accept(ast->children[i], visitor);
    }
    return NULL;
}

define_visitor(mamba_block, node_block_t)
{
    mamba_ctx_ident(visitor)++;

    if (ast->statements)
    {
        ast->statements->accept(ast->statements, visitor);
    }
    else
    {
        mwrite_ident(mamba_ctx_ident(visitor));
        mwrite_code("pass\n");
    }

    mamba_ctx_ident(visitor)--;

    return NULL;
}

define_visitor(mamba_function_dec, node_function_dec_t)
{

    mwrite_code("def ");
    mwrite_code(ast->name);
    mwrite_code("(");

    if (ast->parameters)
        ast->parameters->accept(ast->parameters, visitor);

    mwrite_code("):\n");

    ast->block->accept(ast->block, visitor);

    mwrite_code("\n");

    return NULL;
}

define_visitor(mamba_function_expression, node_function_expression_t)
{

    if (ast->block->type == NODE_BLOCK)
    {
        mamba_error(node_ast_cast(ast->block), "Python doesn't accept multiple lines in a lambda function");
    }

    mwrite_comment(mamba_ctx_comment(visitor));

    mwrite_code("lambda ");

    if (ast->parameters)
        ast->parameters->accept(ast->parameters, visitor);

    mwrite_code(": ");

    ast->block->accept(ast->block, visitor);

    mwrite_code("\n");

    mamba_ctx_comment(visitor) = false;

    return NULL;
}

define_visitor(mamba_call, node_function_call_t)
{
    ast->expression->accept(ast->expression, visitor);
    mwrite_code("(");
    if (ast->arguments)
        ast->arguments->accept(ast->arguments, visitor);
    mwrite_code(")");

    return NULL;
}

define_visitor(mamba_arguments, node_arguments_t)
{
    for (int i = 0; i < ast->nargs; i++)
    {
        ast->args[i]->accept(ast->args[i], visitor);
        if (i != ast->nargs - 1)
            mwrite_code(", ");
    }

    return NULL;
}

define_visitor(mamba_symbol, node_symbol_t)
{
    (void)visitor;

    mwrite_code(ast->name);

    return NULL;
}

define_visitor(mamba_string, node_string_t)
{
    (void)visitor;

    mwrite_code(ast->str);

    return NULL;
}

define_visitor(mamba_return, node_return_t)
{
    mwrite_code("return");

    if (ast->expression)
    {
        mwrite_code(" ");
        ast->expression->accept(ast->expression, visitor);
    }

    mwrite_code("\n");

    return NULL;
}

define_visitor(mamba_eof, node_ast_t)
{
    (void)visitor;
    (void)ast;
    mwrite_code("\n");

    return NULL;
}

define_visitor(mamba_break, node_ast_t)
{
    (void)visitor;
    (void)ast;
    mwrite_code("break\n");

    return NULL;
}

define_visitor(mamba_if_else, node_if_else_t)
{
    if (ast->newline)
        mwrite_ident(mamba_ctx_ident(visitor));

    mwrite_code("if ");
    ast->condition->accept(ast->condition, visitor);
    mwrite_code(":\n");
    ast->then_block->accept(ast->then_block, visitor);

    if (ast->else_block)
    {
        mwrite_ident(mamba_ctx_ident(visitor));

        if (ast->else_block->type == NODE_IF_ELSE)
        {
            mwrite_code("el");
            ((node_if_else_t *)(ast->else_block))->newline = false;
        }
        else
        {
            mwrite_code("else:\n");
        }

        ast->else_block->accept(ast->else_block, visitor);
    }

    return NULL;
}

define_visitor(mamba_ternary, node_ternary_t)
{
    ast->then_block->accept(ast->then_block, visitor);
    mwrite_code(" if ");
    ast->condition->accept(ast->condition, visitor);
    mwrite_code(" else ");
    ast->else_block->accept(ast->else_block, visitor);

    return NULL;
}

define_visitor(mamba_continue, node_ast_t)
{
    (void)visitor;
    (void)ast;
    mwrite_code("continue\n");

    return NULL;
}

define_visitor(mamba_do, node_do_t)
{
    mwrite_code("while True:\n");
    ast->statement->accept(ast->statement, visitor);
    mwrite_ident(mamba_ctx_ident(visitor) + 1);
    mwrite_code("if ");
    ast->expression->accept(ast->expression, visitor);
    mwrite_code(":\n");
    mwrite_ident(mamba_ctx_ident(visitor) + 2);
    mwrite_code("break\n");

    return NULL;
}

define_visitor(mamba_while, node_while_t)
{
    mwrite_code("while ");
    ast->expression->accept(ast->expression, visitor);
    mwrite_code(":\n");
    ast->statement->accept(ast->statement, visitor);

    return NULL;
}

define_visitor(mamba_unary, node_unary_t)
{
    write_ops(ast->op);
    ast->expression->accept(ast->expression, visitor);

    return NULL;
}

define_visitor(mamba_postfix, node_postfix_t)
{
    ast->expression->accept(ast->expression, visitor);
    write_ops(ast->op);

    return NULL;
}

void save_code(node_visitor_t *visitor)
{
    uint8_t *code = visitor->code->data;
    char *token;
    token = strtok(filename_g, ".");

    if (token)
    {
        char *fn = malloc(strlen(token) + 5);
        strcpy(fn, token);
        strcat(fn, ".py");

        FILE *file = fopen(fn, "w");

        if (!file)
            return;

        fwrite(code, 1, visitor->code->used_size, file);
        fclose(file);

        free(fn);
    }

    sarray_free(visitor->code);
}

void mamba_exit(node_visitor_t *visitor)
{
    save_code(visitor);
}

void *mamba_entry(node_visitor_t *visitor, node_ast_t *ast)
{
    (void)visitor;
    (void)ast;

    // printf("[ERROR] node type: %s\n", ast_type_str_g[ast->type]);

    switch (ast->type)
    {
    case NODE_DO:
    case NODE_BREAK:
    case NODE_WHILE:
    case NODE_RETURN:
    case NODE_CONTINUE:
    case NODE_EXPRESSION:
    case NODE_VARIABLE_STATEMENT:
        mwrite_comment(mamba_ctx_comment(visitor));
        mwrite_ident(mamba_ctx_ident(visitor));
        break;

    default:
        break;
    }
    return NULL;
}

void *mamba_leave(node_visitor_t *visitor, node_ast_t *ast)
{
    (void)visitor;
    (void)ast;
    return NULL;
}

mamba_ctx_t mamba_ctx = {
    .ident = 0,
    .comment = false,
};

node_visitor_t mamba_visitor = {
    .fullname = "Black Mamba (Python code generator)",
    .shortname = "py_gen",
    .author = "Kithinji Brian",
    .doc = "Generate python code from AST",
    .version = "0.0.1",
    .code = NULL,
    .ctx = &mamba_ctx,
    .init = mamba_init,
    .exit = mamba_exit,
    .entry = mamba_entry,
    .leave = mamba_leave,
    .do_fun = mamba_do,
    .eof_fun = mamba_eof,
    .class_fun = def_class,
    .object_fun = def_object,
    .block_fun = mamba_block,
    .break_fun = mamba_break,
    .while_fun = mamba_while,
    .struct_fun = def_struct,
    .unary_fun = mamba_unary,
    .array_fun = mamba_array,
    .bool_fun = mamba_boolean,
    .number_fun = mamba_number,
    .symbol_fun = mamba_symbol,
    .return_fun = mamba_return,
    .string_fun = mamba_string,
    .postfix_fun = mamba_postfix,
    .ternary_fun = mamba_ternary,
    .if_else_fun = mamba_if_else,
    .variable_fun = mamba_variable,
    .continue_fun = mamba_continue,
    .function_call_fun = mamba_call,
    .arguments_fun = mamba_arguments,
    .parameter_fun = mamba_parameter,
    .expression_fun = mamba_expression,
    .identifier_fun = mamba_identifier,
    .array_access_fun = mamba_array_access,
    .function_dec_fun = mamba_function_dec,
    .statements_fun = mamba_source_elements,
    .variable_list_fun = mamba_variable_list,
    .parameter_list_fun = mamba_parameter_list,
    .binary_expression_fun = mamba_binary_expression,
    .variable_statement_fun = mamba_variable_statement,
    .function_expression_fun = mamba_function_expression,
    .expression_statement_fun = mamba_expression_statement,
};
