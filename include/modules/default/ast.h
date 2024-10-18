#ifndef NODE_AST_H
#define NODE_AST_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "parser/ast.h"
#include "dsa/sarray.h"
#include "string/str.h"
#include "memory/arena.h"
#include "parser/ptree.h"
#include "parser/error.h"
#include "parser/symtab.h"

#define node_ast_cast(node_ast) ((node_ast_t *)(node_ast))

#define DEFINE_NODE_AST(ptree, _type, _ast_type, _accept_fun) \
    _type *ast = (_type *)ast_alloc(sizeof(_type));           \
    memcpy(&(ast->base.loc), &(ptree->loc), sizeof(YYLTYPE)); \
    ast->base.type = _ast_type;                               \
    ast->base.accept = _accept_fun;                           \
    ast->base.type_str = ast_type_str_g[_ast_type];

#define DEFINE_NODE_SIMPLE(ptree, _ast_type, _accept_fun)          \
    node_ast_t *ast = (node_ast_t *)ast_alloc(sizeof(node_ast_t)); \
    memcpy(&(ast->loc), &(ptree->loc), sizeof(YYLTYPE));           \
    ast->type = _ast_type;                                         \
    ast->accept = _accept_fun;                                     \
    ast->type_str = ast_type_str_g[_ast_type];

#define DEFINE_ACCEPT(_name, _type, _accept_fun)                 \
    void *_name(node_ast_t *ast, node_visitor_t *visitor)        \
    {                                                            \
        visitor->entry(visitor, ast);                            \
        void *ret = visitor->_accept_fun(visitor, (_type *)ast); \
        visitor->leave(visitor, ast);                            \
        return ret;                                              \
    }

struct node_visitor;

#define DATA_TYPE(TYPE)            \
    TYPE(NODE_DO)                  \
    TYPE(NODE_EOF)                 \
    TYPE(NODE_TYPE)                \
    TYPE(NODE_UNARY)               \
    TYPE(NODE_BLOCK)               \
    TYPE(NODE_BREAK)               \
    TYPE(NODE_WHILE)               \
    TYPE(NODE_NUMBER)              \
    TYPE(NODE_SYMBOL)              \
    TYPE(NODE_STRING)              \
    TYPE(NODE_BINARY)              \
    TYPE(NODE_RETURN)              \
    TYPE(NODE_TERNARY)             \
    TYPE(NODE_IF_ELSE)             \
    TYPE(NODE_POSTFIX)             \
    TYPE(NODE_CONTINUE)            \
    TYPE(NODE_VARIABLE)            \
    TYPE(NODE_ARGUMENTS)           \
    TYPE(NODE_PARAMETER)           \
    TYPE(NODE_EXPRESSION)          \
    TYPE(NODE_STATEMENTS)          \
    TYPE(NODE_PARAMETERS)          \
    TYPE(NODE_IDENTIFIER)          \
    TYPE(NODE_FUNCTION_DEC)        \
    TYPE(NODE_VARIABLE_LIST)       \
    TYPE(NODE_FUNCTION_CALL)       \
    TYPE(NODE_PARAMETER_LIST)      \
    TYPE(NODE_VARIABLE_STATEMENT)  \
    TYPE(NODE_FUNCTION_EXPRESSION) \
    TYPE(NODE_EXPRESSION_STATEMENT)

typedef enum
{
    DATA_TYPE(LIST)
} node_type_e;

typedef struct node_ast
{
    node_type_e type;
    YYLTYPE loc;
    const char *type_str;
    void *(*accept)(struct node_ast *, struct node_visitor *);
} node_ast_t;

typedef struct node_number
{
    node_ast_t base;
    int num;
    bool is_bool;
} node_number_t;

typedef struct node_bool
{
    node_ast_t base;
    bool bol;
} node_bool_t;

typedef struct node_symbol
{
    node_ast_t base;
    char *name;
    symbol_t *symbol;
} node_symbol_t;

typedef struct node_string
{
    node_ast_t base;
    char *str;
} node_string_t;

typedef struct node_variable_list
{
    node_ast_t base;
    int nvars;
    node_ast_t **variables;
} node_variable_list_t;

typedef struct node_variable
{
    node_ast_t base;
    bool is_const;
    node_ast_t *identifer;
    node_ast_t *expression;
} node_variable_t;

typedef struct node_parameter_list
{
    node_ast_t base;
    int params_no;
    node_ast_t **parameters;
} node_parameter_list_t;

typedef struct node_parameter
{
    node_ast_t base;
    bool is_mutable;
    node_ast_t *identifer;
    node_ast_t *expression;
} node_parameter_t;

typedef struct node_identifier
{
    node_ast_t base;
    char *name;
    symbol_t *symbol;
    node_ast_t *type;
} node_identifier_t;

typedef enum
{
    TYPE_VOID,
    TYPE_TUPLE,
    TYPE_ARRAY,
    TYPE_STRING,
    TYPE_BOOLEAN,
    TYPE_INTEGER,
    TYPE_FUNCTION,
    TYPE_UNKNOWN,
    TYPE_REFERENCE,
} data_type_e;

typedef struct node_type
{
    node_ast_t base;
    data_type_e data_type;
    union
    {
        size_t tuple_length;
    };
} node_type_t;

typedef struct node_variable_statement
{
    node_ast_t base;
    int nvars;
    node_ast_t *statement;
    node_ast_t **variables;
} node_variable_statement_t;

typedef struct node_function_dec
{
    node_ast_t base;
    char *name;
    symbol_t *symbol;
    node_ast_t *block;
    node_ast_t *parameters;
    node_ast_t *return_type;
    node_ast_t *typed_params;
} node_function_dec_t;

typedef struct node_function_expression
{
    node_ast_t base;
    node_ast_t *block;
    node_ast_t *parameters;
    node_ast_t *return_type;
    node_ast_t *typed_params;
} node_function_expression_t;

typedef struct node_arguments
{
    node_ast_t base;
    int nargs;
    struct node_ast **args;
} node_arguments_t;

typedef struct node_function_call
{
    node_ast_t base;
    node_ast_t *arguments;
    node_ast_t *expression;
} node_function_call_t;

typedef struct node_statements
{
    node_ast_t base;
    int nch;
    node_ast_t **children;
} node_statements_t;

typedef enum
{
    OP_OR,
    OP_NOT,
    OP_DIV,
    OP_MOD,
    OP_AND,
    OP_PLUS,
    OP_MULT,
    OP_LESS,
    OP_MINUS,
    OP_LSHIFT,
    OP_RSHIFT,
    OP_ASSIGN,
    OP_GREATER,
    OP_IS_EQUAL,
    OP_NOT_EQUAL,
    OP_INCREMENT,
    OP_DECREMENT,
    OP_OR_ASSIGN,
    OP_LESS_EQUAL,
    OP_SUB_ASSIGN,
    OP_DIV_ASSIGN,
    OP_MOD_ASSIGN,
    OP_AND_ASSIGN,
    OP_ADD_ASSIGN,
    OP_XOR_ASSIGN,
    OP_BITWISE_OR,
    OP_MULT_ASSIGN,
    OP_BITWISE_XOR,
    OP_BITWISE_AND,
    OP_BITWISE_NOT,
    OP_GREATER_EQUAL,
} op_type_e;

typedef struct node_unary
{
    node_ast_t base;
    op_type_e op;
    bool is_postfix;
    struct node_ast *expression;
} node_unary_t;

typedef struct node_postfix
{
    node_ast_t base;
    op_type_e op;
    struct node_ast *expression;
} node_postfix_t;

typedef struct node_binary
{
    node_ast_t base;
    op_type_e op;
    struct node_ast *left;
    struct node_ast *right;
} node_binary_t;

typedef struct node_if_else
{
    node_ast_t base;
    int newline;
    struct node_ast *condition;
    struct node_ast *then_block;
    struct node_ast *else_block;
} node_if_else_t;

typedef struct node_ternary
{
    node_ast_t base;
    int is_expression;
    int newline;
    struct node_ast *condition;
    struct node_ast *then_block;
    struct node_ast *else_block;
} node_ternary_t;

typedef struct node_expression
{
    node_ast_t base;
    struct node_ast *expression;
} node_expression_t;

typedef struct node_block
{
    node_ast_t base;
    struct node_ast *statements;
} node_block_t;

typedef struct node_return
{
    node_ast_t base;
    struct node_ast *expression;
} node_return_t;

typedef struct node_while
{
    node_ast_t base;
    struct node_ast *statement;
    struct node_ast *expression;
} node_while_t;

typedef struct node_do
{
    node_ast_t base;
    struct node_ast *statement;
    struct node_ast *expression;
} node_do_t;

typedef struct node_visitor
{
    char *fullname;
    char *shortname;
    char *doc;
    char *author;
    sarray_t *code;
    void *ctx;
    void (*init)(void);                                       /* The initialization function: called when the visitor is created */
    void (*exit)(struct node_visitor *);                      /* The exit function: called when the visitor is destroyed */
    void *(*entry)(struct node_visitor *, struct node_ast *); /* The entry function: called when a node is visited */
    void *(*leave)(struct node_visitor *, struct node_ast *); /* The leave function: called when a node is left */
    void *(*do_fun)(struct node_visitor *, struct node_do *);
    void *(*eof_fun)(struct node_visitor *, struct node_ast *);
    void *(*bool_fun)(struct node_visitor *, struct node_bool *);
    void *(*break_fun)(struct node_visitor *, struct node_ast *);
    void *(*type_fun)(struct node_visitor *, struct node_type *);
    void *(*while_fun)(struct node_visitor *, struct node_while *);
    void *(*unary_fun)(struct node_visitor *, struct node_unary *);
    void *(*block_fun)(struct node_visitor *, struct node_block *);
    void *(*continue_fun)(struct node_visitor *, struct node_ast *);
    void *(*number_fun)(struct node_visitor *, struct node_number *);
    void *(*return_fun)(struct node_visitor *, struct node_return *);
    void *(*symbol_fun)(struct node_visitor *, struct node_symbol *);
    void *(*string_fun)(struct node_visitor *, struct node_string *);
    void *(*postfix_fun)(struct node_visitor *, struct node_postfix *);
    void *(*ternary_fun)(struct node_visitor *, struct node_ternary *);
    void *(*if_else_fun)(struct node_visitor *, struct node_if_else *);
    void *(*variable_fun)(struct node_visitor *, struct node_variable *);
    void *(*arguments_fun)(struct node_visitor *, struct node_arguments *);
    void *(*parameter_fun)(struct node_visitor *, struct node_parameter *);
    void *(*identifier_fun)(struct node_visitor *, struct node_identifier *);
    void *(*expression_fun)(struct node_visitor *, struct node_expression *);
    void *(*statements_fun)(struct node_visitor *, struct node_statements *);
    void *(*binary_expression_fun)(struct node_visitor *, struct node_binary *);
    void *(*function_dec_fun)(struct node_visitor *, struct node_function_dec *);
    void *(*variable_list_fun)(struct node_visitor *, struct node_variable_list *);
    void *(*function_call_fun)(struct node_visitor *, struct node_function_call *);
    void *(*parameter_list_fun)(struct node_visitor *, struct node_parameter_list *);
    void *(*expression_statement_fun)(struct node_visitor *, struct node_expression *);
    void *(*variable_statement_fun)(struct node_visitor *, struct node_variable_statement *);
    void *(*function_expression_fun)(struct node_visitor *, struct node_function_expression *);
} node_visitor_t;

extern node_visitor_t type_visitor;
extern node_visitor_t sema_visitor;
extern node_visitor_t mamba_visitor;

extern const char *ptree_type_str_g[];

#endif /* NODE_AST_H */