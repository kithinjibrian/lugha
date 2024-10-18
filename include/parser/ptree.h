#ifndef PARSE_TREE_H
#define PARSE_TREE_H

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "macro.h"
#include "bison.tab.h"

#define PTREE_TYPE(P_TYPE)             \
    P_TYPE(PTREE_IF)                   \
    P_TYPE(PTREE_OR)                   \
    P_TYPE(PTREE_DO)                   \
    P_TYPE(PTREE_DOT)                  \
    P_TYPE(PTREE_FOR)                  \
    P_TYPE(PTREE_NOT)                  \
    P_TYPE(PTREE_AND)                  \
    P_TYPE(PTREE_NUM)                  \
    P_TYPE(PTREE_STR)                  \
    P_TYPE(PTREE_EOF)                  \
    P_TYPE(PTREE_BOOL)                 \
    P_TYPE(PTREE_ELSE)                 \
    P_TYPE(PTREE_CHAR)                 \
    P_TYPE(PTREE_PLUS)                 \
    P_TYPE(PTREE_RPRN)                 \
    P_TYPE(PTREE_LPRN)                 \
    P_TYPE(PTREE_LESS)                 \
    P_TYPE(PTREE_TYPE)                 \
    P_TYPE(PTREE_ENUM)                 \
    P_TYPE(PTREE_LABEL)                \
    P_TYPE(PTREE_EMPTY)                \
    P_TYPE(PTREE_TILDE)                \
    P_TYPE(PTREE_WHILE)                \
    P_TYPE(PTREE_BREAK)                \
    P_TYPE(PTREE_COMMA)                \
    P_TYPE(PTREE_MINUS)                \
    P_TYPE(PTREE_COLON)                \
    P_TYPE(PTREE_BLOCK)                \
    P_TYPE(PTREE_UNION)                \
    P_TYPE(PTREE_UNARY)                \
    P_TYPE(PTREE_FIELDS)               \
    P_TYPE(PTREE_SWITCH)               \
    P_TYPE(PTREE_LBRACE)               \
    P_TYPE(PTREE_RETURN)               \
    P_TYPE(PTREE_RBRACE)               \
    P_TYPE(PTREE_SYMBOL)               \
    P_TYPE(PTREE_STRING)               \
    P_TYPE(PTREE_FACTOR)               \
    P_TYPE(PTREE_DIVIDE)               \
    P_TYPE(PTREE_ASSIGN)               \
    P_TYPE(PTREE_MODULO)               \
    P_TYPE(PTREE_BIT_OR)               \
    P_TYPE(PTREE_STRUCT)               \
    P_TYPE(PTREE_LSHIFT)               \
    P_TYPE(PTREE_RSHIFT)               \
    P_TYPE(PTREE_BIT_XOR)              \
    P_TYPE(PTREE_BIT_AND)              \
    P_TYPE(PTREE_LESS_EQ)              \
    P_TYPE(PTREE_GREATER)              \
    P_TYPE(PTREE_IF_ELSE)              \
    P_TYPE(PTREE_POSTFIX)              \
    P_TYPE(PTREE_METHODS)              \
    P_TYPE(PTREE_T_STRUCT)             \
    P_TYPE(PTREE_LBRACKET)             \
    P_TYPE(PTREE_RBRACKET)             \
    P_TYPE(PTREE_QUESTION)             \
    P_TYPE(PTREE_IS_EQUAL)             \
    P_TYPE(PTREE_CONTINUE)             \
    P_TYPE(PTREE_MULTIPLY)             \
    P_TYPE(PTREE_VARIABLE)             \
    P_TYPE(PTREE_KEYVALUE)             \
    P_TYPE(PTREE_TYPE_LIST)            \
    P_TYPE(PTREE_VOID_TYPE)            \
    P_TYPE(PTREE_NOT_EQUAL)            \
    P_TYPE(PTREE_INCREMENT)            \
    P_TYPE(PTREE_DECREMENT)            \
    P_TYPE(PTREE_SEMICOLON)            \
    P_TYPE(PTREE_PARAMETER)            \
    P_TYPE(PTREE_OR_ASSIGN)            \
    P_TYPE(PTREE_TYPE_ALIAS)           \
    P_TYPE(PTREE_ARRAY_TYPE)           \
    P_TYPE(PTREE_TUPLE_TYPE)           \
    P_TYPE(PTREE_CASE_LABEL)           \
    P_TYPE(PTREE_MUL_ASSIGN)           \
    P_TYPE(PTREE_DIV_ASSIGN)           \
    P_TYPE(PTREE_MOD_ASSIGN)           \
    P_TYPE(PTREE_ADD_ASSIGN)           \
    P_TYPE(PTREE_SUB_ASSIGN)           \
    P_TYPE(PTREE_SHL_ASSIGN)           \
    P_TYPE(PTREE_SHR_ASSIGN)           \
    P_TYPE(PTREE_AND_ASSIGN)           \
    P_TYPE(PTREE_XOR_ASSIGN)           \
    P_TYPE(PTREE_GREATER_EQ)           \
    P_TYPE(PTREE_EXPRESSION)           \
    P_TYPE(PTREE_IDENTIFIER)           \
    P_TYPE(PTREE_STRING_TYPE)          \
    P_TYPE(PTREE_UNIT_STRUCT)          \
    P_TYPE(PTREE_VARIABLE_ID)          \
    P_TYPE(PTREE_KEYWORD_LET)          \
    P_TYPE(PTREE_TYPE_PARAMS)          \
    P_TYPE(PTREE_ENUM_STRUCT)          \
    P_TYPE(PTREE_KEYWORD_FUN)          \
    P_TYPE(PTREE_BOOLEAN_TYPE)         \
    P_TYPE(PTREE_INTEGER_TYPE)         \
    P_TYPE(PTREE_ENUM_DEFAULT)         \
    P_TYPE(PTREE_ENUM_MEMBERS)         \
    P_TYPE(PTREE_METHODS_LIST)         \
    P_TYPE(PTREE_ARRAY_ACCESS)         \
    P_TYPE(PTREE_ELEMENT_LIST)         \
    P_TYPE(PTREE_ARGUMENT_LIST)        \
    P_TYPE(PTREE_LET_STATEMENT)        \
    P_TYPE(PTREE_DEFAULT_LABEL)        \
    P_TYPE(PTREE_OBJECT_ACCESS)        \
    P_TYPE(PTREE_FUNCTION_CALL)        \
    P_TYPE(PTREE_VARIABLE_LIST)        \
    P_TYPE(PTREE_FUNCTION_TYPE)        \
    P_TYPE(PTREE_FORWARD_ARROW)        \
    P_TYPE(PTREE_ARRAY_LITERAL)        \
    P_TYPE(PTREE_PARAMETER_MUT)        \
    P_TYPE(PTREE_OBJECT_LITERAL)       \
    P_TYPE(PTREE_TYPE_REFERENCE)       \
    P_TYPE(PTREE_BACKWARD_ARROW)       \
    P_TYPE(PTREE_VARIABLE_TYPED)       \
    P_TYPE(PTREE_PRIMITIVE_TYPE)       \
    P_TYPE(PTREE_PARAMETER_LIST)       \
    P_TYPE(PTREE_PARAMETER_CONST)      \
    P_TYPE(PTREE_CONST_STATEMENT)      \
    P_TYPE(PTREE_TYPE_ANNOTATION)      \
    P_TYPE(PTREE_TYPED_ARGUMENTS)      \
    P_TYPE(PTREE_SOURCE_ELEMENTS)      \
    P_TYPE(PTREE_TYPED_PARAMETERS)     \
    P_TYPE(PTREE_ENUM_NAMED_STRUCT)    \
    P_TYPE(PTREE_BINARY_EXPRESSION)    \
    P_TYPE(PTREE_VARIABLE_ASSIGNED)    \
    P_TYPE(PTREE_PARAMETER_ASSIGNED)   \
    P_TYPE(PTREE_VARIABLE_STATEMENT)   \
    P_TYPE(PTREE_VARIABLE_UNASSIGNED)  \
    P_TYPE(PTREE_FUNCTION_EXPRESSION)  \
    P_TYPE(PTREE_FUNCTION_DECLARTION)  \
    P_TYPE(PTREE_TERTIARY_EXPRESSION)  \
    P_TYPE(PTREE_PARAMETER_UNASSIGNED) \
    P_TYPE(PTREE_VARIABLE_DECLARATION) \
    P_TYPE(PTREE_EXPRESSION_STATEMENT) \
    P_TYPE(PTREE_ASSIGNMENT_EXPRESSION)

typedef enum
{
    PTREE_TYPE(LIST)
} ptree_type_e;

#define set_loc(_l)                             \
    _l->loc.first_line = yylloc.first_line;     \
    _l->loc.first_column = yylloc.first_column; \
    _l->loc.last_line = yylloc.last_line;       \
    _l->loc.last_column = yylloc.last_column

typedef struct ptree
{
    int nch;                 /* number of children */
    struct ptree **children; /* array of children */
    ptree_type_e type;       /* type of node */
    YYLTYPE loc;
    union
    {
        int num;
        char ch;
        char *str;
    }; /* value of node */
} ptree_t;

extern YYLTYPE yylloc;

void ptree_free(ptree_t *ptree);
ptree_t *ptree_create_num(int num);
ptree_t *ptree_create_bool(int num);
ptree_t *ptree_create_str(char *symbol);
ptree_t *ptree_create_symbol(char *symbol);
void ptree_print(ptree_t *ptree, int depth);
ptree_t *ptree_add(ptree_t *ptree, int nch, ...);
ptree_t *ptree_create(ptree_type_e type, int nch, ...);

#endif /* PARSE_TREE_H */