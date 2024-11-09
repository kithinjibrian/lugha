#ifndef ERROR_H
#define ERROR_H

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "bison.tab.h"
#include "parser/ptree.h"
#include "parser/module.h"

#define COLOR_RESET "\033[0m"
#define COLOR_RED "\033[31m"
#define COLOR_YELLOW "\033[33m"
#define COLOR_CYAN "\033[36m"
#define COLOR_MAGENTA "\033[35m"
#define COLOR_GREEN "\033[32m"
#define COLOR_BOLD "\033[1m"

#define ERROR COLOR_RED COLOR_BOLD "error: " COLOR_RESET
#define SYNTAX_ERROR COLOR_RED COLOR_BOLD "Syntax error: " COLOR_RESET
#define PARSING_ERROR COLOR_YELLOW COLOR_BOLD "Parsing error: " COLOR_RESET
#define SEMANTIC_ERROR COLOR_CYAN COLOR_BOLD "Semantic error: " COLOR_RESET

typedef enum
{
    ERROR_TYPE,
    ERROR_SYNTAX,
    ERROR_PARSING,
    ERROR_GENERAL,
    ERROR_SEMANTIC,
    ERROR_CODE_GEN,
} error_type_e;

extern path_t *path_g;

int count_digits(int num);
void yyerror(const char *msg);

void error(YYLTYPE loc, error_type_e type, const char *msg, ...);

#endif /* ERROR_H */