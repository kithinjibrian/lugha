#ifndef SEMA_H
#define SEMA_H

#include <stdlib.h>

#include "ast.h"
#include "visitor.h"
#include "parser/ptree.h"
#include "parser/error.h"

extern char *filename_g;
extern const char *ast_type_str_g[];

#endif /* SEMA_H */