#ifndef MAIN_H
#define MAIN_H

#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>

#include "dsa/hset.h"
#include "file/path.h"
#include "dsa/tuple.h"
#include "parser/ast.h"
#include "parser/ptree.h"
#include "memory/arena.h"
#include "parser/module.h"

extern FILE *yyin;
extern int yyparse();

#define define_main(type) type main(int argc, char **argv)

#endif /* MAIN_H */