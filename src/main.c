#include "main.h"

char *filename_g;
ptree_t *ptree_g;
const proc_ast_t *proc_ast_g = &node_ast;

void clean_globals()
{
    symtab_free();
    ptree_free(ptree_g);
}

define_main(int)
{
    if (argc != 2)
    {
        perror("file not specified");
        return 1;
    }

    filename_g = argv[1];

    yyin = fopen(filename_g, "r");
    if (!yyin)
    {
        perror("file not found");
        return 1;
    }

    enter_scope("root");

    int flag = yyparse();

    fclose(yyin);

    //    ptree_print(ptree_g, 0);

    proc_ast_g->init(ptree_g);
    proc_ast_g->exit();

    clean_globals();

    return flag;
}