#include "main.h"

char *filename_g;
ptree_t *ptree_g;
const proc_ast_t *proc_ast_g = &node_ast;

char *output_g = NULL;
char *language_g = "python";

void print_usage()
{
    printf("Usage: program [-o file] [-l language] [-h] [-v]\n");
    printf("Options:\n");
    printf("  -o <file>         Specify file output\n");
    printf("  -l <language>     Specify programming langauge\n");
    printf("  -h                Display this help message\n");
    printf("  -v                Enable verbose mode\n");
}

void clean_globals()
{
    symtab_free();
    ptree_free(ptree_g);
}

void parse_args(int argc, char **argv)
{
    static struct option long_options[] = {
        {"output", required_argument, 0, 'o'},
        {"language", required_argument, 0, 'l'},
        {"help", no_argument, 0, 'h'},
        {"verbose", no_argument, 0, 'v'},
        {0, 0, 0, 0}};

    int opt;
    while ((opt = getopt_long(argc, argv, "o:l:hv", long_options, NULL)) != -1)
    {
        switch (opt)
        {
        case 'o':
            output_g = optarg;
            break;
        case 'l':
            language_g = optarg;
            break;
        case 'h':
            print_usage();
            break;
        case 'v':
            printf("Verbose\n");
            break;
        default:
            print_usage();
            break;
        }
    }
}

define_main(int)
{
    // if (argc > 2)
    // {
    //     perror("file not specified");
    //     return 1;
    // }

    filename_g = argv[1];

    parse_args(argc, argv);

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