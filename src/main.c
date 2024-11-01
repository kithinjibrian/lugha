#include "main.h"

char *filename_g;
ptree_t *ptree_g;
const proc_ast_t *proc_ast_g = &node_ast;

char *output_g = NULL;
char *language_g = "python";
char *build_dir_g = "build";

void print_usage()
{
    printf("Usage: program [-o file] [-l language] [-b directory] [-h] [-v]\n");
    printf("Options:\n");
    printf("  -o <file>         Specify file output\n");
    printf("  -l <language>     Specify programming langauge\n");
    printf("  -b <build>        Specify build directory\n");
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
        {"build", required_argument, 0, 'b'},
        {"help", no_argument, 0, 'h'},
        {"verbose", no_argument, 0, 'v'},
        {0, 0, 0, 0}};

    int opt;
    while ((opt = getopt_long(argc, argv, "o:l:b:hv", long_options, NULL)) != -1)
    {
        switch (opt)
        {
        case 'o':
            output_g = optarg;
            break;
        case 'l':
            language_g = optarg;
            break;
        case 'b':
            build_dir_g = optarg;
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

    for (int i = optind; i < argc; i++)
    {
        filename_g = argv[i];
    }
}

define_main(int)
{

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