#include "main.h"

path_t *path_g = NULL;

char *output_g = NULL;
char *language_g = "python";
char *build_dir_g = "dist";

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
        path_g = split_path(argv[i], MUST_EXIST);
        module_parse(path_g);
        free_path(path_g);
    }
}

define_main(int)
{
    parse_args(argc, argv);

    char *cache = malloc(strlen(build_dir_g) + 16);
    snprintf(cache, strlen(build_dir_g) + 16, "%s/__cache__.lg", build_dir_g);

    FILE *fp = fopen(cache, "w");
    if (!fp)
        return 1;

    fwrite("cache", 1, 5, fp);

    fclose(fp);

    free(cache);

    return 0;
}