#include "parser/error.h"

char *get_line_from_source(int line_number)
{
    FILE *file = fopen(filename_g, "r");
    if (!file)
    {
        fprintf(stderr, "Could not open source file.\n");
        return NULL;
    }

    char *line = NULL;
    size_t len = 0;
    int read;
    int current_line = 1;

    // Read the file line by line
    while ((read = getline(&line, &len, file)) != -1)
    {
        if (current_line == line_number)
        {
            fclose(file);
            return line; // Return the matching line
        }
        current_line++;
    }

    fclose(file);
    free(line);
    return NULL; // Return NULL if the line wasn't found
}

void yyerror(const char *msg)
{
    fprintf(stderr, "%s:%d:%d: %s\n",
            filename_g,
            yylloc.first_line, yylloc.first_column, msg);

    char *line = get_line_from_source(yylloc.first_line);
    if (line)
    {
        fprintf(stderr, " | %s\n | ", line);

        for (int i = 0; i < yylloc.first_column; i++)
        {
            fprintf(stderr, " ");
        }
        fprintf(stderr, COLOR_RED "^\n" COLOR_RESET);
        fprintf(stderr, " \n");
        free(line);
    }
    else
    {
        fprintf(stderr, "Error: Could not retrieve source line.\n");
    }
}

int count_digits(int num)
{
    int count = 0;

    if (num == 0)
    {
        return 1;
    }

    if (num < 0)
    {
        num = -num;
    }

    while (num != 0)
    {
        num /= 10;
        count++;
    }

    return count;
}

void error(YYLTYPE loc, error_type_e type, const char *msg, ...)
{
    fprintf(stderr, "\n%s:%d:%d: ", filename_g, loc.first_line, loc.first_column);

    switch (type)
    {
    case ERROR_TYPE:
        fprintf(stderr, COLOR_MAGENTA COLOR_BOLD "TYPE ERROR\n" COLOR_RESET);
        break;
    case ERROR_SYNTAX:
        fprintf(stderr, COLOR_RED COLOR_BOLD "SYNTAX ERROR\n" COLOR_RESET);
        break;
    case ERROR_PARSING:
        fprintf(stderr, COLOR_YELLOW COLOR_BOLD "PARSING ERROR\n" COLOR_RESET);
        break;
    case ERROR_GENERAL:
        fprintf(stderr, COLOR_RED COLOR_BOLD "GENERAL ERROR\n" COLOR_RESET);
        break;
    case ERROR_SEMANTIC:
        fprintf(stderr, COLOR_CYAN COLOR_BOLD "SEMANTIC ERROR\n" COLOR_RESET);
        break;
    case ERROR_CODE_GEN:
        fprintf(stderr, COLOR_GREEN COLOR_BOLD "CODE GENERATION ERROR\n" COLOR_RESET);
        break;
    }

    char *line = get_line_from_source(loc.first_line);

    if (line)
    {
        fprintf(stderr, " %d | ", loc.first_line);

        fprintf(stderr, "%s", line);

        if (line[strlen(line) - 1] != '\n')
            fprintf(stderr, "\n");

        for (int i = 0; i < count_digits(loc.first_line) + 2; i++)
        {
            fprintf(stderr, " ");
        }

        fprintf(stderr, "| ");

        for (int i = 0; i < loc.first_column; i++)
        {
            fprintf(stderr, " ");
        }

        fprintf(stderr, COLOR_RED "^\n" COLOR_RESET);

        free(line);
    }

    va_list args;
    va_start(args, msg);
    vfprintf(stderr, msg, args);
    va_end(args);

    fprintf(stderr, "\n\n");
}