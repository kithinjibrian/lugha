#include "parser/ptree.h"

const char *ptree_type_str_g[] = {
    PTREE_TYPE(LIST_STRINGIFY)};

ptree_t *ptree_create(ptree_type_e type, int nch, ...)
{
    ptree_t *ptree = (ptree_t *)malloc(sizeof(ptree_t));
    set_loc(ptree);
    ptree->type = type;
    ptree->nch = nch;
    ptree->children = (ptree_t **)malloc(sizeof(ptree_t *) * nch);

    va_list args;

    va_start(args, nch);

    for (int i = 0; i < nch; i++)
    {
        ptree->children[i] = va_arg(args, ptree_t *);
    }

    va_end(args);

    return ptree;
}

ptree_t *ptree_add(ptree_t *ptree, int nch, ...)
{
    ptree->nch += nch;

    ptree->children = (ptree_t **)realloc(ptree->children, sizeof(ptree_t *) * ptree->nch);

    va_list args;
    va_start(args, nch);

    for (int i = 0; i < nch; i++)
    {
        ptree->children[ptree->nch - nch + i] = va_arg(args, ptree_t *);
    }

    va_end(args);

    return ptree;
}

static inline ptree_t *_ptree_create_num(ptree_type_e type, int num)
{
    ptree_t *ptree = (ptree_t *)malloc(sizeof(ptree_t));
    set_loc(ptree);
    ptree->type = type;
    ptree->num = num;
    ptree->nch = 0;
    ptree->children = NULL;

    return ptree;
}

ptree_t *ptree_create_num(int num)
{
    return _ptree_create_num(PTREE_NUM, num);
}

ptree_t *ptree_create_bool(int num)
{
    return _ptree_create_num(PTREE_BOOL, num);
}

static inline ptree_t *_ptree_create_str(ptree_type_e type, char *symbol)
{
    ptree_t *ptree = (ptree_t *)malloc(sizeof(ptree_t));
    set_loc(ptree);
    ptree->type = type;
    ptree->nch = 0;
    ptree->children = NULL;
    ptree->str = symbol;

    return ptree;
}

ptree_t *ptree_create_symbol(char *symbol)
{
    return _ptree_create_str(PTREE_SYMBOL, symbol);
}

ptree_t *ptree_create_str(char *symbol)
{
    return _ptree_create_str(PTREE_STRING, symbol);
}

void ptree_print(ptree_t *ptree, int depth)
{
    if (!ptree)
        return;

    for (int i = 0; i < depth; i++)
        printf("  "); // Indentation

    switch (ptree->type)
    {
    case PTREE_ASSIGN:
        printf("=\n");
        break;
    case PTREE_NUM:
        printf("%d\n", ptree->num);
        break;
    case PTREE_STRING:
    case PTREE_SYMBOL:
        printf("%s\n", ptree->str);
        break;
    default:
        printf("%s\n", ptree_type_str_g[ptree->type]);
        break;
    }

    for (int i = 0; i < ptree->nch; i++)
    {
        ptree_print(ptree->children[i], depth + 1);
    }
}

void ptree_free(ptree_t *ptree)
{
    if (!ptree)
        return;

    for (int i = 0; i < ptree->nch; i++)
    {
        ptree_free(ptree->children[i]);
    }

    if (ptree->type == PTREE_STRING ||
        ptree->type == PTREE_SYMBOL)
        free(ptree->str);

    if (ptree->children)
        free(ptree->children);

    free(ptree);
}