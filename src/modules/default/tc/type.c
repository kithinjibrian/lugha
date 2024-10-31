#include "modules/default/types.h"

int type_count = -1;

static char *fresh(int count)
{
    char *name = type_alloc(count_digits(count) + 2);
    sprintf(name, "t%d", count);
    return name;
}

static type_t *new_type_con(node_ast_t *ast, int tag, char *name)
{
    type_t *type = type_alloc(sizeof(type_t));

    type->base.type = NODE_TYPE;

    memcpy(&(type->base.loc), &(ast->loc), sizeof(node_ast_t));

    type->tag = tag;
    type->con.count = 0;
    type->con.index = 0;
    type->con.name = type_strdup(name);

    return type;
}

type_t *_new_type_var(node_ast_t *ast)
{
    type_t *type = type_alloc(sizeof(type_t));

    type->base.type = NODE_TYPE;

    memcpy(&(type->base.loc), &(ast->loc), sizeof(node_ast_t));

    type->tag = TYPE_VAR;

    type->var.name = fresh(type_count++);

    return type;
}

type_t *_new_int(node_ast_t *ast)
{
    return new_type_con(ast, TYPE_CON, "int");
}

type_t *_new_bool(node_ast_t *ast)
{
    return new_type_con(ast, TYPE_CON, "bool");
}

type_t *_new_str(node_ast_t *ast)
{
    return new_type_con(ast, TYPE_CON, "string");
}

type_t *_new_array(node_ast_t *ast)
{
    return new_type_con(ast, TYPE_CON, "[]");
}

type_t *_new_fun(node_ast_t *ast, int n, type_t **types, type_t *return_type)
{
    type_t *type = new_type_con(ast, TYPE_CON, "->");

    type->con.count = types ? n + 1 : 1;
    type->con.types = (type_t **)type_alloc(sizeof(type_t *) * type->con.count);

    for (int i = 0; i < type->con.count; i++)
    {
        if (types == NULL)
            type->con.types[i] = return_type;
        else
        {
            if (i != type->con.count - 1)
                type->con.types[i] = types[i];
            else
                type->con.types[i] = return_type;
        }
    }

    return type;
}

type_t *type_dup(type_t *type)
{
    type_t *new_type = new_type_con(node_ast_cast(type), TYPE_CON, type->con.name);
    new_type->con.count = type->con.count;
    new_type->con.types = type->con.count ? (type_t **)type_alloc(sizeof(type_t *) * type->con.count) : NULL;
    return new_type;
}

bool type_eq(type_t *a, type_t *b)
{
    if (strcmp(
            a->tag == TYPE_CON ? a->con.name : a->var.name,
            b->tag == TYPE_CON ? b->con.name : b->var.name) != 0)
    {
        if (a->tag == TYPE_CON &&
            b->tag == TYPE_CON &&
            a->con.count != b->con.count)
        {
            return false;
        }

        return false;
    }

    for (int i = 0; i < a->con.count; i++)
    {
        if (!type_eq(a->con.types[i], b->con.types[i]))
            return false;
    }

    return true;
}

void _type_str(type_t *type, bool nl)
{
    if (type->tag == TYPE_CON)
    {
        if (strcmp(type->con.name, "->") == 0)
        {
            for (int i = 0; i < type->con.count; i++)
            {
                _type_str(type->con.types[i], false);
                if (i != type->con.count - 1)
                    printf(" -> ");
            }
        }
        else
        {
            printf("%s", type->con.name);
        }
    }
    else
    {
        printf("%s", type->var.name);
    }

    if (nl)
        printf("\n");
}