#include "modules/default/types.h"

int type_count = -1;

static char *fresh(module_t *module, int count)
{
    char *name = mod_alloc(module, count_digits(count) + 2);
    sprintf(name, "t%d", count);
    return name;
}

static type_t *new_type_con(module_t *module, node_ast_t *ast, int tag, char *name)
{
    type_t *type = mod_alloc(module, sizeof(type_t));

    type->base.type = NODE_TYPE;

    memcpy(&(type->base.loc), &(ast->loc), sizeof(node_ast_t));

    type->tag = tag;
    type->label = NULL;
    type->con.count = 0;
    type->con.index = 0;
    type->con.name = mod_strdup(module, name);

    return type;
}

type_t *_new_type_var(module_t *module, node_ast_t *ast)
{
    type_t *type = mod_alloc(module, sizeof(type_t));

    type->base.type = NODE_TYPE;

    if (ast)
        memcpy(&(type->base.loc), &(ast->loc), sizeof(node_ast_t));

    type->tag = TYPE_VAR;
    type->label = NULL;

    type->var.name = fresh(module, ++type_count);

    return type;
}

type_t *_new_trec(module_t *module, node_ast_t *ast, int size, char *name)
{
    type_t *type = mod_alloc(module, sizeof(type_t));
    type->base.type = NODE_TYPE;

    memcpy(&(type->base.loc), &(ast->loc), sizeof(node_ast_t));

    type->tag = TYPE_REC;
    type->rec.size = size;
    type->rec.count = 0;
    type->rec.name = mod_strdup(module, name);

    type->rec.table = (trec_kv_t **)mod_alloc(module, sizeof(trec_kv_t *) * size);

    for (int i = 0; i < size; i++)
        type->rec.table[i] = NULL;

    return type;
}

void trec_add(module_t *module, type_t *type, char *label, type_t *value)
{
    uint32_t index = str_hash(label) % type->rec.size;

    trec_kv_t *kv = type->rec.table[index];

    while (kv != NULL)
    {
        if (strcmp(kv->label, label) == 0)
        {
            kv->type = value;
            return;
        }
        kv = kv->next;
    }

    type->rec.count++;

    trec_kv_t *k = mod_alloc(module, sizeof(trec_kv_t));
    k->label = mod_strdup(module, label);
    k->type = value;
    k->next = type->rec.table[index];
    type->rec.table[index] = k;
}

type_t *trec_get(type_t *type, char *label)
{
    uint32_t index = str_hash(label) % type->rec.size;

    trec_kv_t *kv = type->rec.table[index];

    while (kv != NULL)
    {
        if (strcmp(kv->label, label) == 0)
            return kv->type;
        kv = kv->next;
    }

    return NULL;
}

type_t *_new_int(module_t *module, node_ast_t *ast)
{
    return new_type_con(module, ast, TYPE_CON, "int");
}

type_t *_new_bool(module_t *module, node_ast_t *ast)
{
    return new_type_con(module, ast, TYPE_CON, "bool");
}

type_t *_new_str(module_t *module, node_ast_t *ast)
{
    return new_type_con(module, ast, TYPE_CON, "string");
}

type_t *_new_array(module_t *module, node_ast_t *ast)
{
    return new_type_con(module, ast, TYPE_CON, "[]");
}

type_t *_new_fun(module_t *module, node_ast_t *ast, int n, type_t **types, type_t *return_type)
{
    type_t *type = new_type_con(module, ast, TYPE_CON, "->");

    type->con.count = types ? n + 1 : 1;
    type->con.types = (type_t **)mod_alloc(module, sizeof(type_t *) * type->con.count);

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

type_t *type_dup(module_t *module, type_t *type)
{
    type_t *new_type = new_type_con(module, node_ast_cast(type), TYPE_CON, type->con.name);
    new_type->con.count = type->con.count;
    new_type->con.types = type->con.count ? (type_t **)mod_alloc(module, sizeof(type_t *) * type->con.count) : NULL;
    return new_type;
}

bool type_eq(type_t *a, type_t *b)
{
    if (a->tag == TYPE_VAR && b->tag == TYPE_VAR)
        return strcmp(a->var.name, b->var.name) == 0;

    if (a->tag == TYPE_CON && b->tag == TYPE_CON)
    {
        if (strcmp(a->con.name, b->con.name) != 0)
        {
            return false;
        }
        if (a->con.count != b->con.count)
        {
            return false;
        }
        for (int i = 0; i < a->con.count; i++)
        {
            if (!type_eq(a->con.types[i], b->con.types[i]))
                return false;
        }
        return true;
    }

    if (a->tag == TYPE_REC && b->tag == TYPE_REC)
    {
        if (strcmp(a->rec.name, b->rec.name) != 0)
        {
            return false;
        }

        if (a->rec.count != b->rec.count)
        {
            return false;
        }

        for (int i = 0; i < a->rec.size; i++)
        {
            trec_kv_t *kva = a->rec.table[i];
            trec_kv_t *kvb = b->rec.table[i];

            while (kva)
            {
                if (!type_eq(kva->type, kvb->type))
                    return false;

                kva = kva->next;
                kvb = kvb->next;
            }
        }

        return true;
    }

    return false;
}

void _type_str(type_t *type, bool nl)
{
    if (type->tag == TYPE_VAR)
        printf("%s", type->var.name);

    if (type->tag == TYPE_CON)
    {
        if (strcmp(type->con.name, "->") == 0)
        {
            printf("(");
            for (int i = 0; i < type->con.count; i++)
            {
                _type_str(type->con.types[i], false);
                if (i != type->con.count - 1)
                    printf(", ");
            }
            printf(")");
        }
        else
        {
            printf("%s", type->con.name);
        }
    }

    if (type->tag == TYPE_REC)
    {
        printf("%s", type->rec.name);

        if (type->rec.count <= 0)
            return;

        printf(" (");
        for (int i = 0; i < type->rec.size; i++)
        {
            if (type->rec.table[i] != NULL)
            {
                trec_kv_t *kv = type->rec.table[i];
                while (kv)
                {
                    printf("%s: ", kv->label);
                    _type_str(kv->type, false);
                    printf(", ");
                    kv = kv->next;
                }
            }
        }
        printf(")");
    }

    if (nl)
        printf("\n");
}