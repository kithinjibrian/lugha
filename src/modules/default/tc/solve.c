#include "modules/default/types.h"

bool occurs_check(type_t *a, type_t *b)
{
    if (a->tag == TYPE_VAR)
        return strcmp(a->var.name,
                      b->tag == TYPE_VAR ? b->var.name : b->con.name) == 0;

    for (int i = 0; i < b->con.count; i++)
    {
        if (occurs_check(a, b->con.types[i]))
            return true;
    }

    return false;
}

subst_t *bind(type_t *a, type_t *b)
{
    if (occurs_check(a, b))
    {
        error(
            b->base.loc,
            ERROR_TYPE,
            "Type variable '%s' occurs in '%s'",
            a->var.name,
            b->var.name);

        return NULL;
    }

    subst_t *subst = type_map();

    hmap_insert(subst, a, b);

    return subst;
}

subst_t *unify(type_t *a, type_t *b)
{
    // printf("Unify\n");
    // _type_str(a, false);
    // printf(" : ");
    // _type_str(b, false);
    // printf("\n");

    if (type_eq(a, b))
    {
        return type_map();
    }

    if (a->tag == TYPE_VAR)
        return bind(a, b);

    if (b->tag == TYPE_VAR)
        return bind(b, a);

    if (a->tag == TYPE_CON && b->tag == TYPE_CON)
    {
        if (strcmp(a->con.name, b->con.name) != 0)
        {
            error(
                b->base.loc,
                ERROR_TYPE,
                "Expected type '%s' but got type '%s'",
                a->con.name,
                b->con.name);
        }

        if (a->con.count != b->con.count)
        {
            error(
                b->base.loc,
                ERROR_TYPE,
                "Expected %d arguments but got %d",
                a->con.count - 1,
                b->con.count - 1);
        }

        subst_t *subst = type_map();
        for (int i = 0; i < a->con.count; i++)
        {
            subst_t *new_subst = unify(
                apply(subst, a->con.types[i]),
                apply(subst, b->con.types[i]));

            subst = compose(subst, new_subst);
        }

        return subst;
    }

    return NULL;
}

subst_t *solve(array_t *cnst)
{
    subst_t *subst = type_map();

    //  printf("Solving\n");

    size_t i;
    array_for_each(i, cnst)
    {
        constraint_t *c = array_at(cnst, i);

        subst_t *new_subst = unify(
            apply(subst, c->eq.left),
            apply(subst, c->eq.right));

        if (new_subst)
            subst = compose(subst, new_subst);
    }

    return subst;
}