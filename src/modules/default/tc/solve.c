#include "modules/default/types.h"

hmap_t *schemes = NULL;

void add_scheme(type_t *tvar, scheme_t *scheme)
{
    if (schemes == NULL)
        schemes = type_map();

    hmap_insert(schemes, tvar, scheme);
}

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
                b->con.name,
                a->con.name);

            _type_str(a, false);
            printf(" : ");
            _type_str(b, false);
            printf("\n");

            return NULL;
        }

        if (a->con.count != b->con.count)
        {
            error(
                b->base.loc,
                ERROR_TYPE,
                "Expected %d arguments but got %d",
                b->con.count - 1,
                a->con.count - 1);

            return NULL;
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

    if (a->tag == TYPE_REC && b->tag == TYPE_REC)
    {
        if (strcmp(a->rec.name, b->rec.name) != 0)
        {
            error(
                b->base.loc,
                ERROR_TYPE,
                "Expected type '%s' but got type '%s'",
                b->rec.name,
                a->rec.name);

            return NULL;
        }

        if (a->rec.count != b->rec.count)
        {
            error(
                a->base.loc,
                ERROR_TYPE,
                "All fields in '%s' must be initialized",
                b->rec.name);

            return NULL;
        }
    }

    if (a->tag != b->tag)
    {
        char *a_name, *b_name;

        if (a->tag == TYPE_VAR || b->tag == TYPE_VAR)
            a_name = a->var.name, b_name = b->var.name;
        else if (a->tag == TYPE_CON || b->tag == TYPE_CON)
            a_name = a->con.name, b_name = b->con.name;
        else
            a_name = a->rec.name, b_name = b->rec.name;

        error(
            a->base.loc,
            ERROR_TYPE,
            "Expected type '%s' but got type '%s'",
            b_name,
            a_name);
    }

    return NULL;
}

subst_t *solve(array_t *cnst)
{
    subst_t *subst = type_map();

    if (cnst == NULL)
        return subst;

    size_t i;
    array_for_each(i, cnst)
    {
        constraint_t *c = array_at(cnst, i);
        subst_t *new_subst = NULL;

        if (c->type == EQUALITY_CON)
        {
            new_subst = unify(
                apply(subst, c->eq.left),
                apply(subst, c->eq.right));
        }
        else if (c->type == IMPLICIT_CON)
        {
            hset_t *ftv = type_set();
            hset_t *free_vars = tvs_type(c->imp.consequent);
            hset_enum_t *he = hset_enum_create(free_vars);

            void *value;
            while (hset_enum_next(he, &value))
            {
                type_t *t = (type_t *)value;
                for (size_t i = 0; i < c->imp.M->count; i++)
                {
                    type_t *m = array_at(c->imp.M, i);
                    if (strcmp(t->var.name, m->var.name) == 0)
                        hset_insert(ftv, t);
                }
            }

            scheme_t *scheme = type_alloc(sizeof(scheme_t));
            scheme->set = ftv;
            scheme->type = c->imp.consequent;

            add_scheme(c->imp.antecedent, scheme);
        }

        if (new_subst)
            subst = compose(subst, new_subst);
    }

    return subst;
}