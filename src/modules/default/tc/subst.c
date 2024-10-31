#include "modules/default/types.h"

type_t *apply(subst_t *subst, type_t *type)
{
    if (type->tag == TYPE_VAR)
    {
        type_t *val = (type_t *)hmap_get_direct(subst, type);

        if (val)
            return val;
        return type;
    }
    else if (type->tag == TYPE_CON)
    {

        type_t *new_type = type_dup(type);

        for (int i = 0; i < type->con.count; i++)
        {
            new_type->con.types[i] = apply(subst, type->con.types[i]);
        }

        return new_type;
    }

    return NULL;
}

subst_t *compose(subst_t *a, subst_t *b)
{
    subst_t *u = hmap_union(b, a);

    hmap_enum_t *he = hmap_enum_create(u);

    void *key, *val;
    while (hmap_enum_next(he, &key, &val))
        hmap_insert(u, key, apply(u, val));

    hmap_enum_destroy(he);

    return u;
}