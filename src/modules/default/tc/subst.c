#include "modules/default/types.h"

type_t *apply(module_t *module, subst_t *subst, type_t *type)
{
    if (type->tag == TYPE_VAR)
    {
        type_t *val = (type_t *)hmap_get_direct(subst, type);

        if (val)
        {

            return val;
        }

        return type;
    }
    else if (type->tag == TYPE_CON)
    {

        type_t *new_type = type_dup(module, type);

        for (int i = 0; i < type->con.count; i++)
        {
            new_type->con.types[i] = apply(module, subst, type->con.types[i]);
        }

        return new_type;
    }
    else if (type->tag == TYPE_REC)
    {
        type_t *new_type = new_trec(module, type, type->rec.size, type->rec.name);

        for (int i = 0; i < type->rec.size; i++)
        {
            trec_kv_t *kv = type->rec.table[i];

            while (kv)
            {
                trec_add(module, new_type, kv->label, apply(module, subst, kv->type));
                kv = kv->next;
            }
        }

        return new_type;
    }

    return NULL;
}

constraint_t *apply_constraint(subst_t *subst, constraint_t *constraint)
{
    (void)subst;
    (void)constraint;
    // switch (constraint->type)
    // {
    // case EQUALITY_CON:
    // {
    //     constraint_t *new_constraint = mod_alloc(module_g, sizeof(constraint_t));
    //     new_constraint->type = EQUALITY_CON;
    //     new_constraint->eq.left = apply(subst, constraint->eq.left);
    //     new_constraint->eq.right = apply(subst, constraint->eq.right);
    //     return new_constraint;
    // }
    // default:
    //     break;
    // }

    return NULL;
}

array_t *apply_constraints_list(subst_t *subst, array_t *constraints)
{
    array_t *new_constraints = array_create(sizeof(constraint_t));

    size_t i;
    array_for_each(i, constraints)
    {
        constraint_t *c = array_at(constraints, i);
        array_push(new_constraints, apply_constraint(subst, c));
    }

    return new_constraints;
}

subst_t *compose(module_t *module, subst_t *a, subst_t *b)
{
    subst_t *u = hmap_union(b, a);

    hmap_enum_t *he = hmap_enum_create(u);

    void *key, *val;
    while (hmap_enum_next(he, &key, &val))
        hmap_insert(u, key, apply(module, u, val));

    hmap_enum_destroy(he);

    hmap_destroy(b);
    hmap_destroy(a);

    return u;
}