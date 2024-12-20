#include "modules/default/types.h"

void add_eq_constraint(type_t *left, type_t *right)
{
    if (constraints(&type_visitor) == NULL)
        constraints(&type_visitor) = new_array(constraint_t);

    constraint_t constraint = {
        .type = EQUALITY_CON,
        .eq = {
            .left = left,
            .right = right,
        }};

    array_push(constraints(&type_visitor), &constraint);
}

void add_tc_constraint(type_class_t *tc, type_t *type)
{
    if (constraints(&type_visitor) == NULL)
        constraints(&type_visitor) = new_array(constraint_t);

    constraint_t constraint = {
        .type = TYPECLASS_CON,
        .tc = {
            .type = type,
            .type_class = tc,
        },
    };

    array_push(constraints(&type_visitor), &constraint);
}

void add_imp_constraint(type_t *antecedent, type_t *consequent, array_t *M)
{
    if (constraints(&type_visitor) == NULL)
        constraints(&type_visitor) = new_array(constraint_t);

    constraint_t constraint = {
        .type = IMPLICIT_CON,
        .imp = {
            .antecedent = antecedent,
            .consequent = consequent,
            .M = M,
        }};

    array_push(constraints(&type_visitor), &constraint);
}

void constraint_str(constraint_t *constraint)
{
    if (constraint->type == EQUALITY_CON)
    {
        _type_str(constraint->eq.left, false);
        printf(" : ");
        _type_str(constraint->eq.right, false);
        printf("\n");
    }
    else if (constraint->type == IMPLICIT_CON)
    {
        _type_str(constraint->imp.antecedent, false);
        printf(" < ");
        _type_str(constraint->imp.consequent, false);
        printf(" [");
        for (size_t i = 0; i < constraint->imp.M->count; i++)
        {
            type_t *t = array_at(constraint->imp.M, i);

            printf("%s", t->var.name);

            if (i != constraint->imp.M->count - 1)
                printf(", ");
        }
        printf("]\n");
    }
}

hset_t *tvs_type(type_t *type)
{
    hset_t *tvs = type_set();

    if (type->tag == TYPE_VAR)
    {
        hset_insert(tvs, type);
        return tvs;
    }
    else if (type->tag == TYPE_CON)
    {
        for (int i = 0; i < type->con.count; i++)
        {
            tvs = hset_union(tvs, tvs_type(type->con.types[i]));
        }

        return tvs;
    }

    return NULL;
}

hset_t *tvs_scheme(scheme_t *scheme)
{
    hset_t *set = tvs_type(scheme->type);

    return hset_difference(set, scheme->set);
}

void scheme_str(scheme_t *scheme)
{
    hset_enum_t *he = hset_enum_create(scheme->set);

    printf("Forall [");
    void *value;
    while (hset_enum_next(he, &value))
    {
        _type_str(value, false);
        printf(", ");
    }
    printf("] : ");

    _type_str(scheme->type, false);

    hset_enum_destroy(he);

    printf("\n");
}

type_t *instantiate(scheme_t *scheme)
{
    (void)scheme;
    //  subst_t *subst = type_map();

    hset_enum_t *he = hset_enum_create(scheme->set);

    void *value;
    while (hset_enum_next(he, &value))
    {
        //  hmap_insert(subst, value, new_ttype_var(value));
    }

    hset_enum_destroy(he);

    // return apply(subst, scheme->type);

    return NULL;
}
