#include "parser/module.h"

extern ptree_t *ptree_g;

module_t *root_module_g = NULL;

const proc_ast_t *ast_g = &node_ast;
static memory_arena_t module_arena_g = {NULL};

DEFINE_ALLOC(module_sys_alloc, &module_arena_g);
DEFINE_STRDUP(module_sys_strdup, &module_arena_g);

module_t *get_module_child(module_t *module, char *name)
{
    module_t *child = module->children;
    while (child != NULL)
    {
        if (strcmp(child->name, name) == 0)
            return child;
        child = child->sibling;
    }

    return NULL;
}

abs_path_t *get_abs_path(module_t *module, char *name)
{
    if (module == NULL)
        return NULL;

    abs_path_t *path = (abs_path_t *)mod_alloc(module, sizeof(abs_path_t));

    module_t *child = module;

    size_t len = 0;
    while (child != NULL)
    {
        len += 1;
        child = child->parent;
    }

    len++;

    path->segments = (char **)mod_alloc(module, sizeof(char *) * len);
    path->no_segments = len;

    child = module;
    int i = len - 2;
    while (child != NULL)
    {
        path->segments[i] = mod_strdup(module, child->name);
        child = child->parent;
        --i;
    }

    if (name)
        path->segments[len - 1] = mod_strdup(module, name);
    else
        path->no_segments--;

    symbol_path(module, path);

    return path;
}

static void _print_mod(module_t *module, int level)
{
    if (module == NULL)
        return;

    for (int i = 0; i < level; i++)
    {
        printf("  ");
    }

    printf("module (%s)\n", module->name);

    _scope_print(module->scope_root, level + 1);

    _print_mod(module->children, level + 1);
    _print_mod(module->sibling, level);
}

void *mod_alloc(module_t *module, size_t size)
{
    return arena_alloc(&(module->arena), size);
}

char *mod_strdup(module_t *module, char *str)
{
    return strdup_a(&(module->arena), str);
}

module_t *create_module(char *name, module_t *parent)
{
    module_t *module = (module_t *)module_sys_alloc(sizeof(module_t));
    module->name = module_sys_strdup(name);
    module->parent = parent;
    module->children = NULL;
    module->sibling = NULL;
    module->is_inline = true;
    module->ctx = NULL;

    arena_init(&(module->arena), 1024);

    module->abs_path = get_abs_path(module, NULL);

    char *scope_name = module_sys_alloc(strlen(name) + 10);
    sprintf(scope_name, "%s[global]", name);

    module->scope_root = module->scope = new_scope(module, scope_name);

    for (int i = 0; i < 10; i++)
    {
        module->buffers[i] = NULL;
    }

    if (parent != NULL)
    {
        if (parent->children == NULL)
        {
            parent->children = module; // First child
        }
        else
        {
            module_t *sibling = parent->children;
            while (sibling->sibling != NULL)
            {
                sibling = sibling->sibling;
            }
            sibling->sibling = module; // Add as last sibling
        }
    }

    return module;
}

module_t *module_enter(module_t *current, char *name)
{
    module_t *mod = NULL;

    if (current)
        mod = get_module_child(current, name);

    if (mod == NULL)
        mod = create_module(name, current);

    if (current)
    {
        mod->ctx = current->ctx;

        for (int i = 0; i < 10; i++)
            mod->buffers[i] = current->buffers[i];
    }

    return mod;
}

void _module_leave(module_t **module)
{
    if (!module || !(*module))
        return;

    // print_mod();

    // for (int i = 0; i < 10; i++)
    // {
    //     if ((*module)->buffers[i])
    //     {
    //         sarray_free((*module)->buffers[i]);
    //         (*module)->buffers[i] = NULL;
    //     }
    // }

    // arena_free(&((*module)->ctx.arena));

    // (*module)->root = NULL;
    // (*module)->scope = NULL;
    // (*module)->scope_root = NULL;

    *module = (*module)->parent;
}

symbol_t *symbol_lookup(module_t *module, abs_path_t *path)
{
    (void)module;
    (void)path;

    size_t i = 0;
    size_t len = path->no_segments;

    if (strcmp(path->segments[0], "root") == 0)
        module = root_module_g, i = 1;

    if (len > 1)
    {
        for (; i < len - 1; i++)
        {
            if (strcmp(path->segments[i], "super") == 0)
                module = module->parent;
            else
                module = get_module_child(module, path->segments[i]);

            if (module == NULL)
                break;
        }
    }

    if (module != NULL)
    {
        symbol_t *symbol = get_symbol(module, path->segments[i]);
        if (symbol)
        {
            if (len > 1)
            {
                return symbol->access == ACCESS_PUBLIC ? symbol : NULL;
            }
            else
            {
                return symbol;
            }
        }
        else
            return NULL;
    }

    return NULL;
}

path_t *open_path(path_t *path, char *file)
{
    path_t *new_path = path_file(path, file, 0);

    struct stat st;
    if (stat(new_path->full_path, &st) == 0)
    {
        if (S_ISREG(st.st_mode))
            return new_path;
    }
    else
    {
        char *new_dir = stitch_fd_ff(new_path->full_dir, new_path->filename);

        if (stat(new_dir, &st) == 0)
        {
            if (S_ISDIR(st.st_mode))
            {
                path_t *np = path_fe(new_path, "__mod__", "lg", 1, new_path->filename);
                np->is_dir = true;

                free(new_dir);
                free_path(new_path);

                if (stat(np->full_path, &st) == 0 && S_ISREG(st.st_mode))
                    return np;

                free(np);

                return NULL;
            }
        }

        free(new_dir);
    }

    return new_path;
}

path_t *open_module(module_t *module, char *name)
{
    (void)name;
    (void)module;

    path_t *path = open_path(module->path, name);

    module_parse(path);

    return path;
}

int module_parse(path_t *path)
{
    module_t *mod = NULL;

    yyin = fopen(path->full_path, "r");
    if (!yyin)
    {
        perror("file not found");
        return 1;
    }

    if (strcmp(path->filename, "__mod__") == 0)
    {
        entry_t *entry = tuple_at(path->dirs, tuple_length(path->dirs) - 1);
        mod = module_enter(mod, entry->data);
        mod->is_dir = true;
    }
    else
        mod = module_enter(mod, path->filename);

    root_module_g = mod;

    mod->is_inline = false;
    mod->path = path;

    int flag = yyparse();

    fclose(yyin);

    ast_g->init(mod, ptree_g);
    ast_g->exit();

    ptree_free(mod->ptree);

    mod->path = NULL;
    mod->ptree = NULL;

    module_leave(mod);

    return flag;
}

void print_mod()
{
    _print_mod(root_module_g, 0);
}