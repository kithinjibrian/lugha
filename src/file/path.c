#include "file/path.h"

char *stitch_fe(const char *filename, const char *ext)
{
    char *fe = malloc(strlen(filename) + strlen(ext) + 2);
    snprintf(fe, strlen(filename) + strlen(ext) + 2, "%s.%s", filename, ext);
    return fe;
}

// join full dir and full filename
char *stitch_fd_ff(char *fd, char *ff)
{
    char *fe = malloc(strlen(fd) + strlen(ff) + 2);
    snprintf(fe, strlen(fd) + strlen(ff) + 2, "%s/%s", fd, ff);
    return fe;
}

path_t *split_path(const char *filepath, int flag)
{
    path_t *path = malloc(sizeof(path_t));
    if (!path)
        return NULL;

    path->ext = NULL;
    path->filename = NULL;
    path->full_dir = NULL;
    path->full_filename = NULL;
    path->full_path = strdup(filepath);

    if (!path->full_path)
        return path;

    char *path_copy = strdup(filepath);
    if (!path_copy)
    {
        free(path->full_path);
        free(path);
        return NULL;
    }

    struct stat path_stat;
    if (stat(filepath, &path_stat) == -1)
    {
        if (flag & DIR_CREATE)
            mkdir(path_copy, 0755);
        else
        {
            perror("stat failed");
            free(path_copy);
            free(path->full_path);
            free(path);
            return NULL;
        }
    }

    int is_file = S_ISREG(path_stat.st_mode); // Check if it's a regular file

    if (is_file)
    {
        char *last_slash = strrchr(path_copy, '/');

        if (last_slash)
        {
            *last_slash = '\0';
            path->full_filename = strdup(last_slash + 1);
            if (!path->full_filename)
                goto cleanup;

            path->full_dir = strdup(path_copy);

            char *dir_token = strtok(path_copy, "/");
            path->dirs = new_tuple(4);

            if (!path->dirs)
                goto cleanup;

            while (dir_token)
            {
                tuple_push(path->dirs,
                           entry_data(N_STRING,
                                      dir_token,
                                      strlen(dir_token) + 1));

                dir_token = strtok(NULL, "/");
            }
        }
        else
        {
            path->full_filename = strdup(path_copy);
            if (!path->full_filename)
                goto cleanup;
        }

        if (path->full_filename)
        {
            char *ff_copy = strdup(path->full_filename);
            if (!ff_copy)
                goto cleanup;

            char *dot = strrchr(ff_copy, '.');
            if (dot)
            {
                *dot = '\0';
                path->ext = strdup(dot + 1);

                if (!path->ext)
                    goto cleanup;
            }

            path->filename = strdup(ff_copy);
            if (!path->filename)
                goto cleanup;

            free(ff_copy);
        }
    }
    else
    {
        char *dir_token = strtok(path_copy, "/");
        path->dirs = new_tuple(4);

        while (dir_token)
        {
            tuple_push(path->dirs, entry_data(N_STRING, dir_token, strlen(dir_token) + 1));

            dir_token = strtok(NULL, "/");
        }

        path->full_dir = strdup(path_copy);
    }

    free(path_copy);
    return path;

cleanup:
    if (path_copy)
        free(path_copy);
    tuple_free(path->dirs);
    free(path->ext);
    free(path->filename);
    free(path->full_dir);
    free(path->full_path);
    free(path->full_filename);
    free(path);
    return NULL;
}

path_t *_path_add(path_t *path, int n, va_list args)
{
    path_t *new_path = malloc(sizeof(path_t));
    if (!new_path)
        return NULL;

    new_path->ext = NULL;
    new_path->filename = NULL;
    new_path->full_path = NULL;
    new_path->full_filename = NULL;
    new_path->dirs = tuple_clone(path->dirs);

    if (!new_path->dirs)
        goto cleanup;

    for (int i = 0; i < n; i++)
    {
        char *str = va_arg(args, char *);
        entry_t *entry = entry_data(N_STRING, str, strlen(str) + 1);
        tuple_unshift(new_path->dirs, entry);

        if (!new_path->dirs)
            goto cleanup;
    }

    va_end(args);

    new_path->full_dir = tuple_stringify(
        new_path->dirs,
        "/",
        entry_to_string);

    return new_path;

cleanup:
    tuple_free(new_path->dirs);
    free(new_path->filename);
    free(new_path->full_dir);
    free(new_path);
    return NULL;
}

path_t *_path_push(path_t *path, int n, va_list args)
{
    path_t *new_path = malloc(sizeof(path_t));
    if (!new_path)
        return NULL;

    new_path->ext = NULL;
    new_path->filename = NULL;
    new_path->full_path = NULL;
    new_path->full_filename = NULL;
    new_path->dirs = tuple_clone(path->dirs);

    if (!new_path->dirs)
        goto cleanup;

    for (int i = 0; i < n; i++)
    {
        char *str = va_arg(args, char *);
        entry_t *entry = entry_data(N_STRING, str, strlen(str) + 1);
        tuple_push(new_path->dirs, entry);

        if (!new_path->dirs)
            goto cleanup;
    }

    va_end(args);

    new_path->full_dir = tuple_stringify(
        new_path->dirs,
        "/",
        entry_to_string);

    return new_path;

cleanup:
    tuple_free(new_path->dirs);
    free(new_path->filename);
    free(new_path->full_dir);
    free(new_path);
    return NULL;
}

path_t *path_ext(path_t *path, char *ext, int n, ...)
{
    va_list args;
    va_start(args, n);
    path_t *new_path = _path_add(path, n, args);
    va_end(args);

    if (!new_path)
        return NULL;

    new_path->ext = strdup(ext);
    new_path->filename = strdup(path->filename);
    new_path->full_filename = stitch_fe(new_path->filename, new_path->ext);
    new_path->full_path = stitch_fd_ff(new_path->full_dir, new_path->full_filename);

    return new_path;
}

path_t *path_file(path_t *path, char *file, int n, ...)
{
    va_list args;
    va_start(args, n);
    path_t *new_path = _path_add(path, n, args);
    va_end(args);

    if (!new_path)
        return NULL;

    new_path->ext = strdup(path->ext);
    new_path->filename = strdup(file);
    new_path->full_filename = stitch_fe(new_path->filename, new_path->ext);
    new_path->full_path = stitch_fd_ff(new_path->full_dir, new_path->full_filename);

    return new_path;
}

path_t *path_fe(path_t *path, char *file, char *ext, int n, ...)
{
    va_list args;
    va_start(args, n);
    path_t *new_path = _path_push(path, n, args);
    va_end(args);

    if (!new_path)
        return NULL;

    new_path->ext = strdup(ext);
    new_path->filename = strdup(file);
    new_path->full_filename = stitch_fe(new_path->filename, new_path->ext);
    new_path->full_path = stitch_fd_ff(new_path->full_dir, new_path->full_filename);

    return new_path;
}

void path_delete(path_t *path, size_t index)
{
    if (!path)
        return;

    tuple_delete(path->dirs, index);

    free(path->full_path);
    free(path->full_dir);

    path->full_dir = tuple_stringify(
        path->dirs,
        "/",
        entry_to_string);

    path->full_path = stitch_fd_ff(
        path->full_dir, path->full_filename);
}

void free_path(path_t *path)
{
    if (!path)
        return;

    tuple_free(path->dirs);
    free(path->ext);
    free(path->filename);
    free(path->full_filename);
    free(path->full_dir);
    free(path->full_path);
    free(path->directories);
    free(path);
}

void _print_path(entry_t *entry)
{
    if (entry && entry->tag == N_STRING && entry->data)
    {
        printf(" %s\n", ((char *)entry->data));
    }
}

void print_path(const path_t *path)
{
    if (!path)
    {
        printf("Path is NULL\n");
        return;
    }

    printf("Directories:\n");

    char *dir_str = tuple_stringify(path->dirs, "/", entry_to_string);

    if (dir_str)
    {
        printf("%s\n", dir_str);
        free(dir_str);
    }
    //    tuple_for_each(path->dirs, _print_path);

    if (path->directories)
    {
        for (int i = 0; i < path->count; i++)
        {
            printf("  %s\n", path->directories[i]);
        }
    }

    printf("Filename: %s\n", path->filename ? path->filename : "(none)");
    printf("Extension: %s\n", path->ext ? path->ext : "(none)");
    printf("Full filename: %s\n", path->full_filename ? path->full_filename : "(none)");
    printf("Full directory: %s\n", path->full_dir ? path->full_dir : "(none)");
    printf("Fullpath: %s\n", path->full_path ? path->full_path : "(none)");
}