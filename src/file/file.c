#include "file/file.h"

void _write_file(path_t *path, char *content, size_t size)
{
    (void)size;
    (void)content;

    FILE *fp = fopen(path->full_path, "w");

    if (!fp)
        return;

    fwrite(content, 1, size, fp);
    fclose(fp);
}

int make_dir(path_t *path)
{
    struct stat st;

    if (stat(path->full_dir, &st) == 0)
    {
        if (S_ISDIR(st.st_mode))
            return 0;
        else
            return -1;
    }

    char fullPath[256]; // Buffer to hold the full path
    memset(fullPath, 0, 256);

    for (size_t i = 0; i < tuple_length(path->dirs); i++)
    {
        entry_t *entry = tuple_at(path->dirs, i);

        strcat(fullPath, (char *)entry->data);

        if (i < tuple_length(path->dirs) - 1)
        {
            strcat(fullPath, "/");
        }

        struct stat _st;
        if (stat(fullPath, &_st) == 0)
        {
            if (S_ISDIR(_st.st_mode))
                continue;
        }

        if (mkdir(fullPath, 0755) != 0)
            return -2;
    }

    return 0;
}

int write_file(path_t *path, void *content, size_t size)
{
    (void)size;
    (void)path;
    (void)content;

    int res = make_dir(path);

    if (res != 0)
    {
        printf("Failed to create directory: %s\n", path->full_dir);
        return res;
    }

    _write_file(path, content, size);

    return 0;
}