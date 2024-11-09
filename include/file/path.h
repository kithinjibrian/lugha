#ifndef PATH_H
#define PATH_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>
#include <sys/stat.h>
#include "dsa/tuple.h"

#define MUST_EXIST (1 << 0)
#define DIR_CREATE (1 << 1)
#define FILE_CREATE (1 << 2)

typedef struct path
{
    int count;
    char *ext;
    char *name;
    bool is_dir;
    char *filename;
    tuple_t *dirs;
    char *full_dir;
    char *full_path;
    char **directories;
    char *full_filename;
} path_t;

void path_delete(path_t *path, size_t index);
path_t *path_ext(path_t *path, char *ext, int n, ...);
path_t *path_file(path_t *path, char *file, int n, ...);
path_t *path_fe(path_t *path, char *file, char *ext, int n, ...);
////////////

void free_path(path_t *path);
void print_path(const path_t *path);
char *stitch_fd_ff(char *fd, char *ff);
char *stitch_fd(int n, char **directories);
path_t *split_path(const char *filepath, int flag);
char *stitch_fe(const char *filename, const char *ext);
path_t *path_build(const path_t *base_path, int n, ...);
path_t *path_shift(path_t *base_path, char *ext, int n, ...);
path_t *path_unshift(path_t *base_path, char *ext, int n, ...);
path_t *path_replace(path_t *base_path, char *ext, int n, ...);

#endif /* PATH_H */