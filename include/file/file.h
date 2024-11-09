#ifndef FILE_H
#define FILE_H

#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>

#include "path.h"

int write_file(path_t *path, void *content, size_t size);

#endif /* FILE_H */