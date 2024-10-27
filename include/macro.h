#ifndef MACROS_H
#define MACROS_H

#define INFINITE_LOOP for (;;)

#ifdef __CHAR_UNSIGNED__
#define charmask(c) (c)
#else
#define charmask(c) ((unsigned char)((c) & 0xff))
#endif

#define array_size(x) (sizeof(x) / sizeof(*(x)))

#define cast(type, expr) ((type)(expr))
#define cast_itov(_int) ((void *)(uintptr_t)(_int))
#define cast_vtoi(_type, _ptr) ((_type)(uintptr_t)(_ptr))

#define __UNUSED__ __attribute__((unused))

#define __PACKED__ __attribute__((packed))

#define cleanup(func) __attribute__((cleanup(func)))
#define __INLINE__ inline __attribute__((always_inline))
#define __MUST_CHECK__ __attribute__((warn_unused_result));
#define section(section) __attribute__((__section__(section)))

#define LIST(ITEM) ITEM,
#define LIST_STRINGIFY(STRING) #STRING,

#define PROLOGUE               \
    struct                     \
    {                          \
        const char *fullname;  \
        const char *shortname; \
        const char *doc;       \
        const char *author;    \
        const char *version;   \
    }

#endif /* MACROS_H */