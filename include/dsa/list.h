#ifndef DLIST_H
#define DLIST_H

#include <stdlib.h>
#include <stddef.h>

typedef struct dlist
{
    struct dlist *next;
    struct dlist *prev;
} dlist_t;

#define container_of(ptr, type, member) ({ \
    const __typeof__( ((type *)0)->member ) *__mptr = (ptr);    \
    (type *)( (char *)__mptr - offsetof(type,member) ); })

#define DLIST_HEAD_INIT(name) {&(name), &(name)}

#define DLIST_HEAD(name) \
    struct dlist name = DLIST_HEAD_INIT(name)

/**
 * dlist_for_each	-	iterate over a list
 * @param pos:   &struct list to use as a loop counter.
 * @param head:  head for your list.
 */
#define dlist_for_each(pos, head) \
    for (pos = (head)->next; pos != (head); pos = pos->next)

/**
 * dlist_entry - get the struct for this entry
 * @param ptr:    &struct list pointer.
 * @param type:   type of the struct this is embedded in.
 * @param member: name of the list within the struct type.
 */
#define dlist_entry(ptr, type, member) \
    container_of(ptr, type, member)

/**
 * dlist_for_each_entry	-	iterate over list of given type
 * @param pos:    type * to use as a loop counter.
 * @param head:   head for your list.
 * @param member: name of the list within the struct type.
 */
#define dlist_for_each_entry(pos, head, member)                       \
    for (pos = dlist_entry((head)->next, __typeof__(*(pos)), member); \
         &pos->member != (head);                                      \
         pos = dlist_entry((pos)->member.next, __typeof__(*(pos)), member))

/**
 * dlist_next_entry - get the next element in list
 * @param pos: type * to cursor
 * @param member: name of the list_head within the struct.
 */
#define dlist_next_entry(pos, member) \
    dlist_entry((pos)->member.next, __typeof__(*(pos)), member)

/**
 * list_first_entry - get the first element from a list
 * @param ptr: list head to take the element from.
 * @param type:	type of the struct this is embedded in.
 * @param member: name of the list_head within the struct.
 *
 * Note, that list is expected to be not empty.
 */
#define dlist_first_entry(ptr, type, member) \
    dlist_entry((ptr)->next, type, member)

/**
 * list_last_entry - get the last element from a list
 * @param ptr:	list head to take the element from.
 * @param type:	type of the struct this is embedded in.
 * @param member:name of the list_head within the struct.
 *
 * Note, that list is expected to be not empty.
 */
#define dlist_last_entry(ptr, type, member) \
    dlist_entry((ptr)->prev, type, member)

/**
 * list_next_entry_circular - get the next element in list
 * @param pos:	 type * to cursor.
 * @param head:	 list head to take the element from.
 * @param member: name of the list_head within the struct.
 *
 * Wraparound if pos is the last element (return the first element).
 * Note, that list is expected to be not empty.
 */
#define dlist_next_entry_circular(pos, head, member) \
    (dlist_is_last(&(pos)->member, head) ? dlist_first_entry(head, __typeof__(*(pos)), member) : dlist_next_entry(pos, member))

/**
 * dlist_prev_entry - get the prev element in list
 * @param pos:	type * to cursor
 * @param member: name of the list_head within the struct.
 */
#define dlist_prev_entry(pos, member) \
    dlist_entry((pos)->member.prev, __typeof__(*(pos)), member)

static inline void init_dlist_head(dlist_t *list)
{
    list->next = list;
    list->prev = list;
}

/**
 * dlist_is_last - tests whether @list is the last entry in list @head
 * @param list: entry to test
 * @param head: head of the list
 */
static inline int dlist_is_last(const struct dlist *list, const struct dlist *head)
{
    return list->next == head;
}

/**
 * __dlist_add__ - add a new entry between two known consecutive entries
 *
 * @param list: new entry to be added
 * @param prev: previous entry
 * @param next: next entry
 */
static inline void __dlist_add__(dlist_t *list, dlist_t *prev, dlist_t *next)
{
    list->next = next;
    list->prev = prev;
    prev->next = list;
    next->prev = list;
}

/**
 * dlist_add - add a new entry
 * @param head: list head to add it before
 * @param list: new entry to be added
 */
static inline void dlist_add(dlist_t *head, dlist_t *list)
{
    __dlist_add__(list, head, head->next);
}

/**
 * dlist_add_tail - add a new entry
 * @param head: list head to add it before
 * @param new: new entry to be added
 */
static inline void dlist_add_tail(dlist_t *head, dlist_t *list)
{
    __dlist_add__(list, head->prev, head);
}

/**
 * dlist_del - deletes entry from list by making prev/next entries
 *   point to each other
 *
 */
static inline void __dlist_del__(dlist_t *prev, dlist_t *next)
{
    next->prev = prev;
    prev->next = next;
}

/**
 * dlist_del - deletes entry from list
 * @param entry: the element to delete from the list
 */
static inline void dlist_del(dlist_t *entry)
{
    __dlist_del__(entry->prev, entry->next);
}

/**
 * dlist_pop - delete previous entry from list and returns it
 *
 */
static inline dlist_t *dlist_pop(dlist_t *list)
{
    dlist_t *prev = list->prev;
    if (prev == list)
        return NULL;
    dlist_del(prev);
    return prev;
}

#endif /* DLIST_H */