#ifndef CSTACK_H
#define CSTACK_H 1

/*
 * Forward declaration of stacks (opaque handle).
 */

typedef struct CSTACK_* CSTACK;

/*
 * Stacks are conceptually an array of void* cells, with each cell
 * either directly containing the data, or a pointer to it.
 *
 * To handle the latter a pointer to a per-cell delete function is
 * maintained, enabling the stack code to delete cells which are
 * pointers to the actual data.
 *
 * Note however that the allocation of cell data is the responsibility
 * of the stack's user.
 */

typedef void (*CSTACK_CELL_FREE) (void* cell);

/*
 * Data structure filled by 'stack_get'. The pointer to the cells may be
 * allocated on the heap, or not. If it is allocated the flag 'dynamic' is set
 * to true, and false otherwise.
 */

typedef struct CSTACK_SLICE_ {
    void** cell;
    int    dynamic;
} CSTACK_SLICE;

#define cstack_slice_cleanup(slice) \
    if ((slice).dynamic) { ckfree ((char*) (slice).cell); }

/*
 * Cue to 'cstack_get' where to put the top element of the stack in the returned slice.
 */

typedef enum {
    cstack_normal,  /* cstack_get returns the slice with top-element at left/beginning */
    cstack_revers   /* cstack_get returns the slice with top-element at right/end */
} CSTACK_DIRECTION;

#endif /* CSTACK_H */

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
