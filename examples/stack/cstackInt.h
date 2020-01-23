#ifndef CSTACK_INT_H
#define CSTACK_INT_H 1

#include "cstack/cstackDecls.h"

/*
 * Waste a bit of space (1K) on each new stack to reduce the amount of
 * reallocation going on for most stacks, which should be small.
 */

static const int CSTACK_INITIAL_SIZE = 256;

/*
 * Actual type of the stack data structure. Used only inside of the
 * package.
 */

typedef struct CSTACK_ {
    long int        max;   /* Size of the cell array. */
    long int        top;   /* Index of the topmost _unused_ cell in the
			    * array === Index of the _next_ cell to use
			    * === Size of the stack. */
    CSTACK_CELL_FREE freeCell;
    void*           clientData;

    void**          cell;  /* Array of the stack cells. */
} CSTACK_;

/*
 * Allocation macros for common situations.
 */

#define ALLOC(type)    (type *) ckalloc (sizeof (type))
#define NALLOC(n,type) (type *) ckalloc ((n) * sizeof (type))

/*
 * Assertions in general, and asserting the proper range of an array
 * index.
 */

#undef  CSTACK_DEBUG
#define CSTACK_DEBUG 1

#ifdef CSTACK_DEBUG
#define XSTR(x) #x
#define STR(x) XSTR(x)
#define RANGEOK(i,n) ((0 <= (i)) && (i < (n)))
#define ASSERT(x,msg) if (!(x)) { Tcl_Panic (msg " (" #x "), in file " __FILE__ " @line " STR(__LINE__));}
#define ASSERT_BOUNDS(i,n) ASSERT (RANGEOK(i,n),"array index out of bounds: " STR(i) " > " STR(n))
#else
#define ASSERT(x,msg)
#define ASSERT_BOUNDS(i,n)
#endif

#endif /* CSTACK_INT_H */

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
