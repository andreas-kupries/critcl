#ifndef __CRITCL_UTIL_ALLOC_H
#define __CRITCL_UTIL_ALLOC_H 1

/*
 * = = == === ===== ======== ============= =====================
 */

#include <tcl.h>

/*
 * Helper macros for easy allocation of structures and arrays.
 */

#define ALLOC(type)       (type *) ckalloc   (sizeof (type))
#define NALLOC(type,n)    (type *) ckalloc   (sizeof (type) * (n))
#define REALLOC(x,type,n) (type *) ckrealloc ((char*) x, sizeof (type) * (n))

/*
 * = = == === ===== ======== ============= =====================
 */

#endif /* __CRITCL_UTIL_ALLOC_H */

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
