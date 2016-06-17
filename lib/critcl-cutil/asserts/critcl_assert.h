#ifndef __CRITCL_UTIL_ASSERT_H
#define __CRITCL_UTIL_ASSERT_H 1

/*
 * = = == === ===== ======== ============= =====================
 */

#include <tcl.h>

/*
 * Macros for assertions, controlled via CRITCL_ASSERT.
 * Especially a helper to check array bounds, and counted
 * abort.
 */

#ifdef CRITCL_ASSERT

#define XSTR(x) #x
#define STR(x) XSTR(x)
#define RANGEOK(i,n) ((0 <= (i)) && (i < (n)))

#define ASSERT(x,msg) if (!(x)) { \
	Tcl_Panic (msg " (" #x "), in file " __FILE__ " @line " STR(__LINE__)); \
    }

#define ASSERT_BOUNDS(i,n) \
    ASSERT (RANGEOK(i,n),						\
	    "array index out of bounds: " STR(i) " >= " STR(n))

#define STOPAFTER(x) { \
	static int count = (x); \
	count --; \
	if (!count) { Tcl_Panic ("stop"); } \
    }

#else /* ! CRITCL_ASSERT */

#define ASSERT(x,msg)
#define ASSERT_BOUNDS(i,n)
#define STOPAFTER(x)

#endif

/*
 * = = == === ===== ======== ============= =====================
 */

#endif /* __CRITCL_UTIL_ASSERT_H */

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
