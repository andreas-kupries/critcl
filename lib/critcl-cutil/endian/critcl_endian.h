#ifndef __CRITCL_CUTIL_ENDIAN_H
#define __CRITCL_CUTIL_ENDIAN_H 1

/*
 * Copyright (c) 2017-2020 Andreas Kupries <andreas_kupries@users.sourceforge.net>
 * = = == === ===== ======== ============= =====================
 */

#include <tcl.h>

/*
 * Macros to determine endianess at compile time, in a manner which supports
 * cross compilation.
 *
 * At the end of the file exactly one of the macrocs
 *
 * CRITCL_LITTLE_ENDIAN
 * CRITCL_BIG_ENDIAN
 *
 * will be defined.
 */

/*
 * - - -- --- ----- -------- ------------- ---------------------
 * The code below was taken from tclInt.h
 * Modification: Here we assume existence of the necessary sys headers.
 */

#include <sys/types.h>
#include <sys/param.h>

#undef CRITCL_BIG_ENDIAN
#undef CRITCL_LITTLE_ENDIAN

#ifdef BYTE_ORDER
#    ifdef BIG_ENDIAN
#	 if BYTE_ORDER == BIG_ENDIAN
#            define CRITCL_BIG_ENDIAN
#	 endif
#    endif
#    ifdef LITTLE_ENDIAN
#	 if BYTE_ORDER == LITTLE_ENDIAN
#            define CRITCL_LITTLE_ENDIAN
#	 endif
#    endif
#endif

/*
 * The code above was taken from tclInt.h
 * - - -- --- ----- -------- ------------- ---------------------
 *
 * Assert our claim at the beginning, to have exactly one of the macros defined.
 */

#if defined(CRITCL_BIG_ENDIAN) && defined(CRITCL_LITTLE_ENDIAN)
#error "Endian conflict, system claims to be both big and little."
#endif
#if !defined(CRITCL_BIG_ENDIAN) && !defined(CRITCL_LITTLE_ENDIAN)
#error "Failed to determine endianess."
#endif

/*
 * = = == === ===== ======== ============= =====================
 */

#endif /* __CRITCL_CUTIL_H */

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
