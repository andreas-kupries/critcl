#ifndef CRITCL_TCL9_COMPAT_H
#define CRITCL_TCL9_COMPAT_H

/* Disable the macros making us believe that everything is hunky-dory on compilation, and then
 * reward us with runtime crashes for being a sucker to have believed them.
 */
#define TCL_NO_DEPRECATED

#include "tcl.h"

/*
 * - - -- --- ----- -------- ------------- ---------------------
 * Check for support of the `Tcl_Size` typdef and associated definitions.
 * It was introduced in Tcl 8.7 and 9, and we need backward compatibility
 * definitions for 8.6.
 */

#ifndef TCL_SIZE_MAX
    #include <limits.h>
    #define TCL_SIZE_MAX INT_MAX

    #ifndef Tcl_Size
        typedef int Tcl_Size;
    #endif

    #define TCL_SIZE_MODIFIER ""
    #define Tcl_GetSizeIntFromObj Tcl_GetIntFromObj
    #define Tcl_NewSizeIntObj     Tcl_NewIntObj
#else
    #define Tcl_NewSizeIntObj     Tcl_NewWideIntObj
#endif

/*
 * - - -- --- ----- -------- ------------- ---------------------
 * Critcl (3.2.1+) emits the command creation API using Tcl_Size by default.
 * Map this to the older int-based API when compiling against Tcl 8.x or older.
 */

#if TCL_MAJOR_VERSION <= 8
#define Tcl_CreateObjCommand2 Tcl_CreateObjCommand
#endif

/*
 * - - -- --- ----- -------- ------------- ---------------------
 */

#ifndef CONST
#define CONST const
#endif

#ifndef CONST84
#define CONST84 const
#endif

#ifndef CONST86
#define CONST86 const
#endif

/*
 * - - -- --- ----- -------- ------------- ---------------------
 */
#endif /* CRITCL_TCL9_COMPAT_H */