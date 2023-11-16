#ifndef CRITCL_CALLBACK_INT_H
#define CRITCL_CALLBACK_INT_H
/*
 * critcl callback class - internal declarations
 *
 * Instance information.
 */

#include <callback.h>
#include <tclpre9compat.h>	/* tcl.h + Portability Tcl <=8.6 */

typedef struct critcl_callback {

    int         nfixed;  // Number of elements in the command prefix
    int         nargs;   // Number of elements to reserve for the command arguments
    Tcl_Obj**   command; // Array for the command elements, prefix and arguments
    Tcl_Interp* interp;  // The Tcl interpreter to run the command in

} critcl_callback;

#endif

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
