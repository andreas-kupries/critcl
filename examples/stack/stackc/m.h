/* ::stack - critcl - layer 3 declarations
 * Method functions.
 */

#ifndef _M_H
#define _M_H 1

#include "tcl.h"
#include <cstack/cstackDecls.h>

int stm_CLEAR   (CSTACK s, Tcl_Interp* interp, int objc, Tcl_Obj* CONST* objv);
int stm_DESTROY (CSTACK s, Tcl_Interp* interp, int objc, Tcl_Obj* CONST* objv);
int stm_PEEK    (CSTACK s, Tcl_Interp* interp, int objc, Tcl_Obj* CONST* objv, int pop, int revers);
int stm_PUSH    (CSTACK s, Tcl_Interp* interp, int objc, Tcl_Obj* CONST* objv);
int stm_ROTATE  (CSTACK s, Tcl_Interp* interp, int objc, Tcl_Obj* CONST* objv);
int stm_SIZE    (CSTACK s, Tcl_Interp* interp, int objc, Tcl_Obj* CONST* objv);
int stm_GET     (CSTACK s, Tcl_Interp* interp, int objc, Tcl_Obj* CONST* objv, int revers);
int stm_TRIM    (CSTACK s, Tcl_Interp* interp, int objc, Tcl_Obj* CONST* objv, int ret);

#endif /* _M_H */

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
