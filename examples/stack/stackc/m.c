/* ::stack - critcl - layer 3 definitions.
 *
 * -> Method functions.
 *    Implementations for all stack methods.
 */

#include "util.h"
#include "m.h"

/* .................................................. */

/*
 *---------------------------------------------------------------------------
 *
 * stm_CLEAR --
 *
 *	Removes all elements currently on the stack. I.e empties the stack.
 *
 * Results:
 *	A standard Tcl result code.
 *
 * Side effects:
 *	Only internal, memory allocation changes ...
 *
 *---------------------------------------------------------------------------
 */

int
stm_CLEAR (CSTACK s, Tcl_Interp* interp, int objc, Tcl_Obj* CONST* objv)
{
    /* Syntax: stack clear
     *	       [0]   [1]
     */

    if (objc != 2) {
	Tcl_WrongNumArgs (interp, 2, objv, NULL);
	return TCL_ERROR;
    }

    cstack_pop (s, cstack_size (s));
    return TCL_OK;
}

/*
 *---------------------------------------------------------------------------
 *
 * stm_DESTROY --
 *
 *	Destroys the whole stack object.
 *
 * Results:
 *	A standard Tcl result code.
 *
 * Side effects:
 *	Releases memory.
 *
 *---------------------------------------------------------------------------
 */

int
stm_DESTROY (CSTACK s, Tcl_Interp* interp, int objc, Tcl_Obj* CONST* objv)
{
    /* Syntax: stack destroy
     *	       [0]   [1]
     */

    if (objc != 2) {
	Tcl_WrongNumArgs (interp, 2, objv, NULL);
	return TCL_ERROR;
    }

    Tcl_DeleteCommandFromToken(interp, (Tcl_Command) cstack_clientdata_get (s));
    return TCL_OK;
}

/*
 *---------------------------------------------------------------------------
 *
 * stm_GET --
 *
 *	Non-destructively retrieves all elements of the stack.
 *
 * Results:
 *	A standard Tcl result code.
 *
 * Side effects:
 *	Only internal, memory allocation changes ...
 *
 *---------------------------------------------------------------------------
 */

int
stm_GET (CSTACK s, Tcl_Interp* interp, int objc, Tcl_Obj* CONST* objv, int revers)
{
    /* Syntax: stack get
     *	       [0]  [1]
     */

    long int n;

    if (objc != 2) {
	Tcl_WrongNumArgs (interp, 2, objv, NULL);
	return TCL_ERROR;
    }

    n = cstack_size (s);

    if (!n) {
	Tcl_SetObjResult (interp, Tcl_NewListObj (0,NULL));
    } else {
	CSTACK_SLICE sl;

	cstack_get (s, n, (CSTACK_DIRECTION) revers, &sl);
	Tcl_SetObjResult (interp, Tcl_NewListObj (n, (Tcl_Obj**) sl.cell));
	cstack_slice_cleanup (sl);
    }

    return TCL_OK;
}

/*
 *---------------------------------------------------------------------------
 *
 * stm_TRIM --
 *
 *	Destructively retrieves one or more elements from the top of the
 *	stack, trims the stack to a new size.
 *
 * Results:
 *	A standard Tcl result code.
 *
 * Side effects:
 *	Only internal, memory allocation changes ...
 *
 *---------------------------------------------------------------------------
 */

int
stm_TRIM (CSTACK s, Tcl_Interp* interp, int objc, Tcl_Obj* CONST* objv, int ret)
{
    /* Syntax: stack trim N
     *	       [0]  [1]   [2]
     */

    int n, len;

    if (objc != 3) {
	Tcl_WrongNumArgs (interp, 2, objv, "newsize");
	return TCL_ERROR;
    }

    if (Tcl_GetIntFromObj(interp, objv[2], &n) != TCL_OK) {
	    return TCL_ERROR;
    } else if (n < 0) {
	Tcl_AppendResult (interp, "invalid size ",
			  Tcl_GetString (objv[2]),
			  NULL);
	return TCL_ERROR;
    }

    len = cstack_size (s);

    if (len <= n) {
	Tcl_SetObjResult (interp, Tcl_NewListObj (0,NULL));
    } else {
	if (ret) {
	    CSTACK_SLICE sl;

	    cstack_get (s, len-n, cstack_normal, &sl);

	    Tcl_SetObjResult (interp, Tcl_NewListObj (len-n, (Tcl_Obj**) sl.cell));

	    cstack_slice_cleanup (sl);
	}
	cstack_trim (s, n);
    }

    return TCL_OK;
}

/*
 *---------------------------------------------------------------------------
 *
 * stm_PEEK/POP --
 *
 *	(Non-)destructively retrieves one or more elements from the top of the
 *	stack.
 *
 * Results:
 *	A standard Tcl result code.
 *
 * Side effects:
 *	Only internal, memory allocation changes ...
 *
 *---------------------------------------------------------------------------
 */

int
stm_PEEK (CSTACK s, Tcl_Interp* interp, int objc, Tcl_Obj* CONST* objv, int pop, int revers)
{
    /* Syntax: stack peek|pop ?n?
     *	       [0]  [1]       [2]
     */

    int n = 1;

    if ((objc != 2) && (objc != 3)) {
	Tcl_WrongNumArgs (interp, 2, objv, "?n?");
	return TCL_ERROR;
    }

    if (objc == 3) {
	if (Tcl_GetIntFromObj(interp, objv[2], &n) != TCL_OK) {
	    return TCL_ERROR;
	} else if (n < 1) {
	    Tcl_AppendResult (interp, "invalid item count ",
			      Tcl_GetString (objv[2]),
			      NULL);
	    return TCL_ERROR;
	}
    }

    if (n > cstack_size (s)) {
       Tcl_AppendResult (interp,
                          "insufficient items on stack to fill request",
                          NULL);
        return TCL_ERROR;
    } else {
	CSTACK_SLICE sl;

	cstack_get (s, n, (CSTACK_DIRECTION) revers, &sl);

	if (n == 1) {
	    Tcl_SetObjResult (interp, (Tcl_Obj*) sl.cell[0]);
	} else {
	    Tcl_SetObjResult (interp, Tcl_NewListObj (n, (Tcl_Obj**) sl.cell));
	}

	cstack_slice_cleanup (sl);

	if (pop) {
	    cstack_pop (s, n);
	}
    }

    return TCL_OK;
}

/*
 *---------------------------------------------------------------------------
 *
 * stm_PUSH --
 *
 *	Adds one or more elements to the stack.
 *
 * Results:
 *	A standard Tcl result code.
 *
 * Side effects:
 *	May release and allocate memory.
 *
 *---------------------------------------------------------------------------
 */

int
stm_PUSH (CSTACK s, Tcl_Interp* interp, int objc, Tcl_Obj* CONST* objv)
{
    /* Syntax: stack push item...
     *	       [0]   [1]  [2]
     */

    int i;

    if (objc < 3) {
	Tcl_WrongNumArgs (interp, 2, objv, "item ?item ...?");
	return TCL_ERROR;
    }

    for (i = 2; i < objc; i++) {
	cstack_push (s, objv[i]);
	Tcl_IncrRefCount (objv[i]);
    }

    return TCL_OK;
}

/*
 *---------------------------------------------------------------------------
 *
 * stm_ROTATE --
 *
 *	Rotates the N top elements of the stack by K steps.
 *
 * Results:
 *	A standard Tcl result code.
 *
 * Side effects:
 *	May release and allocate memory.
 *
 *---------------------------------------------------------------------------
 */

int
stm_ROTATE (CSTACK s, Tcl_Interp* interp, int objc, Tcl_Obj* CONST* objv)
{
    /* Syntax: stack rotate count steps
     *	       [0]   [1]    [2]   [3]
     */

    int n, steps;

    if (objc != 4) {
	Tcl_WrongNumArgs (interp, 2, objv, "count steps");
	return TCL_ERROR;
    }

    if (Tcl_GetIntFromObj(interp, objv[2], &n) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Tcl_GetIntFromObj(interp, objv[3], &steps) != TCL_OK) {
	return TCL_ERROR;
    }

    if (n > cstack_size (s)) {
	Tcl_AppendResult (interp, "insufficient items on stack to fill request",
			  NULL);
	return TCL_ERROR;
    }

    cstack_rol (s, n, steps);
    return TCL_OK;
}

/*
 *---------------------------------------------------------------------------
 *
 * stm_SIZE --
 *
 *	Returns the number of elements currently held by the stack.
 *
 * Results:
 *	A standard Tcl result code.
 *
 * Side effects:
 *	None.
 *
 *---------------------------------------------------------------------------
 */

int
stm_SIZE (CSTACK s, Tcl_Interp* interp, int objc, Tcl_Obj* CONST* objv)
{
    /* Syntax: stack size
     *	       [0]   [1]
     */

    if ((objc != 2)) {
	Tcl_WrongNumArgs (interp, 2, objv, NULL);
	return TCL_ERROR;
    }

    Tcl_SetObjResult  (interp, Tcl_NewIntObj (cstack_size (s)));
    return TCL_OK;
}

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
