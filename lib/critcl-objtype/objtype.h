/*
 * For package "@package@".
 * Implementation of Tcl_ObjType "@name@".
 */

#ifndef @stem@_IMPLEMENTATION
#define @stem@_IMPLEMENTATION (1)

#include <string.h> /* memcpy */
#include <tcl.h>
#include <critcl_trace.h>

@trace@ (@stem@);

/* # # ## ### ##### ######## ############# ##################### */
/*
 * Declarations - Tcl_ObjType internals.
 */

static void @fun_destructor@   (Tcl_Obj* objPtr);
static void @fun_copy@ (Tcl_Obj* objPtr, Tcl_Obj* dupObjPtr);
static void @fun_stringify@  (Tcl_Obj* objPtr);
static int  @fun_from_any@   (Tcl_Interp* interp, Tcl_Obj* objPtr);

static Tcl_ObjType @objtypevar@ = {
  "@name@",
  @fun_destructor@,   /* Release Internal Representation            */
  @fun_copy@, /* Duplicate Internal Representation          */
  @fun_stringify@,  /* Convert Internal Representation to String  */
  @fun_from_any@    /* Convert String To Internal Representation  */
};

#define COT_FIR(o) \
    if ((o)->typePtr != NULL) { \
	if ((o)->typePtr->freeIntRepProc != NULL) { \
	    (o)->typePtr->freeIntRepProc (o); \
	} \
	(o)->typePtr = NULL; \
    }

#define COT_PTR(o)      (o)->internalRep.twoPtrValue.ptr1
#define COT_PTB(o)      (o)->internalRep.twoPtrValue.ptr2
#define COT_SET_TYPE(o) (o)->typePtr = &@objtypevar@
#define COT_IS_TYPE(o)  ((o)->typePtr == &@objtypevar@)

#define COT_RCO(o) (o), ((o) ? (o)->refCount : -1)
#define COT_RCV(v) (v), ((v) ? (@refcount@ (v)) : -1)

/* # # ## ### ##### ######## User: General support */
@code_support@
#line 48 "objtype.h"
/* # # ## ### ##### ######## */

/* # # ## ### ##### ######## ############# ##################### */
/*
 * Definitions -- Public API.
 */

/*
 * Generate a Tcl_Obj* representing the 'value'.
 */

Tcl_Obj*
@api_new@ (@intrep@ value)
{
    TRACE_TAG_FUNC (@stem@,
		    "((@intrep@) %p^[%d])",
		    COT_RCV (value));

    Tcl_Obj* obj = Tcl_NewObj ();
    Tcl_InvalidateStringRep (obj);
    COT_PTR (obj) = value;
    @ref@ (value);
    COT_SET_TYPE (obj);

    TRACE_TAG_RETURN (@stem@,
		      "(Tcl_Obj*) %p", obj);
}

/*
 * Extract the 'value' from the Tcl_Obj*, convert to @intrep@ stored in the
 * 'obj' if necessary, or fail.
 */

int
@api_from@ (Tcl_Interp* interp,
	    Tcl_Obj*    obj,
	    @intrep@*   value)
{
    TRACE_TAG_FUNC (@stem@,
		    "((Tcl_Interp*) %p, (Tcl_Obj*) %p^[%d], (@intrep@*) %p)",
		    interp, COT_RCO (obj), value);

    if (!COT_IS_TYPE (obj)) {
	if (@fun_from_any@ (interp, obj) != TCL_OK) {
	    TRACE_TAG_RETURN (@stem@,
			      "(error) %d", TCL_ERROR);
	}
    }
    *value = (@intrep@) COT_PTR (obj);

    TRACE_TAG (@stem@,
	       "RESULT: (@intrep@) %p^[%d]", COT_RCV (*value));
    TRACE_TAG_RETURN (@stem@,
		      "(ok) %d", TCL_OK);
}

/* # # ## ### ##### ######## ############# ##################### */
/*
 * Definitions - ObjType Internals.
 */
static void
@fun_destructor@ (Tcl_Obj* obj)
{
    TRACE_TAG_FUNC (@stem@,
		    "((Tcl_Obj*) %p^[%d])",
		    COT_RCO (obj));

    @intrep@ value = (@intrep@) COT_PTR (obj);
    @unref@ (value);

    TRACE_TAG_RETURN_VOID (@stem@);
}

static void
@fun_copy@ (Tcl_Obj* obj, Tcl_Obj* dupObj)
{
    TRACE_TAG_FUNC (@stem@,
		    "((Tcl_Obj*) src %p^[%d], (Tcl_Obj*) dst %p^[%d])",
		    COT_RCO (obj), COT_RCO (dupObj));

    @intrep@ value = (@intrep@) COT_PTR (obj);
    COT_PTR (dupObj) = value;
    @ref@ (value);
    COT_SET_TYPE (dupObj);

    TRACE_TAG_RETURN_VOID (@stem@);
}

static void
@fun_stringify@ (Tcl_Obj* obj)
{
    TRACE_TAG_FUNC (@stem@,
		    "((Tcl_Obj*) %p^[%d])",
		    COT_RCO (obj));

    @intrep@ value = (@intrep@) COT_PTR (obj);
    Tcl_DString      ds;
    Tcl_DStringInit (&ds);

    @2string@ (value, &ds);

    unsigned int len = Tcl_DStringLength (&ds);
    obj->bytes = ckalloc (len + 1);
    memcpy (obj->bytes, Tcl_DStringValue (&ds), len + 1);
    obj->bytes [len] = '\0';
    obj->length = len;
    Tcl_DStringFree (&ds);

    TRACE_TAG_RETURN_VOID (@stem@);
}

static int
@fun_from_any@ (Tcl_Interp* interp, Tcl_Obj* obj)
{
    TRACE_TAG_FUNC (@stem@,
		    "((Tcl_Interp*) %p, (Tcl_Obj*) %p^[%d])",
		    interp, COT_RCO (obj));

    @intrep@ value = @2value@ (interp, obj);
    if (!value) { TRACE_TAG_RETURN (@stem@,
				    "(error) %d", TCL_ERROR); }
    COT_FIR (obj);
    COT_PTR (obj) = value;
    @ref@ (value);
    COT_SET_TYPE (obj);

    TRACE_TAG_RETURN (@stem@,
		      "(ok) %d", TCL_OK);
}

#undef COT_FIR
#undef COT_PTR
#undef COT_PTB
#undef COT_SET_TYPE
#undef COT_IS_TYPE
#undef COT_RCO
#undef COT_RCV

#endif /* @stem@_IMPLEMENTATION */
/* # # ## ### ##### ######## ############# ##################### */

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
