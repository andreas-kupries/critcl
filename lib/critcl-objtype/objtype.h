/*
 * For package "@package@".
 * Implementation of Tcl_ObjType "@name@".
 */

#ifndef @stem@_IMPLEMENTATION
#define @stem@_IMPLEMENTATION (1)

#include <string.h> /* memcpy (OT_DUP_STR) */

/* # # ## ### ##### ######## ############# ##################### */
/*
 * Declarations - Tcl_ObjType internals.
 */

#if @have_destructor@
static void @fun_destructor@ (Tcl_Obj* objPtr);
#endif
#if @have_copy@
static void @fun_copy@ (Tcl_Obj* objPtr, Tcl_Obj* dupObjPtr);
#endif
static void @fun_stringify@ (Tcl_Obj* objPtr);
static int  @fun_from_any@ (Tcl_Interp* interp, Tcl_Obj* objPtr);

static Tcl_ObjType @objtypevar@ = {
  "@name@",
  @fun_destructor@, /* Release Internal Representation            */
  @fun_copy@,       /* Duplicate Internal Representation          */
  @fun_stringify@,  /* Convert Internal Representation to String  */
  @fun_from_any@    /* Convert String To Internal Representation  */
};

#ifndef FreeIntRep
#define FreeIntRep(objPtr) \
    if ((objPtr)->typePtr != NULL) { \
	if ((objPtr)->typePtr->freeIntRepProc != NULL) { \
	    (objPtr)->typePtr->freeIntRepProc(objPtr); \
	} \
	(objPtr)->typePtr = NULL; \
    }
#endif

#ifndef OT_LONG
#define OT_LONG(o)    ((o)->internalRep.longValue)
#define OT_DOUBLE(o)  ((o)->internalRep.doubleValue)
#define OT_PTR(o)     ((o)->internalRep.otherValuePtr)
#define OT_WIDE(o)    ((o)->internalRep.wideValue)
#define OT_PTR2(o)    ((o)->internalRep.twoPtr.ptr2)
#define OT_LONG2(o)   ((o)->internalRep.ptrAndLongRep.value)
#define OT_STR_VAL(o) (Tcl_GetString (o))
#define OT_STR_LEN(o) ((o)->length)

#define OT_STR_DUP(o, len, buffer)		       \
    (o)->bytes = ckalloc((len) + 1);		       \
    memcpy ((o)->bytes, buffer, (unsigned) (len) + 1); \
    (o)->bytes[(len)] = '\0';			       \
    (o)->length = (len)

#define OT_STR_SET(o, len, buffer)	\
    (o)->bytes = (buffer);		\
    (o)->length = (len)

#define OT_STR_DS(o,ds) \
    OT_STR_DUP (o, Tcl_DStringLength (ds), Tcl_DStringValue (ds)); \
    Tcl_DStringFree ((ds))
#endif

/* # # ## ### ##### ######## User: General support */
@code_support@
#line 68 "objtype.h"
/* # # ## ### ##### ######## */

/* # # ## ### ##### ######## ############# ##################### */
/*
 * Definitions -- Public API.
 */

/*
 * Generate a Tcl_Obj* representing the 'value'.
 * -
 * The value is essentially copied in, the Tcl_Obj* becomes
 * responsible for any release which might be necessary.
 */

Tcl_Obj*
@api_new@ (@intrep@ value)
{
  Tcl_Obj* obj = Tcl_NewObj ();
  Tcl_InvalidateStringRep (obj);

  @code_constructor@
#line 90 "objtype.h"
  obj->typePtr = &@objtypevar@;
  return obj;
}

/*
 * Extract the 'value' from the Tcl_Obj*, assume or convert to
 * @intrep@ stored in the 'obj', or fail.
 */

int
@api_from@ (Tcl_Interp* interp,
	    Tcl_Obj*    obj,
	    @intrep@*   value)
{
  if (obj->typePtr != &@objtypevar@) {
      if (@fun_from_any@ (interp, obj) != TCL_OK) {
	  return TCL_ERROR;
      }
  }

  @code_get@
#line 112 "objtype.h"
  return TCL_OK;
}

/* # # ## ### ##### ######## ############# ##################### */
/*
 * Definitions - ObjType Internals.
 */
#if @have_destructor@
static void
@fun_destructor@ (Tcl_Obj* obj)
{
  @code_destructor@
#line 125 "objtype.h"
}
#endif
#if @have_copy@
static void
@fun_copy@ (Tcl_Obj* obj, Tcl_Obj* dupobj)
{
  @code_copy@
#line 133 "objtype.h"
}
#endif

static void
@fun_stringify@ (Tcl_Obj* obj)
{
  @code_stringify@
#line 141 "objtype.h"
}

static int
@fun_from_any@ (Tcl_Interp* interp, Tcl_Obj* obj)
{
  @intrep@ value;

  @code_from_any@
#line 150 "objtype.h"
  /*
   * Kill the old intrep. This was delayed as much as possible.
   */

  FreeIntRep (obj);
  @code_constructor@
#line 157 "objtype.h"
  obj->typePtr = &@objtypevar@;
  return TCL_OK;
}
#endif /* @stem@_IMPLEMENTATION */
/* # # ## ### ##### ######## ############# ##################### */

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
