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

#if @havefree@
static void @free@ (Tcl_Obj* ObjPtr);
#endif
#if @havedupl@
static void @dupl@ (Tcl_Obj* ObjPtr, Tcl_Obj* dupObjPtr);
#endif
#if @have2str@
static void @2str@ (Tcl_Obj* ObjPtr);
#endif
#if @havefrom@
static int  @from@ (Tcl_Interp* interp, Tcl_Obj* ObjPtr);
#endif

static Tcl_ObjType @objtypevar@ = {
  "@name@",
  @free@, /* Release Internal Representation            */
  @dupl@, /* Duplicate Internal Representation          */
  @2str@, /* Convert Internal Representation to String  */
  @from@  /* Convert String To Internal Representation  */
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

#define OT_LONG(o)   ((o)->internalRep.longValue)
#define OT_DOUBLE(o) ((o)->internalRep.doubleValue)
#define OT_PTR(o)    ((o)->internalRep.otherValuePtr)
#define OT_WIDE(o)   ((o)->internalRep.wideValue)
#define OT_PTR2(o)   ((o)->internalRep.twoPtr.ptr2)
#define OT_LONG2(o)  ((o)->internalRep.ptrAndLongRep.value)

#define OT_DUP_STR(o, len, buffer)		\
    (o)->bytes = ckalloc((len) + 1);		\
    memcpy ((o)->bytes, buffer, (unsigned) (len) + 1); \
    (o)->length = (len);

#define OT_SET_STR(o, len, buffer) \
    (o)->bytes = (buffer);	 \
    (o)->length = (len);

/* # # ## ### ##### ######## User: General support */
@support@
#line 66 "objtype.h"
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

  @constructor@
#line 88 "objtype.h"
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
      if (@from@ (interp, obj) != TCL_OK) {
	  return TCL_ERROR;
      }
  }

  @get@
#line 110 "objtype.h"
  return TCL_OK;
}

/* # # ## ### ##### ######## ############# ##################### */
/*
 * Definitions - ObjType Internals.
 */
#if @havefree@
static void
@free@ (Tcl_Obj* obj)
{
  @destructor@
#line 123 "objtype.h"
}
#endif
#if @havedupl@
static void
@dupl@ (Tcl_Obj* obj,
	Tcl_Obj* dupobj)
{
  @copy@
#line 132 "objtype.h"
}
#endif
#if @have2str@
static void
@2str@ (Tcl_Obj* obj)
{
  @2string@
#line 140 "objtype.h"
}
#endif
#if @havefrom@
static int
@from@ (Tcl_Interp* interp,
	Tcl_Obj*    obj)
{
  @intrep@ value;

  @parse@
#line 151 "objtype.h"
  /*
   * Kill the old intrep. This was delayed as much as possible.
   */

  FreeIntRep (obj);
  @constructor@
#line 158 "objtype.h"
  obj->typePtr = &@objtypevar@;
  return TCL_OK;
}
#endif
#endif /* @stem@_IMPLEMENTATION */
/* # # ## ### ##### ######## ############# ##################### */

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
