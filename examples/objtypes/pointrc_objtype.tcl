# point_objtype.tcl --
#
#       Example for a critcl::objtype generated Tcl_ObjType.
#       Demonstrates a basic structure type with reference counting.
#
# Copyright (c) 2020 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl 8.5
package require critcl 3.1 ;# stubs management

critcl::buildrequirement {
    package require critcl::objtype
}

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::license     {Andreas Kupries} BSD
critcl::summary     {Demonstration of custom structure Tcl_ObjType}
critcl::description {Demonstration of custom structure Tcl_ObjType. 2D Points.}

critcl::subject Tcl_ObjType {2d points} {points in 2d}

# # ## ### ##### ######## ############# #####################
## Implementation.

critcl::objtype define pointrc {
    support {
        typedef struct pointrc {
            int refCount;
	    double x;
	    double y;
        } pointrc;
    }

    intrep pointrc*

    get { *value = (@intrep@) OT_PTR (obj); }

    constructor {
        OT_PTR (obj) = value;
        value->refCount ++;
    }

    destructor { /* Tcl_Obj* obj */
        @intrep@ value = ((@intrep@) OT_PTR (obj));
        OT_PTR (obj) = NULL;
        if (value->refCount > 1) { value->refCount --; return; }
        Tcl_Free (OT_PTR (obj));
    }

    copy { /* Tcl_Obj* obj, Tcl_Obj* dupobj */
        OT_PTR (dupobj) = OT_PTR (obj);
        ((@intrep@) OT_PTR (obj))->refCount ++;
    }

    stringify { /* Tcl_Obj* obj */
        /* Create a string rep which is usable as list.
	 * That makes the parsing (see below) simpler on us.
	 * We make it simple, no tagging. Just the coordinates.
	 */
        @intrep@ p = (@intrep@) OT_PTR (obj);
        Tcl_DString      ds;
        Tcl_DStringInit (&ds);

        char buf [TCL_DOUBLE_SPACE+1];

        Tcl_PrintDouble (0, p->x, buf); Tcl_DStringAppendElement (&ds, buf);
        Tcl_PrintDouble (0, p->y, buf); Tcl_DStringAppendElement (&ds, buf);

        OT_STR_DS (obj, &ds);
    }

    from-any { /* Tcl_Interp* interp, Tcl_Obj* obj, @intrep@ value */
        /* Shimmers incoming intrep to list, then get the elements, and
	 * at last the doubles we need out of these.
	 */
        Tcl_Obj** cv;
        int       cc, res;
        pointrc   vp;

        res = Tcl_ListObjGetElements (interp, obj, &cc, &cv);
        if (res != TCL_OK) { return res; }
        if (cc != 2) {
            Tcl_AppendResult (interp, "@name@ expected exactly 2 doubles", NULL);
            return TCL_ERROR;
        }

        res = Tcl_GetDoubleFromObj (interp, cv[0], &vp.x);
        if (res != TCL_OK) { return res; }

        res = Tcl_GetDoubleFromObj (interp, cv[1], &vp.y);
        if (res != TCL_OK) { return res; }

        vp.refCount = 0;

        value = (@intrep@) Tcl_Alloc (sizeof (pointrc));
        *value = vp;
    }
}

# ### ### ### ######### ######### #########
## Ready
package provide pointrc_objtype 1
