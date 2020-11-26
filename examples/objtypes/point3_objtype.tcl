# point3_objtype.tcl --
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
critcl::description {Demonstration of custom structure Tcl_ObjType. 3D Points.}

critcl::subject Tcl_ObjType {3d points} {points in 3d}
critcl::tcl 8.5

# # ## ### ##### ######## ############# #####################
## Implementation.

critcl::ccode {
    /* Manually implement the opaque point3 type. */
    typedef struct p3d {
	int rc; double x; double y; double z;
    } p3d;
    typedef struct p3d* p3;

    static p3 p3_new (double x, double y, double z) {
	p3 p = (p3) ckalloc (sizeof (p3d));
	p->rc = 0;
	p->x  = x;
	p->y  = y;
	p->z  = z;
	return p;
    }
    static int p3_refcount (p3 p) {
	return p->rc;
    }
    static void p3_ref (p3 p) {
	p->rc ++;
    }
    static void p3_unref (p3 p) {
	if (p->rc <= 1) { ckfree ((char*) p); return; }
	p->rc --;
    }
    static void p3_2string (p3 p, Tcl_DString* ds) {
	char buf [TCL_DOUBLE_SPACE];
	Tcl_PrintDouble (0, p->x, buf);	Tcl_DStringAppendElement (ds, buf);
	Tcl_PrintDouble (0, p->y, buf);	Tcl_DStringAppendElement (ds, buf);
	Tcl_PrintDouble (0, p->z, buf);	Tcl_DStringAppendElement (ds, buf);
    }
    static int p3_to_value_do (Tcl_Interp* ip, Tcl_Obj* o, p3 value) {
	int lc, res;
	Tcl_Obj** lv;
	res = Tcl_ListObjGetElements (ip, o, &lc, &lv);
	if (res != TCL_OK) { return TCL_ERROR; }
	if (lc != 3) {
	    Tcl_AppendResult (ip, "Bad number of elements, expected 3", NULL);
	    return TCL_ERROR;
	}
	if (Tcl_GetDoubleFromObj(ip, lv [0], &value->x) != TCL_OK) return TCL_ERROR;
	if (Tcl_GetDoubleFromObj(ip, lv [1], &value->y) != TCL_OK) return TCL_ERROR;
	if (Tcl_GetDoubleFromObj(ip, lv [2], &value->z) != TCL_OK) return TCL_ERROR;
	return TCL_OK;
    }
    static p3 p3_2value (Tcl_Interp* ip, Tcl_Obj* o) {
	p3 p = (p3) ckalloc (sizeof (p3d));
	int res = p3_to_value_do (ip, o, p);
	if (res == TCL_OK) { return p; }
	ckfree ((char*) p);
	return 0;
    }
}

critcl::objtype handle point3 -type p3

# ### ### ### ######### ######### #########
## Interface for testing

critcl::cproc point3-new {
    double x
    double y
    double z
} point3 {
    return p3_new (x, y, z);
}

critcl::cproc point3-x {point3 p} double { return p->x; }
critcl::cproc point3-y {point3 p} double { return p->y; }
critcl::cproc point3-z {point3 p} double { return p->z; }

# ### ### ### ######### ######### #########
## Ready
package provide point3_objtype 1
