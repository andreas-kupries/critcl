# point3t_objtype.tcl --
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
    typedef struct p3td {
	int rc; double x; double y; double z;
    } p3td;
    typedef struct p3td* p3t;

    static p3t p3t_new (double x, double y, double z) {
	p3t p = (p3t) ckalloc (sizeof (p3td));
	p->rc = 0;
	p->x  = x;
	p->y  = y;
	p->z  = z;
	return p;
    }
    static int p3t_refcount (p3t p) {
	return p->rc;
    }
    static void p3t_ref (p3t p) {
	p->rc ++;
    }
    static void p3t_unref (p3t p) {
	if (p->rc <= 1) { ckfree ((char*) p); return; }
	p->rc --;
    }
    static void p3t_2string (p3t p, Tcl_DString* ds) {
	char buf [TCL_DOUBLE_SPACE];
	Tcl_PrintDouble (0, p->x, buf);	Tcl_DStringAppendElement (ds, buf);
	Tcl_PrintDouble (0, p->y, buf);	Tcl_DStringAppendElement (ds, buf);
	Tcl_PrintDouble (0, p->z, buf);	Tcl_DStringAppendElement (ds, buf);
    }
    static int p3t_to_value_do (Tcl_Interp* ip, Tcl_Obj* o, p3t value) {
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
    static p3t p3t_2value (Tcl_Interp* ip, Tcl_Obj* o) {
	p3t p = (p3t) ckalloc (sizeof (p3td));
	int res = p3t_to_value_do (ip, o, p);
	if (res == TCL_OK) { return p; }
	ckfree ((char*) p);
	return 0;
    }
}

critcl::objtype handle point3t -type p3t -trace on

# ### ### ### ######### ######### #########
## Interface for testing

critcl::cproc point3t-new {
    double x
    double y
    double z
} point3t {
    return p3t_new (x, y, z);
}

critcl::cproc point3t-x {point3t p} double { return p->x; }
critcl::cproc point3t-y {point3t p} double { return p->y; }
critcl::cproc point3t-z {point3t p} double { return p->z; }

# ### ### ### ######### ######### #########
## Ready
package provide point3t_objtype 1
