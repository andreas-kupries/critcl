# point2b_objtype.tcl --
#
#       Example for a critcl::objtype generated Tcl_ObjType.
#       Demonstrates a basic structure type (without reference counting).
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
critcl::tcl 8.5

# # ## ### ##### ######## ############# #####################
## Implementation.

critcl::objtype structure point2b {
    double x
    double y
} -format list

# ### ### ### ######### ######### #########
## Interface for testing

critcl::cproc point2b-new {double x double y} point2b {
    point2b* p = (point2b*) ckalloc (sizeof (point2b));
    p->refCount = 0;
    p->x = x;
    p->y = y;
    return p;
}

critcl::cproc point2b-x {point2b p} double {
    return p->x;
}

critcl::cproc point2b-y {point2b p} double {
    return p->y;
}

# ### ### ### ######### ######### #########
## Ready
package provide point2b_objtype 1
