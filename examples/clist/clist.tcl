# clist.tcl --
#
#	Set of list processing primitives. A Tcl companion file is then
#	used to provide structure on top, here, an ensemble command.
#
# Copyright (c) 2011 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# Example of using a Tcl companion file to put a layer of structure
# (and/or policy) on top of a set of C primitives.

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl 8.5
package require critcl 3 ;# stubs management

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::tcl 8.5
critcl::license {Andreas Kupries} BSD

critcl::summary {Extended list processing command.}

critcl::description {
    This package implements an ensemble command providing
    additional list processing functionality, like map, filter,
    etc.
}

critcl::subject {list processing} {extended list processing}
critcl::subject map foldr filter
critcl::subject {list map} {list foldr} {list filter}

# # ## ### ##### ######## ############# #####################
## Configuration

critcl::tsources clist_tcl.tcl

# # ## ### ##### ######## ############# #####################
## Implementation.
##
## NOTE: This code is 8.4 based and not NRE ready.

namespace eval ::clist {}

# # ## ### ##### ######## ############# #####################

critcl::cproc ::clist::map {
    Tcl_Interp* interp
    Tcl_Obj* cmdprefix
    Tcl_Obj* list
} Tcl_Obj* {
    /* Equivalent to the following Tcl code:
     * -----------------------------------------------------
     * set result {}
     * foreach e $list { lappend result [{*}$cmdprefix $e] }
     * return $result
     * -----------------------------------------------------
     * This is at its core a specialized fold operation.
     * (initial value {}, operation 'list append')
     */

    Tcl_Obj** cv;
    int       cc;

    Tcl_Obj** lv;
    int       lc;

    Tcl_Obj** cmdv;
    int       cmdc;

    int k, code;
    Tcl_Obj*  rlist;

    if (Tcl_ListObjGetElements (interp, cmdprefix, &cc, &cv) != TCL_OK) {
	return NULL;
    }

    if (Tcl_ListObjGetElements (interp, list, &lc, &lv) != TCL_OK) {
	return NULL;
    }

    cmdc = cc + 1;
    cmdv = (Tcl_Obj**) ckalloc (sizeof (Tcl_Obj*) * cmdc);

    for (k = 0; k < cc; k++) { cmdv [k] = cv [k]; Tcl_IncrRefCount (cmdv [k]); }

    rlist = Tcl_NewListObj (0, NULL);
    Tcl_IncrRefCount (rlist);

    for (k=0; k < lc; k++) {
	cmdv [cc] = lv [k];
	Tcl_IncrRefCount (cmdv [cc]);

	code = Tcl_EvalObjv (interp, cmdc, cmdv, 0);

	Tcl_DecrRefCount (cmdv [cc]);

	if ((code != TCL_OK) ||
	    (Tcl_ListObjAppendElement (interp, rlist, Tcl_GetObjResult (interp)) != TCL_OK)) {
	    goto abort;
	}
    }

done:
    for (k=0; k < cc; k++) { Tcl_DecrRefCount (cmdv [k]); }
    ckfree ((char*) cmdv);
    return rlist;

abort:
    Tcl_DecrRefCount (rlist);
    rlist = NULL;
    goto done;
}

# # ## ### ##### ######## ############# #####################

critcl::cproc ::clist::filter {
    Tcl_Interp* interp
    Tcl_Obj* cmdprefix
    Tcl_Obj* list
} Tcl_Obj* {
    /* Equivalent to the following Tcl code:
     * -----------------------------------------------------
     * set result {}
     * foreach e $list {
     *     if {![{*}$cmdprefix $e]} continue;
     *     lappend result $e
     * }
     * return $result
     * -----------------------------------------------------
     * This is at its core a specialized fold operation.
     * (initial value {}, operation 'conditional list append')
     */

    Tcl_Obj** cv;
    int       cc;

    Tcl_Obj** lv;
    int       lc;

    Tcl_Obj** cmdv;
    int       cmdc;

    int k, code, keep;
    Tcl_Obj*  rlist;

    if (Tcl_ListObjGetElements (interp, cmdprefix, &cc, &cv) != TCL_OK) {
	return NULL;
    }

    if (Tcl_ListObjGetElements (interp, list, &lc, &lv) != TCL_OK) {
	return NULL;
    }

    cmdc = cc + 1;
    cmdv = (Tcl_Obj**) ckalloc (sizeof (Tcl_Obj*) * cmdc);

    for (k = 0; k < cc; k++) { cmdv [k] = cv [k]; Tcl_IncrRefCount (cmdv [k]); }

    rlist = Tcl_NewListObj (0, NULL);
    Tcl_IncrRefCount (rlist);

    for (k=0; k < lc; k++) {
	cmdv [cc] = lv [k];
	Tcl_IncrRefCount (cmdv [cc]);

	code = Tcl_EvalObjv (interp, cmdc, cmdv, 0);

	Tcl_DecrRefCount (cmdv [cc]);

	if ((code != TCL_OK) ||
	    (Tcl_GetBooleanFromObj (interp, Tcl_GetObjResult (interp), &keep) != TCL_OK)) {
	    goto abort;
	}

	if (keep &&
	    (Tcl_ListObjAppendElement (interp, rlist, lv[k]) != TCL_OK)) {
	    goto abort;
	}
    }

done:
    for (k=0; k < cc; k++) { Tcl_DecrRefCount (cmdv [k]); }
    ckfree ((char*) cmdv);
    return rlist;

abort:
    Tcl_DecrRefCount (rlist);
    rlist = NULL;
    goto done;
}

# # ## ### ##### ######## ############# #####################

critcl::cproc ::clist::foldr {
    Tcl_Interp* interp
    Tcl_Obj* cmdprefix
    Tcl_Obj* initial
    Tcl_Obj* list
} Tcl_Obj* {
    /* Equivalent to the following Tcl code:
     * -----------------------------------------------------
     * set result $initial
     * foreach e $list {
     *     set result [{*}$cmdprefix $initial $e]
     * }
     * return $result
     * -----------------------------------------------------
     */

    Tcl_Obj** cv;
    int       cc;

    Tcl_Obj** lv;
    int       lc;

    Tcl_Obj** cmdv;
    int       cmdc;

    int k, code, keep;
    Tcl_Obj*  result;

    if (Tcl_ListObjGetElements (interp, cmdprefix, &cc, &cv) != TCL_OK) {
	return NULL;
    }

    if (Tcl_ListObjGetElements (interp, list, &lc, &lv) != TCL_OK) {
	return NULL;
    }

    cmdc = cc + 2;
    cmdv = (Tcl_Obj**) ckalloc (sizeof (Tcl_Obj*) * cmdc);

    for (k = 0; k < cc; k++) { cmdv [k] = cv [k]; Tcl_IncrRefCount (cmdv [k]); }

    result = Tcl_DuplicateObj (initial);
    Tcl_IncrRefCount (result);

    for (k=0; k < lc; k++) {
	cmdv [cc]   = result;
	cmdv [cc+1] = lv [k];

	Tcl_IncrRefCount (cmdv [cc]);
	Tcl_IncrRefCount (cmdv [cc+1]);

	code = Tcl_EvalObjv (interp, cmdc, cmdv, 0);

	Tcl_DecrRefCount (cmdv [cc]);
	Tcl_DecrRefCount (cmdv [cc+1]);

	if (code != TCL_OK) {
	    goto abort;
	}

	Tcl_DecrRefCount (result);
	result = Tcl_GetObjResult (interp);
	Tcl_IncrRefCount (result);
    }

done:
    for (k=0; k < cc; k++) { Tcl_DecrRefCount (cmdv [k]); }
    ckfree ((char*) cmdv);
    return result;

abort:
    Tcl_DecrRefCount (result);
    result = NULL;
    goto done;
}

# ### ### ### ######### ######### #########
## Ready
package provide clist 1
