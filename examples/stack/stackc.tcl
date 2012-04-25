# stackc.tcl --
#
#       Implementation of a stack data structure for Tcl.
#       This code based on critcl v3.1, API compatible to the PTI [x].
#       [x] Pure Tcl Implementation.
#
# Demonstrates not just the stubs import and meta data declaration,
# but also the utility package for the creation of classes and objects
# in C, with both claaes and their instances represented as Tcl
# commands.
#
# Copyright (c) 2012 Andreas Kupries <andreas_kupries@users.sourceforge.net>
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id: stackc.tcl,v 1.1 2008/06/19 23:03:35 andreas_kupries Exp $

package require Tcl 8.4
package require critcl 3.1

critcl::buildrequirement {
    package require critcl::class ; # DSL, easy spec of Tcl class/object commands.
}

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::license {Andreas Kupries} {BSD licensed}

critcl::summary {Stack objects for Tcl.}

critcl::description {
    This package implements stack objects
    for Tcl. It uses the abstract data type
    provided by package 'cstack' for actual
    storage and operations.
}

critcl::subject stack
critcl::subject {data structure}
critcl::subject structure
critcl::subject {abstract data structure}
critcl::subject {generic data structure}

# # ## ### ##### ######## ############# #####################
## Configuration and implementation.

critcl::api import cstack 1

critcl::cheaders stackc/*.h
critcl::csources stackc/*.c

critcl::class::def ::stackc {
    include m.h
    include cstack/cstackDecls.h
    type    CSTACK

    constructor {
	instance = cstack_new (stackc_FreeCell, 0);
    } {
	cstack_clientdata_set (instance, (ClientData) cmd);
    }

    destructor {
	/* Release the whole stack. */
	cstack_del (instance);
    }

    support {
	static void
	stackc_FreeCell (void* cell) {
	    /* Release the cell. */
	    Tcl_DecrRefCount ((Tcl_Obj*) cell);
	}
    }

    mdef clear   as stm_CLEAR
    mdef destroy as stm_DESTROY
    mdef peek    as stm_PEEK 0 0
    mdef peekr   as stm_PEEK 0 1
    mdef pop     as stm_PEEK 1 0
    mdef push    as stm_PUSH
    mdef rotate  as stm_ROTATE
    mdef size    as stm_SIZE
    mdef get     as stm_GET 0
    mdef getr    as stm_GET 1
    mdef trim    as stm_TRIM 1
    mdef trimv   as stm_TRIM 0
}

# ### ### ### ######### ######### #########
## Ready
package provide stackc 1
