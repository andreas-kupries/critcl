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
# Copyright (c) 2012,2022 Andreas Kupries <andreas_kupries@users.sourceforge.net>
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.6
package require critcl 3.2

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

critcl::cheaders stackc/*.h ; # Method declarations and implementation,
critcl::csources stackc/*.c ; # outside of this main file.

critcl::class::define ::stackc {
    include m.h                  ; # Method function declarations.
    include cstack/cstackDecls.h ; # API of the generic CSTACK we are binding to.
    type    CSTACK

    constructor {
	instance = cstack_new (StackcFreeCell, 0);
    } {
	/* Set back reference from CSTACK instance to instance command */
	cstack_clientdata_set (instance, (ClientData) cmd);
    }

    destructor {
	/* Release the whole stack. */
	cstack_del (instance);
    }

    method clear   as stm_CLEAR
    method destroy as stm_DESTROY
    method peek    as stm_PEEK 0 0
    method peekr   as stm_PEEK 0 1
    method pop     as stm_PEEK 1 0
    method push    as stm_PUSH
    method rotate  as stm_ROTATE
    method size    as stm_SIZE
    method get     as stm_GET 0
    method getr    as stm_GET 1
    method trim    as stm_TRIM 1
    method trimv   as stm_TRIM 0

    support {
	static void
	StackcFreeCell (void* cell) {
	    /* Release the cell. */
	    Tcl_DecrRefCount ((Tcl_Obj*) cell);
	}
    }
}

# ### ### ### ######### ######### #########
## Ready
package provide stackc 1
