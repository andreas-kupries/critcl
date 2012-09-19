# cit.tcl --
#
#	Example for critcl::objtype generated Tcl_ObjType.
#	Demonstrates a type with un-allocated data (crimp imagetype)
#	I.e. the internal representation fits into the
#	relevant Tcl_Obj structure fields.
#
# Copyright (c) 20012 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl 8.5
package require critcl 3.1 ;# stubs management
package require critcl::objtype

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::license {Andreas Kupries} BSD

critcl::summary {Demonstration of generated Tcl_ObjType, crimp image type}

critcl::description {
    Demonstration of generated Tcl_ObjType.
    crimp image type
}

critcl::subject Tcl_ObjType

# # ## ### ##### ######## ############# #####################
## Implementation.

critcl::objtype define imagetype {

    support {
	/* Fake structure for the demo. */
	typedef struct _cit_ {
	    char* name;
	} crimp_imagetype;
    }

    intrep  crimp_imagetype*
    newobj  crimp_new_imagetype_obj
    fromobj crimp_get_imagetype_from_obj

    constructor {
	OT_PTR (obj) = (void*) value;
    }

    # free = nothing to be done for the non-allocated pointer.
    # dup  = plain copy of the non-allocated pointer.

    get {
	*value = (@intrep@) OT_PTR (obj);
    }

    2string {
	crimp_imagetype* cit = (@intrep@) OT_PTR (obj);
	int              len = strlen (cit->name);

	OT_DUP_STR (obj, len, cit->name);
    }

    parse {
	const char* name = Tcl_GetString (obj);

	value = crimp_imagetype_find (name);

	if (!value) {
	    Tcl_AppendResult (interp, "expected crimp image type, got \"", name, "\"", NULL);
	    return TCL_ERROR;
	}
    }
}

# ### ### ### ######### ######### #########
## Ready
package provide cit 1
