# intobj.tcl --
#
#	Example for critcl::objtype generated Tcl_ObjType.
#	Demonstrates an integer type.
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

critcl::summary {Demonstration of generated Tcl_ObjType, int}

critcl::description {
    Demonstration of generated Tcl_ObjType, int
}

critcl::subject Tcl_ObjType

# # ## ### ##### ######## ############# #####################
## Configuration

# # ## ### ##### ######## ############# #####################
## Exported API

# # ## ### ##### ######## ############# #####################
## Implementation.

critcl::objtype define int {
    intrep int
    #api    integer
    stubs

    constructor {
	OT_LONG (obj) = value;
    }

    get {
	*value = OT_LONG (obj);
    }

    2string {
	int len;
	char buffer[TCL_INTEGER_SPACE];

	len = sprintf (buffer, "%d", OT_LONG (obj));
	OT_DUP_STR (obj, len, buffer);
    }

    parse {
	/* ... */
    }
}

# ### ### ### ######### ######### #########
## Ready
package provide c::intobj 1
