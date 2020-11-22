# int_objtype.tcl --
#
#	Example for critcl::objtype generated Tcl_ObjType.
#	Demonstrates a simple integer type.
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
critcl::summary     {Demonstration of custom integer Tcl_ObjType}
critcl::description {Demonstration of custom integer Tcl_ObjType}

critcl::subject Tcl_ObjType integer

# # ## ### ##### ######## ############# #####################
## Implementation.

critcl::objtype define int {
    support {
	#include <stdlib.h> /* atoi */
    }
    intrep int
    stubs

    get { *value = OT_LONG (obj); }

    constructor { OT_LONG (obj) = value; }

    stringify {
	char buffer[TCL_INTEGER_SPACE+1];
	int len = sprintf (buffer, "%ld", OT_LONG (obj));
	OT_STR_DUP (obj, len, buffer);
    }

    from-any {
	value = atoi (OT_STR_VAL (obj));
    }
}

# ### ### ### ######### ######### #########
## Ready
package provide int_objtype 1
