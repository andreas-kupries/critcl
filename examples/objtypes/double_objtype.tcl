# double_objtype.tcl --
#
#	Example for critcl::objtype generated Tcl_ObjType.
#	Demonstrates a simple double type.
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
critcl::summary     {Demonstration of custom double Tcl_ObjType}
critcl::description {Demonstration of custom double Tcl_ObjType}

critcl::subject Tcl_ObjType double

# # ## ### ##### ######## ############# #####################
## Implementation.

critcl::objtype define double {
    support {
	#include <stdlib.h> /* atof */
    }
    intrep double

    get { *value = OT_DOUBLE (obj); }

    constructor { OT_DOUBLE (obj) = value; }

    stringify {
	char buffer[TCL_DOUBLE_SPACE+1];
	int len = sprintf (buffer, "%g", OT_DOUBLE (obj));
	OT_STR_DUP (obj, len, buffer);
    }

    from-any {
	value = atof (OT_STR_VAL (obj));
    }
}

# ### ### ### ######### ######### #########
## Ready
package provide double_objtype 1
