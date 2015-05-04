# commands and procs
#
# Copyright (c) 2015 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# Example of basic ccommand, cproc, cdata, and ccode declarations.

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl 8.4
package require critcl 3

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::license {Andreas Kupries} BSD
critcl::summary {Basic example of declarations}
critcl::subject ccommand cproc cdata ccode
critcl::description {
    This code demonstrates a basic set of critcl declarations
    for commands, procedures, constants, and embedded C code.
}

# # ## ### ##### ######## ############# #####################

namespace eval foo {}

critcl::ccode {
    #define FOO "fara"
}

critcl::cdata foo::version 1.0

critcl::cproc foo::double {int x} int {
    return 2 * x;
}

critcl::command foo::bar {} {
    /* clientdata interp objc objv */
    Tcl_AppendResult (interp, "barista", NULL);
    return TCL_OK;
}

critcl::ccode {
    #include <math.h>
}

critcl::cproc foo::sqrt {double x} double {
    return sqrt (x);
}

# ### ### ### ######### ######### #########
## Ready
package provide foo 1
