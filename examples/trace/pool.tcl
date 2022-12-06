# pool.tcl --
#
#	A template demonstrating the cproc/ccommand tracing facility.
#	Built on top of the string pool demonstrator.
#
# Copyright (c) 2014,2022 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl              8.6
package require critcl           3.2
package require critcl::literals 1.1 ;# result-type

# Activate tracing code
critcl::config trace yes

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::license {Andreas Kupries} BSD

critcl::summary {Tracing cproc, ccomands, etc}

critcl::description {
    This package implements nothing. It serves only as a
    demonstration and template on how to activate the builtin
    tracing of cprocs, ccommands, etc.
}

critcl::subject demonstration trace {narrative trace} debug

# # ## ### ##### ######## ############# #####################
## C code.

critcl::literals::def demo {
    here  "here"
    comes "comes"
    the   "the"
    sun   "sun"
}

critcl::cproc str {Tcl_Interp* ip int code} object {
    Tcl_Obj* res = demo (ip, code);
    Tcl_IncrRefCount (res);
    return res;
}

critcl::cproc xstr {Tcl_Interp* ip int code} demo {
    return code;
}

critcl::cconst moon int 55

critcl::ccommand nothing {} {
    /* Set nothing */
    return TCL_OK;
}

# str 0
# str 7 - panic, abort, core dump

# ### ### ### ######### ######### #########
## Ready
package provide pool 1
