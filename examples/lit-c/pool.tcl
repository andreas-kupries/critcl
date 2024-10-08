# pool.tcl --
#
#	A template demonstrating the handling of string/literal pools.
#	Configured for C access, no wrappers for Tcl.
#
# Copyright (c) 2014-2017,2022 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl              8.6 9
package require critcl           3.2
package require critcl::literals 1.2 ;# result-type

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::license {Andreas Kupries} BSD

critcl::summary {String pools}

critcl::description {
    This package implements nothing. It serves only as a
    demonstration and template on how to declare a shared
    string pool and use it in cproc's or ccommand's
}

critcl::subject demonstration \
    {string pool} \
    {literal pool} \
    {shared strings} \
    {shared literals}

# # ## ### ##### ######## ############# #####################
## C code.

critcl::literals::def demo {
    here  "here"
    comes "comes"
    the   "the"
    sun   "sun"
} c

critcl::cproc str {Tcl_Interp* ip int code} object {
    Tcl_Obj* res = Tcl_NewStringObj (demo_cstr (code), -1);
    Tcl_IncrRefCount (res);
    return res;
}

# str 0
# str 7 - panic, abort, core dump

# # ## ### ##### ######## ############# #####################

# ### ### ### ######### ######### #########
## Ready
package provide pool 1
