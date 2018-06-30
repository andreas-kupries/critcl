# pool.tcl --
#
#	A template demonstrating the handling of string/literal pools.
#	Configured for multi-access
#
# Copyright (c) 2018 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl              8.4
package require critcl           3.1.11
package require critcl::literals 1.3 ;# result-type

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
    {string pool} {literal pool} \
    {shared strings} {shared literals} \
    {multi-access}

# # ## ### ##### ######## ############# #####################
## C code.

critcl::literals::def demo {
    here  "here"
    comes "comes"
    the   "the"
    sun   "sun"
} +list

critcl::cproc strs {Tcl_Interp* ip int args} object {
    Tcl_Obj* res = demo_list (ip, args.c, args.v);
    Tcl_IncrRefCount (res);
    return res;
}

# str 0
# str 7 - panic, abort, core dump

# ### ### ### ######### ######### #########
## Ready
package provide pool 1
