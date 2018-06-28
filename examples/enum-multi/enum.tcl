# bitmap.tcl --
#
#	A template demonstrating the handling of enum conversions.
#	Configured for multi-access.
#
# Copyright (c) 2018 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl          8.4
package require critcl       3.1.11
package require critcl::enum 1.1

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::license {Andreas Kupries} BSD

critcl::summary {Enum conversion}

critcl::description {
    This package implements nothing. It serves only as a
    demonstration and template on how to declare an enum
    converter and use it in cproc's or ccommand's.
}

critcl::subject demonstration {enum conversion} {encode enum} \
    {decode enum} {convert enum}

# # ## ### ##### ######## ############# #####################
## C code.

critcl::enum::def demo {
    E_global global
    E_exact  exact
    E_filler filler
}

critcl::cproc decode {Tcl_Interp* ip int args} object {
    Tcl_Obj* res = demo_ToObjList (ip, args.c, args.v);
    Tcl_IncrRefCount (res);
    return res;
}

# decode  2 => filler
# decode  8 => panic, abort, core dump

# ### ### ### ######### ######### #########
## Ready
package provide enum 1
