# bitmap.tcl --
#
#	A template demonstrating the handling of enum conversions.
#
# Copyright (c) 2014,2022 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl          8.6 9
package require critcl       3.1.11
package require critcl::enum 1

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

critcl::cproc encode {Tcl_Interp* ip Tcl_Obj* str} int {
    int val;
    demo_GetFromObj (ip, str, 0, &val);
    return val;
}

critcl::cproc decode {Tcl_Interp* ip int val} object {
    Tcl_Obj* res = demo_ToObj (ip, val);
    Tcl_IncrRefCount (res);
    return res;
}

# Encode hidden in the argtype.
critcl::cproc xencode {Tcl_Interp* ip demo str} int {
    return str;
}

# Encode hidden in the argtype.
critcl::cproc xencode-p {Tcl_Interp* ip demo-prefix str} int {
    return str;
}

# Decode hidden in the resultype
critcl::cproc xdecode {Tcl_Interp* ip int val} demo {
    return val;
}

# encode    exact  => 1
# xencode   filler => 2
# xencode-p glob   => 0
# xencode   glob   => /error/

# decode  2 => filler
# xdecode 0 => global
# decode  8 => panic, abort, core dump

# ### ### ### ######### ######### #########
## Ready
package provide enum 1
