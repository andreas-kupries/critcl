# bitmap.tcl --
#
#	A template demonstrating the handling of bitmap conversions.
#
# Copyright (c) 2014,2022 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl            8.6 9
package require critcl         3.2
package require critcl::bitmap 1

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::license {Andreas Kupries} BSD

critcl::summary {Bitmap conversion}

critcl::description {
    This package implements nothing. It serves only as a
    demonstration and template on how to declare a bitmap
    converter and use it in cproc's or ccommand's.
}

critcl::subject demonstration {bitmap conversion} {encode bitmap} \
    {decode bitmap} {convert bitmap}

# # ## ### ##### ######## ############# #####################
## C code.

critcl::bitmap::def demo {
    global 1
    exact  2
    filler 4
}

critcl::cproc encode {Tcl_Interp* ip Tcl_Obj* flags} int {
    int mask;
    demo_encode (ip, flags, &mask);
    return mask;
}

critcl::cproc decode {Tcl_Interp* ip int mask} object {
    Tcl_Obj* res = demo_decode (ip, mask);
    Tcl_IncrRefCount (res);
    return res;
}

# Encode hidden in the argtype.
critcl::cproc xencode {Tcl_Interp* ip demo flags} int {
    return flags;
}

# Decode hidden in the resultype
critcl::cproc xdecode {Tcl_Interp* ip int mask} demo {
    return mask;
}

# encode {exact filler} => 6
# decode 5              => {global filler}

# ### ### ### ######### ######### #########
## Ready
package provide bitmap 1
