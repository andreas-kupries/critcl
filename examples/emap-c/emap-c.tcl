# emap_ex.tcl --
#
#	A template demonstrating the handling of emap conversions.
#
# Copyright (c) 2014 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl          8.4
package require critcl       3.1.11
package require critcl::emap 1

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::license {Andreas Kupries} BSD

critcl::summary {Bitmap conversion}

critcl::description {
    This package implements nothing. It serves only as a
    demonstration and template on how to declare an emap
    converter and use it in cproc's or ccommand's.
}

critcl::subject demonstration {emap conversion} {encode emap} \
    {decode emap} {convert emap}

# # ## ### ##### ######## ############# #####################
## C code.

critcl::ccode {
    #define STATE_INIT 0
    #define STATE_MIX  1
    #define STATE_DONE 2
}

critcl::emap::def demo {
    init STATE_INIT
    mix  STATE_MIX
    done STATE_DONE
} -mode c
# Add -nocase as last argument for case-insensitive Tcl strings.

critcl::cproc encode {Tcl_Interp* ip Tcl_Obj* state} int {
    return demo_encode_cstr (Tcl_GetString(state));
}

critcl::cproc decode {Tcl_Interp* ip int scode} object {
    Tcl_Obj* res = Tcl_NewStringObj (demo_decode_cstr (scode), -1);
    if (res) { Tcl_IncrRefCount (res); }
    return res;
}

# encode {exact filler} => 6
# decode 5              => {global filler}

# ### ### ### ######### ######### #########
## Ready
package provide emap-c 1
