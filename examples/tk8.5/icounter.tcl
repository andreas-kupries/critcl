# tk8.5.tcl --
#
#       Implementation of a counter associated an interpreter.
#       This code based on critcl v3.1, API compatible to the PTI [x].
#       [x] Pure Tcl Implementation.
#
# Copyright (c) 2016,2022 Andreas Kupries <andreas_kupries@users.sourceforge.net>
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.6 9
package require critcl 3.2

critcl::buildrequirement {
    package require critcl::iassoc ; # Maintain an interpreter association.
}

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::tcl 8.5
critcl::tk

# # ## ### ##### ######## ############# #####################
## Define and maintain the per-interp structure.

critcl::iassoc::def icounter {int base} {
    int counter; /* The counter variable */
} {
    data->counter = base;
} {
    /* Nothing to release */
}

# # ## ### ##### ######## ############# #####################
## Access and expose the per-interp structure to scripts.

critcl::cproc icounter {Tcl_Interp* interp int base} int {
    icounter_data d = icounter (interp, base);
    d->counter ++;
    return d->counter;
}

# ### ### ### ######### ######### #########
## Ready
package provide icounter 1
