# icounter.tcl --
#
#       Implementation of a counter associated an interpreter.
#       This code based on critcl v3.1, API compatible to the PTI [x].
#       [x] Pure Tcl Implementation.
#
# Copyright (c) 2012 Andreas Kupries <andreas_kupries@users.sourceforge.net>
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id: stackc.tcl,v 1.1 2008/06/19 23:03:35 andreas_kupries Exp $

package require Tcl 8.4
package require critcl 3.1

critcl::buildrequirement {
    package require critcl::iassoc ; # Maintain an interpreter association.
}

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::license {Andreas Kupries} {BSD licensed}

critcl::summary {Per interpreter global counters.}

critcl::description {
    This package implements a per-interpreter counter.
}

critcl::subject counter

# # ## ### ##### ######## ############# #####################
## Define and maintain the per-interp structure.

critcl::iassoc::def icounter {
    int counter; /* The counter variable */
} {
    data->counter = 0;
} {
    /* Nothing to release */
}

# # ## ### ##### ######## ############# #####################
## Access and expose the per-interp structure to scripts.

critcl::cproc icounter {Tcl_Interp* interp} int {
    icounter_data d = icounter (interp);
    d->counter ++;
    return d->counter;
}

# ### ### ### ######### ######### #########
## Ready
package provide icounter 1
