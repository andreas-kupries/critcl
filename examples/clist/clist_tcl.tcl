# clist_tcl.tcl --
#
#	Tcl companion file to clist. Takes the primitives exported
#	by the C parts and arranges them into a nice and proper
#	ensemble command.
#
# Copyright (c) 2011 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# Example of using a Tcl companion file to put a layer of structure
# (and/or policy) on top of a set of C primitives.

# # ## ### ##### ######## ############# #####################
## Requirements.

# See clist.tcl

# # ## ### ##### ######## ############# #####################
## Implementation.

namespace eval ::clist {
    namespace export map foldr filter
    namespace ensemble create
}

# ### ### ### ######### ######### #########
## OK

