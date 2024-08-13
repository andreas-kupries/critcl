# ingress.tcl --
#
#	A template demonstrating the handling of all (non-deprecated)
#	argument types to cproc.
#
# Copyright (c) 2020,2022 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl 8.6 9
package require critcl 3.2

critcl::config keepsrc 1

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::license {Andreas Kupries} BSD
critcl::summary {Ingress of arguments for cproc}

critcl::description {
    This package implements nothing. It serves only as a
    demonstration and template on how to declare various
    argument types to cproc.
}

critcl::subject demonstration {cproc argument types}
#critcl::config lines 0

# # ## ### ##### ######## ############# #####################
## C code.

# general types
foreach type {
    boolean int long wideint
    double float
    char* pstring bytes bytearray
    Tcl_Obj* list
    channel
} {
    critcl::cproc $type [list $type x] void {}
    # variadic - triggered by `args` argument name
    #critcl::cproc variadic-$type [list $type args] void {}
}

# constrained types
foreach t {int long wideint double float} {
    foreach {r v} {
	{> 0} 1	    {>= 0} 0	    {> 1} 2	    {>= 1} 1
	{< 0} -1    {<= 0} 0	    {< 1} 0	    {<= 1} 1
    } {
	set type "$t $r"
	critcl::cproc $type [list $type x] void {}
    }
}

# ### ### ### ######### ######### #########
## Ready
package provide ingress 1
