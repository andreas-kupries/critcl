# variadic.tcl --
#
#	A template demonstrating the handling of variadic arguments
#	to cproc.
#
# Copyright (c) 2012,2022 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl 8.6
package require critcl 3.2

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::license {Andreas Kupries} BSD

critcl::summary {Variadic arguments for cproc}

critcl::description {
    This package implements nothing. It serves only as a
    demonstration and template on how to declare cproc's
    with variadic arguments.
}

critcl::subject demonstration {cproc variadic arguments}
#critcl::config lines 0

# # ## ### ##### ######## ############# #####################
## C code.

critcl::cproc variadic {int args} void {
    int i;
    for (i=0; i < args.c; i++) printf ("[%2d] = %d\n", i, args.v[i]);
    fflush(stdout);
}

critcl::cproc ovariadic {Tcl_Obj* args} void {
    int i;
    for (i=0; i < args.c; i++) printf ("[%2d] = '%s'\n", i, Tcl_GetString(args.v[i]));
    fflush(stdout);
}


# ### ### ### ######### ######### #########
## Ready
package provide variadic 1
