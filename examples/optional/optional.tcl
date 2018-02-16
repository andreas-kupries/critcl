# optional.tcl --
#
#	A template demonstrating the handling of optional arguments
#	to cproc.
#
# Copyright (c) 2012 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl 8.4
package require critcl 3.1.2

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::license {Andreas Kupries} BSD

critcl::summary {Optional arguments for cproc}

critcl::description {
    This package implements nothing. It serves only as a
    demonstration and template on how to declare cproc's
    with optional arguments.
}

critcl::subject demonstration {cproc optional arguments}
#critcl::config lines 0

# # ## ### ##### ######## ############# #####################
## C code.

critcl::cproc fixed {int a int b int c int d} void {
    printf ("F|%d|%d|%d|%d|\n", a,b,c,d);
    fflush(stdout);
}

critcl::cproc optional_head {int {a 1} int {b 2} int c int d} void {
    printf ("H|%d|%d|%d|%d|\n", a,b,c,d);
    fflush(stdout);
}

critcl::cproc optional_tail {int a int b int {c 1} int {d 2}} void {
    printf ("T|%d|%d|%d|%d|\n", a,b,c,d);
    fflush(stdout);
}

critcl::cproc optional_middle {int a int {b 1} int {c 2} int d} void {
    printf ("M|%d|%d|%d|%d|\n", a,b,c,d);
    fflush(stdout);
}

# ### ### ### ######### ######### #########
## Ready
package provide optional 1
