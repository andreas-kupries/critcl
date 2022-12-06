# selfcompanion.tcl --
#
#	A template demonstrating how to distribute the critcl file
#	as its own Tcl companion file.
#
#	This is not really the recommended way of handling Tcl
#	companion code, however as critcl 2 supported this, and v3 is
#	still supporting it, an example might still be in order.
#
#	One issue this causes is a dependency on critcl itself in the
#	generated package. Whereas if the Tcl code to distribute is
#	properly split off into a separate companion file such a
#	dependency can be avoided.
#
# Copyright (c) 2011,2022 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl 8.6
package require critcl 3.2

# # ## ### ##### ######## ############# #####################
## Define the Tcl companion code. This is done always, as we
## might be in mode 'build & run'.

proc companion {} {
    return [self]
}

# # ## ### ##### ######## ############# #####################
## Determine the environment, and define/build the C parts only
## if not noted as build already.

#puts Compiled=[critcl::compiled]

if {[critcl::compiled]} return

#critcl::msg "Declaring, building C parts"

#puts Compiling=[critcl::compiling]

if {![critcl::compiling]} {
    return -code error "Unable to build the C parts of selfcompanion"
}

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::license {Andreas Kupries} BSD

critcl::summary {Demonstration of using the critcl file as its own Tcl companion}

critcl::description {
    This package implements nothing. It serves only as a
    demonstration and template on how to setup and distribute
    the critcl file as its own Tcl companion file.
}

critcl::subject template demonstration self-companion

# # ## ### ##### ######## ############# #####################
## Configuration, setup as its own Tcl companion file.

critcl::tsources [info script]

# # ## ### ##### ######## ############# #####################
## C code.

critcl::cdata self "me"

# ... And other parts of the C level ...

# ### ### ### ######### ######### #########
## Ready
package provide selfcompanion 1
