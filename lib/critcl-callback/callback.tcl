# callback.tcl -
#
# C support package for management of callbacks.
# Note, this package does not expose anything at Tcl level.
# It only provides stubs to manage C-level callback managers.

package provide critcl::callback 1

package require critcl 3.1
critcl::buildrequirement {
    package require critcl::cutil ;# assertions, allocation support, tracing
}

if {![critcl::compiling]} {
    error "Unable to build `critcl::callback`, no proper compiler found."
}

# # ## ### ##### ######## #############
## Build configuration
# (1) Assertions, and tracing
# (2) Debugging symbols, memory tracking

critcl::cutil::assertions on
critcl::cutil::tracer     off

critcl::debug symbols
#critcl::debug memory
#critcl::debug symbols memory

critcl::config lines 1
critcl::config trace 0

# # ## ### ##### ######## #############
## Administrivia

critcl::license \
    {Andreas Kupries} \
    {Under a BSD license.}

critcl::summary \
    {Critcl utility package providing Tcl callbacks}

critcl::description {
    Part of Critcl
}

critcl::subject libmarpa marpa parser lexer {c runtime} earley aycock horspool
critcl::subject {joop leo} table-parsing chart-parsing top-down

# # ## ### ##### ######## #############
## Implementation.

critcl::tcl 8.5
critcl::cutil::alloc

# # ## ### ##### ######## #############

critcl::cheaders c/*.h
critcl::csources c/*.c

# Stubs definitions.

critcl::api header c/callback.h

# Create a new callback instance with prefix objc/objv and space for
# `nargs` arguments. The callback keeps the objv elements as is and
# signal this by incrementing their reference counts. The callback
# will be run in the provided interpreter, at the global level and
# namespace.

critcl::api function critcl_callback_p critcl_callback_new {
    Tcl_Interp* interp
    int         objc
    Tcl_Obj**   objv
    int         nargs
}

# Release all memory associated with the callback instance. For the
# objv elements saved during construction (see above) this is signaled
# by decrementing their reference counts.

critcl::api function void critcl_callback_destroy {
    critcl_callback_p callback
}

# Invoke the callback using the objc/objv elements as the arguments.
# The operation will panic or crash if more arguments are provided
# than the callback has space for. See the `nargs` parameter of the
# constructor function above. Less arguments then constructed for
# however are ok.

critcl::api function int critcl_callback_invoke {
    critcl_callback_p callback
    int               objc
    Tcl_Obj**         objv
}

##
return
