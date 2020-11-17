# objtrack.tcl -
#
# C support package. Manages trackers for sets of Tcl_Obj*.
#
# Intended for use with Tcl_ObjTypes to enhance introspection into the
# live set of Tcl_Obj* for such.
#
# Exposes an API at C level for easy definition and management of the
# trackers.
#
# Exposes a number of Tcl level commands introspecting the set of
# trackers and their sets of Tcl_Obj*.

set version__ 0

package require critcl 3.1
critcl::buildrequirement {
    package require critcl::iassoc
    package require critcl::cutil ;# assertions, allocation support, tracing
}

if {![critcl::compiling]} {
    error "Unable to build `critcl::objtrack`, no proper compiler found."
}

# # ## ### ##### ######## #############
## Build configuration
# (1) Assertions, and tracing
# (2) Debugging symbols, memory tracking

critcl::cutil::assertions off
critcl::cutil::tracer     off

#critcl::debug symbols

# Activate when in need of memory debugging - Valgrind is an alternative
#critcl::debug symbols memory

critcl::config lines 1
critcl::config trace 0

# # ## ### ##### ######## #############
## Administrivia

critcl::license \
    {Andreas Kupries} \
    {Under a BSD license.}

critcl::summary \
    {Critcl utility package providing functions, structures and command to track sets of Tcl_Obj*}

critcl::description \
    {Part of Critcl}

critcl::subject critcl objtracks {Management of Tcl_Obj* sets}
critcl::subject {Tcl_Obj* introspection} {Tracking sets of Tcl_Obj*}

# # ## ### ##### ######## #############
## Implementation.

critcl::tcl 8.5
critcl::cutil::alloc

# # ## ### ##### ######## #############
## Tcl API

package provide critcl::objtrack $version__

critcl::cconst critcl::objtrack::version \
    char* "\"${version__}\""

critcl::cproc critcl::objtrack::names {Tcl_Interp* ip} object0 {
    return critcl_objtrack_names (ip);
}

# # ## ### ##### ######## #############
## C API

critcl::cheaders c/*.h
critcl::csources c/*.c

# Stubs definitions.

critcl::api header c/objtrack.h

# Register a tracker with the system for tracker introspection.
#
# Notes:
# - interp must not be 0
# - name must not be 0
# - name must not be an empty string
# - name must not have been used already
# - root has to have a strong root block signature

critcl::api function void critcl_objtrack_new {
    Tcl_Interp*                  interp
    {const char*}                name
    critcl_objtrack_scan_result  scan_result
}

# Enter a Tcl_Obj* into a tracker specified by its root block.
#
# Notes:
# - root must not be 0
# - root has to have a weak root block signature
# - obj  must not be 0
# - obj  must not be tracked already.

critcl::api function void critcl_objtrack_enter {
    Tcl_Interp*    interp
    {const char*}  name
    Tcl_Obj*       obj
}

# Remove a Tcl_Obj* from its tracker.
#
# Notes:
# - obj  must not be 0
# - obj  has to be tracked.

critcl::api function void critcl_objtrack_remove {
    Tcl_Obj*  obj
}

# Generate a report listing the Tcl_Obj* in a tracker specified by its
# root block.
#
# Notes:
# - root must not be 0
# - root has to have a weak root block signature
# - scan_result must not be 0

critcl::api function Tcl_Obj* critcl_objtrack_report {
    Tcl_Interp*    interp
    {const char*}  name
}

#
# Support for internals
#

critcl::iassoc::def critcl_objtrack_registry {} {
    Tcl_HashTable trackers;
} {
    Tcl_InitHashTable (&data->trackers, TCL_STRING_KEYS);
} {
    Tcl_DeleteHashTable (&data->trackers);
}

##
return
