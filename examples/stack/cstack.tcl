# cstack.tcl --
#
#	Low-level stack data structure. With wrapping usable as
#	a Tcl-level stack.
#
# Copyright (c) 2008-2011,2022 Andreas Kupries <andreas_kupries@users.sourceforge.net>

# Example of exporting a C-level stubs API through critcl v3, with a
# package header file containing public type definitions, macros,
# etc., and internal C companion files.

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl 8.6
package require critcl 3.2

# # ## ### ##### ######## ############# #####################
## Administrivia

critcl::license {Andreas Kupries} BSD

critcl::summary {A C-level abstract datatype for stacks}

critcl::description {
    This package implements an abstract
    data type for stacks, at the C-level.
    No Tcl-binding is provided. See package
    'stackc' for that.
}

critcl::subject stack
critcl::subject {data structure}
critcl::subject structure
critcl::subject {abstract data structure}
critcl::subject {generic data structure}

# # ## ### ##### ######## ############# #####################
## Configuration

critcl::api header cstack.h
critcl::cheaders   cstackInt.h

# # ## ### ##### ######## ############# #####################
## Exported API

#
#  Notes
#  - push -- Item allocation is responsibility of caller.
#            Stack takes ownership of the item.
#  - pop  -- Stack frees allocated item.
#  - trim -- Ditto
#  - top  -- Provides top item, no transfer of ownership.
#  - del  -- Releases stack, cell array, and items, if any.
#  - drop -- Like pop, but doesn't free, assumes that caller
#            is taking ownership of the pointer.
#

critcl::api function CSTACK     cstack_new  {CSTACK_CELL_FREE freeCell void* clientdata}
critcl::api function void       cstack_del  {CSTACK s}

critcl::api function {long int} cstack_size {CSTACK s}
critcl::api function void*      cstack_top  {CSTACK s}
critcl::api function void       cstack_push {CSTACK s void*      item}
critcl::api function void       cstack_pop  {CSTACK s {long int} n}
critcl::api function void       cstack_trim {CSTACK s {long int} n}
critcl::api function void       cstack_drop {CSTACK s {long int} n}
critcl::api function void       cstack_rol  {CSTACK s {long int} n {long int} step}
critcl::api function void       cstack_get  {CSTACK s {long int} n CSTACK_DIRECTION dir CSTACK_SLICE* slice}
critcl::api function void       cstack_move {CSTACK s CSTACK      src}

critcl::api function void       cstack_clientdata_set {CSTACK s void* clientdata}
critcl::api function void*      cstack_clientdata_get {CSTACK s}

# # ## ### ##### ######## ############# #####################
## Implementation.

critcl::csources cstack.c
critcl::ccode {} ; # Fake the 'nothing to build detector'

# ### ### ### ######### ######### #########
## Ready
package provide cstack 1
