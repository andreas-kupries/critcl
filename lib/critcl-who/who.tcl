# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the management of the "current" critcl file, i.e. the file
# which is currently processed and accumulating definitions. The
# exposed API allows the core to tinker with the information, enabling
# - redirection of definition to a virtual file.
# - inclusion of files and associating their definition with the includer.
# - ...

# Originally a part of the critcl package.
# Factored out to
# - reduce the size of the critcl package. 
# - enhance readability and clarity in both critcl and this package.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.5        ;# Minimal supported Tcl runtime.
package require debug          ;# debug narrative

package provide critcl::who 4

namespace eval ::critcl::who {
    namespace export is push pop depth
    namespace ensemble create
}

debug level  critcl/who
debug prefix critcl/who {[debug caller] | }

# # ## ### ##### ######## ############# #####################
## API commands.

## - Determine current file
## - Push an override
## - Pop an override
## - Retrieve number of overrides

proc ::critcl::who::is {} {
    debug.critcl/who {}
    variable who

    # An output redirection is active when the stack not empty. Report
    # the last redirection as the current file.
    if {[llength $who]} {
	return [lindex $who end]
    }

    # Report the actual file. To avoid problems with symbolic links or
    # different relative paths pointing to the same file [normalize]
    # is used to get a canonical path.
    return [file normalize [info script]]
}

proc ::critcl::who::push {ref} {
    debug.critcl/who {}
    variable who
    lappend who $ref
    return
}

proc ::critcl::who::pop {} {
    debug.critcl/who {}
    variable who
    # Get current slot, and pop from the diversion stack.
    # Remove the stack when it becomes empty.
    set slot [lindex $who end]
    set who  [lrange $who 0 end-1]

    debug.critcl/who {dropped ($slot)}
    return $slot
}

proc ::critcl::who::depth {} {
    debug.critcl/who {}
    variable who
    return [llength $who]
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::who {
    # Stack of overides. Nothing there yet initially.
    variable who {}
}

# # ## ### ##### ######## ############# #####################
## Ready
return
