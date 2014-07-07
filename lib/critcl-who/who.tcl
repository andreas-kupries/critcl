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

package require Tcl 8.4            ;# Minimal supported Tcl runtime.

package provide  critcl::who 1
namespace eval ::critcl::who {
    namespace export is push pop depth
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## API commands.

## - Determine current file
## - Push an override
## - Pop an override
## - Retrieve number of overrides

proc ::critcl::who::is {} {
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
    variable who
    lappend who $ref
    return
}

proc ::critcl::who::pop {} {
    variable who
    # Get current slot, and pop from the diversion stack.
    # Remove the stack when it becomes empty.
    set slot [lindex $who end]
    set who  [lrange $who 0 end-1]
    return $slot
}

proc ::critcl::who::depth {} {
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
## Internal support commands

# -- none --

# # ## ### ##### ######## ############# #####################
## Initialization

# -- none --

# # ## ### ##### ######## ############# #####################
## Ready
return
