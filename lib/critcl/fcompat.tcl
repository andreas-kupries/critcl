## -*- tcl -*-
# # ## ### ##### ######## ############# #####################

# CriTcl Core Helper. Forward compatibility support.

# I.e. code implementing a number of commands for 8.4 which are
# otherwise only defined in 8.5+
#
# Critcl uses 'lassign', and 'dict'.
#

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.4 ; # Min supported version.

# # ## ### ##### ######## ############# #####################
## I. Make sure that the Tcl interpreter has a 'lassign' command.

if {![llength [info commands ::lassign]]} {
    proc lassign {valueList args} {
        if {[llength $args] == 0} {
            return -code error "wrong # args: lassign list varname ?varname..?"
        }
        uplevel 1 [list foreach $args $valueList {break}]
        return [lrange $valueList [llength $args] end]
    }
}

# # ## ### ##### ######## ############# #####################
## II. Make sure that the Tcl interpreter has a 'dict' command.

if {![llength [info commands ::dict]]} {
    # Try to load the separate 'dict' package first. ActiveTcl 8.4,
    # for example, has it.
    if {[catch {
	package require dict
    }]} {
	# That failed. Now we implement 'dict' by ourselves, in pure
	# Tcl.
	source [file join [file dirname [file normalize [info script]]] \
		    pmdict.tcl]
	package provide dict 1
    }
}

##
# # ## ### ##### ######## ############# #####################
## Ready
return
