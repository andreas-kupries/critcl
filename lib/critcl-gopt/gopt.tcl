# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the management of critcl's global options.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.5        ;# Minimal supported Tcl runtime.
package require debug          ;# debug narrative

package provide critcl::gopt 4

namespace eval ::critcl::gopt {
    namespace export set unset has
    namespace ensemble create
}

debug level  critcl/gopt
debug prefix critcl/gopt {[debug caller] | }

# # ## ### ##### ######## ############# #####################
## API commands.

## - Change the value of an option
## - Retrieve the value of an option.

proc ::critcl::gopt::set {option newvalue} {
    debug.critcl/gopt {}
    variable data

    Check $option
    dict set data $option $newvalue
    return $newvalue
}

proc ::critcl::gopt::get {option} {
    debug.critcl/gopt {}
    variable data

    Check $option
    set result [dict get $data $option]
    debug.critcl/gopt {==> ($result)}
    return $result
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::gopt {
    # Global critcl options controlling various aspects of
    # code generation. Below the defaults.
    variable  data {
	combine  {}
	force    no
	keepsrc  no
	I        {}
	L        {}
	language ""
	lines    yes
	outdir   {}
    }

    # outdir - Path. If set the place where the generated
    #   shared library is saved for permanent use.
    #
    # keepsrc - Boolean. If set all generated .c files are
    #   kept after compilation. Helps with debugging
    #   the critcl package.
    #
    # combine - XXX standalone/dynamic/static
    #   XXX Meaning of combine?
    #
    # force - Boolean. If set (re)compilation is
    #   forced, regardless of the state of
    #   the cache.
    #
    # I - List. Additional include
    #   directories, globally specified by
    #   the user for mode 'generate
    #   package', for all components put
    #   into the package's library.
    #
    # L - List. Additional library search
    #   directories, globally specified by
    #   the user for mode 'generate
    #   package'.
    #
    # language - String. XXX
    #
    # lines - Boolean. If set (default) the generator will emit
    #   #line-directives to help locating C code in the .tcl in case of
    #   compile warnings and errors.

}

# # ## ### ##### ######## ############# #####################
## Internal support commands

proc ::critcl::gopt::Check {option} {
    variable data
    if {[dict exists $data $option]} return
    set choices [linsert [join [lsort -dict [dict keys data] {, }] end-1 or]
    return -code error \
	-errorcode {CRITCL GOPT INVALID} \
	"Bad option \"$option\". Must be one of $choices"
}

# # ## ### ##### ######## ############# #####################
## Initialization

# -- none --

# # ## ### ##### ######## ############# #####################
## Ready
return
