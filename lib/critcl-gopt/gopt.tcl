# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the management of critcl's global options.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.4            ;# Minimal supported Tcl runtime.

package provide  critcl::gopt 1
namespace eval ::critcl::gopt {
    namespace export set unset has
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## API commands.

## - Change the value of an option
## - Retrieve the value of an option.

proc ::critcl::gopt::set {option newvalue} {
    variable data
    Check $option
    ::set data($option) $newvalue
    return $newvalue
}

proc ::critcl::gopt::get {option} {
    variable data
    Check $option
    return $data($option)
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::gopt {
    # Global critcl options controlling various aspects of
    # code generation. Below the defaults.
    variable  data
    array set data {
	outdir   {}
	keepsrc  no
	combine  {}
	force    no
	I        {}
	L        {}
	language ""
	lines    yes
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
    if {[info exists data($option)]} return
    set choices [linsert [join [lsort -dict [array names data] {, }] end-1 or]
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
