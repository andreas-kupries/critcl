# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the management for template files, Tcl header files, etc.
# including the data files themselves. The package is the container
# for these files.

# Originally a part of the critcl package.
# Factored out to
# - reduce the size of the critcl package. 
# - enhance readability and clarity in both critcl and this package.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.5        ;# Minimal supported Tcl runtime.
package require debug          ;# debug narrative

package provide critcl::data 4

namespace eval ::critcl::data {
    namespace export available-tcl cfile file hdr \
	tcl-decls tcl-plat-decls
    namespace ensemble create
}

debug level  critcl/data
debug prefix critcl/data {[debug caller] | }

# # ## ### ##### ######## ############# #####################
## API commands.

## - Retrieve paths to data files held by the package.

proc ::critcl::data::available-tcl {} {
    debug.critcl/data {}
    variable available

    debug.critcl/data {==> $available}
    return  $available
}

proc ::critcl::data::cfile {name} {
    file c/$name
}

proc ::critcl::data::hdr {name} {
    file h/$name
}

proc ::critcl::data::file {name} {
    debug.critcl/data {}
    variable selfdir

    set path  [::file join $selfdir $name]
    debug.critcl/data {==> $path}
    return $path
}

proc ::critcl::data::tcl-decls {tclversion} {
    return [TclDef $tclversion tclDecls.h tclStubsPtr]
}

proc ::critcl::data::tcl-plat-decls {tclversion} {
    return [TclDef $tclversion tclPlatDecls.h tclPlatStubsPtr]
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::data {
    # Location of the package itself.
    variable selfdir

    # List of the Tcl versions for which we have headers available.
    variable available
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

proc ::critcl::data::TclDef {tclversion hdrfile var} {
    debug.critcl/data {}

    set hdr [hdr tcl$tclversion/$hdrfile]

    if {![::file exists $hdr]} {
	HdrError " file not found: $hdrfile" \
	    MISSING $hdrfile
    }
    if {![::file isfile $hdr]} {
	HdrError " not a file: $hdrfile" \
	    NOT-FILE $hdrfile
    }
    if {![::file readable $hdr]} {
	HdrError " not readable: $hdrfile (no permission)" \
	    NOT-READABLE $hdrfile
    }

    if {[catch {
	set hdrcontent [split [Cat $hdr] \n]
    } msg]} {
	HdrError " not readable: $hdrfile ($msg)" \
	    NOT-READABLE $hdrfile
    }

    # Note, Danger: The code below is able to use declarations which
    # are commented out in various ways (#if 0, /* ... */, and //
    # ...), because it is performing a simple line-oriented search
    # without context, and not matching against comment syntax either.

    set ext [Grep *extern* $hdrcontent]
    if {![llength $ext]} {
	HdrError ": No extern declarations found in $hdrfile" \
	    NO-EXTERN $hdrfile
    }

    set vardecl [Grep *${var}* $ext]
    if {![llength $vardecl]} {
	HdrError ": No declarations for $var found in $hdrfile" \
	    NO-DECL $hdrfile
    }

    set def [string map {extern {}} [lindex $vardecl 0]]
    ::critcl::msg " ($var => $def)"

    debug.critcl/data {==> ($def)}
    return $def
}

proc ::critcl::data::HdrError {msg args} {
    debug.critcl/data {}
    set code [linsert $args 0 CRITCL DATA HEADER]
    return -code error -errorcode $code "Header$msg"
}

proc ::critcl::data::Grep {pattern lines} {
    set r {}
    foreach line $lines {
	if {![string match $pattern $line]} continue
	lappend r $line
    }
    return $r
}

proc ::critcl::data::Cat {path} {
    debug.critcl/data {}
    # Easier to write our own copy than requiring fileutil and then
    # using fileutil::cat. Here also needed to avoid a dependency
    # cycle between this and "critcl::common".

    set fd [open $path r]
    set data [read $fd]
    close $fd
    return $data
}

# # ## ### ##### ######## ############# #####################
## Initialization

apply {{} {
    variable selfdir   [::file dirname [::file normalize [info script]]]
    variable available {}

    # Scan the directory holding our copies of the Tcl header and
    # determine for which versions of Tcl we actually have
    # headers. This allows distributions to modify the directory,
    # i.e. drop our copies and refer to the system headers instead, as
    # much as are installed, and critcl adapts. The tcl versions are
    # recorded in ascending order, making upcoming searches easier,
    # the first satisfying version is also always the smallest.

    foreach d [lsort -dict [glob -types {d r} -directory $selfdir/h -tails tcl*]] {
	lappend available [regsub {^tcl} $d {}]
    }

    return
} ::critcl::data}

# # ## ### ##### ######## ############# #####################
## Ready
return
