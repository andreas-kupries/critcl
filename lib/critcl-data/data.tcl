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

package require Tcl 8.4            ;# Minimal supported Tcl runtime.

package provide  critcl::data 1
namespace eval ::critcl::data {
    namespace export available-tcl cfile file hdr \
	tcl-decls tcl-plat-decls
    catch { namespace ensemble create }
}

# # ## ### ##### ######## ############# #####################
## API commands.

## - Retrieve paths to data files held by the package.

proc ::critcl::data::available-tcl {} {
    variable available
    return  $available
}

proc ::critcl::data::cfile {name} { file c/$name }
proc ::critcl::data::hdr   {name} { file h/$name }

proc ::critcl::data::file {name} {
    variable selfdir
    return [file join $selfdir $name]
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

proc ::critcl::TclDef {tclversion hdrfile var} {
    #puts F|$file
    set hdrfile [hdr tcl$tclversion $file] $hdrfile]

    if {![file exists   $hdrfile]} { error "Header file not found: $hdrfile" }
    if {![file isfile   $hdrfile]} { error "Header not a file: $hdrfile" }
    if {![file readable $hdrfile]} { error "Header not readable: $hdrfile (no permission)" }

    #puts H|$hdrfile
    if {[catch {
	set hdrcontent [split [Cat $hdrfile] \n]
    } msg]} {
	error "Header not readable: $hdrfile ($msg)"
    }

    # Note, Danger: The code below is able to use declarations which
    # are commented out in various ways (#if 0, /* ... */, and //
    # ...), because it is performing a simple line-oriented search
    # without context, and not matching against comment syntax either.

    set ext [Grep *extern* $hdrcontent]
    if {![llength $ext]} {
	error "No extern declarations found in $hdr"
    }

    set vardecl [Grep *${var}* $ext]
    if {![llength $vardecl]} {
	error "No declarations for $var found in $hdr"
    }

    set def [string map {extern {}} [lindex $vardecl 0]]
    msg " ($var => $def)"
    return $def
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

proc ::critcl::data::Initialize {} {
    variable selfdir   [file dirname [file normalize [info script]]]
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

    rename ::critcl::data::Initialize {}
    return
}

::critcl::data::Initialize

# # ## ### ##### ######## ############# #####################
## Ready
return
