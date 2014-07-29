# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the management of the cache directory used to keep
# - generated shared libraries (compile & run) to amortize build times
# - generated code and header files, including stub files (all modes).

# Originally a part of the critcl package.
# Factored out to
# - reduce the size of the critcl package. 
# - enhance readability and clarity in both critcl and this package.
#   - Proper communication through APIs.

# ATTENTION: As an internal package it currently does not check if
# users supply paths jumping out of the cache directory
# (i.e. /something, or ../something and the like).
#
# It assumes that callers supply only proper paths.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.4        ;# Minimal supported Tcl runtime.
package require fileutil       ;# general path helper commands.
package require critcl::common ;# general critcl utilities.
package require debug          ;# debug narrative

package provide  critcl::cache 1
namespace eval ::critcl::cache {
    namespace export def get write append copy2 clear
    namespace ensemble create
}

debug level  critcl/cache
debug prefix critcl/cache {[debug caller] | }

# # ## ### ##### ######## ############# #####################
## API commands.

## - Set/retrieve cache directory
## - Write data into a cache file.
## - Append data of a cache file.
## - Copy file into cache.
## - Clear cache (by pattern).

proc ::critcl::cache::def {dir} {
    debug.critcl/cache {}
    # TODO: Check that path exists, is directory, is readable, and writable.
    variable path [file normalize $dir]
    return
}

proc ::critcl::cache::get {{child {}}} {
    debug.critcl/cache {}
    variable path
    if {[llength [info level 0]] < 2} {
	return $path
    }
    # XXX TODO: Validate that $child is a path in the cache directory.
    set dst [file join $path $child]
    file mkdir [file dirname $dst]
    return $dst
}

proc ::critcl::cache::write {file contents} {
    debug.critcl/cache {}
    variable path
    # XXX TODO: Validate that $file is a path in the cache directory.
    set dst [file join $path $file]
    common write $dst $contents
    return $dst
}

proc ::critcl::cache::append {file contents} {
    debug.critcl/cache {}
    variable path
    # XXX TODO: Validate that $file is a path in the cache directory.
    set dst [file join $path $file]
    common append $dst $contents
    return $dst
}

proc ::critcl::cache::copy2 {src {dst {}}} {
    debug.critcl/cache {}
    variable path

    if {[llength [info level 0]] < 3} {
	# No dst => Default to src file part
	set dst [file tail $src]
    }
    # XXX TODO: Validate that $dst is a path in the cache directory.
    set dst [file join $path $file]
    file mkdir [file dirname $dst]

    ## XXX check if this handles a src-directory correctly.

    file copy $src $dst
    return $dst
}

proc ::critcl::cache::clear {args} {
    debug.critcl/cache {}
    variable path
    if {![llength $args]} { lappend args * }
    foreach pattern $args {
	foreach file [glob -nocomplain -directory $path $pattern] {
	    file delete -force $file
	}
    }
    return
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::cache {
    # Location of the cache directory.
    variable path

    namespace import ::critcl::common
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

# -- none --
# TODO: Validate that $dst is a path in the cache directory.

# # ## ### ##### ######## ############# #####################
## Ready
return
