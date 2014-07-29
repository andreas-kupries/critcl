# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the a number of critcl-specific utility functions used in
# the core and other helper packages.

# Originally a part of the critcl package.
# Factored out to
# - reduce the size of the critcl package. 
# - enhance readability and clarity in both critcl and this package.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.5        ;# Minimal supported Tcl runtime.
package require critcl::data 4 ;# Access to data files (license).
package require debug          ;# debug narrative

package provide critcl::common 4

namespace eval ::critcl::common {
    namespace export cat write append \
	text2words text2authors license-text \
	today now maxlen expand-glob separator
    namespace ensemble create

    namespace import ::critcl::data
}

debug level  critcl/common
debug prefix critcl/common {[debug caller] | }

# # ## ### ##### ######## ############# #####################
## API commands.

proc ::critcl::common::separator {} {
    return "/* [string repeat - 70] */"
}

proc ::critcl::common::cat {path} {
    debug.critcl/common {}
    # Easier to write our own copy than requiring fileutil and then
    # using fileutil::cat.

    set fd [open $path r]
    set data [read $fd]
    close $fd

    debug.critcl/common {==> <data elided>}
    return $data
}

proc ::critcl::common::write {dst contents} {
    debug.critcl/common {}

    file mkdir [file dirname $dst]
    set    chan [open $dst w]
    puts  $chan $contents
    close $chan

    debug.critcl/common {/done}
    return
}

proc ::critcl::common::append {dst contents} {
    debug.critcl/common {}

    file mkdir [file dirname $dst]
    set    chan [open $dst a]
    puts  $chan $contents
    close $chan

    debug.critcl/common {/done}
    return
}

proc ::critcl::common::text2words {text} {
    debug.critcl/common {}
    regsub -all {[ \t\n]+} $text { } text
    return [split [string trim $text]]
}

proc ::critcl::common::text2authors {text} {
    debug.critcl/common {}
    regsub -all {[ \t\n]+} $text { } text
    set authors {}
    foreach a [split [string trim $text] ,] {
	lappend authors [string trim $a]
    }
    return $authors
}

proc ::critcl::common::license-text {words} {
    debug.critcl/common {}
    if {[llength $words]} {
	# Use the supplied license details as our suffix.
	return [join $words]
    } else {
	# As no details were supplied we fall back to the critcl
	# license as template for the license of the generated
	# package. Note how we strip the first 2 lines from the
	# file. This removes the author information for critcl itself,
	# allowing us to replace it by the user-supplied author.

	return [join [lrange [split [cat [data file license.terms]] \
				  \n] \
			  2 end] \
		    \n]
    }
}

proc ::critcl::common::today {} {
    debug.critcl/common {}
    return [clock format [clock seconds] -format {%Y-%m-%d}]
}
`
proc ::critcl::common::now {} {
    debug.critcl/common {}
    return [clock format [clock seconds]]
}

proc ::critcl::common::maxlen {list} {
    debug.critcl/common {}

    set max 0
    foreach el $list {
	set l [string length $el]
	if {$l <= $max} continue
	set max $l
    }

    debug.critcl/common {==> $max}
    return $max
}

proc ::critcl::common::expand-glob {base pattern} {
    debug.critcl/common {}
    # Search is relative to the base directory, for a relative
    # pattern. Note however that we cannot use 'glob -directory'
    # here. The PATTERN may already be an absolute path, in which case
    # the join will return the unmodified PATTERN to glob on, whereas
    # with -directory the final pattern will be BASE/PATTERN which
    # won't find anything, even if PATTERN actually exists.

    set files {}
    foreach vfile [glob [file join $base $pattern]] {
	set vfile [file normalize $vfile]
	if {![file exists $vfile]} {
	    error "$vfile: not found"
	}
	lappend files $vfile
    }
    return $files
}

# XXX FIXME lappendlist - remove, replace with proper lappend calls.
proc ::critcl::common::lappendlist {lvar list} {
    if {![llength $list]} return
    upvar $lvar dest
    lappend dest {*}$list
    return
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::common {}

# # ## ### ##### ######## ############# #####################
## Internal support commands

# -- none --

# # ## ### ##### ######## ############# #####################
## Initialization

# -- none --

# # ## ### ##### ######## ############# #####################
## Ready
return
