# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the management of the #line pragmas inserted into C code so
# that error messages refer back to the actual location this code came
# from.

# Originally a part of the critcl package.
# Factored out to
# - reduce the size of the critcl package. 
# - enhance readability and clarity in both critcl and this package.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.5        ;# Minimal supported Tcl runtime.
package require critcl::who    ;# Management of current file.
package require critcl::gopt   ;# Management of critcl's global options.
package require debug          ;# debug narrative

package provide critcl::at 4

namespace eval ::critcl::at {
    namespace export \
	caller caller! \
	here here! \
	get get* \
	incr incrt \
	= cpragma script \
	lines header
    namespace ensemble create
}

debug level  critcl/at
debug prefix critcl/at {[debug caller] | }

# # ## ### ##### ######## ############# #####################
## API commands.

# header  - trim leading empty lines, return count and remainder
# lines   - count lines
# script  - path ovrride for when sourcing files.
# caller  - stash caller location, possibly modified (level change, line offset)
# caller! - format & return caller location, clears stash
# here    - stash current location
# here!   - return format & return  current location, clears stash
# incr*   - modify stashed location (only line number, not file).
# get     - format, return, and clear stash
# get*    - format & return stash

proc ::critcl::at::header {text} {
    debug.critcl/at {}
    if {![regexp {^[\t\n ]+} $text header]} {
	return [list 0 $text]
    }
    set lines [regexp -all {\n} $header]
    # => The C code begins $lines lines after location of the c**
    #    command. This goes as offset into the generated #line pragma,
    #    because now (see next line) we throw away this leading
    #    whitespace.
    set text [string trim $text]
    return [list $lines $text]
}

proc ::critcl::at::lines {text} {
    debug.critcl/at {}
    set n [regexp -all {\n} $text]
    return $n
}

proc ::critcl::at::script {path} {
    debug.critcl/at {}
    variable source
    if {$path eq {}} {
	unset -nocomplain source
    } else {
	# See "Where" for use.
	set source $path
    }
    return
}

proc ::critcl::at::caller {{off 0} {level 0}} {
    debug.critcl/at {}
    ::incr level -3
    Where $off $level [who is]
    return
}

proc ::critcl::at::caller! {{off 0} {level 0}} {
    debug.critcl/at {}
    ::incr level -3
    Where $off $level [who is]
    return [get]
}

proc ::critcl::at::cpragma {leadoffset level file} {
    debug.critcl/at {}
    # internal variant of 'caller!'
    ::incr level -1
    Where $leadoffset $level $file
    return [get]
}

proc ::critcl::at::here {} {
    debug.critcl/at {}
    Where 0 -2 [who is]
    return
}

proc ::critcl::at::here! {} {
    debug.critcl/at {}
    Where 0 -2 [who is]
    return [get]
}

proc ::critcl::at::get {} {
    debug.critcl/at {}
    if {![gopt get lines]} {
	return {}
    }

    variable where
    if {![info exists where]} {
	Error "No location defined" UNDEFINED
    }

    set result [Format $where]
    unset where
    return $result
}

proc ::critcl::at::get* {} {
    debug.critcl/at {}
    variable where
    if {![info exists where]} {
	Error "No location defined" UNDEFINED
    }
    return [Format $where]
}

proc ::critcl::at::= {file line} {
    debug.critcl/at {}
    variable where
    set where [list $file $line]
    return
}

proc ::critcl::at::incr {args} {
    debug.critcl/at {}
    variable where
    lassign $where file line
    foreach offset $args {
	::incr line $offset
    }
    set where [list $file $line]
    return
}

proc ::critcl::at::incrt {args} {
    debug.critcl/at {}
    variable where
    if {$where eq {}} {
	Error "No location to change" EMPTY
    }
    lassign $where file line
    foreach text $args {
	::incr line [lines $text]
    }
    set where [list $file $line]
    return
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::at {
    # Make relevant "current file" and option commands available.
    namespace import ::critcl::who
    namespace import ::critcl::gopt

    # Saved location information
    # (2-element list, file name + line number)
    variable where

    # Override information for when sourcing a file.
    variable source
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

proc ::critcl::at::Where {leadoffset level file} {
    debug.critcl/at {}
    # XXX argument 'file' is not used, superfluous!
    # XXX test this, may allow removal of the dependency
    variable where
    variable source

    set line 1

    # If the interpreter running critcl has TIP 280 support use it to
    # place more exact line number information into the generated C
    # file.

    #puts "XXX-WHERE-($leadoffset $level $file)"
    #set ::errorInfo {}
    if {[catch {
	#SHOWFRAMES $level 0
	array set loc [info frame $level]
	#puts XXX-TYPE-$loc(type)
    }]} {
	#puts XXX-NO-DATA-$::errorInfo
	set where {}
	return
    }

    if {$loc(type) eq "source"} {
	#parray loc
	set  file  $loc(file)
	set  fline $loc(line)

	# Adjust for removed leading whitespace.
	::incr fline $leadoffset

	# Keep the limitations of native compilers in mind and stay
	# inside their bounds.

	if {$fline > $line} {
	    set line $fline
	}

	set where [list [file tail $file] $line]
	return
    }

    if {($loc(type) eq "eval") &&
       [info exists loc(proc)] &&
       ($loc(proc) eq "::critcl::source")
    } {
	# A relative location in critcl::source is absolute in the
	# sourced file.  I.e. we can provide proper line information.

	set  fline $loc(line)
	# Adjust for removed leading whitespace.
	::incr fline $leadoffset

	# Keep the limitations of native compilers in mind and stay
	# inside their bounds.

	if {$fline > $line} {
	    set line $fline
	}

	set where [list [file tail $source] $line]
	return
    }

    #puts XXX-NO-DATA-$loc(type)
    set where {}
    return
}

proc ::critcl::at::Format {loc} {
    debug.critcl/at {}
    if {![llength $loc]} {
	return ""
    }
    lassign $loc file line
    #::critcl::msg "#line $line \"$file\"\n"
    return        "#line $line \"$file\"\n"
}

proc ::critcl::at::SHOWFRAMES {level {all 1}} {
    debug.critcl/at {}
    set n [info frame]
    set i 0
    set id 1
    while {$n} {
	::critcl::msg "[expr {$level == $id ? "**" : "  "}] frame [format %3d $id]: [info frame $i]"
	::incr i -1
	::incr id -1
	::incr n -1
	if {($level > $id) && !$all} break
    }
    return
}

proc ::critcl::at::Error {msg args} {
    debug.critcl/at {}
    set code [linsert $args 0 CRITCL AT]
    return -code error -errorcode $code $msg
}

# # ## ### ##### ######## ############# #####################
## Ready
return
