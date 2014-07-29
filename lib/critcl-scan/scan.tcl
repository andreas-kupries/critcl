# -*- tcl -*-
# # ## ### ##### ######## ############# #####################
## (C) 2014 Andreas Kupries

# Support package for the core Critcl package.

# Contains the static scanner used to extract dependencies, meta data,
# related files, etc from .critcl files.

# Originally a part of the critcl package.
# Factored out to
# - reduce the size of the critcl package. 
# - enhance readability and clarity in both critcl and this package.

# # ## ### ##### ######## ############# #####################
## Requirements.

package require Tcl 8.4        ;# Minimal supported Tcl runtime.
package require critcl::common ;# General shared utility commands.
package require critcl::meta   ;# Management of teapot meta data.
package require debug          ;# debug narrative

package provide critcl::scan 4

namespace eval ::critcl {
    namespace export scan scan-dependencies
}
namespace eval ::critcl::scan {}

debug level  critcl/scan
debug prefix critcl/scan {[debug caller] | }

# Core API commands used here (back references).
# - ::critcl::msg
# - ::critcl::print

# # ## ### ##### ######## ############# #####################
## API commands.

proc ::critcl::scan {file} {
    debug.critcl/scan {}
    set lines [split [scan::cat $file] \n]

    scan::Init {
	org         {}
	version     {}
	files       {}
	imported    {}
	config      {}
	meta-user   {}
	meta-system {}
	tsources    {}
    } $file

    scan::Core $lines {
	critcl::api			sub
	critcl::api/extheader		ok
	critcl::api/function		ok
	critcl::api/header		warn
	critcl::api/import		ok
	critcl::source                  warn
	critcl::cheaders		warn
	critcl::csources		warn
	critcl::license			warn
	critcl::meta			warn
	critcl::owns			warn
	critcl::tcl			ok
	critcl::tk			ok
	critcl::tsources		warn
	critcl::userconfig		sub
	critcl::userconfig/define	ok
	critcl::userconfig/query	ok
	critcl::userconfig/set		ok
	package				warn
    }

    variable scan::capture
    set version [dict get $capture version]
    print "\tVersion:      $version"

    # TODO : Report requirements.

    set n [llength [dict get $capture files]]
    ::critcl::print -nonewline "\tInput:        $file"
    if {$n} {
	::critcl::print -nonewline " + $n Companion"
	if {$n > 1} { ::critcl::print -nonewline s }
    }
    ::critcl::print ""

    # Merge the system and user meta data, with system overriding the
    # user. See 'GetMeta' for same operation when actually builing the
    # package. Plus scan any Tcl companions for more requirements.

    set     md {}
    lappend md [dict get $capture meta-user]
    lappend md [dict get $capture meta-system]

    foreach ts [dict get $capture tsources] {
	# XXX could we use scan::base here ?
	set ts        [file join [file dirname $file] $ts]
	set tscapture [scan-dependencies $file $ts capture]
	lappend md [dict get $tscapture meta-system]
    }

    dict unset capture meta-user
    dict unset capture meta-system
    dict unset capture tsources

    dict set capture meta [dict merge {*}$md]

    if {[dict exists $capture meta require]} {
	foreach r [dict get $capture meta require] {
	    ::critcl::print "\tRequired:     $r"
	}
    }

    return $capture
}

proc ::critcl::scan-dependencies {key file {mode plain}} {
    debug.critcl/scan {}
    set lines [split [scan::cat $file] \n]

    if {$mode eq "capture"} {
	scan::Push
    }

    scan::Init {
	name        {}
	version     {}
	meta-system {}
    } $file

    scan::Core $lines {
	critcl::buildrequirement	warn
	package				warn
    }

    if {$mode eq "capture"} {
	return [scan::Pop]
    }

    variable scan::capture
    dict with capture {
	if {$mode eq "provide"} {
	    ::critcl::msg -nonewline " (provide $name $version)"
	    meta assign $key name     $name
	    meta assign $key version  $version
	}

	dict for {k vlist} [dict get $capture meta-system] {
	    if {$k eq "name"}    continue
	    if {$k eq "version"} continue

	    meta extend $key $k $vlist

	    if {$k ne "require"} continue
	    ::critcl::msg -nonewline " ($k [join $vlist {}])"
	}

	# The above information also goes into the teapot meta data of
	# the file in question. This however is defered until the meta
	# data is actually pulled for delivery to the tool using the
	# package. See 'GetMeta' for where the merging happens.
    }

    return
}

# # ## ### ##### ######## ############# #####################
## Internal state

namespace eval ::critcl::scan {
    # Scanner configuration
    # - Key for collected package requirements
    # - Base directory holding the scanned file.
    # - Map of commands to recognize and process.
    variable rkey
    variable base
    variable config

    # Scanner state, i.e. captured information.
    variable capture

    # Stack of saved captures to handle nested invokation.
    variable saved

    namespace import ::critcl::common
    namespace import ::critcl::meta
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

proc critcl::scan::Init {cap file {key require}} {
    debug.critcl/scan {}
    variable rkey    $key
    variable base    [file dirname [file normalize $file]]
    variable capture $cap
    return
}

proc critcl::scan::Push {} {
    debug.critcl/scan {}
    variable capture
    variable saved
    lappend saved $capture
    unset capture
    return
}

proc critcl::scan::Pop {} {
    debug.critcl/scan {}
    variable capture
    variable saved
    set result $capture
    set capture [lindex $saved end]
    set saved   [lrange $saved 0 end-1]
    return $result
}

proc critcl::scan::Core {lines theconfig} {
    debug.critcl/scan {}
    # config = dictionary
    # - <cmdname> => mode (ok, warn, sub)
    # Unlisted commands are ignored.

    variable config $theconfig

    set collect 0
    set buf {}
    set lno -1
    foreach line $lines {
	#puts |$line|

	incr lno
	if {$collect} {
	    if {![info complete $buf]} {
		append buf $line \n
		continue
	    }
	    set collect 0

	    #puts %%$buf%%

	    # Prevent heavily dynamic code from stopping the scan.
	    # WARN the user.
	    regexp {^(\S+)} $buf -> cmd
	    if {[dict exists $config $cmd]} {
		set mode [dict get $config $cmd]

		if {[catch {
		    # Run in the scan namespace, with its special
		    # command implementations.
		    namespace eval ::critcl::scan $buf
		} msg]} {
		    if {$mode eq "sub"} {
			regexp {^(\S+)\s+(\S+)} $buf -> _ method
			append cmd /$method
			set mode [dict get $config $cmd]
		    }
		    if {$mode eq "warn"} {
			# XXX back reference into critcl core
			::critcl::msg "Line $lno, $cmd: Failed execution of dynamic command may"
			::critcl::msg "Line $lno, $cmd: cause incorrect TEA results. Please check."
			::critcl::msg "Line $lno, $cmd: $msg"
		    }
		}
	    }

	    set buf ""
	    # fall through, to handle the line which just got NOT
	    # added to the buf.
	}

	set line [string trimleft $line " \t:"]
	if {[string trim $line] eq {}} continue

	regexp {^(\S+)} $line -> cmd
	if {[dict exists $config $cmd]} {
	    append buf $line \n
	    set collect 1
	}
    }
}

namespace eval ::critcl::scan::critcl {}

proc ::critcl::scan::critcl::Files {args} {
    debug.critcl/scan {}
    variable ::critcl::scan::capture
    set res {}
    foreach v $args {
	if {[string match "-*" $v]} continue
	foreach f [Expand $v] {
	    dict lappend capture files $f
	    lappend res $f
	}
    }
    return $res
}

proc ::critcl::scan::critcl::Expand {pattern} {
    debug.critcl/scan {}
    variable ::critcl::scan::base

    # ATTENTION: We cannot use "glob -directory" here.
    ##
    # The PATTERN may already be an absolute path, in which case the
    # join will return the unmodified PATTERN to glob on, whereas with
    # -directory the final pattern will be BASE/PATTERN which will not
    # find anything, even if PATTERN actually exists.

    set prefix [file split $base]

    set files {}
    foreach vfile [glob [file join $base $pattern]] {
	set xfile [file normalize $vfile]
	if {![file exists $xfile]} {
	    Error "$vfile: not found" MISSING $vfile
	}

	# Constrain to be inside of the base directory.
	# Snarfed from fileutil::stripPath

	set npath [file split $xfile]

	if {![string match -nocase "${prefix} *" $npath]} {
	    Error "$vfile: Not inside of $base" OUTSIDE $vfile $base
	}

	set xfile [file join {*}[lrange $npath [llength $prefix] end]]
	lappend files $xfile
    }
    return $files
}

# # ## ### ##### ######## ############# #####################
## Internal support commands
#
# The scanner-specific replacements of core critcl commands, and some
# Tcl builtin commands.

namespace eval ::critcl::scan::critcl {
    namespace import ::critcl::common
}

proc ::critcl::scan::critcl::buildrequirement {script} {
    debug.critcl/scan {}
    # Recursive scan of the script, same configuration, except
    # switched to record 'package require's under the build::reqire
    # key.

    variable ::critcl::scan::config
    variable ::critcl::scan::rkey

    set lines [split $script \n]

    set saved $rkey
    set rkey build::require

    ::critcl::scan::Core $lines $config

    set rkey $saved
    return
}

# Meta data.
# Capture specific dependencies
proc ::critcl::scan::critcl::tcl {version} {
    debug.critcl/scan {}
    variable ::critcl::scan::capture
    dict update capture meta-system m {
	dict lappend m require [list Tcl $version]
    }
    return
}

proc ::critcl::scan::critcl::tk {} {
    debug.critcl/scan {}
    variable ::critcl::scan::capture
    dict update capture meta-system m {
	dict lappend m require Tk
    }
    return
}

proc ::critcl::scan::critcl::description {text} {
    debug.critcl/scan {}
    variable ::critcl::scan::capture
    dict set capture meta-system \
	description [text2words $text]
    return
}

proc ::critcl::scan::critcl::summary {text} {
    debug.critcl/scan {}
    variable ::critcl::scan::capture
    dict set capture meta-system \
	summary [text2words $text]
    return
}

proc ::critcl::scan::critcl::subject {args} {
    debug.critcl/scan {}
    variable ::critcl::scan::capture
    dict update capture meta-system m {
	foreach word $args {
	    dict lappend m subject $word
	}
    }
    return
}

proc ::critcl::scan::critcl::meta {key args} {
    debug.critcl/scan {}
    variable ::critcl::scan::capture
    dict update capture meta-user m {
	foreach word $args {
	    dict lappend m $key $word
	}
    }
    return
}

# Capture files
proc ::critcl::scan::critcl::source {path} {
    debug.critcl/scan {}
    # Recursively scan the imported file.
    # Keep the current context.
    variable ::critcl::scan::config

    # XXX in-scanner cross-level references.
    foreach f [Files $path] {
	set lines [split [common cat $f] \n]
	::critcl::scan::Core $lines $config
    }
    return
}
proc ::critcl::scan::critcl::owns     {args} { Files {*}$args }
proc ::critcl::scan::critcl::cheaders {args} { Files {*}$args }
proc ::critcl::scan::critcl::csources {args} { Files {*}$args }
proc ::critcl::scan::critcl::tsources {args} {
    debug.critcl/scan {}
    variable ::critcl::scan::capture
    foreach ts [Files {*}$args] {
	dict lappend capture tsources $ts
    }
    return
}

# Capture license (org name)
proc ::critcl::scan::critcl::license {who args} {
    debug.critcl/scan {}
    variable ::critcl::scan::capture
    dict set capture org $who

    ::critcl::print "\tOrganization: $who"

    # Meta data.
    set elicense [license-text $args]

    dict set capture meta-system \
	license [text2words $elicense]

    dict set capture meta-system \
	author [text2authors $who]
    return
}

# Capture version of the provided package.
proc ::critcl::scan::package {cmd args} {
    debug.critcl/scan {}
    variable capture
    variable rkey

    if {$cmd eq "provide"} {
	# Syntax: package provide <name> <version>

	lassign $args name version
	dict set capture name    $name
	dict set capture version $version

	# Save as meta data as well.

	dict set capture meta-system name     $name
	dict set capture meta-system version  $version
	dict set capture meta-system platform source
	dict set capture meta-system generated::by \
	    [list \
		 [list critcl [::package present critcl]] \
		 $::tcl_platform(user)]
	dict set capture meta-system generated::date \
	    [list [today]]
	return
    }

    if {$cmd eq "require"} {
	# Syntax: package require <name> ?-exact? <version>
	#       : package require <name> <version-range>...

	# Save dependencies as meta data.

	# Ignore the critcl core
	if {[lindex $args 0] eq "critcl"} return

	dict update capture meta-system m {
	    dict lappend m $rkey [TeapotRequire $args]
	}
	return
    }

    # ignore anything else.
    return
}

# Capture the APIs imported by the package
proc ::critcl::scan::critcl::api {cmd args} {
    debug.critcl/scan {}
    variable ::critcl::scan::capture
    switch -exact -- $cmd {
	header {
	    Files {*}$args
	}
	import {
	    # Syntax: critcl::api import <name> <version>
	    lassign $args name _
	    dict lappend capture imported $name
	    ::critcl::print "\tImported:     $name"
	}
	default {}
    }
    return
}

# Capture the user config options declared by the package
proc ::critcl::scan::critcl::userconfig {cmd args} {
    debug.critcl/scan {}
    variable ::critcl::scan::capture
    switch -exact -- $cmd {
	define {
	    # Syntax: critcl::userconfig define <name> <description> <type> ?<default>?
	    lassign $args oname odesc otype odefault
	    set odesc [string trim $odesc]
	    if {[llength $args] < 4} {
		dict lappend capture config [list $oname $odesc $otype]
		::critcl::print "\tUser Config:  $oname ([join $otype { }]) $odesc"
	    } else {
		dict lappend capture config [list $oname $odesc $otype $odefault]
		::critcl::print "\tUser Config:  $oname ([join $otype { }] -> $odefault) $odesc"
	    }
	}
	set - query -
	default {}
    }
    return
}

# # ## ### ##### ######## ############# #####################
## Full internal helper commands.

proc ::critcl::scan::critcl::TeapotRequire {dspec} {
    debug.critcl/scan {}
    # Syntax of dspec: (a) pname
    #             ...: (b) pname req-version...
    #             ...: (c) pname -exact req-version
    #
    # We can assume that the syntax is generally ok, because otherwise
    # the 'package require' itself will fail in a moment, blocking the
    # further execution of the .critcl file. So we only have to
    # distinguish the cases.

    if {([llength $dspec] == 3) &&
	([lindex  $dspec 1] eq "-exact")} {
	# (c)
	lassign $dspec pn _ pv
	set spec [list $pn ${pv}-$pv]
    } else {
	# (a, b)
	set spec $dspec
    }

    return $spec
}

proc ::critcl::scan::critcl::Error {msg args} {
    debug.critcl/scan {}
    set code [linsert $args 0 CRITCL SCAN]
    return -code error -errorcode $code $msg
}

# # ## ### ##### ######## ############# #####################
## Initialization

# -- none --

# # ## ### ##### ######## ############# #####################
## Ready
return
