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

package require Tcl 8.4          ;# Minimal supported Tcl runtime.
package require critcl::common   ;# General shared utility commands.
package require dict84           ;# Forward-compatible dict command.
package require lassign84        ;# Forward-compatible lassign command.

package provide  critcl::scan 1
namespace eval ::critcl::scan {}

# Core API commands used here (back references).
# - ::critcl::msg
# - ::critcl::print

# # ## ### ##### ######## ############# #####################
## API commands.

proc ::critcl::scan {file} {
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

    dict set capture meta \
	[eval [linsert $md 0 dict merge]]
    # meta = dict merge {*}$md

    if {[dict exists $capture meta require]} {
	foreach r [dict get $capture meta require] {
	    ::critcl::print "\tRequired:     $r"
	}
    }

    return $capture
}

proc ::critcl::scan-dependencies {key file {mode plain}} {
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
	    # XXX back reference into critcl core
	    ::critcl::msg -nonewline " (provide $name $version)"
	    ::critcl::ImetaSet $key name     $name
	    ::critcl::ImetaSet $key version  $version
	}

	dict for {k vlist} [dict get $capture meta-system] {
	    if {$k eq "name"}    continue
	    if {$k eq "version"} continue

	    # XXX back reference into critcl core
	    ::critcl::ImetaAdd $key $k $vlist

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

    namespace import ::critcl::common::*
}

# # ## ### ##### ######## ############# #####################
## Internal support commands

proc critcl::scan::Init {cap file {key require}} {
    variable rkey    $key
    variable base    [file dirname [file normalize $file]]
    variable capture $cap
    return
}

proc critcl::scan::Push {} {
    variable capture
    variable saved
    lappend saved $capture
    unset capture
    return
}

proc critcl::scan::Pop {} {
    variable capture
    variable saved
    set result $capture
    set capture [lindex $saved end]
    set saved   [lrange $saved 0 end-1]
    return $result
}

proc critcl::scan::Core {lines theconfig} {
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

proc ::critcl::scan::critcl::Files {args} {
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
	    error "$vfile: not found"
	}

	# Constrain to be inside of the base directory.
	# Snarfed from fileutil::stripPath

	set npath [file split $xfile]

	if {![string match -nocase "${prefix} *" $npath]} {
	    error "$vfile: Not inside of $base"
	}

	set xfile [eval [linsert [lrange $npath [llength $prefix] end] 0 file join ]]
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
    namespace import ::critcl::common::*
}

proc ::critcl::scan::critcl::buildrequirement {script} {
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
    variable ::critcl::scan::capture
    dict update capture meta-system m {
	dict lappend m require [list Tcl $version]
    }
    return
}

proc ::critcl::scan::critcl::tk {} {
    variable ::critcl::scan::capture
    dict update capture meta-system m {
	dict lappend m require Tk
    }
    return
}

proc ::critcl::scan::critcl::description {text} {
    variable ::critcl::scan::capture
    dict set capture meta-system \
	description [text2words $text]
    return
}

proc ::critcl::scan::critcl::summary {text} {
    variable ::critcl::scan::capture
    dict set capture meta-system \
	summary [text2words $text]
    return
}

proc ::critcl::scan::critcl::subject {args} {
    variable ::critcl::scan::capture
    dict update capture meta-system m {
	foreach word $args {
	    dict lappend m subject $word
	}
    }
    return
}

proc ::critcl::scan::critcl::meta {key args} {
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
    # Recursively scan the imported file.
    # Keep the current context.
    variable ::critcl::scan::config

    # XXX in-scanner cross-level references.
    foreach f [Files $path] {
	set lines [split [cat $f] \n]
	::critcl::scan::Core $lines $config
    }
    return
}
proc ::critcl::scan::critcl::owns     {args} { eval [linsert $args 0 Files] }
proc ::critcl::scan::critcl::cheaders {args} { eval [linsert $args 0 Files] }
proc ::critcl::scan::critcl::csources {args} { eval [linsert $args 0 Files] }
proc ::critcl::scan::critcl::tsources {args} {
    variable ::critcl::scan::capture
    foreach ts [eval [linsert $args 0 Files]] {
	dict lappend capture tsources $ts
    }
    return
}

# Capture license (org name)
proc ::critcl::scan::critcl::license {who args} {
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
	    [list [clock format [clock seconds] -format {%Y-%m-%d}]]
	return
    }

    if {$cmd eq "require"} {
	# Syntax: package require <name> ?-exact? <version>
	#       : package require <name> <version-range>...

	# Save dependencies as meta data.

	# Ignore the critcl core
	if {[lindex $args 0] eq "critcl"} return

	dict update capture meta-system m {
	    dict lappend m $rkey \
		[::critcl::TeapotRequire $args]
	    # XXX back reference into critcl core
	}
	return
    }

    # ignore anything else.
    return
}

# Capture the APIs imported by the package
proc ::critcl::scan::critcl::api {cmd args} {
    variable ::critcl::scan::capture
    switch -exact -- $cmd {
	header {
	    eval [linsert $args 0 Files]
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
    variable ::critcl::scan::capture
    switch -exact -- $cmd {
	define {
	    # Syntax: critcl::userconfig define <name> <description> <type> ?<default>?
	    lassign $args oname odesc otype odefault
	    set odesc [string trim $odesc]
	    if {[llength $args] < 4} {
		set odefault [::critcl::UcDefault $otype]
		# XXX back reference into critcl core
	    }
	    dict lappend capture config [list $oname $odesc $otype $odefault]
	    ::critcl::print "\tUser Config:  $oname ([join $otype { }] -> $odefault) $odesc"
	}
	set - query -
	default {}
    }
    return
}

# # ## ### ##### ######## ############# #####################
## Full internal helper commands.

# # ## ### ##### ######## ############# #####################
## Initialization

# -- none --

# # ## ### ##### ######## ############# #####################
## Ready
return
